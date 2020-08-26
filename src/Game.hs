{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Game (runGameIO) where

import           Control.Concurrent.MVar
import           Control.Eff
import           Control.Eff.Coroutine
import           Control.Eff.Reader.Strict
import           Control.Eff.State.Strict
import           Control.Monad           (liftM, when)
import           Control.Monad.Cont      (Cont (..), ContT (..), callCC, cont,
                                          runCont, runContT)
import           Control.Monad.Loops     (whileM_)
import qualified Control.Monad.Trans     as Trans
import           Data.Array              (indices)
import           Data.Array.Base         (MArray (unsafeWrite))
import           Data.Array.MArray       (getElems, newArray_, readArray,
                                          writeArray)
import           Data.Foldable           (for_)
import           Data.Functor.Identity   (Identity (Identity), runIdentity)
import           Data.List               (partition, permutations, sortBy, (\\))
import           Data.Maybe              (fromJust, fromMaybe, isJust,
                                          isNothing)
import           GameType
import           Lens.Micro.Platform
import           RandomUtil
import           Shuffle
import           System.Random
import           Debug.Trace             (trace)

type Mutable x = Reader (MVar x)
type SingleMutable x r = (Member (Mutable x) r, Lifted IO r)
type MutableList xs r = (xs <:: r, Lifted IO r)

newMutable :: Lifted IO r => x -> Eff r (MVar x)
newMutable = lift . newMVar

getMutable :: SingleMutable x r => Eff r x
getMutable = ask >>= (lift . readMVar)

setMutable :: SingleMutable x r => x -> Eff r ()
setMutable x = do
  var <- ask
  lift $ takeMVar var
  lift $ putMVar var x

modifyMutable :: SingleMutable x r => (x -> x) -> Eff r ()
modifyMutable f = getMutable >>= (setMutable . f)

runMutable :: Lifted IO r => x -> Eff (Mutable x ': r) a -> Eff r (a, x)
runMutable x e = do
  var <- lift . newMVar $ x
  a <- runReader var e
  new_x <- lift . readMVar $ var
  return (a, new_x)

execMutable :: Lifted IO r => x -> Eff (Mutable x ': r) a -> Eff r x
execMutable x = fmap snd . runMutable x

evalMutable :: Lifted IO r => x -> Eff (Mutable x ': r) a -> Eff r a
evalMutable x = fmap fst . runMutable x

runMutableState :: SingleMutable x r => Eff (State x ': r) a -> Eff r a
runMutableState e = do
  x <- getMutable
  (a, x') <- runState x e
  setMutable x'
  return a

nextId Clockwise max id
  | id <= 1 = max
  | otherwise = id - 1
nextId Counterclockwise max id
  | id >= max = 1
  | otherwise = id + 1

describeCard :: CardPlayed -> String
describeCard = ((++) . describeColor . view (color . to runIdentity) <*> (":"++) . describeKind . view kind) . view runCardPlayed
  where
    describeColor Red    = "红"
    describeColor Blue   = "蓝"
    describeColor Green  = "绿"
    describeColor Yellow = "黄"
    describeNumber = show . fromEnum @NumberCard
    describeKind (Number n)        = "数字牌" ++ describeNumber n
    describeKind (Spell Universal) = "万能牌"
    describeKind (Spell Draw2)     = "抽两张"
    describeKind (Spell Draw4)     = "抽四张"
    describeKind (Spell Skip)      = "跳过"
    describeKind (Spell Reverse)   = "反转"

displayTurnAnalysis :: Lifted IO r => TurnAnalysis -> Eff r ()
displayTurnAnalysis (PlayerPlayCard id card) = lift $ putStrLn ("玩家 " ++ show id ++ " 打出了" ++ describeCard card)
displayTurnAnalysis (PlayerPass id) = lift $ putStrLn ("玩家 " ++ show id ++ " 选择跳过")
displayTurnAnalysis (PlayerDrawCards id num) = lift $ putStrLn ("玩家 " ++ show id ++ " 抽了 " ++ show num ++ " 张卡")
displayTurnAnalysis (PlayerDrawAndPlay id num card) = lift $ putStrLn ("玩家 " ++ show id ++ " 抽了 " ++ show num ++ " 张卡并打出" ++ describeCard card)
displayTurnAnalysis (PlayerSkiped id) = lift $ putStrLn ("玩家 " ++ show id ++ " 被跳过")

data PlayerTurnRequest = PlayerTurnRequest { runPlayerTurnRequest :: (GameDependency, PlayerDependency)
                                           }
                       | SimplePassTA TurnAnalysis
data PlayerTurnResponse = PlayerTurnResponse { runPlayerTurnResponse :: (GameDependency, PlayerDependency, TurnAnalysis)
                                             }
                        | Void

reverseOrder Counterclockwise = Clockwise
reverseOrder Clockwise        = Counterclockwise

draws :: SingleMutable PlayerDependency r => Int -> (PlayerDependency -> (TurnAnalysis -> ContT TurnAnalysis (Eff r) TurnAnalysis) -> ContT TurnAnalysis (Eff r) TurnAnalysis) -> Eff r TurnAnalysis
draws num k = do
  dep <- getMutable
  if num > length (dep ^. ppile)
  then do
    setMutable (dep & pplayer . candidates %~ ((dep ^. ppile) ++)
                    & ppile .~ []
                    & pdrawed .~ pure num)
    return (PlayerDrawCards (dep ^. pplayer . index) (length (dep ^. ppile)))
  else do
    let (topn, rest) = splitAt num (dep ^. ppile)
    setMutable (dep & pplayer . candidates %~ (topn ++)
                    & ppile .~ rest
                    & pdrawed .~ pure num)
    dep' <- getMutable
    (`runContT` (lift . return)) $ callCC (k dep')

play :: SingleMutable PlayerDependency r => CardOnHand -> (TurnAnalysis -> ContT TurnAnalysis (Eff r) TurnAnalysis) -> Color -> Eff r TurnAnalysis
play card k optionalColor = do
  dep <- getMutable
  when (card `notElem` dep ^. pplayer . candidates) (error (show card ++ " isn't in your candidates!"))
  let cardPlayed = (card & _runCardNew) & color %~ (pure . fromMaybe optionalColor) & CardPlayed
  setMutable (dep & pplayer . candidates %~ (\\ [card])
                  & pturn . order %~ (case card of CardNewKindView (Spell Reverse) -> reverseOrder ; _ -> id)
                  )
  case dep ^. pdrawed of
    Nothing -> (`runContT` (lift . return)) $ k (PlayerPlayCard (dep ^. pplayer . index) cardPlayed)
    Just num -> (`runContT` (lift . return)) $ k (PlayerDrawAndPlay (dep ^. pplayer . index) num cardPlayed)

pass :: SingleMutable PlayerDependency r => (TurnAnalysis -> ContT TurnAnalysis (Eff r) TurnAnalysis) -> Eff r TurnAnalysis
pass k = do
  dep <- getMutable
  (`runContT` (lift . return)) $ k (PlayerPass (dep ^. pplayer . index))

availableCards :: Turn -> [CardOnHand] -> [CardOnHand]
availableCards (NormalCard c k) =
  filter
    (\(CardNew cd) ->
       case cd of
         Card {_color = Nothing}             -> True
         Card {_color = Just c0, _kind = k0} -> c0 == c || k0 == k)

calculateScore :: Player -> Score
calculateScore (view candidates -> cards) =
  sum (fromEnum . unwrapNumber . view (runCardNew . kind) <$> numberCards) +
  (length spellCards * 20) +
  (length universalCards * 50)
  where
    (numberCards, rest) =
      partition
        (\(view (runCardNew . kind) -> k) ->
           case k of
             Number _ -> True
             _        -> False)
        cards
    (universalCards, spellCards) =
      partition
        (\(view (runCardNew . kind) -> k) ->
           case k of
             Spell Universal -> True
             Spell Draw4     -> True
             _               -> False)
        rest
    unwrapNumber (Number n) = n

randomGenWrapper :: (SingleMutable GameDependency r0, (State RandomGenWrapper ': r0) ~ r1) => Eff r1 a -> Eff r0 a
randomGenWrapper f = do
  dep <- getMutable
  (a, g') <- runState (dep ^. rgen) f
  setMutable (dep & rgen .~ g')
  return a

initialPile :: Pile
initialPile =
  CardNew <$>
  concatMap (replicate 4) [Card Nothing (Spell s) | s <- [Draw4, Universal]] ++
  concatMap
    (replicate 2)
    [Card (Just c) (Spell s) | s <- [Draw2, Skip, Reverse], c <- fullColor] ++
  concatMap
    (if' . isZero . _kind <*> return <*> replicate 2)
    [Card (Just c) (Number n) | c <- fullColor, n <- [Zero .. Nine]]
  where
    fullColor = [Red .. Yellow]
    if' b a c =
      if b
        then a
        else c
    isZero (Number Zero) = True
    isZero _             = False

randomGame :: SingleMutable RandomGenWrapper r => Int -> Eff r Game
randomGame ps = do
  randomPile <- runMutableState @RandomGenWrapper $ shuffleE initialPile
  let (randomPlayers, (top:remainingPile)) =
        let (p, c) = distribute randomPile initialCardsOnHand
         in (zipWith Player c [1 .. ps], p)
      len = length randomPlayers
  rc :: Color <- runMutableState @RandomGenWrapper randomE
  return $ Game remainingPile [Turn (top & _runCardNew & color %~ (Identity . fromMaybe rc) & CardPlayed) 0 Clockwise] randomPlayers 1
  where
    initialCardsOnHand = replicate ps []
    distribute pile chs =
      if length (head chs) == 7
        then (pile, chs)
        else let (drawn, pile') = splitAt ps pile
              in distribute pile' (zipWith (:) drawn chs)
    shuffleWrapper xs = randomGenWrapper (shuffleE xs)

stupidAI :: MutableList '[Mutable GameDependency, Mutable PlayerDependency] r => Eff r TurnAnalysis
stupidAI = do
  dep <- getMutable
  let acards = availableCards (dep ^. pturn) (dep ^. pplayer . candidates)
  if null acards
  then draws 1 $ \dep desk ->
    let acards' = availableCards (dep ^. pturn) (dep ^. pplayer . candidates)
    in Trans.lift $ if null acards'
    then pass desk
    else randomPlay acards' desk
  else randomPlay acards return
  where
    randomPlay :: ('[Reader (MVar PlayerDependency), Reader (MVar GameDependency)] <:: r, Lifted IO r) => [CardOnHand] -> (TurnAnalysis -> ContT TurnAnalysis (Eff r) TurnAnalysis) -> Eff r TurnAnalysis
    randomPlay acards@(length -> size) desk = do
      selection <- fmap (acards !!) (randomGenWrapper $ randomRE (0, size - 1))
      randomColor <- randomGenWrapper randomE
      trace ("player chose card " ++ show selection) $
        play selection desk randomColor


playerTurn :: (MutableList '[Mutable PlayerDependency, Mutable GameDependency] r, Member (Yield PlayerTurnRequest PlayerTurnResponse) r) => Eff r ()
playerTurn = do
  dep :: PlayerDependency <- getMutable
  gdep :: GameDependency<- getMutable
  let playerAction = do
        res <- yield (PlayerTurnRequest (gdep, dep))
        let (PlayerTurnResponse (gdep', dep', ta)) = res
        setMutable gdep'
        setMutable dep'
        return ta
  if null (dep ^. ppile)
  then do
    ta <- pass return
    yield @PlayerTurnRequest @PlayerTurnResponse (SimplePassTA ta)
    return ()
  else
    case dep ^. pturn of
      SkipCard ->
       if nextId (dep ^. pturn . order) (gdep ^. game . players & length) (dep ^. pturn . playingPlayer) == (dep ^. pplayer . index)
       then do
        trace "player was skiped" $
          yield @PlayerTurnRequest @PlayerTurnResponse (SimplePassTA (PlayerSkiped (dep ^. pplayer . index)))
        return ()
       else do
        ta <- playerAction
        case ta of
          PlayerPass _ -> return ()
          _            -> setMutable (dep & pturn . playingPlayer .~ dep ^. pplayer . index)
      DrawCards _ Draw2 ->
       if nextId (dep ^. pturn . order) (gdep ^. game . players & length) (dep ^. pturn . playingPlayer) == (dep ^. pplayer . index)
       then do
        ta <- draws 2 $ \_ desk -> desk (PlayerDrawCards (dep ^. pplayer . index) 2)
        yield @PlayerTurnRequest @PlayerTurnResponse (SimplePassTA ta)
        return ()
       else do
        ta <- playerAction
        case ta of
          PlayerPass _ -> return ()
          _            -> setMutable (dep & pturn . playingPlayer .~ dep ^. pplayer . index)
      DrawCards _ Draw4 ->
       if nextId (dep ^. pturn . order) (gdep ^. game . players & length) (dep ^. pturn . playingPlayer) == (dep ^. pplayer . index)
       then do
        ta <- draws 4 $ \_ desk -> desk (PlayerDrawCards (dep ^. pplayer . index) 4)
        yield @PlayerTurnRequest @PlayerTurnResponse (SimplePassTA ta)
        return ()
       else do
        ta <- playerAction
        case ta of
          PlayerPass _ -> return ()
          _            -> setMutable (dep & pturn . playingPlayer .~ dep ^. pplayer . index)
      _ -> playerAction >> return ()

runGame :: SingleMutable GameDependency r => Eff r GameResult
runGame = do
  go
  gdep :: GameDependency <- getMutable
  let (winner : rest) = sortBy (\a b -> compare (length (a^.candidates)) (length (b ^. candidates)) ) (gdep ^. game . players)
  return $ GameResult (winner^.index, sum $ calculateScore <$> rest) (gdep ^. game)
  where
    deal (Y c (PlayerTurnRequest (gdep, dep))) _ = do
      ((ta, dep'), gdep') <- runMutable gdep . runMutable dep $ stupidAI
      trace (show ta) $
        c (PlayerTurnResponse (gdep', dep', ta)) >>= (`deal` ta)
    deal (Y c (SimplePassTA ta)) _ = c Void >>= (`deal` ta)
    deal Done result = return result
    go = do
      gdep <- getMutable
      let game' = gdep ^. game
          dep = PlayerDependency (game' ^. pile) (game' ^. playedTurn ^? ix 0 & fromJust) ((game' ^. players ^? ix ((game' ^. playerIndex) - 1)) & fromJust) Nothing
      (ta, dep') <- runMutable dep . ((>>= flip deal undefined) . runC) $ playerTurn
      displayTurnAnalysis ta
      case dep' ^. ppile of
        [] -> return ()
        _ -> do
          modifyMutable (over game (set pile (dep' ^. ppile)
                      . set playerIndex (nextId (dep' ^. pturn . order) (game' ^. players & length) (game' ^. playerIndex))
                      . set (players . ix ((dep' ^. pplayer . index) - 1)) (dep' ^. pplayer)
                      . set (playedTurn . ix ((dep' ^. pplayer . index) - 1)) (dep' ^. pturn)))
          go

runGameIO :: IO ()
runGameIO = do
  gen <- RandomGenWrapper <$> getStdGen
  runLift . runMutable gen $ do
    ga <- randomGame 2
    evalMutable (GameDependency ga gen) runGame
  return ()
