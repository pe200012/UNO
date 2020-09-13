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

module Game (runGameIO)  where

import           Control.Concurrent.MVar
import           Control.Eff
import           Control.Eff.Coroutine
import           Control.Eff.Reader.Lazy
import           Control.Eff.State.Lazy
import           Control.Monad           (liftM, unless, when)
import           Control.Monad.Cont      (Cont (..), ContT (..), callCC, cont,
                                          runCont, runContT)
import           Control.Monad.Fix       (fix, mfix)
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
import           Debug.Trace             (trace)
import           GameType
import           Lens.Micro.Platform
import           RandomUtil
import           Shuffle
import           System.Random
import           Text.Printf             (printf)
import           Text.Read               (readMaybe)

type MonadPlayer r = (MutableList '[Mutable Pile, Mutable Player] r, Member (Reader Turn) r)

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

describeCardNew :: CardNew -> String
describeCardNew = ((++) . describeColor . view color <*> (":"++) . describeKind . view kind) . view runCardNew
  where
    describeColor Nothing       = "无色"
    describeColor (Just Red)    = "红"
    describeColor (Just Blue)   = "蓝"
    describeColor (Just Green)  = "绿"
    describeColor (Just Yellow) = "黄"
    describeNumber = show . fromEnum @NumberCard
    describeKind (Number n)        = "数字牌" ++ describeNumber n
    describeKind (Spell Universal) = "万能牌"
    describeKind (Spell Draw2)     = "抽两张"
    describeKind (Spell Draw4)     = "抽四张"
    describeKind (Spell Skip)      = "跳过"
    describeKind (Spell Reverse)   = "反转"

displayTurnReport :: Lifted IO r => TurnReport -> Eff r ()
displayTurnReport (PlayerPlayCard id card) = lift $ putStrLn ("玩家 " ++ show id ++ " 打出了" ++ describeCard card)
displayTurnReport (PlayerPass id) = lift $ putStrLn ("玩家 " ++ show id ++ " 选择跳过")
displayTurnReport (PlayerDrawCards id num) = lift $ putStrLn ("玩家 " ++ show id ++ " 抽了 " ++ show num ++ " 张卡")
displayTurnReport (PlayerDrawAndPlay id num card) = lift $ putStrLn ("玩家 " ++ show id ++ " 抽了 " ++ show num ++ " 张卡并打出" ++ describeCard card)
displayTurnReport (PlayerSkiped id) = lift $ putStrLn ("玩家 " ++ show id ++ " 被跳过")
displayTurnReport EmptyPile = lift $ putStrLn "牌堆已空"

displayGameResult :: Lifted IO r => GameResult -> Eff r ()
displayGameResult r = lift $ putStrLn ("终局，胜利者为玩家 " ++ show (r ^. to winner . _1) ++ " ，得分 " ++ show (r ^. to winner . _2) ++ " 。")

displayWinner :: Lifted IO r => Player -> Eff r ()
displayWinner p = lift $ putStrLn ("玩家 " ++ show (p ^. index) ++ " 率先打出所有手牌！")

displayInitialTurn :: Lifted IO r => Turn -> Eff r ()
displayInitialTurn t = lift $ putStrLn ("开局，场牌为" ++ describeCard (t ^. playedCard))

reverseOrder Counterclockwise = Clockwise
reverseOrder Clockwise        = Counterclockwise

draws :: (MonadPlayer r, Member (Mutable (Maybe Int)) r) => Int -> ((TurnReport -> ContT TurnReport (Eff r) TurnReport) -> ContT TurnReport (Eff r) TurnReport) -> Eff r TurnReport
draws num k = do
  pile <- getMutable
  player <- getMutable
  if num > length pile
  then do
    modifyMutable (over candidates (pile ++))
    setMutable @Pile []
    setMutable (Just (length pile))
    return (PlayerDrawCards (player ^. index) (length pile))
  else do
    let (topn, rest) = splitAt num pile
    modifyMutable (over candidates (pile ++))
    setMutable rest
    setMutable (Just num)
    (`runContT` (lift . return)) $ callCC k

play :: (MonadPlayer r, Member (Mutable (Maybe Int)) r) => CardOnHand -> (TurnReport -> ContT TurnReport (Eff r) TurnReport) -> Color -> Eff r TurnReport
play card k optionalColor = do
  player <- getMutable
  when (card `notElem` player ^. candidates) (error (show card ++ " isn't in your candidates!"))
  let cardPlayed = (card & _runCardNew) & color %~ (pure . fromMaybe optionalColor) & CardPlayed
  modifyMutable (over candidates (\\ [card]))
  drew <- getMutable
  case drew of
    Nothing -> (`runContT` (lift . return)) $ k (PlayerPlayCard (player ^. index) cardPlayed)
    Just num -> (`runContT` (lift . return)) $ k (PlayerDrawAndPlay (player ^. index) num cardPlayed)

pass :: (MonadPlayer r, Member (Mutable (Maybe Int)) r) => (TurnReport -> ContT TurnReport (Eff r) TurnReport) -> Eff r TurnReport
pass k = do
  player <- getMutable
  drew <- getMutable
  case drew of
    Nothing -> (`runContT` (lift . return)) $ k (PlayerPass (player ^. index))
    Just num -> (`runContT` (lift . return)) $ k (PlayerDrawCards (player ^. index) num)

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

dependencyWrapper :: (SingleMutable GameDependency r0, (Reader Turn : Mutable Player : Mutable Pile : r0) ~ r1) => Eff r1 a -> Eff r0 a
dependencyWrapper e = do
  ga <- view game <$> getMutable
  ((a, pl), pi) <- runMutable (ga ^. pile) . runMutable ((ga ^. players ^? ix ((ga ^. playerIndex) - 1)) & fromJust) . runReader (ga ^. playedTurn . to head) $ e
  modifyMutable (over game (set pile pi . set (players . ix ((ga ^. playerIndex) - 1)) pl))
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

playerTurn :: SingleMutable GameDependency r => Eff r (Maybe TurnReport)
playerTurn = do
  ga <- view game <$> getMutable
  let lastTurn = ga ^. playedTurn . to head
  if null (ga ^. pile)
  then return (Just EmptyPile)
  else do
    let nextToMe = lastTurn ^. playingPlayer == 0 || nextId (lastTurn ^. order) (lastTurn ^. playingPlayer) (ga ^. players & length) == ga ^. playerIndex
    dependencyWrapper . evalMutable (Nothing :: Maybe Int) $ case ga ^. playedTurn . to head of
      SkipCard | nextToMe -> return (Just $ PlayerSkiped (ga ^. playerIndex))
      DrawCards _ Draw2 | nextToMe -> Just <$> draws 2 (Trans.lift . pass)
      DrawCards _ Draw4 | nextToMe -> Just <$> draws 4 (Trans.lift . pass)
      _ -> return Nothing

stupidAI :: SingleMutable GameDependency r => Eff r TurnReport
stupidAI = do
  ga <- view game <$> getMutable
  let lastTurn = ga ^. playedTurn . to head
      player   = ga ^. players ^? ix ((ga ^. playerIndex) - 1) & fromJust
      acards   = availableCards lastTurn (player ^. candidates)
  dependencyWrapper . evalMutable (Nothing :: Maybe Int) $
    if null acards
    then  draws 1 $ \desk -> do
      ga <- Trans.lift $ view game <$> getMutable
      let player = ga ^. players ^? ix ((ga ^. playerIndex) - 1) & fromJust
          acards = availableCards lastTurn (player ^. candidates)
      Trans.lift $
        if null acards
        then pass desk
        else randomPlay acards desk
    else randomPlay acards return
  where
    randomPlay acards@(length -> size) desk = do
      selection <- fmap (acards !!) (randomGenWrapper $ randomRE (0, size - 1))
      randomColor <- randomGenWrapper randomE
      play selection desk randomColor

humanPlayer :: SingleMutable GameDependency r => Eff r TurnReport
humanPlayer = do
  ga <- view game <$> getMutable
  let pl = ga ^. players ^? ix ((ga ^. playerIndex) - 1) & fromJust
      acards = availableCards (ga ^. playedTurn . to head) (pl ^. candidates)
  lift $ putStrLn ("轮到人类玩家 " ++ show (pl ^. index))
  lift $ putStrLn "您现在拥有手牌如下："
  lift . for_ ([0..] `zip` (pl ^. candidates)) $ \(i, c) -> putStrLn (show i ++ ") " ++ describeCardNew c)
  lift $ putStrLn "可以打出的手牌如下："
  lift . for_ ([0..] `zip` acards) $ \(i, c) -> putStrLn (show i ++ ") " ++ describeCardNew c)
  dependencyWrapper . evalMutable (Nothing :: Maybe Int) . flip fix () $ \rec _ -> do
    lift $ printf "行动：p) 抽一张卡并跳过 d) 抽一张卡 0-%d) 打出对应卡牌 -> " (length acards - 1)
    choice :: String <- lift getLine
    case readMaybe choice of
      Just n | 0 <= n && (length acards - 1) >= n ->
        let card = acards !! n
        in if card ^. runCardNew . color & isNothing
        then
          flip fix () $ \rec' _ -> do
            lift $ putStr "选择颜色 (r/b/g/y/quit) : "
            chosenColor :: String <- lift getLine
            case chosenColor of
              "r" -> play card return Red
              "g" -> play card return Green
              "b" -> play card return Blue
              "y" -> play card return Yellow
              "quit" -> rec ()
              _   -> do
                lift $ putStrLn "无效的颜色，请重新输入。"
                rec' ()
        else play card return undefined
             | otherwise -> do
                lift $ putStrLn "超出有效范围，请重新输入。"
                rec ()
      Nothing ->
        case choice of
          "p" -> draws 1 (Trans.lift . pass)
          "d" -> runReader (ga ^. pile . to head) $ draws 1 (Trans.lift . afterAction)
          _   -> do
            lift $ putStrLn "无效的选项，请重新输入。"
            rec ()
  where afterAction desk = do
          newCard <- ask
          ga <- view game <$> getMutable
          let pl = ga ^. players ^? ix ((ga ^. playerIndex) - 1) & fromJust
              acards = availableCards (ga ^. playedTurn . to head) (pl ^. candidates)
          lift $ putStrLn ("抽到" ++ describeCardNew newCard)
          lift $ putStrLn "可以打出的手牌如下："
          lift . for_ ([0..] `zip` acards) $ \(i, c) -> putStrLn (show i ++ ") " ++ describeCardNew c)
          flip fix () $ \rec _ -> do
            lift $! printf "行动：p) 跳过 0-%d) 打出对应卡牌 -> " (length acards - 1)
            choice :: String <- lift getLine
            case readMaybe choice of
              Just n | 0 <= n && (length acards - 1) >= n ->
                let card = acards !! n
                in if card ^. runCardNew . color & isNothing
                then
                  flip fix () $ \rec' _ -> do
                    lift $ putStr "选择颜色 (r/b/g/y/quit) : "
                    chosenColor :: String <- lift getLine
                    case chosenColor of
                      "r" -> play card desk Red
                      "g" -> play card desk Green
                      "b" -> play card desk Blue
                      "y" -> play card desk Yellow
                      "quit" -> rec ()
                      _   -> do
                        lift $ putStrLn "无效的颜色，请重新输入。"
                        rec' ()
                else play card return undefined
                     | otherwise -> do
                        lift $ putStrLn "超出有效范围，请重新输入。"
                        rec ()
              Nothing ->
                case choice of
                  "p" -> pass desk
                  _   -> do
                    lift $ putStrLn "无效的选项，请重新输入。"
                    rec ()

runGame :: SingleMutable GameDependency r => Eff r GameResult
runGame = do
  ga <- view game <$> getMutable
  displayInitialTurn (ga ^. playedTurn . to head)
  gameLoop
  ga <- view game <$> getMutable
  let (winner : rest) = sortBy (\a b -> compare (length (a^.candidates)) (length (b ^. candidates))) (ga ^. players)
      gr =  GameResult (winner ^. index, sum $ calculateScore <$> rest) ga
  displayGameResult gr
  return gr
  where
    gameLoop = do
      gdep :: GameDependency <- getMutable
      let action = stupidAI
      tr <- do
        report <- playerTurn
        maybe action return report
      currentPlayer <- (\d -> d ^. game . players . to (!! ((d ^. game . playerIndex) - 1))) <$> getMutable
      displayTurnReport tr
      when (null (currentPlayer ^. candidates)) (displayWinner currentPlayer)
      unless (tr == EmptyPile || null (currentPlayer ^. candidates)) $ do
        let lastTurn = gdep ^. game . playedTurn . to head
            keepLastTurn id = Turn (lastTurn ^. playedCard) id (lastTurn ^. order)
            currentOrder card =
              if card ^. runCardPlayed . kind == Spell Reverse
              then reverseOrder (lastTurn ^. order)
              else lastTurn ^. order
            currentTurn =
              case tr of
                PlayerPlayCard id card -> Turn card id (currentOrder card)
                PlayerPass id -> keepLastTurn id
                PlayerDrawCards id _ -> keepLastTurn id
                PlayerDrawAndPlay id _ card -> Turn card id (currentOrder card)
                PlayerSkiped id -> keepLastTurn id
            nextPlayerId = nextId (currentTurn ^. order) (gdep ^. game . players & length) (currentPlayer ^. index)
        modifyMutable (over game (over playedTurn (currentTurn :) . set playerIndex nextPlayerId))
        gameLoop

runGameIO :: IO ()
runGameIO = do
  gen <- RandomGenWrapper <$> getStdGen
  runLift $ do
    (ga, gen') <- runMutable gen (randomGame 4)
    let gdep = GameDependency ga gen'
    runMutable gdep runGame
  return ()
