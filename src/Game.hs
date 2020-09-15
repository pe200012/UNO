{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}
module Game where

import           Control.Eff
import           Control.Eff.Exception   (Exc, throwError_)
import           Control.Eff.State.Lazy
import           Control.Eff.Writer.Lazy (Writer, tell)
import           Control.Monad           (when)
import           Data.Functor.Identity
import           Data.List               (partition, (\\))
import           Data.Maybe              (fromMaybe)
import           GameType
import           Lens.Micro.Platform
import           RandomUtil
import           Shuffle

nextId :: (Ord a, Num a) => Order -> a -> a -> a
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

-- | @calculateScore p@ Calculate the score of the player from his candidates
calculateScore :: Player -> Score
calculateScore (view candidates -> cards) =
  -- [@Number Card@]: 0-9
  sum (fromEnum . unwrapNumber . view (runCardNew . kind) <$> numberCards) +
  -- [@Spell Card@]: Draw2 Skip Reverse
  length spellCards * 20 +
  -- [@Universal Card@]: Cards with no color(Draw4, Universal)
  length universalCards * 50
  where (numberCards, rest) = partition (\(view (runCardNew . kind) -> k) ->
                                            case k of
                                              Number _ -> True
                                              _        -> False) cards
        (universalCards, spellCards) = partition (\(view (runCardNew . kind) -> k) ->
                                            case k of
                                              Spell Universal -> True
                                              Spell Draw4     -> True
                                              _               -> False) rest
        unwrapNumber (Number n) = n

-- | @initialPile@ Contains 108 cards in total
-- prop> has 76 number cards
-- prop> has 24 spell cards
-- prop> has 8 universal/functional cards
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

data DrawFlag = DrawFlag Bool Int deriving Show
data GameException = PlayNonexistingCard CardPlayed
  deriving Show

-- | @draws n k@ Draw n cards into player's candidates, and then pass these cards to continuation. Will early-exit if there aren't enough cards in pile
draws :: ('[State Pile, State Player, State DrawFlag] <:: r) => Int -> ([CardOnHand] -> Eff r TurnReport) -> Eff r TurnReport
draws num k = do
  pile <- get @Pile
  player <- get @Player
  (DrawFlag b n) <- get @DrawFlag
  if num > length pile
  then do
      modify (over candidates (pile ++))
      put @DrawFlag (DrawFlag True (length pile + n))
      put @Pile []
      return (PlayerDrawCards (player ^. index) (length pile))
  else do
      let (topn, rest) = splitAt  num pile
      modify (over candidates (topn ++))
      put @Pile rest
      put @DrawFlag (DrawFlag True (num + n))
      k topn

-- | @play c@ Output CardPlayed c and then return 'TurnReport' according to 'DrawFlag'. Will throw exception 'PlayNonexsitingCard' on non-existing card
play :: ('[State Player, Writer CardPlayed, State DrawFlag, Exc GameException] <:: r) => CardPlayed -> Eff r TurnReport
play card = do
  (DrawFlag drown n) <- get
  cardsonhand <- view candidates <$> get
  id <- view index <$> get
  when (not (originalCard `elem` cardsonhand)) (throwError_ (PlayNonexistingCard card))
  modify (over candidates (\\ [originalCard]))
  tell card
  if drown
  then return (PlayerDrawAndPlay id n card)
  else return (PlayerPlayCard id card)
  where restore c@(CardKindView (Spell Draw4)) = c & _runCardPlayed & color .~ Nothing & CardNew
        restore c@(CardKindView (Spell Universal)) = c & _runCardPlayed & color .~ Nothing & CardNew
        restore c = c & _runCardPlayed & color %~ (return . runIdentity) & CardNew
        originalCard = restore card

-- | @pass@ Skip current turn
pass :: ('[State Player, State DrawFlag] <:: r) => Eff r TurnReport
pass = do
  (DrawFlag drown n) <- get
  id <- view index <$> get
  if drown
  then return $ PlayerDrawCards id n
  else return $ PlayerPass id

randomGame :: Member (State RandomGenWrapper) r => Int -> Eff r Game
randomGame ps = do
  randomPile <- shuffleE initialPile
  let (randomPlayers, (top:remainingPile)) =
        let (p, c) = distribute randomPile initialCardsOnHand
         in (zipWith Player c [1 .. ps], p)
      len = length randomPlayers
  rc <- randomE
  return $ Game remainingPile [Turn (top & _runCardNew & color %~ (return . fromMaybe rc) & CardPlayed) 0 Clockwise] randomPlayers 1
  where
    initialCardsOnHand = replicate ps []
    distribute pile chs =
      if length (head chs) == 7
        then (pile, chs)
        else let (drawn, pile') = splitAt ps pile
              in distribute pile' (zipWith (:) drawn chs)
