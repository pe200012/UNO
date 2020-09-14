{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Game where

import           Control.Eff
import           Control.Eff.State.Lazy
import           Control.Eff.Writer.Lazy (Writer, tell)
import           Data.Functor.Identity
import           Data.List               ((\\))
import           GameType
import           Lens.Micro.Platform
import           RandomUtil
import           Shuffle
import Control.Eff.Exception (throwError_, Exc)
import Control.Monad (when)

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

data DrawFlag = DrawFlag Bool Int deriving Show
data GameException = PlayNonexistingCard CardPlayed
  deriving Show

-- | draws n k draw n cards into player's candidates, and then pass these cards to continuation. Will early-exit if there aren't enough cards in pile
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
