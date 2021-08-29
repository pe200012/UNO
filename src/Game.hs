{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Game
    ( draws
    , play
    , evaluateTurn
    ) where

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Random           ( MonadRandom
                                                , getRandom
                                                )
import           Control.Monad.State            ( MonadState
                                                , StateT
                                                , evalStateT
                                                , gets
                                                , modify
                                                , runStateT
                                                )
import           Control.Monad.Writer           ( MonadWriter
                                                , Writer
                                                , WriterT
                                                , runWriterT
                                                , tell
                                                )
import           Data.Functor.Identity          ( Identity(runIdentity) )
import           Data.List                      ( (\\)
                                                , partition
                                                )
import           Data.Maybe                     ( fromMaybe )
import           GameType
import           Lens.Micro.Platform
import           Shuffle

nextId :: (Ord a, Num a) => Order -> a -> a -> a
nextId Clockwise max id | id <= 1   = max
                        | otherwise = id - 1
nextId Counterclockwise max id | id >= max = 1
                               | otherwise = id + 1

describeCard :: CardPlayed -> String
describeCard = ((++) . describeColor . view (color . to runIdentity) <*> (":" ++) . describeKind . view kind) . view runCardPlayed
  where
    describeColor Red    = "红"
    describeColor Blue   = "蓝"
    describeColor Green  = "绿"
    describeColor Yellow = "黄"
    describeNumber = show . fromEnum @NumberCard
    describeKind (Number n        ) = "数字牌" ++ describeNumber n
    describeKind (Spell  Universal) = "万能牌"
    describeKind (Spell  Draw2    ) = "抽两张"
    describeKind (Spell  Draw4    ) = "抽四张"
    describeKind (Spell  Skip     ) = "跳过"
    describeKind (Spell  Reverse  ) = "反转"

describeCardNew :: CardNew -> String
describeCardNew = ((++) . describeColor . view color <*> (":" ++) . describeKind . view kind) . view runCardNew
  where
    describeColor Nothing       = "无色"
    describeColor (Just Red   ) = "红"
    describeColor (Just Blue  ) = "蓝"
    describeColor (Just Green ) = "绿"
    describeColor (Just Yellow) = "黄"
    describeNumber = show . fromEnum @NumberCard
    describeKind (Number n        ) = "数字牌" ++ describeNumber n
    describeKind (Spell  Universal) = "万能牌"
    describeKind (Spell  Draw2    ) = "抽两张"
    describeKind (Spell  Draw4    ) = "抽四张"
    describeKind (Spell  Skip     ) = "跳过"
    describeKind (Spell  Reverse  ) = "反转"

displayTurnReport :: MonadIO m => TurnReport -> m ()
displayTurnReport (PlayerPlayCard id card       ) = liftIO $ putStrLn ("玩家 " ++ show id ++ " 打出了" ++ describeCard card)
displayTurnReport (PlayerPass id                ) = liftIO $ putStrLn ("玩家 " ++ show id ++ " 选择跳过")
displayTurnReport (PlayerDrawCards id num       ) = liftIO $ putStrLn ("玩家 " ++ show id ++ " 抽了 " ++ show num ++ " 张卡")
displayTurnReport (PlayerDrawAndPlay id num card) = liftIO $ putStrLn ("玩家 " ++ show id ++ " 抽了 " ++ show num ++ " 张卡并打出" ++ describeCard card)
displayTurnReport (PlayerSkipped id             ) = liftIO $ putStrLn ("玩家 " ++ show id ++ " 被跳过")
displayTurnReport EmptyPile                       = liftIO $ putStrLn "牌堆已空"

displayGameResult :: MonadIO m => GameResult -> m ()
displayGameResult r = liftIO $ putStrLn ("终局，胜利者为玩家 " ++ show (r ^. to winner . _1) ++ " ，得分 " ++ show (r ^. to winner . _2) ++ " 。")

displayWinner :: MonadIO m => Player -> m ()
displayWinner p = liftIO $ putStrLn ("玩家 " ++ show (p ^. index) ++ " 率先打出所有手牌！")

displayInitialTurn :: MonadIO m => Turn -> m ()
displayInitialTurn t = liftIO $ putStrLn ("开局，场牌为" ++ describeCard (t ^. playedCard))

reverseOrder :: Order -> Order
reverseOrder Counterclockwise = Clockwise
reverseOrder Clockwise        = Counterclockwise

-- | @calculateScore p@ Calculate the score of the player from his candidates
calculateScore :: Player -> Score
calculateScore (view candidates -> cards) =
  -- [@Number Card@]: 0-9
    sum (fromEnum . unwrapNumber . view (runCardNew . kind) <$> numberCards)
        +
  -- [@Spell Card@]: Draw2 Skip Reverse
          length spellCards
        * 20
        +
  -- [@Universal Card@]: Cards with no color(Draw4, Universal)
          length universalCards
        * 50
  where
    (numberCards, rest) = partition
        (\(view (runCardNew . kind) -> k) -> case k of
            Number _ -> True
            _        -> False
        )
        cards
    (universalCards, spellCards) = partition
        (\(view (runCardNew . kind) -> k) -> case k of
            Spell Universal -> True
            Spell Draw4     -> True
            _               -> False
        )
        rest
    unwrapNumber (Number n) = n

-- | @initialPile@ Contains 108 cards in total
--
-- prop:
--   - has 76 number cards
--
--   - has 24 spell cards
--
--   - has 8 universal/functional cards
initialPile :: Pile
initialPile =
    CardNew
        <$> concatMap (replicate 4) [ Card Nothing (Spell s) | s <- [Draw4, Universal] ]
        ++  concatMap (replicate 2) [ Card (Just c) (Spell s) | s <- [Draw2, Skip, Reverse], c <- fullColor ]
        ++  concatMap (if' . isZero . _kind <*> return <*> replicate 2) [ Card (Just c) (Number n) | c <- fullColor, n <- [Zero .. Nine] ]
  where
    fullColor = [Red .. Yellow]
    if' b a c = if b then a else c
    isZero (Number Zero) = True
    isZero _             = False

data DrawFlag = DrawFlag Bool Int
    deriving Show
data GameException = PlayNonexistingCard CardPlayed
    deriving Show

data DrawPayload = DrawPayload
    { _drawPayloadPile     :: Pile
    , _drawPayloadPlayer   :: Player
    , _drawPayloadDrawFlag :: DrawFlag
    }

makeLenses ''DrawPayload

-- | @draws n k@ Draw n cards into player's candidates, and then pass these cards to continuation. Will early-exit if there aren't enough cards in pile
draws :: MonadState DrawPayload m => Int -> ([CardOnHand] -> m TurnReport) -> m TurnReport
draws num k = do
    pile           <- gets (view drawPayloadPile)
    player         <- gets (view drawPayloadPlayer)
    (DrawFlag b n) <- gets (view drawPayloadDrawFlag)
    if num > length pile
        then do
            modify (over (drawPayloadPlayer . candidates) (pile ++))
            modify (set drawPayloadDrawFlag (DrawFlag True (length pile + n)))
            modify (set drawPayloadPile [])
            return (PlayerDrawCards (player ^. index) (length pile))
        else do
            let (topn, rest) = splitAt num pile
            modify (over (drawPayloadPlayer . candidates) (topn ++))
            modify (set drawPayloadPile rest)
            modify (set drawPayloadDrawFlag (DrawFlag True (num + n)))
            k topn

data PlayPayload = PlayPayload
    { _playPayloadPlayer   :: Player
    , _playPayloadDrawFlag :: DrawFlag
    }

makeLenses ''PlayPayload

-- | @play c@ Output CardPlayed c and then return 'TurnReport' according to 'DrawFlag'. Will throw exception 'PlayNonexsitingCard' on non-existing card
play :: (MonadState PlayPayload m, MonadWriter [CardPlayed] m, MonadError GameException m) => CardPlayed -> m TurnReport
play card = do
    (DrawFlag drown n) <- gets (view playPayloadDrawFlag)
    cardsonhand        <- gets (view (playPayloadPlayer . candidates))
    id                 <- gets (view (playPayloadPlayer . index))
    unless (originalCard `elem` cardsonhand) (throwError (PlayNonexistingCard card))
    modify (over (playPayloadPlayer . candidates) (\\ [originalCard]))
    tell [card]
    if drown then return (PlayerDrawAndPlay id n card) else return (PlayerPlayCard id card)
  where
    restore c@(CardKindView (Spell Draw4)) = c & _runCardPlayed & color .~ Nothing & CardNew
    restore c@(CardKindView (Spell Universal)) = c & _runCardPlayed & color .~ Nothing & CardNew
    restore c = c & _runCardPlayed & color %~ return . runIdentity & CardNew
    originalCard = restore card

data PassPayload = PassPayload
    { _passPayloadPlayer   :: Player
    , _passPayloadDrawFlag :: DrawFlag
    }

makeLenses ''PassPayload

-- | @pass@ Skip current turn
pass :: MonadState PassPayload m => m TurnReport
pass = do
    (DrawFlag drown n) <- gets (view passPayloadDrawFlag)
    id                 <- gets (view (passPayloadPlayer . index))
    if drown then return $ PlayerDrawCards id n else return $ PlayerPass id


randomGame :: MonadRandom m => Int -> m Game
randomGame ps = do
    randomPile <- shuffleM initialPile
    let (randomPlayers, top : remainingPile) = let (p, c) = distribute randomPile initialCardsOnHand in (zipWith Player c [1 .. ps], p)
        len = length randomPlayers
    rc <- getRandom
    return $ Game remainingPile [Turn (top & _runCardNew & color %~ return . fromMaybe rc & CardPlayed) 0 Clockwise] randomPlayers 1
  where
    initialCardsOnHand = replicate ps []
    distribute pile chs = if length (head chs) == 7 then (pile, chs) else let (drawn, pile') = splitAt ps pile in distribute pile' (zipWith (:) drawn chs)


evaluateTurn :: MonadState GameDependency m => WriterT [CardPlayed] (StateT DrawPayload (ExceptT a m)) TurnReport -> m TurnReport
evaluateTurn action = do
    ga <- gets (view game)
    let lastTurn    = ga ^. playedTurn . to head
        nextToMe    = lastTurn ^. playingPlayer == 0 || nextId (lastTurn ^. order) (lastTurn ^. playingPlayer) (ga ^. players & length) == ga ^. playerIndex
        pl          = (ga ^. players) !! (ga ^. playerIndex)
        pi          = ga ^. pile
        initialFlag = DrawFlag False 0
        max         = ga ^. players & length
    case lastTurn of
        SkipCard | nextToMe          -> return (PlayerSkipped (ga ^. playerIndex))
        DrawCards _ Draw2 | nextToMe -> do
            x <- runStateT (draws 2 (const (return (PlayerDrawCards (pl ^. index) 2)))) (DrawPayload pi pl initialFlag)
            let (tr, DrawPayload pi' pl' _) = x
            modify
                (over
                    game
                    (set (players . ix (pl ^. index)) pl' . set pile pi' . set playerIndex (nextId (lastTurn ^. order) max (pl ^. index)) . over
                        playedTurn
                        ((:) . head <*> id)
                    )
                )
            return tr
        DrawCards _ Draw4 | nextToMe -> do
            x <- runStateT (draws 2 (const (return (PlayerDrawCards (pl ^. index) 4)))) (DrawPayload pi pl initialFlag)
            let (tr, DrawPayload pi' pl' _) = x
            modify
                (over
                    game
                    (set (players . ix (pl ^. index)) pl' . set pile pi' . set playerIndex (nextId (lastTurn ^. order) max (pl ^. index)) . over
                        playedTurn
                        ((:) . head <*> id)
                    )
                )
            return tr
        _ -> do
            x <- runExceptT (runStateT (runWriterT action) (DrawPayload pi pl initialFlag))
            let (Right ((tr, cards), DrawPayload pi' pl' _)) = x
                newOrder = if odd $ length (filter ((== Spell Reverse) . view (runCardPlayed . kind)) cards)
                    then reverseOrder (lastTurn ^. order)
                    else lastTurn ^. order
            modify
                (over
                    game
                    (set (players . ix (pl ^. index)) pl' . set pile pi' . set playerIndex (nextId newOrder max (pl ^. index)) . over playedTurn
                                                                                                                                      ((:) . head <*> id)
                    )
                )
            return tr
