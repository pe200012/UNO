{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import           Control.Eff
import           Control.Eff.Exception   (runError)
import           Control.Eff.State.Lazy
import           Control.Eff.Writer.Lazy (runFirstWriter)
import           Control.Monad.Identity  (Identity (..))
import           Data.Coerce             (coerce)
import           Data.List               (sort)
import           Data.Maybe              (fromJust)
import           Game
import           GameType
import           Lens.Micro.Platform
import           System.Random           (randomIO, randomRIO)
import           Test.QuickCheck         (Arbitrary (..), Property,
                                          arbitrarySizedNatural, chooseAny,
                                          chooseInt, elements, label, listOf,
                                          quickCheck, suchThat, vectorOf)
import           Test.QuickCheck.Monadic (monadicIO)

instance Arbitrary Kind where
  arbitrary = do
    n <- Number <$> chooseAny
    s <- Spell <$> chooseAny
    elements [n, s]

instance Arbitrary (Card Identity) where
  arbitrary = Card <$> (return <$> chooseAny) <*> arbitrary

instance Arbitrary (Card Maybe) where
  arbitrary = do
    normalCard <-
      Card <$> (Just <$> chooseAny) <*>
      (suchThat arbitrary ((&&) . (/= Spell Universal) <*> (/= Spell Draw4)))
    specialCard <-
      Card Nothing <$>
      (suchThat arbitrary ((||) . (== Spell Universal) <*> (== Spell Draw4)))
    elements [normalCard, specialCard]

instance Arbitrary Player where
  arbitrary =
    Player <$> (fmap CardNew <$> suchThat arbitrary ((< 30) . length)) <*>
    arbitrary

newtype SufficientPile =
  SufficientPile Pile
  deriving (Show)

instance Arbitrary SufficientPile where
  arbitrary =
    chooseInt (4, 108) >>= \n ->
      SufficientPile <$> (vectorOf n (CardNew <$> arbitrary))

newtype AnyPileTest =
  AnyPileTest Pile
  deriving (Show)

instance Arbitrary AnyPileTest where
  arbitrary =
    AnyPileTest <$>
    suchThat (listOf (CardNew <$> arbitrary)) ((<= 108) . length)

newtype CapablePlayer =
  CapablePlayer Player
  deriving (Show)

instance Arbitrary CapablePlayer where
  arbitrary =
    CapablePlayer <$>
    (Player <$> suchThat (listOf (CardNew <$> arbitrary)) ((> 0) . length) <*>
     suchThat arbitrarySizedNatural (> 1))

newtype IncapablePlayer =
  IncapablePlayer Player
  deriving (Show)

instance Arbitrary IncapablePlayer where
  arbitrary = IncapablePlayer <$> Player [] <$> arbitrarySizedNatural

newtype AnyPlayer =
  AnyPlayer Player
  deriving (Show)

instance Arbitrary AnyPlayer where
  arbitrary =
    AnyPlayer <$>
    (Player <$> listOf (CardNew <$> arbitrary) <*>
     suchThat arbitrarySizedNatural (> 1))

newtype AllSpellPlayer =
  AllSpellPlayer Player
  deriving (Show)

instance Arbitrary AllSpellPlayer where
  arbitrary = allSpellPlayer <$> arbitrary <*> chooseAny <*> chooseAny <*> chooseAny <*> chooseAny

allSpellPlayer :: Int -> Color -> Color -> Color -> Color -> AllSpellPlayer
allSpellPlayer id c1 c2 c3 c4 = AllSpellPlayer $ Player
  [ CardNew (Card (Just c1) (Spell Skip))
  , CardNew (Card (Just c2) (Spell Draw2))
  , CardNew (Card (Just c3) (Spell Draw4))
  , CardNew (Card (Just c4) (Spell Reverse))] id

newtype TestingGameDependency =
  TestingGameDependency GameDependency

instance Show TestingGameDependency where
  show (TestingGameDependency (GameDependency ga _)) = "TestingGameDependency {GameDependency {_game = " ++ show ga ++", _rgen = <Opaque>}}"

instance Arbitrary TestingGameDependency where


prop_drawsCorrectlyDrawNCardsIntoPlayerCandidates ::
     SufficientPile -> Player -> Property
prop_drawsCorrectlyDrawNCardsIntoPlayerCandidates (SufficientPile pile) player =
  monadicIO $ do
    num <- randomRIO (1, length pile)
    let (topn, rest) = splitAt num pile
    let (player', pile') =
          run . runState pile . execState player . evalState (DrawFlag False 0) $
          draws num (const $ return (PlayerDrawCards (player ^. index) num))
    return . label "drawsCorrectlyDrawNCardsIntoPlayerCandidates" $
      (player' ^. candidates & take num) == topn && rest == pile'

prop_drawsTurnOnDrawFlag :: AnyPileTest -> Player -> Property
prop_drawsTurnOnDrawFlag (AnyPileTest pile) player =
  monadicIO $ do
    num <- randomIO
    let (DrawFlag b _) =
          run . evalState pile . evalState player . execState (DrawFlag False 0) $
          draws num (const $ return (PlayerDrawCards (player ^. index) num))
    return $ label "drawsTurnOnDrawFlag" b

prop_drawsCorrectlyHandleNonsufficientPile :: AnyPileTest -> Player -> Property
prop_drawsCorrectlyHandleNonsufficientPile (AnyPileTest pile) player =
  let num = length pile + 1
      ((report, player'), pile') =
        run . runState pile . runState player . evalState (DrawFlag False 0) $
        draws num (const $ return (PlayerPass (player ^. index)))
   in label "drawsCorrectlyHandleNonsufficientPile" $
      (length pile - length pile') ==
      (player' ^. candidates & length) - (player ^. candidates & length) &&
      report == PlayerDrawCards (player ^. index) (length pile - length pile')

prop_drawsCalledMultipleTimesAccumulate :: SufficientPile -> Player -> Property
prop_drawsCalledMultipleTimesAccumulate (SufficientPile pile) player =
  let (DrawFlag _ n) =
        run . evalState pile . evalState player . execState (DrawFlag False 0) $
        draws half (const $ return (PlayerDrawCards (player ^. index) half)) >>
        draws
          half
          (const $ return (PlayerDrawCards (player ^. index) (half * 2)))
   in label "drawsCalledMultipleTimesAccumulate" $ n == half * 2
  where
    half = (length pile) `div` 2

prop_playCorrectlyPlayCard :: CapablePlayer -> Property
prop_playCorrectlyPlayCard (CapablePlayer player) =
  monadicIO $ do
    card <-
      ((player ^. candidates) !!) <$>
      randomRIO (0, (player ^. candidates & length) - 1)
    color' <- randomIO
    let cardPlayed = randomChooseColor (card ^. runCardNew) color'
        (((Right (PlayerPlayCard _ c), _), Just c'), player') =
          run .
          runState player .
          runFirstWriter @CardPlayed .
          runState (DrawFlag False 0) . runError @GameException $
          play cardPlayed
    return . label "playCorrectlyPlayCard" $
      c == c' &&
      (player' ^. candidates & (card :) & sort) == (player ^. candidates & sort)

prop_playShouldThrowErrorOnNonexistingCard ::
     IncapablePlayer -> Card Identity -> Property
prop_playShouldThrowErrorOnNonexistingCard (IncapablePlayer player) randomCard =
  monadicIO $ do
    let (((Left (PlayNonexistingCard (CardPlayed c)), _), Nothing), player') =
          run .
          runState player .
          runFirstWriter @CardPlayed .
          runState (DrawFlag False 0) . runError @GameException $
          play (CardPlayed randomCard)
    return . label "playShouldThrowErrorOnNonexistingCard" $
      c == randomCard && player' == player

prop_drawsAndPlayOutputPlayerDrawAndPlay ::
     SufficientPile -> AnyPlayer -> Property
prop_drawsAndPlayOutputPlayerDrawAndPlay (SufficientPile pile) (AnyPlayer player) =
  monadicIO $ do
    color' <- randomIO
    let (((Right (PlayerDrawAndPlay _ n c), _), Just c'), player') =
          run .
          runState player .
          runFirstWriter @CardPlayed .
          runState (DrawFlag False 0) . runError @GameException . evalState pile $ do
            draws 1 $ \[card] -> play (randomChooseColor (coerce card) color')
    return . label "drawsAndPlayOutputPlayerDrawAndPlay" $ n == 1 && c == c'

prop_initialPileMeetsRequirements :: Property
prop_initialPileMeetsRequirements =
  label "initialPileMeetsRequirements" $
  length initialPile == 108 &&
  length (filter isNumber initialPile) == 76 &&
  length (filter isSpell initialPile) == 24 &&
  length (filter isUniversal initialPile) == 8
  where
    isNumber (CardNewKindView (Number _)) = True
    isNumber _                            = False
    isSpell (CardNewKindView (Spell Draw2))   = True
    isSpell (CardNewKindView (Spell Skip))    = True
    isSpell (CardNewKindView (Spell Reverse)) = True
    isSpell _                                 = False
    isUniversal (CardNewKindView (Spell Draw4))     = True
    isUniversal (CardNewKindView (Spell Universal)) = True
    isUniversal _                                   = False

randomChooseColor :: Card Maybe -> Color -> CardPlayed
randomChooseColor c@(view color -> Nothing) color' =
  c & color .~ (return color') & CardPlayed
randomChooseColor c _ = c & color %~ (return . fromJust) & CardPlayed

main :: IO ()
main = do
  quickCheck prop_drawsCorrectlyDrawNCardsIntoPlayerCandidates
  quickCheck prop_drawsTurnOnDrawFlag
  quickCheck prop_drawsCorrectlyHandleNonsufficientPile
  quickCheck prop_drawsCalledMultipleTimesAccumulate
  quickCheck prop_playCorrectlyPlayCard
  quickCheck prop_playShouldThrowErrorOnNonexistingCard
  quickCheck prop_drawsAndPlayOutputPlayerDrawAndPlay
  quickCheck prop_initialPileMeetsRequirements
