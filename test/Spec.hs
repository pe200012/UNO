{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module Main where

import           Control.Eff             (run)
import           Control.Eff.State.Lazy
import           Control.Monad.Identity  (Identity (..))
import           Game                    (DrawFlag (..), draws)
import           GameType
import           Lens.Micro.Platform
import           System.Random           (randomIO, randomRIO)
import           Test.QuickCheck         (Arbitrary (..), Property, chooseAny,
                                          chooseInt, elements, listOf,
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
        color <- chooseAny
        Card <$> elements [Just color, Nothing] <*> arbitrary

instance Arbitrary Player where
    arbitrary = Player <$> (fmap CardNew <$> suchThat arbitrary ((< 30) . length)) <*> arbitrary

newtype SufficientPile = SufficientPile Pile deriving Show

instance Arbitrary SufficientPile where
    arbitrary = chooseInt (4, 108) >>= \n -> SufficientPile <$> (vectorOf n (CardNew <$> arbitrary))

newtype AnyPileTest = AnyPileTest Pile deriving Show

instance Arbitrary AnyPileTest where
    arbitrary = AnyPileTest <$> suchThat (listOf (CardNew <$> arbitrary)) ((<= 108) . length)

prop_drawsCorrectlyDrawNCardsIntoPlayerCandidates :: SufficientPile -> Player -> Property
prop_drawsCorrectlyDrawNCardsIntoPlayerCandidates (SufficientPile pile) player = monadicIO $ do
    num <- randomRIO (1, length pile)
    let (topn, rest) = splitAt num pile
    let (player', pile') = run . runState pile . execState player . evalState (DrawFlag False 0) $ draws num (const $ return (PlayerDrawCards (player ^. index) num))
    return $ (player' ^. candidates & take num) == topn && rest == pile'

prop_drawsTurnOnDrawFlag :: AnyPileTest -> Player -> Property
prop_drawsTurnOnDrawFlag (AnyPileTest pile) player = monadicIO $ do
    num <- randomIO
    let (DrawFlag b _) = run . evalState pile . evalState player . execState (DrawFlag False 0) $ draws num (const $ return (PlayerDrawCards (player ^. index) num))
    return b

prop_drawsCorrectlyHandleNonsufficientPile :: AnyPileTest -> Player -> Property
prop_drawsCorrectlyHandleNonsufficientPile (AnyPileTest pile) player = monadicIO $ do
    num <- randomIO
    let (player', pile') = run . runState pile . execState player . runState (DrawFlag False 0) $ draws num (const $ return (PlayerDrawCards (player ^. index) num))
    return $ (length pile - length pile') == (player' ^. candidates & length) - (player ^. candidates & length)

prop_drawsCalledMultipleTimesAccumulate :: SufficientPile -> Player -> Property
prop_drawsCalledMultipleTimesAccumulate (SufficientPile pile) player = monadicIO $ do
    let (DrawFlag _ n) = run . evalState pile . evalState player . execState (DrawFlag False 0) $ draws half (const $ return (PlayerDrawCards (player ^. index) half)) >> draws half (const $ return (PlayerDrawCards (player ^. index) (half * 2)))
    return $ n == half * 2
    where half = (length pile) `div` 2

main :: IO ()
main = do
    quickCheck prop_drawsCorrectlyDrawNCardsIntoPlayerCandidates
    quickCheck prop_drawsTurnOnDrawFlag
    quickCheck prop_drawsCorrectlyHandleNonsufficientPile
    quickCheck prop_drawsCalledMultipleTimesAccumulate



