{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Shuffle
  ( shuffle
  , shuffleM
  , shuffleE
  )
where

import qualified Control.Eff            as Eff
import qualified Control.Eff.State.Lazy as Eff.State
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Array             (Array)
import           Data.Array.Base        (MArray (unsafeWrite))
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Foldable          (for_)
import           Data.List              (mapAccumL)
import           Data.STRef             (newSTRef, readSTRef, writeSTRef)
import           GameType
import           System.Random          (RandomGen, StdGen, randomR)

shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle g xs = runST $ do
  let len = length xs
  as  <- newArray_ (0, len - 1) :: ST s (STArray s Int a)
  gen <- newSTRef g
  for_ [0 .. len - 1] $ \i -> unsafeWrite as i (xs !! i)
  for_ [0 .. len - 2] $ \i -> do
    currentGen <- readSTRef gen
    let (j, g') = randomR (i, len - 1) currentGen
    ai <- readArray as i
    aj <- readArray as j
    unsafeWrite as i aj
    unsafeWrite as j ai
    writeSTRef gen g'
  g'  <- readSTRef gen
  xs' <- getElems as
  return (xs', g')

shuffleM :: (MonadState g m, RandomGen g) => [a] -> m [a]
shuffleM xs = do
  g <- get
  let (xs', g') = shuffle g xs
  put g'
  return xs'

shuffleE :: (Eff.Member (Eff.State.State RandomGenWrapper) r) => [a] -> Eff.Eff r [a]
shuffleE xs = do
  (RandomGenWrapper g) :: RandomGenWrapper <- Eff.State.get
  let (xs', g') = shuffle g xs
  Eff.State.put (RandomGenWrapper g')
  return xs'
