{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications #-}

module Shuffle
  ( shuffle
  , shuffleM
  ) where

import           Control.Monad.Random           ( MonadRandom
                                                , getRandom
                                                , getRandomR
                                                , mkStdGen
                                                )
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Array                     ( Array )
import           Data.Array.Base                ( MArray(unsafeWrite) )
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Foldable                  ( for_ )
import           Data.List                      ( mapAccumL )
import           Data.STRef                     ( newSTRef
                                                , readSTRef
                                                , writeSTRef
                                                )
import           System.Random                  ( RandomGen
                                                , StdGen
                                                , randomR
                                                )

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

shuffleM :: MonadRandom m => [a] -> m [a]
shuffleM xs = do
  gen <- mkStdGen <$> getRandom
  return $ fst (shuffle gen xs)
