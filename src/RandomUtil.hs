{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TypeOperators    #-}

module RandomUtil
( randomM
, randomRM
, randomE
, randomRE
) where

import qualified Control.Eff            as Eff
import qualified Control.Eff.State.Lazy as Eff.State
import           Control.Monad.State
import           GameType
import           System.Random

randomM :: (MonadState g m, Random a, RandomGen g) => m a
randomM = do
  g <- get
  let (a, g') = random g
  put g'
  return a

randomRM :: (MonadState g m, Random a, RandomGen g) => (a, a) -> m a
randomRM x = do
  g <- get
  let (a, g') = randomR x g
  put g'
  return a

randomE :: (Eff.Member (Eff.State.State RandomGenWrapper) r, Random a) => Eff.Eff r a
randomE = do
  (RandomGenWrapper g) <- Eff.State.get
  let (a, g') = random g
  Eff.State.put (RandomGenWrapper g')
  return a

randomRE :: (Eff.Member (Eff.State.State RandomGenWrapper) r, Random a) => (a, a) -> Eff.Eff r a
randomRE x = do
  (RandomGenWrapper g) <- Eff.State.get
  let (a, g') = randomR x g
  Eff.State.put (RandomGenWrapper g')
  return a
