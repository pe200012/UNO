{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module GameType where

import           Data.Functor.Identity          ( Identity(Identity) )
import           Lens.Micro.Platform            ( makeLenses
                                                , view
                                                )
import           System.Random                  ( Random(..)
                                                , randomR
                                                )

data Color
  = Red
  | Blue
  | Green
  | Yellow
  deriving (Show, Eq, Enum, Ord)

instance Random Color where
  random = randomR (Red, Yellow)
  randomR (lo, hi) g = (colors !! index, g')
   where
    (index, g') = randomR (0, length colors - 1) g
    colors      = [lo .. hi]

data SpellCard
  = Draw2
  | Draw4
  | Skip
  | Reverse
  | Universal
  deriving (Show, Eq, Ord, Enum)

instance Random SpellCard where
  random = randomR (Draw2, Universal)
  randomR (lo, hi) g = (spells !! index, g')
   where
    (index, g') = randomR (0, length spells - 1) g
    spells      = [lo .. hi]

data NumberCard
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Show, Eq, Enum, Ord)

instance Random NumberCard where
  random = randomR (Zero, Nine)
  randomR (lo, hi) g = (numbers !! index, g')
   where
    (index, g') = randomR (0, length numbers - 1) g
    numbers     = [lo .. hi]

data Kind
  = Number NumberCard
  | Spell SpellCard
  deriving (Show, Eq, Ord)

data Order
  = Clockwise
  | Counterclockwise
  deriving (Show, Eq, Enum)

type PlayerIndex = Int -- count from zero

data Card m = Card
  { _color :: m Color
  , _kind  :: Kind
  }

makeLenses ''Card

deriving instance (Eq (m Color)) => Eq (Card m)

deriving instance (Show (m Color)) => Show (Card m)

deriving instance (Ord (m Color)) => Ord (Card m)

newtype CardNew =
  CardNew
    { _runCardNew :: Card Maybe
    }
  deriving (Show, Eq, Ord)

makeLenses ''CardNew

newtype CardPlayed =
  CardPlayed
    { _runCardPlayed :: Card Identity
    }
  deriving (Show, Eq, Ord)

makeLenses ''CardPlayed

type CardOnHand = CardNew

data Player = Player
  { _candidates :: [CardOnHand]
  , _index      :: PlayerIndex
  }
  deriving (Show, Eq)

makeLenses ''Player

data Turn = Turn
  { _playedCard    :: CardPlayed
  , _playingPlayer :: PlayerIndex
  , _order         :: Order
  }
  deriving Show

makeLenses ''Turn

data TurnReport = PlayerPlayCard PlayerIndex CardPlayed
                | PlayerPass PlayerIndex
                | PlayerDrawCards PlayerIndex Int
                | PlayerDrawAndPlay PlayerIndex Int CardPlayed
                | PlayerSkipped PlayerIndex
                | EmptyPile
      deriving (Show, Eq)

type Pile = [CardNew]

data Game = Game
  { _pile        :: Pile
  , _playedTurn  :: [Turn]
  , _players     :: [Player]
  , _playerIndex :: PlayerIndex
  }
  deriving Show

makeLenses ''Game

type Score = Int

data GameResult = GameResult
  { winner    :: (PlayerIndex, Score)
  , finalGame :: Game
  }
  deriving Show

data GameDependency = GameDependency
  { _game :: Game
  }

makeLenses ''GameDependency

pattern CardKindView :: Kind -> CardPlayed
pattern CardKindView k <- (view (runCardPlayed . kind) -> k)

pattern CardNewKindView :: Kind -> CardNew
pattern CardNewKindView k <- (view (runCardNew . kind) -> k)

pattern DrawCards :: Color -> SpellCard -> Turn
pattern DrawCards c s <-
        (view (playedCard . runCardPlayed) ->
           Card{_color = Identity c, _kind = Spell s})

pattern SkipCard :: Turn
pattern SkipCard <- (view (playedCard . runCardPlayed) -> Card{_kind = Spell Skip})

pattern ReverseCard :: Turn
pattern ReverseCard <- (view (playedCard . runCardPlayed) -> Card{_kind = Spell Reverse})

pattern NormalCard :: Color -> Kind -> Turn
pattern NormalCard c k <-
        (view (playedCard . runCardPlayed) ->
           Card{_color = Identity c, _kind = k})
