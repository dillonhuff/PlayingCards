module Deck(
  newDeck,
  newDeckNoJokers,
  cardsLeft) where

type Deck = [Card]

data Card = Flat Suit Rank
          | Joker
            deriving (Eq, Ord, Show)

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
            deriving (Enum, Eq, Ord, Bounded, Show)

data Rank = Ace
          | King
          | Queen
          | Jack
          | Ten
          | Nine
          | Eight
          | Seven
          | Six
          | Five
          | Four
          | Three
          | Two
            deriving (Enum, Eq, Ord, Bounded, Show)

newDeckNoJokers :: Deck
newDeckNoJokers = [Flat s r | s <- [minBound..maxBound], r <- [minBound..maxBound]]

newDeck :: Deck
newDeck = newDeckNoJokers ++ [Joker, Joker]

cardsLeft :: Deck -> Int
cardsLeft = length
