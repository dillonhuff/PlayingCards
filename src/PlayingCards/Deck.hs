module PlayingCards.Deck(
  card,
  spades, hearts, clubs, diamonds,
  ace, king, queen, jack, ten, nine, eight,
  seven, six, five, four, three, two,
  dealCard,
  containsCard,
  newDeck,
  cardsLeft) where

import Data.List
import Test.QuickCheck

data Deck = Deck [Card]
            deriving (Eq, Ord, Show)

data Card = Flat Suit Rank
            deriving (Eq, Ord, Show)

card = Flat

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
            deriving (Enum, Eq, Ord, Bounded, Show)

spades = Spades
hearts = Hearts
diamonds = Diamonds
clubs = Clubs

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

ace = Ace
king = King
queen = Queen
jack = Jack
ten = Ten
nine = Nine
eight = Eight
seven = Seven
six = Six
five = Five
four = Four
three = Three
two = Two

newDeckCards :: [Card]
newDeckCards = [Flat s r | s <- [minBound..maxBound], r <- [minBound..maxBound]]

newDeck :: Deck
newDeck = Deck newDeckCards

cardsLeft :: Deck -> Int
cardsLeft (Deck d) = length d

dealCard :: Deck -> (Card, Deck)
dealCard (Deck []) = error "Attempt to deal from empty deck"
dealCard (Deck (c:rest)) = (c, Deck rest)

containsCard :: Card -> Deck -> Bool
containsCard c (Deck d) = elem c d

instance Arbitrary Suit where
  arbitrary = elements [Spades, Hearts, Clubs, Diamonds]

instance Arbitrary Rank where
  arbitrary =
    elements [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]

instance Arbitrary Card where
  arbitrary = do
    suit <- arbitrary
    rank <- arbitrary
    return $ Flat suit rank

instance Arbitrary Deck where
  arbitrary = do
    cardsToRemove <- arbitrary
    return $ Deck (newDeckCards \\ cardsToRemove)
