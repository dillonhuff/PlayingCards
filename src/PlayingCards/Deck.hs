module PlayingCards.Deck(
  card,
  spades, hearts, clubs, diamonds,
  ace, king, queen, jack, ten, nine, eight,
  seven, six, five, four, three, two,
  dealCard,
  dealHands,
  containsCard,
  newDeck,
  shuffleDeck,
  cardsLeft) where

import Control.Monad
import Control.Monad.Random
import Data.List
import System.Random.Shuffle
import Test.QuickCheck

data Deck = Deck [Card]
            deriving (Eq, Ord, Show)

type Hand = [Card]

data Card = Flat Rank Suit
            deriving (Eq, Ord)

instance Show Card where
  show (Flat r s) = show r ++ show s

card = Flat

data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
            deriving (Enum, Eq, Ord, Bounded)

instance Show Suit where
  show Spades = "s"
  show Hearts = "h"
  show Diamonds = "d"
  show Clubs = "c"

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
            deriving (Enum, Eq, Ord, Bounded)

instance Show Rank where
  show Ace = "A"
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show Ten = "10"
  show Nine = "9"
  show Eight = "8"
  show Seven = "7"
  show Six = "6"
  show Five = "5"
  show Four = "4"
  show Three = "3"
  show Two = "2"

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
newDeckCards = [Flat r s | r <- [minBound..maxBound], s <- [minBound..maxBound]]

newDeck :: Deck
newDeck = Deck newDeckCards

shuffleDeck :: (MonadRandom m) => Deck -> m Deck
shuffleDeck (Deck d) = liftM Deck (shuffleM d)

cardsLeft :: Deck -> Int
cardsLeft (Deck d) = length d

dealCard :: Deck -> (Card, Deck)
dealCard (Deck []) = error "Attempt to deal from empty deck"
dealCard (Deck (c:rest)) = (c, Deck rest)

dealHands :: Int -> Int -> Deck -> ([Hand], Deck)
dealHands numHands cardsPerHand (Deck d) = case notEnoughCards of
  False -> (hands, remainingDeck)
  True -> error "Not enough cards to deal"
  where
    totalCardsInHands = numHands * cardsPerHand
    totalCards = cardsLeft (Deck d)
    notEnoughCards = (numHands > totalCards && cardsPerHand > 0) ||
                  (cardsPerHand > totalCards && numHands > 0) ||
                  totalCardsInHands > totalCards
    handCardsAndLeftovers = splitAt totalCardsInHands d
    remainingDeck = Deck $ snd handCardsAndLeftovers
    handCards = fst handCardsAndLeftovers
    hands = map (take cardsPerHand) $ take numHands $ iterate (drop cardsPerHand) handCards

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
