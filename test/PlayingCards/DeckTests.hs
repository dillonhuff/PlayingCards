module PlayingCards.DeckTests() where

import PlayingCards.Deck
import PlayingCards.TestUtils

import Test.QuickCheck

allDeckTests = do
  testFunction cardsLeft cardsLeftCases
  quickCheck dealingReducesDeckSize
  quickCheck dealtCardNotInDeck

cardsLeftCases =
   [(newDeck, 52)]

dealingReducesDeckSize d = case cardsLeft d of
  0 -> True
  _ -> cardsLeft (snd (dealCard d)) == (cardsLeft d) - 1

dealtCardNotInDeck d = case cardsLeft d of
  0 -> True
  _ -> not $ containsCard (fst res) (snd res)
  where
    res = dealCard d
