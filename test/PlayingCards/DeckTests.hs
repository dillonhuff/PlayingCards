module PlayingCards.DeckTests() where

import PlayingCards.Deck
import PlayingCards.TestUtils

import Test.QuickCheck

allDeckTests = do
  testFunction cardsLeft cardsLeftCases
  quickCheck dealingReducesDeckSize
  quickCheck dealtCardNotInDeck
  quickCheck dealtHandsSizesCorrect
  quickCheck dealtCardsNotStillInDeck

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

dealtHandsSizesCorrect d m n = case m < cardsLeft d &&
                                    n < cardsLeft d &&
                                    cardsLeft d >= m*n &&
                                    m >= 0 &&
                                    n >= 0 of
  False -> True
  True -> length dealtCards == m*n &&
          length hands == m &&
          cardsLeft deckAfterDealing == cardsLeft d - m*n
  where
    dealRes = dealHands m n d
    hands = fst dealRes
    dealtCards = concat $ hands
    deckAfterDealing = snd dealRes

dealtCardsNotStillInDeck d m n = case m < cardsLeft d &&
                                    n < cardsLeft d &&
                                    cardsLeft d >= m*n &&
                                    m >= 0 &&
                                    n >= 0 of
  False -> True
  True -> not $ or $ map (\c -> containsCard c restOfDeck) dealtCards
  where
    dealRes = dealHands m n d
    dealtCards = concat $ fst dealRes
    restOfDeck = snd dealRes
