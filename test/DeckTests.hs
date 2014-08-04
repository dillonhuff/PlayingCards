module DeckTests() where

import Deck
import TestUtils

allDeckTests = do
  testFunction cardsLeft cardsLeftCases

cardsLeftCases =
  [(newDeckNoJokers, 52),
   (newDeck, 54)]
