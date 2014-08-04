module PlayingCards.TexasHoldEmTests() where

import PlayingCards.TexasHoldEm

import Test.QuickCheck

allTexasHoldEmTests = do
  quickCheck foldingRemovesActivePlayersFromGame

foldingRemovesActivePlayersFromGame game =
  forAll (activePlayerGen game) $
  \player -> not $ isActive (foldPlayer game player) player
  
