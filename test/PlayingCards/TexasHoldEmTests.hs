module PlayingCards.TexasHoldEmTests() where

import PlayingCards.TexasHoldEm

import Test.QuickCheck

allTexasHoldEmTests = do
  quickCheck foldingRemovesActivePlayersFromGame
  quickCheck bettingRemovesFromPlayersStack

foldingRemovesActivePlayersFromGame game =
  forAll (activePlayerGen game) $
  \player -> not $ isActive (foldPlayer game player) player
  
bettingRemovesFromPlayersStack game =
  forAll (playerAndBetGen game) $
  \(player, bet) -> let
    betResult = placeBet game player bet
    newPlayerStack = playerStack betResult player
    oldPlayerStack = playerStack game player in
    oldPlayerStack - bet == newPlayerStack
