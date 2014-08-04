module PlayingCards.TexasHoldEm(
  isActive,
  foldPlayer,
  activePlayerGen) where

import Data.List
import Test.QuickCheck

type Player = Int

data TexasHoldEm = TexH {
  activePlayers :: [Player]
  } deriving (Eq, Ord, Show)
  

isActive :: TexasHoldEm -> Player -> Bool
isActive game player = elem player $ activePlayers game

foldPlayer :: TexasHoldEm -> Player -> TexasHoldEm
foldPlayer game player = TexH activesWithoutPlayer
  where
    activesWithoutPlayer = delete player $ activePlayers game

instance Arbitrary TexasHoldEm where
  arbitrary = do
    players <- playerListGen
    return $ TexH players

playerListGen = do
  players <- arbitrary
  return $ nub $ map abs players

activePlayerGen game = oneof $ map return $ activePlayers game
