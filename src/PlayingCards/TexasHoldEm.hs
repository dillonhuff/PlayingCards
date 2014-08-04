module PlayingCards.TexasHoldEm(
  isActive,
  foldPlayer,
  placeBet,
  playerStack,
  activePlayerGen,
  playerAndBetGen) where

import Data.List as L
import Data.Map as M
import Test.QuickCheck

type Player = Int

data TexasHoldEm = TexH {
  activePlayers :: [Player],
  playerInfo :: Map Player PlayerInfo
  } deriving (Eq, Ord, Show)

data PlayerInfo = PInf {
  stack :: Int
  } deriving (Eq, Ord, Show)

isActive :: TexasHoldEm -> Player -> Bool
isActive game player = elem player $ activePlayers game

foldPlayer :: TexasHoldEm -> Player -> TexasHoldEm
foldPlayer (TexH actives info) player = TexH newActives info
  where
    newActives = L.delete player actives

placeBet :: TexasHoldEm -> Player -> Int -> TexasHoldEm
placeBet game player bet = game

playerStack :: TexasHoldEm -> Player -> Int
playerStack game player = case M.lookup player (playerInfo game) of
  Just pInfo -> stack pInfo
  Nothing -> error $ show player ++ " not present in " ++ show game

maxBet :: TexasHoldEm -> Int
maxBet game = 10000000

instance Arbitrary TexasHoldEm where
  arbitrary = do
    players <- playerListGen
    infoList <- arbitrary
    let infoMap = M.fromList $ zip players infoList
    return $ TexH players infoMap

instance Arbitrary PlayerInfo where
  arbitrary = do
    stackSeed <- arbitrary
    return $ PInf (abs stackSeed)

intInRangeGen = sized $ \n -> choose (0, n)

playerListGen = do
  players <- arbitrary
  return $ nub $ L.map abs players

activePlayerGen game = oneof $ L.map return $ activePlayers game

validBetGen :: TexasHoldEm -> Gen Int
validBetGen game = choose (0, maxBet game)

playerAndBetGen :: TexasHoldEm -> Gen (Player, Int)
playerAndBetGen game = do
  player <- activePlayerGen game
  bet <- validBetGen game
  return $ (player, bet)
