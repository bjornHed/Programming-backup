module Tiles where

import Test.QuickCheck
import System.Random

data Tile = Tile { suit :: Suit, rank :: Rank }
  deriving (Eq, Show)

data Suit = Bamboo | Dots | Characters | Dragon | Wind
  deriving (Eq, Show)

data Rank = Numeric Int | Green | White | Red | East | South | West | North
  deriving (Eq, Show)

data Hand = Empty | Add Hand Tile
  deriving (Eq, Show)

-- | Adds a tile to a hand
addTile :: Hand -> Tile -> Hand
addTile Empty tile     = Add  Empty tile
addTile (Add h t) tile = Add (Add h t) tile

-- | Recursivle add a tile at a time from one hand to the top
-- | of another hand.
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand = hand
(<+) (Add h t) hand = (Add (h<+hand) t)

-- | Gives all the tiles in the wall
fullDeck :: Hand
fullDeck = undefined

oneOfEach :: Hand
oneOfEach = undefined

-- | Gives all the tiles of a specific suit
fullSuit :: Suit -> Hand
fullSuit Dragon =
  (foldr (<+) Empty [(Add Empty (Tile Dragon i)) | i <- [Red, Green, White]])
fullSuit Wind   =
  (foldr (<+) Empty [(Add Empty (Tile Wind i)) | i <- [East,South,West,North]])
fullSuit suit =
  (foldr (<+) Empty [(Add Empty (Tile suit (Numeric n))) | n <- [1..9]])

-- | Code for generating tiles used by QuickCheck
{-instance Arbitrary NumTile where
  arbitrary = do
    suit <- arbitrary
    rank <- oneof [return i | i <- [1..9]]
    return $ NumTile suit rank

instance Arbitrary Tile where
  arbitrary = do
    suit <- arbitrary
    case suit of
      Wind -> -}

--instance Arbitrary Tile where
  --arbitrary = frequency[(144, return (Tile ()))]

{-instance Arbitrary HonorTile where
  arbitrary = frequency [(16, oneof[return (HonorTile (Wind i)) | i <- [East,South,West,North]])
                        ,(12, oneof[return (HonorTile (Dragon i)) | i <- [Green,White,Red]])]

instance Arbitrary Suit where
  arbitrary = oneof [return Bamboo, return Dots, return Characters]-}
