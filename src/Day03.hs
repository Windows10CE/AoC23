module Day03 (solve) where

import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List (tails)
import Data.List.Extra ((!?))
import Control.Monad (foldM)
import Control.Monad.Trans.State

data Point = NumberSegment Char | Symbol Char | Period deriving (Eq, Show)

parseInput :: String -> [[Point]]
parseInput input =
  fmap charToPoint <$> lines input
  where
    charToPoint c | c `elem` ['0'..'9'] = NumberSegment c
                  | c == '.' = Period
                  | otherwise = Symbol c

part1 :: [[Point]] -> Int
part1 points =
  let points' = zip (fmap (`zip` [0..]) points) [0..]
  in foldr rowFold 0 points'
  where
    rowFold :: ([(Point, Int)], Int) -> Int -> Int
    rowFold (row, rowi) currentSum =
      currentSum + evalState (foldM actualFold 0 (tails row)) 0
      where
        actualFold :: Int -> [(Point, Int)] -> State Int Int
        actualFold currentSum' currentPoints = do
          currentSkip <- get
          if currentSkip == 0
          then do
            let numbers = takeWhile (isNumber . fst) currentPoints
            if null numbers
            then pure currentSum'
            else do
              put $ length numbers - 1
              if any (\(_, coli) -> (rowi, coli) `elem` accessible) numbers
              then pure $ currentSum' + read (fmap (\(NumberSegment n, _) -> n) numbers)
              else pure currentSum'
          else do
            put $ currentSkip - 1
            pure currentSum'
    accessible :: S.Set (Int, Int)
    accessible =
      let allIndexes = S.toList $ S.fromList [0..length points - 1] `S.cartesianProduct` S.fromList [0..length (head points) - 1]
      in S.fromList $ filter hasSymbolNeighbor allIndexes
    hasSymbolNeighbor :: (Int, Int) -> Bool
    hasSymbolNeighbor (x, y) =
      let offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
      in any (\(ox, oy) -> isSymbol $ fromMaybe Period $ points !? (x + ox) >>= (\inner -> inner !? (y + oy))) offsets
    isSymbol (Symbol _) = True
    isSymbol _ = False
    isNumber (NumberSegment _) = True
    isNumber _ = False

solve :: String -> (String, String)
solve input =
  let input' = parseInput input
  in (show $ part1 input', "")
