module Day03 (solve) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (tails)
import Data.List.Extra ((!?))
import Control.Monad (foldM)
import Control.Monad.Trans.State

data Point = NumberSegment Char | Symbol Char | Period deriving (Eq, Show)

unwrapNumber :: Point -> Char
unwrapNumber (NumberSegment c) = c
unwrapNumber _ = error "Failed to unwrap number segment"

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
              then pure $ currentSum' + read (fmap (unwrapNumber . fst) numbers)
              else pure currentSum'
          else do
            put $ currentSkip - 1
            pure currentSum'
    accessible :: S.Set (Int, Int)
    accessible =
      let allIndexes = S.fromList [0..length points - 1] `S.cartesianProduct` S.fromList [0..length (head points) - 1]
      in S.filter hasSymbolNeighbor allIndexes
    hasSymbolNeighbor :: (Int, Int) -> Bool
    hasSymbolNeighbor (x, y) =
      let offsetPoints = (\(ox, oy) -> (x + ox, y + oy)) <$> [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
      in any (\(x', y') -> isSymbol $ fromMaybe Period $ points !? x' >>= (!? y')) offsetPoints
    isSymbol (Symbol _) = True
    isSymbol _ = False
    isNumber (NumberSegment _) = True
    isNumber _ = False

part2 :: [[Point]] -> Int
part2 points =
  let points' = zip (fmap (`zip` [0..]) points) [0..]
      finalMap = foldr rowFold M.empty points'
      validGearPartPairs = filter (\gears -> length gears == 2) (snd <$> M.toList finalMap)
  in sum $ product <$> validGearPartPairs
  where
    rowFold :: ([(Point, Int)], Int) -> M.Map (Int, Int) [Int] -> M.Map (Int, Int) [Int]
    rowFold (row, rowi) currentMap =
      evalState (foldM actualFold currentMap (tails row)) 0
      where
        actualFold :: M.Map (Int, Int) [Int] -> [(Point, Int)] -> State Int (M.Map (Int, Int) [Int])
        actualFold currentMap' currentPoints = do
          currentSkip <- get
          if currentSkip == 0
          then do
            let numbers = takeWhile (isNumber . fst) currentPoints
            if null numbers
            then pure currentMap'
            else do
              put $ length numbers - 1
              let num = read $ unwrapNumber . fst <$> numbers
              pure $ foldr (`insertNum` num) currentMap' (S.toList $ S.fromList $ concatMap (\(_, coli) -> gearNeighbors M.! (rowi, coli)) numbers)
          else do
            put $ currentSkip - 1
            pure currentMap'
    insertNum :: (Int, Int) -> Int -> M.Map (Int, Int) [Int] -> M.Map (Int, Int) [Int]
    insertNum point num = M.alter (\case { Just current -> Just $ num : current; Nothing -> Just [num] }) point
    gearNeighbors :: M.Map (Int, Int) [(Int, Int)]
    gearNeighbors =
      let allIndexes = S.fromList [0..length points - 1] `S.cartesianProduct` S.fromList [0..length (head points) - 1]
      in M.fromList $ (\i -> (i, getGearNeighbors i)) <$> S.toList allIndexes
    getGearNeighbors :: (Int, Int) -> [(Int, Int)]
    getGearNeighbors (x, y) =
      let offsetPoints = (\(ox, oy) -> (x + ox, y + oy)) <$> [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
      in filter (\(x', y') -> isGear $ fromMaybe Period $ points !? x' >>= (!? y')) offsetPoints
    isGear (Symbol '*') = True
    isGear _ = False
    isNumber (NumberSegment _) = True
    isNumber _ = False

solve :: String -> (String, String)
solve input =
  let input' = parseInput input
  in (show $ part1 input', show $ part2 input')
