module Day04 (solve) where

import Text.Parsec
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bits (shiftL, shiftR)
import Data.Maybe (fromMaybe)

data Card = Card { winning :: S.Set Int, ours :: S.Set Int }

parseInput :: String -> [Card]
parseInput input =
  case parse parser "" input of
    Left e -> error $ show e
    Right v -> v
  where
    parser = many1 cardParser
    cardParser = do
      string "Card"
      spaces
      many1 digit
      char ':'
      winNums <- many1 numsParser
      char '|'
      ourNums <- many1 numsParser
      pure $ Card (S.fromList winNums) (S.fromList ourNums)
    numsParser = do
      spaces
      num <- read <$> many1 digit
      spaces
      pure num

winCount :: Card -> Int
winCount c = length $ winning c `S.intersection` ours c

part1 :: [Card] -> Int
part1 cards =
  sum $ fmap (powerOfTwo . winCount)  cards
  where
    powerOfTwo n =
      (1 `shiftL` n) `shiftR` 1

part2 :: [Card] -> Int
part2 cards =
  sum $ snd <$> M.toList (foldl folding (M.fromList $ fmap (, 1) [0..(length cards - 1)]) (zip cards [0..]))
  where
    folding cardMap (card, index) =
      let currentCardCount = fromMaybe 0 $ cardMap M.!? index
          offset = winCount card
      in foldr (M.alter (insertOrAdd currentCardCount)) cardMap [(index + 1)..(index + offset)]

    insertOrAdd val (Just current) = Just $ val + current
    insertOrAdd val Nothing = Just val

solve :: String -> (String, String)
solve input =
  let input' = parseInput input
  in (show $ part1 input', show $ part2 input')
