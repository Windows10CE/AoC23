module Day02 (solve) where

import qualified Data.Map as M
import Text.Parsec
import Data.Maybe (fromMaybe)

data Game = Game {
  gameId :: Int,
  rounds :: [M.Map String Int]
}

parseInput :: String -> [Game]
parseInput input = 
  case parse parser "" input of
    Right result -> result
    Left e -> error $ show e
  where
    parser = many1 game
    game = do
      string "Game "
      gId <- read <$> many1 digit
      string ": "
      rnds <- sepBy1 roundParser (string "; ")
      spaces
      pure $ Game gId rnds
    roundParser = M.fromList <$> sepBy1 set (string ", ")
    set = do
      c <- read <$> many1 digit
      char ' '
      name <- string "red" <|> string "blue" <|> string "green"
      pure (name, c)

pairs :: [(String, Int)]
pairs =
  [
    ("red", 12),
    ("green", 13),
    ("blue", 14)
  ]

part1 :: [Game] -> Int
part1 games =
  sum $ gameId <$> filter validGame games
  where
    validGame game =
      all (\(color, maxCount) -> all (\r -> fromMaybe 0 (r M.!? color) <= maxCount) (rounds game)) pairs

part2 :: [Game] -> Int
part2 games =
  sum $ power <$> games
  where
    power game =
      product $ (\(color, _) -> maximum $ (\r -> fromMaybe 0 (r M.!? color)) <$> rounds game) <$> pairs

solve :: String -> (String, String)
solve input =
  let input' = parseInput input
  in (show $ part1 input', show $ part2 input')
