module Day01 (solve) where
import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)

firstLastDigit :: String -> String
firstLastDigit s = let digits = filter isDigit s in [head digits, last digits]

part1 :: [String] -> Int
part1 ls = sum (read . firstLastDigit <$> ls)

firstLastNum :: String -> Int
firstLastNum line = 
  case foldl findFirstAndLast (Nothing, Nothing) $ tails line of
    (Just f, Just l) -> f * 10 + l
    _ -> error line
  where
    replaceIfStartsWithAny :: (Maybe Int, Maybe Int) -> Int -> [String] -> String -> (Maybe Int, Maybe Int)
    replaceIfStartsWithAny fl c choices s = if any (`isPrefixOf` s) choices then replaceNeeded fl c else fl

    findFirstAndLast :: (Maybe Int, Maybe Int) -> String -> (Maybe Int, Maybe Int)
    findFirstAndLast firstAndLast nextWindow =
      let pairs = 
            [
              (1, ["1", "one"]),
              (2, ["2", "two"]),
              (3, ["3", "three"]),
              (4, ["4", "four"]),
              (5, ["5", "five"]),
              (6, ["6", "six"]),
              (7, ["7", "seven"]),
              (8, ["8", "eight"]),
              (9, ["9", "nine"])
            ]
      in foldr (\(c, choices) fl -> replaceIfStartsWithAny fl c choices nextWindow) firstAndLast pairs

    replaceNeeded (Nothing, _) new = (Just new, Just new)
    replaceNeeded (Just f, _) new = (Just f, Just new)

part2 :: [String] -> Int
part2 ls = sum (firstLastNum <$> ls)

solve :: String -> (String, String)
solve input =
  let ls = lines input
  in (show $ part1 ls, show $ part2 ls)
