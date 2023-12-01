module Day01 (solve) where
import Data.Char (isDigit)

firstLastDigit :: String -> String
firstLastDigit s = let digits = filter isDigit s in [head digits, last digits]

part1 :: [String] -> Int
part1 ls = sum (read . firstLastDigit <$> ls)

firstLastNum :: String -> String
firstLastNum line = 
  case foldl findFirstAndLast (Nothing, Nothing) $ droppingHead line of
    (Just f, Just l) -> [f, l]
    _ -> error line
  where
    droppingHead :: [a] -> [[a]]
    droppingHead [] = []
    droppingHead xs@(_:xs') = xs : droppingHead xs'

    startsWith :: Eq e => [e] -> [e] -> Bool
    startsWith [] _ = True
    startsWith (_:_) [] = False
    startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

    replaceIfStartsWithAny :: (Maybe Char, Maybe Char) -> Char -> [String] -> String -> (Maybe Char, Maybe Char)
    replaceIfStartsWithAny fl c choices s = if any (`startsWith` s) choices then replaceNeeded fl c else fl

    findFirstAndLast :: (Maybe Char, Maybe Char) -> String -> (Maybe Char, Maybe Char)
    findFirstAndLast firstAndLast nextWindow =
      let pairs = 
            [
              ('1', ["1", "one"]),
              ('2', ["2", "two"]),
              ('3', ["3", "three"]),
              ('4', ["4", "four"]),
              ('5', ["5", "five"]),
              ('6', ["6", "six"]),
              ('7', ["7", "seven"]),
              ('8', ["8", "eight"]),
              ('9', ["9", "nine"])
            ]
      in foldr (\(c, choices) fl -> replaceIfStartsWithAny fl c choices nextWindow) firstAndLast pairs

    replaceNeeded (Nothing, _) new = (Just new, Just new)
    replaceNeeded (Just f, _) new = (Just f, Just new)

part2 :: [String] -> Int
part2 ls = sum (read . firstLastNum <$> ls)

solve :: String -> (String, String)
solve input =
  let ls = lines input
  in (show $ part1 ls, show $ part2 ls)
