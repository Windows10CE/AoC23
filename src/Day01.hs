module Day01 (solve) where
import Data.Char (isDigit)
import Debug.Trace (traceShowId)

firstLastDigit :: String -> String
firstLastDigit s = let digits = filter isDigit s in [head digits, last digits]

part1 :: [String] -> Int
part1 ls = sum (read . firstLastDigit <$> ls)

firstAndLastNum :: String -> String
firstAndLastNum s = 
  case traceShowId $ foldl findFirstAndLast (Nothing, Nothing) $ droppingHead s of
    (Just f, Just l) -> [f, l]
    (Just f, Nothing) -> [f]
    _ -> error s
  where
    droppingHead :: [a] -> [[a]]
    droppingHead [] = []
    droppingHead xs@(_:xs') = xs : droppingHead xs'

    findFirstAndLast :: (Maybe Char, Maybe Char) -> String -> (Maybe Char, Maybe Char)
    findFirstAndLast firstAndLast nextWindow =
      case traceShowId nextWindow of
        '1' : _ -> replaceNeeded firstAndLast '1'
        'o' : 'n' : 'e' : _ -> replaceNeeded firstAndLast '1'
        '2' : _ -> replaceNeeded firstAndLast '2'
        't' : 'w' : 'o' : _ -> replaceNeeded firstAndLast '2'
        '3' : _ -> replaceNeeded firstAndLast '3'
        't' : 'h' : 'r' : 'e' : 'e' : _ -> replaceNeeded firstAndLast '3'
        '4' : _ -> replaceNeeded firstAndLast '4'
        'f' : 'o' : 'u' : 'r' : _ -> replaceNeeded firstAndLast '4'
        '5' : _ -> replaceNeeded firstAndLast '5'
        'f' : 'i' : 'v' : 'e' : _ -> replaceNeeded firstAndLast '5'
        '6' : _ -> replaceNeeded firstAndLast '6'
        's' : 'i' : 'x' : _ -> replaceNeeded firstAndLast '6'
        '7' : _ -> replaceNeeded firstAndLast '7'
        's' : 'e' : 'v' : 'e' : 'n' : _ -> replaceNeeded firstAndLast '7'
        '8' : _ -> replaceNeeded firstAndLast '8'
        'e' : 'i' : 'g' : 'h' : 't' : _ -> replaceNeeded firstAndLast '8'
        '9' : _ -> replaceNeeded firstAndLast '9'
        'n' : 'i' : 'n' : 'e' : _ -> replaceNeeded firstAndLast '9'
        _ -> firstAndLast

    replaceNeeded (Nothing, _) new = (Just new, Nothing)
    replaceNeeded (Just f, _) new = (Just f, Just new)

part2 :: [String] -> Int
part2 ls = sum (read . firstAndLastNum <$> ls)

solve :: String -> (String, String)
solve input =
  let ls = lines input
  in (show $ part1 ls, show $ part2 ls)
