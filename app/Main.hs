module Main (main) where

import System.Environment (getArgs)
import qualified Day01 as D1
import qualified Day02 as D2

solvers :: [String -> (String, String)]
solvers =
  [
    D1.solve,
    D2.solve
  ]

main :: IO ()
main = do
  args <- getArgs
  let day = read (head args)
  (part1, part2) <- solveDay (solvers !! (day - 1)) day
  putStrLn $ "Part 1: " ++ part1 ++ "\nPart 2: " ++ part2

solveDay :: (String -> (String, String)) -> Int -> IO (String, String)
solveDay solver day = do
  input <- readFile $ "inputs/" ++ show day
  pure $ solver input
