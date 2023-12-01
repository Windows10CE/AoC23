module Main (main) where

import System.Environment (getArgs)
import qualified Day01 as D1

main :: IO ()
main = do
  args <- getArgs
  let (solver, day) = case args of
        ["1"] -> (D1.solve, 1)
        _ -> error "no day"
  (part1, part2) <- solveDay solver day
  putStrLn $ "Part 1: " ++ part1 ++ "\nPart 2: " ++ part2

solveDay :: (String -> (String, String)) -> Int -> IO (String, String)
solveDay solver day = do
  input <- readFile $ "inputs/" ++ show day
  pure $ solver input
