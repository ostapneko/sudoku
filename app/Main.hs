{-# LANGUAGE OverloadedLists #-}

module Main where

import Solver
import qualified Data.Vector as V
import Data.Vector ((!))
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [str] -> do
      let grid = parse str
          solved = solve grid
      putStrLn $ foldr (\w s -> w ++ s) "" $ map show $ V.toList $ V.map val solved
    _ -> do
      prgName <- getProgName
      putStrLn $ "Usage: " ++ prgName ++ " <grid>\n where grid is in a format such as\n ........49.1.6.5....8....9.8..4.37.6....1....54...718.....39..7....4..5....7..81."
