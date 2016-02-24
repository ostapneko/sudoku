{-# LANGUAGE OverloadedLists #-}

module Main where

import Solver
import qualified Data.Vector as V
import Data.Vector ((!))

main :: IO ()
main = do
  let str = "........49.1.6.5....8....9.8..4.37.6....1....54...718.....39..7....4..5....7..81."
  let grid = parse str
      solved = solve grid
  print $ solved
