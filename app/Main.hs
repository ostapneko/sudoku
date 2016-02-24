{-# LANGUAGE OverloadedLists #-}

module Main where

import Lib
import Solver
import qualified Data.Vector as V
import Data.Vector ((!))

-- toNum :: Char -> Maybe Int
-- toNum c =
--   case reads [c] of
--     [(n, "")] -> Just n
--     _ -> Nothing

-- format https://qqwing.com/instructions.html
-- parse :: String -> Grid
-- parse str =
--   let nums = V.fromList $ map toNum str
--       g1 = map (nums !) [0, 1, 2, 9, 10, 11, 18, 19, 20]
--       g2 = map (nums !) [3, 4, 5, 12, 13, 14, 21, 22, 23]
--       g3 = map (nums !) [6, 7, 8, 15, 16, 17, 24, 25, 26]
--       g4 = map (nums !) [27, 28, 29, 36, 37, 38, 45, 46, 47]
--       g5 = map (nums !) [30, 31, 32, 39, 40, 41, 48, 49, 50]
--       g6 = map (nums !) [33, 34, 35, 42, 43, 44, 51, 52, 53]
--       g7 = map (nums !) [54, 55, 56, 63, 64, 65, 72, 73, 74]
--       g8 = map (nums !) [57, 58, 59, 66, 67, 68, 75, 76, 77]
--       g9 = map (nums !) [60, 61, 62, 69, 70, 71, 78, 79, 80]
--   in V.fromList $ map V.fromList [g1, g2, g3, g4, g5, g6, g7, g7, g9]

main :: IO ()
main = do
  let str = ".13.....22.....48....7...19...9..8..7......2....3.......263.9..4.9.7.6....149...8"
  let grid = parse str
  print $ grid
