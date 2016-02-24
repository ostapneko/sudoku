module Solver where

import qualified Data.Vector as V
import Data.Vector ((!), (//))
import qualified Data.Map as M
import qualified Data.Set as S

data Cell
  = Fixed Int
  | Filled Int
  | Empty
  deriving (Show, Eq)

val :: Cell -> Int
val (Fixed i) = i
val (Filled i) = i
val Empty = error "value of empty cell"

type Grid = V.Vector Cell
type Group = V.Vector Cell

toNum :: Char -> Cell
toNum c =
  case reads [c] of
    [(n, "")] -> Fixed n
    _ -> Empty

parse :: String -> Grid
parse str = V.fromList $ map toNum str

col :: Int -> [Int]
col i =
  let offset = i `mod` 9
  in  [ index
      | mult <- [0..8]
      , let index = mult * 9 + offset
      , index /= i
      ]

row :: Int -> [Int]
row i =
  let mult = i `quot` 9
  in [ index
     | offset <- [0..8]
     , let index = mult * 9 + offset
     , index /= i
     ]

group :: Int -> [Int]
group i =
  let colIndex = ((i `mod` 9) `quot` 3) * 3
      rowIndex = ((i `quot` 9) `quot` 3) * 3
  in [ n
     | c <- [colIndex..colIndex + 2]
     , r <- [rowIndex..rowIndex + 2]
     , let n = r * 9 + c
     , n /= i
     ]

canUpdate :: Grid -> Int -> Int -> Bool
canUpdate g index value =
  let idx = (col index) ++ (row index) ++ (group index)
      cells = map (g !) idx
      vals = map val $ filter (/= Empty) cells
  in not $ value `elem` vals

backtrack :: Grid -> Int -> Grid
backtrack _ 0 =
  error "Could not solve this grid"
backtrack g index =
  case g ! (index - 1) of
    Empty ->
      error "Unreacheable: backtracking on empty cell"
    Fixed _ ->
      backtrack g (index - 1)
    Filled 9 ->
      let newGrid = g // [(index - 1, Empty)]
      in backtrack newGrid (index - 1)
    Filled i ->
      let newGrid = g // [(index - 1, Empty)]
      in step newGrid (index - 1) (i + 1)

step :: Grid -> Int -> Int -> Grid
step g index value =
  if index == 81
    then g
    else
      case (g ! index, canUpdate g index value, value) of
        (Empty, True, _) ->
          let newGrid = g // [(index, (Filled value))]
          in step newGrid (index + 1) 1
        (Empty, False, 9) ->
          backtrack g index
        (Empty, False, _) ->
          step g index (value + 1)
        (Fixed _, _, _) ->
          step g (index + 1) 1
        _ ->
          error "Unreacheable: stepping on filled cell"

solve :: Grid -> Grid
solve g = step g 0 1
