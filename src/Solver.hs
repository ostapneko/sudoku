module Solver where

import qualified Data.Vector as V
import qualified Data.Map as M

data Cell
  = Fixed Int
  | Filled Int
  | Empty
  deriving (Show, Eq)

type Grid = V.Vector Cell
type Group = V.Vector Cell

toNum :: Char -> Cell
toNum c =
  case reads [c] of
    [(n, "")] -> Fixed n
    _ -> Empty

parse :: String -> Grid
parse str = V.fromList $ map toNum str
