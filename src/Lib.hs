module Lib
    ( Group(..)
    , isValid
    , rowsValid
    , colsValid
    , genGroups
    , solve
    , Grid(..)
    ) where

import qualified Data.Set as S
import Data.Vector ((!), (//), toList, Vector(..))
import qualified Data.Vector as V
import Data.Maybe

-- 0 1 2
-- 3 4 5
-- 6 7 8
type Group = Vector (Maybe Int)

row :: Group -> Int -> [Maybe Int]
row gp 1 = map (gp !) [0, 1, 2]
row gp 2 = map (gp !) [3, 4, 5]
row gp 3 = map (gp !) [6, 7, 8]
row gp _ = error "Row are from 1 to 3"

col :: Group -> Int -> [Maybe Int]
col gp 1 = map (gp !) [0, 3, 6]
col gp 2 = map (gp !) [1, 4, 7]
col gp 3 = map (gp !) [2, 5, 8]
col gp _ = error "Cols are from 1 to 3"

allNumbers :: S.Set (Maybe Int)
allNumbers = S.fromList $ map Just [1..9]

isValid :: Group -> Bool
isValid g =
  S.fromList (toList g) == allNumbers

dontHaveDuplicate :: Group -> Bool
dontHaveDuplicate g =
  let v = V.filter isJust g
  in (S.size . S.fromList . V.toList) v == V.length v

rowsValid :: Group -> Group -> Group -> Bool
rowsValid left center right =
  let row1 = row left 1 ++ row center 1 ++ row right 1
      row2 = row left 2 ++ row center 2 ++ row right 2
      row3 = row left 3 ++ row center 3 ++ row right 3
  in S.fromList(map S.fromList [row1, row2, row3]) == S.singleton(allNumbers)

colsValid :: Group -> Group -> Group -> Bool
colsValid top middle bottom =
  let col1 = col top 1 ++ col middle 1 ++ col bottom 1
      col2 = col top 2 ++ col middle 2 ++ col bottom 2
      col3 = col top 3 ++ col middle 3 ++ col bottom 3
  in S.fromList(map S.fromList [col1, col2, col3]) == S.singleton(allNumbers)

genGroups :: [Group] -> [Group] -> [Group]
genGroups rest acc =
  case (rest, acc) of
    ([], acc) -> acc
    (h : t, acc) ->
      case V.elemIndex Nothing h of
        Just i ->
          let newGroups = [ gp
                          | n <- [1..9]
                          , let gp = h // [(i, Just n)]
                          , dontHaveDuplicate gp
                          ]
          in genGroups (newGroups ++ t) acc
        _ ->
          genGroups t (h : acc)

type Grid = Vector Group

-- 0 1 2
-- 3 4 5
-- 6 7 8
solve :: Grid -> [Grid]
solve grid =
  [ V.fromList [g0, g1, g2, g3, g4, g5, g6, g7, g8]
  | g0 <- genGroups [grid ! 0] []
  , g1 <- genGroups [grid ! 1] []
  , g2 <- genGroups [grid ! 2] []
  , g3 <- genGroups [grid ! 3] []
  , g4 <- genGroups [grid ! 4] []
  , g5 <- genGroups [grid ! 5] []
  , g6 <- genGroups [grid ! 6] []
  , g7 <- genGroups [grid ! 7] []
  , g8 <- genGroups [grid ! 8] []
  , rowsValid g0 g1 g2
  , rowsValid g3 g4 g5
  , rowsValid g6 g7 g8
  , colsValid g0 g3 g6
  , colsValid g1 g4 g7
  , colsValid g2 g5 g8
  ]
