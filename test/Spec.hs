import Test.Hspec
import Solver
import qualified Data.Vector as V
import Data.Vector ((!), (//))
import qualified Data.Set as S
import Control.Monad.Trans

main :: IO ()
main = hspec $ do
  let solved = parse "913584762257169483648723519136942857795816324824357196572638941489271635361495278"

  describe "col" $ do
    it "returns all the indexes in the same col without itself" $ do
      col 0 `shouldBe` [9, 18..72]

  describe "row" $ do
    it "returns all the indexes in the same row without itself" $ do
      row 0 `shouldBe` [1..8]

  describe "group" $ do
    it "returns all the indexes in the same group without itslef" $ do
      S.fromList (group 0) `shouldBe` S.fromList [1, 2, 9, 10, 11, 18, 19, 20]

  describe "canUpdate" $ do
    it "can update a grid a the given index with the given value" $ do
      let g = parse ".23456789........................................................................"
      canUpdate g 0 1 `shouldBe` True

      -- same row
      let g = parse ".13456789........................................................................"
      canUpdate g 0 1 `shouldBe` False

      -- same column
      let g = parse ".23456789.........1.............................................................."
      canUpdate g 0 1 `shouldBe` False

      -- same group
      let g = parse ".23456789.1......................................................................"
      canUpdate g 0 1 `shouldBe` False

      -- all
      let g = solved // [(80, Empty)]
      canUpdate g 80 8 `shouldBe` True

  describe "step" $ do
    it "solves the problem" $ do
      let g = solved // [(80, Empty)]
      step g 80 8 `shouldBe` (solved // [(80, Filled 8)])

      let g = solved // [(0, Empty)]
      step g 0 9 `shouldBe` (solved // [(0, Filled 9)])

      let g = solved // [(0, Empty), (1, Empty), (80, Empty)]
      step g 0 1 `shouldBe` (solved // [(0, Filled 9), (1, Filled 1), (80, Filled 8)])

  describe "backtrack" $ do
    it "goes back one step to try other solution" $ do
      let g = solved // [(79, Filled 6), (80, Empty)]
      backtrack g 80 `shouldBe` (solved // [(79, Filled 7), (80, Filled 8)])
