import Test.Hspec
import Lib
import Solver
import qualified Data.Vector as V
import qualified Data.Set as S

main :: IO ()
main = hspec $ do
  describe "isValid" $ do
    it "returns true iff the group contains all the numbers from 1 to 9" $ do
      (group 1 2 3 8 4 9 5 7 6) `shouldSatisfy` isValid
      (group 1 1 3 8 4 9 5 7 6) `shouldNotSatisfy` isValid

  describe "rowsValid" $ do
    it "returns true iff the rows made from 3 groups are all valid" $ do
      let g1 = group 1 2 3 4 5 6 7 8 9
          g2 = group 4 5 6 7 8 9 1 2 3
          g3 = group 7 8 9 1 2 3 4 5 6
      rowsValid g1 g2 g3 `shouldBe` True

      let g1 = group 1 2 3 4 5 6 7 8 9
          g2 = group 4 5 6 7 8 9 1 2 3
          g3 = group 4 5 6 7 8 9 1 2 3
      rowsValid g1 g2 g3 `shouldBe` False

  describe "colsValid" $ do
    it "returns true iff the rows made from 3 groups are all valid" $ do
      let g1 = group 1 2 3 4 5 6 7 8 9
          g2 = group 2 3 4 5 6 7 8 9 1
          g3 = group 3 4 5 6 7 8 9 1 2
      colsValid g1 g2 g3 `shouldBe` True

      let g1 = group 1 2 3 4 5 6 7 8 9
          g2 = group 4 5 6 7 8 9 1 2 3
          g3 = group 4 5 6 7 8 9 1 2 3
      colsValid g1 g2 g3 `shouldBe` False

  describe "genGroups" $ do
    it "generates valid groups with no holes from a group with holes" $ do
      let g = [V.fromList [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Nothing, Nothing]]
      let actual = S.fromList (genGroups g [])
          exp = S.fromList [ group 1 2 3 4 5 6 7 8 9
                           , group 1 2 3 4 5 6 7 9 8
                           ]
      actual `shouldBe` exp

group :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Group
group a b c d e f g h i =
  V.fromList $ map Just [a, b, c, d, e, f, g, h, i]
