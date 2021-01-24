import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "impossible row" $ do 
      evalState (inner [3] [Unknown, Unknown] 0 0) Map.empty `shouldBe` False
    it "possible row" $ do 
      evalState (inner [2] [Unknown, Unknown] 0 0) Map.empty `shouldBe` True
    it "property - exact fix" $ do 
      property $ prop_fit genList
    it "property - not fix" $ do 
      property $ prop_not_fit genList
    it "property - exact fix" $ do 
      property $ prop_fit genList1
    it "property - not fix" $ do 
      property $ prop_not_fit genList1

genList :: Gen [Int]
genList = shuffle [1,2,3,1,4,2,3,4,2,5]

genList1 :: Gen [Int]
genList1 = sublistOf [1,2,3,1,4,2,3,4,2,5]

prop_fit :: Gen [Int] -> Property
prop_fit gen =
  forAll gen (\x -> let l = sum(x) + length(x) - 1 in 
        evalState (inner x (take l $ repeat Unknown) 0 0) Map.empty 
    )

prop_not_fit :: Gen [Int] -> Property
prop_not_fit gen =
  forAll gen (\x -> let l = sum(x) + length(x) - 2 in 
        not $ evalState (inner x (take l $ repeat Unknown) 0 0) Map.empty 
    )
