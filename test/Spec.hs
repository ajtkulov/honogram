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
    it "possible row - 1" $ do 
      evalState (inner [1] [Unknown, Fill, Unknown] 0 0) Map.empty `shouldBe` True
    it "property - exact fit" $ do 
      property $ prop_fit genList
    it "property - not fit" $ do 
      property $ prop_not_fit genList
    it "property - exact fit" $ do 
      property $ prop_fit genList1
    it "property - not fit" $ do 
      property $ prop_not_fit genList1
    it "err1" $ do 
       best [1] [Unknown , Unknown , Unknown ] `shouldBe` [Unknown , Unknown , Unknown ]

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
