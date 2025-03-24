import Test.Hspec
import Primes

main :: IO ()
main = hspec $ do
  describe "isPrime function" $ do
    it "correctly identifies non-primes <= 1" $ do
      isPrime 0 `shouldBe` False
      isPrime 1 `shouldBe` False
      isPrime (-5) `shouldBe` False
    
    it "correctly identifies small primes" $ do
      isPrime 2 `shouldBe` True
      isPrime 3 `shouldBe` True
      isPrime 5 `shouldBe` True
      isPrime 7 `shouldBe` True
      isPrime 11 `shouldBe` True
      
    it "correctly identifies non-primes" $ do
      isPrime 4 `shouldBe` False
      isPrime 6 `shouldBe` False
      isPrime 8 `shouldBe` False
      isPrime 9 `shouldBe` False
      isPrime 10 `shouldBe` False
      
    it "handles larger prime numbers" $ do
      isPrime 97 `shouldBe` True
      isPrime 101 `shouldBe` True