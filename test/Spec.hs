import Test.Hspec
import Primes

main :: IO ()
main = hspec $ do
  describe "is_prime function" $ do
    it "correctly identifies non-primes <= 1" $ do
      is_prime 0 `shouldBe` False
      is_prime 1 `shouldBe` False
      is_prime (-5) `shouldBe` False
    
    it "correctly identifies small primes" $ do
      is_prime 2 `shouldBe` True
      is_prime 3 `shouldBe` True
      is_prime 5 `shouldBe` True
      is_prime 7 `shouldBe` True
      is_prime 11 `shouldBe` True
      
    it "correctly identifies non-primes" $ do
      is_prime 4 `shouldBe` False
      is_prime 6 `shouldBe` False
      is_prime 8 `shouldBe` False
      is_prime 9 `shouldBe` False
      is_prime 10 `shouldBe` False
      
    it "handles larger prime numbers" $ do
      is_prime 97 `shouldBe` True
      is_prime 101 `shouldBe` True