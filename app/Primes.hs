module Primes (isPrime, coprime) where

-- checks if a given number is prime (naive approach)
isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 = True
  | even n = False
  | otherwise = not $ any divides [3, 5 .. integerSqrt n]
  where
    divides :: Integer -> Bool
    divides i = mod n i == 0
    integerSqrt :: Integer -> Integer
    integerSqrt x = floor $ sqrt (fromIntegral x :: Double)

-- determines whether the chosen public key is coprime to phi(pq)
coprime :: Integer -> Integer -> Integer -> Bool
coprime p q e = gcdEuclid e phi == 1
  where
    phi = (p - 1) * (q - 1)
    gcdEuclid :: (Integral a) => a -> a -> a
    gcdEuclid a 0 = a
    gcdEuclid a b = gcdEuclid b (mod a b)
