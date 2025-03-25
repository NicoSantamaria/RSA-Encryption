import Primes (coprime, isPrime)
import RSA (Keys (Keys), Numbers (Numbers), decode, getPrivateKey, getPublicKey, getPublicMessage)

-- generates public and private keys from the input set of numbers, using the RSA module
rsaReceiver :: Numbers -> Keys
rsaReceiver (Numbers p q e) = Keys (getPublicKey $ Numbers p q e) (getPrivateKey $ Numbers p q e)

-- Function to get a prime number from the user with a default value
getPrime :: Integer -> IO Integer
getPrime defaultVal = do
  putStrLn $ "Please choose a prime. Type nothing to use the default value: " ++ show defaultVal
  input <- getLine
  if null input
    then return defaultVal
    else do
      let userNum = read input :: Integer
      if isPrime userNum
        then return userNum
        else do
          putStrLn $ input ++ " is not a prime."
          getPrime defaultVal

-- get a user-defined public key
getUserPublicKey :: Integer -> IO Integer
getUserPublicKey defaultVal = do
  putStrLn "Please choose a prime public key."
  getPrime defaultVal

-- prompt the user to find prime factors which are coprime to the chosen public key
getUserFactor :: Integer -> IO Integer
getUserFactor defaultVal = do
  getPrime defaultVal

-- prompt the user to generate two prime factors for the public key
getUserPrimeFactors :: Integer -> Integer -> Integer -> IO Numbers
getUserPrimeFactors firstDefaultPrime secondDefaultPrime e = do
  p <- getUserFactor firstDefaultPrime
  q <- getUserFactor secondDefaultPrime
  if coprime p q e
    then return (Numbers p q e)
    else do
      putStrLn "The public key e must be coprime to (p - 1)(q - 1), where p and q are your chosen prime factors. Please choose again."
      getUserPrimeFactors firstDefaultPrime secondDefaultPrime e

-- check for divide by zero errors: cant be congruent to the public key mod?
getMessage :: Integer -> IO Integer
getMessage defaultVal = do
  putStrLn $ "Please choose a message. Type nothing to use the default value: " ++ show defaultVal
  input <- getLine
  if null input
    then return defaultVal
    else return (read input :: Integer)

-- check that the encrypted and decrypted messages match
successMessage :: Integer -> Integer -> IO ()
successMessage a b
  | a == b = do putStrLn "Success!"
  | otherwise = do putStrLn "Something went wrong, the encrypted message was not properly recovered."

main :: IO ()
main = do
  putStrLn "Welcome to the RSA playground!"
  putStrLn "Here, you can choose prime factors and see which public and private keys they generate."
  putStrLn "Then, you can choose a message and see the message encrypted and decrypted with the generated keys."

  -- get e from the user
  putStrLn "First, choose a public key. Some strong public keys are Fermat primes, or primes of the form 2^k+1, such as 3, 5, 17, 257, and 65537."
  e <- getUserPublicKey 65537
  putStrLn $ "You have chosen public key " ++ show e ++ "."
  putStrLn ""

  -- get p and q from the user, then generate keys and show them
  putStrLn "To encrypt messages, RSA needs two prime numbers to generate the private key. Which prime numbers would you like?"
  numbersIO <- getUserPrimeFactors 7170669219235139 3557745895880441 e
  let (Numbers p q _) = numbersIO
  let numbers = Numbers p q e
  let keys = rsaReceiver numbers
  putStrLn $ "You have chosen primes " ++ show p ++ " and " ++ show q ++ "."
  putStrLn $ "Thus, the public key is " ++ show (getPublicKey numbers) ++ "."
  putStrLn $ "The private key is " ++ show (getPrivateKey numbers) ++ "."
  putStrLn ""

  -- get m from the user
  putStrLn "Lastly, choose a private message. The message will be expressed modulo the public key."
  mIO <- getMessage 4727576933
  let m = mod mIO e
  putStrLn $ "You have chosen private message " ++ show m ++ "."

  -- compute encrypted message and decrypted message
  let encrypted = getPublicMessage keys m
  let decrypted = decode keys encrypted

  putStrLn $ "The encrypted public message is then " ++ show encrypted ++ "."
  putStrLn $ "After decryption, the received private message is " ++ show decrypted
  successMessage m decrypted
