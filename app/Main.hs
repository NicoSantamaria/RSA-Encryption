import RSA(Numbers(Numbers), Keys(Keys), getPublicKey, getPrivateKey, getPublicMessage, decode)
import Primes(isPrime, coprime)

-- TODO: check that p and q are safe choices for e = 65537, per the following stack exchange post:
-- For RSA to work, we require that its public key function 𝑥↦𝑥𝑒mod(𝑝𝑞) be a reversible mapping on [0,𝑛). With 𝑝 and 𝑞 coprime, that's equivalent (by the CRT) to 𝑥↦𝑥𝑒mod𝑝 being a reversible mapping on [0,𝑝) and 𝑥↦𝑥𝑒mod𝑞 being a reversible mapping on [0,𝑞). With 𝑝 and 𝑞 prime, that's equivalent to 𝑒 being coprime with both 𝑝−1 and 𝑞−1.

-- We could choose for 𝑒 the smallest integer greater than 1 that is coprime with both 𝑝−1 and 𝑞−1 (implying 𝑒 odd and at least 3 for large primes 𝑝 and 𝑞). That's not too long to find by trial and error. However, it is customary to use 𝑒 at least 16-bit so that 𝑒≥65537. This is mandated by some standards (sometime: for encryption only), and justified if we use an ad-hoc padding (e.g. RSAES-PKCS1-v1_5) because it mitigates padding oracle attacks to some degree.

-- The most common practice is to choose 𝑒=65537, and then choose 𝑝 with 𝑝mod𝑒≠1, which ensures 𝑒 is coprime with both 𝑝−1, since 𝑒 is prime; same for 𝑞.

-- When using a secure padding, and no rule forbids it, we can choose 𝑒=3, and then choose primes of the form 3𝑘+2.
-- https://crypto.stackexchange.com/questions/13166/method-to-calculating-e-in-rsa

-- generates public and private keys from the input set of numbers, using the RSA module
rsaReceiver :: Numbers -> Keys
rsaReceiver (Numbers p q e) = Keys (getPublicKey $ Numbers p q e) (getPrivateKey $ Numbers p q e)

-- a test test of numbers p q and e
exampleNumbers :: Numbers
exampleNumbers = Numbers 7170669219235139 3557745895880441 65537

-- TODO: getPublicKey and getPrime are so similar... surely they can be abstracted
-- TODO: check that the input is of the form 2k + 1
getPublicKey :: Integer -> IO Integer
getPublicKey defaultVal = do
    putStrLn $ "Please choose a public key. Type nothing to use the default value: " ++ show defaultVal
    input <- getLine
    if null input 
        then return defaultVal
        else do
            let userNum = read input :: Integer
            if isPrime userNum 
                then return userNum
                else do
                    putStrLn $ input ++ " is not a prime."
                    getPublicKey defaulVal

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

-- when run, shows that the private message 4727576933 decodes back to 4727576933
main :: IO()
main = do
    putStrLn $ "Welcome to the RSA playground! Here, you can choose numbers..."
    putStrLn $ "First, choose a public key. Strong public keys are primes of the form 2k+1. Examples include 3, 5, 17, 257, and 65537."
    e <- getPublicKey 65537

    putStrLn $ "To encrypt messages, RSA needs two prime numbers to generate the private key. Which prime numbers would you like?"
    p <- getPrime 7170669219235139
    q <- getPrime 3557745895880441
    putStrLn $ "You have chosen primes " ++ show p ++ " and " ++ show q ++ "."

    putStrLn $ "Lastly, choose a message."

    -- TODO: compute with user-defined primes
    print $ decode keys (getPublicMessage keys 4727576933)
        where keys = rsaReceiver exampleNumbers

