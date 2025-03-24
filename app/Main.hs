import RSA(Numbers(Numbers), Keys(Keys), getPublicKey, getPrivateKey, getPublicMessage, decode)
import Primes(isPrime, coprime)

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
    putStrLn $ "Please choose a prime public key."
    input <- getPrime defaultVal
    return input

-- check for divide by zero errors: cant be congruent to the public key mod?
getMessage :: Integer -> IO Integer
getMessage defaultVal = do
    putStrLn $ "Please choose a message. Type nothing to use the default value: " ++ show defaultVal
    input <- getLine
    if null input
        then return defaultVal
        else return (read input :: Integer)


main :: IO()
main = do
    putStrLn $ "Welcome to the RSA playground! Here, you can choose numbers..."

    putStrLn $ "First, choose a public key. Some strong public keys are Fermat primes, or primes of the form 2^k+1, such as 3, 5, 17, 257, and 65537."
    e <- getUserPublicKey 65537
    putStrLn $ "You have chosen public key " ++ show e ++ "." 

    putStrLn $ "To encrypt messages, RSA needs two prime numbers to generate the private key. Which prime numbers would you like?"
    p <- getPrime 7170669219235139
    q <- getPrime 3557745895880441
    putStrLn $ "You have chosen primes " ++ show p ++ " and " ++ show q ++ "."

    putStrLn $ "Lastly, choose a private message."
    m <- getMessage 4727576933
    putStrLn $ "You have chosen private message " ++ show m ++ "." 

    -- Create the numbers and keys
    let numbers = Numbers p q e
    let keys = rsaReceiver numbers
    
    -- Show the result of encryption and decryption
    let encrypted = getPublicMessage keys m
    let decrypted = decode keys encrypted
    
    putStrLn $ "Encrypted message: " ++ show encrypted
    putStrLn $ "Decrypted message: " ++ show decrypted

