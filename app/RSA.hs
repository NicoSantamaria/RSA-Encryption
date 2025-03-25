module RSA (
    Numbers (Numbers),
    Keys (Keys),
    getPublicKey,
    getPrivateKey,
    getPublicMessage,
    power,
    decode,
) where

-- type for inputting pairs of primes with inverse
data Numbers = Numbers
    { firstPrime :: Integer
    , secondPrime :: Integer
    , publicKeyGenerator :: Integer
    }
    deriving (Show)

-- type for computation of public and private keys
data Keys = Keys
    { publicKey :: (Integer, Integer)
    , privateKey :: (Integer, Integer)
    }
    deriving (Show)

-- fast exponential using powers of 2 in a field
power :: Integer -> Integer -> Integer -> Integer
power _ 0 _ = 1
power b 1 n = mod b n
power b y n
    | y < 0 = inverse (power b (abs y) n) n
    | even y = mod (power b (div y 2) n ^ (2 :: Integer)) n
    | otherwise = mod (power b (div (y - 1) 2) n ^ (2 :: Integer) * b) n

-- compute multiplicative inverse with respect to a given modulus
inverse :: Integer -> Integer -> Integer
inverse 1 b = 1
inverse a b = div (r * b + 1) a
  where
    r = a - inverse (mod b a) a

-- compute the public key
getPublicKey :: Numbers -> (Integer, Integer)
getPublicKey (Numbers p q e) = (e, p * q)

-- compute the private key
getPrivateKey :: Numbers -> (Integer, Integer)
getPrivateKey (Numbers p q e) = (d, n)
  where
    phi = (p - 1) * (q - 1)
    n = p * q
    d = inverse e phi

-- compute the public message from the keys and private message
getPublicMessage :: Keys -> Integer -> Integer
getPublicMessage (Keys (e, n) _) privMessage = power privMessage e n

-- decode the public message to the private message with the public and private keys
decode :: Keys -> Integer -> Integer
decode (Keys _ (d, n)) pubMessage =
    power pubMessage d n
