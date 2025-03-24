module RSA(
    Numbers(Numbers),
    Keys(Keys),
    getPublicKey,
    getPrivateKey,
    getPublicMessage,
    power,
    decode
    ) where 

    -- type for inputting pairs of primes with inverse
    data Numbers = Numbers {
        p :: Integer,
        q :: Integer,
        e :: Integer
        } deriving (Show)

    -- type for computation of public and private keys
    data Keys = Keys {
        publicKey :: (Integer, Integer),
        privateKey :: (Integer, Integer)
        } deriving (Show)

    -- fast exponential using powers of 2 in a field
    power :: Integer -> Integer -> Integer -> Integer
    power b 1 n = mod b n
    power b y n
        | even y = mod (power b (div y 2) n ^ 2) n
        | odd  y = mod (power b (div (y-1) 2) n ^ 2 * b) n


    -- determines whether the chosen public key is coprime to phi(pq)
    coprime :: Numbers -> Bool
    coprime (Numbers p q e) = gcdEuclid e phi == 1
        where 
            phi = (p - 1) * (q - 1)
            gcdEuclid :: Integral a => a -> a -> a
            gcdEuclid a 0 = a
            gcdEuclid a b = gcdEuclid b (mod a b)

    -- compute multiplicative inverse with respect to a given modulus
    inverse :: Integer -> Integer -> Integer
    inverse 1 b = 1
    inverse a b = div (r * b + 1) a
        where r = a - inverse (mod b a) a

    -- compute the public key
    getPublicKey :: Numbers -> (Integer, Integer)
    getPublicKey (Numbers p q e) = (e, p * q)

    -- compute the private key
    getPrivateKey :: Numbers -> (Integer, Integer)
    getPrivateKey (Numbers p q e) = (inverse e phi, phi)
        where phi = (p - 1) * (q - 1)

    -- compute the public message from the keys and private message
    getPublicMessage :: Keys -> Integer -> Integer
    getPublicMessage (Keys pubKey privKey) privMessage = uncurry (power privMessage) pubKey

    -- decode the public message to the private message with the public and private keys
    decode :: Keys -> Integer -> Integer
    decode (Keys pubKey privKey) pubMessage = power pubMessage (fst privKey) (snd pubKey)