{-
Haskell implementation of the RSA cryptosystem.
-}


-- RECIPIENT 
--  choose primes p, q and public key e
p :: Integer
p = 7170669219235139
q :: Integer
q = 3557745895880441
e :: Integer
e = 97

-- publish e, p * q. e must be coprime to (p-1)(q-1)
publicKey :: (Integer, Integer)
publicKey = (e, p * q)

-- stores the private key e^-1 mod (p - 1)(q - 1) and (p - 1)(q - 1)
privateKey :: Integer -> (Integer, Integer)
privateKey e = (inverse e phi, phi)
    where phi = (p - 1) * (q - 1)


-- SENDER
-- store private message, must be coprime to p*q (which is almost certain)
privateMessage :: Integer
privateMessage = 4727576933

-- publish the encypted public message
publicMessage :: Integer
publicMessage = uncurry (power privateMessage) publicKey


-- RECIPIENT
-- decode the published publicMessage
decode :: Integer -> Integer
decode c = power publicMessage (fst (privateKey e)) (snd publicKey)


-- fast exponential using powers of 2 in a field
power :: Integer -> Integer -> Integer -> Integer
power b 1 n = mod b n
power b y n
    | even y = mod (power b (div y 2) n ^ 2) n
    | odd  y = mod (power b (div (y-1) 2) n ^ 2 * b) n


-- find greatest common divisor of two integers using Euclids Division Algorithm
gcdEuclid :: Integral a => a -> a -> a
gcdEuclid a 0 = a
gcdEuclid a b = gcdEuclid b (mod a b)


-- determines whether the chosen public key is coprime to phi(pq)
coprime :: Integer -> Bool
coprime a = gcdEuclid a phi == 1
    where phi = (p - 1) * (q - 1)


-- compute multiplicative inverse with respect to a given modulus
inverse :: Integer -> Integer -> Integer
inverse 1 b = 1
inverse a b = div (r * b + 1) a
    where r = a - inverse (mod b a) a


-- tests that public message decodes correctly
test :: Bool
test = decode publicMessage == privateMessage