import RSA(Numbers(Numbers), Keys(Keys), getPublicKey, getPrivateKey, getPublicMessage, power, decode)

-- generates public and private keys from the input set of numbers, using the RSA module
rsaReceiver :: Numbers -> Keys
rsaReceiver (Numbers p q e) = Keys (getPublicKey $ Numbers p q e) (getPrivateKey $ Numbers p q e)

-- a test test of numbers p q and e
exampleNumbers :: Numbers
exampleNumbers = Numbers 7170669219235139 3557745895880441 97

-- when run, shows that the private message 4727576933 decodes back to 4727576933
main :: IO()
main = do
    print $ decode keys (getPublicMessage keys 4727576933)
        where keys = rsaReceiver exampleNumbers