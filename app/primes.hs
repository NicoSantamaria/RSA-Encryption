-- checks if a given number is prime (naive approach)
is_prime :: Integer -> Bool
is_prime n
    | n <= 1    = False
    | n == 2    = True
    | even n    = False
    | otherwise = not $ any (divides) [3, 5..integer_sqrt n]
        where 
            divides :: Integer -> Bool
            divides i = mod n i == 0
            integer_sqrt :: Integer -> Integer
            integer_sqrt = floor . sqrt . fromIntegral
