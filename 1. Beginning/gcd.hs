-- calculating gcd using Euclid's formula
-- to get gcd(a,b) where a > b
-- if b == 0, then answer is a 
-- otherwise gcd(a,b) = gcd(b, mod a b)

mygcd :: Int -> Int -> Int
mygcd a b
    | a < b = gcd b a 
    | b == 0 = a 
    | otherwise = gcd b (mod a b)