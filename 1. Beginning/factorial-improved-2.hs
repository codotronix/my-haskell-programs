-- factorial with negative inputs

factorial :: Int -> Int
factorial n 
    | n == 0 = 1
    | n > 0 = n * (factorial(n-1))
    | otherwise = factorial(-n)