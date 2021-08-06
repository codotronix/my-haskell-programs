-- This factorial function cannot handle negative inputs
-- For negative inputs, it will go into infinite loop

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)