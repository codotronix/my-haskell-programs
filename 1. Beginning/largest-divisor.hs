-- find the largest divisor of n, 
-- which is less than n 

ldivisor :: Int -> Int
ldivisor n = finddivisor n (n-1)

-- Let's take the helpof another function
-- Because we need to track the original input 
-- And also the recursively lowering number, (until it reaches 1)

finddivisor :: Int -> Int -> Int
finddivisor n m
    | mod n m == 0 = m
    | otherwise = finddivisor n (m-1)