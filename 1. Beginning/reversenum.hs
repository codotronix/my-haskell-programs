-- reversenum 1234 = 4321

reversenum :: Int -> Int
reversenum n = recrev n 0

-- My helper function
recrev :: Int -> Int -> Int
recrev n r 
    | n == 0 = r
    | otherwise = recrev (div n 10) ((r * 10) + (mod n 10))