-- intlogv2 (base, number) = result
intlogv2 :: Int -> Int -> Int
intlogv2 b 1 = 0
intlogv2 b n 
    | n >= b = 1 + (intlogv2 b (div n b))
    | otherwise = 0
