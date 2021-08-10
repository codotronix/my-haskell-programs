-- we know log 32 with base 2 is 5
-- or log n with base b is x iff b ** x = n 
-- here we will find the integer logs, not the fractions 
-- i.e. we will stop at 1

intlog :: Int -> Int -> Int
intlog b n
    | n == 0 = 1
    | otherwise = recursivemult b n 1 0


-- let's keep on multiplying the base b, until it becomes >= n
-- and let's keep track of how many times we are multiplying  

recursivemult :: Int -> Int -> Int -> Int -> Int
recursivemult b n mult counter
    | mult == n = counter
    | mult > n = counter - 1
    | otherwise = recursivemult b n (mult * b) (counter + 1)