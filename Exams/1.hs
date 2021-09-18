{--
1. Write a function f1 :: [a] -> Int -> [a] 
that takes a list and a positive number n, 
and drops elements of the list at index i*n-1, for i > 0. 
Remember that list indices start at 0.

Test cases:  	
f1 [1,2,3,4,5] 2 = [1,3,5]
f1 [2,4,6,8,10] 1 = []
f1 [1,3,5,7,9,11,13,15] 4 = [1,3,5,9,11,13]
--}

f1 :: [Int] -> Int -> [Int]
f1 _ 1 = []
f1 a n = removexfromy a (getindices (length a) n 0 []) [] 0


-- get a list of indices for i*n-1
-- it will take a length of original list and n 
-- and will return a list with all the indices to be removed 
getindices :: Int -> Int -> Int -> [Int] -> [Int]
getindices l n i xy
    | l == i = xy
    | otherwise = getindices l n (i+1) (xy ++ [i*n-1])


-- Take a list of integers a
-- Take a list of indices to be removed b
-- A blak list y to put the rest of the elemets
-- return a list without the indices of list 2, i.e. y
-- s starting index i=0
removexfromy :: [Int] -> [Int] -> [Int] -> Int -> [Int]
removexfromy [] _ y _ = y
removexfromy _ [] y _ = y
removexfromy a b y i
    | i == (length a) = y 
    | elem i b = removexfromy a b y (i+1)
    | otherwise = removexfromy a b (y ++ [a !! i]) (i+1)


-------------------------------------------------------------------------

{--
2. Define a function f2 :: Int -> Int that takes a non-negative integer 
and places its rightmost digit at the leftmost position.

Test cases: 	
f2 1 = 1
f2 123 = 312
f2 67890 = 6789
f2 678900 = 67890
f2 678901 = 167890
--}

f2 :: Int -> Int
f2 n 
    | (howlong n) == 1 = n
    | otherwise = (10 ^ ((howlong n) - 1) * (rem n 10)) + (div n 10)

-- Calculate the length of a number
howlong :: Int -> Int
howlong n
    | div n 10 == 0 = 1
    | otherwise = 1 + (howlong (div n 10))