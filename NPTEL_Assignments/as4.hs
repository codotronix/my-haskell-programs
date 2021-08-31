-- 1. Define a function f1 :: [Int] -> [Int] which takes a list l of nonnegative 
-- numbers as input, and replaces each n in l by 3*n if n is a power of 3, 
-- and by 0 if it is not a power of 3. 

-- Examples:
--   f1 [] = []
--   f1 [1] = [3]
--   f1 [1, 2, 3] = [3, 0, 9]
--   f1 [0, 2, 4, 6] = [0, 0, 0, 0]


ispowerof3 :: Int -> Bool
ispowerof3 0 = False
ispowerof3 1 = True
ispowerof3 n = (mod n 3 == 0) && ispowerof3 (div n 3)

f1 :: [Int] -> [Int]
f1 [] = []
f1 xs
    | ispowerof3 (head xs) = (3 * (head xs)): f1 (tail xs)
    | otherwise = 0: f1 (tail xs)


------------------------------------------------------------------------------------------------


-- 2. For a list l, define S(l) to be the set of all indices i of l (remember that 
-- indices start from 0) such that l!!i > l!!(i+1). Define a function 
-- f2 :: [Int] -> [Int] which takes a nonempty list l of 
-- integers as input and outputs a S(l) in order.

-- Examples:
--   f2 [] = []
--   f2 [1] = []
--   f2 [1, 2, 3, 2, 1] = [2, 3]
--   f2 [1, 2, 3, 4, 5, 6] = []

f2 :: [Int] -> [Int]
f2 [] = []
f2 [x] = []
f2 [x,y] 
    | x > y = [0]
    | otherwise = []
f2 x = h2 x 0


h2 :: [Int] -> Int -> [Int]
h2 (x:xs) i
    | xs == [] = []
    | x > head xs = i: h2 xs (i+1)
    | otherwise = h2 xs (i+1)


------------------------------------------------------------------------------------------------


-- 3. Define a function f3 :: [Int] -> [Int] that removes adjacent duplicates. 
-- i.e. if the same element occurs n times contiguously, we retain only one copy.

-- Examples:
--   f3 [1, 1, 1, 2, 2, 3, 3, 3, 3] = [1, 2, 3]
--   f3 [1, 2, 1, 2, 3, 1, 1, 2, 2] = [1, 2, 1, 2, 3, 1, 2]

f3 :: [Int] -> [Int]
f3 [] = []
f3 [x] = [x]
f3 (x:xs) 
    | x == head xs = f3 xs
    | otherwise = x: (f3 xs)

------------------------------------------------------------------------------------------------

-- 4. Define a function f4 :: [Int] -> [[Int]] that partitions the list into all 
-- its upruns. An uprun is a maximal non-decreasing segment of the given list. 

-- Examples:

-- f4 [] = []
-- f4 [5] = [[5]]
-- f4 [1, 2, 3, 4, 5] = [[1,2,3,4,5]]
-- f4 [1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1] = [[1,2,3,4,5,6],[5],[4],[3],[2],[1]]

f4 :: [Int] -> [[Int]]
f4 [] = []
f4 [x] = [[x]]
f4 x = h4 x [] []

h4 :: [Int] -> [Int] -> [[Int]] -> [[Int]]
h4 [] y r = r ++ [y]
h4 [x] y r = r ++ [y ++ [x]]
h4 (x:xs) y r
    | x < head xs = h4 xs (y ++ [x]) r 
    | otherwise = h4 xs [] (r ++ [y ++ [x]]) 


-------------------------------------------------------------------------------------------------