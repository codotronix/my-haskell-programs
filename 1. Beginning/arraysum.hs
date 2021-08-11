-- given an array of Ints, return the sum of the elements

arraysum :: [Int] -> Int
arraysum [] = 0
arraysum (x:xs) = x + (arraysum xs)