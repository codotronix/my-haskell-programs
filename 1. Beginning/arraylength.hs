-- let's try to calculate the length of given list

arraylen :: [Int] -> Int
arraylen [] = 0
arraylen (x:xs) = 1 + (arraylen xs)