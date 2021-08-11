-- Let's try to reverse a given array
-- remember [a,b] ++ [c,d,e] = [a,b,c,d,e]
-- '++' concatenates 2 lists

arrayreverse :: [Int] -> [Int]
arrayreverse [] = []
arrayreverse (x:xs) = (arrayreverse xs) ++ [x]
