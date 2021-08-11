-- In haskel, if we write 3:[5,6,9,34]
-- It will return us [3,5,6,9,34]
-- i.e. the : operator acts as a "prepend" to list
-- so, n1: [n2, n3] = [n1, n2, n3]

-- Now let's try to create an "append" function

append :: Int -> [Int] -> [Int]
append n [] = [n]
append n (x:xs) = x : (append n xs)