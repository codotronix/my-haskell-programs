-- To multiply 2 numbers x, y
-- we will keep on halfing x and doubling y, at each step,
-- until x becomes 0
-- then we will add up all the y values for which x is odd
-- that will be result of x*y

mult :: Int -> Int -> Int
mult x y = helpmult x y 0

-- let's have 's' to store the sum of all prev y's
helpmult :: Int -> Int -> Int -> Int
helpmult 0 y s = s
helpmult x y s 
    | mod x 2 == 0 = s + (helpmult (div x 2) (y * 2) s)
    | otherwise = s + y + (helpmult (div x 2) (y * 2) s)



-- THE BETTER APPROACH
-- FROM THE BOOK of ALGORITHMS by DASGUPTA
multbetter :: Int -> Int -> Int
multbetter _ 0 = 0
multbetter x y
    | mod y 2 == 0 = 2 * (multbetter x (div y 2))
    | otherwise = x + 2 * (multbetter x (div y 2))