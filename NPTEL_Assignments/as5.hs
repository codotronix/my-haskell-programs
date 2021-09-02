{-
1. Define a function subSeq :: String -> String -> Bool which checks whether 
the first argument is a subsequence of the second. A subsequence is obtained by 
deleting some letters in a string and retaining the other characters in the same 
order as in the original string.

Test cases:
subSeq "ab" "abc" = True
subSeq "ab" "acb" = True
subSeq "ab" "bca" = False
subSeq ""   "bea" = True
subSeq "ba" "ba"  = True
-}

subSeq :: String -> String -> Bool
subSeq "" _ = True
subSeq s1 s2 = isSubSeq s1 s2 (length s1) (length s2)


isSubSeq :: String -> String -> Int -> Int -> Bool
isSubSeq s1 s2 l1 l2
    | l1 == 0 = True
    | l2 <= 0 = False
    | head s1 == head s2 = isSubSeq (tail s1) (tail s2) (l1-1) (l2-1)
    | head s1 /= head s2 = isSubSeq s1 (tail s2) l1 (l2-1)

------------------------------------------------------------------------------------------------------------

{-
2. Define a function subWord :: String -> String -> Bool which checks whether 
the first argument is a subword of the second. A subword is obtained by deleting 
some number (possibly 0) of letters at the left end and right end in a string 
and retaining the other characters in the same order.

Test cases:
subWord "ab" "abc" = True
subWord "ab" "acb" = False
subWord "ca" "bca" = True
subWord ""   "bea" = True
subWord "ba" "ba"  = True

Note the, subWord "ab" "acb" = False
that means in a word, we can't skip any letter once the word has started
-}

subWord :: String -> String -> Bool
subWord "" _ = True
subWord s1 s2 = isSubWord s1 s2 (length s1) (length s2) s1 False

isSubWord :: String -> String -> Int -> Int -> String -> Bool -> Bool
isSubWord s1 s2 l1 l2 s1Original isWordStarted
    | l1 == 0 = True
    | l2 <= 0 = False
    | head s1 == head s2 = isSubWord (tail s1) (tail s2) (l1-1) (l2-1) s1Original True
    | head s1 /= head s2 && isWordStarted == False = isSubWord s1Original (tail s2) (length s1Original) (l2-1) s1Original isWordStarted
    | head s1 /= head s2 && isWordStarted == True = isSubWord s1Original (tail s2) (length s1Original) (l2-1) s1Original False

------------------------------------------------------------------------------------------------------------


{-
3. A two-dimensional matrix can be represented as a list of rows, each row 
itself being a list of elements. So in general it is of type [[a]]. Not every 
list of lists is a matrix, though. For instance, [[1,2,3], [], [2,4]] is a list 
of three lists, each of a different size.
-}

{-
(a) Define a function isMatrix :: [[a]] -> Bool that checks if a list of lists 
is a valid matrix (nonzero number of rows, each of the same nonzero length).

Test cases:
isMatrix [] = False
isMatrix [[],[],[]] = False
isMatrix [[2,3], [4,5], [6,7]] = True 
isMatrix [[2,3,4,5,6,7]] = True
-}

isMatrix :: [[a]] -> Bool
isMatrix [] = False
isMatrix [x] = True                                                -- only 1 element
isMatrix [x,y]                                                      -- when only 2 elements left 
    | length x == 0 = False
    | length x == length y = True
    | otherwise = False
isMatrix (x:xs)                                                     -- more than 2
    | length x == length (head xs) = isMatrix xs
    | otherwise = False


----------------------------------------------------------------------------------------------

{-
(b) A square matrix is one where the number of rows is equal to the number of 
columns. Define a function isSquareMatrix :: [[a]] -> Bool that checks if a 
list of lists is a square matrix.

Test cases:
isSquareMatrix [] = False
isSquareMatrix [[]] = False
isSquareMatrix [[1]] = True
isSquareMatrix [[1,2,3],[4,5,6],[7,8,9]] = True
isSquareMatrix [[1,2,3,4],[5,6,7,8],[9,10,11,12]] = False
-}

isSquareMatrix :: [[a]] -> Bool
isSquareMatrix x
    | not (isMatrix x) = False
    | length x == length (head x) = True
    | otherwise = False

-----------------------------------------------------------------------------------------------
{-
(c) Two matrices are addable if they have the same number of rows and same 
number of columns. Define a function addable :: [[a]] -> [[a]] -> Bool that 
checks if two matrices are addable.

Test cases: 
addable [[1,2],[3,4]] [[1,2],[3,4]] = True
addable [[1,2],[3,4]] [[5,6,7],[8,9,10]] = False
addable [[1,2],[3,4]] [[1,2],[3,4],[3,4]] = False
-}

addable :: [[a]] -> [[a]] -> Bool
addable x y
    | not (isMatrix x) = False
    | not (isMatrix y) = False
    | length x /= length y = False
    | length (head x) /= length (head y) = False
    | otherwise = True

-------------------------------------------------------------------------------------------------

{-
(d) Define a function addMatrices :: [[Int]] -> [[Int]] -> [[Int]] that computes 
the sum of the input matrices.

Test cases:
addMatrices [[1,2]] [[3,4]] = [[4,6]]
addMatrices [[1,2],[3,4]] [[1,2],[3,4]] = [[2,4],[6,8]]
-}

addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices [x] [y] = [addArray x y]
addMatrices x y = (addArray (head x) (head y)) : addMatrices (tail x) (tail y)

addArray :: [Int] -> [Int] -> [Int]
addArray [] y = y
addArray x y = ( head x + head y ): addArray (tail x) (tail y)

-------------------------------------------------------------------------------------------------

{-
(e) Matrix m1 is multiplyable with matrix m2 if the number of columns in m1 is 
the same as the number of rows in m2. Define a function 
multiplyable :: [[a]] -> [[a]] -> Bool that checks if matrix m1 is 
multiplyable with m2.

Test cases:
multiplyable [[1,2,3],[4,5,6]] [[1,2],[3,4]] = False
multiplyable [[1,2,3],[4,5,6],[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = True
-}

multiplyable :: [[a]] -> [[a]] -> Bool
multiplyable x y
    | not (isMatrix x) = False
    | not (isMatrix y) = False
    | length (head x) /= length y = False
    | otherwise = True

------------------------------------------------------------------------------------------------
{-
(f) Define a function multiplyMatrices :: [[Int]] -> [[Int]] -> [[Int]] that 
computes the product of the input matrices.

Test cases:
multiplyMatrices [[1,2],[3,4]] [[1,2,3],[4,5,6]] = [[9,12,15],[19,26,33]]
multiplyMatrices [[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = [[22,28],[49,64]]
-}
multiplyMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
multiplyMatrices us vs = map (mult [] vs) us
  where
    mult xs [] _ = xs
    mult xs _ [] = xs
    mult [] (zs:zss) (y:ys) = mult (map (y *) zs) zss ys
    mult xs (zs:zss) (y:ys) = mult (zipWith (\u v -> u + v * y) xs zs) zss ys


multArr :: [Int] -> [Int] -> [Int]
multArr [x] [y] = [x*y]
multArr x y = (head x * head y) : (multArr (tail x) (tail y))
