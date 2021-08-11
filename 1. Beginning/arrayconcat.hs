-- though we have the '++' operator to concat 2 lists
-- let's create one ourselves

arrayconcat :: [Int] -> [Int] -> [Int]
arrayconcat [] l = l 
arrayconcat (x:xs) l = x : (arrayconcat xs l)