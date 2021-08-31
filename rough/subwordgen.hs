-- Copied from a student probem
-- who was trying to generate the subwords of a word

swg [] = []
swg [x] = [x]
swg (x:y:xs) = (x:xs) ++ ", " ++ (y:xs) ++ ", " ++ (x:y:xs) ++ ", " ++ swg (x:xs) ++ ", " ++ swg (y:xs)

