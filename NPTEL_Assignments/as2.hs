-- run below fn with input [1..5]

f1 = g1 (\x -> x)
g1 k [] = k 100
g1 k (x:xs) = g1 ((x*) . k) xs


-- run the below program with 6
f2 = snd . g2
g2 0 = (0,0)
g2 n = let (x,y) = g2 (n-1)
      in (x+1, y-x)