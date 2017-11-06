len :: [t] -> Int
len []     = 0
len (x:xs) = 1 + len xs

-- A bad way of computing the length of a list
len' xs = sum [1 | x <- xs]

----------------------------------------

last' :: [t] -> t
last' []     = error "Empty list"
last' [x]    = x
last' (x:xs) = last' xs