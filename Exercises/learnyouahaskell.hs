-- * Examples from Learn You A Haskell


head' = head [1,2,3] -- prints 1

tail' = tail [1,2,3] -- prints [2,3]

last' = last [1,2,3] -- prints 3

init' = init [1,2,3] -- prints [1,2]

-----------------------------------------------------------------------------

take' n = take n [1,2,3] -- prints the first n element from the list

drop' n = drop n [1,2,3] -- drops  the first n element from the list

-----------------------------------------------------------------------------

elem' n = n `elem` [1,2,3,4] -- checks if n is an element in the list

-----------------------------------------------------------------------------

range1 = [1..20]
range2 = [2,4..20]
range3 = [(-1)..(-5)] -- prints empty list


-----------------------------------------------------------------------------

cycle'  n = take n (cycle [1,2,3]) -- cycle loops a list infintely 

repeat' n = take n (repeat 5) -- repeat loops an element infintely

-----------------------------------------------------------------------------

-- removes non-uppercase letters from a string
removeNotUpperCase string = [ c | c <- string, c `elem` ['A'..'Z']]

onlyEvenNumbers xxs = [ [ x | x <- xs, even x ] | xs <- xxs]

-----------------------------------------------------------------------------

zip1  = zip [1,2,3,4,5] [6,7,8,9,10]
zip2  = zip [1,2,3,4,5] [6,7,8]
zip3' = zip [1..]       ["Orange", "Apple", "Pear", "Mango"]

-----------------------------------------------------------------------------




























