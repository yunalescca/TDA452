xs = replicate 10 "Haskell"

ys = map (take 2) xs 

-- :sprint xs will only print xs = _, because it hasn't calculated anything yet
-- :sprint ys                 ys = _
-- ys !! 5                    "Ha"
-- :sprint ys                 ys = _ : _ : _ : _ : _ : "Ha" : _ 
-- :sprint xs                 xs = ('H' : 'a' : _) : ('H' : 'a' : _) : ('H' : 'a' : _) :
--                                 ('H' : 'a' : _) : ('H' : 'a' : _) : ('H' : 'a' : _) : _


-----------------------------------------------------------------------------

