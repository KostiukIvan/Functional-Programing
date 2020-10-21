data Mb a = Lista [a] [a]

dnp :: Mb a -> a -> Mb a
dnp (Lista f s) e = (Lista (e : f) s) 

dnk :: Mb a -> a -> Mb a
dnk (Lista f s) e = (Lista f (e : s)) 


mb2list :: Mb a -> [a]
mb2list (Lista f s) = f ++ (revers s)
--mb2list (dnp (dnp (dnp (dnk (dnk (dnk (Lista [] []) 6) 5) 4) 3) 2) 1)
--[1,2,3,6,5,4]

ull :: Mb a -> Mb a
ull (Lista f s) = (Lista [] s)
--mb2list (ull (dnp (dnp (dnp (dnk (dnk (dnk (Lista [] []) 6) 5) 4) 3) 2) 1))
--[6,5,4]

upl :: Mb a -> Mb a
upl (Lista s f) = (Lista s [])
--mb2list (upl (dnp (dnp (dnp (dnk (dnk (dnk (Lista [] []) 6) 5) 4) 3) 2) 1))
--[1,2,3]


revers :: [a] -> [a]
revers l = foldl (flip (:)) [] l
