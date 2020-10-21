-- 2013/2014 --
--Zadanie 1--                                                  0
-- 1 2 3 4 / 1 2 4 3 / 1 2
perm :: [a] -> [[a]]
perm []     = return []
perm (x:xs) = (perm xs) >>= (ins x)
    where
    ins :: a -> [a] -> [[a]]
    ins x []     = [[x]]
    ins x (y:ys) = [x:y:ys] ++ ( map (y:) (ins x ys) )
--Zadanie 2--
--takeWhile (<50) (map kw [0..]) where kw x = x*x
--data Choice = Nothing | Maybe Integer
programik :: IO ()
programik = do putStr "Wprowadź napis: "
               cs <- getLine
               if palindrom cs
                  then putStrLn "Tak"
                  else putStrLn "Nie"

palindrom :: String -> Bool
palindrom x = (x == reverse x)
--is3 :: Integer -> Integer
--is3 0 = 0
--is3 x = if  (x**(1/3)) - (fromInteger(round(x ** (1/3)))) < 3e-13 then 5 else 4

--Zadanie 3--

-- 2014/2015 --
--Zadanie 1--
--Zadanie 2--
--Zadanie 3--

-- 2015/2016 --
--Zadanie 1--
--Zadanie 2--
--Zadanie 3--

-- 2016/2017 --                                 1
--Zadanie 1-- 
zipM :: [Maybe a] -> [Maybe b] -> Maybe [(a,b)] 
zipM [] _ = Just []
zipM _ [] = Just []
zipM (x:xs) (y:ys) = Just ((fromJust x, fromJust y) : fromJust (zipM xs ys))

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."

--Zadanie 2--
fmape :: [a -> a] -> a -> [a] 
fmape [] d = []
fmape (x:xs) d = (:) (x d) (fmape xs d) 

--Zadanie 3--

-- 2017/2018 --                            
--Zadanie 1--
prefixes :: [a] -> [[a]]
prefixes [] = [[]]
--prefixes (x:xs) = [] : (map (x:) (prefixes xs))
prefixes (x:xs) = ((.) (flip (:)) (map (x:)) . prefixes) xs []




--prefixes (x:xs) = [x] ++ (prefixes xs)
--Zadanie 2--
comparer :: [Maybe a] -> Maybe a -> Maybe a
comparer [] d = d 
comparer (x:xs) d =	if xs == [] then
						d >>= x
					else
						(comparer xs d) (>>=) x

				
--Zadanie 3--

-- 2018/2019 --
--Zadanie 1--
divBy3 :: Integer -> Bool
divBy3 x = if  ((.) (==) . (rem)) x 3 0 then True else False

count_in_list :: [Integer] -> Integer
count_in_list [] = 0
count_in_list (x:xs) = if divBy3 x then ( (.) (flip (.) count_in_list)  (+) ) 1 xs else count_in_list xs

f :: [[Integer]] -> Integer
f [] = 0
f (x:xs) = if ((.) divBy3 count_in_list) x then ( (.)  (flip (.) f) (+)) 1 xs else f xs



--Zadanie 2--






--Zadanie 3--
