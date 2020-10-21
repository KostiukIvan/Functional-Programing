													--Kolos 2013 -- Done
			--Zadanie 1 --
f :: Integer ->[[Integer]]
f 0 = []
f n = map (0:) (f (n-1)) ++ map (1:) (e (n - 1))

e :: Integer -> [[Integer]]
e 0 = [[]]
e n = map (0:) (e (n-1)) ++ map (1:) (f (n-1))

b :: Integer-> [[Integer]]
b 0 = [[]]
b n = map (0:) (b (n-1)) ++ map (1:) (b (n -1))

f2 n = filter (even.foldl(+)0)(b n)

			--Zadanie 2 --
diffsum :: [[Integer]] -> [[Integer]]
diffsum [] = [[]]
diffsum (x:xs) = if (foldl (+) 0)x `isin` map(foldl (+) 0)xs then diffsum xs else x : diffsum xs

isin :: Integer -> [Integer] -> Bool
isin l [] = False
isin l (t:arr) = if l == t then True else isin l arr


			--Zadanie 3--
compref :: [Integer] -> [Integer] -> Integer
compref [] [] = 0
compref (x:xs) (y:ys) = if x == y then 1 + compref xs ys else compref xs ys

															--Kolos 2014 -- Done
			--Zadanie 1 --
numocc :: Integer -> [[Integer]] -> [Integer]
numocc k l = map(counter) (map(filter(==k))l)
 
counter :: [Integer] -> Integer
counter [] = 0
counter (x:xs) = 1+ counter xs

			--Zadanie 2 --
data CT a = Empty | Leaf a | Join (CT a) Op (CT a)
data Op = Add | Neg | Mul

wf :: CT a -> Bool
wf Empty = False
wf (Join Empty Neg y) = wf y
wf (Join x Neg Empty) = wf x
wf (Join _ Neg _ ) = False
wf (Join Empty Add _ ) = False
wf (Join _ Add Empty) = False
wf (Join x Add y) = wf x && wf y
wf (Join Empty Mul _ ) = False
wf (Join _ Mul Empty) = False
wf (Join x Mul y) = wf x && wf y
wf _ = True

eval :: Num a => CT a -> a
eval Empty = error "Zle"
eval (Join Empty Neg y) = -eval y
eval (Join x Neg Empty) = -eval x
eval (Join _ Neg _) = error "Zle"
eval (Join Empty Add _) = error "Zle"
eval (Join _ Add Empty) = error "Zle"
eval (Join x Add y) = eval x + eval y
eval (Join Empty Mul _) = error "Zle"
eval (Join _ Mul Empty) = error "Zle"
eval (Join x Mul y) = eval x * eval y
eval (Leaf x) = x

			--Zadanie 3 --
h :: [a] -> [a]
h [] = []
h (x:xs) = fwrite(x:xs)

fwrite :: [a] -> [a]
fwrite [] = []
fwrite (x:xs) = x : gwrite(xs)

gwrite :: [a] -> [a]
gwrite [] = []
gwrite (x:xs) = fwrite(xs)


															--Kolos 2015 -- Done
			--Zadanie 1 --
dlugosc :: [Char] -> Integer
dlugosc [] = 0
dlugosc (x) = if check x  then 0 else 1+ dlugosc( first x ) 

first :: [Char] -> [Char]
first [] = []
first (x:xs) = iter x xs

iter :: Char ->[Char] -> [Char]
iter e [] = [e]
iter e (x:xs) 
	| e == 'a' && x == 'b' = 'a' : first(xs)
	| e == 'a' && x == 'a' = 'a':'a':'a':first(xs)
	| e == 'b' && x == 'a' = 'b' : first(xs)
	| e == 'b' && x == 'b' = 'a' : first(xs)

check :: [Char] -> Bool
check (x:xs) = if cases x xs then True else len (x:xs) < 2

cases :: Char -> [Char] -> Bool
cases e [] = True
cases e (x:xs) = if e == x then cases x xs else False

len :: [Char] -> Integer
len [] = 0
len (x:xs) = 1 + len(xs)

			--Zadanie 2 --
val :: Integer -> Integer -> Integer
val 0 b = 0
val x b = if x `mod` b == 0 then 1 + val (x `div` b) b else 0

val_g ::Integer -> Integer -> [Integer]
val_g k v = val_g_start 100 k v

val_g_start :: Integer -> Integer -> Integer ->[Integer]
val_g_start 0 k v = []
val_g_start n k v = if val n k == v then n : val_g_start (n-1) k v else val_g_start (n-1) k v

			--Zadanie 3 --
data Klos a = Body [a] [a]
	deriving Show

wnpk :: Klos a -> a -> Klos a
wnpk (Body bg fn) el = (Body (el:bg) fn)

wnkk :: Klos a -> a -> Klos a
wnkk (Body bg fn) el = (Body bg (el : fn))

k2list :: Klos a -> [a]
k2list (Body bg fn) = bg ++ (krevers fn)

krevers :: [a] -> [a]
krevers l = foldl (flip (:)) [] l

																--Kolos 2016 --
			--Zadanie 1 --
pownum :: Integer -> [Integer]
pownum n = generater 0 n

generater :: Integer -> Integer -> [Integer]
generater 0 n = [] ++ generater 1 n
generater i n = if (checker i n) == i then i : generater (i+1) n else generater (i+1) n

checker :: Integer -> Integer -> Integer
checker 0 n = 0
checker x n = potenga (x `mod` 10) n + checker (x `div` 10) n

potenga :: Integer -> Integer -> Integer
potenga x 0 = 1
potenga x n = x * (potenga x (n-1))

			--Zadanie 2 --
ps :: [a]  -> [[a]]
ps [] = []
ps (x:xs) = psreverse (psgener (psreverse (x:xs))) ++ pssufiks xs

pssufiks :: [a] -> [[a]]
pssufiks [] = []
pssufiks (x:xs) = (x:xs) : pssufiks xs 

psgener :: [a] -> [[a]]
psgener [] = []
psgener (x:xs) = psreverse (x:xs) : psgener (xs) 

psreverse :: [a] -> [a]
psreverse = foldl (flip (:)) []
			--Zadanie 3 --
data Rd a =  Node a [Rd a]

el :: Eq a => Rd a -> a -> Bool
el (Node x []) e = if x == e then True else False
el (Node x (y:ys)) e = if x == e then True else el y e ||  any( == True) (map(flip el e) ys)

any1 :: [Bool] -> Bool
any1 [] = False
any1 (x:xs) = if x == True then True else any1 xs

subset :: Eq a => a -> a -> Rd a -> Rd a
subset x y (Node cur []) = if x == cur then (Node y []) else (Node cur [])
subset x y (Node cur (f:ls)) = if x == cur then (Node y (map (subset x y) (f:ls))) else (Node cur (map (subset x y) (f:ls)))

rd2list :: Rd a -> [a]
rd2list (Node cur []) = [cur]
rd2list (Node cur (x:xs)) = cur : (concat $ map rd2list (x:xs) )
																--Kolos 2017 --
--Zadanie 1 --
rd :: Integer -> [Integer]
rd n = rdgener (n*n) n

rdgener :: Integer-> Integer -> [Integer]
rdgener 1 n = []
rdgener k n = if (sumdiv k 2) == (n) then k : rdgener (k-1) n else  rdgener (k-1) n 

sumdiv:: Integer -> Integer -> Integer
sumdiv 2 d = 1
sumdiv 1 d = 1
sumdiv k d = if k `mod` d == 0 then d + sumdiv ( divuntill (k `div` d) d) d  else sumdiv k (d+1) 

divuntill :: Integer -> Integer -> Integer
divuntill i d = if i `mod` d == 0 then divuntill (i `div` d) d else i

--Zadanie 2 --
repl :: Eq a => [a] -> [(a, a)] -> [a]
repl [] l = []
repl (x:xs) l = if (replis x (unzip (l)) ) == True then (replelem x (unzip (l))) : (repl xs l) else x : (repl xs l)

replis :: Eq a => a -> ([a],[a]) -> Bool
replis e ([],[]) = False
replis e  ((x:xs),(y:ys)) = if e == x then True else replis e (xs,ys)

replelem ::Eq a => a -> ([a],[a]) -> a
replelem e  ((x:xs),(y:ys)) = if e == x then y else replelem e (xs,ys)

--Zadanie 3 --	
data Sdb a = SEmpty | SNode a (Sdb a) | DNode a (Sdb a) (Sdb a)

sel :: Eq a => Sdb a -> a -> Bool
sel (SEmpty) e = False
sel (SNode x child) e = if x == e then True else sel child e
sel (DNode x left right) e = if x == e then True else (sel left e) || (sel right e)


seq :: Eq a => Sdb a -> Sdb a -> Bool
seq a b = all(==True)(seqhelp a b)

seqhelp :: Eq a => Sdb a -> Sdb a -> [Bool]
seqhelp (SEmpty) (SEmpty) = [True]
seqhelp (SEmpty) _ = [False]
seqhelp  _ (SEmpty) = [False]
seqhelp (SNode a achild) (SNode b bchild) = if a==b then True : seqhelp achild bchild else [False]
seqhelp (SNode a achild) _ = [ False ] 
seqhelp  _ (SNode b bchild) = [ False ] 
seqhelp (DNode a alchild archild) (DNode b blchild brchild) = if a == b then  True:  [(  ( all(==True) (seqhelp alchild blchild) ) &&  ( all(==True) (seqhelp archild brchild) )  ) || (  ( all(==True) (seqhelp archild blchild) ) &&  ( all(==True) (seqhelp alchild brchild) )  )]
															else [False]
seqhelp (DNode a alchild archild) _ = [False]
seqhelp _ (DNode a alchild archild)= [False]
--seq (DNode 5 (DNode 3 (SNode 2 (SEmpty)) (SNode 4 (SEmpty)) )(DNode 1 (SNode 6 (SEmpty))(SNode 7 (SEmpty))))  (DNode 5 (DNode 1 (SNode 6 (SEmpty)) (SNode 7 (SEmpty)) )(DNode 3 (SNode 2 (SEmpty))(SNode 4 (SEmpty)))) 

--BFS--
sdb2list :: Eq a =>(Sdb a) -> [a]
sdb2list t  = sdbhelp t []  
--sdb2list (DNode 1 (DNode 3 (SNode 7 (SEmpty)) 										   (SNode 6 (SEmpty)) )			   						(DNode 2 (SNode 5 (SEmpty))										   (SNode 4 ((DNode 8 (DNode 10 (SNode 14 (SEmpty)) 									       (SNode 13 (SEmpty)) )			   					    (DNode 9 (SNode 12 (SEmpty))									       (SNode 11 (SEmpty))))))))  

sdbhelp :: Eq a => (Sdb a) -> [(Sdb a)] ->[a]
sdbhelp SEmpty []  = []
sdbhelp (SNode a achild) [] = a : sdbhelp  achild [achild] 
sdbhelp (DNode a alchild archild) [] = a : sdbhelp  alchild [(alchild) , (archild)]
sdbhelp  t (x:xs) =  (getval x) ++ (sdbhelp  x (xs ++ (getchildren x)))

getchildren :: Sdb a -> [(Sdb a)]
getchildren SEmpty = []
getchildren (SNode a ch) = [ch]
getchildren (DNode a cha chb) = [(cha) , (chb)]

getval :: Sdb a -> [a]
getval SEmpty = []
getval (SNode a ch) = [a]
getval (DNode a cha chb) = [a]


																--Kolos 2018 --
			--Zadanie 1 --	

aaa :: Integer -> Integer
aaa 0 = 1
aaa (x + 1) = x*bbb x - 3*aaa x

bbb :: Integer -> Integer
bbb 0 = 1
bbb (x + 1) = 3*bbb x + x*x*aaa x - x*x

suma :: (Integer->Integer) -> Integer ->Integer
suma foo 0 = 0
suma foo x = foo x + suma foo (x-1)


counter2 :: (Integer -> Integer) -> Integer -> Integer -> Integer
counter2 foo 0 m = counter2 foo 1 m
counter2 foo x m = if (suma foo x) > m then x else counter2 foo (x+1) m 

seqIndex :: Integer -> Integer
seqIndex m = counter2 aaa 0 m

			--Zadanie 2 --
data Expr a = Value a
	| Addad (Expr a) (Expr a)
	| Mulal (Expr a) (Expr a)
	| Subab (Expr a) (Expr a)
	| P
ekuk :: (Eq a) => Expr a -> Expr a -> Bool
ekuk (Value a) (Value b) = if a == b then True else False 
ekuk (Addad af ac) (Addad bf bc) = if (ekuk af bf) && (ekuk ac bc) then True else False
ekuk (Mulal af ac) (Mulal bf bc) = if (ekuk af bf) && (ekuk ac bc) then True else False
ekuk (Subab af ac) (Subab bf bc) = if (ekuk af bf) && (ekuk ac bc) then True else False
ekuk P _ = True
ekuk _ P = True
ekuk _ _ = False
 
			--Zadanie 3 --
cykl :: [Integer] -> [[Integer]]
cykl l = pefor l l
--cykl l = foldl (++) [l] [gener (lenth l) l]

pefor :: [Integer] -> [Integer] ->[[Integer]]
pefor v c = if (krok v) == c then [c] else [krok v] ++ pefor (krok v) c

gener :: Integer -> [Integer] -> [[Integer]]
gener 0 arr = []
gener l arr = [krok arr] ++ gener (l-1) (krok arr)

krok :: [Integer] -> [Integer]
krok (x:xs) = xs ++ [x] 

lenth :: [Integer] -> Integer
lenth [] = 0
lenth (x:xs) = 1 + lenth xs





																-- Final Project --
--Function take set of words in a parameter.
--This function will return False if words are not a code. It worst cause it mean that this set of words is a code.				
is_code :: [String] -> Bool
is_code code = logical_and (show_all_causes code) 



--Function take set of words in a parameter.
--This function will print set of tuples like [(["01","1"],["011"],False),(["011","1","011"],["01110","1","1"],False),
--(["01","1","1","011"],["01110","1","1"],False),(["1","1","1","011"],["1110","1","1"],False),
--(["1"],["10011"],True),(["01","011"],["010","1","1"],False),(["1","1","011"],["1101","1"],False),([],[],True)]
--It is used to show, how does our program works. First and second element of tuple its are our words which we try to build. If our words are equal, then
--third elemnt will be False. In worst case our third element will be True.
show_all_causes :: [String] -> [([String], [String], Bool)]
show_all_causes code = build_words_for_all_pair code (find_pair_prefix_string code code code [] [])


--Function take 6 parameters. First code words, second is prefix, third is word, fourth and fiveth are like second and third.  
--Sixth paremeter have to be empty. It is buffer for data.
--                    code       first       second      first    second  
try_to_build_word :: [String] ->[String] -> [String] -> String -> String -> String ->([String],[String],Bool)
try_to_build_word [] _ _ _ _ _= ([],[],True)
try_to_build_word code fx sx f s [] = 	if (len f) > (len s) then
											--add suffix to s
											if (get_words_for_prefix code (get_dangling_suffix s f) []) == [] then
												(fx, sx, True)
											else
												build_words_for_all_prefixs code fx sx f s (get_words_for_prefix code (get_dangling_suffix s f) [])
										else
											if len f == len s then
												--return True
												(fx,sx,False)
											else
												--add suffix to f
												if (get_words_for_prefix code (get_dangling_suffix f s) []) == [] then
													(fx, sx, True)
												else
													build_words_for_all_prefixs code fx sx f s (get_words_for_prefix code (get_dangling_suffix f s) [])											
try_to_build_word code fx sx f s p =  	if (len f) > (len s) then
											--add suffix to s
											try_to_build_word code fx (sx++[p]) f (s++p) []
										else
											if len f == len s then
												--return True
												(fx,sx,False)
											else
												--add suffix to f
												try_to_build_word code (fx++[p]) sx (f++p) s []



--This function run function try_to_build_word for while parameter prefix is not emty
--								code       first       second      first    second    prefixes    first     second  IsCode														
build_words_for_all_prefixs :: [String] ->[String] -> [String] -> String -> String -> [String] ->([String],[String],Bool)
build_words_for_all_prefixs code fx sx f s [] = (fx, sx, True)
build_words_for_all_prefixs code fx sx f s (p:px) = try_to_build_word code fx sx f s p


				
--This function will be try to build words for every pair in our set (["1","0","","001"]["10","00110","00110"]) 
--					   code    	  prefixes     words
build_words_for_all_pair :: [String] -> ([String],[String]) -> [([String], [String], Bool)]
build_words_for_all_pair code ([],[]) = [([],[],True)]
build_words_for_all_pair code ((p:px),(w:wx)) = [try_to_build_word code [p] [w] p w []] ++ (build_words_for_all_pair code (px,wx))



--Check if the first parameter is a prefix of second parameter
is_prefix :: [Char] -> [Char] -> Bool
is_prefix [] _ = True
is_prefix (x:xs) [] = False --If second string is smaller from first then return False
is_prefix (f_x:f_xs) (s_x:s_xs) = if f_x == s_x then is_prefix f_xs s_xs else False



--Before run this method, check if first string is the prefix of second one
-- return: word - prefix 
get_dangling_suffix :: String -> String -> String
get_dangling_suffix [] x = x
get_dangling_suffix (f_x:f_xs) (s_x:s_xs) = get_dangling_suffix f_xs s_xs 



-- Function belove will return all words which contain our prefix
-- And words which are prefix of our prefix. 
--                       codes      prefix       []
get_words_for_prefix :: [String] -> String -> [String] -> [String] 
get_words_for_prefix [] p [] = []
get_words_for_prefix [] p x = x
get_words_for_prefix (x:xs) p retur = 	if is_prefix p x || is_prefix x p then 
											get_words_for_prefix xs p (retur ++ [x])
										else
											get_words_for_prefix xs p retur



--Function belove return all pairs prefix - word
--                          data       data        data          []         []        prefixes   words
find_pair_prefix_string :: [String] ->[String] -> [String] -> [String] -> [String] -> ([String],[String])
find_pair_prefix_string orig _ [] [] [] = ([], []) -- Do not find anything
find_pair_prefix_string orig _ [] prefix words = (prefix, words) 
find_pair_prefix_string orig [] (s_x:s_xs) prefix words = find_pair_prefix_string orig orig s_xs prefix words
find_pair_prefix_string orig (f_x:f_xs) (s_x:s_xs) prefix words = 	if f_x /= s_x && is_prefix f_x s_x then 
																		find_pair_prefix_string orig f_xs (s_x:s_xs) (prefix ++ [f_x]) (words ++ [s_x]) 
																	else 
																		find_pair_prefix_string orig f_xs (s_x:s_xs) prefix words



logical_and :: [([String],[String],Bool)] -> Bool
logical_and [] = True
logical_and (x:xs) = (return_third_elem x )&& logical_and xs 

return_third_elem :: ([String], [String], Bool) -> Bool
return_third_elem (f,s,th) = th



																		--Tests
-- is_code ["1","011","01110","1110","10011"]
-- is_code ["01","011","0"]
-- is_code ["01","011","0","001"]
-- is_code ["01","011","0","101"]
-- is_code ["01","011","0","101"]
--Function belove will be print additional information.
-- show_all_causes ["1","011","01110","1110","10011"]
-- show_all_causes ["01","011","0"]
-- show_all_causes ["01","011","0","001"]
-- show_all_causes ["01","011","0","101"]
-- show_all_causes ["01","011","0","101"]
















