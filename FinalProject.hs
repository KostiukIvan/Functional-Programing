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

len :: [Char] -> Integer
len [] = 0
len (x:xs) = 1 + len(xs)



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
















