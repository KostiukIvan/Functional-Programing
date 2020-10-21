bp :: Int -> [String]
bp n =  map(`++`(foldl (flip (:) ) [] ))(filter(ispalindrom 0 0) (permutations (n `div` 2)))

permutations :: Int-> [String]
permutations 0 = [[]]
permutations n = map ('a':) (permutations (n-1)) ++ map ('b':) (permutations (n -1))

palind :: String -> String
palind l = l ++ (foldl (flip (:) ) [] l)

ispalindrom :: Int -> Int ->  String -> Bool
ispalindrom  a b [] = if a == b then True else False
ispalindrom  a b (x:xs) = if x == 'a' then ispalindrom  (a+1) b xs else ispalindrom  a (b + 1) xs

-- bp 6
--["aaabbb","aababb","aabbab","aabbba","abaabb","ababab","ababba","abbaab","abbaba","abbbaa","baaabb","baabab","baabba","babaab","bababa","babbaa","bbaaab","bbaaba","bbabaa","bbbaaa"]