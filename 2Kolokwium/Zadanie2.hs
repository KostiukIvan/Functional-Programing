import Control.Monad

main = 	do
		let loop = do
				putStrLn "Your text:"
				text <- getLine
				(count text)
				when (text /= ".") loop
		loop 
		putStrLn "After the loop"
			
			
--Tu potrzebno jeszcze dopisac funkcje ktora by conwertowala [(Char,Int)] na String. 
			
			
			
count :: String -> [(Char,Int)]
count [] = []
count (x:xs) = (x, countLet (x:xs) x) : (count (remove xs x))



countLet :: String -> Char -> Int
countLet [] a = 0
countLet (x:xs) a = if x == a then
					1 + countLet xs a
					else
					countLet xs a
					
					
remove :: String -> Char -> String
remove [] a = []
remove (x:xs) a = if x == a then
					remove xs a
					else
					x : remove xs a