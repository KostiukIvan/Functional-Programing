import Control.Monad

main = 	do
		let loop = do
				putStrLn "Your text:"
				text <- getLine
				putStrLn $ "Reversed text is : " ++ (revers text)
				when (text /= ".") loop
		loop 
		putStrLn "After the loop"
			
			
			
			
			
revers :: String -> String
revers = foldl (flip (:)) []