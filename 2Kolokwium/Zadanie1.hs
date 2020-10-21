ce :: [[Int]] -> [Int]	
ce [] = []
ce (x:xs) = --if 0 == (sum x) `mod` 2  then 
			--if (==) (mod (sum x) 2) 0 then
			--if (==) (flip mod 2 (sum x)) 0 then
			--if flip (==) 0 (flip mod 2 (sum x))  then
			
			if flip (==) 0 (flip mod 2 (sum x))  then
				--x ++ ce xs
				--(++) x (ce xs)
				((++) x .ce) xs
			else
				ce xs
			
			
