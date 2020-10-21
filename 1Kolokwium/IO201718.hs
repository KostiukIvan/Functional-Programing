compr :: [IO()] -> IO()
compr x = foldr (>>) (return ()) x 

main = do compr [putChar 'a',
            do putChar 'b'
               putChar 'c',
            do c <- getChar
               putChar c,
			   putChar 'A']






-- the type of monad m
--data m a = ...

-- return takes a value and embeds it in the monad.
--return :: a -> m a

-- bind is a function that combines a monad instance m a with a computation
-- that produces another monad instance m b from a's to produce a new
-- monad instance m b--
--(>>=) :: m a -> (a -> m b) -> m b
