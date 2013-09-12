
import System.IO

main = do
	putStrLn "Generate Nth Fib: "
	n <- getLine
	putStrLn "Enter output file: "
	fileName <- getLine
	writeFile fileName (show (fibs !! (read n)))
--	writeFile fileName (show (take n fibs))
--	writeFile fileName (show (fibs !! (read n)))

fibs :: [Integer]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs) ]
