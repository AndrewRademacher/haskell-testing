module Main where

main :: IO ()
main = do
	putStrLn "Generate Nth Fib: "
	n <- getLine
	putStrLn ""
	putStrLn (show (fibs !! (read n)))

fibs :: [Integer]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs) ]
