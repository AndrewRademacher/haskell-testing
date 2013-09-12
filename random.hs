
import System.Random

main = do
	setStdGen (mkStdGen 42)	--optional
	s <- randomStuff
	print s
	s <- randomStuff
	print s
	s <- randomStuff
	print s

randomStuff :: IO [Float]
randomStuff = do
	n <- randomRIO (1, 7)
	sequence (replicate n (randomRIO (0, 1)))
