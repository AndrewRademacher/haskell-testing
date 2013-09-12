
class BasicEq a where
	isEqual			:: a -> a -> Bool
	isEqual x y		= not (isNotEqual x y)

	isNotEqual		:: a -> a -> Bool
	isNotEqual x y	= not (isEqual x y)

instance BasicEq Bool where
	isEqual True True	= True
	isEqual False False	= True
	isEqual _ _			= False

data Color = Red | Green | Blue deriving (Read, Show, Eq, Ord)

main :: IO ()
main = do
	putStrLn "Please enter a double: "
	inpStr <- getLine
	let inpDouble = read inpStr :: Double
	putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))
