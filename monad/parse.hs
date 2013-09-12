
import qualified	Data.ByteString.Lazy.Char8		as L8
import qualified	Data.ByteString.Lazy			as L
import				Data.Char (isSpace)
import				Data.Int

--Chain Monad (I don't actually know if this qualifies as a monad).

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

--Parse Monad (I don't actually know if this qualifies as a monad).

data ParseState = ParseState {
	string	:: L.ByteString,
	offset	:: Int64
} deriving (Show)

newtype Parse a = Parse {
	runParse	:: ParseState -> Either String (a, ParseState)
}

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser where
	chainedParser initState = case runParse firstParser initState of
		Left errMessage					-> Left errMessage
		Right (firstResult, newState)	-> runParse (secondParser firstResult) newState

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

--Bind to Monad instance
instance Monad Parse where
	return = identity
	(>>=) = (==>)
	fail = bail
