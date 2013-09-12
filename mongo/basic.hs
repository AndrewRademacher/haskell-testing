
import Database.MongoDB
import Control.Monad.Trans (liftIO)

data Address = Address {
	line1	:: String,
	line2	:: Maybe String,
	city	:: String,
	state	:: String,
	zip		:: Int
} deriving (Show)

data Person = Person {
	first	:: String,
	middle	:: String,
	last	:: String,
	email	:: String,
	phone	:: String,
	address	:: Address
} deriving (Show)

main = do
	
