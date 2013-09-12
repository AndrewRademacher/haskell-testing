
data Address = Address {
	line1	:: String,
	line2	:: String,
	city	:: String,
	state	:: String,
	zip		:: Int
} deriving (Show)

data Contact = Contact {
	firstName	:: String,
	middleName	:: Maybe String,
	lastName	:: String,
	address		:: Address,
	phone		:: String,
	email		:: String
} deriving (Show)
