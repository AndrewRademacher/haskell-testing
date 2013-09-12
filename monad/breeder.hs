
-- Data Definition

data Person = Person {
	personName	:: Maybe String,
	pet			:: Maybe Pet
} deriving (Eq, Show)

data Pet = Pet {
	petName	:: Maybe String,
	owner	:: Maybe Person,
	mother	:: Maybe Pet
} deriving (Eq, Show)

-- Sample Data

colleen = Person {
	personName = Just "Colleen",
	pet	= (Just (Pet {
		petName = Just "Ebony",
		owner = Nothing,
		mother = (Just (Pet {
			petName = Just "Fluffy",
			owner = (Just (Person {
				personName = Just "Sarah",
				pet = Nothing
			})),
			mother = Nothing
		}))
	}))
}

richard = Person {
	personName = Just "Richard",
	pet = Nothing
}

-- Functions

getBreederName :: Person -> Maybe String
getBreederName p = pet p >>= mother >>= owner >>= personName

getUglyBreederName :: Person -> Maybe String
getUglyBreederName p = maybe	(Nothing)
								(\p -> maybe	(Nothing)
												(\m -> maybe	(Nothing)
																(\o -> personName o)
																(owner m))
												(mother p))
								(pet p)
