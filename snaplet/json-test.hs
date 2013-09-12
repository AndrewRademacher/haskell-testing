
{-# LANGUAGE DeriveDataTypeable #-}

import Text.JSON
import Text.JSON.Generic

data User = User {
	screen_name	:: String
} deriving (Eq, Show, Data, Typeable)

data Status = Status {
	user	:: User,
	text	:: String
} deriving (Eq, Show, Data, Typeable)

main :: IO ()
main = do
	json <- readFile "./tweets.json"
	let statuses = decodeJSON json :: [Status]
	putStrLn $ show statuses
	writeFile "./tweets-out.json" (encodeJSON statuses)
