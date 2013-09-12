{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, ExtendedDefaultRules #-}

module Main where

import          Control.Applicative
import          Snap.Core
import          Snap.Util.FileServe
import          Snap.Http.Server
import			GHC.Generics (Generic)
import			Data.Typeable.Internal (Typeable)
import			Data.Either (either)
-- JSON
import			Data.Aeson (FromJSON, ToJSON, decode, encode)
import			Data.ByteString.Char8 (pack)
import			Data.ByteString.UTF8 (fromString, toString)
-- Mongo
import			Database.MongoDB
import			System.IO.Unsafe (unsafePerformIO)
import			Data.Bson
import			Data.Bson.Generic

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
	route [
		("foo", writeBS "bar"),
		("echo/:echoparam", echoHandler),
		("contact", contactHandler),
		("contact/:id", contactGet)
	] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

-- Add contact

data Address = Address {
	line1	:: String,
	line2	:: Maybe String,
	city	:: String,
	state	:: String,
	zip		:: Int
} deriving (Eq, Show, Generic, Typeable)
instance FromJSON Address
instance ToJSON Address
instance FromBSON Address
instance ToBSON Address

data Contact = Contact {
	first	:: String,
	middle	:: String,
	last	:: String,
	phone	:: String,
	email	:: String,
	address	:: Address
} deriving (Eq, Show, Generic, Typeable)
instance FromJSON Contact
instance ToJSON Contact
instance FromBSON Contact
instance ToBSON Contact

pipe = unsafePerformIO $ runIOE $ connect $ host "localhost"
run act = access pipe master "web" act

contactGet :: Snap ()
contactGet = do
	id <- getParam "id"
	maybe	(writeBS "You must provide an id.")
			(\id -> do
				doc <- run $ findOne $ select ["_id" =: (read (toString id) :: ObjectId)] "contacts"
				either	(\f -> writeBS "Error reading database.")
						(\d -> maybe	(writeBS "No document found.")
										(\d -> maybe	(writeBS "Unable to parse document.")
														(\c -> writeLBS $ encode c)
														(fromBSON d :: Maybe Contact))
										d)										
						doc)
			id

contactHandler :: Snap ()
contactHandler =	method GET get		<|> method POST post <|>
					method PUT put where
	get = do
		writeBS "Get - not yet implemented"
	post = do
		json <- readRequestBody 1024
		maybe	(writeBS "Couldn't decode message")
				(\c -> do
					res <- run $ insert "contacts" $ toBSON c
					either	(\f -> writeBS "Save Failed")
							(\r -> writeBS $ fromString $ show r)
							res)
				(decode json :: Maybe Contact)
	put = do
		json <- readRequestBody 1024
		let (contact) = decode json :: Maybe Contact
		maybe (writeBS "Couldn't decode message")
			(\c -> writeLBS $ encode c) contact
