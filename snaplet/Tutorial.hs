
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import				Control.Lens.TH
import				Data.IORef
import qualified	Data.ByteString.Char8 as B
import				Data.Maybe
import				Snap
import				Snap.Snaplet.Heist
import				Part2

data App = App {
	_heist			:: Snaplet (Heist App),
	_foo			:: Snaplet Foo,
	_bar			:: Snaplet Bar,
	_companyName	:: IORef B.ByteString
}

makeLenses ''App

appInit :: SnapletInit App App
appInit = makeSnaplet "myapp" "My example application" Nothing $ do
	hs <- nestSnaplet "heist" heist $ heistInit "templates"
	fs <- nestSnaplet "foo" foo $ fooInit
	bs <- nestSnaplet "" bar $ nameSnaplet "newname" $ barInit foo
	addRoutes [
		("/hello", writeText "hello world"),
		("/fooname", with foo namePage),
		("/barname", with bar namePage),
		("/company", companyHandler) ]
	--	("/test", testHandler) ]
	addRoutes [
		("/test/:id", method GET getTest),
		("/test", method GET getTest),
		("/test", method POST postTest) ]
	wrapSite (<|> heistServe)
	ref <- liftIO $ newIORef "fooCorp"
	return $ App hs fs bs ref

main :: IO ()
main = serveSnaplet defaultConfig appInit

instance HasHeist App where heistLens = subSnaplet heist

namePage :: Handler b v ()
namePage = do
	mname <- getSnapletName
	writeText $ fromMaybe "This shouldn't happen" mname

companyHandler :: Handler App App ()
companyHandler =	method GET getter	<|> method POST setter <|>
					method PUT updater	<|> method DELETE deleter where
	getter = do
		nameRef <- gets _companyName
		name <- liftIO $ readIORef nameRef
		writeBS name
	setter = do
		mname <- getParam "name"
		nameRef <- gets _companyName
		liftIO $ maybe (return ()) (writeIORef nameRef) mname
		getter
	updater = do
		writeBS "update"
	deleter = do
		writeBS "delete"

--Test 
getTest :: Handler App App ()
getTest = do
	param <- getParam "id"
	maybe (writeBS "Get") writeBS param

postTest :: Handler App App ()
postTest = do
	body <- getRequestBody
	writeLBS body

testHandler :: Handler App App ()
testHandler =	method GET get	<|> method POST post <|>
				method PUT put	<|> method DELETE delete where
	get = do
		param <- getParam "id"
		maybe (writeBS "Get") writeBS param
	post = do
		writeBS "Post"
	put = do
		writeBS "Put"
	delete = do
		writeBS "Delete"
