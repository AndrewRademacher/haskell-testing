
import           Data.Maybe
import           Control.Monad.Cont
import           Control.Monad.Trans

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
        return  = MaybeT . return . Just
        x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                              case maybe_value of
                                  Nothing    -> return Nothing
                                  Just value -> runMaybeT $ f value

instance (Monad m) => MonadPlus (MaybeT m) where
        mzero     = MaybeT $ return Nothing
        mplus x y = MaybeT $ do maybe_value <- runMaybeT x
                                case maybe_value of
                                    Nothing -> runMaybeT y
                                    Just _  -> return maybe_value

instance MonadTrans MaybeT where
        lift = MaybeT . (liftM Just)

isValid :: String -> Bool
isValid s = length s >= 8

getValidPassword :: MaybeT IO String
getValidPassword = do s <- lift getLine
                      guard (isValid s)
                      return s

askPassword :: MaybeT IO ()
askPassword = do lift $ putStrLn "Insert your new password: "
                 value <- msum $ repeat getValidPassword
                 lift $ putStrLn "Storing in database..."
