module Logger (
	Logger,
	Log,
	runLogger,
	record
) where

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
--	(>>=) :: Logger a -> (a -> Logger b) -> Logger b
	m >>= k =
		let	(a, w)	= execLogger m
			n		= k a
			(b, x)	= execLogger n
		in Logger (b, w ++ x)
	return a = Logger (a, [])

