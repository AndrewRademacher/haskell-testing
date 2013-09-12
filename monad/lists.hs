
--	Define Data Model

data Broker = Broker {
	bName	:: String,
	clients	:: [Client]
} deriving (Eq, Show)

data Client = Client {
	cName		:: String,
	investments	:: [Investment]
} deriving (Eq, Show)

data Investment = Investment {
	iName	:: String,
	markets	:: [Market]
} deriving (Eq, Show)

data Market = Market {
	mName	:: String,
	balance	:: Integer
} deriving (Eq, Show)

--	Sample Data

colin = Broker {
	bName = "Colin",
	clients = [Client {
		cName = "Fred",
		investments = [
			Investment {
				iName = "Shares",
				markets = [(Market "Japan" 7000), (Market "America" 8888)]	
			},
			Investment {
				iName = "Gold",
				markets = [(Market "China" 3333)]
			}
		]
	}]
}

-- Implementation

getBrokerTotal :: Broker -> Integer
getBrokerTotal b = sum $ clients colin >>= investments >>= markets >>= (\m -> [balance m])

getUglyBrokerTotal :: Broker -> Integer
getUglyBrokerTotal b = sum $ concatMap (\m -> [balance m]) $ concatMap markets $ concatMap investments $ clients colin
