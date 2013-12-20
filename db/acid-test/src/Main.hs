{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Monad.Reader (ask)
import           Control.Monad.State (modify)
import           Data.Text
import           Data.Time
import           Data.Acid
import           Data.Function
import qualified Data.IntMap                        as IntMap
import           Data.List
import           Data.Ord
import           Data.SafeCopy
import           Data.Typeable

data Failure = Failure { failureReason :: Text
                       , failureTime   :: UTCTime
                       } deriving (Show, Typeable)

data FailureDB = FailureDB { allFailures :: IntMap.IntMap Failure }
               deriving (Typeable)

failuresOverTime :: Query FailureDB [Failure]
failuresOverTime = 
        sortBy (comparing failureTime) . IntMap.elems . allFailures <$> ask

addFailure :: Failure -> Update FailureDB ()
addFailure failure = modify go
    where go (FailureDB db) = FailureDB $
                case IntMap.maxViewWithKey db of
                    Just ((max, _), _) -> IntMap.insert (max + 1) failure db
                    Nothing            -> IntMap.singleton 1 failure

deriveSafeCopy 0 'base ''Failure
deriveSafeCopy 0 'base ''FailureDB
makeAcidic ''FailureDB ['failuresOverTime, 'addFailure]

main :: IO ()
main = do
        state <- openLocalState (FailureDB IntMap.empty)

        -- Record a new failure
        now <- getCurrentTime
        update state (AddFailure $ Failure "ENOMISSLES" now)

        -- Query for all failures
        allFailures <- query state FailuresOverTime

        mapM_ print allFailures

