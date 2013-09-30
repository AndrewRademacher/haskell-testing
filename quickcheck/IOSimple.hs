{-# LANGUAGE TemplateHaskell #-}

import           Data.Char
import           Test.QuickCheck
import           Test.QuickCheck.All

--  Intangled IO and Computation
getListMix = find 5 where
    find 0 = return []
    find n = do
        ch <- getChar
        if ch `elem` ['a'..'e'] 
            then do
                tl <- find(n - 1)
                return (ch : tl)
            else
                find n

--  Seperated IO and Computation
getList :: IO [Char]
getList = fmap take5 getContents

take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])

--  Testing Properties
--prop_Length :: [Char] -> Bool
prop_Length s = length(take5 s) <= 5

--prop_Filter :: [Char] -> Bool
prop_Filter s = all (`elem` ['a'..'e']) (take5 s)

--  Testing Facilites
deepCheck p = quickCheckWith (stdArgs { maxSuccess = 10000 }) p

testAll = $quickCheckAll
