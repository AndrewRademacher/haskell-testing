{-# LANGUAGE TemplateHaskell #-}

import           Test.QuickCheck
import           Test.QuickCheck.All

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevId :: [Int] -> Bool
prop_RevId xs = reverse xs == xs

runTests = $quickCheckAll
