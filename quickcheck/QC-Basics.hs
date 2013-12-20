{-# LANGUAGE TemplateHaskell #-}

import           Test.QuickCheck
import           Test.QuickCheck.All
import           Data.List

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs

checkAll = $quickCheckAll

prop_sort_model :: [Int] -> Bool
prop_sort_model xs = sort xs == qsort xs

prop_idempotent :: [Int] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum :: [Int] -> Property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered :: [Int] -> Bool
prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation :: [Int] -> Bool
prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum :: [Int] -> Property
prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

prop_append :: [Int] -> [Int] -> Property
prop_append xs ys = not (null xs) ==>
                    not (null ys) ==>
                        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)
