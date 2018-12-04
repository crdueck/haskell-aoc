module Main where

import Data.Map (Map)
import Data.Monoid (Sum(..))
import qualified Data.Map as Map
import qualified Data.Foldable as F

testCase :: ([String], Int)
testCase =
    ( ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
    , 12
    )

type S = (Sum Int, Sum Int)

two :: S
two = (1, 0)

three :: S
three = (0, 1)

checksum :: S -> Int
checksum (m, n) = getSum (m * n)

histogram :: String -> Map Char Int
histogram = foldr (Map.alter k) Map.empty
    where k = Just . maybe 1 (+1)

solution :: [String] -> Int
solution = checksum . foldMap (count . histogram)
    where count m
            | F.any (==2) m = if F.any (==3) m
                              then three <> two
                              else two
            | F.any (==3) m = three
            | otherwise = mempty

main = print (solution (fst testCase))
