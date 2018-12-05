{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (zipWithM)
import Data.List as L (delete, tails, find)
import Data.Map (Map)
import Data.Monoid (Sum(..),First(..))
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
                              then two <> three
                              else two
            | F.any (==3) m = three
            | otherwise = mempty

findFirstDifference :: Eq a => [a] -> [a] -> Maybe a
findFirstDifference [] _ = Nothing
findFirstDifference _ [] = Nothing
findFirstDifference (x:xs) (y:ys)
    | x /= y    = Just y
    | otherwise = findFirstDifference xs ys

hamming :: Eq a => [a] -> [a] -> Int
hamming xs = getSum . mconcat . zipWith k xs
    where k a b = if a == b then 0 else 1

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- not sure which lens this is, I'm sure it's defined somewhere
someLens :: Lens a (b, a) a b
someLens afb a = (\b -> (b, a)) <$> afb a

firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . foldMap First

solution' :: [String] -> Maybe String
solution' xs = uncurry L.delete <$> firstJust ys
    where ys = zipWith k xs (L.tails xs)
          k a = go
            where go [] = Nothing
                  go (x:xs)
                    | hamming a x == 1 = findFirstDifference a `someLens` x
                    | otherwise        = go xs

main = do
    inputs <- lines <$> readFile "02/input.txt"
    print (solution  inputs)
    print (solution' inputs)
