{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Bifunctor (second)
import Control.Monad (zipWithM)
import Data.List as L (delete, tails)
import Data.Map (Map)
import Data.Monoid (Sum(..),First(..))
import qualified Data.Foldable as F
import qualified Data.Map as Map

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

firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . foldMap First

solution' :: [String] -> Maybe String
solution' xs = uncurry L.delete <$> firstJust ys
    where ys = zipWith k xs (L.tails xs)
          k xs = firstJust
               . map (fmap swap . strength)
               . tabulate (singleton . diffR xs)

-- right biased difference
diffR :: Eq a => [a] -> [a] -> [a]
diffR [] ys = ys
diffR xs [] = xs
diffR (x:xs) (y:ys)
    | x == y    = diffR xs ys
    | otherwise = y : diffR xs ys

tabulate :: (a -> b) -> [a] -> [(a, b)]
tabulate f = map (\a -> (a, f a))

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

singleton :: [a] -> Maybe a
singleton (x:[]) = Just x
singleton _      = Nothing

strength :: Functor f => (a, f b) -> f (a, b)
strength (a, fb) = fmap (\b -> (a, b)) fb

main = do
    inputs <- lines <$> readFile "02/input.txt"
    print (solution  inputs)
    print (solution' inputs)
