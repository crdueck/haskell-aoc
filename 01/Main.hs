module Main where

import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..), First(..))
import Data.Set (Set)
import Text.ParserCombinators.ReadP as P
import qualified Data.Set as Set

testCases :: [([String], Int)]
testCases =
    [ (["+1", "+1", "+1"], 3)
    , (["+1", "+1", "-2"], 0)
    , (["-1", "-2", "-3"], -6)
    ]

newtype Action = Action { runAction :: Sum Int }

newAction :: (Int -> Int) -> String -> Action
newAction k = Action . Sum . k . read

instance Read Action where
    readsPrec _ = P.readP_to_S $ do
        sig <- P.get
        tok <- P.munch1 isDigit
        return $ case sig of
          '-' -> newAction negate tok
          _   -> newAction id     tok

solution :: [String] -> Int
solution = getSum . foldMap (runAction . read)

scan :: Monoid m => [m] -> [m]
scan = scanl mappend mempty

solution' :: [String] -> Maybe Int
solution' = fmap getSum . findDup . scan . cycle . map (runAction . read)

findDup :: Ord a => [a] -> Maybe a
findDup = either Just (const Nothing) . foldM k Set.empty
    where k seen a =
            if Set.member a seen
            then Left a
            else Right (Set.insert a seen)

main = do
    input <- lines <$> readFile "01/input.txt"
    print (solution  input)
    print (solution' input)
