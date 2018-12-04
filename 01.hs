module Main where

import Data.Char (isDigit)
import Data.Monoid (Sum(..))
import Text.ParserCombinators.ReadP as P

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

main = mapM_ (print . solution . fst) testCases
