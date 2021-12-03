
module Common.Helper where

countIf :: [Bool] -> Int
countIf = foldr (\b x -> if b then x + 1 else x) 0