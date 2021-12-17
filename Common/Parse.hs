module Common.Parse where

import Control.Monad.Trans.State ( state, State )

takeN :: Int -> State String String
takeN n = state $ splitAt n

trimN :: Int -> State String () 
trimN n = state $ \s -> ((), drop n s)

trimLeft :: Char -> State String ()
trimLeft c = state $ \s -> ((), dropWhile (==c) s)

delim :: Char -> State String String
delim c = state $ \s -> let (fst, rest) = break (==c) s in (fst, drop 1 rest)
