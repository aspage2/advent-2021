module Common.Parse where

import Control.Monad.Trans.State ( state, State )

trimLeft :: Char -> State String String
trimLeft c = state $ \s -> ("", dropWhile (==c) s)

delim :: Char -> State String String
delim c = state $ \s -> let (fst, rest) = break (==c) s in (fst, tail rest)
