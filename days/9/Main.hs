import Debug.Trace
import Control.Monad.Trans.State 
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import System.Environment
import qualified Data.Array as A
import Data.Char
import Control.Monad

{- Day 9 - Basins

Gee wow, this is hot garbage
-}

type Ind = (Int, Int)
type Grid = A.Array Ind Int

parseGrid :: String -> Grid
parseGrid s = let
    ls = lines s
    height = length ls
    width = length (head ls)
    vs = zipWith (\x y -> ((x `div` width, x `mod` width), digitToInt y)) [0..] (concat ls)
    in A.array ((0, 0), (height - 1, width - 1)) vs

localMinima :: Grid -> [Ind]
localMinima g = filter cond idxs
    where
        (_, (h, w)) = A.bounds g
        n = neighbors (w, h)
        idxs = [(r, c) | r <- [0..h], c <- [0..w]]
        cond (r, c) = let v = g A.!(r,c) 
                      in all (\ idx -> g A.! idx > v) (n (r, c))

neighbors :: (Int, Int) -> Ind -> [Ind]
neighbors (w, h) (r, c) = filter cond [(r + 1, c), (r, c + 1), (r - 1, c), (r, c - 1)]
    where cond (r', c') = r' >= 0 && c' >= 0 && r' <= h && c' <= w

getBasin :: Ind -> Grid -> S.Set Ind
getBasin (r, c) g = _gb (r, c) S.empty
    where
        (_, (h, w)) = A.bounds g
        _gb cell' s' = if g A.! cell' == 9 
            then s'
            else let
                s'' = S.insert cell' s'
                ns = filter (\c -> S.notMember c s'') (neighbors (w, h) cell')
                in foldr _gb s'' ns

type GridState = [S.Set Ind]

visited :: Ind -> GridState -> Bool
visited i = any (S.member i)

getBasinS :: Ind -> Grid -> State GridState ()
getBasinS i g = let inds = getBasin i g 
    in
        state $ \basins -> ((), inds : basins)

getAllBasin :: Ind -> Grid -> State GridState ()
getAllBasin st g = _gab st
    where
        (_, (h, w)) = A.bounds g
        idxs = [(r, c) | r <- [0..h], c <- [0..w]]
        cond vs ind = not (visited ind vs) && g A.! ind /= 9
        _gab i = if g A.! i == 9 then return () else do
                      getBasinS i g
                      vs <- get
                      case take 1 $ filter (cond vs) idxs of
                            [] -> return ()
                            [i'] -> _gab i'
                            _ -> undefined

main = do
    g <- parseGrid <$> (getArgs >>= readFile . head)
    print $ sum . map ((+1) . (g A.!)) . localMinima $ g
    print $ getBasin (0, 3) g
    let basins = execState (getAllBasin (0, 0) g) []

    print . negate . product . take 3 . sort . map (negate . length) $ basins
