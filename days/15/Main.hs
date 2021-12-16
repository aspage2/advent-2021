import Data.Maybe
import Data.List
import Debug.Trace
import System.Environment
import qualified Data.Map as M
import qualified Data.PQueue.Min as PQ

type Vertex = (Int, Int)
type Edges = M.Map (Vertex, Vertex) Int


neighbors :: (Int, Int) -> Vertex -> [Vertex]
neighbors (numrow, numcol) (r, c) = filter cond candidates
    where
        cond (r', c') = 0 <= r' && r' < numrow && 0 <= c' && c' < numcol 
        candidates = [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]

c2i '0' = 0
c2i '1' = 1
c2i '2' = 2
c2i '3' = 3
c2i '4' = 4
c2i '5' = 5
c2i '6' = 6
c2i '7' = 7
c2i '8' = 8
c2i '9' = 9
c2i _ = undefined

{- Day 15 - It's just astar

We're looking for the optimal path from (0, 0) to (n, m) on the grid,
which is literally just A* search.

We can use a heuristic function h((r, c)) = (height - r) + (width - c),
which is just the taxi-cab distance from the current point to the end.
Because we can't possible overestimate the cost at a node (we can't be
more direct and cheaper than a taxicab path of all 1's), h is admissable,
so we are guaranteed to find the optimal path.

-}

parse :: String -> Edges
parse s = foldr step M.empty (zip [0..] concatd)
    where
        ls = lines s
        width = length . head $ ls
        height = length ls
        concatd = concat ls
        step (i, cost) m = let 
                        cell' = (i `div` width, i `mod` width)
                        ns = neighbors (height, width) cell'
                        in M.union (M.fromList $ map (\n -> ((n, cell'), c2i cost)) ns) m

data QE = QE Vertex Int

instance Eq QE where
    (==) (QE _ i) (QE _ j) = i == j

instance Ord QE where
    (<=) (QE _ i) (QE _ j) = i <= j

rebuild :: M.Map Vertex Vertex -> Vertex -> [Vertex]
rebuild vs = unfoldr step
    where
        step v | v == (0, 0) = Nothing
               | otherwise = M.lookup v vs >>= (\v' -> Just (v', v'))

h :: (Int, Int) -> Vertex -> Int
h (nr, nc) (r, c) = nr - r + nc - c - 2

search :: (Int, Int) -> Edges -> [Vertex]
search bounds = _search 
    bounds
    M.empty
    (M.singleton (0, 0) 0)
    (M.singleton (0, 0) h')
    (PQ.singleton $ QE (0, 0) h')
    where
        h' = h bounds (0, 0)

inf = 1 / 0

_search :: (Int, Int) -> M.Map Vertex Vertex -> M.Map Vertex Int -> M.Map Vertex Int -> PQ.MinQueue QE -> Edges -> [Vertex]
_search bounds cameFrom gScore fScore pq es = let
    (QE current _, pq') = PQ.deleteFindMin pq
    step v' (cf', gs', fs', pq') = let
        tentativeGScore = fromJust (M.lookup (current, v') es) + d current v'
        in if tentativeGScore < fromMaybe inf (M.lookup v' gs')
    in if current == bounds 
       then rebuild cameFrom current 
       else let
           (cameFrom', gScore', fScore', pq'') = foldr step (cameFrom, gScore, fScore, pq') (neighbors bounds current)
            in _search bounds cameFrom' gScore' fScore' pq'' es
       where
           d v1 v2 = fromJust $ M.lookup (v1, v2) es




main = do
    contents <- getArgs >>= readFile . head

    print $ parse contents