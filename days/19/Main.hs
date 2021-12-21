import Debug.Trace
import Control.Monad.Trans.State
import qualified Data.Map as M
import Control.Monad
import Common.Parse
import qualified Data.Set as S
import System.Environment
import Data.Maybe

type Space = S.Set Pt
type Vec = (Int, Int, Int)
type Pt = (Int, Int, Int)

-- Parse

parseCoord :: String -> Pt
parseCoord = let
    d = read <$> delim ','
    rest = read <$> get
    in evalState (liftM3 (,,) d d rest)


parseScanner :: State [String] Space
parseScanner = S.fromList <$> (takeN 1 >> ps)
    where ps = do
                v <- get
                if null v || null (head v)
                    then takeN 1 >> return []
                    else liftM2 (:) (parseCoord . head <$> takeN 1) ps


parseScanners :: State [String] [Space]
parseScanners = do
    s <- get
    if null s
        then return []
        else liftM2 (:) parseScanner parseScanners

parse :: String -> [Space]
parse = evalState parseScanners . lines

-------------------------------------------------------

diff :: Pt -> Pt -> Vec
(x, y, z) `diff` (x', y', z') = (x - x', y - y', z - z')

translate :: Vec -> Pt -> Pt
translate (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

neg :: Vec -> Vec
neg (x, y, z) = (-x, -y, -z)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : as) = Just a

-- Determine a vector which translates at least 12 points from
-- the first space to the second space.
getTranslationVec :: Space -> Space -> Maybe Vec
getTranslationVec s1 s2 = let
    vecs = [v2 `diff` v1 | v1 <- S.toList s1, v2 <- S.toList s2]
    in safeHead (filter (\v -> numAlign v s1 s2 >= 12) vecs)
    where
        numAlign v s1 s2 = let
            s1' = S.map (translate v) s1
            in S.size (S.intersection s1' s2)

-- ############## Rotation

type Rotation = Pt -> Pt
transforms :: [Rotation]
transforms = [
    -- z up
    id, -- e.g. don't rotate
    \(x, y, z) -> (-y, x, z),
    \(x, y, z) -> (-x, -y, z),
    \(x, y, z) -> (y, -x, z),
    -- -z up
    \(x, y, z) -> (x, -y, -z),
    \(x, y, z) -> (y, x, -z),
    \(x, y, z) -> (-x, y, -z),
    \(x, y, z) -> (-y, -x, -z),
    -- y up
    \(x, y, z) -> (x, -z, y),
    \(x, y, z) -> (z, x, y),
    \(x, y, z) -> (-x, z, y),
    \(x, y, z) -> (-z, -x, y),
    -- -y up
    \(x, y, z) -> (x, z, -y),
    \(x, y, z) -> (-z, x, -y),
    \(x, y, z) -> (-x, -z, -y),
    \(x, y, z) -> (z, -x, -y),
    -- x up
    \(x, y, z) -> (-z, y, x),
    \(x, y, z) -> (-y, -z, x),
    \(x, y, z) -> (z, -y, x),
    \(x, y, z) -> (y, z, x),
    -- -x up
    \(x, y, z) -> (z, y, -x),
    \(x, y, z) -> (-y, z, -x),
    \(x, y, z) -> (-z, -y, -x),
    \(x, y, z) -> (y, -z, -x)
 ]

find :: Space -> Space -> Maybe (Rotation, Vec)
find s1 s2 = safeHead (mapMaybe (\r -> (\x -> (r, x)) <$> getTranslationVec s1 (S.map r s2)) transforms)

doAThing :: Space -> M.Map Int Space -> Maybe (Space, M.Map Int Space)
doAThing s0 m = let
    r = safeHead $ mapMaybe (\(i, s') -> (\x -> (i, s', x)) <$> find s0 s') (M.assocs m)
    in case r of 
        Just (i, s', (rot, v)) -> let s1 = S.map (translate (neg v)) (S.map rot s') in Just (S.union s0 s1, M.delete i m)
        Nothing -> Nothing

part1 :: Space -> M.Map Int Space -> Space
part1 s m | M.null m = s
          | otherwise = case doAThing s m of
                            Just (s', m') -> traceShow (S.size s') (part1 s' m')
                            Nothing -> error "can't do it"
main = do
    contents <- getArgs >>= readFile . head
    
    let (s0, ss) = splitAt 1 $ parse contents
    let m = M.fromList (zip [0..] ss)

    print $ S.size $ part1 (head s0) m