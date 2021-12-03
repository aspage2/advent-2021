import Data.List
import System.Environment
import Control.Applicative

toBools :: String -> [Bool]
toBools = map (=='1')

acc :: [[Bool]] -> [Int]
acc bs = foldr foldop zeros bs where
    zeros = map (const 0) (head bs)
    foldop = zipWith (\b x -> if b then x + 1 else x - 1)

gamma :: [[Bool]] -> Int
gamma = toInt . map (>0) . acc

epsilon :: [[Bool]] -> Int
epsilon = toInt . map (<0) . acc

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]


mcb :: Int -> [[Bool]] -> Int
mcb i = foldr (foldop . (!!i)) 0
    where foldop v acc = if v then acc + 1 else acc - 1

filterox :: Int -> [[Bool]] -> [[Bool]]
filterox i bs = case mcb i bs of 
    v | v >= 0 -> filter (!!i) bs
      | otherwise -> filter (not . (!!i)) bs

filterco2 :: Int -> [[Bool]] -> [[Bool]]
filterco2 i bs = case mcb i bs of
    v | v >= 0 -> filter (not . (!!i)) bs
      | otherwise -> filter (!!i) bs


filterStep :: (Int -> [[Bool]] -> [[Bool]]) -> [[Bool]] -> Int
filterStep f ls = _fs 0 ls where
    len = length . head $ ls
    _fs _ [] = undefined 
    _fs _ [line] = toInt line
    _fs i ls' = let newls = f (i `mod` len) ls' in _fs (i + 1) newls


ogr :: [[Bool]] -> Int
ogr = filterStep filterox

csr :: [[Bool]] -> Int
csr = filterStep filterco2

toInt :: [Bool] -> Int
toInt bs = let
    pairs = enumerate (map conv . reverse $ bs)
    in sum . map (\(n, p) -> p * (2 ^ n)) $ pairs
    where conv True = 1
          conv False = 0

toStr :: [[Bool]] -> String
toStr = intercalate "\n" . map (map (\b -> if b then '1' else '0'))

powerConsumption :: [[Bool]] -> Int
powerConsumption = liftA2 (*) gamma epsilon

main :: IO ()
main = do
    payload <- getArgs >>= readFile . head
    let bs = map toBools . lines $ payload
    print $ liftA2 (*) ogr csr bs
