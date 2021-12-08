import Data.List
import qualified Data.Map as M
import System.Environment
import Data.Maybe
import Control.Monad.Trans.State
import Control.Monad
import Common.Parse


data Case = Case { patterns :: [String] , output :: [String] } deriving (Show)

invLookup :: (Eq a, Eq k) => a -> M.Map k a -> k
invLookup v = fst . head . M.toList . M.filter (==v)

parseCase :: String -> Case
parseCase = evalState $ liftM2 Case
    (replicateM 10 (delim ' '))
    (trimN 2 >> replicateM 4 (delim ' '))

numEasyDigits :: Case -> Int
numEasyDigits = length . filter cond . output
    where
        cond x = let z = length x in z == 2 || z == 3 || z == 4 || z == 7 

getOfLen :: Int -> Case -> String
getOfLen l = head . filter ((==l) . length) . patterns

multiLookup :: (Ord k) => [k] -> M.Map k a -> [a]
multiLookup xs m = mapMaybe (`M.lookup` m) xs

listDiff :: Eq a => [a] -> [a] -> [a]
listDiff cutList = filter (`notElem` cutList)

conversionMap :: Case -> M.Map Char Char
conversionMap _case =
    let
        ins = patterns _case 
        counts = foldr step M.empty (concat ins) 
            where step c m' = M.insertWith (+) c 1 m'
        
        -- Segments b, e and f have unique occurrences among the 10 patterns
        -- b == 6, e == 4, f == 9
        bef = [('b', countCond 6 counts), ('e', countCond 4 counts), ('f', countCond 9 counts)]
        mBEF = M.fromList bef

        c = head . listDiff (multiLookup "f" mBEF) $ getOfLen 2 _case
        mBEFC = M.insert 'c' c mBEF

        d = head . listDiff (multiLookup "bcf" mBEFC) $ getOfLen 4 _case
        mBEFCD = M.insert 'd' d mBEFC

        a = head . listDiff (multiLookup "cf" mBEFCD) $ getOfLen 3 _case
        mBEFCDA = M.insert 'a' a mBEFCD

        ml = multiLookup "abcdef" mBEFCDA
        g = head . filter (`notElem` ml) $ "abcdefg"

        final = M.insert 'g' g mBEFCDA
    in M.fromList (map (\(x, y) -> (y, x)) (M.toList final))
    where
        countCond n counts = fst . head . filter ((==n) . snd) . M.toList $ counts

dl :: String -> Int
dl "cf" = 1
dl "acdeg" = 2
dl "acdfg" = 3
dl "bcdf" = 4
dl "abdfg" = 5
dl "abdefg" = 6
dl "acf" = 7
dl "abcdefg" = 8
dl "abcdfg" = 9
dl "abcefg" = 0
dl s = error $ "bad int: " ++ s

getDigit :: Case -> Int
getDigit c = let
    cm = conversionMap c
    digits = map (dl . sort . map (fromJust . (`M.lookup` cm))) (output c)
    in snd $ foldr step (0, 0) digits
    where
        step d (i, acc) = (i + 1, d * 10 ^ i + acc)



main = do
    contents <- getArgs >>= readFile . head

    let cases = map parseCase . lines $ contents
    print $ sum . map numEasyDigits $ cases
    print $ sum . map getDigit $ cases