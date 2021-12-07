
import System.Environment
import Common.Helper

type FuelModel = Int -> Int -> Int

simpleFuelModel :: FuelModel
simpleFuelModel to from = abs (to - from)

realFuelModel :: FuelModel
realFuelModel to from = let n = abs (to - from) in n * (n + 1) `div` 2

fuelCost :: FuelModel -> [Int] -> Int -> Int
fuelCost fm crabs i = sum $ map (fm i) crabs

-- Brute-force solution. The calculation is small, so we can get away with it.
-- The problem space can probably be pruned but :shrug:
minFuelCost :: FuelModel -> [Int] -> Int
minFuelCost fm crabs = let
    mn = minimum crabs
    mx = maximum crabs
    in minimum $ map (fuelCost fm crabs) [mn..mx]

problemParse :: String -> [Int]
problemParse = map read . splitOn ','

main = do
    d <- problemParse <$> (getArgs >>= readFile . head)
    print $ minFuelCost simpleFuelModel d
    print $ minFuelCost realFuelModel d
