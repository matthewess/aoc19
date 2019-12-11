module D10MonitoringStation where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn, chunksOf)
import Data.List (intercalate, sort, groupBy, sortBy, maximumBy, transpose)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Function (on)



groupByEquivalence :: Ord b => (a -> b) -> [a] -> [[a]]
groupByEquivalence relation xs =
    map (map fst) . groupBy ((==) `on` snd) . sortBy (comparing snd) $ zip xs (map relation xs)


-- Part I

type Point = (Int, Int)


polarAngleWRTPoint :: Point -> Point -> Double
polarAngleWRTPoint (x, y) (a, b) = pi - (atan2 (fromIntegral $ a - x) (fromIntegral $ b - y))


radius :: Point -> Point -> Int
radius (x, y) (a, b) = (a - x) ^ 2 + (b - y) ^ 2


uniqueAnglesWRTPoint :: Point -> [Point] -> Int
uniqueAnglesWRTPoint p = (subtract 1) . length . groupByEquivalence (polarAngleWRTPoint p)


asteroidOnlyCoords :: String -> [Point]
asteroidOnlyCoords x =
    let
        assign :: Int -> [Char] -> [(Point, Char)]
        assign y xs = zip (zip [0..] (repeat y)) xs

        astroidOnlyCoords =
            map fst . filter ((== '#') . snd) . concat $ zipWith assign [0..] (lines x)
    in
        astroidOnlyCoords


maxCoverageAchievableForBestMonitoringSystem :: [Point] -> Int
maxCoverageAchievableForBestMonitoringSystem asteroids =
    maximum $ map (\a -> uniqueAnglesWRTPoint a asteroids) asteroids


p1 :: String -> Int
p1 = maxCoverageAchievableForBestMonitoringSystem . asteroidOnlyCoords


-- Part II

bestStationLocation :: [Point] -> Point
bestStationLocation asteroids =
    maximumBy (comparing (\a -> uniqueAnglesWRTPoint a asteroids)) asteroids


asteroidDestructionFrom :: Point -> [Point] -> [[Point]]
asteroidDestructionFrom point =
    groupByEquivalence (polarAngleWRTPoint point)


clockwiseAsteroidDestruction :: Point -> [Point] -> [Point]
clockwiseAsteroidDestruction p =
    concat . transpose . map (sortBy (comparing (radius p))) . asteroidDestructionFrom p


p2 :: String -> Int
p2 aMap =
    let
        ast = asteroidOnlyCoords aMap
        bestPoint = bestStationLocation ast
    in
        (\(x, y) -> x * 100 + y) . (!! 199) $ clockwiseAsteroidDestruction bestPoint ast


-- Runner

main :: IO ()
main = getContents >>=
    print . p2
