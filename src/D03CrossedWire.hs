{-# LANGUAGE RecordWildCards #-}

module D03CrossedWire where

import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import Data.List (elemIndex, minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

data Direction = U | R | D | L deriving (Eq, Show)
data WeightedDirection = WD
    { direction :: Direction
    , units :: Int
    } deriving (Eq, Show)
type Point = (Int, Int)
type Path = [Point]


parseWeightedDirection :: String -> WeightedDirection
parseWeightedDirection ('R':xs) = WD R (read xs)
parseWeightedDirection ('D':xs) = WD D (read xs)
parseWeightedDirection ('L':xs) = WD L (read xs)
parseWeightedDirection ('U':xs) = WD U (read xs)
parseWeightedDirection _ = error "Invalid input"


parse :: String -> [WeightedDirection]
parse = map parseWeightedDirection . splitOn ","


weightedDirectionToPath :: WeightedDirection -> Point -> Path
weightedDirectionToPath WD{..} =
    let
        incrementFunction =
            case direction of
                U -> (\(x, y) -> (x, y + 1))
                D -> (\(x, y) -> (x, y - 1))
                R -> (\(x, y) -> (x + 1, y))
                L -> (\(x, y) -> (x - 1, y))
    in
        take units . tail . iterate incrementFunction


weightedDirectionsToPath :: Point -> [WeightedDirection] -> Path
weightedDirectionsToPath _ [] = []
weightedDirectionsToPath start (x:xs) = res ++ weightedDirectionsToPath (last res) xs
    where res = weightedDirectionToPath x start


pathIntersections :: Path -> Path -> [Point]
pathIntersections path1 path2 = Set.toList $ Set.intersection (Set.fromList path1) (Set.fromList path2)


intersectionPoints :: [WeightedDirection] -> [WeightedDirection] -> [Point]
intersectionPoints wp1 wp2 = pathIntersections (expand wp1) (expand wp2) where expand = weightedDirectionsToPath (0, 0)


manhattenDistanceFromOrigin :: Point -> Int
manhattenDistanceFromOrigin (x, y) = abs x + abs y


manhattenDistanceToClosestIntersection :: String -> String -> Int
manhattenDistanceToClosestIntersection ws1 ws2 =
    minimum . map manhattenDistanceFromOrigin $ intersectionPoints (parse ws1) (parse ws2)


-- Part II

totalSteps :: Path -> Path -> Point -> Int
totalSteps p1 p2 p =
    let
        idx1 = fromJust $ elemIndex p p1
        idx2 = fromJust $ elemIndex p p2
    in
        idx1 + idx2


-- (+2) hack to compensate for the first step which is ignored
closestSteps' :: Path -> Path -> Int
closestSteps' p1 p2 = (+2) . minimum . map (totalSteps p1 p2) $ pathIntersections p1 p2


closestSteps :: String -> String -> Int
closestSteps ps1 ps2 = closestSteps' (weightedDirectionsToPath (0,0) $ parse ps1) (weightedDirectionsToPath (0,0) $ parse ps2)


main = getContents >>=
    print . ((\[x, y] -> manhattenDistanceToClosestIntersection x y) &&& (\[x, y] -> closestSteps x y)) . lines


