module D08SpaceImageFormat where


import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Data.List (minimumBy, intercalate)
import Data.Ord (comparing)


-- Part I

imageWidth = 25
imageHeight = 6


total :: Int -> [Int] -> Int
total n = length . filter (== n)


getLayers :: Int -> Int -> [Int] -> [[Int]]
getLayers width height = chunksOf (width * height)


layerWithMinZeros :: [[Int]] -> [Int]
layerWithMinZeros = minimumBy (comparing (total 0))


onesMultTwos :: [Int] -> Int
onesMultTwos = (uncurry (*)) . (total 1 &&& total 2)


p1 :: [Int] -> Int
p1 = onesMultTwos . layerWithMinZeros . getLayers imageWidth imageHeight


-- Part II

-- NOTE: No pixel will be transparent
resolvePixel :: [Int] -> Int
resolvePixel = head . dropWhile (== 2)


-- NOTE: all layers are same size
mergeLayers :: [[Int]] -> [Int]
mergeLayers xs
    | all null xs = []
    | otherwise = resolvePixel (map head xs) : mergeLayers (map tail xs)


imageShow :: Int -> String
imageShow 0 = " "
imageShow 1 = "█"


-- helpful for human eye decoding of message
formatForDisplay :: [[Int]] -> String
formatForDisplay = unlines .  map (intercalate "" . map imageShow)


p2 :: [Int] -> String
p2 = formatForDisplay . chunksOf imageWidth . mergeLayers . getLayers imageWidth imageHeight


-- Runner

main :: IO ()
main = do
    contents <- getContents
    print . p1 . map digitToInt . init $ contents
    putStr . p2 . map digitToInt . init $ contents

