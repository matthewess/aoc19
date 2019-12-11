{-# LANGUAGE QuasiQuotes #-}
module D10MonitoringStationTest where

import D10MonitoringStation (Point, uniqueAnglesWRTPoint, asteroidOnlyCoords, p1, asteroidDestructionFrom)

import Test.HUnit

asteroidField1 :: String
asteroidField1 = unlines
    [ ".#..#"
    , "....."
    , "#####"
    , "....#"
    , "...##"
    ]


asteroidField2 :: String
asteroidField2 = unlines
    [ "......#.#."
    , "#..#.#...."
    , "..#######."
    , ".#.#.###.."
    , ".#..#....."
    , "..#....#.#"
    , "#..#....#."
    , ".##.#..###"
    , "##...#..#."
    , ".#....####"
    ]

asteroidCoords1 :: [Point]
asteroidCoords1 = [(1,0),(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(3,4),(4,4)]


testAsteroidOnlyCoords =
    [ TestLabel "" $ TestCase $ assertEqual "" asteroidCoords1 (asteroidOnlyCoords asteroidField1)
    ]


testUniqueAnglesWRTPoint = 
    [ TestLabel "" $ TestCase $ assertEqual "" 8 (uniqueAnglesWRTPoint (3,4) asteroidCoords1)
    ]


testP1 =
    [ TestLabel "" $ TestCase $ assertEqual "" 8 (p1 asteroidField1)
    , TestLabel "" $ TestCase $ assertEqual "" 33 (p1 asteroidField2)
    ]


asteroidField3 :: String
asteroidField3 = unlines
    [ ".#....#####...#.."
    , "##...##.#####..##"
    , "##...#...#.#####."
    , "..#.....X...###.."
    , "..#.#.....#....##"
    ]


testAsteroidDestruction =
    [ TestLabel "" $ TestCase $ assertEqual "" [] (asteroidDestructionFrom (8,3) $ asteroidOnlyCoords asteroidField3)
    ]


d10Tests :: [Test]
d10Tests = testAsteroidOnlyCoords ++ testUniqueAnglesWRTPoint ++ testP1 ++ testAsteroidDestruction
