module D03CrossedWireTest where

import D03CrossedWire
    ( manhattenDistanceToClosestIntersection
    , parse
    , WeightedDirection(..)
    , Direction(..)
    , weightedDirectionsToPath
    , intersectionPoints
    , closestSteps
    )

import Test.HUnit


testManhattanDistance = undefined

testP1 =
    [ TestLabel "" $ TestCase $ assertEqual "" 6 (manhattenDistanceToClosestIntersection "R8,U5,L5,D3" "U7,R6,D4,L4")
    , TestLabel "" $ TestCase $ assertEqual "" 135 (manhattenDistanceToClosestIntersection "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    , TestLabel "" $ TestCase $ assertEqual "" 159 (manhattenDistanceToClosestIntersection "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")
    ]


weightedDirection1 = [WD R 8, WD U 5, WD L 5, WD D 3]
weightedDirection2 = [WD U 7, WD R 6, WD D 4, WD L 4]

path1 =
    [ (1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)
    , (8,1),(8,2),(8,3),(8,4),(8,5)
    , (7,5),(6,5),(5,5),(4,5),(3,5)
    , (3,4),(3,3),(3,2)
    ]


path2 =
    [ (0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7)
    , (1,7),(2,7),(3,7),(4,7),(5,7),(6,7)
    , (6,6),(6,5),(6,4),(6,3)
    , (5,3),(4,3),(3,3),(2,3)
    ]

weightedDirection4 =
    [ WD R 98
    , WD U 47
    , WD R 26
    , WD D 63
    , WD R 33
    , WD U 87
    , WD L 62
    , WD D 20
    , WD R 33
    , WD U 53
    , WD R 51
    ]

weightedDirection3 =
    [ WD U 98
    , WD R 91
    , WD D 20
    , WD R 16
    , WD D 67
    , WD R 40
    , WD U 7
    , WD R 15
    , WD U 6
    , WD R 7
    ]

testParse =
    [ TestLabel "" $ TestCase $ assertEqual ""  weightedDirection1 (parse "R8,U5,L5,D3")
    , TestLabel "" $ TestCase $ assertEqual ""  weightedDirection2 (parse "U7,R6,D4,L4")
    , TestLabel "" $ TestCase $ assertEqual ""  weightedDirection3 (parse "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    , TestLabel "" $ TestCase $ assertEqual ""  weightedDirection4 (parse "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
    ]


testExpansion =
    [ TestLabel "" $ TestCase $ assertEqual ""  path1 (weightedDirectionsToPath (0,0) weightedDirection1)
    , TestLabel "" $ TestCase $ assertEqual ""  path2 (weightedDirectionsToPath (0,0) weightedDirection2)
    ]

testIntersection =
    [ TestLabel "" $ TestCase $ assertEqual "" [(3,3),(6,5)] (intersectionPoints weightedDirection1 weightedDirection2)
    , TestLabel "" $ TestCase $ assertEqual "" [(107,47),(107,51),(107,71),(124,11),(157,18)] (intersectionPoints weightedDirection3 weightedDirection4)
    ]


testClosestSteps =
    [ TestLabel "" $ TestCase $ assertEqual "" 30 (closestSteps "R8,U5,L5,D3" "U7,R6,D4,L4")
    , TestLabel "" $ TestCase $ assertEqual "" 410 (closestSteps "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    , TestLabel "" $ TestCase $ assertEqual "" 610 (closestSteps "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")
    ]


d03Tests :: [Test]
d03Tests = testP1 ++ testParse ++ testExpansion ++ testIntersection ++ testClosestSteps
