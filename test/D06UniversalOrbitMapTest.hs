module D06UniversalOrbitMapTest where

import Test.HUnit hiding (Node)

import D06UniversalOrbitMap (Graph, fromEdges, totalOrbits, allOrbits, jumpsRequired, pathFromCenter)
import qualified Data.Set as S


g :: Graph
g = fromEdges
    [ ("COM","B")
    , ("B","C")
    , ("C","D")
    , ("D","E")
    , ("E","F")
    , ("B","G")
    , ("G","H")
    , ("D","I")
    , ("E","J")
    , ("J","K")
    , ("K","L")
    ]


g1 :: Graph
g1 = fromEdges
    [ ("COM","B")
    , ("B","C")
    , ("C","D")
    , ("D","E")
    , ("E","F")
    , ("B","G")
    , ("G","H")
    , ("D","I")
    , ("E","J")
    , ("J","K")
    , ("K","L")
    , ("K", "YOU")
    , ("I", "SAN")
    ]


testTotalOrbits =
    [ TestLabel "" $ TestCase $ assertEqual "" 42 (totalOrbits g)
    ]


testPathFromCenter =
    [ TestLabel "" $ TestCase $ assertEqual "" ["COM", "B", "C", "D", "I", "SAN"] (pathFromCenter "SAN" g1 "COM")
    , TestLabel "" $ TestCase $ assertEqual "" ["COM", "B", "C", "D", "E", "J", "K", "YOU"] (pathFromCenter "YOU" g1 "COM")
    ]


testJumpsRequired =
    [ TestLabel "" $ TestCase $ assertEqual "" 4 (jumpsRequired g1)
    ]



d06Tests :: [Test]
d06Tests = testTotalOrbits ++ testPathFromCenter ++ testJumpsRequired
