module D06UniversalOrbitMapTest where

import Test.HUnit hiding (Node)

import D06UniversalOrbitMap (Graph, fromEdges, totalOrbits, allOrbits)
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


testTotalOrbits =
    [ TestLabel "" $ TestCase $ assertEqual "" 42 (totalOrbits g)
    ]


d06Tests :: [Test]
d06Tests = testTotalOrbits
