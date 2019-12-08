module D08SpaceImageFormatTest where


import Test.HUnit

import D08SpaceImageFormat (getLayers, layerWithMinZeros, onesMultTwos, mergeLayers)


testGetLayers =
    [ TestLabel "" $ TestCase $ assertEqual "" [[1,2,3,4,5,6], [7,8,9,0,1,2]] (getLayers 3 2 [1,2,3,4,5,6,7,8,9,0,1,2])
    ]

testLayerWithMinZeros =
    [ TestLabel "" $ TestCase $ assertEqual "" [1,2,3,4,5,6] (layerWithMinZeros [[1,2,3,4,5,6], [7,8,9,0,1,2]])
    ]

testTotalOnesMultTwos =
    [ TestLabel "" $ TestCase $ assertEqual "" 1 (onesMultTwos [1,2,3,4,5,6])
    , TestLabel "" $ TestCase $ assertEqual "" 4 (onesMultTwos [1,2,3,4,5,6,7,8,9,0,1,2])
    ]

testMergeLayers =
    [ TestLabel "" $ TestCase $ assertEqual "" [0,1,1,0] (mergeLayers [[0,2,2,2],[1,1,2,2],[2,2,1,2],[0,0,0,0]])
    ]

d08Tests :: [Test]
d08Tests = testGetLayers ++ testLayerWithMinZeros ++ testTotalOnesMultTwos ++ testMergeLayers

