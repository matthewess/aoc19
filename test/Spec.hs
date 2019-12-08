import Test.HUnit

import IntCodeInterpreterTest (intCodeTests)
import D01TyrannyOfRocketEquationTest (d01Tests)
import D021202ProgramAlarmTest (d02Tests)
import D03CrossedWireTest (d03Tests)
import D04SecureContainerTest (d04Tests)
import D05SunnyWithAChanceOfAsteroidsTest (d05Tests)
import D06UniversalOrbitMapTest (d06Tests)
import D07AmplificatoinCircuitTest (d07Tests)


allTests :: Test
allTests = TestList $
    intCodeTests ++ d01Tests ++ d02Tests ++ d03Tests ++ d04Tests ++ d05Tests ++ d06Tests ++ d07Tests
    

main :: IO Counts
main = runTestTT allTests
