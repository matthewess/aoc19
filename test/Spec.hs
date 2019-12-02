import Test.HUnit

import D01TyrannyOfRocketEquationTest (d01Tests)
import D021202ProgramAlarmTest (d02Tests)


allTests :: Test
allTests = TestList $
    d01Tests ++ d02Tests
    

main :: IO Counts
main = runTestTT allTests
