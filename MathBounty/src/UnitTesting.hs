module UnitTesting where

import Test.HUnit     as HU
import MathBountyV2 

-- import qualified HedgeHog.Gen      as Gen
-- import qualified HedgeHog.Range    as Range
-- import Test.Tasty
-- import Tasty.ExpectedFailure
-- import Test.Tasty.HedgeHog

fakeValidator :: Integer -> Integer -> Integer -> Bool
fakeValidator datum redeemer sContext 
            | datum == redeemer       =  True
            | otherwise               =  False
                                        


test3 = TestCase (assertEqual "For datum equals redeemer" True (fakeValidator 3 2 4))

-- test2 = TestCase (do (x,y) <- partA 3
--                      assertEqual "for the first result of partA," 5 x
--                      b <- partB y
--                      assertBool ("(partB " ++ show y ++ ") failed") b)

tests2 = TestList [TestLabel "test3" test1] --, TestLabel "test2" test2
                     
foo :: Int -> (Int, Int)
foo x = (1, x)

partA :: Int -> IO (Int, Int)
partA v = return (v+2, v+3)

partB :: Int -> IO Bool
partB v = return (v > 5)

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

test2 :: Test
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

tests' :: Test
tests' = HU.test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? (foo 3),
                "test2" ~: do (x, y) <- partA 3
                              assertEqual "for the first result of partA," 5 x
                              partB y @? "(partB " ++ show y ++ ") failed" ]

main :: IO Counts
main = do _ <- runTestTT tests2
          runTestTT tests'
