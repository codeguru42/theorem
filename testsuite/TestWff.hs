-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module TestWff (allTests) where

import Control.Applicative ((<$>), (<*>))
import Test.HUnit.Base (Test(..), (~=?), (~:), (~?))
import Test.HUnit.Text (runTestTT)
import TruthTable
import Utils
import Wff

allTests :: Test
allTests = "Test Wff"
        ~: TestList
         [ testParse
         , testParseError
         , testParseAll
         , testParseAllError
         , testEval
         ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

testParse :: Test
testParse = "Test parse" ~: makeTestList (map Right expecteds) (map parse inputs)
    where expecteds = [ (Not $ Var 'p') `Or` (Var 'q')
                      , (Not $ Var 'p') `Or` (Var 'q') `Or` (Var 'r')
                      , Not $ Var 'a'
                      , (Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p')
                      , (Not $ (Not $  Var 'p') `Or` (Var 'p' `Or` Var 'p'))
                            `Or` (Var 'p' `Or` (Not $ Var 'p'))
                      , (Not $ (Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p'))
                            `Or` ((Not $ (Not $  Var 'p') `Or` (Var 'p' `Or` Var 'p'))
                                    `Or` (Var 'p' `Or` (Not $ Var 'p')))
                      , (Not $ (Not $ Var 'p') `Or` (Var 'p'))
                            `Or` ((Not $ (Var 'q') `Or` (Var 'p'))
                                `Or` ((Var 'p') `Or` (Var 'q')))
                      ]
          inputs    = [ "[~p|q]"
                      , "[[~p|q]|r]"
                      , "~a"
                      , "[~[p|p]|p]"
                      , "[~[~p|[p|p]]|[p|~p]]"
                      , "[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]"
                      , "[~[~p|p]|[~[q|p]|[p|q]]]"
                      ]

testParseError :: Test
testParseError = "Test parse error conditions"
              ~: TestList
               $ map
                 (\i
                  -> i
                  ~: isLeft (parse i)
                  ~? "Expected Left value")
                 tests
    where tests = ["~A", "a~b", "a|", "a|b", "~", "a~", "[", "[a", "[a|",
                   "[a|b", "[a|]", "[|]", "]"]

testParseAll :: Test
testParseAll = "Test parseAll" ~: makeTestList (map Right expecteds) (map parseAll actuals)
    where expecteds = [ [ (Not $ Var 'p') `Or` (Var 'q')
                        , (Not $ Var 'p') `Or` (Var 'q') `Or` (Var 'r')
                        , Not $ Var 'a'
                        , (Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p')
                        , (Not $ (Not $  Var 'p') `Or` (Var 'p' `Or` Var 'p'))
                            `Or` (Var 'p' `Or` (Not $ Var 'p'))
                        , (Not $ (Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p'))
                            `Or` ((Not $ (Not $  Var 'p')
                                `Or` (Var 'p' `Or` Var 'p'))
                                `Or` (Var 'p' `Or` (Not $ Var 'p')))
                        , (Not $ (Not $ Var 'p') `Or` (Var 'p'))
                            `Or` ((Not $ (Var 'q') `Or` (Var 'p'))
                            `Or` ((Var 'p') `Or` (Var 'q')))
                        ]
                      ]
          actuals = [ ["[~p|q]"
                      ,"[[~p|q]|r]"
                      ,"~a"
                      ,"[~[p|p]|p]"
                      ,"[~[~p|[p|p]]|[p|~p]]"
                      ,"[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]"
                      ,"[~[~p|p]|[~[q|p]|[p|q]]]"
                      ]
                    ]

testParseAllError :: Test
testParseAllError = "Test parseAll"
                 ~: TestList
                  $ map
                   (\ss
                    -> show ss
                    ~: isLeft (parseAll ss)
                    ~? "Expected Left value")
                   tests
  where tests = [["[~p|q]"
                 ,"[[~p|q|r]"
                 ,"~a"
                 ,"[~[p|p]|p]"
                 ]
                ]

testEval :: Test
testEval = TestList $ zipWith3 testEval' wffStrs expecteds actuals
    where testEval' wffStr expecteds actuals = wffStr ~: makeTestList (map (Right . Just) expecteds) actuals
          expecteds      = [ [True, False]
                           , [False, True]
                           , [True, True, True, False]
                           ]
          actuals        = zipWith actuals' wffs allAssignments
          actuals' (Left s) assignments' = replicate (length assignments') (Left s)
          actuals' (Right wff) assignments' = map (Right . eval wff) assignments'
          vars           = [ ['p']
                           , ['p']
                           , ['p', 'q']
                           ]
          allAssignments = map assignments vars
          wffs           = map parse wffStrs
          wffStrs        = [ "p"
                           , "~p"
                           , "[p|q]"
                           ]
