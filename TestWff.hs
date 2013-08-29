-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module TestWff (allTests) where

import Test.HUnit.Base (Test(..), (~=?), (~:), (~?))
import Test.HUnit.Text (runTestTT)
import Wff

allTests :: Test
allTests = "Test Wff"
            ~: TestList
                [ testParse
                , testParseError
                , testParseAll
                ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

testParse :: Test
testParse = "Test parse"
            ~: TestList $ map (\(e, i) -> i ~: Right e ~=? parse i) tests
  where tests = [((Not $ Var 'p') `Or` (Var 'q'), "[~p|q]")
                ,((Not $ Var 'p') `Or` (Var 'q') `Or` (Var 'r')
                 ,"[[~p|q]|r]")
                ,(Not $ Var 'a', "~a")
                ,((Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p')
                 ,"[~[p|p]|p]")
                ,((Not $ (Not $  Var 'p') `Or` (Var 'p' `Or` Var 'p'))
                  `Or` (Var 'p' `Or` (Not $ Var 'p'))
                 ,"[~[~p|[p|p]]|[p|~p]]")
                ,((Not $ (Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p'))
                   `Or` ((Not $ (Not $  Var 'p')
                               `Or` (Var 'p' `Or` Var 'p'))
                         `Or` (Var 'p' `Or` (Not $ Var 'p')))
                 ,"[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]")
                ,((Not $ (Not $ Var 'p') `Or` (Var 'p'))
                  `Or` ((Not $ (Var 'q') `Or` (Var 'p'))
                        `Or` ((Var 'p') `Or` (Var 'q')))
                 , "[~[~p|p]|[~[q|p]|[p|q]]]")
                ]

testParseError :: Test
testParseError = "Test parse error conditions"
            ~: TestList
            $ map
              (\i -> i ~: isLeft (parse i) ~? "Expected Left value")
              tests
  where tests = ["~A", "a~b", "a|", "a|b", "~", "a~", "[", "[a", "[a|",
                 "[a|b", "[a|]", "[|]", "]"]

testParseAll :: Test
testParseAll = "Test parseAll"
               ~: TestList
               $ map
                 (\(ws, ss) -> show ss ~: Right ws ~=? parseAll ss)
                 tests
  where tests = [([(Not $ Var 'p') `Or` (Var 'q')
                  ,(Not $ Var 'p') `Or` (Var 'q') `Or` (Var 'r')
                  ,Not $ Var 'a'
                  ,(Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p')
                  ,(Not $ (Not $  Var 'p')
                          `Or` (Var 'p' `Or` Var 'p'))
                   `Or` (Var 'p' `Or` (Not $ Var 'p'))
                  ,(Not $ (Not $ (Var 'p') `Or` (Var 'p'))
                          `Or` (Var 'p'))
                   `Or` ((Not $ (Not $  Var 'p')
                                `Or` (Var 'p' `Or` Var 'p'))
                         `Or` (Var 'p' `Or` (Not $ Var 'p')))
                  ,(Not $ (Not $ Var 'p') `Or` (Var 'p'))
                   `Or` ((Not $ (Var 'q') `Or` (Var 'p'))
                         `Or` ((Var 'p') `Or` (Var 'q')))
                  ]
                 ,["[~p|q]"
                  ,"[[~p|q]|r]"
                  ,"~a"
                  ,"[~[p|p]|p]"
                  ,"[~[~p|[p|p]]|[p|~p]]"
                  ,"[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]"
                  ,"[~[~p|p]|[~[q|p]|[p|q]]]"
                  ]
                 )
                ]

testParseAllError :: Test
testParseAllError = "Test parseAll"
               ~: TestList
               $ map
               (\ss -> show ss ~: isLeft (parseAll ss)
                               ~? "Expected Left value")
               tests
  where tests = [["[~p|q]"
                 ,"[[~p|q|r]"
                 ,"~a"
                 ,"[~[p|p]|p]"
                 ]
                ]
