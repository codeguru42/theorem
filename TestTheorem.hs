-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (catMaybes)
import Test.HUnit.Base (Test(..), (~=?), (~:))
import Test.HUnit.Text (runTestTT)
import Wff

testParse :: Test
testParse = "Test parse"
            ~: TestList $ map (\(e, i) -> i ~: Just e ~=? parse i) tests
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
            ~: TestList $ map (\i -> i ~: Nothing ~=? parse i) tests
  where tests = ["~A", "a~b", "a|", "a|b", "~", "a~", "[", "[a", "[a|",
                 "[a|b", "[a|]", "[|]", "]"]

testIsAxiom :: Test
testIsAxiom = "Test isAxiom"
              ~: TestList
              $ map (\(e, i) -> i ~: Just e ~=? isAxiom <$> parse i)
                tests
  where tests = [(False, "[~p|q]")
                ,(False, "[[~p|q]|r]")
                ,(True, "[~[a|a]|a]")
                ,(True, "[~a|[b|a]]")
                ,(True, "[~[~a|b]|[~[c|a]|[b|c]]]")
                ,(True, "[~[p|p]|p]")
                ,(False, "[~[~p|[p|p]]|[p|~p]]")
                ,(True, "[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]")
                ,(True, "[~[~p|p]|[~[q|p]|[p|q]]]")
                ]

testIsModusPonens :: Test
testIsModusPonens = "Test isModusPonens"
                    ~: TestList
                    $ map (\(e, i1, i2, i3)
                           -> show (i1, i2, i3)
                              ~: Just e
                              ~=? isModusPonens
                              <$> parse i1 <*> parse i2 <*> parse i3)
                    tests
                      where tests = [(True, "[~p|q]", "p", "q")
                                    ]

testIsSubstitution :: Test
testIsSubstitution = "Test isSubstitution"
                    ~: TestList
                    $ map (\(e, i1, i2)
                           -> show (i1, i2)
                              ~: Just e
                              ~=? isSubstitution <$> parse i1
                                                 <*> parse i2)
                    tests
                      where tests = [(True, "[~[a|b]|q]", "[~p|q]")
                                    ,(True, "[[a|b]|[~[a|b]|~c]]"
                                     ,"[p|[~p|q]]")
                                    ,(False, "[[a|b]|[~c|q]]"
                                     ,"[p|[~p|q]]")
                                    ,(True, "[[a|b]|[~[a|b]|[a|b]]]"
                                     ,"[p|[~p|q]]")
                                    ,(False, "[a|b]", "[p|p]")
                                    ]

testIsProof = "Test isProof"
              ~: TestList
              $ map (\(e, g, i) ->
                      last i ~: e ~=? isProof (parseWffs g)
                                              (parseWffs i))
              tests
  where tests = [(True -- 1103
                 ,[]
                 ,["[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]"
                  ,"[~[p|p]|p]"
                  ,"[~[~p|[p|p]]|[p|~p]]"
                  ,"[~p|[p|p]]"
                  ,"[p|~p]"])
                ,(True -- 1104
                 ,["[p|~p]"]
                 ,["[p|~p]"
                  ,"[~p|~~p]"])
                ,(False
                 ,["[~p|~~p]"]
                 ,["[~~~p|p]"])
                ,(True -- 1105
                 ,["[p|~p]"
                  ,"[~p|~~p]"]
                 ,["[~[~~p|~~~p]|[~[p|~p]|[~~~p|p]]]"
                  ,"[~p|~~p]"
                  ,"[~~p|~~~p]"
                  ,"[~[p|~p]|[~~~p|p]]"
                  ,"[p|~p]"
                  ,"[~~~p|p]"])
                ,(True -- 1107
                 ,["[~p|p]"]
                 ,["[~p|p]"
                  ,"[~[~p|p]|[~[q|p]|[p|q]]]"
                  ,"[~[q|p]|[p|q]]"])
                ]
        parseWffs = catMaybes . map parse

main = do
  runTestTT $ TestList [testParse
                       ,testParseError
                       ,testIsAxiom
                       ,testIsModusPonens
                       ,testIsSubstitution
                       ,testIsProof]
