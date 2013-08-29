-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>), (<*>))
import Proof
import Test.HUnit.Base (Test(..), (~=?), (~:), (~?))
import Test.HUnit.Text (runTestTT)
import Wff

allTests :: Test
allTests = "Test Proof"
            ~: TestList
                [ testIsAxiom
                , testIsModusPonens
                , testIsSubstitution
                , testIsProof
                ]

testIsAxiom :: Test
testIsAxiom = "Test isAxiom"
              ~: TestList
              $ map (\(e, i) -> i ~: Right e ~=? isAxiom <$> parse i)
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
                              ~: Right e
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
                              ~: Right e
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
              $ map (\(r, g, i) ->
                      last i ~: Right r ~=? isProof <$> (parseAll g)
                                                   <*> (parseAll i))
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

main = runTestTT allTests
