-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>))
import Test.HUnit.Base (Test(..), (~=?), (~:))
import Test.HUnit.Text (runTestTT)
import Wff (Wff(..), parse, isAxiom)

testParse :: Test
testParse = "Test parse" ~:
            TestList $ map (\(e, i) -> i ~: e ~=? parse i) tests
  where tests = [(Just $ (Not $ Var 'p') `Or` (Var 'q'), "[~p|q]"),
                 (Just $ (Not $ Var 'p') `Or` (Var 'q') `Or` (Var 'r'),
                  "[[~p|q]|r]"),
                 (Nothing, "~A"),
                 (Nothing, "a~b"),
                 (Nothing, "a|"),
                 (Nothing, "a|b"),
                 (Nothing, "~"),
                 (Just . Not $ Var 'a', "~a"),
                 (Nothing, "a~"),
                 (Nothing, "["),
                 (Nothing, "[a"),
                 (Nothing, "[a|"),
                 (Nothing, "[a|b"),
                 (Nothing, "[a|]"),
                 (Nothing, "[|]")
                ]

testIsAxiom :: Test
testIsAxiom = "Test isAxiom" ~:
              TestList $
              map (\(e, i) -> i ~: Just e ~=? isAxiom <$> parse i) tests
  where tests = [(False, "[~p|q]"),
                 (False, "[[~p|q]|r]"),
                 (True, "[~[a|a]|a]"),
                 (True, "[~a|[b|a]]"),
                 (True, "[~[~a|b]|[~[c|a]|[b|c]]]")
                ]

main = do
  runTestTT $ TestList [testParse,
                        testIsAxiom]
