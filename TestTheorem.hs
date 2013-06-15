-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Test.HUnit.Base (Test(..), (~=?), (~:))
import Test.HUnit.Text (runTestTT)
import Wff

testParse :: Test
testParse = "Test parse"
            ~: TestList $ map (\(e, i) -> i ~: e ~=? parse i) tests
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
                 (Nothing, "[|]"),
                 (Just $ (Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p'),
                  "[~[p|p]|p]"),
                 (Just $ (Not $ (Not $  Var 'p') `Or` (Var 'p' `Or` Var 'p'))
                  `Or` (Var 'p' `Or` (Not $ Var 'p')),
                  "[~[~p|[p|p]]|[p|~p]]"),
                 (Just $
                  (Not $ (Not $ (Var 'p') `Or` (Var 'p')) `Or` (Var 'p'))
                  `Or` ((Not $ (Not $  Var 'p') `Or` (Var 'p' `Or` Var 'p'))
                        `Or` (Var 'p' `Or` (Not $ Var 'p'))),
                  "[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]")
                ]

testIsAxiom :: Test
testIsAxiom = "Test isAxiom"
              ~: TestList
              $ map (\(e, i) -> i ~: Just e ~=? isAxiom <$> parse i) tests
  where tests = [(False, "[~p|q]"),
                 (False, "[[~p|q]|r]"),
                 (True, "[~[a|a]|a]"),
                 (True, "[~a|[b|a]]"),
                 (True, "[~[~a|b]|[~[c|a]|[b|c]]]"),
                 (True, "[~[p|p]|p]"),
                 (False, "[~[~p|[p|p]]|[p|~p]]"),
                 (True, "[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]")
                ]

testIsProof = "Test isProof"
              ~: TestList
              $ map (\(e, g, i) ->
                      last i ~: e ~=? isProof (parseWffs g) (parseWffs i))
              tests
  where tests = [(True,
                  [],
                  ["[~[~[p|p]|p]|[~[~p|[p|p]]|[p|~p]]]",
                   "[~[p|p]|p]",
                   "[~[~p|[p|p]]|[p|~p]]",
                   "[~p|[p|p]]",
                   "[p|~p]"
                  ]
                 ),
                 (True,
                  ["[p|~p]"],
                  ["[p|~p]",
                   "[~p|~~p]"
                  ]
                 ),
                 (False,
                  ["[~p|~~p]"],
                  ["[~~~p|p]"
                  ]
                 ),
                 (True,
                  ["[p|~p]",
                   "[~p|~~p]"],
                  ["[~[~~p|~~~p]|[~[p|~p]|[~~~p|p]]]",
                   "[~p|~~p]",
                   "[~~p|~~~p]",
                   "[~[p|~p]|[~~~p|p]]",
                   "[p|~p]",
                   "[~~~p|p]"
                  ]
                 )
                ]
        parseWffs = catMaybes . map parse

main = do
  runTestTT $ TestList [testParse,
                        testIsAxiom,
                        testIsProof]
