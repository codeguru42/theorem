-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Data.Maybe (mapMaybe)
import Wff (parse, isAxiom)

testParse = do
  mapM_ print $ zip tests $ map parse tests
  where tests = ["[~p|q]",
                 "[[~p|q]|r]",
                 "~A",
                 "a~b",
                 "a|",
                 "a|b",
                 "~",
                 "~a",
                 "a~",
                 "[",
                 "[a",
                 "[a|",
                 "[a|b",
                 "[a|]",
                 "[|]"
                ]

testIsAxiom = do
  let wffs = mapMaybe parse tests
  mapM_ print $ zip wffs $ map isAxiom wffs
  where tests = ["[~p|q]",
                 "[[~p|q]|r]",
                 "[~[a|a]|a]",
                 "[~a|[b|a]]",
                 "[~[~a|b]|[~[c|a]|[b|c]]]"
                ]

main = do
  print "**** Test parse ****"
  testParse
  print "**** Test isAxiom *****"
  testIsAxiom
