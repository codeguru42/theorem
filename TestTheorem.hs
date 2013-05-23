-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Wff(parse)

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

main = testParse