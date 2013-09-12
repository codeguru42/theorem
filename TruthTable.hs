-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module TruthTable
    ( truthTable
    ) where

truthTable :: Int -> [[Bool]]
truthTable n = map column [1..n]
    where column k = take (2 ^ n)
                        $ cycle ((take (2 ^ (n - k)) $ repeat True)
                              ++ (take (2 ^ (n - k)) $ repeat False))
