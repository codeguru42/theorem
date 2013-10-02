-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module TruthTable
    ( truthTable
    , assignments
    ) where

import Control.Applicative ((<$>), (<*>))
import Wff (Name)

-- `truthTable n` returns a list of the values in each row of a truth table
-- for an expression with `n` propositional variables.
truthTable :: Int -- the number of variables in the truth table
           -> [[Bool]]
truthTable n = cross $ replicate n [True, False]

assignments :: [Name] -> [[(Name, Bool)]]
assignments ps = map (zip ps) . truthTable $ length ps

-- `cross xs` returns a list of lists of length `n` where `n = length xs`. The
-- resulting list contains the cross product of the lists in `xs`.
cross :: [[a]] -- the lists to take the cross product of
      -> [[a]]
cross [] = [[]]
cross (x:xs) = (:) <$> x <*> cross xs
