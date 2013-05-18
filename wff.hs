-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

type Name = Char

data Wff = Var Name
         | Not Wff
         | Or Wff Wff

instance Show Wff
         where show (Var p) = [p]
               show (Not a) = '~' : show a
               show (Or a b) = ('[' : show a) ++ ('|' : show b) ++ "]"

parse :: String -> Wff
parse = fst . parse'
        where parse' ('~':rest) = (Not a, rest')
                where (a, rest') = parse' rest
              parse' ('[':rest) = (a `Or` b, rest'')
                where (a, ('|':rest') ) = parse' rest
                      (b, (']':rest'')) = parse' rest'
              parse' (c:rest) = (Var c, rest)

main = do
  print $ parse "[~p|q]"
  print $ parse "[[~p|q]|r]"
