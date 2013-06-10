-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module Wff
       (Wff(..),
        Name,
        parse,
        isAxiom
       ) where

import Data.Char(isLower)

type Name = Char

data Wff = Var Name
         | Not Wff
         | Or Wff Wff
         deriving (Eq)

instance Show Wff
         where show (Var p) = [p]
               show (Not a) = '~' : show a
               show (Or a b) = ('[' : show a) ++ ('|' : show b) ++ "]"

parse :: String -> Maybe Wff
parse s
  | null rest = wff
  | otherwise = Nothing
    where (wff, rest) = parse' s
          parse' ('~':rest) = (x, rest')
            where (a', rest') = parse' rest
                  x = if a' == Nothing
                      then Nothing
                      else let Just a = a'
                           in Just $ Not a
          parse' ('[':rest) = (x, if null rest''
                                  then ""
                                  else tail rest'')
            where (a', rest') = parse' rest
                  (b', rest'') = parse' (if null rest'
                                         then ""
                                         else tail rest')
                  x = if null rest'
                         || a' == Nothing
                         || null rest''
                         || b' == Nothing
                         || head rest' /= '|'
                         || head rest'' /= ']'
                      then Nothing
                      else let Just a = a'
                               Just b = b'
                           in Just $ a `Or` b
          parse' s@(c:rest)
            | isLower c  = (Just $ Var c, rest)
            | otherwise = (Nothing, s)
          parse' _ = (Nothing, [])

isAxiom :: Wff -> Bool
-- Axiom Schemata 3
isAxiom (Not (Not a `Or` b) `Or` (Not (c `Or` d) `Or` (e `Or` f))) =
  a == d && b == e && c == f
-- Axiom Schemata 2
isAxiom (Not a `Or` (b `Or` c)) = a == c
-- Axiom Schemata 1
isAxiom ((Not (a `Or` b)) `Or` c) = a == b && a == c
isAxiom _ = False
