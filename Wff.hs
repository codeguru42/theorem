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

import Control.Monad (guard)
import Data.Char (isLower)

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
parse s = do
  (x, "") <- parse' s
  Just x
    where  parse' ('~':rest) = do
             (a, rest') <- parse' rest
             Just (Not a, rest')
           parse' ('[':rest) = do
             (a, ('|':rest')) <- parse' rest
             (b, (']':rest'')) <- parse' rest'
             Just (a `Or` b, rest'')
           parse' (c:rest) = do
             guard $ isLower c
             Just (Var c, rest)
           parse' [] = Nothing

isAxiom :: Wff -> Bool
-- Axiom Schemata 3
isAxiom (Not (Not a `Or` b) `Or` (Not (c `Or` d) `Or` (e `Or` f))) =
  a == d && b == e && c == f
-- Axiom Schemata 2
isAxiom (Not a `Or` (b `Or` c)) = a == c
-- Axiom Schemata 1
isAxiom ((Not (a `Or` b)) `Or` c) = a == b && a == c
isAxiom _ = False
