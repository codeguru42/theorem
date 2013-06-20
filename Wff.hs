-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module Wff
       (Wff(..),
        Name,
        parse,
        isAxiom,
        isModusPonens,
        isSubstitution,
        isProof
       ) where

import Control.Applicative (liftA2)
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

isModusPonens :: Wff -> Wff -> Wff -> Bool
isModusPonens a b c = a == Not b `Or` c

isSubstitution  :: Wff -> Wff -> Bool
isSubstitution x' x = fst $ isSubstitution' [] x' x
  where isSubstitution' subs (Not a') (Not a) = isSubstitution' subs a' a
        isSubstitution' subs (a' `Or` b') (a `Or` b) = (lSub && rSub, subs'')
          where (lSub, subs') = isSubstitution' subs a' a
                (rSub, subs'') =  isSubstitution' subs b' b
        isSubstitution' subs wff (Var p) = if sub == Nothing
                                           then (True, (Var p, wff) : subs)
                                           else (Just wff == sub, subs)
          where sub = lookup (Var p) subs
        isSubstitution' subs _ _ = (False, subs)

isProof :: [Wff] -> [Wff] -> Bool
isProof given proof = all isValidStep proof
  where isValidStep x = isAxiom x
                        || x `elem` given
                        || any ($ x) (liftA2 isModusPonens prev prev)
                        || any (isSubstitution x) prev
          where prev = takeWhile (/= x) proof
