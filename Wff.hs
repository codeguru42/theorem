-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module Wff
       ( Wff(..)
       , Name
       , parse
       , parseAll
       , eval
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

parse :: String -> Either String Wff
parse s = do
  (x, rest) <- parse' s
  if null rest
  then return x
  else Left $ "Input not consumed: " ++ show rest
    where  parse' s
             = case s of
                 ('~':rest) -> do
                   (a, rest') <- parse' rest
                   return (Not a, rest')
                 ('[':rest) -> do
                   (a, s') <- parse' rest
                   case s' of
                     ('|':rest') -> do
                       (b, s'') <- parse' rest'
                       case s'' of
                         (']':rest'') -> return (a `Or` b, rest'')
                         otherwise -> Left
                           $ "Expected ']' but got " ++ show (head s'')
                     otherwise -> Left
                       $ "Expected '|' but got " ++ show (head s')
                 (c:rest) -> if isLower c
                   then return (Var c, rest)
                   else Left
                     $ "Expected '~', '[', or lower case but got "
                       ++ show c
                 [] -> Left "Unexpected end of input"

parseAll :: [String] -> Either String [Wff]
parseAll = mapM parse

type Assignment = [(Name, Bool)]

eval :: Wff -> Assignment -> Maybe Bool
eval (Var p)    assignment = lookup p assignment
eval (Not a)    assignment = undefined
eval (a `Or` b) assignment = undefined
