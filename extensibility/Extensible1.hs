{- Copyright (c) 2016 Richard Eisenberg

   Two mutually recursive datatypes representing a timy fragment of Haskell.
-}

{-# LANGUAGE GADTSyntax #-}

module Extensible where

data Exp where
  Var :: String     -> Exp
  App :: Exp -> Exp -> Exp
  Lam :: Pat -> Exp -> Exp
  Lit :: Int        -> Exp

data Pat where
  VarP  :: String     -> Pat
  ViewP :: Exp -> Pat -> Pat

-- \ (negate -> x) -> (+) 3 x
example :: Exp
example = Lam (ViewP (Var "negate") (VarP "x")) (App (App (Var "+") (Lit 3)) (Var "x"))
