{- Copyright (c) 2016 Richard Eisenberg

   Two mutually recursive datatypes representing a timy fragment of Haskell,
   both before and after typechecking.
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

-- Simple representation of types
data Ty where
  IntTy :: Ty
  (:->) :: Ty -> Ty -> Ty

data TExp where
  TVar :: String             -> TExp
  TApp :: TExp -> TExp       -> TExp
  TLam :: Ty -> TPat -> TExp -> TExp  -- type info added here ...
  TLit :: Int                -> TExp

data TPat where
  TVarP  :: Ty -> String -> TPat      -- ... and here
  TViewP :: TExp -> TPat -> TPat

-- \ (negate -> x) -> (+) 3 x
example :: Exp
example = Lam (ViewP (Var "negate") (VarP "x"))
              (App (App (Var "+") (Lit 3)) (Var "x"))

texample :: TExp
texample = TLam IntTy (TViewP (TVar "negate") (TVarP IntTy "x"))
                (TApp (TApp (TVar "+") (TLit 3)) (TVar "x"))
