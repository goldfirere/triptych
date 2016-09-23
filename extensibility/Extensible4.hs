{- Copyright (c) 2016 Richard Eisenberg

   Two mutually recursive datatypes representing a timy fragment of Haskell,
   made extensible via the use of a monolithic Tag type.
-}

{-# LANGUAGE GADTSyntax, DataKinds, TypeFamilies #-}

module Extensible where

import Data.Kind
import Data.Void

data Tag where
  VarTag    :: Tag
  AppTag    :: Tag
  LamTag    :: Tag
  LitTag    :: Tag
  ExpExtTag :: Tag

  VarPTag   :: Tag
  ViewPTag  :: Tag
  PatExtTag :: Tag
    -- all the tags are in the same structure. Not extensible.

data Exp :: (Tag -> Type) -> Type where
  Var    :: ext VarTag -> String             -> Exp ext
  App    :: ext AppTag -> Exp ext -> Exp ext -> Exp ext
  Lam    :: ext LamTag -> Pat ext -> Exp ext -> Exp ext
  Lit    :: ext LitTag -> Int                -> Exp ext
  ExpExt :: ext ExpExtTag                    -> Exp ext

data Pat :: (Tag -> Type) -> Type where
  VarP   :: ext VarPTag -> String              -> Pat ext
  ViewP  :: ext ViewPTag -> Exp ext -> Pat ext -> Pat ext
  PatExt :: ext PatExtTag                      -> Pat ext

-- Simple representation of types
data Ty where
  IntTy :: Ty
  (:->) :: Ty -> Ty -> Ty

-- No extensions to datatype
data family Parsed :: Tag -> Type
data instance Parsed VarTag = PVar
data instance Parsed AppTag = PApp
data instance Parsed LamTag = PLam
data instance Parsed LitTag = PLit
data instance Parsed ExpExtTag  -- empty

data instance Parsed VarPTag = PVarP
data instance Parsed ViewPTag = PViewP
data instance Parsed PatExtTag  -- empty

data family Checked :: Tag -> Type
data instance Checked VarTag = CVar
data instance Checked AppTag = CApp
data instance Checked LamTag = CLam Ty   -- extension here ...
data instance Checked LitTag = CLit
data instance Checked ExpExtTag  -- empty

data instance Checked VarPTag = CVarP Ty -- ... and here
data instance Checked ViewPTag = CViewP
data instance Checked PatExtTag  -- empty

data family Desugared :: Tag -> Type
data instance Desugared VarTag = DVar
data instance Desugared AppTag = DApp
data instance Desugared LamTag = DLam Ty
data instance Desugared LitTag = DLit
data instance Desugared ExpExtTag
  = DOp (Int -> Int -> Int) (Exp Desugared) (Exp Desugared)  -- new constructor

data instance Desugared VarPTag = DVarP Ty
data instance Desugared ViewPTag = DViewP
data instance Desugared PatExtTag  -- empty


-- \x -> (+) 3 x
example :: Exp Parsed
example = Lam PLam (ViewP PViewP (Var PVar "negate") (VarP PVarP "x"))
              (App PApp (App PApp (Var PVar "+") (Lit PLit 3)) (Var PVar "x"))

texample :: Exp Checked
texample = Lam (CLam IntTy) (ViewP CViewP (Var CVar "negate") (VarP (CVarP IntTy) "x"))
              (App CApp (App CApp (Var CVar "+") (Lit CLit 3)) (Var CVar "x"))

dexample :: Exp Desugared
dexample = Lam (DLam IntTy) (ViewP DViewP (Var DVar "negate") (VarP (DVarP IntTy) "x"))
              (ExpExt (DOp (+) (Lit DLit 3) (Var DVar "x")))
