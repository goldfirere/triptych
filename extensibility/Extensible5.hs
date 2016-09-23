{- Copyright (c) 2016 Richard Eisenberg

   Two mutually recursive datatypes representing a timy fragment of Haskell,
   made extensible via the use of separate Tag types.
-}

{-# LANGUAGE GADTSyntax, TypeInType, TypeFamilies, RankNTypes #-}

module Extensible where

import Data.Kind
import Data.Void

-- Tag for expressions
data ETag where
  VarTag    :: ETag
  AppTag    :: ETag
  LamTag    :: ETag
  LitTag    :: ETag
  ExpExtTag :: ETag

-- Tag for patterns
data PTag where
  VarPTag   :: PTag
  ViewPTag  :: PTag
  PatExtTag :: PTag

data Exp (ext :: forall tag. tag -> Type)  -- Higher-rank kind!
  = Var    (ext VarTag) String
  | App    (ext AppTag) (Exp ext) (Exp ext)
  | Lam    (ext LamTag) (Pat ext) (Exp ext)
  | Lit    (ext LitTag) Int
  | ExpExt (ext ExpExtTag)

data Pat (ext :: forall tag. tag -> Type)
  = VarP   (ext VarPTag) String
  | ViewP  (ext ViewPTag) (Exp ext) (Pat ext)
  | PatExt (ext PatExtTag)

data Ty where
  IntTy :: Ty
  (:->) :: Ty -> Ty -> Ty

-- Other than the data family kinds, below here is the same as previous example
data family Parsed :: forall tag. tag -> Type
data instance Parsed VarTag = PVar
data instance Parsed AppTag = PApp
data instance Parsed LamTag = PLam
data instance Parsed LitTag = PLit
data instance Parsed ExpExtTag  -- empty

data instance Parsed VarPTag = PVarP
data instance Parsed ViewPTag = PViewP
data instance Parsed PatExtTag  -- empty

data family Checked :: forall tag. tag -> Type
data instance Checked VarTag = CVar
data instance Checked AppTag = CApp
data instance Checked LamTag = CLam Ty
data instance Checked LitTag = CLit
data instance Checked ExpExtTag  -- empty

data instance Checked VarPTag = CVarP Ty
data instance Checked ViewPTag = CViewP
data instance Checked PatExtTag  -- empty

data family Desugared :: forall tag. tag -> Type
data instance Desugared VarTag = DVar
data instance Desugared AppTag = DApp
data instance Desugared LamTag = DLam Ty
data instance Desugared LitTag = DLit
data instance Desugared ExpExtTag = DOp (Int -> Int -> Int) (Exp Desugared) (Exp Desugared)

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
