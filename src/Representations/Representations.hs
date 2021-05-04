{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Representations.Representations
  (
    Lambda(..)
  , Cartesian(..)
  , Constant(..)
  , Heyting(..)
  , HOL(..)
  , Equality(..)
  , Context(..)
  , KnownType(..)
  , Type(..)
  , Var(..)
  ) where

import GHC.TypeLits
import Representations.Eval.Model


--------------------------------------------------------------------------------
-- | Types

-- | Syntax of types
data Type a where
  E :: Type Entity
  T :: Type Bool
  Arrow :: Type a -> Type b -> Type (a -> b)
  Unit :: Type ()
  Prod :: Type a -> Type b -> Type (a, b)

-- | Types whose syntax is known
class KnownType a where
  knownType :: Type a

instance KnownType Entity where
  knownType = E

instance KnownType Bool where
  knownType = T

instance (KnownType a, KnownType b) => KnownType (a -> b) where
  knownType = Arrow knownType knownType

instance KnownType () where
  knownType = Unit

instance (KnownType a, KnownType b) => KnownType (a, b) where
  knownType = Prod knownType knownType

instance Show (Type a) where
  show E = "e"
  show T = "t"
  show (Arrow ty1 ty2) = "(" ++ show ty1 ++ " → " ++ show ty2 ++ ")"
  show Unit = "★"
  show (Prod ty1 ty2) = "(" ++ show ty1 ++ " × " ++ show ty2 ++ ")"


--------------------------------------------------------------------------------
-- | Algebras

class Lambda repr where -- more often called "Closed"
  app :: repr (a -> b) -> repr a -> repr b
  lam :: KnownType a => (repr a -> repr b) -> repr (a -> b)

class Cartesian repr where
  unit :: repr ()
  pair :: repr a -> repr b -> repr (a, b)
  fst_ :: repr (a, b) -> repr a
  snd_ :: repr (a, b) -> repr b

class Constant a (str :: Symbol) repr where
  c :: repr a

class Heyting repr where
  (/\), (\/), (-->) :: repr Bool -> repr Bool -> repr Bool
  true, false :: repr Bool

class HOL a repr where
  forall, exists :: repr (a -> Bool) -> repr Bool

class Equality a repr where
  equals :: repr a -> repr a -> repr Bool

class Context a repr where
  type Gamma a
  empty :: repr (Gamma a)
  upd :: repr a -> repr (Gamma a) -> repr (Gamma a)
  sel :: repr (Gamma a) -> repr a


--------------------------------------------------------------------------------
-- | Variables

data Var = Var Char Int

instance Enum Var where
  succ (Var c i) = case c of
                     'x' -> Var 'y' i
                     'y' -> Var 'z' i
                     'z' -> Var 'u' i
                     'u' -> Var 'v' i
                     'v' -> Var 'w' i
                     'w' -> Var 'x' (succ i)
  toEnum i = case mod i 6 of
               0 -> Var 'x' (div i 6)
               1 -> Var 'y' (div i 6)
               2 -> Var 'z' (div i 6)
               3 -> Var 'u' (div i 6)
               4 -> Var 'v' (div i 6)
               5 -> Var 'w' (div i 6)
  fromEnum (Var c i) = case c of
                         'x' -> i*6
                         'y' -> i*6 + 1
                         'z' -> i*6 + 2
                         'u' -> i*6 + 3
                         'v' -> i*6 + 4
                         'w' -> i*6 + 5

instance Show Var where
  show (Var c i) = if i == 0 then [c] else c : show i
