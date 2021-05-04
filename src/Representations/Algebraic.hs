{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Representations.Algebraic
  (
    type (∘)(..)
  , Id(..)
  , FreeGM(..)
  , Monoidal
  , return
  , (>>=)
  , (>>)
  , join
  , Get(..)
  , Put(..)
  , Choose(..)
  , Scope(..)
  , Det(..)
  , get
  , put
  , choose
  , scope
  , det
  , E
  , T
  , Quantifier
  , Determiner
  , QuantifyTuple(..)
  , Handleable(..)
  , eval_with
  ) where

import Prelude hiding (Monad(..))
import Representations.Eval.Model
import Representations.Representations


--------------------------------------------------------------------------------
-- | Algebraic effects encoded as functors

-- | Functor composition and identity
newtype (f ∘ g) v = Compose { getCompose :: f (g v) } deriving Functor
data Id v = Id v deriving Functor

-- | The free graded monad over Functor
data FreeGM f v where
  Pure :: v -> FreeGM Id v
  Join :: Functor f => f (FreeGM g v) -> FreeGM (f ∘ g) v

instance Functor (FreeGM Id) where
  fmap f (Pure v) = Pure $ f v

instance Functor (FreeGM g) => Functor (FreeGM (f ∘ g)) where  
  fmap f (Join m) = Join $ fmap (fmap f) m

-- | Functor is monoidal
type family Monoidal op where
  Monoidal ((f ∘ g) ∘ h) = f ∘ Monoidal (g ∘ h) -- associativity
  Monoidal (Id ∘ g) = g                         -- left id
  Monoidal (f ∘ Id) = f                         -- right id
  
-- | Monadic interface
return :: v -> FreeGM Id v
return = Pure

(>>=) :: FreeGM f v -> (v -> FreeGM g w) -> FreeGM (Monoidal (f ∘ g)) w
Pure v >>= k = k v
Join m >>= k = Join $ fmap (>>= k) m

(>>) :: FreeGM f v -> FreeGM g w -> FreeGM (Monoidal (f ∘ g)) w
m >> n = m >>= const n

join :: FreeGM f (FreeGM g v) -> FreeGM (Monoidal (f ∘ g)) v
join m = m >>= id

-- | Type abbreviations
type E repr = repr Entity
type T repr = repr Bool
type Pred p repr = repr (p -> Bool)
type Quantifier repr = Pred Entity repr -> T repr
type Determiner repr = Pred Entity repr -> Quantifier repr

-- | Functors
data Get s v = Get (s -> v) deriving Functor
data Put s v = Put s v deriving Functor
data Choose repr p v = Choose (Pred p repr) (repr p -> v) deriving Functor
data Scope repr v = Scope (Quantifier repr) (E repr -> v) deriving Functor
data Det repr v = Det (Determiner repr) (Determiner repr -> v) deriving Functor

get :: FreeGM (Get s ∘ Id) s
get = Join $ Get return

put :: s -> FreeGM (Put s ∘ Id) ()
put s = Join $ Put s $ return ()

choose :: Pred p repr -> FreeGM (Choose repr p ∘ Id) (repr p)
choose pred = Join $ Choose pred return

scope :: Quantifier repr -> FreeGM (Scope repr ∘ Id) (E repr)
scope q = Join $ Scope q return

det :: Determiner repr -> FreeGM (Det repr ∘ Id) (Determiner repr)
det d = Join $ Det d return


--------------------------------------------------------------------------------
-- | Effect handling

-- | Class used to handle a computation
class Handleable f p s repr | f -> p where
  handle :: FreeGM f (T repr)
         -> FreeGM (Get s ∘ (Choose repr p ∘ (Put s ∘ Id))) (T repr)
  
instance (Lambda repr,
          Heyting repr)
      => Handleable Id () s repr where
  handle (Pure v) = do
    s <- get
    _p <- choose (lam $ const true)
    put s
    return v

instance Handleable f p s repr
      => Handleable (Get s ∘ f) p s repr where
  handle (Join (Get f)) = do
    s <- get
    case handle (f s) of
      Join (Get g) -> g s

instance Handleable f p s repr
      => Handleable (Put s ∘ f) p s repr where
  handle (Join (Put s f)) = do
    _s' <- get
    case handle f of
      Join (Get g) -> g s

getPredParam :: FreeGM (Get s ∘ (Choose repr p ∘ f)) v -> s -> Pred p repr
getPredParam (Join (Get g)) s = case g s of
                                  Join (Choose pred h) -> pred

instance (Lambda repr,
          Cartesian repr,
          Heyting repr,
          KnownType e,
          KnownType p,
          Handleable f p s repr)
      => Handleable (Choose repr e ∘ f) (e, p) s repr where
  handle (Join (Choose pred f)) = do
    s <- get
    ep <- choose (lam $ \ep' -> app pred (fst_ ep')
                   /\ app (getPredParam (handle (f (fst_ ep'))) s) (snd_ ep'))
    let e = fst_ ep
        p = snd_ ep
    case handle (f e) of
      Join (Get g) -> case g s of
                        Join (Choose _p' h) -> h p

instance (Cartesian repr,
          Handleable f p s repr)
      => Handleable (Choose repr () ∘ f) p s repr where
  handle (Join (Choose _ f)) = handle $ f unit

class QuantifyTuple tpl repr where  
  quantify :: (forall a. (HOL a repr, KnownType a)
                      => repr (a -> Bool) -> repr Bool)
           -> repr (tpl -> Bool)
           -> repr Bool

-- | Evalutate a computation to (a representation of) a Bool, using your
-- favorite quantifier
instance (Lambda repr, Cartesian repr)
      => QuantifyTuple () repr where
  quantify _ f = app f unit

instance (Lambda repr,
          Cartesian repr,
          HOL a repr,
          KnownType a,
          KnownType t,
          QuantifyTuple t repr)
      => QuantifyTuple (a, t) repr where
  quantify q f = q $ lam $ \x -> quantify q $ lam $ \t -> app f (pair x t)

-- | Evalutate a computation to (a representation of) a Bool, using your
-- favorite quantifier
eval_with :: forall repr a p s.
             (Lambda repr,
              Cartesian repr,
              KnownType p,
              QuantifyTuple p repr)
          => (forall a. (HOL a repr, KnownType a)
                     => repr (a -> Bool) -> repr Bool)
          -> (T repr -> T repr -> T repr)
          -> FreeGM (Get s ∘ (Choose repr p ∘ (Put s ∘ Id))) (T repr)
          -> s -> T repr
eval_with q r (Join (Get f)) s
  = case f s of
      Join (Choose pred g) -> quantify q
                              (lam (\p -> case g p of
                                            Join (Put _s' h)
                                              -> case h of
                                                   Pure a -> app pred p `r` a))
  
instance (Lambda repr,
          Cartesian repr,
          Heyting repr,
          KnownType p,
          Handleable f p s repr,
          QuantifyTuple p repr)
      => Handleable (Scope repr ∘ f) () s repr where
  handle (Join (Scope q f)) = do
    s <- get
    _p <- choose (lam $ const true)
    put s
    return (q $ lam (\x -> eval_with exists (/\) (handle @f @p (f x)) s))

instance (Lambda repr,
          Cartesian repr,
          Heyting repr,
          KnownType p,
          Handleable f p s repr,
          QuantifyTuple p repr)
      => Handleable (Det repr ∘ f) () s repr where
  handle (Join (Det d f)) = do
    s <- get
    _p <- choose (lam $ const true)
    put s
    return (d (lam (\x -> eval_with exists (/\)
                     (handle @f @p (f (\p q -> app p x))) s)) -- convervativity
              (lam (\x -> eval_with exists (/\)
                     (handle @f @p (f (\p q -> app p x /\ app q x))) s)))
