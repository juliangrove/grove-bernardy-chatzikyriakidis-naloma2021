{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Representations.Bayesian.Probability where

import Control.Monad (ap)

class Space repr where
  type EvalType repr
  integrate :: repr a -> (a -> EvalType repr) -> EvalType repr
  finite :: [a] -> repr a
  factor :: EvalType repr -> repr ()
  sigma :: repr a -> (a -> repr b) -> repr (a, b)
  project :: (a -> b) -> repr a -> repr b
  bind :: repr a -> (a -> repr b) -> repr b
  bind m k = project snd $ sigma m k
  factorWith :: (a -> EvalType repr) -> repr a -> repr a
  factorWith f s = bind s $ \x -> bind (factor (f x)) $ \_ -> finite [x]
  scale :: EvalType repr -> repr a -> repr a
  scale c = factorWith (const c)
  uniformDiscrete :: Fractional (EvalType repr) => [a] -> repr a
  uniformDiscrete as = scale (1 / fromIntegral (length as)) (finite as)
  measure :: Num (EvalType repr) => repr a -> EvalType repr
  measure s = integrate s (const 1)
  normalize :: Fractional (EvalType repr) => repr a -> repr a
  normalize s = scale (1 / measure s) s
  expectedValueOfDistr :: repr (EvalType repr) -> EvalType repr
  expectedValueOfDistr d = integrate d id
  indicator :: Num (EvalType repr) => Bool -> EvalType repr
  indicator b = if b then 1 else 0
  isTrue :: Num (EvalType repr) => Bool -> repr ()
  isTrue = factor @repr . indicator @repr
  expectedValue :: (Num (EvalType repr), Fractional (EvalType repr))
                => repr a -> (a -> EvalType repr) -> EvalType repr
  expectedValue s f = integrate s f / measure s
  probability1 :: Fractional (EvalType repr)
               => repr a -> (a -> Bool) -> EvalType repr
  probability1 s e = expectedValue s (indicator @repr . e)
  probability :: Fractional (EvalType repr) => repr Bool -> EvalType repr
  probability s = expectedValue s (indicator @repr)
  subspace :: Num (EvalType repr) => (a -> Bool) -> repr a -> repr a
  subspace e s = project fst $ sigma s (isTrue . e)
  condProb :: (Num (EvalType repr), Fractional (EvalType repr))
           => repr a -> (a -> Bool) -> (a -> Bool) -> EvalType repr
  condProb s f g = probability1 (subspace g s) f
  lookUp :: (Eq a, Num (EvalType repr)) => a -> repr a -> EvalType repr
  lookUp a s = integrate s (\x -> indicator @repr (x == a))
  bernoulli :: Num (EvalType repr) => EvalType repr -> repr Bool
  bernoulli p = factorWith (\b -> if b then p else 1 - p) (finite [True, False])

instance {-# OVERLAPS #-} Space repr => Functor repr where
  fmap = project
instance {-# OVERLAPS #-} Space repr => Applicative repr where
  pure a = finite [a]
  (<*>) = ap
instance {-# OVERLAPS #-} Space repr => Monad repr where
  (>>=) = bind

type R = Double

newtype RSpace a = RSpace { integrator :: (a -> R) -> R }
type Distr a = RSpace a -- The measure of a distribution is always 1.

instance Space RSpace where
  type EvalType RSpace = R
  finite as = RSpace $ \g -> sum $ map g as
  factor r = RSpace $ \g -> r * g ()
  integrate = integrator
  sigma a f = RSpace $ \g -> integrator a $ \x -> integrator (f x) $ \y -> g (x, y)
  project f (RSpace m) = RSpace $ \k -> m $ k . f
