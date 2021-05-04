{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Representations.Bayesian.Bayesian
  (
    Dynamic
  , RSA(..)
  , getDepths
  , runRSA
  , filterByProp
  , makeBernoulli
  ) where

import Representations.Eval.Model
import Representations.Representations as Reps
import Representations.FOL.FOL
import Representations.Bayesian.Probability
import FOL.FOL
import FOL.Solver
import Control.Applicative
import GHC.TypeLits
import Data.Proxy


--------------------------------------------------------------------------------
-- | Data

type Sel = [FolTerm Entity] -> FolTerm Entity

newtype SelReader a = SelReader { getSels :: [Sel] -> FolTerm a }
newtype Dynamic a = Dynamic { getDepths :: [Int] -> ([Int], SelReader a) }

newDepth :: Int -> RSpace Int
newDepth d = uniformDiscrete [0 .. d - 1]

-- newDepth' :: Int -> RSpace Int
-- newDepth' d = normalize $ do
  -- depth <- newDepth d
  -- factor (log $ fromIntegral $ d - depth + 1)
  -- return depth

mkSel :: Int -> Sel
mkSel = flip (!!)

tryProve' :: [Value] -> Value -> Status
tryProve'  = tryProve 10

literalListener :: RSpace [FolTerm Bool]
                -> ([Sel] -> FolTerm Bool)
                -> [Int]
                -> [Int]
                -> [Bool]
                -> RSpace R
literalListener gamma phi ds phiDepths uAlt = do
  let filteredGamma = normalize $ do
        gamma' <- gamma
        depths <- sequence (zipWith3
                            (\depth d keep -> case keep of
                                                True -> return depth
                                                False -> newDepth d)
                            phiDepths
                            ds
                            uAlt)
        let sels = map mkSel depths
            u' = phi sels
        filterByProp gamma' u'
        return gamma'
  filteredGamma' <- filteredGamma      
  let phiSels = map mkSel phiDepths
      result = compatible filteredGamma' (phi phiSels)
  return (indicator @RSpace result)
 
pragmaticSpeaker :: RSpace [FolTerm Bool]
                 -> ([Sel] -> FolTerm Bool)
                 -> [Int]
                 -> [Int]
                 -> R
                 -> R
                 -> R
                 -> RSpace [Bool] -- return a distribution of u's (true = take )
pragmaticSpeaker gamma phi ds phiDepths alpha npC pC = do
  uAlt <- sequence $ replicate (length ds) $ bernoulli 0.5 -- alternative utterances (either use np or anaphor).
  let l0skew = expectedValueOfDistr
               (normalize $ literalListener gamma phi ds phiDepths uAlt)
      cost u = foldr (\b acc -> (if b then npC else pC) + acc) 0 u -- if u[i] then we use the np, otherwise the anaphor.
      softMax uttr p = exp (alpha * (log p - cost uttr))
  factor (softMax uAlt l0skew)
  return uAlt

pragmaticListener :: RSpace [FolTerm Bool]
                  -> ([Sel] -> FolTerm Bool)
                  -> [Int]
                  -> R
                  -> R
                  -> R
                  -> RSpace [Int] 
pragmaticListener gamma u ds alpha npC pC = do
  phiDepths <- traverse newDepth ds -- interpretation (phiDepths ≅ θ)
  let s1skew = lookUp
               (replicate (length ds) False) -- version where we always pick the anaphor, ie. what was actually uttered.
               (normalize $ pragmaticSpeaker gamma u ds phiDepths alpha npC pC)
      prior = expectedValueOfDistr $ normalize $ do
        gamma' <- gamma
        let phiSels = map mkSel phiDepths
            result = compatible gamma' (u phiSels)
        return (indicator @RSpace result)
  factor (s1skew * prior)
  return phiDepths -- θ

data RSA = RSA { gamma :: RSpace [FolTerm Bool] -- distribution of prior knowledge/situations
               , utterance :: Dynamic Bool
               , alpha :: R
               , npCost :: R
               , pronounCost :: R
               }

runRSA :: RSA -> [([Int], R)]
runRSA (RSA gamma utterance alpha npCost pronounCost) =
  let phi = getSels $ snd $ getDepths utterance []
      ds = fst $ getDepths utterance []
      dstr = normalize $ pragmaticListener gamma phi ds alpha npCost pronounCost
  in map (\ds' -> (ds', lookUp ds' dstr)) $ sequence [ [0 .. d - 1] | d <- ds ]

makeBernoulli :: FolTerm Bool -> R -> RSpace (FolTerm Bool)
makeBernoulli t p = do
  b <- bernoulli p
  return (if b then t else t --> false)

compatible :: [FolTerm Bool] -> FolTerm Bool -> Bool
compatible gamma phi = let gamma' = map termToFol gamma
                           phi' = termToFol phi
                           result = tryProve' gamma' phi'
                       in result /= Contradiction

filterByProp :: [FolTerm Bool] -> FolTerm Bool -> RSpace ()
filterByProp gamma phi = isTrue $ compatible gamma phi


--------------------------------------------------------------------------------
-- | Intances

-- | CCCs
instance Lambda Dynamic where
  app m n = Dynamic $ \ds -> let ds' = fst $ getDepths m ds
                                 ds'' = fst $ getDepths n ds'
                             in (ds'', SelReader $ \sels ->
                                    app
                                    (getSels (snd $ getDepths m ds) sels)
                                    (getSels (snd $ getDepths n ds') sels))
  lam f = Dynamic $ \ds -> let ds' = fst $ getDepths (f (Dynamic $ \ds' -> (ds', SelReader $ \_ -> Con ""))) ds
                           in (ds', SelReader $ \sels ->
                               lam $ \x ->
                               getSels (snd $ getDepths
                                        (f (Dynamic $ \ds' ->
                                               (ds', SelReader $ \_ -> x)))
                                         ds)
                               sels)

instance Cartesian Dynamic where
  unit = Dynamic $ \ds -> (ds, SelReader $ const unit)
  pair m n = Dynamic $ \ds -> let ds' = fst $ getDepths m ds
                                  ds'' = fst $ getDepths n ds'
                              in (ds'', SelReader $ \sels ->
                                     pair
                                     (getSels (snd $ getDepths m ds) sels)
                                     (getSels (snd $ getDepths n ds') sels))
  fst_ m = Dynamic $ \ds -> let ds' = fst $ getDepths m ds
                            in (ds', SelReader $ \sels ->
                                   fst_ (getSels (snd $ getDepths m ds) sels)) 
  snd_ m = Dynamic $ \ds -> let ds' = fst $ getDepths m ds
                            in (ds', SelReader $ \sels ->
                                   snd_ (getSels (snd $ getDepths m ds) sels))
 
-- | Constants
instance KnownSymbol str => Constant ty str Dynamic where
  c = Dynamic $ \ds -> (ds, SelReader $ const (c @ty @str))

-- | Logic
instance Heyting Dynamic where
  phi /\ psi = Dynamic $ \ds -> let ds' = fst $ getDepths phi ds
                                    ds'' = fst $ getDepths psi ds'
                                in (ds'', SelReader $ \sels ->
                                       (/\)
                                       (getSels (snd $ getDepths phi ds) sels)
                                       (getSels (snd $ getDepths psi ds') sels))
  phi \/ psi = Dynamic $ \ds -> let ds' = fst $ getDepths phi ds
                                    ds'' = fst $ getDepths psi ds'
                                in (ds'', SelReader $ \sels ->
                                       (\/)
                                       (getSels (snd $ getDepths phi ds) sels)
                                       (getSels (snd $ getDepths psi ds') sels))
  phi --> psi = Dynamic $ \ds -> let ds' = fst $ getDepths phi ds
                                     ds'' = fst $ getDepths psi ds'
                                 in (ds'', SelReader $ \sels ->
                                        (-->)
                                        (getSels (snd $ getDepths phi ds) sels)
                                        (getSels (snd $ getDepths psi ds') sels))
  true = Dynamic $ \ds -> (ds, SelReader $ const true)
  false = Dynamic $ \ds -> (ds, SelReader $ const true)

instance KnownType a => HOL a Dynamic where
  forall k = Dynamic $ \ds -> let ds' = fst $ getDepths k ds
                              in (ds', SelReader $ \sels ->
                                     forall
                                     (getSels (snd $ getDepths k ds) sels))
  exists k = Dynamic $ \ds -> let ds' = fst $ getDepths k ds
                              in (ds', SelReader $ \sels ->
                                     exists
                                     (getSels (snd $ getDepths k ds) sels))

instance Equality a Dynamic where
  equals m n = Dynamic $ \ds -> let ds' = fst $ getDepths m ds
                                    ds'' = fst $ getDepths n ds'
                                in (ds'', SelReader $ \sels ->
                                       equals
                                       (getSels (snd $ getDepths m ds) sels)
                                       (getSels (snd $ getDepths n ds') sels))

-- | Contexts
instance Context Entity Dynamic where
  type Gamma Entity = [Entity]

  empty = Dynamic $ \ds -> (0:ds, SelReader $ const $ Reps.empty)
  upd m c = Dynamic $ \ds -> let ds' = fst $ getDepths m ds
                                 (d:ds'') = fst $ getDepths c ds'
                             in (succ d : ds'', SelReader $ \sels ->
                                    upd
                                    (getSels (snd $ getDepths m ds) sels)
                                    (getSels (snd $ getDepths c ds') sels))
  sel c = Dynamic $ \ds -> let (d:ds') = fst $ getDepths c ds
                           in (ds' ++ [d], SelReader $ \sels ->
                                  case (getSels (snd $ getDepths c ds) sels) of
                                    Ctx c' -> (sels !! length ds') c')
  
