{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Representations.FOL.FOL where

import qualified FOL.FOL as FOL
import Representations.Eval.Model
import Representations.Representations
import GHC.TypeLits
import Data.Proxy


--------------------------------------------------------------------------------
-- | Data

-- | Intermediate FOL HOAS
data FolTerm a where
  Var_ :: FOL.Value -> FolTerm a
  Con :: String -> FolTerm a
  App :: FolTerm (a -> b) -> FolTerm a -> FolTerm b
  Lam :: Type a -> (FolTerm a -> FolTerm b) -> FolTerm (a -> b)
  TT :: FolTerm ()
  Pair :: FolTerm a -> FolTerm b -> FolTerm (a, b)
  Fst :: FolTerm (a, b) -> FolTerm a
  Snd :: FolTerm (a, b) -> FolTerm b
  And, Or, Impl :: FolTerm Bool -> FolTerm Bool -> FolTerm Bool
  True_, False_ :: FolTerm Bool
  Forall, Exists :: Type a -> (FolTerm a -> FolTerm Bool) -> FolTerm Bool
  Equals :: FolTerm a -> FolTerm a -> FolTerm Bool
  Ctx :: Gamma (FolTerm a) -> FolTerm (Gamma a)
  Sel :: Context a FolTerm => FolTerm (Gamma a) -> FolTerm a
  Type :: Type a -> FolTerm Bool

viewApp :: FolTerm a -> Maybe (String, [FOL.Value])
viewApp (Con x) = Just (x,[])
viewApp (App x y) = case viewApp x of
  Just (f, args) -> Just (f, args ++ [termToFol y])
  _ -> Nothing
viewApp _ = Nothing

termToFol :: FolTerm a -> FOL.Value
termToFol x = case viewApp x of
  Just (f, args) -> FOL.VApp f (args)
  Nothing -> 
   case x of
     Var_ x -> x
     True_  -> FOL.VTru
     False_ -> FOL.VFal
     And x y -> FOL.VAnd (termToFol x) (termToFol y)
     Impl x y -> FOL.VOr (FOL.VNot (termToFol x)) (termToFol y)
     Or x y -> FOL.VOr (termToFol x) (termToFol y)
     Forall _ f -> FOL.VAll (\x -> termToFol (f (Var_ x)))
     Exists _ f -> FOL.VExi (\x -> termToFol (f (Var_ x)))
     _ -> error "termToFol: unsupported input"


--------------------------------------------------------------------------------
-- | Instances

-- | CCCs
instance Lambda FolTerm where
  app m n = case m of
              Lam t f -> f n
              _ -> App m n
  lam = Lam knownType

instance Cartesian FolTerm where
  unit = TT
  pair = Pair
  fst_ m = case m of
             Pair n o -> n
             _ -> Fst m
  snd_ m = case m of
             Pair n o -> o
             _ -> Snd m

-- | Constants
instance KnownSymbol str => Constant ty str FolTerm where
  c = Con $ symbolVal (Proxy @str)

-- | Logic
instance Heyting FolTerm where
  phi /\ psi = case phi of
                 True_ -> psi
                 False_ -> False_
                 _ -> case psi of
                        True_ -> phi
                        False_ -> False_
                        _ -> And phi psi
  phi \/ psi = case phi of
                 True_ -> True_
                 False_ -> psi
                 _ -> case psi of
                        True_ -> True_
                        False_ -> phi
                        _ -> Or phi psi
  phi --> psi = case phi of
                  True_ -> psi
                  False_ -> True_
                  _ -> case psi of
                         True_ -> True_
                         _ -> Impl phi psi
  true = True_
  false = False_

instance KnownType a => HOL a FolTerm where
  forall = Forall knownType . app
  exists = Exists knownType . app

instance Equality a FolTerm where
  equals = Equals

-- | Contexts
instance Context a FolTerm where
  type Gamma a = [a]
  empty = Ctx []
  upd m (Ctx c) = Ctx $ m:c
  sel = Sel
