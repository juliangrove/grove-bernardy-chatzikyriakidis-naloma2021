{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Fragment.Fragment where

import Prelude hiding (Monad(..))
import qualified Prelude as Prel
import Representations.Eval.Model
import Representations.Algebraic
import Representations.Representations
import Representations.FOL.FOL
import FOL.FOL
import FOL.Solver
import Representations.Bayesian.Bayesian
import qualified Representations.Bayesian.Probability as P

--------------------------------------------------------------------------------
-- | Monadic application combinators

-- | Forward application
(|>) :: (Functor (FreeGM g),
         Functor (FreeGM f),
         Lambda repr)
     => FreeGM f (repr (v -> w))
     -> FreeGM g (repr v)
     -> FreeGM (Monoidal (f ∘ g)) (repr w)
m |> n = join $ fmap (\f -> fmap (\x -> app f x) n) m

-- | Backward application
(<|) :: (Functor (FreeGM g),
         Functor (FreeGM f),
         Lambda repr)
     => FreeGM f (repr v)
     -> FreeGM g (repr (v -> w))
     -> FreeGM (Monoidal (f ∘ g)) (repr w)
m <| n = join $ fmap (\x -> fmap (\f -> app f x) n) m

-- | Discourse update
(>@) :: (Functor (FreeGM f),
         Functor (FreeGM g),
         Heyting repr)
     => FreeGM f (T repr)
     -> FreeGM g (T repr)
     -> FreeGM (Monoidal (f ∘ g)) (T repr)
phi >@ psi = join $ fmap (\p -> fmap (\q -> p /\ q) psi) phi


--------------------------------------------------------------------------------
-- | Lexicon

every' :: (Lambda repr,
           Heyting repr,
           HOL Entity repr)
       => Determiner repr
every' pred k = forall $ lam (\x -> app pred x --> app k x)

every :: (Lambda repr,
          Heyting repr,
          HOL Entity repr)
      => FreeGM f (repr (Entity -> Bool))
      -> FreeGM (Det repr ∘ Monoidal (f ∘ (Scope repr ∘ Id))) (E repr)
every pred = do
  q <- det every'
  pred' <- pred
  scope (q pred')
  
some :: Lambda repr
     => repr (Entity -> Bool) -> FreeGM (Choose repr Entity ∘ Id) (E repr)
some pred = choose pred

bind :: Context a repr
     => FreeGM f (repr a)
     -> FreeGM
         (Monoidal (f ∘ (Get (repr (Gamma a)) ∘ (Put (repr (Gamma a)) ∘ Id))))
         (repr a)
bind m = do
  x <- m
  s <- get
  put (upd x s)
  return x

it, he, she, him, her :: Context a repr
                      => FreeGM (Get (repr (Gamma a)) ∘ Id) (repr a)
it = do
  s <- get
  return (sel s)
he = it
she = it
him = it
her = it


who :: (Lambda repr,
        Heyting repr)
    => repr ((Entity -> Bool) -> (Entity -> Bool) -> Entity -> Bool)
who = lam (\p -> lam (\q -> lam (\x -> app p x /\ app q x)))

that :: (Lambda repr,
        Heyting repr)
     => repr ((Entity -> Bool) -> (Entity -> Bool) -> Entity -> Bool)
that = lam (\p -> lam (\q -> lam (\x -> app p x /\ app q x)))

is :: Lambda repr => repr ((Entity -> Bool) -> Entity -> Bool)
is = lam id

for :: Lambda repr => repr (Entity -> Entity)
for = lam id

-- | One-place predicates

animate :: Constant (Entity -> Bool) "animate" repr
        => repr (Entity -> Bool)
animate = c @(Entity -> Bool) @"animate" 

command :: Constant (Entity -> Bool) "command" repr
        => repr (Entity -> Bool)
command = c @(Entity -> Bool) @"command"

dog :: Constant (Entity -> Bool) "dog" repr
    => repr (Entity -> Bool)
dog = c @(Entity -> Bool) @"dog"

cat :: Constant (Entity -> Bool) "cat" repr
    => repr (Entity -> Bool)
cat = c @(Entity -> Bool) @"cat"

ginger_ale :: Constant (Entity -> Bool) "ginger-ale" repr
           => repr (Entity -> Bool)
ginger_ale = c @(Entity -> Bool) @"ginger-ale"

good :: Constant (Entity -> Bool) "good" repr
     => repr (Entity -> Bool)
good = c @(Entity -> Bool) @"good"

happy :: Constant (Entity -> Bool) "happy" repr
      => repr (Entity -> Bool)
happy = c @(Entity -> Bool) @"happy"

prepared :: Constant (Entity -> Bool) "prepared" repr
         => repr (Entity -> Bool)
prepared = c @(Entity -> Bool) @"prepared"

thing :: (Lambda repr, Heyting repr) => repr (Entity -> Bool)
thing = lam (const true)

-- | Two-place predicates

chase :: Constant (Entity -> Entity -> Bool) "chase" repr
      => repr (Entity -> Entity -> Bool)
chase = c @(Entity -> Entity -> Bool) @"chase"

catch :: Constant (Entity -> Entity -> Bool) "catch" repr
      => repr (Entity -> Entity -> Bool)
catch = c @(Entity -> Entity -> Bool) @"catch"

drank :: Constant (Entity -> Entity -> Bool) "drank" repr
      => repr (Entity -> Entity -> Bool)
drank = c @(Entity -> Entity -> Bool) @"drank"

see :: Constant (Entity -> Entity -> Bool) "see" repr
    => repr (Entity -> Entity -> Bool)
see = c @(Entity -> Entity -> Bool) @"see"

waiting :: Constant (Entity -> Entity -> Bool) "wait" repr
        => repr (Entity -> Entity -> Bool)
waiting = c @(Entity -> Entity -> Bool) @"wait"

-- | Names and definite descriptions

amy :: Constant Entity "amy" repr
    => repr Entity
amy = c @Entity @"amy"

ashley :: Constant Entity "ashley" repr
       => repr Entity
ashley = c @Entity @"ashley"

emacs :: Constant Entity "emacs" repr
      => repr Entity
emacs = c @Entity @"emacs"

matt :: Constant Entity "matt" repr
     => repr Entity
matt = c @Entity @"matt"

jean_philippe :: Constant Entity "jean-philippe" repr
              => repr Entity
jean_philippe = c @Entity @"jean-philippe"

julian :: Constant Entity "julian" repr
       => repr Entity
julian = c @Entity @"julian"

stergios :: Constant Entity "stergios" repr
         => repr Entity
stergios = c @Entity @"stergios"

the_command :: Constant Entity "the_command" repr
            => repr Entity
the_command = c @Entity @"the_command"

-- | Evaluate a sentence into (a representation of) a Bool.
runSentence :: forall repr a b p f.
               (Lambda repr,
                Cartesian repr,
                Heyting repr,
                KnownType p,
                QuantifyTuple p repr,
                Context b repr,
                Handleable f p (repr (Gamma b)) repr)
            => FreeGM f (T repr) -> T repr
runSentence phi = eval_with @repr @a exists (/\) (handle phi) (empty @b @repr)
