{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Fragment.RSA_Examples where

import Prelude hiding (Monad(..))
import qualified Prelude as Prel
import Representations.Eval.Model
import Representations.Algebraic
import Representations.Representations
import Representations.FOL.FOL
import FOL.Solver
import Representations.Bayesian.Bayesian
import qualified Representations.Bayesian.Probability as P
import Fragment.Fragment


--------------------------------------------------------------------------------
-- | Example discourses

-- | 1. Emacs is waiting for the command. It is prepared.
-- 
-- Background knowledge:
-- - 'Emacs is waiting for something': 0.05
-- - 'Emacs is prepared': 0.05
-- - 'Emacs is animate': 0.2
-- - 'the command is waiting for someting': 0.05
-- - 'the command is prepared': 0.05
-- - 'the command is animate': 0.2
--
-- Filtered by the following lexical entailments:
-- - 'everything that is waiting for something is animate'
-- - 'everything that is prepared is animate'

ex1 temp npC pC = let runwait = runSentence @FolTerm @Entity @Entity
                      runprepared = runSentence @FolTerm @Entity @Entity
                      runanimate = runSentence @FolTerm @Entity @Entity
                      runwaitanimate = runSentence @FolTerm @Entity @Entity
                      runpreparedanimate = runSentence @FolTerm @Entity @Entity
                      runu = runSentence @Dynamic @Entity @Entity
                      emacsWaitingPremise = runwait (
                        return emacs <| (return is |> (return waiting |> (return for |> some thing)))
                        )
                      emacsPreparedPremise = runprepared (
                        return emacs <| (return is |> return prepared)
                        )
                      emacsAnimatePremise = runanimate (
                        return emacs <| (return is |> return animate)
                        )
                      tcWaitingPremise = runwait (
                        return the_command <| (return is |> (return waiting |> (return for |> some thing)))
                        )
                      tcPreparedPremise = runprepared (
                        return the_command <| (return is |> return prepared)
                        )
                      tcAnimatePremise = runanimate (
                        return the_command <| (return is |> return animate)
                        )
                      waitingAnimate = runwaitanimate ( -- lexical entailment
                        every (return thing <| (return that |> (return is |> (return waiting |> (return for |> some thing))))) <| (return is |> return animate)
                        )
                      preparedAnimate = runpreparedanimate ( -- lexical entailment
                        every (return thing <| (return that |> (return is |> return prepared))) <| (return is |> return animate)
                        )
                      u = runu (
                        bind (return emacs) <| (return is |> (return waiting |> (return for |> bind (return the_command)))) -- antecedent
                        >@
                        (it <| (return is |> return prepared)) -- anaphora
                        )
                  in RSA { gamma = P.normalize $
                           makeBernoulli emacsWaitingPremise 0.05 Prel.>>= \ewp ->
                             makeBernoulli tcWaitingPremise 0.05 Prel.>>= \tcwp ->
                             makeBernoulli emacsPreparedPremise 0.05 Prel.>>= \epp ->
                             makeBernoulli tcPreparedPremise 0.05 Prel.>>= \tcpp ->
                             makeBernoulli emacsAnimatePremise 0.2 Prel.>>= \eap ->
                             makeBernoulli tcAnimatePremise 0.2 Prel.>>= \tcap ->
                             let premises = [ewp, tcwp, epp, tcpp, eap, tcap] in
                             filterByProp premises waitingAnimate Prel.>>
                             filterByProp premises preparedAnimate Prel.>>
                             Prel.return premises
                         , utterance = u
                         , alpha = temp
                         , npCost = npC
                         , pronounCost = pC }

-- | 2. Ashley is waiting for Amy. She seems her.
-- 
-- Background knowledge:
-- - 'Ashley is waiting for something': 0.05
-- - 'Ashley sees something': 0.05
-- - 'Ashley is animate': 0.9
-- - 'Amy is waiting for someting': 0.05
-- - 'Amy sees something': 0.05
-- - 'Amy is animate': 0.9
--
-- Filtered by the following lexical entailments:
-- - 'everything that is waiting for something is animate'
-- - 'everything that sees something is animate'

ex2 temp npC pC = let runwait = runSentence @FolTerm @Entity @Entity
                      runsee = runSentence @FolTerm @Entity @Entity
                      runanimate = runSentence @FolTerm @Entity @Entity
                      runwaitanimate = runSentence @FolTerm @Entity @Entity
                      runseeanimate = runSentence @FolTerm @Entity @Entity
                      runu = runSentence @Dynamic @Entity @Entity
                      ashleyWaitingPremise = runwait (
                        return ashley <| (return is |> (return waiting |> (return for |> some thing)))
                        )
                      ashleySeePremise = runsee (
                        return ashley <| (return see |> some thing)
                        )
                      ashleyAnimatePremise = runanimate (
                        return ashley <| (return is |> return animate)
                        )
                      amyWaitingPremise = runwait (
                        return amy <| (return is |> (return waiting |> (return for |> some thing)))
                        )
                      amySeePremise = runsee (
                        return amy <| (return see |> some thing)
                        )
                      amyAnimatePremise = runanimate (
                        return amy <| (return is |> return animate)
                        )
                      waitingAnimate = runwaitanimate ( -- lexical entailment
                        every (return thing <| (return that |> (return is |> (return waiting |> (return for |> some thing))))) <| (return is |> return animate)
                        )
                      seeAnimate = runseeanimate ( -- lexical entailment
                        every (return thing <| (return that |> (return see |> some thing))) <| (return is |> return animate)
                        )
                      u = runu (
                        bind (return ashley) <| (return is |> (return waiting |> (return for |> bind (return amy)))) -- antecedent
                        >@
                        (she <| (return see |> her)) -- anaphora
                        )
                  in RSA { gamma = P.normalize $
                           makeBernoulli ashleyWaitingPremise 0.05 Prel.>>= \aswp ->
                             makeBernoulli amyWaitingPremise 0.05 Prel.>>= \amwp ->
                             makeBernoulli ashleySeePremise 0.05 Prel.>>= \assp ->
                             makeBernoulli amySeePremise 0.05 Prel.>>= \amsp ->
                             makeBernoulli ashleyAnimatePremise 0.9 Prel.>>= \asap ->
                             makeBernoulli amyAnimatePremise 0.9 Prel.>>= \amap ->
                             let premises = [aswp, amwp, assp, amsp, asap, amap] in
                             filterByProp premises waitingAnimate Prel.>>
                             filterByProp premises seeAnimate Prel.>>
                             Prel.return premises
                         , utterance = u
                         , alpha = temp
                         , npCost = npC
                         , pronounCost = pC }
