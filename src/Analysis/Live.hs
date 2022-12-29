{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Analysis.Live where

import Data.Maybe
import qualified Data.Set as S

import Compiler.Hoopl
import Analysis.IR
import Analysis.OptSupport
import Statement (Expression(VariableName))

type Var = String
type Live = S.Set Var

liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice
  { fact_name = "Live variables"
  , fact_bot  = S.empty
  , fact_join = add
  }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `S.union` old
              ch = changeIf (S.size j > S.size old)

liveness :: BwdTransfer Instruction Live
liveness = mkBTransfer live
  where
    live :: Instruction e x -> Fact x Live -> Live
    live   (Label _)       f = f
    live n@(Let x _)       f = addUses (S.delete x f) n
    live n@(Read x)        f = addUses (S.delete x f) n
    live n@(Write _)       f = addUses f n
    live n@(Goto l)        f = addUses (fact f l) n
    live n@(If _ tl fl)    f = addUses (fact f tl `S.union` fact f fl) n
    live n@(Call _ _ l)    f = addUses (fact f l) n
    live n@(Return _)      _ = addUses (fact_bot liveLattice) n
    live Skip              f = f
    live n@(While _ l1 l2) f = addUses (fact f l1 `S.union` fact f l2) n

    fact :: FactBase (S.Set Var) -> Label -> Live
    fact f l = fromMaybe S.empty $ lookupFact l f

    addUses :: S.Set Var -> Instruction e x -> Live
    addUses = fold_EN (fold_EE addVar)

    addVar s (VariableName v) = S.insert v s
    addVar s _                = s

deadAsstElim :: forall m . FuelMonad m => BwdRewrite m Instruction Live
deadAsstElim = mkBRewrite d
  where
    d :: Instruction e x -> Fact x Live -> m (Maybe (Graph Instruction e x))
    d (Let x _) live
        | not (x `S.member` live) = return $ Just emptyGraph
    d _ _ = return Nothing
