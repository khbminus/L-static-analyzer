{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE GADTs #-}
module Analysis.OptSupport where

import Statement
import Analysis.IR

fold_EE :: (a -> Expression -> a) -> a -> Expression      -> a
fold_EN :: (a -> Expression -> a) -> a -> Instruction e x -> a

fold_EE f z e@(Const _)              = f z e
fold_EE f z e@(VariableName _)       = f z e
fold_EE f z e@(FunctionCall _ exprs) = f (foldl f z exprs)  e
fold_EE f z e@(Application _ e1 e2)  =
  let afterE1 = fold_EE f z e1
      afterE2 = fold_EE f afterE1 e2
  in f afterE2 e

fold_EN _ z (Label _)                 = z
fold_EN f z (Analysis.IR.Let _ e)     = f z e
fold_EN f z (Analysis.IR.If e _ _)    = f z e
fold_EN f z (Return es)               = maybe z (f z) es
fold_EN _ z (Goto _)                  = z
fold_EN f z (Analysis.IR.Write e)     = f z e
fold_EN _ z (Analysis.IR.Read _)      = z
fold_EN _ z Analysis.IR.Skip          = z
fold_EN f z (Call _ es _)             = foldl f z es
fold_EN f z (Analysis.IR.While e _ _) = f z e