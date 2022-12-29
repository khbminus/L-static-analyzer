{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Analysis.OptSupport where

import Statement
import Analysis.IR

fold_EE :: (a -> Expression -> a) -> a -> Expression      -> a
fold_EN :: (a -> Expression -> a) -> a -> Instruction e x -> a

fold_EE f z e@(Const _)         = f z e
fold_EE f z e@(VariableName _)  = f z e
fold_EE f z e@(FunctionCall _ exprs) = f (foldl f z exprs)  e
fold_EE f z e@(Application _ e1 e2)  =
  let afterE1 = fold_EE f z e1
      afterE2 = fold_EE f afterE1 e2
  in f afterE2 e
  
fold_EN _ z (Label _)       = z
fold_EN f z (Let _ e)       = f z e
fold_EN f z (If e _ _)      = f z e
fold_EN f z (Return es)     = maybe z (f z) es
fold_EN _ z (Goto _)        = z
fold_EN f z (Write e)       = f z e
fold_EN f z (Read e)        = z
fold_EN _ z (Skip _)        = z
fold_EN f z (Call _ _ es _) = foldl f z es