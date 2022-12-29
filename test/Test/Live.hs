{-# LANGUAGE NamedFieldPuns #-}
module Test.Live where

import Compiler.Hoopl
import Analysis.IR (Proc (..), M)
import Statement (Statement)
import Grammar (parseStatement)
import Data.Either (isLeft, fromRight)
import Analysis.AstToIr (astToIR)
import Analysis.Live (liveLattice, liveness, deadAsstElim)

import Test.Tasty.HUnit (assertBool)

type ErrorM = Either String

optTest' :: M [Proc] -> ErrorM (M [Proc])
optTest' procs =
    return $ procs >>= mapM optProc
  where
    optProc proc@Proc {entry, body, args} = do
        (body', _, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) body mapEmpty
        return $ proc { body = body' }
    bwd = BwdPass { bp_lattice  = liveLattice
                  , bp_transfer = liveness
                  , bp_rewrite  = deadAsstElim
                  }

parse :: String -> Maybe [Statement]
parse str = if any isLeft parsed
    then Nothing
    else Just $ foldl f [] parsed
    where
        parse' ls = map parseStatement ls
        parsed = parse' $ lines str
        -- f :: [Statement] -> Either (ParseErrorBundle String Void) [Statement] -> [Statement]
        f b a = b ++ fromRight [] a

optimize :: String -> String
optimize text = do
    case fmap astToIR (parse text) of
        Nothing -> error "Parsing error"
        Just ir -> case optTest' (fmap snd ir) of
            Left err -> error err
            Right p  -> do
                let opted = runSimpleUniqueMonad $ runWithFuel fuel p
                    -- lbmaps = runSimpleUniqueMonad $ runWithFuel fuel (liftM (fst . unzip) p)
                    -- expected = runSimpleUniqueMonad $ runWithFuel fuel exps
                -- TODO: get Instructions from [Proc]
                -- foldl (++) [] (map show opted)
  where
    fuel = 9999

unit_Liveness = do
    -- putStrLn $ optimize "def f() { x := 1; y := 5; y := x }" 