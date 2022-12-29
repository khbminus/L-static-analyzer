{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Analysis.AstToIr(astToIR, LabelMap) where

import           Compiler.Hoopl hiding ((<*>), LabelMap)
import qualified Compiler.Hoopl as H ((<*>))
import qualified Data.Map       as M
import qualified Statement      as A -- AST
import qualified Analysis.IR    as I
import Control.Monad ( ap, liftM )

-- astToIR transforming file with functions to IR Graph
-- It ignores code that not in function
astToIR :: [A.Statement] -> I.M (LabelMap, [I.Proc])
astToIR code = run $ do
    mapM getFunction (filter isFunctionDeclaration code)

getFunction :: A.Statement -> LabelMapM I.Proc
getFunction (A.FunctionDeclaration name (A.Function args body _)) = do
    (entry, body') <- toBody body
    putLabel name entry
    return $ I.Proc {I.name = name, I.args = args, I.body = body', I.entry = entry}
getFunction _ = error "only function declaration are supported"

toBody :: [A.Statement] -> LabelMapM (Label, Graph I.Instruction C C)
toBody body = do
   let blocks = splitIntoBlocks body
   (lastLabel, lastGraph) <- lastBlock
   (fullLabel, fullGraph) <- fullBlockTransform blocks lastLabel
   return (fullLabel, fullGraph |*><*| lastGraph)

lastBlock :: LabelMapM (Label, Graph I.Instruction C C)
lastBlock = do
    label <- newLabel
    return (label, mkFirst (I.Label label) H.<*> mkMiddles [] H.<*> mkLast (I.Return Nothing))

splitIntoBlocks :: [A.Statement] -> [([A.Statement], Maybe A.Statement)]
splitIntoBlocks xs = splitHelper xs [] []
    where
        splitHelper :: [A.Statement] -> [A.Statement] -> [([A.Statement], Maybe A.Statement)] -> [([A.Statement], Maybe A.Statement)]
        splitHelper [] accBlock acc = acc ++ [(accBlock, Nothing)]
        splitHelper (x : xs) accBlock acc = case x of
            y@A.If {} -> splitHelper xs [] (acc ++ [(accBlock, Just y)])
            y@(A.While _ _) -> splitHelper xs [] (acc ++ [(accBlock, Just y)])
            y@(A.FunctionCallStatement _ _) -> splitHelper xs [] (acc ++ [(accBlock, Just y)])
            y -> splitHelper xs (accBlock ++ [y]) acc

blockTransform :: ([A.Statement], Maybe A.Statement) -> Label -> LabelMapM (Label, Graph I.Instruction C C)
blockTransform (code, last) next = do
    label <- newLabel
    let ms = map toMid code
    (last', lastGraph) <- toLast last next
    let graph = mkFirst (I.Label label) H.<*> mkMiddles ms H.<*> mkLast last'
    return (label, graph |*><*| lastGraph)

fullBlockTransform :: [([A.Statement], Maybe A.Statement)] -> Label -> LabelMapM (Label, Graph I.Instruction C C)
fullBlockTransform [] _ = error "Can't process empty body"
fullBlockTransform [x] next = blockTransform x next
fullBlockTransform (x : xs) next = do
    (realNextLabel, nextGraph) <- fullBlockTransform xs next
    (nowLabel, nowGraph) <- blockTransform x realNextLabel
    return (nowLabel, nowGraph |*><*| nextGraph)

toLast :: Maybe A.Statement -> Label -> LabelMapM (I.Instruction O C, Graph I.Instruction C C)
toLast Nothing next = return (I.Goto next, emptyClosedGraph)
toLast (Just (A.If e t f)) next = do
    let trueBlocks = splitIntoBlocks t
    let falseBlocks = splitIntoBlocks f
    (trueLabel, trueGraph) <- fullBlockTransform trueBlocks next
    (falseLabel, falseGraph) <- fullBlockTransform falseBlocks next
    return (I.If e trueLabel falseLabel, trueGraph |*><*| falseGraph)
toLast (Just (A.While e s)) next = do
    let blocks = splitIntoBlocks s
    whileLabel <- newLabel
    (sLabel, sGraph) <- fullBlockTransform blocks whileLabel
    let whileGraph = mkFirst (I.Label whileLabel) H.<*> mkMiddles [] H.<*> mkLast (I.While e sLabel next)
    return (I.Goto whileLabel, whileGraph |*><*| sGraph)
toLast (Just (A.FunctionCallStatement name args)) next = return (I.Call name args next, emptyClosedGraph)
toLast _ _ = error "invalid last"


toMid :: A.Statement -> I.Instruction O O
toMid (A.Let v e) = I.Let v e
toMid (A.FunctionCallStatement _ _) = error "can't be right here"
toMid (A.FunctionDeclaration _ _) = error "Non top-level function declaration is not allowed" -- FIXME
toMid (A.Write expr) =  I.Write expr
toMid (A.Read expr) = I.Read expr
toMid A.Skip = I.Skip
toMid (A.While _ _) = error "can't be right here"
toMid A.If {} = error "can't be right here"

run :: LabelMapM a -> I.M (LabelMap, a)
run (LabelMapM f) = f M.empty

type LabelMap = M.Map String Label
data LabelMapM a = LabelMapM (LabelMap -> I.M (LabelMap, a))


instance Functor LabelMapM where
    fmap = liftM

instance Applicative LabelMapM where
    pure x = LabelMapM (\m -> return (m, x))
    (<*>) = ap

instance Monad LabelMapM where
    return = pure
    LabelMapM f1 >>= k = LabelMapM (\m ->
        do
            (m', x) <- f1 m
            let (LabelMapM f2) = k x
            f2 m'
        )

putLabel :: String -> Label -> LabelMapM ()
putLabel name label = LabelMapM f
    where f m = 
            do
                return (M.insert name label m, ())

newLabel :: LabelMapM Label
newLabel = LabelMapM f
    where f m =
            do
                l <- freshLabel
                return (m, l)

isFunctionDeclaration :: A.Statement -> Bool
isFunctionDeclaration (A.FunctionDeclaration _ _ ) = True
isFunctionDeclaration _ = False