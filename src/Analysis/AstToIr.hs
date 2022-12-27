module Analysis.AstToIr(astToIR) where

import qualified Statement as A -- AST
import qualified Analysis.IR as I
import           Compiler.Hoopl hiding ((<*>))
import qualified Compiler.Hoopl as H ((<*>))

astToIR :: String -> A.Function -> I.M I.Proc
astToIR name (A.Function args body _) = do
    (entry, body') <- toBody body
    return $ I.Proc {I.name = name, I.args = args, I.body = body', I.entry = entry}

toBody :: [A.Statement] -> I.M (Label, Graph I.Instruction C C)
toBody bs = do
   let blocks = splitIntoBlocks bs
   (lastLabel, lastGraph) <- lastBlock
   (fullLabel, fullGraph) <- fullBlockTransform blocks lastLabel
   return (fullLabel, fullGraph |*><*| lastGraph)

lastBlock :: I.M (Label, Graph I.Instruction C C)
lastBlock = do
    label <- freshLabel
    return (label, mkFirst (I.Label label) H.<*> mkMiddles [] H.<*> mkLast (I.Return []))

splitIntoBlocks :: [A.Statement] -> [([A.Statement], Maybe A.Statement)]
splitIntoBlocks xs = splitHelper xs [] []
    where
        splitHelper :: [A.Statement] -> [A.Statement] -> [([A.Statement], Maybe A.Statement)] -> [([A.Statement], Maybe A.Statement)]
        splitHelper [] accBlock acc = acc ++ [(accBlock, Nothing)]
        splitHelper (x : xs) accBlock acc = case x of
            y@A.If {} -> splitHelper xs [] (acc ++ [(accBlock, Just y)])
            y@(A.While _ _) -> splitHelper xs [] (acc ++ [(accBlock, Just y)])
            y -> splitHelper xs (accBlock ++ [y]) acc

blockTransform :: ([A.Statement], Maybe A.Statement) -> Label -> I.M (Label, Graph I.Instruction C C)
blockTransform (code, last) next = do
    label <- freshLabel
    let ms = map toMid code
    (last', lastGraph) <- toLast last next
    let graph = mkFirst (I.Label label) H.<*> mkMiddles ms H.<*> mkLast last'
    return (label, graph |*><*| lastGraph)

fullBlockTransform :: [([A.Statement], Maybe A.Statement)] -> Label -> I.M (Label, Graph I.Instruction C C)
fullBlockTransform [] _ = error "Can't process empty body"
fullBlockTransform [x] next = blockTransform x next
fullBlockTransform (x : xs) next = do
    (realNextLabel, nextGraph) <- fullBlockTransform xs next
    (nowLabel, nowGraph) <- blockTransform x realNextLabel
    return (nowLabel, nowGraph |*><*| nextGraph)

toLast :: Maybe A.Statement -> Label -> I.M (I.Instruction O C, Graph I.Instruction C C)
toLast Nothing next = return (I.Goto next, emptyClosedGraph)
toLast (Just (A.If e t f)) next = do
    let trueBlocks = splitIntoBlocks t
    let falseBlocks = splitIntoBlocks f
    (trueLabel, trueGraph) <- fullBlockTransform trueBlocks next
    (falseLabel, falseGraph) <- fullBlockTransform falseBlocks next
    return (I.If e trueLabel falseLabel, trueGraph |*><*| falseGraph)
toLast (Just (A.While e s)) next = do
    let blocks = splitIntoBlocks s
    whileLabel <- freshLabel
    (sLabel, sGraph) <- fullBlockTransform blocks whileLabel
    let whileGraph = mkFirst (I.Label whileLabel) H.<*> mkMiddles [] H.<*> mkLast (I.If e sLabel next)
    return (I.Goto whileLabel, whileGraph |*><*| sGraph)
toLast _ _ = error "invalid last"


toMid :: A.Statement -> I.Instruction O O
toMid (A.Let v e) = I.Let v e
toMid (A.FunctionCallStatement  _ _) = undefined -- FIXME
toMid (A.FunctionDeclaration _ _) = undefined -- FIXME
toMid (A.Write expr) =  I.Write expr
toMid (A.Read expr) = I.Read expr
toMid A.Skip = I.Skip
toMid (A.While _ _) = error "can't be right here"
toMid A.If {} = error "can't be right here"