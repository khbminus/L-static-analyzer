{-# LANGUAGE GADTs,TypeFamilies #-}
module Analysis.IrToAst(procToFunc, irToAst, fromProcToBlocks) where

import qualified Statement       as A
import qualified Analysis.IR     as I
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Compiler.Hoopl
import           Control.Monad.State
import           Data.Maybe ( fromMaybe )


-- intermediate data class to represent part in transformation
-- 1) Convert Graph to named blocks of middles and refs to next blocks
-- 2) Recursively 
data IRBlock = IRBlock {label :: Label, body :: [A.Statement], next :: I.Instruction O C} deriving (Show)
data IRProc = IRProc {procName :: String, args :: [String], procBody :: [IRBlock]} deriving (Show)

-- ******** BLOCKS GENERATION ********

fromIrInstCO :: I.Instruction C O -> () -> (Label, [A.Statement])
fromIrInstCO inst _ = case inst of
    I.Label l -> (l, [])

fromIrInstOO :: I.Instruction O O -> (Label, [A.Statement]) -> (Label, [A.Statement])
fromIrInstOO inst acc = case inst of
    I.Let v e -> getNext (A.Let v e) acc
    I.Read v -> getNext (A.Read v) acc
    I.Write e -> getNext (A.Write e) acc
    I.Skip -> getNext A.Skip acc
    where
        getNext :: A.Statement -> (Label, [A.Statement]) -> (Label, [A.Statement])
        getNext stmt (label, accInstructions) = (label, stmt : accInstructions)

fromIrInstOC :: I.Instruction O C -> (Label, [A.Statement]) -> IRBlock
fromIrInstOC inst (label, body) = IRBlock {label = label, body = reverse body, next = inst}

type instance IndexedCO C () (String, [A.Statement]) = ()
type instance IndexedCO C IRBlock (String, [A.Statement]) = IRBlock

fromBlock :: Block I.Instruction C C -> () -> IRBlock
fromBlock = foldBlockNodesF3 (fromIrInstCO, fromIrInstOO, fromIrInstOC)

fromGraph :: Label -> Graph I.Instruction C C -> [IRBlock]
fromGraph entry g = let entryNode = gUnitOC (BlockOC BNil (I.Goto entry))
                        blks = reverse $ postorder_dfs (gSplice entryNode g)
                    in foldl helper [] blks
                    where
                        helper :: [IRBlock] -> Block I.Instruction C C -> [IRBlock]
                        helper p blk = (fromBlock blk () : p)

fromProcToBlocks :: I.Proc -> IRProc
fromProcToBlocks I.Proc {I.name = n, I.args = a, I.body = body, I.entry = ent} = IRProc {procName = n, procBody = fromGraph ent body, args = a}

-- ******** BLOCK MERGING ********

data IRBlockStorage = IRBlockStorage {blocks :: M.Map Label IRBlock, ast :: M.Map Label [A.Statement], returns :: M.Map Label (Maybe A.Expression)}

getSuffix :: S.Set Label -> Label -> State IRBlockStorage [A.Statement]
getSuffix whileHeads lbl = do
    storage <- get
    case M.lookup lbl (ast storage) of
        Just stms -> return stms
        Nothing -> do
            if S.member lbl whileHeads then return []
                else  case M.lookup lbl (blocks storage) of
                Just block -> transformBlock block whileHeads
                Nothing -> error $ "Can't find block with label " ++ show lbl

transformLast :: Label -> I.Instruction O C -> S.Set Label -> State IRBlockStorage [A.Statement]
transformLast lbl inst whileHeads = do
    case inst of
        I.Call name args next -> fmap (A.FunctionCallStatement name args:) (getSuffix whileHeads next)
        I.Goto next -> getSuffix whileHeads next
        I.If e t f -> do
            tCode <- getSuffix whileHeads t
            fCode <- getSuffix whileHeads f
            return [A.If e tCode fCode]
        I.While e start next -> do
            whileBody <- getSuffix (S.insert lbl whileHeads) start
            suffix <- getSuffix whileHeads next
            return $ A.While e whileBody : suffix
        I.Return x -> do
            mp <- get
            let rts = returns mp
            if M.member lbl rts then error $ "Too many returns for label " ++ show lbl
            else do
                put $ mp {returns = M.insert lbl x rts}
                return []



transformBlock :: IRBlock -> S.Set Label -> State IRBlockStorage [A.Statement]
transformBlock blk whileHeads = do
    if S.member (label blk) whileHeads then return []
    else do
        suffixBody <- transformLast (label blk) (next blk) whileHeads
        let body' = body blk ++ suffixBody
        mp <- get
        if not $ M.member (label blk) (ast mp) then do
            put $ mp {ast = M.insert (label blk) body' (ast mp)}
            return body'
        else do
            return body'

findReturn :: Label -> S.Set Label -> State IRBlockStorage (Maybe (Maybe A.Expression))
findReturn blk used = do
    storage <- get
    case M.lookup blk (blocks storage) of
        Nothing -> return Nothing
        Just blk' -> 
            case next blk' of
                I.Return x -> return $ Just x
                I.Goto l -> go l
                I.Call _ _ l -> go l
                I.If _ t _ -> go t -- only one return is possible, moreover return is placed in end of each branch of if
                I.While _ _ l -> go l
    where
        go :: Label -> State IRBlockStorage (Maybe (Maybe A.Expression))
        go l = if S.member l used then return Nothing else findReturn l (S.insert l used)


procToFunc :: I.Proc -> A.Statement
procToFunc p = let irProc = fromProcToBlocks p in
               let blocks' = map (\x -> (label x, x)) (procBody irProc) in
               let storage = IRBlockStorage {blocks = M.fromList blocks', ast = M.empty, returns = M.empty } in
               let (body', storage') = runState (transformBlock (get' storage) S.empty) storage in
               A.FunctionDeclaration (procName irProc) (A.Function (args irProc) body' (getReturn storage'))
               where
                get' :: IRBlockStorage -> IRBlock
                get' s = case M.lookup (I.entry p) (blocks s) of
                    Just x -> x
                    Nothing -> error $ "can't find start block with label " ++ show (I.entry p)
                getReturn :: IRBlockStorage -> Maybe A.Expression
                getReturn s = fromMaybe Nothing $ evalState (findReturn (I.entry p) S.empty) s

irToAst :: [I.Proc] -> [A.Statement]
irToAst = map procToFunc