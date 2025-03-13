{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Codegen where

import Backend (parseAll)
import Control.Monad.Trans.State (State, get, modify, runState)
import Data.ByteString.Char8 (unpack)
import Data.Map
import Data.Set (Set, empty, fromList, member)
import Debug.Trace
import Grammar
import LLVM (moduleLLVMAssembly, withModuleFromAST)
import LLVM.AST hiding (function)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.CallingConvention as LLVM.CallingConvention
import qualified LLVM.AST.Constant as LLVM
import LLVM.AST.Global (Global (basicBlocks, linkage, name, parameters, returnType, visibility))
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Linkage as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Visibility as LLVM
import LLVM.Context (withContext)

newtype SymTable = SymTable {table :: Map String LLVM.Type}

getFunctionType :: String -> SymTable -> LLVM.Type
getFunctionType s t = table t ! trace s s

data CodeState = CodeState
  { inst :: [LLVM.Named LLVM.Instruction],
    initInst :: [LLVM.Named LLVM.Instruction],
    symTable :: SymTable,
    blocks :: Map String LLVM.BasicBlock,
    b :: [[LLVM.Named LLVM.Instruction]],
    bId :: Int,
    defs :: [LLVM.Definition],
    vId :: Int,
    registered :: Set String
  }

type CodeGen = State CodeState

defaultState :: CodeState
defaultState = CodeState {inst = [], initInst = [], symTable = SymTable {table = Data.Map.empty}, b = [[]], blocks = Data.Map.empty, bId = 0, defs = [], vId = 0, registered = Data.Set.empty}

alloca :: String -> CodeGen LLVM.Operand
alloca s = do
  pushInst (LLVM.mkName s := LLVM.Alloca LLVM.i32 Nothing 0 [])
  return $ LLVM.LocalReference LLVM.i32 (LLVM.mkName s)

appendInst :: LLVM.Named LLVM.Instruction -> CodeState -> CodeState
appendInst i old = old {b = let lb : ob = b old in (lb ++ [i]) : ob}

pushInst :: LLVM.Named LLVM.Instruction -> CodeGen ()
pushInst i = modify $ \s -> appendInst i s

assign :: LLVM.Operand -> LLVM.Operand -> CodeGen ()
assign dst v = pushInst (Do $ LLVM.Store False dst v Nothing 0 [])

flushBlock :: LLVM.Terminator -> CodeGen ()
flushBlock t = do
  s <- getBlockUid
  modify $ \old -> old {blocks = insert s (LLVM.BasicBlock (LLVM.mkName (trace ("Flushing " ++ s) s)) (head $ trace (show $ head (b old)) (b old)) (Do t)) (blocks old), b = let _ : ob = b old in ob}

flushBlockWithName :: String -> LLVM.Terminator -> CodeGen ()
flushBlockWithName s t = modify $ \old -> old {blocks = insert s (LLVM.BasicBlock (LLVM.mkName (trace ("Flushing " ++ s) s)) (head $ trace (show $ head (b old)) (b old)) (Do t)) (blocks old), b = let _ : ob = b old in ob}

getBlockUid :: CodeGen String
getBlockUid = show . bId <$> get

renewBlockUid :: CodeGen String
renewBlockUid = do
  modify $ \s -> s {bId = bId s + 1}
  getBlockUid

getVId :: CodeGen String
getVId = show . vId <$> get

newVId :: CodeGen String
newVId = do
  modify $ \s -> s {vId = trace (show $ vId s) (vId s) + 1}
  getVId

enterBlock :: CodeGen String
enterBlock = do
  uid <- renewBlockUid
  modify $ \old -> old {b = [] : b old}
  return $ trace ("entering block" ++ uid) uid

define :: String -> [String] -> [BasicBlock] -> CodeGen ()
define i args bs = do
  modify $ \old ->
    old
      { defs =
          LLVM.GlobalDefinition
            functionDefaults
              { name = LLVM.mkName i,
                parameters = (Prelude.map (\s -> LLVM.Parameter LLVM.i32 (LLVM.mkName s) []) args, False),
                returnType = LLVM.i32,
                basicBlocks = bs
              }
            : defs old,
        symTable = SymTable $ insert i (LLVM.FunctionType LLVM.i32 (Prelude.map (const LLVM.i32) args) False) (table $ symTable old)
      }

addSig :: String -> [String] -> CodeGen ()
addSig i args = do
  modify $ \old ->
    old
      { symTable = SymTable $ insert i (LLVM.FunctionType LLVM.i32 (Prelude.map (const LLVM.i32) args) False) (table $ symTable old)
      }

assignInstr :: LLVM.Instruction -> CodeGen LLVM.Operand
assignInstr i = do
  vid <- newVId
  pushInst $ LLVM.mkName vid := i
  return $ LLVM.LocalReference LLVM.i32 (LLVM.mkName vid)

gen :: Expr -> CodeGen LLVM.Operand
gen (Var n) = do
  set <- registered <$> get
  (if Data.Set.member n set then return (LLVM.LocalReference LLVM.i32 (LLVM.mkName n)) else assignInstr $ LLVM.Load False LLVM.i32 (LLVM.LocalReference LLVM.i32 (LLVM.mkName n)) Nothing 0 [])
gen (Compare e1 e2) = do
  op1 <- gen e1
  op2 <- gen e2
  assignInstr $ LLVM.AST.ICmp LLVM.EQ op1 op2 []
gen (Sum e1 e2) = do
  op1 <- gen e1
  op2 <- gen e2
  assignInstr $ LLVM.AST.Add False False op1 op2 []
gen (Minus e1 e2) = do
  op1 <- gen e1
  op2 <- gen e2
  assignInstr $ LLVM.AST.Sub False False op1 op2 []
gen (Number a) = return $ LLVM.ConstantOperand $ LLVM.Int 32 (toInteger a)
gen (Boolean b) = return $ LLVM.ConstantOperand $ LLVM.Int 1 (toInteger $ fromEnum b)
gen (Assign i e) = do
  op <- gen e
  assign (LLVM.LocalReference LLVM.i32 (LLVM.mkName i)) op
  return $ LLVM.LocalReference LLVM.i32 (LLVM.mkName i)
gen (Let i e) = do
  op <- gen e
  ptr <- alloca i
  assign ptr op
  return op
gen (Func i args exp) = do
  addSig i args
  s <- get
  let blks = codeGenWithSymTable exp (symTable s) (Data.Set.fromList args)
   in define i args blks
  return $ LLVM.ConstantOperand $ LLVM.Int 32 0
gen (CallBlock exprs) = do
  enterBlock
  genBlock exprs
gen (Block exprs) = do
  enterBlock
  genBlock exprs
gen (If cond t e) = do
  c <- gen cond
  uid <- trace "Test" <$> getBlockUid
  (itLabel, otLabel) <- trace "tlabel" <$> createBlock t
  (ieLabel, oeLabel) <- trace "eLabel" <$> createBlock e
  nextB <- show . (+ 1) . bId <$> get
  setTerm (trace ("tlabel" ++ oeLabel) oeLabel) nextB
  setTerm otLabel nextB
  -- flushBlockWithName (trace ("eLabel" ++ eLabel) eLabel) $ LLVM.Br  (LLVM.mkName $ trace ("Next" ++ nextB) nextB)[]
  -- flushBlockWithName (trace ("tLabel" ++ tLabel) tLabel) $ LLVM.Br (LLVM.mkName $ trace ("Next" ++ nextB) nextB) []
  flushBlockWithName (trace ("Flush if" ++ uid) uid) $ LLVM.CondBr c (LLVM.mkName itLabel) (LLVM.mkName ieLabel) []
  enterBlock
  return c
gen (Return e) = do
  o <- gen e
  flushBlock $ LLVM.Ret (Just o) []
  return o
gen (Grammar.Call i exprs) = do
  ops <- mapM gen exprs
  s <- symTable <$> get
  let ty = getFunctionType i s
   in let op = Right (LLVM.ConstantOperand (LLVM.GlobalReference $ LLVM.mkName i))
       in let args = Prelude.map (\s -> (s, [])) ops
           in assignInstr $ LLVM.Call Nothing LLVM.CallingConvention.C [] ty op args [] []
gen (Print e) = do
  arg <- gen e
  let op = Right (LLVM.ConstantOperand (LLVM.GlobalReference $ LLVM.mkName "putchar"))
   in assignInstr $ LLVM.Call Nothing LLVM.CallingConvention.C [] putCharType op [(arg, [])] [] []

setTerm :: String -> String -> CodeGen ()
setTerm blk next = do
  blks <- blocks <$> get
  let (LLVM.BasicBlock n i _) = blks ! blk
   in modify $ \s -> s {blocks = insert blk (LLVM.BasicBlock n i (Do $ LLVM.Br (LLVM.mkName next) [])) (blocks s)}
  return ()

createBlock :: Expr -> CodeGen (String, String)
createBlock (Block exprs) = do
  iuid <- enterBlock
  genBlock exprs
  flushBlock $ LLVM.Ret Nothing []
  ouid <- getBlockUid
  return (iuid, ouid)

genBlock :: [Expr] -> CodeGen LLVM.Operand
genBlock [e] = gen e
genBlock (e : exprs) = do
  gen e
  genBlock exprs

codegen :: Expr -> [BasicBlock]
codegen e = Prelude.map snd $ toList (blocks s)
  where
    (_, s) = runState (gen e) defaultState

codeGenWithSymTable :: Expr -> SymTable -> Set String -> [BasicBlock]
codeGenWithSymTable e symT reg = Prelude.map snd $ toList (blocks s)
  where
    (_, s) = runState (gen e) (defaultState {symTable = symT, registered = reg})

defgen :: Expr -> [LLVM.Definition]
defgen e = defs s
  where
    (_, s) = runState (gen e) defaultState

mainDef :: Expr -> LLVM.Definition
mainDef e =
  LLVM.GlobalDefinition
    functionDefaults
      { name = Name "main",
        parameters = ([], False),
        returnType = VoidType,
        basicBlocks = codegen e
      }

putCharDef :: Definition
putCharDef =
  LLVM.GlobalDefinition
    LLVM.functionDefaults
      { name = LLVM.mkName "putchar",
        parameters = ([LLVM.Parameter LLVM.i32 (Name "fmt") []], False),
        returnType = LLVM.i32,
        linkage = LLVM.External,
        visibility = LLVM.Default,
        basicBlocks = []
      }

putCharType :: Type
putCharType = LLVM.FunctionType LLVM.i32 [LLVM.i32] False

compile :: String -> IO ()
compile file = do
  code <- readFile file
  let m = module_ $ parseAll code
   in ppModule m

module_ :: Expr -> LLVM.AST.Module
module_ e = LLVM.defaultModule {moduleName = "FoxModule", moduleDefinitions = putCharDef : defgen e}

ppModule :: LLVM.AST.Module -> IO ()
ppModule ast = withContext $ \ctx ->
  withModuleFromAST ctx ast $ \m -> do
    llstr <- moduleLLVMAssembly m
    writeFile "samples/fox.lc" $ unpack llstr
