{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified LLVM.AST.Constant as LLVM (
  Constant(..))

import LLVM.AST (
  Module(Module), Definition(GlobalDefinition), BasicBlock(BasicBlock),
  functionDefaults,
  Named(Do), Terminator(Ret), Operand(ConstantOperand))
import LLVM.AST.Global (
  Global(name, returnType, basicBlocks))
import LLVM.AST.Type (
  i1)
import LLVM.AST.IntegerPredicate (IntegerPredicate(EQ))

import LLVM.Module (
  withModuleFromAST, writeObjectToFile, File(File))
import LLVM.Context (
  withContext)
import LLVM.Target (
  withHostTargetMachineDefault)

-- | A language of boolean expressions with xor.
data Exp =
  Lit Bool |
  And Exp Exp |
  Or Exp Exp |
  Not Exp |
  Xor Exp Exp

-- | Generate an LLVM constant from a boolean expression.
codegen :: Exp -> LLVM.Constant
codegen (Lit False) = LLVM.Int 1 0  -- i1 false (0)
codegen (Lit True) = LLVM.Int 1 1   -- i1 true (1)
codegen (And lhs rhs) =
  LLVM.And (codegen' lhs) (codegen' rhs)  -- AND
codegen (Or lhs rhs) =
  LLVM.Or (codegen' lhs) (codegen' rhs)   -- OR
codegen (Xor lhs rhs) =
  LLVM.Xor (codegen' lhs) (codegen' rhs)  -- XOR
codegen (Not e) =
  LLVM.Xor (codegen' e) (LLVM.Int 1 1)    -- NOT (XOR with 1)

-- | Helper function to wrap LLVM.Constant as Operand.
codegen' :: Exp -> LLVM.Constant
codegen' = codegen

-- | An example boolean expression.
myExpression :: Exp
myExpression = Xor (Lit True) (And (Lit False) (Lit True))

-- | The result of generating code from 'myExpression'.
myConstant :: LLVM.Constant
myConstant = codegen myExpression

-- | An LLVM constant operand.
myOperand :: Operand
myOperand = ConstantOperand myConstant

-- | An LLVM terminator that returns a constant operand.
myTerminator :: Terminator
myTerminator = Ret (Just myOperand) []

-- | An LLVM basic block with name \"myEntrypoint\" and no intructions.
myBasicBlock :: BasicBlock
myBasicBlock = BasicBlock "myEntrypoint" [] (Do myTerminator)

-- | An LLVM definition of a single function \"myMain\".
myDefinition :: Definition
myDefinition = GlobalDefinition (functionDefaults {
  name = "myMain",
  returnType = i1,
  basicBlocks = [myBasicBlock]})

-- | An LLVM module with name \"myModule\" and a single definition.
myModule :: Module
myModule = Module "myModule" "myModule.be" Nothing Nothing [myDefinition]

-- | Write out an object file \"myModule.o\" corresponding to the constant
-- LLVM module 'myModule'.
main :: IO ()
main = do
  withContext (\context ->
    withModuleFromAST context myModule (\modulePtr ->
      withHostTargetMachineDefault (\targetMachine ->
        writeObjectToFile targetMachine (File "myModule.o") modulePtr)))
