{-# LANGUAGE LambdaCase #-}
module Test.Execute where

import Test.Tasty.HUnit (assertEqual)
import Statement (Expression(VariableName, Const), Statement (Skip, Write, Read))
import Execute (execute)
import Context (Context(..), empty, setVar, setError)
import Error (RuntimeError(VarNameError, UnsupportedError, InvalidInputError))
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef, modifyIORef)
import qualified GHC.Err as Err

initTestContext :: [String] -> IO (Context, IO [String])
initTestContext input = do
    inputsRef <- newIORef input
    outputsRef <- newIORef []
    let getOutput :: IO [String]
        getOutput = readIORef outputsRef

    let getTestLine :: IO String
        getTestLine = atomicModifyIORef inputsRef (\case
            i : is -> (is,i) -- the i becomes the return value
            [] -> Err.error "fake inputs exhausted")
    let putTestLine :: String -> IO ()
        putTestLine str = atomicModifyIORef outputsRef (\inputs -> (inputs ++ [str], ()))

    pure (empty {getNextLine = getTestLine, putLine = putTestLine }, getOutput)

unit_executeWrite :: IO ()
unit_executeWrite = do
    let writeConst = Write (Const 1)
    let writeVar = Write (VariableName "var")

    (testContext, getOutput) <- initTestContext []
    exitContext <- execute testContext [writeConst]
    output      <- getOutput
    assertEqual "write const" testContext exitContext
    assertEqual "write const" ["1"] output

    (testContext, getOutput) <- initTestContext []
    exitContext <- execute testContext [writeVar]
    output      <- getOutput
    context     <- setError testContext (VarNameError "var")
    assertEqual "write var fail" context exitContext
    assertEqual "write var fail" [] output

    (testContext0, getOutput) <- initTestContext ["123"]
    testContext <- setVar testContext0 "var" 123
    exitContext <- execute testContext [writeVar]
    output      <- getOutput
    assertEqual "write var success" testContext exitContext
    assertEqual "write var success" ["123"] output

unit_executeUnsupported :: IO ()
unit_executeUnsupported = do
    let skip = Skip

    exitContext <- execute empty [skip]
    context     <- setError empty UnsupportedError
    assertEqual "unsupported" context exitContext

unit_executeRead :: IO ()
unit_executeRead = do
    let readVar = Read "var"

    (testContext, getOutput) <- initTestContext ["123"]
    exitContext <- execute testContext [readVar]
    output      <- getOutput
    context     <- setVar testContext "var" 123
    assertEqual "read success" context exitContext
    assertEqual "read success" [] output

    (testContext, getOutput) <- initTestContext ["fds"]
    exitContext <- execute testContext [readVar]
    output      <- getOutput
    context     <- setError testContext (InvalidInputError "fds")
    assertEqual "read failure" context exitContext
    assertEqual "read failure" [] output
