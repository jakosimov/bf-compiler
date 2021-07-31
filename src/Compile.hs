{-# LANGUAGE NamedFieldPuns #-}
module Compile where

import Parsing (Identifier, Signed, IntegerType, FloatType)
import qualified Parsing as P
import Data.List (intercalate)

data DataType = IntType Signed IntegerType
              | FloatType FloatType

data FunctionSignature = FunctionSignature Identifier DataType [DataType]
data VariableSignature = VariableSignature Identifier DataType

instance Show DataType where
  show d = undefined

instance Show FunctionSignature where
  show (FunctionSignature i d ps) =
    show d ++ " " ++ show i ++ "("
    ++ intercalate ", " (map show ps) ++ ")"

data Bindings = Bindings { boundSignatures :: [FunctionSignature]
                         , boundVariables :: [VariableSignature]
                         }

data CompilationError = CompilationError String Int -- ??

-- Should all parse structures contain a line number??

compileDataType :: P.DataType -> DataType
compileDataType d =
  undefined

findFunctionSignatures :: P.SourceFile -> [FunctionSignature]
findFunctionSignatures (P.SourceFile source) =
  foldr step [] source
  where step (P.SourceFunctionDeclaration func) xs =
          let rType  = P.returnType func
              name   = P.functionName func
              pTypes = map (\(P.Parameter t _) -> compileDataType t) $ P.parameters func
          in FunctionSignature name (compileDataType rType) pTypes : xs
        step _ xs = xs

stepFunctionStatement :: Bindings -> P.Statement -> (Bindings, a)
stepFunctionStatement bindings statement =
  undefined

compileFunction :: P.FunctionDefinition -> a
compileFunction P.FunctionDefinition { P.returnType, P.functionName, P.parameters } =
  undefined

testFile :: String
testFile = "test.c"

testOn :: Show a => (P.SourceFile -> a) -> IO ()
testOn f =
  do (pre, result) <- P.parseFile testFile
     case result of
       Right r -> print (f r)
       Left e  -> print e
