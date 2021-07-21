{-# LANGUAGE NamedFieldPuns #-}
module Parsing where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Data.Char
import Data.List (intersperse)

newtype CType = CType String

newtype Identifier = Identifier String

data Expression =
    FunctionCall Identifier [Expression]
  | IdentifierExpression Identifier
  | AssignmentExpression VariableAssignment
  | StandardIntegerExpression Integer
  | StandardFloatExpression Integer

data VariableDeclaration = VariableDeclaration CType Identifier (Maybe Expression)

data VariableAssignment = VariableAssignment Identifier Expression

data ForInitializer =
    DeclarationInitializer VariableDeclaration
  | VariableInitializer VariableAssignment
  | EmptyInitializer

data Statement =
    DeclarationStatement VariableDeclaration
  | ExpressionStatement Expression
  | ReturnStatement (Maybe Expression)
  | IfStatement Expression Body
  | ElseIfStatement Expression Body
  | ElseStatement Body
  | WhileStatement Expression Body
  | ForStatement ForInitializer Expression Expression Body -- Not correct first argument
  | BreakStatement

type Body = [Statement]

data Parameter = Parameter CType Identifier

data FunctionDefinition =
  FunctionDefinition { returnType :: CType
                     , functionName :: Identifier
                     , parameters :: [Parameter]
                     , functionBody :: Body
                     }

instance Show CType where
  show (CType string) = map toUpper string

instance Show Identifier where
  show (Identifier string) = string

instance Show Parameter where
  show (Parameter t i) =
    show t ++ " " ++ show i

instance Show Expression where
  show expr = case expr of
    IdentifierExpression i -> show i
    AssignmentExpression a -> show a
    StandardIntegerExpression i -> show i
    StandardFloatExpression f -> show f
    FunctionCall i ps -> "FUNCTIONCALL"

instance Show VariableDeclaration where
  show (VariableDeclaration t i expr) =
    show t ++ " " ++ show i ++
    case expr of
      Nothing -> ""
      Just e  -> " := " ++ show e

instance Show VariableAssignment where
  show (VariableAssignment i expr) =
   show i ++ " := " ++ show expr

instance Show Statement where
  show s = case s of
    DeclarationStatement d -> "_DCLR_ " ++ show d ++ ";;"
    ExpressionStatement e -> "_EXPR_ " ++ show e ++ ";;"
    _ -> "???????? :-/"

instance Show FunctionDefinition where
  show FunctionDefinition { returnType, functionName, parameters, functionBody } =
    "FUNCTION DECLARATION \n" ++
    "Return type: " ++ show returnType ++ "\n" ++
    "Name: " ++ show functionName ++ "\n" ++
    "Parameters: " ++ concat (intersperse ", " (map show parameters)) ++ "\n" ++
    "Body: \n" ++ concat (intersperse "\n" (map show functionBody))

testParse :: Parser a -> String -> Either ParseError a
testParse parser = parse parser "test"

testText = "void hello(int a, char b) { int a = 2; int b; b = 3; }"

indentifierCharacter :: Parser Char
indentifierCharacter = letter <|> digit

legalIdentifier :: Parser String
legalIdentifier =
  do first <- letter
     rest <- many indentifierCharacter
     return (first:rest)

identifier :: Parser Identifier
identifier = fmap Identifier legalIdentifier

cType :: Parser CType
cType = fmap CType legalIdentifier

expression :: Parser Expression
expression =
  do spaces
     result <- choice $ map try [ assignmentExpression
                                , functionCall
                                , standardIntegerExpression
                                , standardFloatExpression
                                , identifierExpression
                                ]
     spaces
     return result

functionCall :: Parser Expression
functionCall =
  do name <- identifier
     spaces
     char '('
     arguments <- sepBy expression (char ',')
     char ')'
     return $ FunctionCall name arguments

identifierExpression :: Parser Expression
identifierExpression = fmap IdentifierExpression identifier

assignmentExpression :: Parser Expression
assignmentExpression = fmap AssignmentExpression variableAssignment

standardIntegerExpression :: Parser Expression
standardIntegerExpression =
  do negativeSign <- optionMaybe (char '-')
     spaces
     integer <- many1 digit
     let result = read integer *
           case negativeSign of
             Just _   -> -1
             Nothing  -> 1
     return $ StandardIntegerExpression result

standardFloatExpression :: Parser Expression
standardFloatExpression =
  do negativeSign <- optionMaybe (char '-')
     spaces
     first <- many1 digit
     char '.'
     second <- many1 digit
     let value = read (first ++ '.':second)
         result = value *
           case negativeSign of
             Just _   -> -1
             Nothing  -> 1
     return $ StandardFloatExpression result

assignmentRightHand :: Parser Expression
assignmentRightHand =
  do spaces
     char '='
     spaces
     result <- expression
     spaces
     return result

variableDeclaration :: Parser VariableDeclaration
variableDeclaration =
  do spaces
     varType <- cType
     spaces
     name <- identifier
     value <- optionMaybe assignmentRightHand
     return $ VariableDeclaration varType name value

variableAssignment :: Parser VariableAssignment
variableAssignment =
  do spaces
     name <- identifier
     value <- assignmentRightHand
     return $ VariableAssignment name value

statement :: Parser Statement
statement =
  do spaces
     result <- choice $ map try [ declarationStatement
                                , expressionStatement
                                -- , keywordStatement
                                ]
     spaces
     return result

declarationStatement :: Parser Statement
declarationStatement =
  do result <- variableDeclaration
     spaces
     char ';'
     return $ DeclarationStatement result

expressionStatement :: Parser Statement
expressionStatement =
  do result <- expression
     spaces
     char ';'
     return $ ExpressionStatement result

returnStatement :: Parser Statement
returnStatement =
  do string "return"
     spaces
     result <- optionMaybe expression
     spaces
     char ';'
     return $ ReturnStatement result

ifStatement :: Parser Statement
ifStatement =
  do string "if"
     spaces
     char '('
     expr <- expression
     char ')'
     spaces
     statements <- body
     spaces
     return $ IfStatement expr statements


elseIfStatement :: Parser Statement
elseIfStatement =
  do string "else if"
     spaces
     char '('
     expr <- expression
     char ')'
     spaces
     statements <- body
     spaces
     return $ ElseIfStatement expr statements

elseStatement :: Parser Statement
elseStatement =
  do string "else"
     spaces
     statements <- body
     spaces
     return $ ElseStatement statements

whileStatement :: Parser Statement
whileStatement =
  do string "while"
     spaces
     char '('
     expr <- expression
     char ')'
     spaces
     statements <- body
     spaces
     return $ WhileStatement expr statements

forStatement :: Parser Statement
forStatement = undefined

breakStatement :: Parser Statement
breakStatement = fmap (const BreakStatement) (between spaces spaces $ string "break;")

functionParameter :: Parser Parameter
functionParameter =
  do spaces
     valueType <- cType
     spaces
     name <- identifier
     spaces
     return $ Parameter valueType name

functionParameters :: Parser [Parameter]
functionParameters =
  do char '('
     parameters <- sepBy functionParameter (char ',')
     char ')'
     return parameters

body :: Parser Body
body =
  do char '{'
     spaces
     result <- many statement
     spaces
     char '}'
     return result

functionDefinition :: Parser FunctionDefinition
functionDefinition =
  do returnType <- cType
     spaces
     functionName <- identifier
     spaces
     parameters <- functionParameters
     spaces
     functionBody <- body
     return FunctionDefinition { returnType
                               , functionName
                               , parameters
                               , functionBody
                               }
