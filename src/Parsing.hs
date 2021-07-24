{-# LANGUAGE NamedFieldPuns #-}
module Parsing where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Data.Char
import System.IO
import Data.List (intercalate)

newtype CType = CType String

newtype Identifier = Identifier String

data Expression =
    FunctionCall Identifier [Expression]
  | IdentifierExpression Identifier
  | AssignmentExpression VariableAssignment
  | StandardIntegerExpression Integer -- Add for other types of int
  | StandardFloatExpression Integer -- Add for other types of float
  | AdditionExpression Expression Expression
  | MultiplicationExpression Expression Expression
  | SubtractionExpression Expression Expression
  | DivisionExpression Expression Expression

data VariableDeclaration = VariableDeclaration CType Identifier (Maybe Expression)

data VariableAssignment = VariableAssignment Identifier Expression

data ForInitializer =
    DeclarationInitializer VariableDeclaration
  | ExpressionInitializer Expression
  | EmptyInitializer

data Statement =
    DeclarationStatement VariableDeclaration
  | ExpressionStatement Expression
  | ReturnStatement (Maybe Expression)
  | IfStatement Expression Body
  | ElseIfStatement Expression Body
  | ElseStatement Body
  | WhileStatement Expression Body
  | ForStatement ForInitializer Expression Expression Body
  | BreakStatement

type Body = [Statement]

data Parameter = Parameter CType Identifier

data FunctionDefinition =
  FunctionDefinition { returnType :: CType
                     , functionName :: Identifier
                     , parameters :: [Parameter]
                     , functionBody :: Body
                     }

data SourceFile = SourceFile FunctionDefinition

instance Show CType where
  show (CType string) = map toUpper string

instance Show Identifier where
  show (Identifier string) = string

instance Show Parameter where
  show (Parameter t i) =
    show t ++ " " ++ show i

instance Show Expression where
  show expr = case expr of
    IdentifierExpression i        -> show i
    AssignmentExpression a        -> show a
    StandardIntegerExpression i   -> show i
    StandardFloatExpression f     -> show f
    FunctionCall i ps             -> "FUNCTIONCALL"
    AdditionExpression e e'       -> show e ++ " + " ++ show e'
    SubtractionExpression e e'    -> show e ++ " - " ++ show e'
    MultiplicationExpression e e' -> show e ++ " * " ++ show e'
    DivisionExpression e e'       -> show e ++ " / " ++ show e'

instance Show VariableDeclaration where
  show (VariableDeclaration t i expr) =
    show t ++ " " ++ show i ++
    case expr of
      Nothing -> ""
      Just e  -> " := " ++ show e

instance Show VariableAssignment where
  show (VariableAssignment i expr) =
   show i ++ " := " ++ show expr

showBody :: Body -> String
showBody body = intercalate "\n" (map show body)

instance Show ForInitializer where
  show i = case i of
    DeclarationInitializer v -> show v
    ExpressionInitializer e  -> show e
    EmptyInitializer         -> "--"

instance Show Statement where
  show s = case s of
    DeclarationStatement d    -> "_DCLR_ " ++ show d ++ ";;"
    ExpressionStatement e     -> "_EXPR_ " ++ show e ++ ";;"
    ReturnStatement (Just e)  -> "RETURN " ++ show e ++ ";;"
    ReturnStatement Nothing   -> "RETURN ;;"
    IfStatement e b           -> "IF (" ++ show e ++ ")\n" ++ showBody b ++ "\nEND IF"
    ElseIfStatement e b       -> "ELSEIF (" ++ show e ++ ")\n" ++ showBody b ++ "\nEND ELSE IF"
    ElseStatement b           -> "ELSE\n" ++ showBody b ++ "\nEND ELSE"
    WhileStatement e b        -> "WHILE (" ++ show e ++ ")\n" ++ showBody b ++ "\nEND WHILE"
    ForStatement init c inc b -> "FOR (" ++ show init ++ ";" ++ show c ++ ";"  ++ show inc ++ ")\n" ++ showBody b ++ "\nEND FOR"
    BreakStatement            -> "BREAK ;;"


instance Show FunctionDefinition where
  show FunctionDefinition { returnType, functionName, parameters, functionBody } =
    "\nFUNCTION DECLARATION \n" ++
    "Return type: " ++ show returnType ++ "\n" ++
    "Name: " ++ show functionName ++ "\n" ++
    "Parameters: " ++ intercalate ", " (map show parameters) ++ "\n" ++
    "Body: \n" ++ showBody functionBody

instance Show SourceFile where
  show (SourceFile d) = show d

testParse :: Parser a -> String -> Either ParseError a
testParse parser = parse parser "test"

testText = "void hello(int a, char b) { int a = 2; int b; b = 3; }"

spaced :: Parser a -> Parser a
spaced = between spaces spaces

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
                                , additionExpression
                                , subtractionExpression
                                , multiplicationExpression
                                , divisionExpression
                                , parenthesisExpression
                                , functionCall
                                , standardIntegerExpression
                                , standardFloatExpression
                                , identifierExpression
                                ]
     spaces
     return result

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (char ',')

inParens :: Parser a -> Parser a
inParens = between (char '(') (char ')')

functionCall :: Parser Expression
functionCall =
  do name <- identifier
     spaces
     arguments <- inParens $ commaSep expression
     return $ FunctionCall name arguments

identifierExpression :: Parser Expression
identifierExpression = IdentifierExpression <$> identifier

assignmentExpression :: Parser Expression
assignmentExpression = AssignmentExpression <$> variableAssignment

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

parenthesisExpression :: Parser Expression
parenthesisExpression = inParens expression

operatorExpression :: Char -> (Expression -> Expression -> Expression) -> Parser Expression
operatorExpression operator constr  =
  do a <- expression
     spaces
     char operator
     spaces
     constr a <$> expression

additionExpression :: Parser Expression
additionExpression = operatorExpression '+' AdditionExpression

multiplicationExpression :: Parser Expression
multiplicationExpression = operatorExpression '-' SubtractionExpression

subtractionExpression :: Parser Expression
subtractionExpression = operatorExpression '*' MultiplicationExpression

divisionExpression :: Parser Expression
divisionExpression = operatorExpression '/' DivisionExpression

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
     VariableAssignment name <$> assignmentRightHand

statement :: Parser Statement
statement =
  do spaces
     result <- choice $ map try [ declarationStatement
                                , expressionStatement
                                , returnStatement
                                , ifStatement
                                , elseIfStatement
                                , elseStatement
                                , whileStatement
                                , forStatement
                                , breakStatement
                                ]
     spaces
     return result

semicolon :: Parser Char
semicolon = char ';'

declarationStatement :: Parser Statement
declarationStatement =
  do result <- variableDeclaration
     spaces
     semicolon
     return $ DeclarationStatement result

expressionStatement :: Parser Statement
expressionStatement =
  do result <- expression
     spaces
     semicolon
     return $ ExpressionStatement result

returnStatement :: Parser Statement
returnStatement =
  do string "return"
     spaces
     result <- optionMaybe expression
     spaces
     semicolon
     return $ ReturnStatement result

condition :: Parser Expression
condition = between spaces spaces $ inParens expression

ifStatement :: Parser Statement
ifStatement =
  do string "if"
     expr <- condition
     statements <- body
     spaces
     return $ IfStatement expr statements


elseIfStatement :: Parser Statement
elseIfStatement =
  do string "else if"
     expr <- condition
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
     expr <- condition
     statements <- body
     spaces
     return $ WhileStatement expr statements

forInitializer :: Parser ForInitializer
forInitializer =
  choice $ map try [ declarationInitializer
                   , expressionInitializer
                   , emptyInitializer
                   ]
  where declarationInitializer = DeclarationInitializer <$> variableDeclaration
        expressionInitializer = ExpressionInitializer <$> expression
        emptyInitializer      = EmptyInitializer <$ spaces

assocIndex :: Eq a => [[(a, b)]] -> a -> Maybe Int
assocIndex [] _ = Nothing
assocIndex (xs:xss) x'
  | any ((==x') . fst) xs = Just 0
  | otherwise             = case assocIndex xss x' of
      Just n  -> Just $ n + 1
      Nothing -> Nothing

-- data Atom = AtomicIdentifier String |

leftAssociativeOperators :: [[(Char, Expression -> Expression -> Expression)]] -> Parser Expression
leftAssociativeOperators operations = undefined
  where atomicExpression = identifier
        operators        = map fst $ concat operations


forStatement :: Parser Statement
forStatement =
  do string "for"
     spaces
     char '('
     init <- spaced forInitializer
     semicolon
     condition <- spaced expression
     semicolon
     incrementation <- spaced expression
     char ')'
     statements <- spaced body
     return $ ForStatement init condition incrementation statements

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
  do inParens $ commaSep functionParameter

body :: Parser Body
body = between (char '{') (char '}') $ spaced $ many statement

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

sourceFile :: Parser SourceFile
sourceFile = fmap SourceFile (between spaces spaces functionDefinition)

testFile :: String
testFile = "test.c"

doPreprocessing :: String -> String
doPreprocessing string =
  unlines $ map (removeComments . removePreprocessors) ls
  where ls = lines string
        removePreprocessors = takeWhile (/='#')
        removeComments = third . foldl helper (False, ' ', [])
        third (_, _, x) = x
        helper (True, _, xs) c       = (True, c, xs)
        helper (False, '/', xs) '/'  = (True, '/', xs)
        helper (False, _, xs) x      = (False, x, xs ++ [x])

testOn :: IO ()
testOn =
  do handle <- openFile testFile ReadMode
     contents <- hGetContents handle
     let preprocessed = doPreprocessing contents
     putStrLn preprocessed
     putStrLn "\nResult: \n"
     print $ parse sourceFile testFile preprocessed
     hClose handle
