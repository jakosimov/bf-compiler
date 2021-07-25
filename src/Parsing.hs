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

data Atomic =
    FunctionCall Identifier [Expression]
  | IdentifierAtomic Identifier
  | IntegerLiteral Integer -- Add for other types of int
  | FloatLiteral Integer -- Add for other types of float

data Expression =
    AtomicExpression Atomic
  | AssignmentExpression Expression Expression
  | AdditionExpression Expression Expression
  | MultiplicationExpression Expression Expression
  | SubtractionExpression Expression Expression
  | DivisionExpression Expression Expression
  | DereferenceExpression Expression
  | ReferenceExpression Expression
  | PreIncrementExpression Expression
  | PreDecrementExpression Expression
  | PostIncrementExpression Expression
  | PostDecrementExpression Expression
  | GreaterThanExpression Expression Expression
  | LessThanExpression Expression Expression
  | GreaterOrEqualExpression Expression Expression
  | LessOrEqualExpression Expression Expression
  | EqualToExpression Expression Expression
  | NotEqualToExpression Expression Expression
  | AndExpression Expression Expression
  | OrExpression Expression Expression
  | NotExpression Expression

data VariableDeclaration = VariableDeclaration CType Identifier (Maybe Expression)

data Assoc = RightAssociative | LeftAssociative

data Repeatable = Repeatable | NonRepeatable

type BinaryP a = Parser (a -> a -> a)
type UnaryP a = Parser (a -> a)

data Operator a =
    BinaryOperator (BinaryP a) Assoc
  | PrefixOperator (UnaryP a) Repeatable
  | SuffixOperator (UnaryP a) Repeatable

type OperatorTable a = [[Operator a]]
type OperatorLevel a   = [Operator a]

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

data SourceFile = SourceFile FunctionDefinition | GlobalDeclaration VariableDeclaration


instance Show CType where
  show (CType string) = map toUpper string

instance Show Identifier where
  show (Identifier string) = string

instance Show Parameter where
  show (Parameter t i) =
    show t ++ " " ++ show i

instance Show Atomic where
  show atomic = case atomic of
    FunctionCall iden exprs -> show iden ++ "(" ++ intercalate ", " (map show exprs) ++ ")"
    IdentifierAtomic i      -> show i
    IntegerLiteral i        -> show i
    FloatLiteral f          -> show f

instance Show Expression where
  show expr = case expr of
    AtomicExpression atom        -> show atom
    AssignmentExpression l r     -> bin ":=" l r
    AdditionExpression a b       -> bin "+" a b
    SubtractionExpression a b    -> bin "-" a b
    MultiplicationExpression a b -> bin "*" a b
    DivisionExpression a b       -> bin "/" a b
    DereferenceExpression a      -> pre "*" a
    ReferenceExpression a        -> pre "&" a
    PreIncrementExpression a     -> pre "++" a
    PreDecrementExpression a     -> pre "--" a
    PostIncrementExpression a    -> suf "++" a
    PostDecrementExpression a    -> suf "--" a
    GreaterThanExpression a b    -> bin ">" a b
    LessThanExpression a b       -> bin "<" a b
    GreaterOrEqualExpression a b -> bin ">=" a b
    LessOrEqualExpression a b    -> bin ">=" a b
    EqualToExpression a b        -> bin "==" a b
    NotEqualToExpression a b     -> bin "!=" a b
    AndExpression a b            -> bin "&&" a b
    OrExpression a b             -> bin "||" a b
    NotExpression a              -> pre "!" a
    where showInParens s = "(" ++ s ++ ")"
          bin s a b = showInParens $ show a ++ " "  ++ s ++ " " ++ show b
          pre s a = showInParens $ s ++ show a
          suf s a = showInParens $ show a ++ s


instance Show VariableDeclaration where
  show (VariableDeclaration t i expr) =
    show t ++ " " ++ show i ++
    case expr of
      Nothing -> ""
      Just e  -> " := " ++ show e

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
  show (GlobalDeclaration d) = "Global Declaration: MISSING REPRESENTATION"

testParse :: Parser a -> String -> Either ParseError a
testParse parser = parse parser "test"

testExpr :: String -> Either ParseError Expression
testExpr = parse expression "expressionTest"

testText :: String
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


commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (char ',')

inParens :: Parser a -> Parser a
inParens = between (char '(') (char ')')

-- START OF DEFINITIONS OF ATOMICS ----

atomic :: Parser Atomic
atomic = spaced $ choice $ map try [ functionCall
                                   , identifierExpression
                                   , integerLiteral
                                   , floatLiteral
                                   ]

functionCall :: Parser Atomic
functionCall =
  do name <- identifier
     spaces
     arguments <- inParens $ commaSep expression
     return $ FunctionCall name arguments

identifierExpression :: Parser Atomic
identifierExpression = IdentifierAtomic <$> identifier

integerLiteral :: Parser Atomic
integerLiteral =
  do num <- many1 digit
     return . IntegerLiteral $ read num

floatLiteral :: Parser Atomic
floatLiteral =
  do first <- many1 digit
     char '.'
     second <- many1 digit
     let fullString = first ++ "." ++ second
     return . FloatLiteral $ read fullString

-- END OF DEFINITIONS OF ATOMICS ----

-- START OF DEFINITIONS OF EXPRESSIONS ----

parenthesizedExpression = inParens (spaced expression)

atomicExpression :: Parser Expression
atomicExpression = (AtomicExpression <$> atomic) <|> parenthesizedExpression

expression :: Parser Expression
expression = makeOperationsParser operatorTable atomicExpression

groupOperations :: OperatorLevel a -> ([BinaryP a], [BinaryP a], [UnaryP a], [UnaryP a])
groupOperations = foldl helper ([], [], [], [])
  where helper (leftAssoc, rightAssoc, prefixes, suffixes) operator =
          case operator of
            BinaryOperator p LeftAssociative  -> (p:leftAssoc, rightAssoc, prefixes, suffixes)
            BinaryOperator p RightAssociative -> (leftAssoc, p:rightAssoc, prefixes, suffixes)
            PrefixOperator p _                -> (leftAssoc, rightAssoc, p:prefixes, suffixes)
            SuffixOperator p _                -> (leftAssoc, rightAssoc, prefixes, p:suffixes)

makePrefixP :: UnaryP a -> UnaryP a
makePrefixP prefix = prefixP
  where prefixP =
          do result <- prefix
             rest <- prefixP
             return $ result . rest
          <|> return id

makeSuffixP :: UnaryP a -> UnaryP a
makeSuffixP suffix = suffixP
  where suffixP =
          do op <- suffix
             rest <- suffixP
             return $ rest . op
          <|> return id

makeTermP :: Parser a -> UnaryP a -> UnaryP a -> Parser a
makeTermP term prefix suffix =
  do prefixF <- prefixP
     x <- term
     suffixF <- suffixP
     return $ suffixF (prefixF x)
  where prefixP = makePrefixP prefix
        suffixP = makeSuffixP suffix

makeRightAssocP :: BinaryP a -> Parser a -> a -> Parser a
makeRightAssocP rightAssoc termP = rightAssocP
  where rightAssocP x =
          do op <- rightAssoc
             term <- termP
             rest <- rightAssocP term <|> return term
             return $ op x rest

makeLeftAssocP :: BinaryP a -> Parser a -> a -> Parser a
makeLeftAssocP leftAssoc termP = leftAssocP
  where leftAssocP x =
          do op <- leftAssoc
             term <- termP
             let result = op x term
             leftAssocP result <|> return result

makeOperationLevelParser :: Parser a -> OperatorLevel a -> Parser a
makeOperationLevelParser simpleExpr operatorsInLevel =
  do term <- termP
     rightAssocP term <|> leftAssocP term <|> return term

  where (leftAssociatives, rightAssociatives, prefixes, suffixes) = groupOperations operatorsInLevel
        rightAssoc = choice $ map try rightAssociatives
        leftAssoc  = choice $ map try leftAssociatives
        prefix     = choice $ map try prefixes
        suffix     = choice $ map try suffixes

        termP       = makeTermP simpleExpr prefix suffix
        rightAssocP = makeRightAssocP rightAssoc termP
        leftAssocP  = makeLeftAssocP leftAssoc termP

makeOperationsParser :: OperatorTable a -> Parser a -> Parser a
makeOperationsParser operations atomicExpression =
  foldl makeOperationLevelParser atomicExpression operations

newLeftAssoc :: String -> (a -> a -> a) -> Operator a
newLeftAssoc str op = BinaryOperator (spaced (string str) >> return op) LeftAssociative

newRightAssoc :: String -> (a -> a -> a) -> Operator a
newRightAssoc str op = BinaryOperator (spaced (string str) >> return op) RightAssociative

newPrefix :: String -> (a -> a) -> Operator a
newPrefix str op = PrefixOperator (spaced (string str) >> return op) Repeatable

newSuffix :: String -> (a -> a) -> Operator a
newSuffix str op = SuffixOperator (spaced (string str) >> return op) Repeatable

operatorTable :: OperatorTable Expression
operatorTable = [ [ newPrefix "*" DereferenceExpression, newPrefix "&" ReferenceExpression ]
                , [ newPrefix "!" NotExpression ]
                , [ newPrefix "++" PreIncrementExpression, newPrefix "--" PreDecrementExpression ]
                , [ newSuffix "++" PostIncrementExpression, newSuffix "--" PostDecrementExpression ]
                , [ newLeftAssoc "*" MultiplicationExpression, newLeftAssoc "/" DivisionExpression ]
                , [ newLeftAssoc "+" AdditionExpression, newLeftAssoc "-" SubtractionExpression ]
                , [ newRightAssoc ">" GreaterThanExpression, newRightAssoc "<" LessThanExpression
                  , newRightAssoc ">=" GreaterOrEqualExpression, newRightAssoc "<=" LessOrEqualExpression ]
                , [ newRightAssoc "==" EqualToExpression, newRightAssoc "!=" NotEqualToExpression ]
                , [ newLeftAssoc "&&" AndExpression, newLeftAssoc "||" OrExpression ]
                , [ newRightAssoc "=" AssignmentExpression ]
                ]

-- END OF DEFINITIONS OF EXPRESSIONS ----

-- START OF DEFINITIONS OF STATEMENTS ----

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

-- END OF DEFINITIONS OF STATEMENTS ----

-- START OF DEFINITIONS OF FUNCTION DECLARATIONS ----

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

-- END OF DEFINITIONS OF FUNCTION DECLARATIONS ----

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
