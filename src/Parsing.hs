{-# LANGUAGE NamedFieldPuns #-}
module Parsing where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Data.Char
import System.IO
import Data.List (intercalate)

data DataType = DataType String
              | PointerType DataType

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

data VariableDeclaration = VariableDeclaration DataType Identifier (Maybe Expression)

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
    DeclarationInitializer [VariableDeclaration]
  | ExpressionInitializer Expression
  | EmptyInitializer

data ForClause =
    ExpressionClause Expression
  | EmptyClause

data Statement =
    DeclarationStatement [VariableDeclaration]
  | ExpressionStatement Expression
  | ReturnStatement (Maybe Expression)
  | IfStatement Expression Body
  | ElseIfStatement Expression Body
  | ElseStatement Body
  | WhileStatement Expression Body
  | ForStatement ForInitializer ForClause ForClause Body
  | BreakStatement

type Body = [Statement]

data Parameter = Parameter DataType Identifier

data FunctionDefinition =
  FunctionDefinition { returnType :: DataType
                     , functionName :: Identifier
                     , parameters :: [Parameter]
                     , functionBody :: Body
                     }

data SourceFile = SourceFile FunctionDefinition | GlobalDeclaration VariableDeclaration


instance Show DataType where
  show (DataType string) = map toUpper string
  show (PointerType t) = show t ++ "*"

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

instance Show ForClause where
  show c = case c of
    ExpressionClause e -> show e
    EmptyClause        -> "--"

instance Show Statement where
  show s = case s of
    DeclarationStatement ds   -> intercalate ", " (map show ds) ++ ";;"
    ExpressionStatement e     -> show e ++ ";;"
    ReturnStatement (Just e)  -> "RETURN " ++ show e ++ ";;"
    ReturnStatement Nothing   -> "RETURN ;;"
    IfStatement e b           -> "IF (" ++ show e ++ ")\n" ++ showBody b ++ "\nEND IF"
    ElseIfStatement e b       -> "ELSEIF (" ++ show e ++ ")\n" ++ showBody b ++ "\nEND ELSE IF"
    ElseStatement b           -> "ELSE\n" ++ showBody b ++ "\nEND ELSE"
    WhileStatement e b        -> "WHILE (" ++ show e ++ ")\n" ++ showBody b ++ "\nEND WHILE"
    ForStatement init c inc b -> "FOR (" ++ show init ++ " ; " ++ show c ++ " ; "  ++ show inc ++ ")\n" ++ showBody b ++ "\nEND FOR"
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

testP :: Parser a -> String -> Either ParseError a
testP parser = parse parser "test"

testExpr :: String -> Either ParseError Expression
testExpr = parse expression "expressionTest"

testStatement :: String -> Either ParseError Statement
testStatement = parse statement "statementTest"

testText :: String
testText = "void hello(int a, char b) { int a = 2; int b; b = 3; }"

spaced :: Parser a -> Parser a
spaced = between (try spaces) (try spaces)

identifierStartLetter :: Parser Char
identifierStartLetter = letter <|> char '_'

indentifierCharacter :: Parser Char
indentifierCharacter = identifierStartLetter <|> digit

legalIdentifier :: Parser String
legalIdentifier =
  do first <- try identifierStartLetter
     rest <- many indentifierCharacter
     return (first:rest)

identifier :: Parser Identifier
identifier = fmap Identifier legalIdentifier <?> "identifier"

dataType :: Parser DataType
dataType = (DataType <$> legalIdentifier) <?> "data type"
  -- do identifier <- legalIdentifier
  --    pointerSymbols <- many (spaced (char '*'))
  --    let wrapInPointer dataType _ = PointerType dataType
  --        startType                = DataType identifier
  --    return $ foldl wrapInPointer startType pointerSymbols
  -- <?> "data type"

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (char ',')

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p (char ',')

inParens :: Parser a -> Parser a
inParens = between (char '(') (char ')')

-- START OF DEFINITIONS OF ATOMICS ----

atomic :: Parser Atomic
atomic = spaced $ choice [ functionCall
                         , identifierExpression
                         , integerLiteral
                         , floatLiteral
                         ]

functionCall :: Parser Atomic
functionCall =
  do name <- try $ do name <- identifier
                      spaces
                      char '('
                      return name
     arguments <- commaSep expression
     char ')'
     return $ FunctionCall name arguments

identifierExpression :: Parser Atomic
identifierExpression = IdentifierAtomic <$> identifier

integerLiteral :: Parser Atomic
integerLiteral =
  do num <- try $ many1 digit
     return . IntegerLiteral $ read num
  <?> "integer literal"

floatLiteral :: Parser Atomic
floatLiteral =
  do (first, second) <- try $ do first <- many1 digit
                                 char '.'
                                 second <- many1 digit
                                 return (first, second)
     let fullString = first ++ "." ++ second
     return . FloatLiteral $ read fullString
  <?> "float literal"

-- END OF DEFINITIONS OF ATOMICS ----

-- START OF DEFINITIONS OF EXPRESSIONS ----

parenthesizedExpression = inParens (spaced expression)

atomicExpression :: Parser Expression
atomicExpression = (AtomicExpression <$> atomic) <|> parenthesizedExpression

expression :: Parser Expression
expression = makeOperationsParser operatorTable atomicExpression <?> "expression"

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
        prefix     = choice (map try prefixes) <?> "prefix operator"
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
statement = statementP  <?> "statement"
  where statementP = spaced $ choice [ returnStatement
                                     , ifStatement
                                     , elseIfStatement
                                     , elseStatement
                                     , whileStatement
                                     , forStatement
                                     , breakStatement
                                     , declarationStatement
                                     , expressionStatement
                                     ]

semicolon :: Parser Char
semicolon = char ';'

assignmentRightHand :: Parser Expression
assignmentRightHand =
  do try $ spaced $ char '='
     result <- expression
     spaces
     return result

wrapTypeInPointer :: DataType -> Int -> DataType
wrapTypeInPointer d 0 = d
wrapTypeInPointer d n = PointerType $ wrapTypeInPointer d (n-1)

pointerAsterisks :: Parser [Char]
pointerAsterisks = many asterix
  where asterix = try $ do spaces
                           c <- char '*'
                           spaces
                           return '*'

declarationVariable :: DataType -> Parser VariableDeclaration
declarationVariable dataType =
  do (asterisks, name) <- try $ do spaces
                                   asterisks <- pointerAsterisks
                                   spaces
                                   name <- identifier
                                   return (asterisks, name)
     let fullType = wrapTypeInPointer dataType (length asterisks)
     value <- optionMaybe assignmentRightHand
     return $ VariableDeclaration fullType name value

variableDeclaration :: Parser [VariableDeclaration]
variableDeclaration =
  do varType <- dataType
     spaces
     commaSep1 (declarationVariable varType)
  <?> "variable declaration"


declarationStatement :: Parser Statement
declarationStatement =
  do result <- try variableDeclaration
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
  do try $ string "return"
     spaces
     result <- optionMaybe expression
     spaces
     semicolon
     return $ ReturnStatement result

condition :: Parser Expression
condition = between spaces spaces $ inParens expression

ifStatement :: Parser Statement
ifStatement =
  do try $ string "if"
     expr <- condition
     statements <- body
     spaces
     return $ IfStatement expr statements

elseIfStatement :: Parser Statement
elseIfStatement =
  do try $ string "else if"
     expr <- condition
     statements <- body
     spaces
     return $ ElseIfStatement expr statements

elseStatement :: Parser Statement
elseStatement =
  do try $ string "else"
     spaces
     statements <- body
     spaces
     return $ ElseStatement statements

whileStatement :: Parser Statement
whileStatement =
  do try $ string "while"
     expr <- condition
     statements <- body
     spaces
     return $ WhileStatement expr statements

forInitializer :: Parser ForInitializer
forInitializer =
  choice [ declarationInitializer
         , expressionInitializer
         ]
  <|> return EmptyInitializer
  where declarationInitializer = DeclarationInitializer <$> variableDeclaration
        expressionInitializer = ExpressionInitializer <$> expression

forClause :: Parser ForClause
forClause = expressionClause <|> return EmptyClause <?> "for clause"
  where expressionClause = ExpressionClause <$> expression

forStatement :: Parser Statement
forStatement =
  do try $ string "for"
     spaces
     char '('
     init <- spaced forInitializer
     semicolon
     condition <- spaced forClause
     semicolon
     incrementation <- spaced forClause
     char ')'
     statements <- spaced body
     return $ ForStatement init condition incrementation statements

breakStatement :: Parser Statement
breakStatement = BreakStatement <$ try (string "break;")

-- END OF DEFINITIONS OF STATEMENTS ----

-- START OF DEFINITIONS OF FUNCTION DECLARATIONS ----

fullType :: Parser DataType
fullType =
  do baseType <- dataType
     wrapTypeInPointer baseType . length <$> pointerAsterisks

functionParameter :: Parser Parameter
functionParameter =
  do spaces
     valueType <- fullType
     spaces
     name <- identifier
     spaces
     return $ Parameter valueType name

functionParameters :: Parser [Parameter]
functionParameters =
  do inParens $ commaSep functionParameter

body :: Parser Body
body = between (char '{') (char '}') $ many statement

functionDefinition :: Parser FunctionDefinition
functionDefinition =
  do returnType <- fullType
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
