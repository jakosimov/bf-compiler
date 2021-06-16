module Parsing where
import qualified Data.Map as Map

data Parser a = Parser (String -> Either String (a, String))

run :: Parser a -> String -> Either String (a, String)
run (Parser f) str = f str

(|>>) parser f = Parser $ \str ->
    case run parser str of
      Left error -> Left error
      Right (x, rest) -> Right (f x, rest)

instance Functor Parser where
  fmap f parser = parser |>> f

parseChar :: Char -> Parser Char
parseChar c = Parser $ \str ->
  case str of
    [] -> Left "End of input"
    (x:xs) ->
      if x == c then
        Right (c, xs)
      else let
        errorMessage = "Expected '" ++ [c] ++ "', found " ++ [x]
      in Left errorMessage

andThen :: Parser a -> Parser b -> Parser (a,b) 
p1 `andThen` p2 = Parser $ \str ->
  case run p1 str of
    Left error1 -> Left error1
    Right (result1, rest1) ->
      case run p2 rest1 of
        Left error2 -> Left error2
        Right (result2, rest2) -> Right ((result1, result2), rest2)

(.>>.) :: Parser a -> Parser b -> Parser (a,b)
(.>>.) = andThen

(.>>) :: Parser a -> Parser b -> Parser a
p1 .>> p2 = (p1 .>>. p2) |>> (\(a, b) -> a)

(>>.) :: Parser a -> Parser b -> Parser b
p1 >>. p2 = (p1 .>>. p2) |>> (\(a, b) -> b)

eitherOr :: Parser a -> Parser a -> Parser a
p1 `eitherOr` p2 = Parser $ \str ->
  case run p1 str of
    Left error ->
      run p2 str
    result -> result

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = eitherOr

alwaysSuccess :: a -> Parser a
alwaysSuccess x = Parser (\str -> Right (x, str))

alwaysFailure :: String -> Parser a
alwaysFailure error = Parser (\_ -> Left error)

combineResults :: Parser [b] -> Parser [b] -> Parser [b]
combineResults p1 p2 = (p1 `andThen` p2) |>> (\(a, b) -> a ++ b)

seqParse :: [Parser a] -> Parser [a]
seqParse ps = foldr combineResults (alwaysSuccess []) ps'
  where ps' = map (|>> (:[])) ps 

choice :: [Parser a] -> Parser a
choice ps = foldl (<|>) (alwaysFailure errorMsg) ps 
  where errorMsg = "Empty choice"

anyOf :: [Char] -> Parser Char
anyOf cs = choice (map parseChar cs)

many :: Parser a -> Parser [a]
many p = combineResults p' (many p) <|> alwaysSuccess []
  where p' = p |>> (:[])

many1 :: Parser a -> Parser [a]
many1 p = combineResults p' (many p)
  where p' = p |>> (:[])

opt :: Parser a -> Parser (Maybe a)
opt p = p' <|> alwaysSuccess Nothing
  where p' = p |>> Just

between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = a >>. b .>> c

sepBy1 :: Parser a -> Parser b -> Parser ([a],[b])
a `sepBy1` seperator =
  a
  .>>. (many (seperator .>>. a))
  |>> (\(first, rest) -> (first:map snd rest, map fst rest))

sepBy :: Parser a -> Parser b -> Parser ([a],[b])
a `sepBy` seperator = (a `sepBy1` seperator) <|> alwaysSuccess ([], [])

lowercase :: Parser Char
lowercase = anyOf ['a'..'z']

uppercase :: Parser Char
uppercase = anyOf ['A'..'Z']

digit :: Parser Char
digit = anyOf ['0'..'9']

whitespaceChar :: Parser Char
whitespaceChar = anyOf [' ', '\t', '\n']

whitespace :: Parser [Char]
whitespace = many1 whitespaceChar

digits :: Parser [Char]
digits = many1 digit

doubleQuote :: Parser Char
doubleQuote = parseChar '"'
 
parseInt :: Parser Integer
parseInt = digits |>> read

parseString :: String -> Parser String
parseString str = seqParse (map parseChar str) 

stringLiteralCharacter :: Parser String
stringLiteralCharacter = choice $ charParsers ++ escCharParsers
  where chars             = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
        charParsers       = map parseString . map (:[]) $ chars
        escChars       = ['"', '\\', 'n'] -- ...
        escCharParsers = map parseString . map (\char -> "\\" ++ [char]) $ escChars
        
  
-- stringLiteral :: Parser String
-- stringLiteral = 
--   between doubleQuote (many stringLiteralCharacter) doubleQuote

  


