module Scheme.Parser
  ( Parser,
    ParseError (..),
    SourcePos (..),
    char,
    eof,
    parseExpr,
    parseProgram,
    renderParseError,
    runParser,
    satisfy,
    string,
    tryP,
  )
where

import Control.Applicative (Alternative (..), many, optional, some)
import Control.Monad (MonadPlus (..))
import Data.Char (isDigit, isLetter, isSpace)
import Data.List (intercalate, nub, sort)
import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Scheme.Syntax (SExpr (..))

data SourcePos = SourcePos
  { sourceOffset :: Int,
    sourceLine :: Int,
    sourceColumn :: Int
  }
  deriving (Eq, Show)

data ParseError = ParseError
  { parseErrorPos :: SourcePos,
    parseErrorUnexpected :: Maybe String,
    parseErrorExpected :: [String],
    parseErrorIncomplete :: Bool
  }
  deriving (Eq, Show)

data ParseState = ParseState
  { stateInput :: String,
    statePos :: SourcePos
  }

-- StateT discards its state on failure, so consumption is carried by the
-- underlying result and accumulated by its Applicative/Monad instances.
data ParseOutcome a
  = Parsed a Bool
  | Failed (Maybe ParseError) Bool

newtype ParseResult a = ParseResult {unParseResult :: ParseOutcome a}

type Parser = StateT ParseState ParseResult

instance Functor ParseResult where
  fmap f (ParseResult result) = ParseResult $ case result of
    Parsed value consumed -> Parsed (f value) consumed
    Failed err consumed -> Failed err consumed

instance Applicative ParseResult where
  pure value = ParseResult (Parsed value False)
  ParseResult function <*> ParseResult value = ParseResult $ case function of
    Failed err consumed -> Failed err consumed
    Parsed f consumedFunction -> case value of
      Failed err consumedValue -> Failed err (consumedFunction || consumedValue)
      Parsed result consumedValue -> Parsed (f result) (consumedFunction || consumedValue)

instance Monad ParseResult where
  ParseResult result >>= next = ParseResult $ case result of
    Failed err consumed -> Failed err consumed
    Parsed value consumedFirst ->
      case unParseResult (next value) of
        Failed err consumedNext -> Failed err (consumedFirst || consumedNext)
        Parsed final consumedNext -> Parsed final (consumedFirst || consumedNext)

instance Alternative ParseResult where
  empty = ParseResult (Failed Nothing False)
  ParseResult left <|> right = ParseResult $ case left of
    success@(Parsed _ _) -> success
    Failed leftError True -> Failed leftError True
    Failed leftError False ->
      case unParseResult right of
        success@(Parsed _ _) -> success
        Failed rightError rightConsumed
          | rightConsumed -> Failed rightError True
          | otherwise -> Failed (mergeMaybeErrors leftError rightError) False

instance MonadPlus ParseResult where
  mzero = empty
  mplus = (<|>)

runParser :: Parser a -> String -> Either ParseError a
runParser parser input =
  case unParseResult (runStateT parser initialState) of
    Parsed (value, _) _ -> Right value
    Failed (Just err) _ -> Left err
    Failed Nothing _ -> Left (mkError initialState Nothing ["an expression"] True)
  where
    initialState = ParseState input (SourcePos 0 1 1)

tryP :: Parser a -> Parser a
tryP parser = StateT $ \state ->
  ParseResult $ case unParseResult (runStateT parser state) of
    Failed err _ -> Failed err False
    Parsed value consumed -> Parsed value consumed

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy expectation predicate = StateT $ \state ->
  ParseResult $ case stateInput state of
    [] -> Failed (Just (mkError state Nothing [expectation] True)) False
    value : rest
      | predicate value ->
          Parsed (value, ParseState rest (advance (statePos state) value)) True
      | otherwise -> Failed (Just (mkError state (Just [value]) [expectation] False)) False

char :: Char -> Parser Char
char value = satisfy [value] (== value)

string :: String -> Parser String
string = traverse char

eof :: Parser ()
eof = StateT $ \state ->
  ParseResult $ case stateInput state of
    [] -> Parsed ((), state) False
    value : _ -> Failed (Just (mkError state (Just [value]) ["end of input"] False)) False

parseExpr :: String -> Either ParseError SExpr
parseExpr = runParser (spaceConsumer *> datum <* spaceConsumer <* eof)

parseProgram :: String -> Either ParseError [SExpr]
parseProgram = runParser (spaceConsumer *> many datum <* spaceConsumer <* eof)

datum :: Parser SExpr
datum = lexeme (quoted <|> boolean <|> stringLiteral <|> list <|> atom)

quoted :: Parser SExpr
quoted = do
  _ <- char '\''
  value <- spaceConsumer *> datum
  pure (List [Symbol "quote", value])

boolean :: Parser SExpr
boolean = do
  _ <- char '#'
  value <- satisfy "t or f" (\c -> c == 't' || c == 'f')
  tokenBoundary
  pure (Boolean (value == 't'))

stringLiteral :: Parser SExpr
stringLiteral = do
  _ <- char '"'
  content <- many stringCharacter
  _ <- char '"'
  pure (StringLiteral content)

stringCharacter :: Parser Char
stringCharacter = escaped <|> satisfy "string character" (\value -> value /= '"' && value /= '\\')
  where
    escaped = do
      _ <- char '\\'
      value <- satisfy "string escape" (`elem` ['"', '\\', 'n', 't', 'r'])
      pure $ case value of
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'
        other -> other

list :: Parser SExpr
list = do
  _ <- char '('
  spaceConsumer
  listContents []

listContents :: [SExpr] -> Parser SExpr
listContents reversed = do
  next <- peekChar
  case next of
    Nothing -> expected ")"
    Just ')' -> char ')' *> pure (List (reverse reversed))
    Just '.'
      | not (null reversed) -> do
          dotIsDelimiter <- dotDelimiterAhead
          if dotIsDelimiter
            then do
              _ <- char '.'
              spaceConsumer1
              tailValue <- datum
              spaceConsumer
              _ <- char ')'
              pure (DottedList (reverse reversed) tailValue)
            else readElement
    _ -> readElement
  where
    readElement = do
      value <- datum
      listContents (value : reversed)

atom :: Parser SExpr
atom = do
  token <- some (satisfy "symbol or integer" (not . isDelimiter))
  if isIntegerToken token
    then pure (Number (read token))
    else
      if isSymbolToken token
        then pure (Symbol token)
        else invalidToken "a valid symbol or integer"

spaceConsumer :: Parser ()
spaceConsumer = () <$ many spaceUnit

spaceConsumer1 :: Parser ()
spaceConsumer1 = () <$ some spaceUnit

spaceUnit :: Parser ()
spaceUnit = (() <$ satisfy "whitespace" isSpace) <|> lineComment

lineComment :: Parser ()
lineComment = do
  _ <- char ';'
  _ <- many (satisfy "comment character" (/= '\n'))
  _ <- optional (char '\n')
  pure ()

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaceConsumer

peekChar :: Parser (Maybe Char)
peekChar = StateT $ \state ->
  let value = case stateInput state of
        [] -> Nothing
        c : _ -> Just c
   in ParseResult (Parsed (value, state) False)

dotDelimiterAhead :: Parser Bool
dotDelimiterAhead = StateT $ \state ->
  let result = case stateInput state of
        '.' : next : _ -> isSpace next || next == ';'
        _ -> False
   in ParseResult (Parsed (result, state) False)

tokenBoundary :: Parser ()
tokenBoundary = StateT $ \state ->
  ParseResult $ case stateInput state of
    [] -> Parsed ((), state) False
    value : _
      | isDelimiter value -> Parsed ((), state) False
      | otherwise -> Failed (Just (mkError state (Just [value]) ["token boundary"] False)) False

expected :: String -> Parser a
expected expectation = StateT $ \state ->
  let atEnd = null (stateInput state)
      unexpected = case stateInput state of
        [] -> Nothing
        value : _ -> Just [value]
   in ParseResult (Failed (Just (mkError state unexpected [expectation] atEnd)) False)

invalidToken :: String -> Parser a
invalidToken expectation = StateT $ \state ->
  let unexpected = case stateInput state of
        [] -> Nothing
        value : _ -> Just [value]
   in ParseResult (Failed (Just (mkError state unexpected [expectation] False)) False)

isDelimiter :: Char -> Bool
isDelimiter value = isSpace value || value `elem` ['(', ')', '\'', '"', ';']

isIntegerToken :: String -> Bool
isIntegerToken token =
  case token of
    '+' : digits -> not (null digits) && all isDigit digits
    '-' : digits -> not (null digits) && all isDigit digits
    _ -> not (null token) && all isDigit token

isSymbolToken :: String -> Bool
isSymbolToken [] = False
isSymbolToken (first : rest) = validInitial first && all validSubsequent rest
  where
    validInitial value = isLetter value || value `elem` "!$%&*/:<=>?^_~+-"
    validSubsequent value = validInitial value || isDigit value || value `elem` ".@"

advance :: SourcePos -> Char -> SourcePos
advance pos value
  | value == '\n' = SourcePos (sourceOffset pos + 1) (sourceLine pos + 1) 1
  | otherwise = SourcePos (sourceOffset pos + 1) (sourceLine pos) (sourceColumn pos + 1)

mkError :: ParseState -> Maybe String -> [String] -> Bool -> ParseError
mkError state unexpected expectations incomplete =
  ParseError (statePos state) unexpected expectations incomplete

mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors left right =
  case compare (sourceOffset (parseErrorPos left)) (sourceOffset (parseErrorPos right)) of
    LT -> right
    GT -> left
    EQ ->
      ParseError
        (parseErrorPos left)
        (parseErrorUnexpected left <|> parseErrorUnexpected right)
        (sort (nub (parseErrorExpected left ++ parseErrorExpected right)))
        (parseErrorIncomplete left || parseErrorIncomplete right)

mergeMaybeErrors :: Maybe ParseError -> Maybe ParseError -> Maybe ParseError
mergeMaybeErrors Nothing right = right
mergeMaybeErrors left Nothing = left
mergeMaybeErrors (Just left) (Just right) = Just (mergeErrors left right)

renderParseError :: ParseError -> String
renderParseError err =
  "line "
    ++ show (sourceLine pos)
    ++ ", column "
    ++ show (sourceColumn pos)
    ++ ": unexpected "
    ++ maybe "end of input" show (parseErrorUnexpected err)
    ++ expectedSuffix
  where
    pos = parseErrorPos err
    expectedSuffix = case parseErrorExpected err of
      [] -> ""
      values -> "; expected " ++ intercalate ", " values
