{-# LANGUAGE LambdaCase #-}

module MyLib (run) where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)

run :: String -> IO ()
run str =
  mapM_
    ( \case
        Left err -> print err
        Right token -> print token
    )
    results
  where
    results = evalState scanTokens (defaultInput str)

data TokenType
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier
  | SString String
  | Number Double
  | And
  | Class
  | Else
  | BFalse
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | BTrue
  | Var
  | While
  | Eof
  deriving (Read, Show, Eq, Ord)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    line :: Int
  }
  deriving (Read, Show, Eq)

data InputState = InputState
  { source :: String,
    inputLine :: Int,
    current :: String
  }
  deriving (Read, Show, Eq)

defaultInput :: String -> InputState
defaultInput source = InputState source 1 ""

advance :: State InputState Char
advance = do
  (InputState source inputLine current) <- get
  put $ InputState (tail source) inputLine (head source : current)
  return $ head source

advanceWhile :: (Char -> Bool) -> State InputState ()
advanceWhile predicate = do
  s <- get
  if isOver s || not (predicate $ peek s)
    then return ()
    else do
      when
        (peek s == '\n')
        (modify $ \st -> st {inputLine = inputLine st + 1})
      _ <- advance
      advanceWhile predicate

currentLexeme :: InputState -> String
currentLexeme s = reverse $ current s

createToken :: InputState -> TokenType -> Token
createToken inputState@(InputState _ inputLine _) tokenType =
  let text = currentLexeme inputState
   in Token tokenType text inputLine

data LoxError = LoxError
  { sourceLine :: Int,
    message :: String
  }
  deriving (Read, Show, Eq)

type TokenResult = ExceptT LoxError Maybe Token

parseString :: State InputState (ExceptT LoxError Maybe TokenType)
parseString = do
  _ <- advanceWhile (/= '"')
  eof <- gets isOver
  line <- gets inputLine
  if eof
    then return $ throwError (LoxError line "Unterminated string.")
    else do
      _ <- advance
      lexeme <- gets currentLexeme
      return . lift . return $ SString (init . tail $ lexeme)

parseDigit :: State InputState (ExceptT LoxError Maybe TokenType)
parseDigit = do
  _ <- advanceWhile isDigit
  c <- gets peek
  n <- gets peekNext
  when
    (c == '.' && isDigit n)
    ( do
        _ <- advance
        advanceWhile isDigit
    )
  lexeme <- gets currentLexeme
  return . lift . return $ Number (read lexeme)

parseIdentifier :: State InputState (ExceptT LoxError Maybe TokenType)
parseIdentifier = do
  _ <- advanceWhile isAlphaNum
  lexeme <- gets currentLexeme
  return . lift . return $
    case lexeme of
      "and" -> And
      "class" -> Class
      "else" -> Else
      "false" -> BFalse
      "for" -> For
      "fun" -> Fun
      "if" -> If
      "nil" -> Nil
      "or" -> Or
      "print" -> Print
      "return" -> Return
      "super" -> Super
      "this" -> This
      "true" -> BTrue
      "var" -> Var
      "while" -> While
      _ -> Identifier

scanToken :: State InputState TokenResult
scanToken = do
  c <- advance
  line <- gets inputLine
  tokenType <-
    case c of
      '(' -> returnToken LeftParen
      ')' -> returnToken RightParen
      '{' -> returnToken LeftBrace
      '}' -> returnToken RightBrace
      ',' -> returnToken Comma
      '.' -> returnToken Dot
      '-' -> returnToken Minus
      '+' -> returnToken Plus
      ';' -> returnToken Semicolon
      '*' -> returnToken Star
      '!' ->
        do
          equals <- match '='
          returnToken (if equals then BangEqual else Bang)
      '=' ->
        do
          equals <- match '='
          returnToken (if equals then EqualEqual else Equal)
      '<' ->
        do
          equals <- match '='
          returnToken (if equals then LessEqual else Less)
      '>' ->
        do
          equals <- match '='
          returnToken (if equals then GreaterEqual else Greater)
      '/' -> do
        comment <- match '/'
        if comment
          then do
            _ <- advanceWhile (/= '\n')
            return $ lift Nothing
          else returnToken Slash
      ' ' -> return $ lift Nothing
      '\r' -> return $ lift Nothing
      '\t' -> return $ lift Nothing
      '\n' -> do
        modify $ \s -> s {inputLine = inputLine s + 1}
        return $ lift Nothing
      '"' -> parseString
      _ -> parseOther c line
  s <- get
  return $ createToken s <$> tokenType
  where
    returnToken = return . lift . return
    parseOther c line
      | isDigit c = parseDigit
      | isAlpha c = parseIdentifier
      | otherwise = return $ throwError (LoxError line "Unexpected character.")

match :: Char -> State InputState Bool
match expected = do
  s <- get
  if isOver s || head (source s) /= expected
    then return False
    else do
      _ <- advance
      return True

peek :: InputState -> Char
peek inputState@(InputState source _ _) =
  if isOver inputState then '\0' else head source

peekNext :: InputState -> Char
peekNext inputState@(InputState source _ _) =
  if isOver inputState then '\0' else head (tail source)

isOver :: InputState -> Bool
isOver s = null $ source s

resetCurrent :: InputState -> InputState
resetCurrent (InputState source inputLine _) =
  InputState source inputLine ""

scanTokens :: State InputState [Either LoxError Token]
scanTokens = do
  over <- gets isOver
  if over
    then return []
    else do
      modify resetCurrent
      token <- scanToken
      tokens <- scanTokens
      case runExceptT token of
        Nothing -> return tokens
        Just token' -> return $ token' : tokens
