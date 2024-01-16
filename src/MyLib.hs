{-# LANGUAGE LambdaCase #-}

module MyLib (run) where

import Control.Monad.Except
import Control.Monad.State

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
  | String
  | Number
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
  deriving (Read, Show, Enum, Eq, Ord)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    literal :: String,
    line :: Int
  }
  deriving (Read, Show, Eq)

data InputPos = InputPos
  { start :: Int,
    current :: Int,
    inputLine :: Int
  }
  deriving (Read, Show, Eq)

data InputState = InputState
  { source :: String,
    inputPos :: InputPos
  }

defaultInput :: String -> InputState
defaultInput source = InputState source (InputPos 0 0 1)

advance :: State InputState Char
advance = do
  (InputState source (InputPos start current inputLine)) <- get
  put $ InputState source (InputPos start (current + 1) inputLine)
  return $ source !! current

createToken :: InputState -> TokenType -> Token
createToken (InputState source (InputPos start current inputLine)) tokenType = Token tokenType text "" inputLine
  where
    text = take (current - start) $ drop start source

data LoxError = LoxError
  { sourceLine :: Int,
    message :: String
  }
  deriving (Read, Show, Eq)

type TokenResult = ExceptT LoxError Maybe Token

-- type TokenResult = MaybeT (Either LoxError) Token

scanToken :: State InputState TokenResult
scanToken = do
  c <- advance
  state <- get
  let line = inputLine $ inputPos state
  tokenType <-
    ( case c of
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
        _ -> return $ throwError (LoxError line "Unexpected character.")
    ) ::
      State InputState (ExceptT LoxError Maybe TokenType)
  state' <- get
  return $ createToken state' <$> tokenType
  where
    returnToken = return . lift . return

match :: Char -> State InputState Bool
match expected = do
  state <- get
  if isOver state || (source state !! current (inputPos state) /= expected)
    then return False
    else do
      _ <- advance
      return True

peek :: InputState -> Char
peek (InputState source (InputPos _ current _)) = source !! current

isOver :: InputState -> Bool
isOver state = length (source state) <= current (inputPos state)

setStartToCurrent :: State InputState ()
setStartToCurrent = do
  (InputState source (InputPos _ current inputLine)) <- get
  put $ InputState source (InputPos current current inputLine)

scanTokens :: State InputState [Either LoxError Token]
scanTokens = do
  over <- gets isOver
  if over
    then return []
    else do
      setStartToCurrent
      token <- scanToken
      tokens <- scanTokens
      case runExceptT token of
        Nothing -> return tokens
        Just token' -> return $ token' : tokens
