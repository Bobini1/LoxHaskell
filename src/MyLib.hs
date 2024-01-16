{-# LANGUAGE LambdaCase #-}

module MyLib (run) where

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

scanToken :: State InputState (Either LoxError Token)
scanToken = do
  c <- advance
  state <- get
  let line = inputLine $ inputPos state
  scan c line
  where
    scan c line = do
      tokenType <-
        ( case c of
            '(' -> return $ Right LeftParen
            ')' -> return $ Right RightParen
            '{' -> return $ Right LeftBrace
            '}' -> return $ Right RightBrace
            ',' -> return $ Right Comma
            '.' -> return $ Right Dot
            '-' -> return $ Right Minus
            '+' -> return $ Right Plus
            ';' -> return $ Right Semicolon
            '*' -> return $ Right Star
            '!' ->
              do
                equals <- match '='
                return $ Right (if equals then BangEqual else Bang)
            '=' ->
              do
                equals <- match '='
                return $ Right (if equals then EqualEqual else Equal)
            '<' ->
              do
                equals <- match '='
                return $ Right (if equals then LessEqual else Less)
            '>' ->
              do
                equals <- match '='
                return $ Right (if equals then GreaterEqual else Greater)
            _ -> return $ Left $ LoxError line "Unexpected character."
          )
      state <- get
      return $ createToken state <$> tokenType

match :: Char -> State InputState Bool
match expected = do
  state <- get
  if isOver state || (source state !! current (inputPos state) /= expected)
    then return False
    else do
      _ <- advance
      return True

isOver :: InputState -> Bool
isOver state = length (source state) <= current (inputPos state)

setStartToCurrent :: State InputState ()
setStartToCurrent = do
  (InputState source (InputPos _ current inputLine)) <- get
  put $ InputState source (InputPos current current inputLine)

scanTokens :: State InputState [Either LoxError Token]
scanTokens =
  loop False []
  where
    loop over tokens = do
      if over
        then return tokens
        else do
          setStartToCurrent
          token <- scanToken
          state <- get
          loop (isOver state) (token : tokens)
