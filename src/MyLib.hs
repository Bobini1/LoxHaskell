module MyLib (run) where

import Control.Monad
import Control.Monad.State
import Data.HashMap.Lazy (HashMap, empty)

run :: String -> IO ()
run str = do
  case fst $ runState scanToken (defaultInput str) of
    Left err -> print err
    Right token -> print token

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
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
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
  return $ scan state c line
  where
    scan state c line = do 
      tokenType <- case c of
        '(' -> Right LeftParen
        ')' -> Right RightParen
        '{' -> Right LeftBrace
        '}' -> Right RightBrace
        ',' -> Right Comma
        '.' -> Right Dot
        '-' -> Right Minus
        '+' -> Right Plus
        ';' -> Right Semicolon
        '*' -> Right Star
        _ -> Left $ LoxError line "Unexpected character."
      return $ createToken state tokenType
