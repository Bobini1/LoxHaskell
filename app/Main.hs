module Main where

import System.IO (readFile')
import System.Environment (getArgs)
import System.Exit
import GHC.GHCi.Helpers (flushAll)
import MyLib

readInput :: String -> IO ()
readInput path = do
  contents <- readFile' path
  run contents

runPrompt :: IO ()
runPrompt = do
  putStr ">> "
  flushAll
  contents <- getContents
  let inputLines = lines contents
  mapM_ (\line -> run line >> putStr "\n>> " >> flushAll) inputLines

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [path] -> readInput path
    _ -> die "Bad input"
