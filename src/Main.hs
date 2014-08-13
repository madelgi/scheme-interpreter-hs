module Main where

{-# LANGUAGE ExistentialQuantification #-}

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Control.Monad.Error
import System.IO                                -- For REPL
import Data.IORef                               -- For stateful vars
import Interpreter.Evaluator
import Interpreter.Parser

main :: IO ()
main = do 
   args <- getArgs
   if null args
      then runRepl
      else runOne $ args

--{{{ REPL

-- runs a loop that waits for user input, reads the input, evaluates it, ad nauseum
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (=="quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
   env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
   (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !!0)]))
      >>= hPutStrLn stderr

-- repeating Monadic function that does not return a value. The 
-- function takes a predicate that signals when to stop, an action 
-- to perform before the test, and a function returning an action 
-- to do to the input
--
-- NOTE: The underscore is a naming convention for manadic functions
-- that repeat but do not return a value
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

-- Prints a prompt and read a line of input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Evaluates a string and prints the result
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- Pull the code to parse and evaluate a string, and trap the errors out of main 
-- into its own function
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- Prints out a string and immediately flushes the stream.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

--}}}
