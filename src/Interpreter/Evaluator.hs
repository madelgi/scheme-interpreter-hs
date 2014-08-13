{-# LANGUAGE ExistentialQuantification #-}

module Interpreter.Evaluator where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Control.Monad.Error
import System.IO                                -- For REPL
import Data.IORef                               -- For stateful vars
import Interpreter.Parser 

-- EVALUATOR --{{{ 

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives  = [("+", numericBinop (+))
              ,("-", numericBinop (-))
              ,("*", numericBinop (*))
              ,("/", numericBinop div)
              ,("mod", numericBinop mod)
              ,("quotient", numericBinop quot)
              ,("remainder", numericBinop rem)
              ,("=", numBoolBinop (==))
              ,("<", numBoolBinop (<))
              ,(">", numBoolBinop (>))
              ,("/=", numBoolBinop (/=))
              ,(">=", numBoolBinop (>=))
              ,("<=", numBoolBinop (<=))
              ,("&&", boolBoolBinop (&&))
              ,("||", boolBoolBinop (||))
              ,("string=?", strBoolBinop (==))
              ,("string<?", strBoolBinop (<))
              ,("string>?", strBoolBinop (>))
              ,("string>=?", strBoolBinop (>=))
              ,("string<=?", strBoolBinop (<=))
              ,("symbol?", unaryOp symbolb)
              ,("string?", unaryOp stringb)
              ,("number?", unaryOp numberb)
              ,("bool?", unaryOp boolb)
              ,("list?", unaryOp listb)
              ,("toSymb", unaryOp toSymb)
              ,("toStr", unaryOp toStr)
              ,("car", car)
              ,("cdr", cdr)
              ,("cons", cons)
              ,("eq?", eqv)
              ,("eqv?", eqv)
              ,("equal?", equal)
              ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives  = [("apply", applyProc)
                ,("open-input-file", makePort ReadMode)
                ,("open-output-file", makePort WriteMode)
                ,("close-input-port", closePort)
                ,("close-output-port", closePort)
                ,("read", readProc)
                ,("write", writeProc)
                ,("read-contents", readContents)
                ,("read-all", readAll)
                ]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
   where makeFunc constructor (var, func) = (var, constructor func)

-------------------------
{- Primitive functions -}
-------------------------

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f []   = throwError $ NumArgs 1 []
unaryOp f [x]  = return $ f x

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left  <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- symbolb, numberb, stringb
--
-- This group of functions checks whether a LispVal is a Lisp symbol,
-- number, or string, respectively. They then return the appropriate
-- Boolean value
--
symbolb :: LispVal -> LispVal
symbolb (Atom _)  = Bool True
symbolb _         = Bool False

numberb :: LispVal -> LispVal
numberb (Number _)   = Bool True
numberb _            = Bool False

stringb :: LispVal -> LispVal
stringb (String _)   = Bool True
stringb _            = Bool False

boolb :: LispVal -> LispVal
boolb (Bool _) = Bool True
boolb _        = Bool False

listb :: LispVal -> LispVal
listb (List _)          = Bool True
listb (DottedList _ _)  = Bool True
listb _                 = Bool False

-- toStr and toSymb
--
-- These functions convert symbols to strings and vice versa
--
toSymb :: LispVal -> LispVal
toSymb (String s) = Atom s
toSymb _          = Atom ""

toStr :: LispVal -> LispVal
toStr (Atom s) = String s
toStr _        = String ""

-- car, cdr, and cons
--
-- The following functions are list primitives
--
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]          = return x
car [DottedList (x:xs) _]  = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]          = return $ List xs
cdr [DottedList [_] x]     = return x
cdr [DottedList (_:xs) x]  = return $ DottedList xs x
cdr [badArg]               = throwError $ TypeMismatch "pair" badArg
cdr badArgList             = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

-- eqv
--
-- Check if two LispVals are equal
--
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]               = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]           = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]           = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]               = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)]   = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]               = return $ Bool $ (length arg1 == length arg2) &&
                                                               (all eqvPair $ zip arg1 arg2)
      where eqvPair (x1, x2) = case eqv [x1, x2] of
                                 Left err -> False
                                 Right (Bool val) -> val
eqv [_, _]                                   = return $ Bool False
eqv badArgList                               = throwError $ NumArgs 2 badArgList

-- equal
--
-- Check if two LispVals are equal, *ignoring* type. For example,
-- this function would register 2 and "2" as equal. Uses the helper function
-- ``unpackEquals``.
--
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)  = return val
eval env val@(Number _)  = return val
eval env val@(Bool _)    = return val
eval env (Atom id)       = getVar env id
eval env (List [Atom "quote", val])  = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
         do result <- eval env pred
            case result of
                 Bool False -> eval env alt
                 Bool True  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = 
   eval env form >>= setVar env var
eval env (List [Atom "load", String filename]) = 
   load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "define", Atom var, form]) = 
   eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
   makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
   makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = 
   makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
   makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
   makeVarArgs varargs env [] body
eval env (List (function : args)) = do
   func <- eval env function
   argVals <- mapM (eval env) args
   apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- Move these
--
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal


----------------------------
{- IO Primitive functions -}
----------------------------

-- Deconstruct the argument list into the form that
-- ``apply`` expects
--
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

-- Wraps the Haskell function ``openFile``
--
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- Wraps the haskell function ``hClose``
--
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)

-- Wraps the haskell function ``hGetLine`` and sends the result
-- to parseExpr, converting it into a LispVal
--
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

-- Converts a LispVal to a string and then writes it out on the
-- specified port
--
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

-- Reads the whole file into a string in memory
--
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- Reasponsible for reading and parsing a file full of 
-- statements
--
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

-- Wraps the return value of ``load`` in a List constructor
--
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
--}}} 

-- VARIABLE ASSIGNMENTS --{{{

{- This code implements variable storage for our Scheme interpreter. 
 - In Scheme, variables can be reset to new values. This presents a
 - complication for Haskell, which only supports immutable variables.
 - This implementation gets around this by using a feature known as
 - State threads. Specifically, we make use of the Data.IORef module
 -}

-- liftThrows 
--
-- We can lift IO vals into the combined IOThrowsError monad using a
-- built-in function. This function provides analagous support for the 
-- untransformed upper type ThrowsError
--
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)   = throwError err
liftThrows (Right val)  = return val

-- runIOThrows
--
-- ``runIOThrows`` takes an IOThrowsError String and returns an IO 
-- String
--
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- isBound
--
-- Determine if a given variable is already bound in the environment.
-- 
--    (1) readIORef envRef -- extracts environment from IORef wrapper
--    (2) lookup var (^) -- checks if var is in our environment
--    (3) maybe False (const True) (^) -- Return False if (^) is not 
--        of the form Just (.), else return True
--    (4) Wrap it in IO
--
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- getVar
-- 
-- Retrieve the current value of a variable
--
--    (1) Retrieve the actual environment from the IORef. In this case,
--        we must also use ``liftIO`` because <- uses the ``IOThrowsError``
--        monad, so we must strip that as well
--    (2) lookup var env -- check if var is in the invironement
--    (3) if (^) is not of the form Just (.), return the error message.
--        Otherwise, apply liftIO . readIORef to generate an IOThrowsError
--        action that reads the returned IORef.
--
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do 
   env <- liftIO $ readIORef envRef
   maybe (throwError $ UnboundVar "Getting an unbound variable" var)
         (liftIO . readIORef)
         (lookup var env)

-- setVar
--
-- This is similar to the previous two. Now, however, if the lookup
-- succeeds, we use ``writeIORef`` to change the variable. ``writeIORef`` 
-- takes its arguments in the opposite order, so we use flip to achieve
-- this. We return the value we just set.
--
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envref var value = do
   env <- liftIO $ readIORef envref
   maybe (throwError $ UnboundVar "Setting an unbound variable" var)
         (liftIO . (flip writeIORef value))
         (lookup var env)
   return value

-- defineVar
--
-- ``defineVar`` takes an environment, string, and a LispVal, and then:
--
--    (1) if the variable is already bound, reset it
--    (2) if the variable is not bound, add it to our environment
--        and bind it to the given LispVal
--
-- Case (1) is simple, as it uses ``setVar`` for most of the work. Case (2)
-- has new stuff going on. Here, we create a new IORef to hold the varaible,
-- and assign the result to valueRef. We then read the current environment 
-- and assign it to env. We then write to envRef, appending our new 
-- (var, valueRef) pair. Lift the whole block into the IOThrowsError monad.
--
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
   alreadyDefined <- liftIO $ isBound envRef var
   if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

-- bindVars
--
-- This function binds a whole buncha variables at once. 
--    
--    (1) addBinding -- takes a variable name and value, creates
--        an IORef to hold the value, and then returns a pair with
--        the altered value
--    (2) extendEnv -- calls addBinding on each (String, LispVal) pair
--        to create (String, IORef LispVal pairs), and then appends the
--        current environment to the end of that.
--
-- After understanding the two helper functions, the final pipeline is 
-- relatively straightforward
--
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
   where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
         addBinding (var, value) = do ref <- newIORef value
                                      return (var, ref)

--}}}

-- HELPER FUNCTIONS (EVALUATOR) --{{{

-- apply
--
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
   if num params /= num args && varargs == Nothing
      then throwError $ NumArgs (num params) args
      else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
   where remainingArgs        = drop (length params) args
         num                  = toInteger . length
         evalBody env         = liftM last $ mapM (eval env) body
         bindVarArgs arg env  = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing      -> return env
apply (IOFunc func) args      = func args

-- unpackNum, unpackStr, unpackBool, unpackEquals
--
-- unpackNum takes a numeric LispVal, and returns a ThrowsError val. 
-- same for ``unpackStr`` and ``unpackBool``. ``unpackEquals`` takes
-- two LispVals and an Unpacker, and then determines if two LispVals
-- are equal when it unpacks them.
--
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
               do unpacked1 <- unpacker arg1
                  unpacked2 <- unpacker arg2
                  return $ unpacked1 == unpacked2
         `catchError` (const $ return False)
--}}}

