module Interpreter.Parser where

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

--{{{ Definitions of LispVals and LispError

---------------------------
-- Our LispVal data type --
---------------------------
data LispVal   = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | Float Double
               | Ratio Rational
               | Complex (Complex Double)
               | String String
               | Bool Bool
               | Character Char
               | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
               | Func { params   :: [String]
                      , vararg   :: (Maybe String)
                      , body     :: [LispVal]
                      , closure  :: Env
                      }
               | IOFunc ([LispVal] -> IOThrowsError LispVal)
               | Port Handle

-- Custom implementation of show for LispVal
showVal :: LispVal -> String
showVal (String contents)        = "\"" ++ contents ++ "\""
showVal (Atom name)              = name
showVal (List contents)          = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail)   = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number contents)        = show contents
showVal (Float contents)         = show contents
showVal (Ratio contents)         = show contents
showVal (Complex contents)       = show contents
showVal (Bool True)              = "#t"
showVal (Bool False)             = "#f"
showVal (PrimitiveFunc _)        = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

instance Show LispVal where show = showVal

--------------------------------------------
-- LispError data type for error handling --
--------------------------------------------
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- Custom implementation of show for LispError
showError :: LispError -> String
showError (UnboundVar message varname)    = message ++ ": " ++ varname
showError (BadSpecialForm message form)   = message ++ ": " ++ show form
showError (NotFunction message func)      = message ++ ": " ++ show func
showError (NumArgs expected found)        = "Expected " ++ show expected
                                         ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found)   = "Invalid type: expected " ++ expected
                                         ++ ", found " ++ show found
showError (Parser parseErr)               = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
   noMsg  = Default "An error has occurred"
   strMsg = Default

-- LispError type constructors
type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO
type Env = IORef [(String, IORef LispVal)]

-- Helper action for creating an empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- Traps error values
trapError action = catchError action (return . show)

-- Extract data from the either Monad so it can be passed
-- to other functions
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Converts LispVals into string representations, and then applies
-- the unwords function. The unwords function glues together a list
-- of words with spaces.
unwordsList :: [LispVal] -> String
unwordsList  = unwords . map showVal

--}}}

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
   Left err    -> throwError $ Parser err
   Right val   -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

parseExpr :: Parser LispVal
parseExpr =  parseAtom 
         <|> parseString
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseRatio
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         <|> parseQuoted
         <|> parseLists

--{{{ Parsers

-- Parser that recognizes one of the symbols
-- allowed in Scheme identifiers.
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>^_~?"

-- Parser that handles whitespace
spaces :: Parser ()
spaces  = skipMany1 space

-- Parser for Atoms 
--
-- TODO:: You can probably take out the case thing now,
-- seeing as Bools have their own thing
--
parseAtom :: Parser LispVal
parseAtom = do
               first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                           "#t" -> Bool True
                           "#f" -> Bool False
                           _    -> Atom atom

-- Parse a list of LispVals
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- Parse a dotted list of LispVals
parseDottedList :: Parser LispVal
parseDottedList = do
   head <- endBy parseExpr spaces
   tail <- char '.' >> spaces >> parseExpr
   return $ DottedList head tail

parseLists :: Parser LispVal
parseLists = do
               char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

-- A parser for numbers. Supports decimal, hexadecimal, octal,
-- and binary numbers
parseNumber :: Parser LispVal
parseNumber  = parseDec1 <|> parseDec2 <|> parseHex <|> parseOct <|> parseBin

-- Parser for decimals
parseDec1 :: Parser LispVal
parseDec1  = many1 digit >>= (return . Number . read)

-- Parser for decimals with '#d' tag
parseDec2 :: Parser LispVal
parseDec2 = do 
   try $ string "#d"
   num <- many1 digit
   (return . Number . read) num

-- Parser for hexadecimals
parseHex :: Parser LispVal
parseHex  = do 
   try $ string "#x"
   num <- many1 hexDigit
   return $ Number (hex2dig num)

-- Parser for octals
parseOct :: Parser LispVal
parseOct  = do 
   try $ string "#o"
   num <- many1 octDigit
   return $ Number (oct2dig num)

-- Parser for binanry numbers
parseBin :: Parser LispVal
parseBin  = do 
   try $ string "#b"
   num <- many1 (oneOf "10")
   return $ Number (bin2dig num)

-- Several helper functions used to convert octal, hex, and 
-- binary numbers into decimal numbers. Both ``oct2dig`` and
-- ``hex2dig`` make use of built in Haskell functions. ``bin2dig``
-- must be dealt with in a more custom manner
-- 
oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig   = bin2dig' 0
bin2dig' num "" = num
bin2dig' num (x:xs) = let old = 2 * num + (if x == '0' then 0 else 1) in
                      bin2dig' old xs

-- TODO: Support parseFloat, parseRatio, and
-- parseComplex more fully. Currently, our interpreter
-- will recognize these objects, but can't do any
-- computations with them

-- Parser for floating point values
parseFloat :: Parser LispVal
parseFloat = do
               x <- many1 digit
               char '.'
               y <- many1 digit
               return $ Float (fst . head $ readFloat (x++"."++y))

-- Parser for Haskell's rational type 
parseRatio :: Parser LispVal
parseRatio =   do x <-many1 digit
                  char '/'
                  y <- many1 digit
                  return $ Ratio ((read x) % (read y))


-- Parser for complex numbers
parseComplex :: Parser LispVal
parseComplex  = do 
   x <- (try parseFloat <|> parseDec1)
   char '+'
   y <- (try parseFloat <|> parseDec1)
   char 'i'
   return $ Complex (toDouble x :+ toDouble y)

-- Converts LispVals to Doubles
toDouble :: LispVal -> Double
toDouble (Float f)   = f
toDouble (Number n)  = fromIntegral n

-- Parser for strings 
parseString :: Parser LispVal
parseString  = do
   char '"'
   x <- many $ parseEscape <|> noneOf "\\\""
   char '"'
   return $ String x

-- Support for several different escape values
parseEscape :: Parser Char
parseEscape  = do
   char '\\'
   esc <- oneOf "\\\"ntrb"
   return $ case esc of
      '\\'  -> esc
      '"'   -> esc
      'n'   -> '\n'
      't'   -> '\t'
      'r'   -> '\r'
      'b'   -> '\b'

-- Parser for boolean values
parseBool :: Parser LispVal
parseBool = do
   char '#'
   (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

-- Parser for character literals
parseCharacter :: Parser LispVal
parseCharacter  = do
   try $ string "#\\"
   value <- try (string "newline" <|> string "space")
            <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
   return $ Character $ case value of
      "space"     -> ' '
      "newline"   -> '\n'
      otherwise   -> (value !! 0)
 
-- Parser for Scheme's single-quote syntax
parseQuoted :: Parser LispVal
parseQuoted = do
   char '\''
   x <- parseExpr
   return $ List [Atom "quote", x]

--}}}
