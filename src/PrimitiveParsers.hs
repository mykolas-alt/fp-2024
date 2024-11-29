{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PrimitiveParsers
  ( char,
    letter,
    digit,
    string,
    space,
    Parser (..),
    atomic,
    parse,
    alphaNum,
  )
where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State (State, get, put, runState)
import qualified Data.Char as C

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

atomic :: Parser a -> Parser a
atomic p = do
  input <- lift get
  catchE p (\err -> lift (put input) >> throwE err)

single :: Parser Char
single = do
  input <- lift get
  case input of
    [] -> throwE "Parser received empty input."
    (x : xs) -> lift $ put xs >> return x

sat :: (Char -> Bool) -> Parser Char
sat p = do
  input <- lift get
  case input of
    [] -> throwE "Parser received empty input."
    (x : xs) ->
      if p x
        then lift $ put xs >> return x
        else
          throwE $ "Parser error at: \"" ++ input ++ "\""

char :: Char -> Parser Char
char c = catchE (sat (== c)) (\err -> throwE $ ('\'' : c : '\'' : " could not be parsed. ") ++ err)

letter :: Parser Char
letter = catchE (sat C.isLetter) (\err -> throwE $ "Could not parse letter. " ++ err)

space :: Parser Char
space = char ' '

digit :: Parser Char
digit = catchE (sat C.isDigit) (\err -> throwE $ "A digit could not be parsed. " ++ err)

string :: String -> Parser String
string str = atomic $ mapM char str

alphaNum :: Parser Char
alphaNum = catchE (sat C.isAlphaNum) (\err -> throwE $ "An alphanumeric character could not be parsed. " ++ err)
