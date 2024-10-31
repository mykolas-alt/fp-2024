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
    alphaNum,
  )
where

import Control.Applicative (Alternative (empty), (<|>))
import qualified Data.Char as C

newtype Parser a where
  P :: {parse :: String -> Either String (a, String)} -> Parser a

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    a <- p
    return $ f a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = do
    f <- pf
    f <$> pa

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = P $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r

single :: Parser Char
single = P $ \str -> case str of
  [] -> Left "Empty String"
  (x : xs) -> Right (x, xs)

sat :: (Char -> Bool) -> Parser Char
sat p = P $ \case
  [] -> Left "Empty String"
  s@(x : xs) -> if p x then Right (x, xs) else Left $ "Could not recognize: " ++ s

char :: Char -> Parser Char
char c = sat (== c)

letter :: Parser Char
letter = sat C.isLetter

space :: Parser Char
space = char ' '

digit :: Parser Char
digit = sat C.isDigit

string :: String -> Parser String
string [] = return []
string (c : cs) = do
  c' <- char c
  cs' <- string cs
  return (c' : cs')

alphaNum :: Parser Char
alphaNum = sat C.isAlphaNum
