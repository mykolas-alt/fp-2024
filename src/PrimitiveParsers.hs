{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module PrimitiveParsers
  ( parseChar,
    parseLetter,
    parseDigit,
    parseString,
    parseSpace,
  )
where

import qualified Data.Char as C

type Parser a = String -> Either String (a, String)

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h : t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h : t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h : t) = if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")

parseString :: String -> Parser String
parseString [] s = Right ([], s)
parseString str@(c : cs) s =
  let err = (str ++ " is not found in: " ++ s)
   in case parseChar c s of
        Left _ -> Left err
        Right (v1, r1) -> case parseString cs r1 of
          Left _ -> Left err
          Right (v2, r2) -> Right (v1 : v2, r2)