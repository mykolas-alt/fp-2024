{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ParsingHelpers where

type Parser a = String -> Either String (a, String)

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 a b input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right ((v1, v2), r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

or2 :: Parser a -> Parser a -> Parser a
or2 a b input =
  case a input of
    Right r1 -> Right r1
    Left e1 ->
      case b input of
        Right r2 -> Right r2
        Left e2 -> Left (e1 ++ ", " ++ e2)

orX :: [Parser a] -> Parser a
orX [] _ = Left "None of the parsers were able to parse successfully"
orX (p : ps) s =
  case p s of
    Left e1 -> case orX ps s of
      Left e2 -> Left (e1 ++ ",\n" ++ e2)
      Right a -> Right a
    Right a -> Right a

many1 :: Parser a -> Parser [a]
many1 p s =
  case p s of
    Left e1 -> Left e1
    Right (v1, r1) -> Right (str, snd res')
      where
        str = v1 : fst res'
        res' = many1' r1
        many1' s2 =
          case p s2 of
            Left _ -> ([], s2)
            Right (v2, r2) -> (v2 : fst res, snd res)
              where
                res = many1' r2

parseN :: Int -> Parser a -> Parser [a]
parseN 0 _ s = Right ([], s)
parseN n p s =
  case p s of
    Left e1 -> Left e1
    Right (v1, r1) -> case parseN (n - 1) p r1 of
      Left e2 -> Left e2
      Right (v2, r2) -> Right (v1 : v2, r2)
