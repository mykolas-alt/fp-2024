{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ParsingHelpers where

type Parser a = String -> Either String (a, String)

and3 :: ((a, b, c) -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f a b c input =
  case a input of
    Left e1 -> Left e1
    Right (v1, r1) ->
      case b r1 of
        Left e2 -> Left e2
        Right (v2, r2) -> case c r2 of
          Right (v3, r3) -> Right (f (v1, v2, v3), r3)
          Left e3 -> Left e3

and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5 f a b c d e input =
  case a input of
    Left e1 -> Left e1
    Right (v1, r1) ->
      case b r1 of
        Left e2 -> Left e2
        Right (v2, r2) -> case c r2 of
          Left e3 -> Left e3
          Right (v3, r3) -> case d r3 of
            Left e4 -> Left e4
            Right (v4, r4) -> case e r4 of
              Left e5 -> Left e5
              Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f a b input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (f v1 v2, r2)
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
