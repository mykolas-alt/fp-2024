{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Data.Char (digitToInt)
import Data.List (intercalate)
import ParsingHelpers
import PrimitiveParsers

-- types
-- rental_store = "VHS Rental Store" ':' (catalog | customer_database | rental_transactions)
data RentalStore = VHSRentalStore Catalog
  deriving (Eq)

parseRentalStore :: Parser RentalStore
parseRentalStore = and2 (\_ x -> VHSRentalStore x) (parseString "VHS Rental Store:") parseCatalog

instance Show RentalStore where
  show (VHSRentalStore c) = "VHS Rental Store:" ++ show c

-- catalog = "Catalog" ':' movie_list
newtype Catalog = Catalog MovieList
  deriving (Eq)

parseCatalog :: Parser Catalog
parseCatalog = and2 (\_ x -> Catalog x) (parseString "Catalog:") parseMovieList

instance Show Catalog where
  show (Catalog ml) = "Catalog:" ++ show ml

-- movie = title ',' year ',' genre ',' rating ',' availability
data Movie = Movie Title Year Genre Rating Availability
  deriving (Eq)

parseMovie :: Parser Movie
parseMovie = and5 Movie parseTitle (afterSep parseYear) (afterSep parseGenre) (afterSep parseRating) (afterSep parseAvailability)
  where
    afterSep = and2 (\_ y -> y) parseSeperator

instance Show Movie where
  show (Movie t y g r a) = intercalate "," [show t, show y, show g, show r, show a]

-- movie_list = movie | (movie ',' movie_list)
data MovieList = Single Movie | List Movie MovieList
  deriving (Eq)

instance Show MovieList where
  show (Single m) = show m
  show (List m ml) = show m ++ "," ++ show ml

parseSingleMovie :: Parser MovieList
parseSingleMovie s = case parseMovie s of
  Left e -> Left e
  Right (m, r) -> Right (Single m, r)

parseListMovie :: Parser MovieList
parseListMovie = and3 (\(x, _, y) -> List x y) parseMovie parseSeperator parseMovieList

parseMovieList :: Parser MovieList
parseMovieList = or2 parseListMovie parseSingleMovie

-- BNF: title = alphanumeric+
type Title = String

parseTitle :: Parser Title
parseTitle = many1 parseAlphaNum

-- BNF: year = digit digit digit digit
type Year = Int

parseYear :: Parser Year
parseYear s =
  case parseN 4 parseDigit s of
    Left _ -> Left ("Unable to parse year(YYYY) from: " ++ s)
    Right (xs, r) -> Right (foldl (\x y -> x * 10 + y) 0 (map digitToInt xs), r)

-- BNF: availability = "Available" | "Rented"
data Availability = Available | Rented
  deriving (Show, Read, Eq)

parseAvailability :: Parser Availability
parseAvailability s =
  case parseData [Available, Rented] s of
    Left e1 -> Left ("Could not parse Availability: " ++ e1)
    Right (v1, r1) -> Right (read v1, r1)

-- BNF: rating = "G" | "PG" | "PG-13" | "R" | "NR"
data Rating = G | PG | PG13 | R | NR
  deriving (Show, Read, Eq)

parseRating :: Parser Rating
parseRating s =
  case parseData [G, PG13, PG, R, NR] s of
    Left e1 -> Left ("Could not parse Rating: " ++ e1)
    Right (v1, r1) -> Right (read v1, r1)

-- BNF: genre = "Action" | "Comedy" | "Drama" | "Horror" | "Romance" | "Sci-Fi" | "Documentary" | "Family"
data Genre = Action | Comedy | Drama | Horror | Romance | SciFi | Documentary | Family
  deriving (Show, Read, Eq)

parseGenre :: Parser Genre
parseGenre s =
  case parseData [Action, Comedy, Drama, Horror, Romance, SciFi, Documentary, Family] s of
    Left e1 -> Left ("Could not parse Genre: " ++ e1)
    Right (v1, r1) -> Right (read v1, r1)

parseAlphaNum :: Parser Char
parseAlphaNum = or2 parseLetter parseDigit

parseSeperator :: Parser Char
parseSeperator = parseChar ','

parseData :: (Show a) => [a] -> Parser String
parseData = orX . map (parseString . show)