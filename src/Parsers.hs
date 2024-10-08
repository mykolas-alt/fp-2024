{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Data.Char (digitToInt)
import Data.List (intercalate)
import ParsingHelpers
import PrimitiveParsers

-- types
-- rental_store = "VHS Rental Store" ':' (catalog | customer_database | rental_transactions)
data RentalStore = VHSRentalStore Catalog

parseRentalStore :: Parser RentalStore
parseRentalStore s =
  case and2 (parseString "VHS Rental Store:") parseCatalog s of
    Left e -> Left e
    Right ((_, c), r) -> Right (VHSRentalStore c, r)

instance Show RentalStore where
  show (VHSRentalStore c) = "VHS Rental Store:" ++ show c

-- catalog = "Catalog" ':' movie_list
newtype Catalog = Catalog MovieList

parseCatalog :: Parser Catalog
parseCatalog s =
  case and2 (parseString "Catalog:") parseMovieList s of
    Left e -> Left e
    Right ((_, ml), r) -> Right (Catalog ml, r)

instance Show Catalog where
  show (Catalog ml) = "Catalog:" ++ show ml

-- movie = title ',' year ',' genre ',' rating ',' availability
data Movie = Movie Title Year Genre Rating Availability
  deriving (Eq)

-- >>> parseMovie "Pavadinimas,1990,Action,PG,Available"
-- Right ("Pavadinimas",1990,Action,PG,Available,"")
parseMovie :: Parser Movie
parseMovie s =
  case and2 parseTitle (parseChar ',') s of
    Left e1 -> Left e1
    Right ((v1a, _), r1) -> case and2 parseYear (parseChar ',') r1 of
      Left e2 -> Left e2
      Right ((v2a, _), r2) -> case and2 parseGenre (parseChar ',') r2 of
        Left e3 -> Left e3
        Right ((v3a, _), r3) -> case and2 parseRating (parseChar ',') r3 of
          Left e4 -> Left e4
          Right ((v4a, _), r4) -> case parseAvailability r4 of
            Left e5 -> Left e5
            Right (v5a, r5) -> Right (Movie v1a v2a v3a v4a v5a, r5)

instance Show Movie where
  show (Movie t y g r a) = intercalate "," [show t, show y, show g, show r, show a]

--   show (Movie t y g r a) = concat ["Title: ", show t, ", Year: ", show y, ", Genre: ", show g, ", Rating: ", show r, ", Availability: ", show a]

-- movie_list = movie | (movie ',' movie_list)
data MovieList = Single Movie | List Movie MovieList
  deriving (Eq)

instance Show MovieList where
  show (Single m) = show m
  show (List m ml) = show m ++ "," ++ show ml

parseMovieList :: Parser MovieList
parseMovieList s =
  case parseMovie s of
    Left e1 -> Left e1
    Right (m, r) -> case many1 (and2 (parseChar ',') parseMovie) r of
      Left _ -> Right (Single m, r)
      Right (arr, r2) -> Right (concatMovies (m : map snd arr), r2)
      where
        concatMovies [x] = Single x
        concatMovies (x : xs) = List x (concatMovies xs)
        concatMovies _ = error "Empty array"

-- BNF: title = string
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
  let allAvailabilities = map (parseString . show) [Available, Rented]
   in case orX allAvailabilities s of
        Left e1 -> Left ("Could not parse Availability: " ++ e1)
        Right (v1, r1) -> Right (read v1, r1)

-- BNF: rating = "G" | "PG" | "PG-13" | "R" | "NR"
data Rating = G | PG | PG13 | R | NR
  deriving (Show, Read, Eq)

parseRating :: Parser Rating
parseRating s =
  let allRatings = map (parseString . show) [G, PG, PG13, R, NR]
   in case orX allRatings s of
        Left e1 -> Left ("Could not parse Rating: " ++ e1)
        Right (v1, r1) -> Right (read v1, r1)

-- BNF: genre = "Action" | "Comedy" | "Drama" | "Horror" | "Romance" | "Sci-Fi" | "Documentary" | "Family"
data Genre = Action | Comedy | Drama | Horror | Romance | SciFi | Documentary | Family
  deriving (Show, Read, Eq)

parseGenre :: Parser Genre
parseGenre s =
  let allGenres = map (parseString . show) [Action, Comedy, Drama, Horror, Romance, SciFi, Documentary, Family]
   in case orX allGenres s of
        Left e1 -> Left ("Could not parse Genre: " ++ e1)
        Right (v1, r1) -> Right (read v1, r1)

parseAlphaNum :: Parser Char
parseAlphaNum = or2 parseLetter parseDigit

parseSeperator :: Parser String
parseSeperator s = case and2 (parseChar ',') (parseChar ' ') s of
  Left e -> Left e
  Right ((a, b), r) -> Right ([a, b], r)

-- >>> test = and2 (parseChar ',') (parseChar ' ')
-- >>> test ", ok"
-- Right ((',',' '),"ok")
