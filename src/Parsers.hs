{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parsers where

import Control.Applicative (Alternative (empty, some), (<|>))
import Control.Monad (replicateM)
import Data.Char (digitToInt)
import Data.List (intercalate)
import PrimitiveParsers

-- types
-- rental_store = "VHS Rental Store" ':' (catalog | customer_database | rental_transactions)
newtype RentalStore = VHSRentalStore Catalog
  deriving (Eq)

rentalStore :: Parser RentalStore
rentalStore = atomic $ do
  _ <- string "VHS Rental Store:"
  VHSRentalStore <$> catalog

instance Show RentalStore where
  show (VHSRentalStore c) = "VHS Rental Store:" ++ show c

-- catalog = "Catalog" ':' movie_list
newtype Catalog = Catalog MovieList
  deriving (Eq)

catalog :: Parser Catalog
catalog = atomic $ do
  _ <- string "Catalog:"
  Catalog <$> movieList

instance Show Catalog where
  show (Catalog ml) = "Catalog:" ++ show ml

-- movie = title ',' year ',' genre ',' rating ',' availability
data Movie = Movie Title Year Genre Rating Availability
  deriving (Eq)

movie :: Parser Movie
movie = atomic $ do
  t <- title
  _ <- seperator
  y <- year
  _ <- seperator
  g <- genre
  _ <- seperator
  r <- rating
  _ <- seperator
  Movie t y g r <$> availability

instance Show Movie where
  show (Movie t y g r a) = intercalate "," [t, show y, show g, show r, show a]

-- movie_list = movie | (movie ',' movie_list)
data MovieList = Single Movie | List Movie MovieList
  deriving (Eq)

instance Show MovieList where
  show (Single m) = show m
  show (List m ml) = show m ++ "," ++ show ml

singleMovie :: Parser MovieList
singleMovie = Single <$> movie

listMovie :: Parser MovieList
listMovie = atomic $ do
  m <- movie
  _ <- seperator
  List m <$> movieList

movieList :: Parser MovieList
movieList = atomic $ listMovie <|> singleMovie

-- BNF: title = alphanumeric+
type Title = String

title :: Parser Title
title = atomic $ some alphaNum

-- BNF: year = digit digit digit digit
type Year = Int

year :: Parser Year
year = atomic $ do
  ds <- replicateM 4 digit
  return $ foldl (\x y -> x * 10 + y) 0 (map digitToInt ds)

-- BNF: availability = "Available" | "Rented"
data Availability = Available | Rented
  deriving (Show, Read, Eq, Enum)

availability :: Parser Availability
availability = dataParser $ enumFrom (toEnum 0)

-- BNF: rating = "G" | "PG" | "PG-13" | "R" | "NR"
data Rating = G | PG13 | PG | R | NR
  deriving (Show, Read, Eq, Enum)

rating :: Parser Rating
rating = dataParser $ enumFrom (toEnum 0)

-- BNF: genre = "Action" | "Comedy" | "Drama" | "Horror" | "Romance" | "Sci-Fi" | "Documentary" | "Family"
data Genre = Action | Comedy | Drama | Horror | Romance | SciFi | Documentary | Family
  deriving (Show, Read, Eq, Enum)

genre :: Parser Genre
genre = dataParser $ enumFrom (toEnum 0)

seperator :: Parser Char
seperator = char ','

dataParser :: (Read a, Show a) => [a] -> Parser a
dataParser = foldr (\a -> (<|>) (fmap read (string $ show a))) empty
