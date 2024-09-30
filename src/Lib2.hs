{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Data.Maybe (fromJust, isNothing)

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = AddMovie Movie | Show

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _ = False

instance Show Query where
  show (AddMovie m) = "AddMovie " ++ show m
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery = parseQuery' . splitQuery

parseQuery' :: [String] -> Either String Query
parseQuery' ("show" : _) = Right Show
parseQuery' ("AddMovie" : args) = parseAddMovie args
parseQuery' xs = Left ("blogai blogai... " ++ show xs)

splitQuery :: String -> [String]
splitQuery s
  | null str = []
  | head str == '"' = takeWhile (/= '"') (tail str) : splitQuery (dropWhitespace (tail (dropWhile (/= '"') (tail str))))
  | otherwise = takeWhile (not . whitespace) str : splitQuery (dropWhile (not . whitespace) str)
  where
    str = dropWhitespace s

dropWhitespace :: String -> String
dropWhitespace = dropWhile whitespace

whitespace :: Char -> Bool
whitespace c = c `elem` [' ', '\t', '\n', '\v', '\r', '\f']

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = VHSRentalStore Catalog CustomerDatabase RentalTransactions

instance Show State where
  show (VHSRentalStore c cdb rt) = "VHS Rental store:\n  " ++ show c

newtype Catalog = Catalog MovieList

type MovieList = List Movie

data Movie = Movie Title Year Genre Rating Availability

instance Show Movie where
  show (Movie t y g r a) = concat ["Title: ", show t, ", Year: ", show y, ", Genre: ", show g, ", Rating: ", show r, ", Availability: ", show a]

type Year = Int

data Genre = Action | Comedy | Drama | Horror | Romance | SciFi | Documentary | Family
  deriving (Show)

allGenres :: [Genre]
allGenres = [Action, Comedy, Drama, Horror, Romance, SciFi, Documentary, Family]

data Rating = G | PG | PG13 | R | NR
  deriving (Show)

allRatings :: [Rating]
allRatings = [G, PG, PG13, R, NR]

data Availability = Available | Rented
  deriving (Show)

allAvailabilities :: [Availability]
allAvailabilities = [Available, Rented]

newtype CustomerDatabase = CustomerDatabase CustomerList

type CustomerList = List String

newtype RentalTransactions = RentalTransactions TransactionList

type TransactionList = List String

instance Show Catalog where
  show (Catalog Empty) = "Catalog is empty"
  show (Catalog ml) = "Catalog (" ++ show (listLength ml) ++ "):\n    " ++ show ml

data List a = Empty | Item a | Items a (List a)

newtype Title = Title String

instance Show Title where
  show (Title s) = '"' : s ++ "\""

instance (Show a) => Show (List a) where
  show (Item i) = show i
  show (Items i Empty) = show i
  show (Items i il) = show i ++ ", " ++ show il
  show Empty = ""

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = VHSRentalStore (Catalog Empty) (CustomerDatabase Empty) (RentalTransactions Empty)

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (VHSRentalStore (Catalog ml) cd rt) (AddMovie m) = Right (Just "Successfully added.", ns)
  where
    ns = VHSRentalStore (Catalog (appendToList m ml)) cd rt
stateTransition c Show = Right (Just (show c), c)
stateTransition _ q = Left ("Not implemented 3" ++ show q)

listLength :: List a -> Int
listLength Empty = 0
listLength (Item _) = 1
listLength (Items _ xs) = 1 + listLength xs

appendToList :: a -> List a -> List a
appendToList i Empty = Item i
appendToList i1 (Item i2) = Items i2 (Item i1)
appendToList i1 (Items i2 il) = Items i2 (appendToList i1 il)

parseAddMovie :: [String] -> Either String Query
parseAddMovie [t, y, g, r, a]
  | null t = Left "Invalid title"
  | isNothing y' = Left "Invalid Year. Format: YYYY"
  | isNothing g' = Left ("Invalid Genre. Choices: " ++ show allGenres)
  | isNothing r' = Left ("Invalid Rating. Choices: " ++ show allRatings)
  | isNothing a' = Left ("Invalid Availability. Choices: " ++ show allAvailabilities)
  | otherwise = Right (AddMovie movie)
  where
    y' = parseYear y
    g' = parseGenre g
    r' = parseRating r
    a' = parseAvailability a
    movie = Movie (Title t) (fromJust y') (fromJust g') (fromJust r') (fromJust a')
parseAddMovie _ = Left "Incorrect usage. Use: AddMovie Title Year Genre Rating Availability"

parseYear :: String -> Maybe Year
parseYear s
  | isYear s = Just (foldl (\x y -> 10 * x + parseDigit y) 0 s)
  | otherwise = Nothing

parseToData :: (Show a) => [a] -> String -> Maybe a
parseToData [] _ = Nothing
parseToData (x : xs) s
  | show x == s = Just x
  | otherwise = parseToData xs s

parseRating :: String -> Maybe Rating
parseRating = parseToData allRatings

parseAvailability :: String -> Maybe Availability
parseAvailability = parseToData allAvailabilities

parseGenre :: String -> Maybe Genre
parseGenre = parseToData allGenres

isYear :: String -> Bool
isYear s = length s == 4 && all isDigit s

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

type Digit = Int

parseDigit :: Char -> Digit
parseDigit c = fromEnum c - fromEnum '0'
