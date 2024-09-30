{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = AddMovie String | Show

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _ = False

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery = parseQuery' . splitQuery

parseQuery' :: [String] -> Either String Query
parseQuery' ("show" : _) = Right Show
parseQuery' ["add"] = Left "No movie name provided"
parseQuery' ["add", str] = Right (AddMovie str)
parseQuery' ("add" : str : _) = Left "Too many args"
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
data State = VHSRentalStore Catalog

instance Show State where
  show (VHSRentalStore c) = "VHS Rental store:\n  " ++ show c

data Catalog = Catalog MovieList

instance Show Catalog where
  show (Catalog ml) = "Catalog (" ++ show (listLength ml) ++ "):\n    " ++ show ml

data MovieList = Empty | Movie Title | Movies Title MovieList

newtype Title = Title String

instance Show Title where
  show (Title s) = '"' : s ++ "\""

instance Show MovieList where
  show (Movie s) = show s
  show (Movies s Empty) = show s
  show (Movies s ml) = show s ++ ", " ++ show ml
  show Empty = ""

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = VHSRentalStore (Catalog Empty)

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (VHSRentalStore (Catalog ml)) (AddMovie n)
  | null n = Left "Movie name cannot be empty"
  | otherwise = Right (Just "Successfully added", VHSRentalStore (Catalog (appendToList n ml)))
stateTransition c Show = Right (Just (show c), c)
stateTransition _ _ = Left "Not implemented 3"

listLength :: MovieList -> Int
listLength Empty = 0
listLength (Movie _) = 1
listLength (Movies _ xs) = 1 + listLength xs

appendToList :: String -> MovieList -> MovieList
appendToList s Empty = Movie (Title s)
appendToList ns (Movie os) = Movies os (Movie (Title ns))
appendToList ns (Movies os ml) = Movies os (appendToList ns ml)