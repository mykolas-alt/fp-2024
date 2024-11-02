{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    parseQuery,
    query,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Parsers
import PrimitiveParsers

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
-- BNF: root = "init " rental_store | "addMovies" movie_list | "addMovie " movie | "takeMovie " movie | "removeMovie " movie | "show"
data Query = Init RentalStore | AddMovies MovieList | TakeMovie Movie | RemoveMovie Movie | AddMovie Movie | Show | Uninit

-- | The instances are needed basically for tests
instance Eq Query where
  (==) q1 q2 = show q1 == show q2

instance Show Query where
  show (Init rs) = "init " ++ show rs
  show Show = "show"
  show Uninit = "uninit"
  show (TakeMovie m) = "takeMovie " ++ show m
  show (RemoveMovie m) = "removeMovie " ++ show m
  show (AddMovie m) = "addMovie " ++ show m
  show (AddMovies ml) = "addMovie " ++ show ml

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery s =
  case parse query s of
    Left e -> Left e
    Right (q, r) -> if null r then Right q else Left ("Unrecognized characters:" ++ r)

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = Uninitialized | Store RentalStore
  deriving (Show, Eq)

data List a = Empty | Item a | Items a (List a)

instance (Show a) => Show (List a) where
  show (Item i) = show i
  show (Items i Empty) = show i
  show (Items i il) = show i ++ ", " ++ show il
  show Empty = ""

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = Uninitialized

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition s Show = Right (Just (show s), s)
stateTransition _ Uninit = Right (Just "Uninitializing state", Uninitialized)
stateTransition Uninitialized (Init rs) = Right (Just "Successfully initialized", Store rs)
stateTransition _ (Init _) = Left "State is already initialized"
stateTransition s (AddMovie m) = addMovie s m
stateTransition s (AddMovies ml) = addMovies s ml
stateTransition s (RemoveMovie m) = removeMovie s m
stateTransition s (TakeMovie m) = takeMovie s m

takeMovie :: State -> Movie -> Either String (Maybe String, State)
takeMovie Uninitialized _ = Left "State has to be initialized to take a movie"
takeMovie (Store (VHSRentalStore (Catalog ml))) m =
  case takeFromList m ml of
    Left err -> Left err
    Right (msg, nml) -> Right (msg, Store (VHSRentalStore (Catalog nml)))
  where
    markTaken (Movie x1 x2 x3 x4 _) = Movie x1 x2 x3 x4 Rented
    takeFromList toTk (Single m1)
      | toTk == m1 = Right (Just "Successfully updated", Single (markTaken m1))
      | otherwise = Left ("Could not find movie: " ++ show m1)
    takeFromList toTk (List m1 ml1)
      | toTk == m1 = Right (Just "Successfully updated", List (markTaken m1) ml1)
      | otherwise = case takeFromList toTk ml1 of
          Left e -> Left e
          Right (msg, ml2) -> Right (msg, List m1 ml2)

addMovie :: State -> Movie -> Either String (Maybe String, State)
addMovie Uninitialized _ = Left "State has to be initialized to add a movie"
addMovie (Store (VHSRentalStore (Catalog ml))) m = Right (Just "Success", ns)
  where
    ns = Store (VHSRentalStore (Catalog (List m ml)))

addMovies :: State -> MovieList -> Either String (Maybe String, State)
addMovies s (Single m) = addMovie s m
addMovies s (List m ml) =
  case addMovie s m of
    Left e -> Left e
    Right (_, ns) -> addMovies ns ml

removeMovie :: State -> Movie -> Either String (Maybe String, State)
removeMovie Uninitialized _ = Left "State has to be initialized to remove a movie"
removeMovie (Store (VHSRentalStore (Catalog ml))) m =
  case removeFromList m ml of
    Left e -> Left e
    Right nml ->
      if nml == ml
        then Left ("Movie " ++ show m ++ " was not found in the catalog")
        else Right (Just "Successfully removed", Store (VHSRentalStore (Catalog nml)))

-- Negaliu palikti tuscio pagal gramatika
removeFromList :: Movie -> MovieList -> Either String MovieList
removeFromList toRm s@(Single m)
  | m == toRm = Left "Can't leave list empty"
  | otherwise = Right s
removeFromList toRm (List m ml)
  | m == toRm = Right ml
  | otherwise = case removeFromList toRm ml of
      Left _ -> Right (Single m)
      Right ml2 -> Right (List m ml2)

uninitParser :: Parser Query
uninitParser = do
  _ <- string "uninit"
  return Uninit

showParser :: Parser Query
showParser = do
  _ <- string "show"
  return Show

initParser :: Parser Query
initParser = do
  _ <- string "init "
  Init <$> rentalStore

addMovieParser :: Parser Query
addMovieParser = do
  _ <- string "addMovie "
  AddMovie <$> movie

addMoviesParser :: Parser Query
addMoviesParser = do
  _ <- string "addMovies "
  AddMovies <$> movieList

takeMovieParser :: Parser Query
takeMovieParser = do
  _ <- string "takeMovie "
  TakeMovie <$> movie

removeMovieParser :: Parser Query
removeMovieParser = do
  _ <- string "removeMovie "
  RemoveMovie <$> movie

query :: Parser Query
query =
  showParser
    <|> initParser
    <|> uninitParser
    <|> addMovieParser
    <|> addMoviesParser
    <|> removeMovieParser
    <|> takeMovieParser
