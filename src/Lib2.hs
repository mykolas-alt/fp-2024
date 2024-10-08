{-# LANGUAGE InstanceSigs #-}

module Lib2
  ( Query (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Parsers
import ParsingHelpers
import PrimitiveParsers

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
-- BNF: root = "init " rental_store | "takeMovie " movie | "removeMovie " movie | "addMovie " | "show"
data Query = Init RentalStore | Show

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _ = False

instance Show Query where
  show (Init rs) = "init " ++ show rs
  show Show = "show"

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery s =
  case orX [parseShow, parseInit] s of
    Left e -> Left e
    Right (q, r) ->
      if null r then Right q else Left ("Unrecognized characters: " ++ r)

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = Uninitialized | Store RentalStore
  deriving (Show)

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
stateTransition Uninitialized (Init rs) = Right (Just "Successfully initialized", Store rs)
stateTransition _ (Init _) = Left "State is already initialized"

parseShow :: Parser Query
parseShow s =
  case parseString "show" s of
    Left e -> Left e
    Right (_, r) -> Right (Show, r)

parseInit :: Parser Query
parseInit s =
  case and2 (parseString "init ") parseRentalStore s of
    Left e -> Left e
    Right ((_, rs), r) -> Right (Init rs, r)