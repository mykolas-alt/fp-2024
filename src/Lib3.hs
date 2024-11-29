{-# LANGUAGE InstanceSigs #-}

module Lib3
  ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements (..),
  )
where

import Control.Applicative (Alternative (many), (<|>))
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Data.Maybe (fromJust, isNothing)
import qualified Lib2
import qualified Parsers as P
import PrimitiveParsers
import System.Directory (doesFileExist)

data StorageOp = Save String (Chan ()) | Load (Chan (Maybe String))

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save s chan -> do
      writeFile fileName s
      writeChan chan ()
    Load chan -> do
      exists <- doesFileExist fileName
      if exists
        then do
          s' <- readFile fileName
          writeChan chan $ Just s'
        else writeChan chan Nothing

fileName :: String
fileName = "state.txt"

data Statements
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = show q
  show (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . show) qs ++ "END\n"

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand str = case parse command str of
  (Left err, _) -> Left err
  (Right cmd, r) -> Right (cmd, r)

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements str = case parse statements str of
  (Left err, _) -> Left err
  (Right s, r) -> Right (s, r)

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState Lib2.Uninitialized = Single Lib2.Uninit
marshallState (Lib2.Store (P.VHSRentalStore (P.Catalog ml))) = Batch [Lib2.Uninit, Lib2.Init $ P.VHSRentalStore $ P.Catalog ml]

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file.
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements = show

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition ::
  TVar Lib2.State ->
  Command ->
  Chan StorageOp ->
  IO (Either String (Maybe String))
stateTransition s SaveCommand ioChan = do
  s' <- readTVarIO s
  chan <- newChan :: IO (Chan ())
  writeChan ioChan (Save (renderStatements $ marshallState s') chan)
  readChan chan
  return $ Right $ Just "State saved successfully"
stateTransition s LoadCommand ioChan = do
  chan <- newChan :: IO (Chan (Maybe String))
  writeChan ioChan (Load chan)
  qs <- readChan chan
  if isNothing qs
    then return (Left "No state file found")
    else case parseStatements $ fromJust qs of
      Left e -> do
        return $ Left $ "Failed to load from file:\n" ++ e
      Right (qs', _) -> stateTransition s (StatementCommand qs') ioChan
stateTransition s (StatementCommand sts) _ = atomically $ atomicStatemets s sts

transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList _ [] = Left "Empty query list"
transitionThroughList s (q : qs) = case Lib2.stateTransition s q of
  Left e -> Left e
  Right (msg, ns) ->
    if null qs
      then Right (msg, ns)
      else case transitionThroughList ns qs of
        Left e -> Left e
        Right (msg', ns') -> Right ((\x y -> x ++ "\n" ++ y) <$> msg <*> msg', ns')

atomicStatemets :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatemets s (Batch qs) = do
  s' <- readTVar s
  case transitionThroughList s' qs of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
atomicStatemets s (Single q) = do
  s' <- readTVar s
  case Lib2.stateTransition s' q of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg

statements :: Parser Statements
statements =
  ( do
      _ <- string "BEGIN\n"
      qs <-
        many
          ( do
              q <- Lib2.query
              _ <- string ";\n"
              return q
          )
      _ <- string "END\n"
      return $ Batch qs
  )
    <|> (Single <$> Lib2.query)

loadParser :: Parser Command
loadParser = do
  _ <- string "load"
  return LoadCommand

saveParser :: Parser Command
saveParser = do
  _ <- string "save"
  return SaveCommand

command :: Parser Command
command = StatementCommand <$> statements <|> loadParser <|> saveParser
