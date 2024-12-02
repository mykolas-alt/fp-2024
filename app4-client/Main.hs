{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Concurrent (Chan, forkIO, killThread, newChan)
import Control.Lens
import Control.Monad.Free (Free (..), liftF)
import Data.ByteString
import Data.Maybe (fromMaybe)
import Data.String.Conversions
import GHC.Conc (TVar, newTVar, newTVarIO, readTVar, readTVarIO)
import qualified Lib2
import qualified Lib3
import Network.Wreq
import Parsers (Availability (Available), Catalog (Catalog), Genre (Action, Comedy, Documentary, Horror), Movie (Movie), MovieList (List, Single), Rating (NR, PG, PG13, R), RentalStore (VHSRentalStore))

-- BNF: root = "init " rental_store | "addMovies" movie_list | "addMovie " movie | "takeMovie " movie | "removeMovie " movie | "show"
-- data Query = Init RentalStore | AddMovies MovieList | TakeMovie Movie | RemoveMovie Movie | AddMovie Movie | Show | Uninit
data MyDomainAlgebra a
  = Init RentalStore a
  | AddMovies MovieList a
  | TakeMovie Movie a
  | RemoveMovie Movie a
  | AddMovie Movie a
  | Show (String -> a)
  | Uninit a
  | Save a
  | Load a
  deriving (Functor)

type VHSRentalProgram = Free MyDomainAlgebra

initializeRentalStore :: RentalStore -> VHSRentalProgram ()
initializeRentalStore store = liftF $ Init store ()

addMoviesToStore :: MovieList -> VHSRentalProgram ()
addMoviesToStore movies = liftF $ AddMovies movies ()

addMovieToStore :: Movie -> VHSRentalProgram ()
addMovieToStore movie = liftF $ AddMovie movie ()

rentMovie :: Movie -> VHSRentalProgram ()
rentMovie movie = liftF $ TakeMovie movie ()

showAvailableMovies :: VHSRentalProgram String
showAvailableMovies = liftF $ Show id

uninitRentalStore :: VHSRentalProgram ()
uninitRentalStore = liftF $ Uninit ()

save :: VHSRentalProgram ()
save = liftF $ Save ()

load :: VHSRentalProgram ()
load = liftF $ Load ()

interpretorSingleRequest :: VHSRentalProgram a -> IO a
interpretorSingleRequest (Pure a) = return a
interpretorSingleRequest (Free step) = do
  next <- runStep step
  interpretorSingleRequest next
  where
    runStep :: MyDomainAlgebra a -> IO a
    runStep (Init store next) = sendSingleStatement (Lib2.Init store) >> return next
    runStep (AddMovies ml next) = sendSingleStatement (Lib2.AddMovies ml) >> return next
    runStep (TakeMovie m next) = sendSingleStatement (Lib2.TakeMovie m) >> return next
    runStep (RemoveMovie m next) = sendSingleStatement (Lib2.RemoveMovie m) >> return next
    runStep (AddMovie m next) = sendSingleStatement (Lib2.AddMovie m) >> return next
    runStep (Show next) = do
      str <- sendSingleStatement Lib2.Show
      return $ next str
    runStep (Uninit next) = sendSingleStatement Lib2.Uninit >> return next
    runStep (Save next) = postAsString "save" >> return next
    runStep (Load next) = postAsString "load" >> return next

interpretWithBatching' :: VHSRentalProgram a -> [Lib2.Query] -> IO a
interpretWithBatching' (Pure a) batch = dumpBatch batch >> return a
interpretWithBatching' (Free step) batch = do
  case step of
    Init store next -> interpretWithBatching' next $ batch ++ [Lib2.Init store]
    AddMovies ml next -> interpretWithBatching' next $ batch ++ [Lib2.AddMovies ml]
    TakeMovie m next -> interpretWithBatching' next $ batch ++ [Lib2.TakeMovie m]
    RemoveMovie m next -> interpretWithBatching' next $ batch ++ [Lib2.RemoveMovie m]
    AddMovie m next -> interpretWithBatching' next $ batch ++ [Lib2.AddMovie m]
    Uninit next -> interpretWithBatching' next $ batch ++ [Lib2.Uninit]
    Show next -> do
      _ <- dumpBatch batch
      str <- sendSingleStatement Lib2.Show
      interpretWithBatching' (next str) []
    Save next -> dumpBatch batch >> postAsString "save" >> interpretWithBatching' next []
    Load next -> dumpBatch batch >> postAsString "load" >> interpretWithBatching' next []

interpretWithBatching :: VHSRentalProgram a -> IO a
interpretWithBatching prog = interpretWithBatching' prog []

dumpBatch :: [Lib2.Query] -> IO (Maybe String)
dumpBatch [] = return Nothing
dumpBatch [single] = Just <$> sendSingleStatement single
dumpBatch batch = Just <$> sendAsBatch batch

sendAsBatch :: [Lib2.Query] -> IO String
sendAsBatch = postAsString . Lib3.renderStatements . Lib3.Batch

sendSingleStatement :: Lib2.Query -> IO String
sendSingleStatement = postAsString . Lib3.renderStatements . Lib3.Single

postAsString :: String -> IO String
postAsString s = do
  let rawRequest = cs s :: ByteString
  putStrLn $ "Sending request:\n" ++ cs rawRequest
  resp <- post "http://localhost:3000" rawRequest
  return $ cs $ resp ^. responseBody

testInterpretator :: VHSRentalProgram a -> IO a
testInterpretator p = do
  state <- newTVarIO Lib2.emptyState
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  initialState <- readTVarIO state
  putStrLn $ "Initial state:\n" ++ show initialState ++ "\n"
  _ <- forkIO $ Lib3.storageOpLoop chan
  testInterpretator' state chan p
  where
    testInterpretator' :: TVar Lib2.State -> Chan Lib3.StorageOp -> VHSRentalProgram a -> IO a
    testInterpretator' _ _ (Pure a) = return a
    testInterpretator' state chan (Free step) = do
      next <- runStep state chan step
      putStrLn "State after:"
      newState <- readTVarIO state
      putStrLn $ show newState ++ "\n"
      testInterpretator' state chan next
    runStep :: TVar Lib2.State -> Chan Lib3.StorageOp -> MyDomainAlgebra a -> IO a
    runStep state chan (Init store next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.Init store) chan >> return next
    runStep state chan (AddMovies ml next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddMovies ml) chan >> return next
    runStep state chan (TakeMovie m next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.TakeMovie m) chan >> return next
    runStep state chan (RemoveMovie m next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.RemoveMovie m) chan >> return next
    runStep state chan (AddMovie m next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddMovie m) chan >> return next
    runStep state chan (Show next) = do
      str <- transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single Lib2.Show) chan
      return $ next str
    runStep state chan (Uninit next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single Lib2.Uninit) chan >> return next
    runStep state chan (Save next) = transitionAndPrint state Lib3.SaveCommand chan >> return next
    runStep state chan (Load next) = transitionAndPrint state Lib3.LoadCommand chan >> return next

transitionAndPrint :: TVar Lib2.State -> Lib3.Command -> Chan Lib3.StorageOp -> IO String
transitionAndPrint state cmd chan = do
  putStrLn $ "Command:\n" ++ show cmd
  res <- Lib3.stateTransition state cmd chan
  let str = either id (fromMaybe "Success") res
  putStrLn "Result:"
  putStrLn str
  return str

process :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
process state storageChan input = case Lib3.parseCommand input of
  Left e -> return e
  Right (cmd, "") -> do
    info <- Lib3.stateTransition state cmd storageChan
    case info of
      Left e -> return e
      Right mb -> return $ fromMaybe "Success" mb
  Right (_, str) -> return $ "Could not parse: " ++ str

--------------------------------------------------------

program :: VHSRentalProgram (String, String)
program = do
  initializeRentalStore $ VHSRentalStore $ Catalog $ Single movie1
  save
  addMovieToStore $ Movie "KitasPavadinimas" 1950 Horror R Available
  rentMovie movie1
  b <- showAvailableMovies
  addMoviesToStore movies
  rentMovie movie2
  load
  a <- showAvailableMovies
  uninitRentalStore
  return (b, a)
  where
    movie1 = Movie "Pavadinimas" 1935 Action PG13 Available
    movie2 = Movie "PuikusFilmas" 1978 Documentary NR Available
    movies = Parsers.List movie2 $ Single (Movie "PrastasFilmas" 2021 Comedy PG Available)

main :: IO ()
main = do
  str <- interpretorSingleRequest program
  -- str <- interpretWithBatching program
  -- str <- testInterpretator program
  print str
