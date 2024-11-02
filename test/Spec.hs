{-# LANGUAGE ImportQualifiedPost #-}

import Data.Either (Either (Right))
import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Parsers qualified as P
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lib1UnitTests, lib2UnitTests, lib3UnitTests]

lib1UnitTests :: TestTree
lib1UnitTests =
  testGroup
    "Lib1 tests"
    [testCase "List of completions is not empty" $ null Lib1.completions @?= False]

lib2UnitTests :: TestTree
lib2UnitTests =
  testGroup
    "Lib2 tests"
    [ testCase "Bad query" $
        let q = "addMovie FunkcinisProgramavimas,2024,NeegzistuojantisZanras,PG13,Available"
         in Lib2.parseQuery q @?= Left "Could not recognize: addMovie FunkcinisProgramavimas,2024,NeegzistuojantisZanras,PG13,Available",
      testCase "Successfully parsing addMovies" $
        let m1str = "FunkcinisProgramavimas,2024,Horror,PG13,Available"
            m2str = "Rambo,1982,Action,PG,Available"
            qstr = m1str ++ "," ++ m2str
            m1 = P.Movie "FunkcinisProgramavimas" 2024 P.Horror P.PG13 P.Available
            m2 = P.Movie "Rambo" 1982 P.Action P.PG P.Available
            ml = P.List m1 (P.Single m2)
         in Lib2.parseQuery ("addMovies " ++ qstr) @?= Right (Lib2.AddMovies ml),
      testCase "Successfully parsing addMovie" $
        Lib2.parseQuery "addMovie FunkcinisProgramavimas,2024,Horror,PG13,Available"
          @?= Right (Lib2.AddMovie (P.Movie "FunkcinisProgramavimas" 2024 P.Horror P.PG13 P.Available)),
      testCase "Successfully parsing init" $
        let q = "init VHS Rental Store:Catalog:Pavadinimas,1999,Action,PG,Available"
            r = Right (Lib2.Init (P.VHSRentalStore (P.Catalog (P.Single (P.Movie "Pavadinimas" 1999 P.Action P.PG P.Available)))))
         in Lib2.parseQuery q @?= r,
      testCase "Init state from Uninitialized" $
        let s = Lib2.Uninitialized
            q = Lib2.Init (P.VHSRentalStore (P.Catalog (P.Single (P.Movie "Pavadinimas" 1999 P.Action P.PG P.Available))))
            ns = Lib2.Store (P.VHSRentalStore (P.Catalog (P.Single (P.Movie "Pavadinimas" 1999 P.Action P.PG P.Available))))
         in Lib2.stateTransition s q @?= Right (Just "Successfully initialized", ns),
      testCase "Add movie to initialized state" $
        let m1 = P.Movie "Pavadinimas" 1999 P.Action P.PG P.Available
            m2 = P.Movie "KitasPavadinimas" 1999 P.Action P.PG P.Available
            s = Lib2.Store (P.VHSRentalStore (P.Catalog (P.Single m1)))
            q = Lib2.AddMovie m2
            ml = P.List m2 (P.Single m1)
            ns = Lib2.Store (P.VHSRentalStore (P.Catalog ml))
         in Lib2.stateTransition s q @?= Right (Just "Success", ns),
      testCase "Add movie to uninitialized state" $
        let m1 = P.Movie "Pavadinimas" 1999 P.Action P.PG P.Available
            s = Lib2.Uninitialized
            q = Lib2.AddMovie m1
         in Lib2.stateTransition s q @?= Left "State has to be initialized to add a movie"
    ]

-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
lib3UnitTests :: TestTree
lib3UnitTests =
  testGroup
    "Lib3 tests"
    [ testCase "Property test single 1" $
        let m1 = P.Movie "Pavadinimas" 1999 P.Action P.PG P.Available
            m2 = P.Movie "KitasPavadinimas" 1999 P.Action P.PG P.Available
            ml = P.List m2 (P.Single m1)
            s = Lib3.Single $ Lib2.Init $ P.VHSRentalStore $ P.Catalog ml
         in Lib3.parseStatements (Lib3.renderStatements s) @?= Right (s, ""),
      testCase "Property test single 2" $
        let s = Lib3.Single Lib2.Uninit
         in Lib3.parseStatements (Lib3.renderStatements s) @?= Right (s, ""),
      testCase "Property test batch" $
        let m1 = P.Movie "Pavadinimas" 1999 P.Action P.PG P.Available
            m2 = P.Movie "KitasPavadinimas" 1999 P.Action P.PG P.Available
            m3 = P.Movie "Trecias" 2020 P.Horror P.R P.Rented
            ml = P.List m2 (P.Single m1)
            s = Lib3.Batch [Lib2.Uninit, Lib2.Init $ P.VHSRentalStore $ P.Catalog ml, Lib2.AddMovies $ P.Single m3]
         in Lib3.parseStatements (Lib3.renderStatements s) @?= Right (s, "")
    ]
