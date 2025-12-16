{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module InterpreterSpec where

import AST
import Data.Aeson qualified as JSON
import Interpreter (Solution, query)
import Parser
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Text.Parsec (parse)

spec_interpreter :: Spec
spec_interpreter = do
  describe "passing" do
    let passing :: FilePath
        passing = "tests" </> "passing"

    runIO (listDirectory passing) >>= mapM_ \directory -> do
      let test :: FilePath
          test = passing </> directory

      it directory do
        source <- loadProgram (test </> "source.prolog")
        question <- loadQuery (test </> "query.prolog")
        expected <- loadSolutions (test </> "output.json")

        query source question `shouldBe` expected

  describe "failing" do
    let failing :: FilePath
        failing = "tests" </> "failing"

    runIO (listDirectory failing) >>= mapM_ \directory -> do
      let test :: FilePath
          test = failing </> directory

      it directory do
        source <- loadProgram (test </> "source.prolog")
        question <- loadQuery (test </> "query.prolog")

        query source question `shouldBe` []

---

loadProgram :: FilePath -> IO Program
loadProgram path = do
  content <- readFile path

  case parse program path content of
    Left message -> error ("parser: " ++ show message)
    Right source -> pure source

loadQuery :: FilePath -> IO Fact
loadQuery path = do
  content <- readFile path

  case parse fact path content of
    Left message -> error (show message)
    Right question -> pure question

loadSolutions :: FilePath -> IO [Solution]
loadSolutions path = do
  JSON.decodeFileStrict path >>= \case
    Just solutions -> pure solutions
    Nothing -> error ("Failed to decode JSON from " ++ path)