{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import AST
import Parser
import Test.Hspec
import Text.Parsec

check :: Parsec String () a -> String -> Either ParseError a
check p = parse p ""

spec_parser :: Spec
spec_parser = do
  describe "identifier" $ do
    it "simple identifier" $
      check identifier "foo" `shouldBe` Right (Identifier "foo")

    it "with hyphens" $
      check identifier "foo-bar" `shouldBe` Right (Identifier "foo-bar")

    it "with numbers" $
      check identifier "foo123" `shouldBe` Right (Identifier "foo123")

    it "parses an identifier with mixed alphanumeric and hyphens" $
      check identifier "foo-bar-123" `shouldBe` Right (Identifier "foo-bar-123")

    it "fails on uppercase first character" $
      check identifier "Foo" `shouldSatisfy` isLeft

    it "fails on empty string" $
      check identifier "" `shouldSatisfy` isLeft

  describe "variable" $ do
    it "simple" $
      check variable "Foo" `shouldBe` Right (Variable "Foo")

    it "with hyphens" $
      check variable "Foo-Bar" `shouldBe` Right (Variable "Foo-Bar")

    it "with numbers" $
      check variable "Foo123" `shouldBe` Right (Variable "Foo123")

    it "with mixed alphanumeric and hyphens" $
      check variable "Foo-Bar-123" `shouldBe` Right (Variable "Foo-Bar-123")

    it "fails on lowercase first character" $
      check variable "foo" `shouldSatisfy` isLeft

    it "fails on empty string" $
      check variable "" `shouldSatisfy` isLeft

  describe "fact" $ do
    it "without arguments" $
      check fact "foo" `shouldBe` Right (Fact (Identifier "foo") [])

    it "empty arguments" $
      check fact "foo()" `shouldBe` Right (Fact (Identifier "foo") [])

    it "one argument" $
      check fact "foo(\"bar\")" `shouldBe` Right (Fact (Identifier "foo") [Value "bar"])

    it "multiple arguments" $
      check fact "foo(\"bar\", \"baz\")"
        `shouldBe` Right (Fact (Identifier "foo") [Value "bar", Value "baz"])

    it "variable placeholder" $
      check fact "foo(X)" `shouldBe` Right (Fact (Identifier "foo") [Named (Variable "X")])

    it "mixed arguments" $
      check fact "foo(X, \"bar\")"
        `shouldBe` Right (Fact (Identifier "foo") [Named (Variable "X"), Value "bar"])

  describe "argument" $ do
    it "string" $
      check argument "\"hello\"" `shouldBe` Right (Value "hello")

    it "string with escaped quotes" $
      check argument "\"hello\\\"world\"" `shouldBe` Right (Value "hello\"world")

    it "variable placeholder" $
      check argument "X" `shouldBe` Right (Named (Variable "X"))

  describe "statement" $ do
    it "cut" $
      check statement "!" `shouldBe` Right Cut

    it "with fact" $
      check statement "foo(\"abc\")" `shouldBe` Right (Search (Fact (Identifier "foo") [Value "abc"]))

    it "without arguments" $
      check statement "foo" `shouldBe` Right (Search (Fact (Identifier "foo") []))

  describe "definition" $ do
    it "without from" $
      check definition "foo." `shouldBe` Right (Fact (Identifier "foo") [] :- [])

    it "fact and arguments" $
      check definition "foo(\"abc\")." `shouldBe` Right (Fact (Identifier "foo") [Value "abc"] :- [])

    it "from" $
      check definition "foo :- bar."
        `shouldBe` Right (Fact (Identifier "foo") [] :- [Search (Fact (Identifier "bar") [])])

    it "multiple from statements" $
      check definition "foo :- bar, baz."
        `shouldBe` Right (Fact (Identifier "foo") [] :- [Search (Fact (Identifier "bar") []), Search (Fact (Identifier "baz") [])])

    it "cut in from" $
      check definition "foo :- !, bar."
        `shouldBe` Right (Fact (Identifier "foo") [] :- [Cut, Search (Fact (Identifier "bar") [])])

  describe "program" $ do
    it "empty program" $
      check program "" `shouldBe` Right (Program [])

    it "one definition" $
      check program "foo." `shouldBe` Right (Program [Fact (Identifier "foo") [] :- []])

    it "multiple definitions" $
      check program "foo.\nbar."
        `shouldBe` Right (Program [Fact (Identifier "foo") [] :- [], Fact (Identifier "bar") [] :- []])

    it "newlines between definitions" $
      check program "foo.\n\nbar."
        `shouldBe` Right (Program [Fact (Identifier "foo") [] :- [], Fact (Identifier "bar") [] :- []])

    it "ignores comments at start" $
      check program "# This is a comment\nfoo."
        `shouldBe` Right (Program [Fact (Identifier "foo") [] :- []])

    it "ignores comments between definitions" $
      check program "foo.\n# A comment\nbar."
        `shouldBe` Right (Program [Fact (Identifier "foo") [] :- [], Fact (Identifier "bar") [] :- []])

    it "ignores multiple comments" $
      check program "# Comment 1\nfoo.\n# Comment 2\n# Comment 3\nbar."
        `shouldBe` Right (Program [Fact (Identifier "foo") [] :- [], Fact (Identifier "bar") [] :- []])

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
