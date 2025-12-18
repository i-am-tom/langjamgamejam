{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import AST
import Control.Applicative (asum)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec

program :: (Stream s m Char) => ParsecT s u m Program
program = do
  _pDefinitions <- many do
    skipMany comment
    definition

  _ <- eof
  pure Program {..}

comment :: (Stream s m Char) => ParsecT s u m ()
comment = do
  _ <- char '#'
  _ <- many (satisfy (/= '\n'))
  _ <- optionMaybe (char '\n')

  pure ()

---

definition :: (Stream s m Char) => ParsecT s u m Definition
definition = spaced do
  this <- fact
  that <- spaced $ asum [string ":-" *> commaSeparated statement, pure []]
  _ <- char '.'

  pure (this :- that)

---

statement :: (Stream s m Char) => ParsecT s u m Statement
statement = spaced do
  asum
    [ char '!' $> Cut,
      string "\\+" *> fmap Negation fact,
      fmap Search fact
    ]

---

fact :: (Stream s m Char) => ParsecT s u m Fact
fact = spaced do
  _fIdentifier <- identifier
  _fArguments <- parenthesised (commaSeparated argument) <|> pure []

  pure Fact {..}

---

argument :: (Stream s m Char) => ParsecT s u m Argument
argument = asum [fmap Value (text <* spaces), fmap Named variable]
  where
    text :: (Stream s m Char) => ParsecT s u m Text
    text = fmap Text.pack (between (char '"') (char '"') (many stringChar))

    stringChar :: (Stream s m Char) => ParsecT s u m Char
    stringChar = (char '\\' *> char '"') <|> satisfy (/= '"')

---

identifier :: (Stream s m Char) => ParsecT s u m Identifier
identifier = do
  h <- lower
  t <- many (alphaNum <|> char '-')

  let name :: Text
      name = Text.pack (h : t)

  pure (Identifier name)

---

variable :: (Stream s m Char) => ParsecT s u m Variable
variable = do
  h <- upper
  t <- many (alphaNum <|> char '-')

  let name :: Text
      name = Text.pack (h : t)

  pure (Variable name)

---

parenthesised :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
parenthesised = between (char '(') (char ')') . spaced

commaSeparated :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [a]
commaSeparated p = sepBy p (spaced (char ','))

spaced :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
spaced = between spaces spaces
