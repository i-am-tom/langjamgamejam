default:
  @just --list

build:
  cabal update
  cabal build

test:
  cabal test

format:
  ormolu -i **/*.hs

format_check:
  ormolu -c **/*.hs

lint:
  hlint source tests

clean:
  cabal clean

all: format lint build test
ci: format_check lint build test