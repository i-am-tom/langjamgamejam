default:
  @just --list

build:
  cabal update
  cabal build

test:
  cabal test --enable-tests

format:
  ormolu -i **/*.hs

format_check:
  ormolu -c **/*.hs

lint:
  hlint language tests

clean:
  cabal clean

all: format lint build test
ci: format_check lint build test