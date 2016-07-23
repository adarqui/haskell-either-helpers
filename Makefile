build:
	stack build --fast

clean:
	stack clean

build-watch:
	stack build --fast --file-watch

docs:
	cabal haddock --hyperlink-source

tests:
	stack test --fast

ghci:
	stack ghci haskell-either-helpers
