all:
	cabal configure && cabal build && cabal test && cabal install
