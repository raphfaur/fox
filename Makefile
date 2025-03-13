
BINARY=fox
MAKEDIR=mkdir
RMDIR=rm -rf
RENAME=mv
GHC=ghc
CABAL=cabal


compile: $(dirs)
	$(CABAL) install --overwrite-policy=always --install-method=copy --installdir .
	$(RENAME) fox $(BINARY)

.PHONY: clean

clean:
	$(RMDIR) $(BINARY)
