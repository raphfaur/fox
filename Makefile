TMPDIR=objects
SRC=src

BINARY=fox
MAKEDIR=mkdir
RMDIR=rm -rf
GHC=ghc

dirs := $(TMPDIR)

compile: $(dirs)
	$(GHC) --make -i$(SRC) -odir $(TMPDIR) -hidir $(TMPDIR) -o $(BINARY) $(SRC)/main.hs

$(dirs):
	$(MAKEDIR) $@

.PHONY: clean

clean:
	$(RMDIR) objects
	$(RMDIR) $(BINARY)
