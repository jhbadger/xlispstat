SRCDIR=../..

SRC=common.lsp common2.lsp common3.lsp conditns.lsp loadfsl.lsp pathname.lsp \
    stepper.lsp shlib.lsp

FSL=common.fsl common2.fsl common3.fsl conditns.fsl loadfsl.fsl pathname.fsl \
    stepper.fsl shlib.fsl

.SUFFIXES: .lsp .fsl

.lsp.fsl:
	echo "(compile-file \"$<\") (exit)" \
	  | ../sources/xlisp.bin ../sources/cmpload

all: $(FSL)

$(FSL): $(SRC)

# this is used if compiling in a subdirectory of the xlispstat tree
$(SRC):
	ln -s $(SRCDIR)/$@ $@

# this is used for creating the separate xlisp only distribution
srcfiles:
	(cd $(SRCDIR); tar cf - $(SRC)) | tar xf -

clean:
	rm -f *.fsl
