SRCDIR=../..

SRC=backquot.lsp cmpmacro.lsp convert.lsp cells.lsp simplify.lsp lift.lsp \
gencode.lsp peephole.lsp assemble.lsp cmpfront.lsp

FSL=backquot.fsl cmpmacro.fsl convert.fsl cells.fsl simplify.fsl lift.fsl \
gencode.fsl peephole.fsl assemble.fsl cmpfront.fsl

CMPLSP=../cmplsp/common.lsp ../cmplsp/conditns.lsp ../cmplsp/loadfsl.lsp

all: $(FSL)

$(CMPLSP):
	(cd ../cmplsp; $(MAKE) common.lsp conditns.lsp loadfsl.lsp)

$(FSL): $(SRC) $(CMPLSP)

.SUFFIXES: .lsp .fsl

.lsp.fsl:
	echo "(compile-file \"$<\") (exit)" \
	  | ../sources/xlisp.bin ../sources/cmpload

# this is used if compiling in a subdirectory of the xlispstat tree
$(SRC):
	ln -s $(SRCDIR)/compiler/$@ $@

# this is used for creating the separate xlisp only distribution
srcfiles:
	(cd $(SRCDIR)/compiler; tar cf - $(SRC) README) | tar xf -

clean:
	rm -f *.fsl
