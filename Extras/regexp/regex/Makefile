CC=gcc
CFLAGS=-g -I. -DPOSIX_MISTAKE
AR=ar
RANLIB=ranlib

SRCS=	regcomp.c regerror.c regexec.c regfree.c

OBJ=	regcomp.o regerror.o regexec.o regfree.o

LIB=	libregex.a

$(LIB):	$(OBJ)
	$(AR) cr $(LIB) $(OBJ)
	$(RANLIB) $(LIB)

clean:
	rm -f *.o *.a
