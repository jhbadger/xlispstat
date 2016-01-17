BCDIR = \BC5
BCLIBDIR = $(BCDIR)\LIB
INCLUDES = $(BCDIR)\INCLUDE;..
FLAGS = -I$(INCLUDES) -L$(BCLIBDIR)

xlsclient.exe: xlsclient.c
	bcc32 $(FLAGS) xlsclient.c

clean:
	del *.obj
	del *.res

veryclean: clean
	del *.exe
	del "*.#*"
	del *.csm
