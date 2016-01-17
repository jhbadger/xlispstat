BCDIR = \BC5
BCLIBDIR = $(BCDIR)\LIB
INCLUDES = $(BCDIR)\INCLUDE;..
DEFINES = STRICT;NOTTY;
CFLAGS = -v -w -W -I$(INCLUDES) -D$(DEFINES) -H -H=lspedit.csm
RFLAGS = -I$(INCLUDES) -D$(DEFINES)
LDFLAGS32 = -L$(BCLIBDIR) -Tpe -aa -c -x $(BCLIBDIR)\c0w32.obj
LDFLAGS16 = -L$(BCLIBDIR) -Twe -c -C -x $(BCLIBDIR)\c0wl.obj
SYSLIBS32 = $(BCLIBDIR)\import32.lib $(BCLIBDIR)\cw32mt.lib
SYSLIBS16 = $(BCLIBDIR)\import.lib $(BCLIBDIR)\mathwl.lib $(BCLIBDIR)\cwl.lib

!if $d(WIN16)
  BCC = bcc -ml
  TLINK = tlink
  BRC = brc
  LDFLAGS = $(LDFLAGS16)
  SYSLIBS = $(SYSLIBS16)
!else
  BCC = bcc32
  TLINK = tlink32
  BRC = brc32
  LDFLAGS = $(LDFLAGS32)
  SYSLIBS = $(SYSLIBS32)
!endif

OFILES=ledit.obj lspedit.obj winutils.obj

lspedit.exe: $(OFILES) lspedit.def lspedit.res
  $(TLINK) @&&|
 $(LDFLAGS) $(OFILES)
 $<,$*
 $(SYSLIBS)
 lspedit.def
 lspedit.res
|

lspedit.obj: lspedit.h lspedit.c
	$(BCC) -c $(CFLAGS) lspedit.c

ledit.obj: ..\ledit.h ..\winutils.h ..\ledit.c
	$(BCC) -c $(CFLAGS) ..\ledit.c

winutils.obj: ..\winutils.h ..\winutils.c
	$(BCC) -c $(CFLAGS) ..\winutils.c

lspedit.res: lspedit.rc
	$(BRC) -R $(RFLAGS) -FO$@ lspedit.rc

clean:
	del *.obj
	del *.res

veryclean: clean
	del *.exe
	del "*.#*"
	del *.csm
