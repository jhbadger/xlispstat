BCDIR = \BC5
BCLIBDIR = $(BCDIR)\LIB
BCBINDIR = $(BCDIR)\BIN
INCLUDES = $(BCDIR)\INCLUDE;.;..\..\include
CFLAGS = -w- -W -3 $(OFLAGS) -H -H=$(TARGET).csm -DSTRICT -I$(INCLUDES)
RFLAGS = -I$(INCLUDES)
LDFLAGS32 = -L$(BCLIBDIR) -Tpe -aa -c -x $(BCLIBDIR)\c0w32.obj
LDFLAGS16 = -L$(BCLIBDIR) -Twe -c -C $(BCLIBDIR)\c0wl.obj
SYSLIBS32 = $(BCLIBDIR)\import32.lib $(BCLIBDIR)\cw32mt.lib
SYSLIBS16 = $(BCLIBDIR)\import.lib $(BCLIBDIR)\mathwl.lib $(BCLIBDIR)\cwl.lib

!if ! $d(NODEBUG)
  DFLAGS = -v
!endif

!if $d(WIN16)
  MFILES=mswmem.obj mswalloc.obj
  BCC = $(BCBINDIR)\bcc -ml $(DFLAGS) -dc
  LINK = $(BCBINDIR)\tlink $(DFLAGS)
  BRC = $(BCBINDIR)\brc -t
  TARGET = wxls
  LDFLAGS = $(LDFLAGS16)
  SYSLIBS = $(SYSLIBS16)
!else
  BCC = $(BCBINDIR)\bcc32 $(DFLAGS)
  LINK = $(BCBINDIR)\ilink32 $(DFLAGS)
  BRC = $(BCBINDIR)\brc32
  TARGET = wxls32
  LDFLAGS = $(LDFLAGS32)
  SYSLIBS = $(SYSLIBS32)
  OFLAGS = -O
!endif
IMPLIB = $(BCBINDIR)\implib

OFILES=xlisp.obj xlarray.obj xlbfun.obj xlbignum.obj \
xlcont.obj xldbug.obj xldmem.obj \
xleval.obj xlfio.obj xlglob.obj xlio.obj xljump.obj \
xllist.obj xlmath3.obj xlpp.obj xlprin.obj xlrand.obj xlread.obj xlseq.obj \
xlstr.obj xlstruct.obj xlsubr.obj xlsym.obj xlsys.obj xltvec.obj \
xlbcode.obj xlbcutil.obj xlshlib.obj dlfcn.obj xlmodule.obj dummycod.obj \
xlwrap.obj statdum.obj basics.obj \
betab.obj bivnor.obj common.obj \
compound.obj ddists.obj dists.obj gamln.obj gammab.obj \
linalg.obj ludecomp.obj math.obj mats1.obj mats2.obj nor.obj \
objects.obj ppnd.obj sortdata.obj eigen.obj \
stats.obj stmem.obj studentb.obj \
utils.obj utils2.obj minimize.obj \
cholesky.obj svdecomp.obj qrdecomp.obj makerot.obj \
cfft.obj lowess.obj kernel.obj splines.obj blas.obj \
obinit.obj xlinit.obj

WFILES=wxlisp.obj ledit.obj winutils.obj mswdynld.obj

GRFILES=dialogs.obj hrdwrobs.obj iview.obj iviewdat.obj iviewint.obj \
iviewscl.obj menus.obj windows.obj xssctplt.obj xssctmat.obj \
xsnewplt.obj xsnames.obj xsivint.obj xshist.obj xsgraph.obj xsiview.obj \
xsiview2.obj xsiview3.obj xsspin.obj xsivwin.obj xsivwin2.obj \
filedlgs.obj mswdlg.obj mswgraph.obj mswmenus.obj mswrszbr.obj mswwins.obj

VFILES=statinit.obj xlimage.obj mswstuff.obj

ALLFILES=$(WFILES) $(MFILES) $(OFILES) $(GRFILES) $(VFILES)

.c.obj:
	$(BCC) -c @&&!
$(CFLAGS)
! $<
{..}.c.obj:
	$(BCC) -c @&&!
$(CFLAGS)
! $<

$(TARGET).exe: $(ALLFILES) xlftab.obj $(TARGET).res $(TARGET).def
	$(LINK) @&&|
  $(LDFLAGS) $(ALLFILES) xlftab.obj
 $<,$*
 $(SYSLIBS)
 $(TARGET).def
 $(TARGET).res
|
	$(IMPLIB) $(TARGET).lib $(TARGET).exe

$(TARGET).res: $(TARGET).rc obinit.obj xlinit.obj statinit.obj
	$(BRC) -R $(RFLAGS) -FO$@ $(TARGET).rc

XLISP_INCLUDES = ..\..\include\xlisp.h ..\..\include\xldmem.h ..\..\include\xlftab.h \
..\..\include\xlglob.h xlconfig.h

wxlisp.obj:	$(XLISP_INCLUDES) wxlisp.h ledit.h winutils.h \
..\..\include\version.h
mswgraph.obj:	$(XLISP_INCLUDES) wxlisp.h
ledit.obj:	$(XLISP_INCLUDES) ledit.h winutils.h
winutils.obj:	$(XLISP_INCLUDES) winutils.h
xlftab.obj:	$(XLISP_INCLUDES) ..\..\include\osdefs.h ..\..\include\osptrs.h
$(OFILES):	$(XLISP_INCLUDES) 
$(VFILES):	$(XLISP_INCLUDES) ..\..\include\version.h
 

# This rule automatically generates the .def file with export entries
# for all public symbols. This is a bit dangerous since using data
# that is not imported properly will cause a crash.
wxls32.def: dumpexts.exe $(ALLFILES) xlftab.obj
	dumpexts -o $< WXLS32 @&&|
  $(ALLFILES) xlftab.obj
|


# This builds the dumpexts.exe utility adapted form the one in the Tcl
# 8.0 distribution.
dumpexts.exe: dumpexts.obj
	$(BCBINDIR)\ilink32 -Tpe -ap -c $(TOOLS)\lib\c0x32 -L$(BCLIBDIR) \
		dumpexts.obj,$@,,import32 cw32,,

dumpexts.obj: dumpexts.c
	$(BCBINDIR)\bcc32 -I$(INCLUDES) -c -W -3 -d -w -WC dumpexts.c

clean:
	del *.obj
	del *.res

veryclean: clean
	del ..\*.exe
	del *.exe
	del "*.#*"
	del *.csm
