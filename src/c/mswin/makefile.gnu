VPATH=..

CC=gcc

CFLAGS = -Wall -DSTRICT -DMinGW32 -I. -I.. -g

OFILES=xlisp.o xlarray.o xlbfun.o xlbignum.o xlcont.o xldbug.o xldmem.o \
	xleval.o xlfio.o xlglob.o xlimage.o xlio.o xljump.o \
	xllist.o xlmath3.o xlpp.o xlprin.o xlrand.o xlread.o xlseq.o \
	xlstr.o xlstruct.o xlsubr.o xlsym.o xlsys.o xltvec.o \
	xlbcode.o xlbcutil.o xlshlib.o dlfcn.o xlmodule.o dummycod.o \
	xlwrap.o mswstuff.o statdum.o basics.o betab.o bivnor.o common.o \
	compound.o ddists.o dists.o gamln.o gammab.o \
	linalg.o ludecomp.o math.o mats1.o mats2.o nor.o \
	objects.o ppnd.o sortdata.o eigen.o stats.o stmem.o studentb.o \
	utils.o utils2.o minimize.o cholesky.o svdecomp.o qrdecomp.o \
	makerot.o cfft.o lowess.o kernel.o splines.o blas.o \
	obinit.o xlinit.o statinit.o xlftab.o

WFILES=wxlisp.o ledit.o winutils.o mswdynld.o

GRFILES=dialogs.o hrdwrobs.o iview.o iviewdat.o iviewint.o \
	iviewscl.o menus.o windows.o xssctplt.o xssctmat.o \
	xsnewplt.o xsnames.o xsivint.o xshist.o xsgraph.o xsiview.o \
	xsiview2.o xsiview3.o xsspin.o xsivwin.o xsivwin2.o \
	filedlgs.o mswdlg.o mswgraph.o mswmenus.o mswrszbr.o mswwins.o

ALLOFILES = $(WFILES) $(OFILES) $(GRFILES) resources.o

../wxls32.exe: $(ALLOFILES)
	$(CC) -g -mwindows -o junk -Wl,--base-file,my.base $(ALLOFILES)
	dlltool --dllname $@ --base-file my.base --output-exp my.exp
	$(CC) -g -mwindows -o $@ $(ALLOFILES) -Wl,my.exp

XLISP_INCLUDES = ..\xlisp.h ..\xldmem.h ..\xlftab.h ..\xlglob.h xlconfig.h

wxlisp.o:	$(XLISP_INCLUDES) wxlisp.h ledit.h winutils.h ..\version.h
mswgraph.o:	$(XLISP_INCLUDES) wxlisp.h
ledit.o:	$(XLISP_INCLUDES) ledit.h winutils.h
winutils.o:	$(XLISP_INCLUDES) winutils.h
xlftab.o:	$(XLISP_INCLUDES) ..\xlisp.h ..\osdefs.h ..\osptrs.h
$(OFILES):	$(XLISP_INCLUDES) ..\xlisp.h
statinit.o xlimage.o mswstuff.o: $(XLISP_INCLUDES) ..\version.h

resources.o: wxls32.rc
	windres -i wxls32.rc -o resources.o --define MinGW32
