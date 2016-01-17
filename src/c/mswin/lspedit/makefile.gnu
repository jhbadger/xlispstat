CC=gcc

OFILES=ledit.o lspedit.o winutils.o resources.o

CFLAGS= -Wall -DNOTTY -DSTRICT -DMinGW32 -I.. -I. -g

lspedit.exe: $(OFILES) lspedit.def
	$(CC) -g -mwindows -o junk -Wl,--base-file,my.base $(OFILES)
	dlltool --dllname $@ --base-file my.base --output-exp my.exp
	$(CC) -g -mwindows -o $@ $(OFILES) -Wl,my.exp
	strip $@
	del my.exp
	del my.base
	del junk.exe

ledit.o: ../ledit.c ../ledit.h ../winutils.h
	$(CC) $(CFLAGS) -c ../ledit.c

lspedit.o: lspedit.h

winutils.o: ../winutils.c ../winutils.h
	$(CC) $(CFLAGS) -c ../winutils.c

resources.o: lspedit.rc ledit.ico
	windres -i lspedit.rc -o resources.o --define MinGW32

ledit.ico: ../icons/ledit.ico
	copy ..\icons\ledit.ico ledit.ico

clean:
	del *.o
	del ledit.ico
