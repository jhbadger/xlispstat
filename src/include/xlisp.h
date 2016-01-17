
#ifndef XLISP_H
#define XLISP_H

/* XLISP-PLUS is based on:
*/

/* xlisp - a small subset of lisp */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

/* Public Domain contributors to this modified distribution:
    Tom Almy, Mikael Pettersson, Neal Holtz, Johnny Greenblatt, 
    Ken Whedbee, Blake McBride, Pete Yadlowsky, Hume Smith,
    Wolfgang Kechel, and Richard Zidlicky */

/* Portions of this code from XLISP-STAT Copyright (c) 1988, Luke Tierney */

/* system specific definitions */
#ifndef XLGLOBAL
#define XLGLOBAL extern
#endif

#include "xlconfig.h"
#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>
#include <string.h>
#include <math.h>
#include <time.h>

#ifndef HUGE
#define HUGE 1e38
#endif

/************ Notice to anyone attempting modifications ****************/
/* Compared to original XLISP, length of strings in an LVAL exclude the
   terminating null. When appropriate, characters are consistantly treated
   as unsigned, and the null, \0, character is allowed. Don't write any new
   code that assumes NULL and/or NIL are zero */

/* xlenv environment frames may now also contain entries whose car is a fixnum.
   This is to support proper handling of block/return and tagbody/go -- see
   notes below at the definition of xlbindtag */

/********************** PREFERENCE OPTIONS ****************/

/* There used to be many different preference options; if
   you turned them all off you got "standard" xlisp 2.0. But because
   of option proliferation, and the change of name, this is no longer
   true: there are many fewer options, and most functions are now
   standard. */

/* You can also use dynamic array allocation by substituting dldmem.c
   and dlimage.c for xldmem.c and xlimage.c. Using this alternative
   adds 1184 bytes of code */

/* Costs indicated for Borland Turbo C++ V1.0 (as a C compiler) */

/* Not all permutations of these choices have been tested, but luckily most
   won't interract. */

/* This eliminates the problems of fixnums overflowing if no bignums
   The cost is slightly slower performance and 1100 bytes
   requires COMPLX, but no BIGNUMS */
#define NOOVFIXNUM

/* This option modifies performance, but don't affect execution of
   application programs (other than speed) */
/*#define JMAC*/	    /* performance enhancing macros, Johnny Greenblatt
			(7.5K at full config). Don't bother for 16 bit
			MSDOS compilers. */

/* This option is for IBM PC 8 bit ASCII. To use in other environments,
   you would need to modify the STUFF files, and possibly change the 
   definitions for the macros TOUPPER TOLOWER ISUPPER and ISLOWER.
   Option adds 464 bytes */
#define ASCII8

/* This option, in addition to ASCII8, is used for 8 bit ANSI characters,
   as used in Microsoft Windows. */
/*#define ANSI8 */


/* This option makes CERROR and ERROR work like in earlier versions of
   XLISP-PLUS (2.1e or earlier), instead of like Common Lisp. Note that
   all supplied libraries and demos use the new definition, so using this
   option will require modification of the LSP files. */
/*#define OLDERRORS */

/* This option is necessary for Microsoft Windows 3.0. It handles file
   streams using a local table of file defining structures. For non-windows
   use, the benefits are file streams can print their associated file names
   and files streams are preserved across saves. It also allows the
   functions TRUENAME and DELETE-FILE */
#define FILETABLE

/* This option allows xlisp to be called as a server. There is no outer loop.
   The STUFF file will have to modified appropriately, as well as xldbug. */
/*#define SERVER*/  /* server version */

/* This option adds a *readtable-case* global variable that has the same
   effect as the readtable-case function described in CLtL, 2nd Ed. 
   It is contributed by Blake McBride, root@blakex.raindernet.com, who
   places it in the public domain */
#define READTABLECASE

/* This option adds the :KEY arguments to appropriate functions. It's
   easy to work around when missing (adds about 2k bytes) */
#define KEYARG

/* Use environmental variable of same name as a search
    path for LOAD and RESTORE commands. Might not be
    available on some systems */
/*#define PATHNAMES "XLPATH"*/

/* Use generational garbage collector instead of original mark-and sweep
  collector. */
#define NEWGC

/* Add byte code compilation */
#define BYTECODE

/* The remainder of options solely add various functions. If you are
   pressed for space, you might try eliminating some of these (particularly
   TIMES, and BIGNUMS) */

#define SRCHFCN     /* SEARCH (1040 bytes)*/

#define MAPFCNS     /* SOME EVERY NOTANY NOTEVERY MAP (2352 bytes)*/

#define POSFCNS     /* POSITION- COUNT- FIND-  functions (1168 bytes)*/

#define REMDUPS     /* REMOVE-DUPLICATES (1440 bytes)*/

#define REDUCE      /* REDUCE, by Luke Tierney (with modifications). 
                       (1008 bytes)*/

#define SUBSTITUTE  /* adds SUBSTITUTE- and NSUBSTITUTE- functions. */

#define TIMES       /* time functions TIME GET-INTERNAL-RUN-TIME
                       GET-INTERNAL-REAL-TIME and constant
                       INTERNAL-TIME-UNITS-PER-SECOND (5286 bytes)*/

#define HASHFCNS    /* Hash table functions (Ken Whedbee):
                       SETHASH (SETF (SETHASH..)), MAKE-HASH-TABLE, 
                       TAA's REMHASH, MAPHASH, CLRHASH, HASH-TABLE-COUNT
                       (2608 bytes)*/

#define SETS        /* Luke Tierney's set functions ADJOIN UNION INTERSECTION
                        SET-DIFFERENCE SUBSETP (1328 bytes)*/

#define APPLYHOOK   /* adds applyhook support, strangely missing before 
                       (1312 bytes)*/

#define BIGNUMS     /* bignum and radix support. Requires COMPLX. */

/*#define BIGENDIAN*/ /* Define this for correct operation of read-byte and
					   write-byte in a big-endian system */
/*#define BIGENDIANFILE*/ /* define this for read-byte and write-byte to
                        utilize bigendian files. This option kept separate
                        so that binary files can be transfered between xlisp
                        systems on different processors */

#define LEXBIND	    /* Lexical tag scoping for TAGBODY/GO and BLOCK/RETURN.
			If not defined, use original dynamic scoping
			(Code from Luke Tierney) */

#define PACKAGES    /* Changes from using *obarray* to a simplified
		       package implementation (code from Luke Tierney)
			(11000 bytes) */

#define MULVALS     /* Changes to support multiple value returns
		       (code from Luke Tierney) (3500 bytes) */

#define CONDITIONS  /* Hooks to support conditions */

#define PRINTCIRCLE /* print and read circle support */

#define SAVERESTORE
                    /* SAVE and RESTORE commands (an original option!) 
                        (3936 bytes) */

/* The following option only available for certain compilers noted
   below */

/*#define GRAPHICS*/    /* add graphics commands
                        MODE COLOR MOVE DRAW MOVEREL DRAWREL
                       and screen commands CLS CLEOL GOTO-XY
                        (3k) */




/************ END OF PREFERENCE OPTIONS **************/


/* handle dependencies */

#ifdef BIGENDIAN
#ifndef GENERIC
#define GENERIC
#endif
#endif

#ifdef BYTECODE
#ifndef MULVALS
#define MULVALS
#endif
#ifndef PACKAGES
#define PACKAGES
#endif
#ifndef HASHFCNS
#define HASHFCNS
#endif
#endif

#ifndef XLISP_ONLY
#define XLISP_STAT
#endif


/*************** COMPILER/ENVIRONMENT OPTIONS ****************/


/* Default compiler options: */
/* NNODES       number of nodes to allocate in each request (2000) */
/* VSSIZE       number of vector nodes to allocate in each request (6000) */
/* EDEPTH       evaluation stack depth (650) */
/* ADEPTH       argument stack depth (1000) */
/* SFIXMIN	minimum static fixnum (-128, in xldmem.h) */
/* SFIXMAX	maximum static fixnum (255, in xldmem.h) */
/* FNAMEMAX	Maximum size of file name strings (63) */
/* MULVALLIMIT	Maximum number of returnable values (128) */
/* MAXFIX       maximum positive value of an integer (0x7fffffffL) */
/* MAXSLEN      maximum sequence length, <= maximum unsigned, on 16 bit
                systems should be the maximum string length that can be
                malloc'ed (100000000)*/
/* MAXVECLEN    maximum vector length, should normally be MAXSLEN, but on
                16 bit systems needs to be the maximum vector size that can
                be malloc'ed (MAXSLEN) */
/* MAXPLEN      maximum value for *print-length* */
/* MAXPLEV      maximum value for *print-level* */
/* FORWARD      type of a forward declaration () */
/* LOCAL        type of a local function (static) */
/* AFMT         printf format for addresses ("%x") */
/* FIXTYPE      data type for fixed point numbers (long) */
/* ITYPE        fixed point input conversion routine type (long atol()) */
/* ICNV         fixed point input conversion routine (atol) */
/* INTFMT       printf format for fixed point numbers ("%ld") (no BIGNUMS)*/
/* FLOTYPE      data type for floating point numbers (double) */
/* OFFTYPE      number the size of an address (int) */
/* CVPTR        macro to convert an address to an OFFTYPE. We have to go
                through hoops for some MS-DOS compilers that like to
                normalize pointers. In these days of Windows, compilers
                seem to be better behaved. Change to default definition
                only after extensive testing. This is no big deal as it
                only effects the SAVE command. (OFFTYPE)(x) */
/* ALIGN32      Compiler has 32 bit ints and 32 bit alignment of struct
                elements */
/* DOSINPUT     OS specific code can read using OS's line input functon */
/* IEEEFP       IEEE FP -- proper printing of +-INF and NAN
                       for compilers that can't hack it.
                       Currently for little-endian systems. */
/* CDECL        C style declaration, for compilers that can also generate
                Pascal style, to allow calling of main() ([nothing])*/
/* ANSI         define for ANSI C compiler */

/* STDIO and MEM and certain STRING calls can be overridden as needed
   for various compilers or environments. By default, the standard
   library functions are used. Any substitute function must mimic the
   standard function in terms of arguments and return values */

/* OSAOPEN      Open ascii file (fopen) */
/* OSBOPEN      Open binary file (fopen) */
/* MODETYPE     Type of open mode (const char *) */
/* OPEN_RO      Open mode for read only ("r") */
/* OPEN_UPDATE  Open mode for update ("r+") */
/* CREATE_WR    Open mode for create for writing ("w") */
/* CREATE_UPDATE Open mode for create update ("w+") */
/* CLOSED       Closed file, or return value when open fails (NULL) */
/* OSGETC       Binary/text Character read (fgetc) */
/* OSPUTC       Binary/text Character write (fputc) */
/* OSREAD       Binary read of file (fread) */
/* OSWRITE      Binary write of file (fwrite) */
/* OSCLOSE      Close the file (fclose) */
/* OSSEEK       Seek in file (fseek(fp,loc,SEEK_SET)) */
/* OSSEEKCUR    Seek for changing direction (fseek(fp,loc,SEEK_CUR)) */
/* OSSEEKEND    Seek to end  (fseek(fp,0L,SEEK_END)) */
/* OSTELL       Tell file location (ftell) */
/* FILEP        File pointer type (FILE *),
                used in all the above functions */
/* STDIN        Standard input (a FILEP) (stdin) */
/* STDOUT       Standard output (stdout) */
/* CONSOLE      Console (stderr) */

/* MALLOC       Memory allocation (malloc) */
/* CALLOC       Memory allocation (calloc) */
/* MFREE        Memory allocation (free) */

/* Systems that differentiate between Ascii and Binary files can either
   handle the "problem" via open (OSAOPEN vs OSBOPEN) or by using the following
   overrides. When these are used, OSGETC and OSPUTC are only used for
   binary files. We can tell the difference because the file objects have
   a binary/ascii bit. We really want to use the following on systems that
   can't handle fseek() properly in ASCII files (such as the GCC compiler
   runtimes). We could do everything this way, but I wanted to leave the
   hook to run older system dependent stuff.c files. */

/* OSAGETC      Text character read, if different from OSGETC */
/* OSAPUTC      Text character write, if different from OSPUTC*/

/* These are needed in case far pointer override is necessary: */

/* STRCMP       String compare (strcmp) */
/* STRCPY       String copy (strcpy) */
/* STRNCPY      String copy (strncpy) */
/* STRCAT       String concatenate (strcat) */
/* STRLEN       String length (strlen) */
/* MEMCPY       Memory copy (memcpy) */
/* MEMSET       Memory set (memset) */
/* MEMMOVE      Memory move (memmove) */

/* The following are when system stack checking is incorporated */
/* This feature from Richard Zidlicky */

/* STSZ		Size of stack (passed from compiler command line) */
/* GCSTMARGIN	Do not try GC with less stack than this (2048) */
/* GCMARGLO	Fatal death if less than this much stack during GC */
/* MARGLO	Goto toplevel when stack below this (512) */

/* The following are definitions for various characters input from the
   keyboard. The default values are for MS-DOS, and match the documentation.
   The characters are only referenced in the "STUFF" file. */
/* C_BREAK	Enter break level (control-B) */
/* C_TOPLEV	Goto top level (control-C) */
/* C_CLEAN	Go up one level (control-G) */
/* C_CONT	Continue (control-P) */
/* C_EOF	End of file (control-Z) */
/* C_PAUSE	Pause (control-S) */
/* C_STATUS	Status message (control-T) */
/* C_TAB	Tab character/name completion */
/* C_BS		Destructive backspace (Control-H or Backspace) */
/* C_ESC	Abort input line (escape) */
/* C_DEL	Delete character at cursor (Delete) */
/* C_LA		Nondestructive backspace (left arrow) */
/* C_RA		Nondestructive space (right arrow) */
/* C_UA		Previous command (up arrow) */
/* C_DA		Next command (down arrow) */
/* C_HOME	Start of line (home) */
/* C_END	End of line (end) */


/* for Zortech C  -- Versions 2.0 and above, please */
/* Works for Large Model, 268PM model (Z), and 386PM model (X) */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/25 */
#ifdef __ZTC__
#ifdef DOS386   /* 80386 compiler */
#define EDEPTH 4000 
#define ADEPTH 8000
#define VSSIZE 20000
#define ALIGN32
#define ANSI
#if __ZTC__ < 0x300
#define IEEEFP      /* they fixed this */
#endif
#define CDECL   _cdecl
#define DOSINPUT
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#else           /* 80286PM or Real mode */
#ifdef DOS16RM
#define EDEPTH          2000
#define ADEPTH          3000
#endif
#define MAXSLEN         (65519U)
#define MAXVECLEN       (16379U)
#define MAXPLEN         (32767)
#define MAXPLEV         (32767)
#define ANSI
#define AFMT            "%lx"
#define OFFTYPE         unsigned long
#if __ZTC__ < 0x300
#define IEEEFP      /* they fixed this */
#endif
#define CDECL   _cdecl
#define DOSINPUT
#undef JMAC	    /* not worth effort if cramped for space */
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#endif
#endif

/* MS Windows */
#ifdef _Windows
#ifdef BIGNUMS
#if defined(__TURBOC__) && defined(BIGNUMS)
#  define LDEXP myldexp
   double myldexp(double, int);
#endif
#endif
#ifdef STSZ
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#ifndef EDEPTH
#define EDEPTH ((STSZ-MARGLO)/22)
#endif
#ifndef ADEPTH
#define ADEPTH ((STSZ-MARGLO)/16)
#endif
#endif
#define ANSI
#define AFMT            "%lx"
#define OFFTYPE         unsigned long
/*#define CVPTR(x)      ((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))*/
#ifndef MSDOS
#define MSDOS
#endif
#include <windows.h>
#include <windowsx.h>
#ifdef WIN32
extern long win32stsz;
#ifndef WIN32S_STSZ
#define WIN32S_STSZ 20000
#endif
#ifndef WIN32NT_STSZ
#define WIN32NT_STSZ 250000
#endif
#ifdef IN
#undef IN
#endif
#define CONTEXT xlsCONTEXT
#else
#define longjmp(x,y) Throw((LPCATCHBUF) &(x),y)
#define setjmp(x) Catch((LPCATCHBUF) &(x))
#define jmp_buf CATCHBUF
#endif /* WIN32 */
#define main xlsmain
#define FILETABLE  /* force the file table */
#define NNODES 2000	/* These need to be set big for best results */
#define VSSIZE 16000
#define GCSTMARGIN (4000)
#define MARGLO (3000)	/* Windows seems to have serious problems if stack
			   falls below this */
#define FNAMEMAX 127
#ifndef WIN32
#define MAXSLEN         (65519U)
#define MAXVECLEN       (16383U)
#define MAXPLEN         (32767)
#define MAXPLEV         (32767)
#endif /* WIN32 */
#ifndef CDECL
#define CDECL _Cdecl
#endif
#define DOSINPUT
#undef JMAC	    /* not worth effort if cramped for space */
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _Cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#endif /* _Windows */

/* for the JPI TopSpeed C Compiler, Medium or Large memory model */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/34 */
#ifdef __TSC__
#pragma data(heap_size=>4096,stack_size=>STSZ)
#ifdef STSZ  /* value of STSZ should be the same as stack_size, above */
extern char *stackbase; /* in theory-- we should use TSC's function */
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/34)
#define ADEPTH (STSZ/24)
#endif
#define IEEEFP
#define MAXSLEN         (65519U)
#define MAXVECLEN       (16379U)
#define MAXPLEN         (32767)
#define MAXPLEV         (32767)
#define ANSI
#define AFMT            "%lx"
#define OFFTYPE         unsigned long
#define CVPTR(x)        ((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))
#define CDECL           /* don't use CDECL with this compiler */
#define DOSINPUT
#undef JMAC	    /* not worth effort if cramped for space */
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE *osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#endif

/* for the Microsoft C compiler - MS-DOS, large model */
/* Version 5.0.  Avoid optimizations. Should work with earlier as well. */
/* Version 6.0A. Most opts ok. Avoid those that conflict with longjump */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/30 */
#ifdef MSC
#ifdef STSZ
/* MSC seems to suck up alot of stack for system use -- set the
   stack size 1k larger than STSZ */
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/30)
#define ADEPTH (STSZ/20)
#endif
#define MAXSLEN         (65519U)
#define MAXVECLEN       (16379U)
#define MAXPLEN         (32767)
#define MAXPLEV         (32767)
#define ANSI
#define AFMT            "%lx"
#define OFFTYPE         unsigned long
#define CVPTR(x)        ((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))
#define CDECL _cdecl
#define DOSINPUT
#undef JMAC	    /* not worth effort if cramped for space */
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#endif

/* EMX GCC and OS/2 */
#ifdef EMX
#ifdef STSZ /* Stacks can be made very large, but we will keep it in reason.
			   The makefile shows how to change the size */
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/32)
#define ADEPTH (STSZ/24)
#define GCSTMARGIN (0)	/* don't GC checking -- allow it to run always! */
#else
#define EDEPTH 4000
#define ADEPTH 8000
#endif
#define VSSIZE 20000
#define ALIGN32
#define ANSI
#define  SEEK_CUR 1
#define  SEEK_END 2
#define  SEEK_SET 0
/* library improperly handles ASCII files re lseek() */
#define OSBOPEN osopen
#define OSAOPEN osopen
#define OSAGETC osagetc
#define OSAPUTC osaputc
#ifdef FILETABLE
extern int osagetc(int), osaputc(int,int);
extern int osopen(const char *name, const char *mode);
#else /* No FILETABLE */
extern int osagetc(FILE*), osaputc(int,FILE*);
extern FILE *osopen(const char *name, const char *mode);
#endif
#undef MEDMEM
#undef GRAPHICS
#include <sys\param.h>
#define FNAMEMAX MAXPATHLEN
#define STRMAX MAXPATHLEN   /* TAA Mod so buffer will be big enough */
#endif

/* For GCC on MSDOS (see DOSSTUFF.C) */
#ifdef GCC
#ifdef STSZ /* stack can't really overflow here, except with really large
	       stacks which run out of disk swap space -- so we will
	       just use the checking to catch runaway recursions */
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/32)
#define ADEPTH (STSZ/24)
#define GCSTMARGIN (0)	/* don't GC checking -- allow it to run always! */
#else
#define EDEPTH 4000
#define ADEPTH 8000
#endif
#define VSSIZE 20000
#define ALIGN32
#define ANSI
#define  SEEK_CUR 1
#define  SEEK_END 2
#define  SEEK_SET 0
/* #define IEEEFP   Fixed at release 1.09 */
/* library improperly handles ASCII files re lseek() */
#define OSBOPEN osopen
#define OSAOPEN osopen
#define OSAGETC osagetc
#define OSAPUTC osaputc
/* Turns out fseek SEEK_CUR is buggy as well :-( */
#define OSSEEKCUR(f,pos) OSSEEK(f, OSTELL(f) + (pos))
#ifdef FILETABLE
extern int osagetc(int), osaputc(int,int);
extern int osopen(const char *name, const char *mode);
#else /* No FILETABLE */
extern int osagetc(FILE*), osaputc(int,FILE*);
extern FILE *osopen(const char *name, const char *mode);
#endif
#define DOSINPUT
#undef MEDMEM
#endif

/* for the UNIX C compiler */
#ifdef UNIX
#ifdef SUNVIEW
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <pixrect/pixrect_hs.h>
#endif /* SUNVIEW */
#ifdef X11WINDOWS
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>
#include <sys/types.h>
#include <sys/times.h>
#endif /* X11WINDOWS */

#ifndef EDEPTH
#define EDEPTH 8000 
#endif
#ifndef ADEPTH
#define ADEPTH 8000
#endif
#define AFMT                    "%lx"
#define OFFTYPE                 unsigned long    /* TAA Added 2/94 */
#ifndef SEEK_SET
#define SEEK_SET                0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR                1
#endif
#ifndef SEEK_END
#define SEEK_END                2
#endif
#ifdef GRAPHICS
#undef GRAPHICS
#endif
#ifdef ASCII8
#undef ASCII8
#endif
/*#define remove unlink*/   /* not all Unix systems have remove */
#ifdef FILETABLE
#ifdef ANSI
extern int osopen(char *name, char *mode);
#else
extern int osopen();
#endif
#define OSAOPEN osopen
#define OSBOPEN osopen
/* use default FILETABLE declaration for OSCLOSE */
#endif
/* Unix filenames can be long! */
#include <sys/param.h>
#ifndef MAXPATHLEN
#define MAXPATHLEN  1024
#endif
#define FNAMEMAX MAXPATHLEN
#define STRMAX MAXPATHLEN   /* TAA Mod so buffer will be big enough */
#endif

/* IBM/370 implementations using the SAS/C compiler */
#ifdef __SASC__
#define VOID void
#define EDEPTH 4000
#define ADEPTH 8000
#define ALIGN32
#define AFMT			"%lx"
#ifndef SEEK_SET
#define SEEK_SET		0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR		1
#endif
#ifndef SEEK_END
#define SEEK_END		2
#endif
#undef GRAPHICS
#undef MEDMEM
#undef ASCII8
#ifdef FILETABLE
extern int osopen();
#define OSAOPEN osopen
#define OSBOPEN osopen
/* use default FILETABLE declaration for OSCLOSE */
#endif
/* MVS/CMS filenames can be long! */
#include <sys/param.h>
#ifndef MAXPATHLEN
  /* Some versions of SAS/C define this, others don't... */
#define MAXPATHLEN  1024
#endif
#define FNAMEMAX MAXPATHLEN
#define STRMAX MAXPATHLEN
#endif

/* Amiga Lattice 5.04 (From Hume Smith) */
#ifdef AMIGA
#define EDEPTH 4000
#define ADEPTH 6000
#define ALIGN32
#define AFMT            "%lx"
#define SEEK_SET      0
#define SEEK_CUR      1
#define SEEK_END      2
#undef GRAPHICS
#undef FILETABLE    /* not ported */
#undef ASCII8
#endif

/* for the Macintosh */
#ifdef MACINTOSH
#define NNODES         2000
#define AFMT           "%lx"
#define OFFTYPE        unsigned long
#ifdef STSZ
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#endif
#ifdef applec
# include <Quickdraw.h>
# include <Windows.h>
# include <Controls.h>
# include <Menus.h>
# include <Dialogs.h>
# include <ToolUtils.h>
# include <Events.h>
# include <Fonts.h>
# include <OSUtils.h>
# include <TextEdit.h>
# include <OSEvents.h>
# include <Lists.h>
# include <Memory.h>
# include <Script.h>
# include <Files.h>
# include <SegLoad.h>
# include <Desk.h>
# include <Packages.h>
# include <Scrap.h>
# include <Resources.h>
# include <Strings.h>
#endif /* applec */
#include <ColorPicker.h>
# define newstring newstring_ /* to avoid a name conflict */
# define SysBeep SYSBEEPMPW  /* to avoid a name conflict */
#ifndef MAXPATHLEN
#define MAXPATHLEN 255
#endif
#define FNAMEMAX MAXPATHLEN
#ifndef applec
# define isascii(c)		((unsigned char)(c)<=0177)
#endif  /* applec */
#ifndef FILETABLE
# define OSBOPEN osbopen
extern FILE *osbopen(const char *name, const char *mode);
#endif /* FILETABLE */
#endif /* MACINTOSH */

/*>>>>>>> For other systems -- You are on your own! */

/* Take care of VOID default definition */

#ifndef VOID
#define VOID void
#endif

/* Handle the FILETABLE specification -- non-windows */
#ifdef FILETABLE
#define FTABSIZE 13
#define FILEP int
#define CLOSED (-1)     /* because FILEP is now table index */
#define STDIN (0)
#define STDOUT (1)
#define CONSOLE (2)
#ifndef OSAOPEN
#define OSAOPEN osaopen
extern FILEP osaopen(const char *name, const char *mode);
#endif
#ifndef OSBOPEN
#define OSBOPEN osbopen
extern FILEP osbopen(const char *name, const char *mode);
#endif
#ifndef OSGETC
#define OSGETC(f) fgetc(filetab[f].fp)
#endif
#ifndef OSPUTC
#define OSPUTC(i,f) fputc(i,filetab[f].fp)
#endif
#ifndef OSREAD
#define OSREAD(x,y,z,f) fread(x,y,z,filetab[f].fp)
#endif
#ifndef OSWRITE
#define OSWRITE(x,y,z,f) fwrite(x,y,z,filetab[f].fp)
#endif
#ifndef OSCLOSE
#define OSCLOSE osclose
#ifdef ANSI
extern void osclose(int i); /* we must define this */
#else
extern VOID osclose();
#endif
#endif
#ifndef OSSEEK
#define OSSEEK(f,loc) fseek(filetab[f].fp,loc,SEEK_SET)
#endif
#ifndef OSSEEKEND
#define OSSEEKEND(f) fseek(filetab[f].fp,0L,SEEK_END)
#endif
#ifndef OSSEEKCUR
#define OSSEEKCUR(f,loc) fseek(filetab[f].fp,loc,SEEK_CUR)
#endif
#ifndef OSTELL
#define OSTELL(f) ftell(filetab[f].fp)
#endif
#endif

#ifdef ASCII8
/* 8 bit ASCII character handling */
#define LC8 1   /* lower case 8bit */
#define LUC8 2  /* lower case 8bit with upper case version */
#define LU8 (LC8 | LUC8)
#define UC8 4   /* upper case 8bit (always have lower case version) */
/* ISUPPER return true for all upper case characters */
#define ISUPPER(c) (UC8 & ascii8tbl[(unsigned char)(c)])
/* ISLOWER returns true for all lowercase characters which have upper case versions */
#define ISLOWER(c) (LUC8 & ascii8tbl[(unsigned char)(c)])
/* ISLOWERA returns true for all lowercase characters */
#define ISLOWERA(c) (LC8 & ascii8tbl[(unsigned char)(c)])
/* ISLOWER7 returns true for characters a-z only */
#define ISLOWER7(c) (isascii(c) && islower(c))
/* these versions of TOUPPER and TOLOWER only work on characters that
   can be converted in case. The functions are the same, and do a table lookup*/
#define TOLOWER(c) (ascii8cnv[(unsigned char)(c) - 'A'])
#define TOUPPER(c) (ascii8cnv[(unsigned char)(c) - 'A'])
#else
/* We will modify the IS* functions so that they work over the full 8 bit
   character range since these characters can still be generated. */
#ifdef __SASC__
#define ISLOWER(c) (islower(c))
#define ISUPPER(c) (isupper(c))
#define TOUPPER(c) toupper(c)
#define TOLOWER(c) tolower(c)
#define ISLOWER7(c) (islower(c))
#define ISLOWERA(c) (islower(c))
#else
#define ISLOWER(c) (((unsigned)(c)) < 128 && islower(c))
#define ISUPPER(c) (((unsigned)(c)) < 128 && isupper(c))
#define TOUPPER(c) toupper(c)
#define TOLOWER(c) tolower(c)
#define ISLOWER7(c) (((unsigned)(c)) < 128 && islower(c))
#define ISLOWERA(c) (((unsigned)(c)) < 128 && islower(c))
#endif
#endif

#ifndef LDEXP   /* handle bad LDEXP function (Borland) */
#define LDEXP ldexp
#endif

/************ DEFAULT DEFINITIONS  ******************/
#ifndef C_BREAK
#define C_BREAK ('\002')
#endif
#ifndef C_TOPLEV
#define C_TOPLEV ('\003')
#endif
#ifndef C_CLEAN
#define C_CLEAN ('\007')
#endif
#ifndef C_CONT
#define C_CONT	('\020')
#endif
#ifndef C_EOF
#define C_EOF	('\032')
#endif
#ifndef C_PAUSE
#define C_PAUSE ('\023')
#endif
#ifndef C_STATUS
#define C_STATUS ('\024')
#endif
#ifndef C_TAB
#define C_TAB ('\t')
#endif
#ifndef C_BS
#define C_BS  ('\010')
#endif
#ifndef C_DEL
#define C_DEL (339)
#endif
#ifndef C_ESC
#define C_ESC ('\033')
#endif
#ifndef C_LA
#define C_LA (331)
#endif
#ifndef C_RA
#define C_RA (333)
#endif
#ifndef C_UA
#define C_UA (328)
#endif
#ifndef C_DA
#define C_DA (336)
#endif
#ifndef C_HOME
#define C_HOME (327)
#endif
#ifndef C_END
#define C_END (335)
#endif

#ifndef NNODES
#define NNODES          2000
#endif
#ifndef VSSIZE
#define VSSIZE          6000
#endif
#ifndef EDEPTH
#define EDEPTH          650
#endif
#ifndef ADEPTH
#define ADEPTH          1000
#endif
#ifndef FORWARD
#define FORWARD
#endif
#ifndef LOCAL
#define LOCAL           static
#endif
#ifndef AFMT
#define AFMT            "%lx"
#endif
#ifndef FIXTYPE
#define FIXTYPE         long
#endif
#ifdef ANSI /* ANSI C Compilers already define this! */
#include <limits.h>
#include <float.h>
#define MAXFIX  LONG_MAX
#define MINFIX  LONG_MIN
#else
#ifndef DBL_MAX
#define DBL_MAX (1.7976931348623167e+308)
#endif
#ifndef MAXFIX
#define MAXFIX          (0x7fffffffL)
#endif
#ifndef MINFIX
#define MINFIX          ((- MAXFIX) - 1)
#endif
#endif
#ifndef MAXSLEN
#define MAXSLEN         (100000000)   /* no sequences longer than this */
#endif
#ifndef MAXVECLEN
#define MAXVECLEN       MAXSLEN
#endif
#ifndef MAXPLEN
#define MAXPLEN         MAXSLEN
#endif
#ifndef MAXPLEV
#define MAXPLEV         MAXSLEN
#endif
#ifndef ITYPE
#define ITYPE           long atol()
#endif
#ifndef ICNV
#define ICNV(n)         atol(n)
#endif
#ifndef INTFMT
#define INTFMT          "%ld"
#endif
#ifndef FLOTYPE
#define FLOTYPE         double
#endif
#ifndef OFFTYPE
#define OFFTYPE         unsigned long
#endif
#ifndef CVPTR
#define CVPTR(x)        ((OFFTYPE)(x))
#endif
#ifndef CDECL
#define CDECL
#endif
#ifndef FNAMEMAX
#define FNAMEMAX 63
#endif
#ifndef OSAOPEN
#define OSAOPEN fopen
#endif
#ifndef OSBOPEN
#define OSBOPEN fopen
#endif
#ifndef MODETYPE
#define MODETYPE const char *
#endif
#ifndef OPEN_RO
#define OPEN_RO "r"
#endif
#ifndef OPEN_UPDATE
#define OPEN_UPDATE "r+"
#endif
#ifndef CREATE_WR
#define CREATE_WR "w"
#endif
#ifndef CREATE_UPDATE
#define CREATE_UPDATE "w+"
#endif
#ifndef CLOSED
#define CLOSED NULL
#endif
#ifndef OSGETC
#define OSGETC fgetc
#endif
#ifndef OSPUTC
#define OSPUTC fputc
#endif
#ifndef OSREAD
#define OSREAD fread
#endif
#ifndef OSWRITE
#define OSWRITE fwrite
#endif
#ifndef OSCLOSE
#define OSCLOSE fclose
#endif
#ifndef OSSEEK
#define OSSEEK(fp,loc) fseek(fp,loc,SEEK_SET)
#endif
#ifndef OSSEEKEND
#define OSSEEKEND(fp) fseek(fp,0L,SEEK_END)
#endif
#ifndef OSSEEKCUR
#define OSSEEKCUR(fp,loc) fseek(fp,loc,SEEK_CUR)
#endif
#ifndef OSTELL
#define OSTELL ftell
#endif
#ifndef FILEP
#define FILEP FILE *
#endif
#ifndef STDIN
#define STDIN stdin
#endif
#ifndef STDOUT
#define STDOUT stdout
#endif
#ifndef CONSOLE
#define CONSOLE stderr
#endif
#ifndef MALLOC
#define MALLOC malloc
#endif
#ifndef CALLOC
#define CALLOC calloc
#endif
#ifndef MFREE
#define MFREE free
#endif
#ifndef STRCMP
#define STRCMP strcmp
#endif
#ifndef STRCPY
#define STRCPY strcpy
#endif
#ifndef STRNCPY
#define STRNCPY strncpy
#endif
#ifndef STRCAT
#define STRCAT strcat
#endif
#ifndef STRLEN
#define STRLEN strlen
#endif
#ifndef MEMCPY
#define MEMCPY memcpy
#endif
#ifndef MEMSET
#define MEMSET memset
#endif
#ifndef MEMMOVE
#define MEMMOVE memmove
#endif
#ifdef STSZ
#ifndef GCSTMARGIN
#define GCSTMARGIN (2048)
#endif
#ifndef MARGLO
#define MARGLO (512)
#endif
#ifndef GCMARGLO
#define GCMARGLO (256)
#endif
#ifndef STACKREPORT
#define STACKREPORT(x) stackreport()
extern int stackreport(VOID);
#endif
#endif
#ifdef MULVALS
#ifndef MULVALLIMIT
#define MULVALLIMIT (128)
#endif
#endif
#ifndef XL_SETJMP
#  define XL_SETJMP(env) setjmp(env)
#endif
#ifndef XL_LONGJMP
#  define XL_LONGJMP(env,val) longjmp(env,val)
#endif
#ifndef XL_JMP_BUF
#  define XL_JMP_BUF jmp_buf
#endif

/* useful definitions */
#ifndef TRUE
#define TRUE    1
#endif
#ifndef FALSE
#define FALSE   0
#endif

#ifndef PI
#define PI 3.1415926535897932384626433832795028841972
#endif

#ifdef ANSI
#include <stdlib.h>
#endif

#ifndef ANSI
extern char *malloc(), *calloc();
extern VOID free();
extern int system();
#endif /* ANSI */

#ifdef ANSI /* thanks for this trick go to Hume Smith */
#define _(x) x
#else
#define _(x) ()
#endif

/* Alternate macros for prototyping, borrowed from TeX distribution */
#ifdef ANSI

#define P1H(p1) (p1)
#define P2H(p1,p2) (p1, p2)
#define P3H(p1,p2,p3) (p1, p2, p3)
#define P4H(p1,p2,p3,p4) (p1, p2, p3, p4)
#define P5H(p1,p2,p3,p4,p5) (p1, p2, p3, p4, p5)
#define P6H(p1,p2,p3,p4,p5,p6) (p1, p2, p3, p4, p5, p6)
#define P7H(p1,p2,p3,p4,p5,p6,p7) (p1, p2, p3, p4, p5, p6, p7)
#define P8H(p1,p2,p3,p4,p5,p6,p7,p8) (p1, p2, p3, p4, p5, p6, p7, p8)
#define P9H(p1,p2,p3,p4,p5,p6,p7,p8,p9) (p1, p2, p3, p4, p5, p6, p7, p8, p9)
#define P10H(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
#define P11H(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11) (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
#define P12H(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12) (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
#define P13H(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13) (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)

#define P1C(t1,n1)(t1 n1)
#define P2C(t1,n1, t2,n2)(t1 n1, t2 n2)
#define P3C(t1,n1, t2,n2, t3,n3)(t1 n1, t2 n2, t3 n3)
#define P4C(t1,n1, t2,n2, t3,n3, t4,n4)(t1 n1, t2 n2, t3 n3, t4 n4)
#define P5C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5)
#define P6C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5, t6 n6)
#define P7C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5, t6 n6, t7 n7)
#define P8C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5, t6 n6, t7 n7, t8 n8)
#define P9C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5, t6 n6, t7 n7, t8 n8, t9 n9)
#define P10C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9, t10,n10) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5, t6 n6, t7 n7, t8 n8, t9 n9, t10 n10)
#define P11C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9, t10,n10, t11,n11) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5, t6 n6, t7 n7, t8 n8, t9 n9, t10 n10, t11 n11)
#define P12C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9, t10,n10, t11,n11, t12,n12) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5, t6 n6, t7 n7, t8 n8, t9 n9, t10 n10, t11 n11, t12 n12)
#define P13C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9, t10,n10, t11,n11, t12,n12, t13,n13) \
  (t1 n1, t2 n2, t3 n3, t4 n4, t5 n5, t6 n6, t7 n7, t8 n8, t9 n9, t10 n10, t11 n11, t12 n12, t13 n13)

#else /* not ANSI */

#define P1H(p1) ()
#define P2H(p1, p2) ()
#define P3H(p1, p2, p3) ()
#define P4H(p1, p2, p3, p4) ()
#define P5H(p1, p2, p3, p4, p5) ()
#define P6H(p1, p2, p3, p4, p5, p6) ()
#define P7H(p1, p2, p3, p4, p5, p6, p7) ()
#define P8H(p1, p2, p3, p4, p5, p6, p7, p8) ()
#define P9H(p1, p2, p3, p4, p5, p6, p7, p8, p9) ()
#define P10H(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) ()
#define P11H(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) ()
#define P12H(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) ()
#define P13H(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) ()

#define P1C(t1,n1) (n1) t1 n1;
#define P2C(t1,n1, t2,n2) (n1,n2) t1 n1; t2 n2;
#define P3C(t1,n1, t2,n2, t3,n3) (n1,n2,n3) t1 n1; t2 n2; t3 n3;
#define P4C(t1,n1, t2,n2, t3,n3, t4,n4) (n1,n2,n3,n4) \
  t1 n1; t2 n2; t3 n3; t4 n4;
#define P5C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5) (n1,n2,n3,n4,n5) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5;
#define P6C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6) (n1,n2,n3,n4,n5,n6) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6;
#define P7C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7) (n1,n2,n3,n4,n5,n6,n7) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7;
#define P8C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8) (n1,n2,n3,n4,n5,n6,n7,n8) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8;
#define P9C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9) \
  (n1,n2,n3,n4,n5,n6,n7,n8,n9) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8; t9 n9;
#define P10C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9, t10,n10) \
  (n1,n2,n3,n4,n5,n6,n7,n8,n9,n10) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8; t9 n9; t10 n10;
#define P11C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9, t10,n10, t11,n11) \
  (n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8; t9 n9; t10 n10; t11 n11;
#define P12C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9, t10,n10, t11,n11, t12,n12) \
  (n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8; t9 n9; t10 n10; t11 n11; t12 n12;
#define P13C(t1,n1, t2,n2, t3,n3, t4,n4, t5,n5, t6,n6, t7,n7, t8,n8, t9,n9, t10,n10, t11,n11, t12,n12, t13,n13) \
  (n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13) \
  t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8; t9 n9; t10 n10; t11 n11; t12 n12; t13 n13;

#endif /* ANSI */

/************* END OF COMPILER/ENVIRONMENT OPTIONS ************/

/* $putpatch.c$: "MODULE_XLISP_H_PROVIDES" */

/* include the dynamic memory definitions */
#include "xldmem.h"

/* program limits */
#ifndef STRMAX
#define STRMAX          600             /* maximum length of a string constant */
#endif
#define HSIZE           199             /* symbol hash table size */
#define SAMPLE          1000            /* control character sample rate */

/* function table offsets for the initialization functions */
#define FT_RMHASH       0
#define FT_RMQUOTE      1
#define FT_RMDQUOTE     2
#define FT_RMBQUOTE     3
#define FT_RMCOMMA      4
#define FT_RMLPAR       5
#define FT_RMRPAR       6
#define FT_RMSEMI       7
#define FT_CLNEW        10
#define FT_CLISNEW      11
#define FT_CLANSWER     12
#define FT_OBISNEW      13
#define FT_OBCLASS      14
#define FT_OBSHOW       15
#define FT_OBPRIN1      16
#define FT_CLMETHOD     17

/* macro to push a value onto the argument stack */
#define pusharg(x)      {LVAL tmp__;\
                         if (xlsp >= xlargstktop) xlargstkoverflow();\
                         tmp__ = (x);\
                         *xlsp++ = tmp__;}

/* The standard cc optimizer for sun4's under SunOS 4.1.3 seems to have
a bug that causes the standard definition to miscompile at least in
evpusharg in xleval.c. It looks like it increments xlsp too early, thus
leaving a potentially invalid entlry in the argument stack the GC tro
trip over. The following change seems to prevent this problem. I don;t
know where else the problem exists, and to make life simple I just use
the alternate definition on all suns. It *should* not be any less
efficient if the optimizer does a decent job. */

#ifdef sun
#undef pusharg
#define pusharg(x)      {LVAL __pushargTMP__; \
                         if (xlsp >= xlargstktop) xlargstkoverflow();\
                         __pushargTMP__ = (x); \
                         *xlsp++ = __pushargTMP__;}
#endif

/* macros to protect pointers */
#define xlstkcheck(n)   {if (xlstack - (n) < xlstkbase) xlstkoverflow();}
#define xlsave(n)       {*--xlstack = &n; n = NIL;}
#define xlprotect(n)    {*--xlstack = &n;}

/* check the stack and protect a single pointer */
#define xlsave1(n)      {if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n; n = NIL;}
#define xlprot1(n)      {if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n;}

/* macros to pop pointers off the stack */
#define xlpop()         {++xlstack;}
#define xlpopn(n)       {xlstack+=(n);}

/* macros to manipulate the lexical environment */
#define xlframe(e)      cons(NIL,e)
#define xlfbind(s,v)    xlpbind(s,v,xlfenv);
#define xlpbind(s,v,e)  {rplaca(e,cons(cons(s,v),car(e)));}

#ifdef LEXBIND
/* macros for installing tag bindings in xlenv */
/* Added feature from Luke Tierney 09/93 */
/* These are ued to insure that return and go only find lexically visible
   tags. Currently the binding is formed by putting the context pointer
   of the block, as a fixnum, into the car of a binding in xlenv. This
   should work fine as long as nothing tacitly assumes these cars must be
   symbols. If that is a problem, some special symbol can be uses (a gensym
   or s_unbound, for example). */
#define tagentry_p(x) (fixp(car(x)))
#define tagentry_value(x) (cdr(x))
#define tagentry_context(x) ((CONTEXT *) getfixnum(car(x)))
#define xlbindtag(c,t,e) xlpbind(cvfixnum((FIXTYPE) (c)),(t),e);
#endif


/* macros to manipulate the dynamic environment */
#define xldbind(s,v)    {xldenv = cons(cons(s,getvalue(s)),xldenv);\
                         setvalue(s,v);}
#define xlunbind(e)     {for (; xldenv != (e); xldenv = cdr(xldenv))\
                           setvalue(car(car(xldenv)),cdr(car(xldenv)));}

/* macro to manipulate dynamic and lexical environment */

#define xlbind(s,v) {if (specialp(s)) xldbind(s,v) else xlpbind(s,v,xlenv)}
#define xlpdbind(s,v,e) {e = cons(cons(s,getvalue(s)),e);\
                         setvalue(s,v);}

/* type predicates */                          
#define null(x)         ((x) == NIL)
#define atom(x)         (null(x) || ntype(x) != CONS)
#define listp(x)        (null(x) || ntype(x) == CONS)

#define consp(x)        (ntype(x) == CONS)
#define subrp(x)        (ntype(x) == SUBR)
#define fsubrp(x)       (ntype(x) == FSUBR)
#define stringp(x)      (ntype(x) == STRING)
#define symbolp(x)      (ntype(x) == SYMBOL)
#define streamp(x)      (ntype(x) == STREAM)
#define objectp(x)      (ntype(x) == OBJECT)
#define fixp(x)         (ntype(x) == FIXNUM)
#define rndstatep(x)    (ntype(x) == RNDSTATE)
#define floatp(x)       (ntype(x) == FLONUM)
#define complexp(x)     (ntype(x) == COMPLEX)
#ifdef BIGNUMS
#define ratiop(x)       (ntype(x) == RATIO)
#define bignump(x)		(ntype(x) == BIGNUM)
/* "rationalp" checks for rational numeric types */
#define rationalp(x)    (fixp(x) || bignump(x) || ratiop(x))
/* "integerp" checks for integer numeric types */
#define integerp(x)	(fixp(x) || bignump(x))
/* "realp" checks for non-complex numeric types */
#define realp(x)        (fixp(x) || floatp(x) || bignump(x) || ratiop(x))
#else
#define rationalp(x)    (fixp(x))
#define integerp(x)     (fixp(x))
#define realp(x)        (floatp(x) || fixp(x))
#endif /* BIGNUMS */
#define numberp(x)      (realp(x) || complexp(x))
#define vectorp(x)      (ntype(x) == VECTOR)
#define closurep(x)     (ntype(x) == CLOSURE)
#define charp(x)        (ntype(x) == CHAR)
#define ustreamp(x)     (ntype(x) == USTREAM)
#define structp(x)      (ntype(x) == STRUCT)
#define darrayp(x)      (ntype(x) == DARRAY)
#define adatap(x)       (ntype(x) == ADATA)  /* L. Tierney */
#define tvecp(x)        (ntype(x) == TVEC)   /* L. Tierney */
#define natptrp(x)      (ntype(x) == NATPTR)   /* L. Tierney */
#define seqp(x)         (tvecp(x) || stringp(x) || vectorp(x) || listp(x))
#ifdef BYTECODE
#define bcclosurep(x)	(ntype(x) == BCCLOSURE)
#define cpsnodep(x)	(ntype(x) == CPSNODE)
#define bcodep(x)	(ntype(x) == BCODE)
#endif /* BYTECODE */
#ifdef PACKAGES
#define packagep(x)	(ntype(x) == PACKAGE)
#define keywordp(x) (symbolp(x)&&(getpackage(x)==xlkeypack))
#endif /* PACKAGES */

#define boundp(x)       (getvalue(x) != s_unbound)
#define fboundp(x)      (getfunction(x) != s_unbound)

/* shorthand functions */
#define consa(x)        cons(x,NIL)
#define consd(x)        cons(NIL,x)

/* argument list parsing macros */
#define xlgetarg()      (testarg(nextarg()))
#define xllastarg()     {if (xlargc != 0) xltoomany();}
#define testarg(e)      (moreargs() ? (e) : xltoofew())
#define typearg(tp)     (tp(*xlargv) ? nextarg() : xlbadtype(*xlargv))
#define nextarg()       (--xlargc, *xlargv++)
#define moreargs()      (xlargc > 0)

/* macros to get arguments of a particular type */
#define xlgacons()      (testarg(typearg(consp)))
#define xlgalist()      (testarg(typearg(listp)))
#define xlgasymbol()    (testarg(typearg(symbolp)))
#define xlgastring()    (testarg(typearg(stringp)))
#define xlgastrorsym()  (testarg(symbolp(*xlargv) ? getpname(nextarg()) : typearg(stringp)))
#define xlgaobject()    (testarg(typearg(objectp)))
#define xlgafixnum()    (testarg(typearg(fixp)))
#define xlgaflonum()    (testarg(typearg(floatp)))
#define xlgachar()      (testarg(typearg(charp)))
#define xlgavector()    (testarg(typearg(vectorp)))
#define xlgastream()    (testarg(typearg(streamp)))
#define xlgaustream()   (testarg(typearg(ustreamp)))
#define xlgaclosure()   (testarg(typearg(closurep)))
#define xlgastruct()    (testarg(typearg(structp)))
#define xlgadarray()    (testarg(typearg(darrayp)))
#define xlgatvec()      (testarg(typearg(tvecp)))
#define xlganatptr()    (testarg(typearg(natptrp)))
#define xlgaseq()       (testarg(typearg(seqp)))
#ifdef BYTECODE
#define xlgacpsnode()	(testarg(typearg(cpsnodep)))
#define xlgabcode()	(testarg(typearg(bcodep)))
#endif /* BYTECODE */
#ifdef PACKAGES
#define xlgapackage()	(testarg(typearg(packagep)))
#endif /* PACKAGES */
#define xlganumber()    (testarg(typearg(numberp)))
#define xlgainteger()   (testarg(typearg(integerp)))

/* FILETABLE specification -- non-windows */
#ifdef FILETABLE
typedef struct {
    FILE *fp;
    char *tname;    /* true file name */
#ifdef _Windows
    char reopenmode[4];	    /* mode to reopen file */
    unsigned long filepos;  /* position of file */
#endif
} FILETABLETYPE;
#endif

/* function definition structure */
typedef struct {
    char *fd_name;      /* function name */
    int fd_type;        /* function type */
    LVAL (*fd_subr) _((void));  /* function entry point */
} FUNDEF;

/* execution context flags */
#define CF_GO           0x0001
#define CF_RETURN       0x0002
#define CF_THROW        0x0004
#define CF_ERROR        0x0008
#define CF_CLEANUP      0x0010
#define CF_CONTINUE     0x0020
#define CF_TOPLEVEL     0x0040
#define CF_BRKLEVEL     0x0080
#define CF_UNWIND       0x0100

#ifdef BYTECODE
/* bytecode types and structure */
typedef unsigned char bytecode;

typedef struct continuation {
  LVAL *base, *top;
  union {
    bytecode *pc;
    int entry;
  } pe;
  int vreg;
} CONTINUATION, *CONTINUATIONP;
#endif /* BYTECODE */

/* execution context */
typedef LVAL *FRAMEP;
typedef struct context {
    int c_flags;                        /* context type flags */
    LVAL c_expr;                        /* expression (type dependent) */
    XL_JMP_BUF c_jmpbuf;                /* longjmp context */
    struct context *c_xlcontext;        /* old value of xlcontext */
    LVAL **c_xlstack;			/* old value of xlstack */
    LVAL *c_xlargv;			/* old value of xlargv */
    int c_xlargc;                       /* old value of xlargc */
    LVAL *c_xlfp;			/* old value of xlfp */
    LVAL *c_xlsp;			/* old value of xlsp */
    LVAL c_xlenv;                       /* old value of xlenv */
    LVAL c_xlfenv;                      /* old value of xlfenv */
    LVAL c_xldenv;                      /* old value of xldenv */
#ifdef BYTECODE
    struct continuation *c_xlcstop;     /* old value of xlcstop */
#endif /* BYTECODE */
} CONTEXT;

/* OS system interface, *stuff file */
extern VOID oscheck _((void));  /* check for control character during exec */
extern VOID osinit _((char *banner)); /* initialize os interface */
extern VOID osfinish _((void)); /* restore os interface */
extern VOID osflush _((void));  /* flush terminal input buffer */
extern VOID osforce _((FILEP fp));  /* force file output */
extern long osrand _((long));   /* next random number in sequence */
#ifdef PATHNAMES
extern FILEP ospopen _((char *name, int ascii)); /* open file using path */
#endif
extern VOID osfinit _((void));  /* initialize os functions */
extern VOID osreset _((void));  /* reset os interface on jump to toplevel */
extern VOID xoserror _((char *msg)); /* print an error message */
extern int  ostgetc _((void));      /* get a character from the terminal */
extern VOID ostputc _((int ch));    /* put a character to the terminal */
#ifdef TIMES
extern unsigned long ticks_per_second _((void));
extern unsigned long run_tick_count _((void));
extern unsigned long real_tick_count _((void));
extern unsigned long gc_tick_count _((void));
extern unsigned long system_tick_count _((void));
#endif
extern int renamebackup _((char *filename));
#ifdef FILETABLE
extern int truename _((char *name, char *rname));
extern int osmtime _((char *fname, time_t *mtime)); /* get file modification time */
extern LVAL dirlist _((char *name));
#endif
extern VOID set_gc_cursor _((int on));
extern VOID enable_interrupts _((void));
extern VOID disable_interrupts _((void));
#ifdef MACINTOSH
extern  int macxlinit _((char *resfile));
extern VOID macloadinits _((void));
#endif /* MACINTOSH */

#ifdef BIGNUMS
/* for xlbignum.c */
extern LVAL copybignum _((LVAL x, int sign));
extern LVAL normalBignum _((LVAL x));
extern LVAL cvtulongbignum _((unsigned long n, int sign));
extern LVAL cvtfixbignum _((FIXTYPE n));
extern LVAL cvtflobignum _((FLOTYPE n));
extern int cvtbigfixnum _((LVAL x, FIXTYPE *n));
extern int comparebignum _((LVAL x, LVAL y));
extern int zeropbignum _((LVAL x));
extern FLOTYPE cvtbigflonum _((LVAL x));
extern FLOTYPE cvtbigratioflonum _((LVAL num, LVAL denom));
extern FLOTYPE cvtratioflonum _((LVAL ratio));
extern int cvtbigulong _((LVAL x, unsigned long *n));
extern LVAL cvtstrbignum _((char *s, int radix));
extern char *cvtbignumstr _((LVAL x, int radix));
extern LVAL addsubbignum _((LVAL ux, LVAL vx, int subvflag));
extern LVAL multbignum _((LVAL ux, LVAL vx));
extern LVAL divbignum _((LVAL dividend, LVAL divisor, LVAL *remainder));
#endif

/* for xlisp.c */
extern VOID xlrdsave _((LVAL expr));
extern VOID xlevsave _((LVAL expr));
extern VOID xlfatal _((char *msg));
extern VOID do_exits _((void));
extern VOID wrapup _((void));
extern int xsload _((char *name, int vflag, int pflag));

/* for xleval */
extern LVAL xlxeval _((LVAL expr));
extern VOID xlabind _((LVAL fun, int argc, LVAL *argv));
extern VOID xlfunbound _((LVAL sym));
extern VOID xlargstkoverflow _((void));
extern int  macroexpand _((LVAL fun, LVAL args, LVAL *pval));
extern int  pushargs _((LVAL fun, LVAL args));
extern LVAL makearglist _((int argc, LVAL *argv));
extern VOID xlunbound _((LVAL sym));
extern VOID xlstkoverflow _((void));

/* for xlio */
extern int xlgetc _((LVAL fptr));
extern VOID xlungetc _((LVAL fptr, int ch));
extern int xlpeek _((LVAL fptr));
extern VOID xlputc _((LVAL fptr, int ch));
extern VOID xlflush _((void));
extern VOID stdprint _((LVAL expr));
extern VOID stdputstr _((char *str));
extern VOID errprint _((LVAL expr));
extern VOID errputstr _((char *str));
extern VOID dbgprint _((LVAL expr));
extern VOID dbgputstr _((char *str));
extern VOID trcprin1 _((LVAL expr));
extern VOID trcputstr _((char *str));

/* for xlprin */
extern VOID xlputstr _((LVAL fptr, char *str));
extern VOID xlprint _((LVAL fptr, LVAL vptr, int flag));
extern VOID xlprintl _((LVAL fptr, LVAL vptr, int flag));
extern int  xlgetcolumn _((LVAL fptr));
extern int  xlfreshline _((LVAL fptr));
extern VOID xlterpri _((LVAL fptr));
extern VOID xlputstr _((LVAL fptr, char* str));
extern int read_exponent _((char *s));

/* for xljump */
extern VOID xljump _((CONTEXT *target, int mask, LVAL val));
extern VOID xlbegin _((CONTEXT *cptr, int flags, LVAL expr));
extern VOID xlend _((CONTEXT *cptr));
extern VOID xlgo _((LVAL label));
extern VOID xlreturn _((LVAL name, LVAL val));
extern VOID xlthrow _((LVAL tag, LVAL val));
extern VOID xlsignal _((char *emsg, LVAL arg));
extern VOID xltoplevel _((int print));
extern VOID xlbrklevel _((void));
extern VOID xlcleanup _((void));
extern VOID xlcontinue _((void));

/* for xllist */
extern VOID xlcircular _((void));
#ifdef HASHFCNS
extern VOID xlsetgethash _((LVAL key, LVAL table, LVAL value));
#endif
extern LVAL mklist _((int, LVAL));

/* for xlsubr */
extern int xlgetkeyarg _((LVAL key, LVAL *pval));
extern int xlgkfixnum _((LVAL key, LVAL *pval));
extern VOID xltest _((LVAL *pfcn, int *ptresult));
extern VOID xllastkey _((void));
extern int needsextension _((char *name));
extern int eql _((LVAL arg1, LVAL arg2));
extern int equal _((LVAL arg, LVAL arg2));
#ifdef KEYARG
extern LVAL xlkey _((void));
extern LVAL xlapp1 _((LVAL fun, LVAL arg));
extern int dotest1 _((LVAL arg1, LVAL fun, LVAL kfun));
extern int dotest2 _((LVAL arg1, LVAL arg2, LVAL fun, LVAL kfun));
extern int dotest2s _((LVAL arg1, LVAL arg2, LVAL fun, LVAL kfun));
#else
extern int dotest1 _((LVAL arg1, LVAL fun));
extern int dotest2 _((LVAL arg1, LVAL arg2, LVAL fun));
#endif
extern FLOTYPE makefloat _((LVAL arg));
extern LVAL cvstrornil _((char *s));
extern long lisp2long _((LVAL x));
extern LVAL long2lisp _((long x));
extern unsigned long lisp2ulong _((LVAL x));
extern LVAL ulong2lisp _((unsigned long x));
#define MAKEFLOAT(x) \
  (floatp(x) ? getflonum(x) : fixp(x) ? (double) getfixnum(x) : makefloat(x))

/* for xlobj */
extern int xlobsetvalue _((LVAL pair, LVAL sym, LVAL val));
extern int xlobgetvalue _((LVAL pair, LVAL sym, LVAL *pval));
extern VOID putobj _((LVAL fptr, LVAL obj));

/* for xlread */
extern LVAL tentry _((int ch));
extern int xlload _((char *fname, int vflag, int pflag));
extern int xlread _((LVAL fptr, LVAL *pval, int rflag, int pwflag));
extern int xlisnumber _((char *str, LVAL *pval));

/* for xlstruct */
extern LVAL xlrdstruct _((LVAL list));
extern VOID xlprstruct _((LVAL fptr, LVAL vptr, FIXTYPE plevel, int flag));

/* for xlfio */
extern VOID xlformat _((LVAL lfmt, LVAL stream));
extern LVAL getstroutput _((LVAL stream));
extern VOID write_double_efmt _((char * s, double y, int d));

/* for xltvec.c */
extern LVAL mktvec _((int n, LVAL etype));
extern int  gettvecsize _((LVAL x));
extern LVAL gettvecelement _((LVAL x, int i));
extern VOID settvecelement _((LVAL x, int i, LVAL v));
extern LVAL gettvecetype _((LVAL x));
extern int  gettveceltsize _((LVAL x));
extern VOID xlreplace P6H(LVAL, LVAL, int, int, int, int);

/* for xlarray.c */
extern FIXTYPE llength _((LVAL list));
extern LVAL coerce_to_list _((LVAL));
extern LVAL coerce_to_tvec _((LVAL, LVAL));
extern LVAL split_list _((LVAL, int));
extern LVAL checknonnegint _((LVAL));
extern LVAL nested_list_to_list _((LVAL, int));
extern LVAL nested_list_to_array _((LVAL list, int rank));
extern LVAL copylist _((LVAL list));
extern LVAL copyvector _((LVAL v));
extern LVAL array_to_nested_list _((LVAL array));
extern FIXTYPE rowmajorindex _((LVAL x, LVAL indices, int from_stack));
extern LVAL mkarray _((LVAL dim, LVAL key, LVAL key_arg, LVAL etype));

/* xlseq.c */
extern LVAL xlnreverse P1H(LVAL);

/* save/restore functions */
#ifdef SAVERESTORE
extern int xlirestore _((char *fname));
extern int xlisave _((char *fname));
#endif

/* package functions */
#ifdef PACKAGES
#define SYM_NOT_FOUND 0
#define SYM_INTERNAL  1
#define SYM_EXTERNAL  2
#define SYM_INHERITED 3

#define goodpackagep(x) (packagep(x) && ! null(getpacknames(x)))

extern VOID xlexport _((LVAL sym, LVAL pack));
extern VOID xlimport _((LVAL sym, LVAL pack));
extern LVAL xlfindpackage _((char *name));
extern int xlfindsymbol _((char *name, LVAL pack, LVAL *psym));
extern LVAL xlpackagename _((LVAL pack));
extern LVAL xlintern _((char *name, LVAL pack));
extern LVAL xlgetpackage _((LVAL arg));
extern LVAL xldelete1 _((LVAL x, LVAL y));
#endif /* PACKAGES */

/* external procedure declarations */
extern VOID obsymbols _((void));    /* initialize oop symbols */
extern VOID ossymbols _((void));    /* initialize os symbols */
extern VOID xlsymbols _((void));    /* initialize interpreter symbols */
extern VOID xloinit _((void));      /* initialize object functions */
extern VOID xlsinit _((void));      /* initialize xlsym.c */
extern VOID xlrinit _((void));      /* initialize xlread.c */
extern VOID xlminit _((void));      /* init xldmem */
extern VOID xldinit _((void));      /* initilaixe debugger */
extern  int xlinit _((char *resfile));  /* xlisp initialization routine */
extern LVAL xleval _((LVAL expr));  /* evaluate an expression */
extern LVAL xlapply _((int argc));  /* apply a function to arguments */
extern LVAL evmethod _((LVAL obj, LVAL msgcls, LVAL method)); /* evaluate a method */
extern LVAL xlsubr _((char *sname, int type, LVAL (*fcn)(void),int offset));
                                /* enter a subr/fsubr */
extern LVAL xlenter _((char *name));/* enter a symbol */
extern LVAL findprop _((LVAL list, LVAL prp)); /* find a property in list */
extern LVAL xlmakesym _((char *name));  /* make an uninterned symbol */
extern LVAL xlgetvalue _((LVAL sym));   /* get value of a symbol (checked) */
extern VOID xlsetvalue _((LVAL sym, LVAL val)); /* set the value of symbol */
extern LVAL xlxgetvalue _((LVAL sym));  /* get value of a symbol */
extern LVAL xlgetfunction _((LVAL sym));/* get functional value of a symbol */
extern LVAL xlxgetfunction _((LVAL sym));
                            /* get functional value of a symbol (checked) */
extern LVAL xlexpandmacros _((LVAL form));      /* expand macros in a form */
extern LVAL xlgetprop _((LVAL sym, LVAL prp));  /* get the value of a property */
extern VOID xlputprop _((LVAL sym, LVAL val, LVAL prp)); /*set value of property*/
extern VOID xlremprop _((LVAL sym, LVAL prp));  /* remove a property */
extern LVAL xlclose _((LVAL name, LVAL type, LVAL fargs, LVAL body, LVAL env, LVAL fenv));
                                /* create a function closure */
extern int hash _((char *str, int len));    /* Hash the string */
extern int xlhash _((LVAL obj, int len));   /* Hash anything */

/* argument list parsing functions */
extern LVAL xlgetfile _((int outflag));     /* get a file/stream argument */
extern LVAL xlgetfname _((void));   /* get a filename argument */

/* error reporting functions (don't *really* return at all) */
extern LVAL xltoofew _((void));     /* report "too few arguments" error */
extern VOID xltoomany _((void));    /* report "too many arguments" error */
extern VOID xltoolong _((void));    /* too long to process error */
extern LVAL xlbadtype _((LVAL arg));/* report "bad argument type" error */
extern LVAL xlerror _((char *emsg, LVAL arg));  /* report arbitrary error */
extern VOID xlcerror _((char *cmsg, char *emsg, LVAL arg)); /*recoverable error*/
extern VOID xlerrprint _((char *hdr,char *cmsg, char *emsg, LVAL arg));
extern VOID xlbaktrace _((int n));  /* do a backtrace */
extern VOID xlabort _((char *emsg));    /* serious error handler */
extern VOID xlfail _((char *emsg));     /* xlisp error handler */
extern VOID xlbreak _((char *emsg, LVAL arg));  /* enter break look */
extern VOID xlnoassign _((LVAL arg));   /* report assignment to constant error */
extern int xlcvttype _((LVAL arg));
extern int syminterned _((LVAL sym));
extern void xlsigint _((void));

/* complex numbers */
typedef struct {
  double r, i;
} dcomplex;

double d_sign P2H(double *, double *);
double z_abs P1H(dcomplex *);
VOID z_div P3H(dcomplex *, dcomplex *, dcomplex *);
VOID z_sqrt P2H(dcomplex *, dcomplex *);

extern LVAL xlcallsubr1 _((LVAL (*fun)(void), LVAL x));
extern LVAL xlcallsubr2 _((LVAL (*fun)(void), LVAL x, LVAL y));
extern LVAL xlapplysubr _((LVAL (*fun)(void), LVAL arg));
#ifdef BYTECODE
extern VOID bcsymbols _((void));
extern VOID init_bytecode _((void));
extern LVAL BC_evform  _((LVAL form));
extern LVAL BC_evfun _((LVAL fun, int argc, LVAL *argv));
extern LVAL xlmakebcode _((void));
extern LVAL xladd2 _((LVAL x, LVAL y));
extern LVAL xlsub2 _((LVAL x, LVAL y));
extern LVAL xlmul2 _((LVAL x, LVAL y));
extern LVAL xldiv2 _((LVAL x, LVAL y));
extern LVAL xlmin2 _((LVAL x, LVAL y));
extern LVAL xlmax2 _((LVAL x, LVAL y));
extern LVAL xllss2 _((LVAL x, LVAL y));
extern LVAL xlleq2 _((LVAL x, LVAL y));
extern LVAL xlequ2 _((LVAL x, LVAL y));
extern LVAL xlneq2 _((LVAL x, LVAL y));
extern LVAL xlgeq2 _((LVAL x, LVAL y));
extern LVAL xlgtr2 _((LVAL x, LVAL y));
extern int  num_cmp2 _((int which, LVAL x, LVAL y));
extern LVAL xladd1 _((LVAL x));
extern LVAL xlsub1 _((LVAL x));
extern LVAL slot_value _((LVAL x, LVAL y));
extern LVAL set_slot_value _((LVAL x, LVAL y, LVAL z));
#endif /* BYTECODE */

extern VOID initrndstate _((void));
extern double xlunirand _((void));

#ifdef STSZ
extern VOID stchck _((void));
#endif

#ifdef SERVER
extern int initXlisp _((char *resfile));    /* Initialize, return error code */
extern int execXlisp _((char *cmd, int restype, 
        char **resstr, LVAL * resval)); /* execute expression */
extern VOID wrapupXlisp _((void));          /* relinquish memory, quit */
#endif

extern LVAL xlparsetype _((LVAL typ));
extern int checkfeatures _((LVAL arg, int which));  /* features featuure */

#define NIL (&isnil)

/* definitions for handling NaN and Infinity on IEEE754 systems */
/**** fix to use builtin finite() and isnan() if available? */
/**** need machine.h file for UNIX to set little/bigendian */
#ifdef IEEEFP
#ifdef MACINTOSH
#define is_nan(x) ((x) != (x))
#define ieeehi(x) ((unsigned long *)(&(x)))[0]
#define is_finite(x) ((ieeehi(x) & 0x7FF00000L) != 0x7FF00000L)
#endif

#ifdef UNIX
#define is_nan(x) isnan(x)
#ifndef linux
#define is_finite(x) finite(x)
#else
/* work around bug in optimized code -- from Bernhard Walter */
#define is_finite(x) (isnan(x) ? 0 : finite(x))
#endif
#endif

#ifdef MSDOS
#ifndef UINT32
#define UINT32 unsigned long            /* unsigned 32-bit integer type */
#endif
#ifndef IEEELO                          /* assumes little endian; */
#define IEEELO 0                        /* switch these for big endian */
#define IEEEHI 1
#endif 

#define ieeehi(x) ((UINT32 *)(&(x)))[IEEEHI]
#define ieeelo(x) ((UINT32 *)(&(x)))[IEEELO]

#define is_finite(x) ((ieeehi(x) & 0x7FF00000L) != 0x7FF00000L)
#define is_nan(x) (((ieeehi(x) & 0x7FF00000L) == 0x7FF00000L) \
                   && ((ieeehi(x) & 0xFFFFFL) != 0 || ieeelo(x) != 0))
#endif
#endif

#include "xlftab.h"
#include "xlglob.h"

#define xlatexit(f) atexit(f)

/* Should be last in file: */
/* $putpatch.c$: "MODULE_XLISP_H_GLOBALS" */
#endif /* XLISP_H */
