/* xlfio.c - xlisp file i/o */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

#ifdef FILETABLE
#include <errno.h>
#ifdef MACINTOSH
#include <stat.h>
#else
#include <sys/stat.h>
#endif
#endif

/* forward declarations */
LOCAL LVAL printit P2H(int, int);
LOCAL FIXTYPE flatsize P2H(LVAL, int);
LOCAL FILEP opencmd P3H(char *, char *, int);
LOCAL VOID toomanyopt P1H(LVAL);
LOCAL char * skip_pp P3H(char *, int *, int *);
LOCAL char * decode_pp P7H(char *, FIXTYPE *, int, int *, int *, int *, LVAL);
LOCAL VOID opt_print P6H(LVAL, LVAL, int, FIXTYPE *, int, int);
LOCAL int  trimzeros P2H(char *, int);
LOCAL int  allzeros P1H(char *);
LOCAL VOID write_double_ffmt P4H(char *, double, int, int);
LOCAL VOID integer_print P5H(LVAL, LVAL, int, FIXTYPE *,int);
LOCAL VOID flonum_fprint P4H(LVAL, LVAL, FIXTYPE *,int);
LOCAL VOID flonum_eprint P4H(LVAL, LVAL, FIXTYPE *,int);
LOCAL VOID flonum_gprint P4H(LVAL, LVAL, FIXTYPE *,int);
LOCAL VOID tab_print P3H(LVAL, FIXTYPE *, int);
LOCAL VOID indirect_print P2H(LVAL, int);
LOCAL VOID case_convert_print P4H(char *, LVAL, int, int);
LOCAL VOID conditional_print P5H(char *, LVAL, FIXTYPE, int, int);
LOCAL VOID iterative_print P5H(char *, LVAL, FIXTYPE, int, int);
LOCAL char *skip_past_directive P3H(char *, int, int);

/* xread - read an expression */
/* eof-error-p added - L. Tierney */
LVAL xread(V)
{
    LVAL fptr,eof,val;
    int eof_error_p, recursive_p = FALSE;
    
    /* get file pointer and eof value */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    if (moreargs() && !null(xlgetarg())) recursive_p = TRUE;
    xllastarg();

    /* read an expression */
    if (!xlread(fptr, &val, recursive_p, FALSE)) {
        if (eof_error_p) xlfail("end of file on read");
	else val = eof;
    }

    /* return the expression */
    return (val);
}

/* TAA MOD 9/97 -- added read-preserving-whitespace */
LVAL xreadpw(V)
{
    LVAL fptr,eof,val;
    int eof_error_p, recursive_p = FALSE;
    
    /* get file pointer and eof value */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    if (moreargs() && !null(xlgetarg())) recursive_p = TRUE;
    xllastarg();

    /* read an expression */
    if (!xlread(fptr, &val, recursive_p, TRUE)) {
        if (eof_error_p) xlfail("end of file on read");
	else val = eof;
    }

    /* return the expression */
    return (val);
}

/* xprint - built-in function 'print' */
LVAL xprint(V)
{
    return (printit(TRUE,TRUE));
}

/* xprin1 - built-in function 'prin1' */
LVAL xprin1(V)
{
    return (printit(TRUE,FALSE));
}

/* xprinc - built-in function princ */
LVAL xprinc(V)
{
    return (printit(FALSE,FALSE));
}

/* xfreshline - start a new line if not at begining of line */
LVAL xfreshline(V)
{
    LVAL fptr;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* optionally terminate the print line and return action */
    return (xlfreshline(fptr)? s_true : NIL);
}


/* xterpri - terminate the current print line */
LVAL xterpri(V)
{
    LVAL fptr;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* terminate the print line and return nil */
    xlterpri(fptr);
    return (NIL);
}

/* printit - common print function */
LOCAL LVAL printit P2C(int, pflag, int, tflag)
{
    LVAL fptr,val;

    /* get expression to print and file pointer */
    val = xlgetarg();
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

#ifndef OLDPRINT /* fixed to make PRINT correspond the CL specs - L. Tierney */
    /* terminate the previous line if necessary */
    if (tflag) xlterpri(fptr);
#endif /* OLDPRINT */
    
    /* print the value */
    xlprint(fptr,val,pflag);

#ifndef OLDPRINT
    /* print space if needed */
    if (tflag) xlputc(fptr, ' ');
#endif /* OLDPRINT */

#ifdef OLDPRINT
    /* terminate the print line if necessary */
    if (tflag)
	xlterpri(fptr);
#endif /* OLDPRINT */
    /* return the result */
    return (val);
}

/* xflatsize - compute the size of a printed representation using prin1 */
LVAL xflatsize(V)
{
  /* TAA MOD -- rewritten to use a USTREAM 1/21/97 */
  LVAL val;

  /* get the expression */
  val = xlgetarg();
  xllastarg();

  return (cvfixnum(flatsize(val, TRUE)));
}

/* xflatc - compute the size of a printed representation using princ */
LVAL xflatc(V)
{
  /* TAA MOD -- rewritten to use a USTREAM 1/21/97 */
  LVAL val;

  /* get the expression */
  val = xlgetarg();
  xllastarg();

  return (cvfixnum(flatsize(val, FALSE)));
}

/* flatsize - compute the size of a printed expression */
LOCAL FIXTYPE flatsize P2C(LVAL, val, int, pflag)
{
  /* TAA MOD -- rewritten to use a USTREAM 1/21/97 */
  LVAL ustream;
  FIXTYPE size;

  /* create and protect the stream */
  ustream = newustream();
  xlprot1(ustream);

  /* print the value to compute its size */
  xlprint(ustream,val,pflag);

  /* calculate size */
  for (size = 0, ustream = gethead(ustream);
       !null(ustream);
       size++, ustream = cdr(ustream)) ;

  /* unprotect */
  xlpop();
    
  /* return the length of the expression */
  return (size);
}

enum ACTIONS {A_NIL, A_ERR, A_REN, A_OVER, A_APP, A_SUPER, A_CREATE};

LOCAL FILEP opencmd P3C(char *, name, char *, mode, int, binary)
{
  if (binary) return OSBOPEN(name, mode);
  else return OSAOPEN(name, mode);
}

/* xopen - open a file */
LVAL xopen(V)
{
#ifdef BIGNUMS
    FIXTYPE nbits = 0;
#endif
    char *name;         /* file name strings */
    FILEP fp;           /* opened file pointer */
    LVAL fname;         /* file name string LVAL */
    LVAL temp;          /* key arguments */
    int iomode;         /* file mode, as stored in node */
    enum ACTIONS exist; /* exist action */
    enum ACTIONS nexist;/* non-exist action */
    int binary;

    /* get file name */
    name = getstring(fname = xlgetfname());

    /* get direction */
    if (xlgetkeyarg(k_direction,&temp) && temp != k_input) {
        if (temp == k_output) iomode = S_FORWRITING;
        else if (temp == k_io) iomode = S_FORREADING|S_FORWRITING;
        else if (temp == k_probe) iomode = 0;
        else goto argerror;
    }
    else iomode = S_FORREADING;

    /* get type */

    if (xlgetkeyarg(k_elementtype,&temp) && temp != a_char ) {
#ifdef BIGNUMS
      int notsigned = TRUE;
      nbits = 8;		/* default size */
      if (temp == a_sbyte) notsigned = FALSE;
      else if (consp(temp) && car(temp) == a_sbyte &&
	       consp(cdr(temp)) && fixp(car(cdr(temp))) &&
	       null(cdr(cdr(temp)))) {
	nbits = getfixnum(car(cdr(temp)));
	notsigned = FALSE;
      }
      else if (consp(temp) && car(temp) == a_unbyte &&
	       consp(cdr(temp)) && fixp(car(cdr(temp))) &&
	       null(cdr(cdr(temp)))) {
	nbits = getfixnum(car(cdr(temp)));
      }
      else if (temp != a_unbyte && temp != a_fixnum)
	goto argerror;
      if (nbits < 0 || (nbits & 7) != 0 || nbits > /*32L* */MAXVECLEN)
	goto argerror;		/* invalid value for number of bits */
      if (iomode) iomode |= (notsigned ? S_BINARY|S_UNSIGNED : S_BINARY);
      binary = TRUE;
#else
      if (temp == a_fixnum ) {
	if (iomode) iomode |= S_BINARY; /* mark as binary file type */
	binary = TRUE;
      }
      else goto argerror;
#endif
    }
    else
      binary = FALSE;

    /* get exists action */
    if (xlgetkeyarg(k_exist, &temp) &&
        (iomode & S_FORWRITING) &&  /* ignore value if :input or :probe */
        temp != k_rename && temp != k_newversion) {
        if (null(temp)) exist = A_NIL;
        else if (temp == k_error) exist = A_ERR;
        else if (temp == k_overwrite) exist = A_OVER;
        else if (temp == k_append) exist = A_APP;
        else if (temp == k_supersede || temp == k_rendel)
            exist = A_SUPER;
        else goto argerror;
}
    else exist = A_REN;

    /* get non-exist action */

    if (xlgetkeyarg(k_nexist, &temp)) {
        if (null(temp)) nexist = A_NIL;
        else if (temp == k_error) nexist = A_ERR;
        else if (temp == k_create) nexist = A_CREATE;
        else goto argerror;
    }
    else {  /* handle confusing mess of defaults */
        if (iomode == S_FORREADING || exist == A_OVER || exist == A_APP)
            nexist = A_ERR;
        else if (iomode & S_FORWRITING) nexist = A_CREATE;
        else nexist = A_NIL;
    }

    xllastkey();

    /* attempt to open the file */

    if ((fp = opencmd(name,
		      (iomode & S_FORWRITING) ? OPEN_UPDATE : OPEN_RO,
		      binary))!=CLOSED) {
        /* success! */
        if (iomode & S_FORWRITING) switch (exist) { /* do exist action */
            case A_ERR: /* give error */
                OSCLOSE(fp);
                xlerror("file exists", fname);
                break;
            case A_REN: /* create new version */
                OSCLOSE(fp);
                fp = CLOSED;
                if (!renamebackup(name))
                    xlerror("couldn't create backup file", fname);
                break;
            case A_APP: /* position to end of file */
                OSSEEKEND(fp);
                break;
            case A_SUPER:   /* supersede file */
                OSCLOSE(fp);
                fp = CLOSED;
                break;
            case A_NIL:     /* return NIL */
                OSCLOSE(fp);
                return NIL;
            /*case A_OVER:*/    /* overwrite -- does nothing special */
            default: ;
        }
    }
    else {  /* file does not exist */
        switch (nexist) {
            case A_ERR: /* give error */
                xlerror("file does not exist", fname);
                break;
            case A_NIL:     /* return NIL */
                return NIL;
            /*case A_CREATE:*/  /* create a new file */
            default: ;
        }
    }

    /* we now create the file if it is not already open */
    if (fp == CLOSED)
        if ((fp = opencmd(name,
			  (iomode&S_FORREADING)? CREATE_UPDATE: CREATE_WR,
			  binary)) == CLOSED)
            xlerror("couldn't create file", fname);

    /* take concluding actions */
    if (iomode == 0) { /* probe */
        OSCLOSE(fp);
        fp = CLOSED;
    }

#ifdef BIGNUMS
    temp = cvfile(fp, iomode);
    temp->n_bsiz = (short)(unsigned short)(nbits/8);
    return temp;
#else
    return cvfile(fp,iomode);
#endif
    argerror: xlerror("invalid argument", temp);
    return NIL;
}


/* xfileposition - get position of file stream */
LVAL xfileposition(V)
{
    long j,fsize;
    double i;
    int t = 0;
    LVAL pos, fptr;
    FILEP fp;
    /* get file pointer */
    fp = getfile(fptr = xlgastream());

    /* make sure the file exists */
    if (fp == CLOSED)
        xlfail("file not open");

    /* get current position, adjusting for posible "unget" */
    j = OSTELL(fp) + (getsavech(fptr) ? -1L : 0L);

    if (moreargs()) { /* must be set position */
        pos = xlgetarg();
        xllastarg();
        if (pos == k_end) t=OSSEEKEND(fp);
        else if (pos == k_start) t=OSSEEK(fp,0L);
        else if (fixp(pos)) {   /* check for in range, then position */
            /* STDIO allows positioning beyond end of file, so we must check
                the file size (boo his!) */
            i = getfixnum(pos);
#ifdef BIGNUMS
            if (fptr->n_sflags & S_BINARY) i *= fptr->n_bsiz;
#endif
            t = OSSEEKEND(fp);
            fsize = OSTELL(fp);
            if (t == 0 && fp != CONSOLE && (i < 0 || i > fsize)) {
                OSSEEK(fp,j);
                xlerror("position outside of file", pos);
            }
            t = OSSEEK(fp, (long)i);
        }
        else xlbadtype(pos);

        setsavech(fptr,'\0');   /* toss unget character, if any */
        fptr->n_sflags &= ~(S_READING|S_WRITING);
                                /* neither reading or writing currently */
        /* t is non-zero if couldn't do seek */
        return (t != 0 || fp == CONSOLE ? NIL : s_true);
    }

#ifdef BIGNUMS
    return ((j == -1L || fp == CONSOLE) ? NIL :
            cvfixnum(fptr->n_sflags & S_BINARY ? j/fptr->n_bsiz : j));
#else
    return ((j == -1L || fp == CONSOLE) ? NIL : cvfixnum(j));
#endif
}

/* xfilelength - returns length of file */
LVAL xfilelength(V)
{
#ifdef BIGNUMS
    LVAL stream;
#endif
    FILEP fp;
    long i,j;

    /* get file pointer */
#ifdef BIGNUMS
    fp = getfile(stream = xlgastream());
#else
    fp = getfile(xlgastream());
#endif
    xllastarg();

    /* make sure the file exists */
    if (fp == CLOSED)
        xlfail("file not open");

    /* not all stdio packages will catch the following gaffe */
    if (fp == CONSOLE) return NIL;

    if ((i=OSTELL(fp)) == -1L ||
        OSSEEKEND(fp) ||
        (j = OSTELL(fp)) == -1L ||
        OSSEEK(fp,i)) {
        return NIL;
    }

#ifdef BIGNUMS
    return cvfixnum(stream->n_sflags & S_BINARY ? j/stream->n_bsiz : j);
#else
    return cvfixnum(j);
#endif
}

#ifdef FILETABLE
/* xfilemtime - returns modification time of file */
LVAL xfilemtime(V)
{
  LVAL fname;
  char *str;
  time_t mtime;
  
  str = getstring(fname = xlgetfname());
  xllastarg();

  if (osmtime(str, &mtime))
  	xlerror("can't get modification time", fname);
  
  if ((double) mtime > (double) MAXFIX)
    return cvflonum((FLOTYPE) mtime);
  else
    return cvfixnum((FIXTYPE) mtime);
}

/* xrenamefile - renames file */
LVAL xrenamefile(V)
{
  LVAL oldname, newname;
  char *oldstr, *newstr;
  
  oldstr = getstring(oldname = xlgetfname());
  newstr = getstring(newname = xlgastring());
  xllastarg();

  /* in applec and THINK_C rename fails if the new file exists, */
  /* so remove it first */
  if ((remove(newstr) != 0 && errno == EACCES) ||
       rename(oldstr, newstr) != 0)
  	xlerror("can't rename file", oldname);
  /**** need to fix the truename of open file streams */
  /**** need to add multiple return values */
  
  return newname;
}
#endif /* FILETABLE */

LVAL xforceoutput(V)
{
  LVAL fptr;

  fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
  xllastarg();

  if (streamp(fptr)) osforce(getfile(fptr));

  return(NIL);
}


#ifdef FILETABLE
LVAL xtruename(V)
{
    LVAL f = xlgetfname();
    char namebuf[FNAMEMAX+1];

    xllastarg();


    STRCPY(buf, getstring(f));

    if (!truename(buf, namebuf)) xlerror("strange file name", f);

    return cvstring(namebuf);
}

LVAL xdeletefile(V)
{
    LVAL arg;
    FILEP fp;

    /* get the argument */

    arg = xlgetarg();
    xllastarg();

    if (streamp(arg) && getfile(arg) > CONSOLE) {
        /* close file first */
        fp = getfile(arg);
        STRCPY(buf, filetab[fp].tname);
        OSCLOSE(fp);
        setsavech(arg, '\0');
        setfile(arg,CLOSED);
    }
    else {
        if (symbolp(arg)) arg = getpname(arg);
        else if (!stringp(arg)) xlbadtype(arg);

        if (getslength(arg) >= FNAMEMAX)
            xlerror("file name too long", arg);

        STRCPY(buf,getstring(arg));
    }
    if (remove(buf) != 0 && errno == EACCES)
        xlerror("cannot delete file", arg);

    return s_true;
}

LVAL xbasedir()
{
  LVAL name;

  name = xlgastring();
  xllastarg();

  return dirlist(getstring(name));
}

LVAL xfiletype()
{
  struct stat s;
  char *name = getstring(xlgastring());
  xllastarg();
  if (stat(name, &s) != 0)
    return NIL;
  else if (S_ISDIR(s.st_mode))
    return xlenter(":DIRECTORY");
  else if (S_ISCHR(s.st_mode))
    return xlenter(":CHARACTER-SPECIAL");
  else if (S_ISBLK(s.st_mode))
    return xlenter(":BLOCK-SPECIAL");
  else if (S_ISREG(s.st_mode))
    return xlenter(":REGULAR");
  else if (S_ISFIFO(s.st_mode))
    return xlenter(":FIFO");
  else
    return xlenter(":UNKNOWN");
}
#endif

/* xclose - close a file */
LVAL xclose(V)
{
    LVAL fptr;
    FILEP fp;   /* TAA MOD to allow closing closed files,
                    prohibit closing the console, return the correct
                    values (true on success), and close string streams */


    /* get file pointer */
    fptr = xlgetarg();
    xllastarg();

    /* handle string stream case by converting to a closed file! */
    if (ustreamp(fptr)) {
        fptr->n_type = STREAM;
        setfile(fptr, CLOSED);
        setsavech(fptr, '\0');
        return (s_true);
    }

    /* give error of not file stream */
    if (!streamp(fptr)) xlbadtype(fptr);


    /* make sure the file exists */

    if ((fp = getfile(fptr)) == CLOSED || fp == CONSOLE)
        return (NIL);

    /* close the file */
    OSCLOSE(fp);
    setsavech(fptr, '\0');
    setfile(fptr,CLOSED);

    /* return true */
    return (s_true);
}

/* xrdchar - read a character from a file */
/* eof, eof-error-p added - L. Tierney */
LVAL xrdchar(V)
{
    LVAL fptr, eof;
    int ch, eof_error_p;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    if (moreargs()) xlgetarg(); /* remove and ignore recursive-p argument */
    xllastarg();

    /* get character and check for eof */
    ch = xlgetc(fptr);
    if (ch == EOF && eof_error_p) xlfail("end of file on read");
    return (ch == EOF ? eof : cvchar(ch));
}

#ifdef BIGENDIAN
#define bs(n) (n) /* byte select in short */
#else
#define bs(n) ((n)^1)
#endif

/* xrdbyte - read a byte from a file */
/* eof, eof-error-p added - L. Tierney */
#ifdef BIGNUMS
LVAL xrdbyte(V)
{
  LVAL fptr, eof, val;
  BIGNUMDATA *vx;
  unsigned char *v;
  int ch, eof_error_p, i, size, ofs;
  FIXTYPE temp;

  /* get file pointer */
  fptr = xlgastream();
  if ((fptr->n_sflags & S_BINARY) == 0)
    xlfail("not a binary file");
  eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
  eof = (moreargs() ? xlgetarg() : NIL);
  xllastarg();

  if (fptr->n_bsiz == 1) {	/* file of bytes */
    ch = xlgetc(fptr);
    if (ch == EOF && eof_error_p) xlfail("end of file on read");
    return(ch == EOF ? eof :
	   ((fptr->n_sflags&S_UNSIGNED) ? cvfixnum((FIXTYPE)ch)
	    : cvfixnum((FIXTYPE)(/*signed*/ char)ch)));
  }
  else {			/* file of more than that */
    size = (fptr->n_bsiz+sizeof(BIGNUMDATA)-1)/sizeof(BIGNUMDATA);
    /* size of bignum needed */
    if (size < 2) size = 2;
    ofs = size*sizeof(BIGNUMDATA) - fptr->n_bsiz; /* unused bytes */
    val = newbignum(size);
    vx = getbignumarray(val)+1;	/* point to data array start */
    v = (unsigned char *)vx;
#ifdef BIGENDIANFILE
    for (i = ofs; i < size*sizeof(BIGNUMDATA); i++)
#else
      for (i = size*sizeof(BIGNUMDATA)-1; i >= ofs; i--)
#endif
	{
	  ch = xlgetc(fptr);
	  if (ch == EOF) {
	    if (eof_error_p) xlfail("end of file on read");
	    else return eof;
	  }
	  v[bs(i)] = (unsigned char)ch;
	}
    if ((/*signed*/ char)(v[bs(ofs)]) < 0 && (fptr->n_sflags&S_UNSIGNED)==0)
      {
	/* we need to handle negative number */
	unsigned long sum;
	int carry = 1;
	vx[-1] = 1;
	for (i = ofs-1; i >=0; i--) v[bs(i)] = 0xff;
	for (i = size-1; i >= 0; i--) {
	  sum = (unsigned long)(BIGNUMDATA)(~vx[i]) + carry;
	  carry = (int)(sum >> 16);
	  vx[i] = (BIGNUMDATA)sum;
	}
      }
    val = normalBignum(val);    /* normalize in case of leading zeroes */
    return (cvtbigfixnum(val, &temp) ? cvfixnum(temp) : val);
  }
}
#else
LVAL xrdbyte(V)
{
    LVAL fptr, eof;
    int ch, eof_error_p;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    xllastarg();

    /* get character and check for eof */
    ch = xlgetc(fptr);
    if (ch == EOF && eof_error_p) xlfail("end of file on read");
    return(ch == EOF ? eof : cvfixnum((FIXTYPE)ch));
}
#endif

/* xpkchar - peek at a character from a file */
/* eof, eof-error-p added */
LVAL xpkchar(V)
{
    LVAL flag,fptr,eof;
    int ch,eof_error_p;

    /* peek flag and get file pointer */
    flag = (moreargs() ? xlgetarg() : NIL);
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    if (moreargs()) xlgetarg(); /* remove and ignore recursive-p argument */
    xllastarg();

    /* skip leading white space and get a character */
    if (!null(flag))
	while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
	    xlgetc(fptr);
    else
	ch = xlpeek(fptr);

    /* return the character */
    if (ch == EOF && eof_error_p) xlfail("end of file on read");
    return (ch == EOF ? eof : cvchar(ch));
}

/* xwrchar - write a character to a file */
LVAL xwrchar(V)
{
    LVAL fptr,chr;

    /* get the character and file pointer */
    chr = xlgachar();
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* put character to the file */
    xlputc(fptr,getchcode(chr));

    /* return the character */
    return (chr);
}

/* xwrbyte - write a byte to a file */
#ifdef BIGNUMS
/* we will continue XLISP's tradition of not checking for value
   to write being out of range. At any rate, this will save time. */
LVAL xwrbyte()
{
  LVAL fptr,chr,chr2;
  BIGNUMDATA *vx;
  unsigned char *v;
  int size, i, ofs;

  /* get the byte and file pointer */
  chr = xlgetarg();
  if (!(fixp(chr) || bignump(chr))) xlbadtype(chr);
  fptr = xlgastream();
  if ((fptr->n_sflags & S_BINARY) == 0)
    xlfail("not a binary file");
  xllastarg();

  /* can't really do an unsigned write of a negative number */
  if ((fptr->n_sflags&S_UNSIGNED) &&
      ((fixp(chr)&&getfixnum(chr)<0) || (bignump(chr)&&getbignumsign(chr))))
    xlerror("Can't do unsigned write-byte of", chr);
	

  if (fptr->n_bsiz == 1 && fixp(chr)) { /* handle easy case */
    /* put byte to the file */
    xlputc(fptr,(int)getfixnum(chr));
    return (chr);
  }
  /* work only with bignums from now on */
  if (fixp(chr)) chr2 = cvtfixbignum(getfixnum(chr));
  else chr2 = chr;
  vx = getbignumarray(chr2);
  size = getbignumsize(chr2) * sizeof(BIGNUMDATA); /* number size in bytes */
  ofs =  fptr->n_bsiz - size;	/* number of excess bytes to write */
  if (*vx++) {			/* negative value */
#ifdef BIGENDIANFILE
    int j;
    v = (unsigned char *)vx;
    for (i = ofs; i > 0; i--) xlputc(fptr, 0xff); /* filler */
    for (i = size-1; i >= -ofs && i >= 0; i--) { /* find end of carries */
      if (v[bs(i)] != 0) {	/* only zeroes will generate carries */
	for (j = (ofs >= 0 ? 0 : -ofs); j < i; j++) {
	  xlputc(fptr, (unsigned char) (~v[bs(j)]));
	}
	break;
      }
    }
    for (; i < size; i++) xlputc(fptr, 1 + (unsigned char)(~v[bs(i)]));
#else
    unsigned sum;
    int carry=1;
    v = (unsigned char *)vx;
    for (i = size-1; i >= -ofs && i >= 0; i--) {
      sum = (unsigned)(unsigned char)~v[bs(i)] + carry;
      carry = sum >> 8;
      xlputc(fptr, (unsigned char) sum);
    }
    for (i = ofs; i > 0; i--) xlputc(fptr, 0xff); /* filler */
#endif
  }
  else {			/* postive value */
    v = (unsigned char *)vx;
#ifdef BIGENDIANFILE
    for (i = ofs; i > 0; i--) xlputc(fptr, 0); /* filler */
    for (i = (ofs >= 0 ? 0 : -ofs); i < size; i++) xlputc(fptr, v[bs(i)]);
#else
    for (i = size-1; i >= -ofs && i >= 0; i--) xlputc(fptr, v[bs(i)]);
    for (i = ofs; i > 0; i--) xlputc(fptr, 0); /* filler */
#endif
  }

  /* return the byte */
  return (chr);
}
#else
LVAL xwrbyte(V)
{
    LVAL fptr,chr;

    /* get the byte and file pointer */
    chr = xlgafixnum();
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* put byte to the file */
    xlputc(fptr,(int)getfixnum(chr));

    /* return the character */
    return (chr);
}
#endif

/* xreadline - read a line from a file */
/* eof, eof-error-p added - L. Tierney */
LVAL xreadline(V)
{
    char *p, *sptr;
    LVAL fptr,str,newstr,eof;
    int len,blen,ch,eof_error_p;

    /* protect some pointers */
    xlsave1(str);

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    if (moreargs()) xlgetarg(); /* remove and ignore recursive-p argument */
    xllastarg();

    /* get character and check for eof */
    len = blen = 0; p = buf;
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n') {

        /* check for buffer overflow TAA MOD to use MEMCPY instead of strcat*/
	if (blen >= STRMAX) {
	    newstr = newstring(len + STRMAX);
	    sptr = getstring(newstr);
	    if (str != NIL) MEMCPY(sptr, getstring(str), len);
            MEMCPY(sptr+len, buf, blen);
	    p = buf; blen = 0;
	    len += STRMAX;
	    str = newstr;
	}

	/* store the character */
	*p++ = ch; ++blen;
    }

    /* check for end of file */
    if (len == 0 && p == buf && ch == EOF) {
	xlpop();
	if (eof_error_p) xlfail("end of file on read");
	return (eof);
    }

    /* append the last substring */
    /* conditional removed because code always executes! */
    newstr = newstring(len + blen);
    sptr = getstring(newstr);
    if (str != NIL) MEMCPY(sptr, getstring(str), len);
    MEMCPY(sptr+len, buf, blen);
    sptr[len+blen] = '\0';
    str = newstr;

    /* restore the stack */
    xlpop();

    /* return the string */
    return (str);
}

/* xunrdchar - unread a character from a file */
LVAL xunrdchar(V)
{
    LVAL fptr,chr;

    /* get the character and file pointer */
    chr = xlgachar();
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* unread character from the file */
    xlungetc(fptr,getchcode(chr));

    /* return the character */
    return (NIL);
}

/* xmkstrinput - make a string input stream */
/* TAA MOD - reworked for unsigned lengths */
LVAL xmkstrinput(V)
{
    unsigned start,end=0,len,i;
    FIXTYPE temp;
    char *str;
    LVAL string,val;

    /* protect the return value */
    xlsave1(val);
    
    /* get the string and length */
    string = xlgastring();
    str = getstring(string);
    len = getslength(string);

    /* get the starting offset */
    if (moreargs()) {
	val = xlgafixnum();
	temp = getfixnum(val);
	if (temp < 0 || temp > len)
	    xlerror("string index out of bounds",val);
	start = (unsigned) temp;
    }
    else start = 0;

    /* get the ending offset */
    if (moreargs()) {       /* TAA mod to allow NIL for end offset */
      val = nextarg();
      if (null(val)) end = len;
      else if (fixp(val)) {
	temp = getfixnum(val);
	if (temp < start || temp > len)
	  xlerror("string index out of bounds",val);
	end = (unsigned) temp;
      }
      else xlbadtype(val);

      xllastarg();
    }
    else end = len;

    /* make the stream */
    val = newustream();

    /* copy the substring into the stream */
    for (i = start; i < end; ++i)
	xlputc(val,str[i]);

    /* restore the stack */
    xlpop();

    /* return the new stream */
    return (val);
}

/* xmkstroutput - make a string output stream */
LVAL xmkstroutput(V)
{
    return (newustream());
}

/* xgetstroutput - get output stream string */
LVAL xgetstroutput(V)
{
    LVAL stream;
    stream = xlgaustream();
    xllastarg();
    return (getstroutput(stream));
}

/* xgetlstoutput - get output stream list */
LVAL xgetlstoutput(V)
{
    LVAL stream,val;

    /* get the stream */
    stream = xlgaustream();
    xllastarg();

    /* get the output character list */
    val = gethead(stream);

    /* empty the character list */
    sethead(stream,NIL);
    settail(stream,NIL);

    /* return the list */
    return (val);
}

#define FMTMAX 256
LOCAL VOID toomanyopt P1C(LVAL, fmt)
{
    xlerror("too many prefix parameters in format",fmt);
}

/* decode prefix parameters and modifiers for a format directive */
/* TAA MOD Entirely rewritten -- return value -1 for unassigned since
   negative numbers are inappropriate for all arguments we are concerned
   with. Also clips args to reasonable values, allows both : and @ modifiers
   at once. */
LOCAL char *decode_pp P7C(char *,    fmt,
                          FIXTYPE *, pp,     /* prefix parameters */
                          int,       maxnpp, /* maximum number of them */
                          int *,     npp,    /* actual number of them */
                          int *,     colon,  /* colon modifier given? */
                          int *,     atsign, /* atsign modifier given? */
                          LVAL,      lfmt)   /* format string for failure */
{
    int i;
    int gotone = 0;
    FIXTYPE accum;

    for (i = 0; i < maxnpp; i++) pp[i] = -1;    /* initially all undefined */
    *npp = 0;
    *colon = 0;
    *atsign = 0;
    do {
        if (*fmt == '\'') { /* character code */
            pp[*npp] = *(++fmt);
            gotone = 1;
            fmt++;
        }
	else if (*fmt == '#') { /* xlargc is value */
	    accum = xlargc;
	    if (accum>FMTMAX) accum = FMTMAX;
            pp[*npp] = accum;
            gotone = 1;
            fmt++;
	}
        else if (*fmt == 'v' || *fmt == 'V') { /* lisp arg is value */
	    LVAL arg = xlgetarg();
	    if (fixp(arg)) {
	        accum = getfixnum(arg);
		if (accum < 0) accum = 0;   /* clip at reasonable values */
		else if (accum>FMTMAX) accum = FMTMAX;
	    }
	    else if (charp(arg))
	        accum = getchcode(arg);
	    else {
	        if (! null(arg))
		    xlbadtype(arg);
		accum = -1;
	    }
            pp[*npp] = accum;
            gotone = 1;
            fmt++;
        }
        else if (isdigit(*fmt)) { /* integer literal */
            accum = 0;
            do {
                accum = accum*10 + (int)(*fmt++ - '0');
                if (accum > FMTMAX)
                    accum = FMTMAX; /* Clip at reasonable value */
            } while (isdigit(*fmt));
            gotone = 1;
            pp[*npp] = accum;
        }
	else if (*fmt == '#') {     /* use number of remaining arguments */
	  pp[*npp] = xlargc;
	  gotone = 1;
	  fmt++;
	}
        else if (*fmt == ',') {     /* empty field */
            gotone = 1;
        }
        else  break;                /* nothing to process */

        if (*fmt != ',') break;         /* no comma -- done */
        *npp += 1;                  /* got an argument */
        fmt++;                          /* toss comma */
        if( *npp >= maxnpp ) toomanyopt(lfmt);
    } while (TRUE);
    *npp += gotone;

    do {    /* pick up any colon or atsign modifier */
        if (*fmt == ':') *colon = 1;
        else if (*fmt == '@') *atsign = 1;
        else break;
        fmt++;
    } while (TRUE);
    return fmt;
}

#define mincol  pp[0]
#define colinc  pp[1]
#define minpad  pp[2]
#define padchar pp[3]

/* opt_print - print a value using prefix parameter options */
LOCAL VOID opt_print P6C(LVAL,      stream,
                         LVAL,      val,
                         int,       pflag,   /* quoting or not */
                         FIXTYPE *, pp,      /* prefix parameters */
                         int,       colon,   /* colon modifier given? */
                         int,       atsign)  /* at-sign modifier given? */
{
    int flatsiz = 0;
    int i;

    if (mincol < 0) mincol = 0; /* handle default values */
    if (colinc < 1) colinc = 1;    /* also arg of 0 for colinc */
    if (minpad < 0) minpad = 0;
    if (padchar < 0) padchar = ' ';

    if( mincol < minpad )
            mincol = minpad;

    if (mincol > 0) {
        /* we will need to pad, so must calculate flat size */
        if (colon && null(val))         /* flat size is 2 */
            flatsiz = 2;
        else
	    flatsiz = (int)flatsize(val, pflag);
	if (atsign) {       /* padding may be required on left */
	    for( i = 0; i < minpad; flatsiz++, i++ )
	        xlputc(stream,(int)padchar);
	    while( flatsiz < mincol ) {
	        for( i = 0; i < colinc; i++ )
		    xlputc(stream,(int)padchar);
		flatsiz += (int)colinc;
	    }
        }
    }

    /* print the value */
    if( colon && null(val))
        xlputstr(stream,"()");
    else
        xlprint(stream,val,pflag);

    if( mincol > 0 && !atsign ) {       /* padding required on right */
        for( i = 0; i < minpad; flatsiz++, i++ )
            xlputc(stream,(int)padchar);
        while( flatsiz < mincol ) {
            for( i = 0; i < colinc; i++ )
                xlputc(stream,(int)padchar);
            flatsiz += (int)colinc;
        }
    }
}
#undef round
#define round pp[1]
LOCAL VOID integer_print P5C(LVAL,      stream,
                             LVAL,      val,
                             int,       pflag,   /* Style */
                             FIXTYPE *, pp,      /* prefix parameters */
                             int,       atsign)  /* at-sign modifier given? */
{
#ifdef BIGNUMS
  char *bufr;
  FIXTYPE radix = 10;
#endif
  int fillchar, i;

#ifdef BIGNUMS
  if (pflag == 'R') {
    radix = pp[0];
    if (radix < 2 || radix > 36)
      xlerror("bad radix specified", cvfixnum(radix));
    pp++;
  }
#endif

  fillchar = (int)pp[1];

  if (fillchar < 0) fillchar = ' ';

#ifdef BIGNUMS
  /* can't print in binary or arbitrary radix with printf */
  if (fixp(val) && (pflag == 'B' || pflag == 'R')) {
    if (getfixnum(val) == 0) pflag = 'D'; /* print zero in "decimal" */
    else val = cvtfixbignum(getfixnum(val));
  }
#endif

  if (fixp(val)) { /* D O or X and fixnum */
    FIXTYPE v = getfixnum(val); /* TAA mod 3/95 to handle @X and @O
				   and negative values with X and O */
    switch (pflag) {
    case 'D':
      sprintf(buf, (atsign?"%+ld":"%ld"), v);
      break;
    case 'O':
      if (v < 0) sprintf(buf, "-%lo", -v);
      else sprintf(buf, (atsign ? "+%lo" : "%lo"), v);
      break;
    case 'X':
      if (v < 0) sprintf(buf, "-%lx", -v);
      else sprintf(buf, (atsign ? "+%lx" : "%lx"), v);
      break;
    }
    if (mincol > 0) {   /* need to fill */
      for (i = (int)mincol-strlen(buf); i-- > 0;)
	xlputc(stream,fillchar);
    }
    xlputstr(stream,buf);
    return;
  }
#ifdef BIGNUMS
  else if (bignump(val)) { /* D O or X and bignum */
    switch (pflag) {
    case 'D': radix = 10; break;
    case 'X': radix = 16; break;
    case 'B': radix = 2; break;
    case 'O': radix = 8; break;
    }
    bufr = cvtbignumstr(val, (int)radix);
    if (atsign && getbignumsign(val)) atsign = 0; /* add leading "+"? */
    if (mincol > 0) {       /* need to fill */
      for (i = (int)mincol - atsign - STRLEN(bufr); i-- > 0;)
	xlputc(stream,fillchar);
    }
    if (atsign) xlputc(stream, '+');
    xlputstr(stream,bufr);
    MFREE(bufr);
    return;
  }
#endif
  else {     /* not a number */
    padchar = colinc = minpad = -1; /* zap arg if provided */
    opt_print(stream,val,FALSE,pp,0,0);
    return;
  }
}

/*
Floating point formatting has been modified (L. Tierney) to correspond
more closely to CL. All prefix parameters for ~F, ~E and ~G except the
scale parameter k are supported. (I don't have much use for k, and it
would be hard to add since it can be negative.) The routines operate
by letting sprintf do the formatting work and parsing the results it
produces. IEEE NaN's and Infinities are recognized by checking for an
alpha character as the first character in the printed representation
of fabs(num). Also, I have tried to write all inequalities in such a
way as to insure that Infinities and NaN's should only be handled in
the ~E directive.  Most reasonable cases seem to be handled correctly,
but I may have gotten some of the extreme cases wrong (w = 1, d = 0 and
that sort of stuff). There seems to be some disagreement among CL
implementations on these.
*/

/* trim trailing zeros in fractional part; return length of fraction left */
LOCAL int trimzeros P2C(char *, s, int, skip_one)
{
  int d, dd, i;

  /* locate the decimal point */
  for (i = 0, d = -1; s[i] != 0; i++) {
    if (s[i] == '.') {
      d = i;
      break;
    }
  }

  /* drop trailing zeros if decimal point is found */
  if (d != -1) {
    dd = (skip_one) ? d + 1 : d;
    for (i = d + 1; s[i] != 0 && isdigit(s[i]); i++);
    s[i] = 0;
    for (i--; i > dd && s[i] == '0'; i--)
      s[i] = 0;
    return(strlen(s+d+1));
  }
  else return(0);
}

LOCAL int allzeros P1C(char *, s)
{
  for (; *s != 0; s++)
    if (*s != '.' && *s != '0')
      return(FALSE);
  return(TRUE);
}

LOCAL VOID write_double_ffmt P4C(char *, s, double, y, int, e, int, d)
{
  char cmd[50];
  int f, i, m;

  f = e + d;
  if (f > 16) f = 16;
  sprintf(cmd, "%%.%de", f > 0 ? f : 0);
  sprintf(s, cmd, y);

  /* re-read exponent in case changed by rounding */
  if (read_exponent(strchr(s, 'e') + 1) > e) {
    s[f > 0 ? f + 2 : 2] = '0'; /* extend fractional part with a zero */
    f++;                    /* increment length of fractional part */
    e++;                    /* increment exponent */
  }

  s[1] = '.';               /* make sure decimal point is there */
  s[f > 0 ? f + 2 : 2] = 0; /* terminate the string */

  if (e >= 0) {
    m = e > f ? f : e;               /* shift the decimal point */
    MEMMOVE(s + 1, s + 2, m);
    f -= m;
    s[e + 1] = '.';
    for (i = m + 1; i < e + 1; i++)  /* insert zeros */
      s[i] = '0';
    if (f < d) {                     /* add trailing zeros if needed */
      for (i = e + f + 2; i < e + d + 2; i++)
	s[i] = '0';
      s[e + d + 2] = 0;
    }
  }
  else if (f >= 0) {
    MEMMOVE(s + 2 - e, s + 2, f);  /* shift fractional part */
    s[1 - e] = s[0];               /* move leading digit */
    s[0] = '0';                    /* set leading digit to zero */
    for (i = 2; i < 1 - e; i++)    /* insert zeros if needed */
      s[i] = '0';
    s[2 + f - e] = 0;              /* terminate string */
  }
  else {
    int ld = s[0];                 /* round up leading digit if needed */
    s[0] = (d == 0 && e == -1 && ld >= '5') ? '1' : '0';
    s[1] = '.';
    for (i = 2; i < d + 1; i++)    /* insert zeros if needed */
      s[i] = '0';
    if (d > 0)                     /* round up final digit if needed */
      s[d + 1] = (f == -1 && ld >= '5') ? '1' : '0';
    s[d + 2] = 0;                  /* terminate string */
  }
}

VOID write_double_efmt P3C(char *, s, double, y, int, d)
{
  char cmd[50];
  int Finite;

  /* tries to use d - 1 digits if the result reads in as equal to y */
  sprintf(cmd, "%%.%de", d > 0 ? d - 1 : 0);
  sprintf(s, cmd, y);
#ifdef IEEEFP
  if (! is_finite(y))
    sprintf(s, is_nan(y) ? "NaN" : "Inf");
#endif /* IEEEFP */
  Finite = isdigit(s[0]);
  if (d > 0 && Finite) {
    double n;
    sscanf(s, "%lf", &n);
    if (n != y) {
      sprintf(cmd, "%%.%de", d);
      sprintf(s, cmd, y);
    }
  }
}

LOCAL VOID flonum_fprint P4C(LVAL,   stream,
                             LVAL,   val,
                             FIXTYPE *, pp,  /* prefix parameters */
                             int,    atsign) /* at-sign modifier given? */
{
  if (! realp(val)) {   /* not a real number */
    padchar = colinc = minpad = -1; /* zap arg if provided */
    opt_print(stream,val,FALSE,pp,0,0);
    return;
  }
  else {
    FLOTYPE num = makefloat(val);
    FLOTYPE y = fabs(num);
    int needsign = (atsign || num < 0.0) ? TRUE : FALSE;
    int w = pp[0], d = pp[1];
    int overflowchar = pp[3], fillchar = pp[4];
    int i, rw, intsize, fracsize, exponent, len, haved;
    char *p;

#ifdef __SASC__
    /* IBM 370 floating pt format; the largest number isn't */
    /* quite so large... - Dave Rivers (rivers@ponds.uucp) */
    if (y == 0.0 || (y > 1e-100 && y < 1e75))
#else
    if (y == 0.0 || (y > 1e-100 && y < 1e100))
#endif
      {
      /* don't generate extra big number */
      /* test should be false for Infinity, NaN */

      /* control width and decimals */
      if (w > 100) w = 100;
      if (d > 100) d = 100;

      /* compute the sizes of the integer and fractional parts */
      if (y == 0.0) {
	exponent = 0;
	intsize = 0;
	fracsize = 1;
      }
      else {
	write_double_efmt(buf, y, 16);
	p = strchr(buf, 'e');
	if (p != NULL) {
	  exponent = read_exponent(p + 1);
	  intsize = (exponent >= 0) ? 1 + exponent : 0;
	  i = trimzeros(buf, FALSE);
	  fracsize = (exponent < i) ? i - exponent : 1;
	}
	else { /* should not happen */
	  exponent = intsize = fracsize = 0;
	}
      }
      if (d == 0 && intsize == 0) intsize = 1;

      /* if d is given, check for overflow; otherwise, compute d */
      if (d >= 0) {
	haved = TRUE;
	if (w >= 0) {
	  rw = (needsign) ? d + 2 : d + 1;
	  rw += intsize;
	  if (rw > w) {
	    if (overflowchar >= 0) goto overflow;
	    else w = rw;
	  }
	}
      }
      else {
	haved = FALSE;
	if (w >= 0) {
	  d = w - 1 - intsize;
	  if (needsign) d--;
	  if ((intsize == 0 && d < 1) || d < 0) {
	    if (overflowchar >= 0) goto overflow;
	    else {
	      d = (intsize == 0) ? 1 : 0;
	      w = (needsign) ? intsize + 2 + d : intsize + 1 + d;
	    }
	  }
	}
	else {
	  d = fracsize;
	}
	write_double_ffmt(buf, y, exponent, d);
	if (y == 0.0 || ! allzeros(buf))
	  d = trimzeros(buf, TRUE);
      }

      /* write number using computed d */
      write_double_ffmt(buf, y, exponent, d);

      /* fiddle it if no leading zero or rounded to be too long */
      p = buf;
      len = strlen(buf);
      if (w >= 0 && len > ((needsign) ? w - 1 : w)) {
	if (p[0] == '0')
	  p++;
	else if (p[len - 1] == '0' && ! haved)
	  p[len - 1] = 0;
	else if (overflowchar >= 0)
	  goto overflow;
      }

      /* if w is supplied, output pad characters */
      if (w >= 0) {
	if (fillchar < 0) fillchar = ' ';
	i = w - strlen(p);
	if (needsign) i--;
	while (i-- > 0)
	  xlputc(stream,fillchar);
      }

      /* print the sign if needed */
      if (num < 0) xlputc(stream, '-');
      else if (atsign) xlputc(stream, '+');

      /* output number */
      xlputstr(stream,p);
      return;
    }
    else if (w >= 0 && overflowchar >= 0) goto overflow;
    else {
      /* do E format */
      for (i = 0; i < 7; i++) pp[i] = -1; /* zap any arguments */
      flonum_eprint(stream, val, pp, atsign);
      return;
    }

    /* handle overflows */
  overflow:
    for (i = 0; i < w; i++) xlputc(stream, overflowchar);
  }
}

LOCAL VOID flonum_eprint P4C(LVAL,      stream,
                             LVAL,      val,
                             FIXTYPE *, pp,     /* prefix parameters */
                             int,       atsign) /* at-sign modifier given? */
{
  if (! realp(val)) {   /* not a real number */
    padchar = colinc = minpad = -1; /* zap arg if provided */
    opt_print(stream,val,FALSE,pp,0,0);
  }
  else {
    FLOTYPE num = makefloat(val);
    FLOTYPE y = fabs(num);
    int needsign = (atsign || num < 0.0) ? TRUE : FALSE;
    int w = pp[0], d = pp[1], dd = pp[1], e = pp[2], ee = pp[2];
    int overflowchar = pp[4], fillchar = pp[5], expchar = pp[6];
    int i, rw, fracsize, expsize, exponent, finite;
    char cmd[50], *p;

    /* control width and decimals */
    if (w > 100) w = 100;
    if (d > 100) d = 100;

    /* compute the sizes of the parts */
    if (y == 0.0) {
      finite = TRUE;
      fracsize = 1;
      expsize = 1;
      exponent = 0;
    }
    else {
      write_double_efmt(buf, y, d >= 0 ? d : 16);
      finite = isdigit(buf[0]);
      for (p = buf; *p == '.' || isdigit(*p); p++);
      if (finite && isalpha(*p)) {
	exponent = read_exponent(p + 1);
	fracsize = trimzeros(buf, TRUE);
	sprintf(buf, "%+d", exponent);
	expsize = strlen(buf) - 1;
      }
      else {
	fracsize = strlen(buf);
	e = exponent = expsize = 0;
      }
    }

    /* set the fill character */
    if (fillchar < 0) fillchar = ' ';

    /* handle non-finite numbers */
    if (! finite) {
      if (w >= 0) {
	i = strlen(buf);
	if (needsign) i++;
	if (w < i && overflowchar >= 0) goto overflow;
	else
	  for (i = w - i; i-- > 0;)
	    xlputc(stream, fillchar);
      }
      if (num < 0.0) xlputc(stream, '-');
      else if (atsign) xlputc(stream, (num > 0.0) ? '+' : fillchar);
      xlputstr(stream,buf);
      return;
    }

    /* check for exponent overflow and adjust e */
    if (e >= 0) {
      if (expsize > e) {
	if (w >= 0 && overflowchar >= 0) goto overflow;
	else e = expsize;
      }
    }
    else e = expsize;

    /* if d is given, check for overflow; otherwise, compute d */
    if (d >= 0) {
      if (w >= 0) {
	rw = (needsign) ? d + e + 5 : d + e + 4;
	if (rw > w) {
	  if (overflowchar >= 0) goto overflow;
	  else w = rw;
	}
      }
    }
    else {
      if (w >= 0) {
	d = w - e - 4;
	if (needsign) d--;
	if (d < 0) {
	  if (overflowchar >= 0) goto overflow;
	  else {
	    d = 0;
	    w = (needsign) ? e + 5 : e + 4;
	  }
	}
      }
      else {
	d = fracsize;
      }
      sprintf(cmd, "%%.%de", d);
      sprintf(buf, cmd, y);

      /* adjust and recheck the exponent */
      for (p = buf; *p == '.' || isdigit(*p); p++);
      if (isalpha(*p)) {
	exponent = read_exponent(p + 1);
	if (dd < 0) d = trimzeros(buf, TRUE);
	sprintf(buf, "%+d", exponent);
	expsize = strlen(buf) - 1;
	if (expsize > e) {
	  if (ee >= 0 && expsize > ee && w >= 0 && overflowchar >= 0)
	    goto overflow;
	  else e = expsize;
	}
      }
    }

    /* write number using computed and modified d */
    sprintf(cmd, "%%.%de", d);
    sprintf(buf, cmd, y);

    /* remove the exponent and fiddle it if nothing after the decimal */
    if (d == 0) {
      buf[1] = '.';
      buf[2] = 0;
    }
    else buf[2 + d] = 0;

    /* if w is supplied, output pad characters */
    if (w >= 0) {
      i = w - e - 2 - strlen(buf);
      if (needsign) i--;
      while (i-- > 0)
	xlputc(stream,fillchar);
    }

    /* print the sign if needed */
    if (num < 0) xlputc(stream, '-');
    else if (atsign) xlputc(stream, '+');

    /* output number */
    xlputstr(stream,buf);
    
    /* print the exponent */
    xlputc(stream, (expchar >= 0) ? expchar : 'E');
    xlputc(stream, (exponent >= 0) ? '+' : '-');
    for (i = e - expsize; i > 0; i--) xlputc(stream, '0');
    sprintf(buf, "%d", (exponent >= 0) ? exponent : -exponent);
    xlputstr(stream, buf);
    return;

    /* handle overflows */
  overflow:
    for (i = 0; i < w; i++) xlputc(stream, overflowchar);
  }
}

LOCAL VOID flonum_gprint P4C(LVAL,      stream,
                             LVAL,      val,
                             FIXTYPE *, pp,     /* prefix parameters */
                             int,       atsign) /* at-sign modifier given? */
{
  int fillchar;

  fillchar = (int)pp[2];

  if (fillchar < 0) fillchar = ' ';

  if (! realp(val)) {   /* not a real number */
    padchar = colinc = minpad = -1; /* zap arg if provided */
    opt_print(stream,val,FALSE,pp,0,0);
  }
  else {
    FLOTYPE num = makefloat(val);
    FLOTYPE y = fabs(num);
    int w = pp[0], d = pp[1], e = pp[2];
    int overflowchar = pp[4], fillchar = pp[5];
    int i, fracsize, expsize, exponent, finite;
    int ww, dd, ee, q, n;
    char *p;

    /* control width and decimals */
    if (w > 100) w = 100;
    if (d > 100) d = 100;

    /* compute the sizes of the parts */
    if (y == 0.0) {
      finite = TRUE;
      fracsize = 0;
      expsize = 1;
      exponent = 0;
    }
    else {
      write_double_efmt(buf, y, 16);
      finite = isdigit(buf[0]);
      for (p = buf; *p == '.' || isdigit(*p); p++);
      if (finite && isalpha(*p)) {
	exponent = read_exponent(p + 1);
	fracsize = trimzeros(buf, FALSE);
	sprintf(buf, "%+d", exponent);
	expsize = strlen(buf) - 1;
      }
      else {
	fracsize = strlen(buf);
	exponent = expsize = 0;
      }
    }

    /* compute n such that 10^(n-1) <= y < 10^n, with n = 0 for y = 0 */
    if (y == 0.0) n = 0;
    else n = exponent + 1;

    /* compute ee */
    ee = (e >= 0) ? e + 2 : 4;

    /* compute ww */
    ww = (w >= 0) ? w - ee : -1;

    /* compute d, if not supplied, and dd */
    if (d < 0) {
      q = 1 + fracsize;
      i = (n > 7) ? 7 : n;
      d = (q > i) ? q : i;
    }
    dd = d - n;

    /* print the number using F or E format */
    if (finite && 0 <= dd && dd <= d) {
      /* use F format */
      pp[0] = ww;
      pp[1] = (dd == 0 || pp[1] >= 0) ? dd : -1; /* to get zeros trimmed */
      /*pp[1] = dd;*/
      pp[2] = -1;
      pp[3] = overflowchar;
      pp[4] = fillchar;
      flonum_fprint(stream, val, pp, atsign);
      if (w >= 0)
	for (i = 0; i < ee; i++)
	  xlputc(stream, ' ');
    }
    else {
      /* use E format */
      pp[1] = (pp[1] >= 0) ? d : -1; /* to get zeros trimmed */
      /*pp[1] = d;*/
      flonum_eprint(stream, val, pp, atsign);
    }
  }
}

#undef colinc
/* tabulate */
LOCAL VOID tab_print P3C(LVAL, stream, FIXTYPE *, pp, int, atsign)
{
    int pos = xlgetcolumn(stream);  /* where are we now??? */
    int count;                      /* number of spaces to insert */
    int column = (int)pp[0];        /* desired column */
    int colinc = (int)pp[1];        /* desired column increment */

    if (column < 0) column = 1; /* handle defaults */
    if (colinc < 0) colinc = 1;

    if (atsign) { /* relative */
        if (colinc == 0) colinc = 1;
        count = column + (colinc - (pos + column) % colinc) % colinc;
    }
    else { /* absolute */
        if (pos >= column) {
            if (colinc > 0) {
                int k = (pos+ (colinc-1) - column)/colinc;
                count = column-pos + k*colinc;
                if (count==0) count = colinc;
            }
            else count = 0;
        }
        else count = column - pos;
    }
    while (count-- > 0)
        xlputc(stream, ' ');
}

LOCAL VOID indirect_print P2C(LVAL, stream, int, atsign)
{
  LVAL *oldargv, lfmt, args;
  int oldargc;

  lfmt = xlgastring();

  if (atsign) xlformat(lfmt, stream);
  else {
    args = xlgalist();
    oldargv = xlargv;
    oldargc = xlargc;
    xlargv = xlsp;
    for (xlargc = 0; consp(args); args = cdr(args), xlargc++)
      pusharg(car(args));
    xlformat(lfmt, stream);
    xlargv = oldargv;
    xlargc = oldargc;
  }
}

/* adapted from changecase in xlstr.c */
LOCAL VOID case_convert_print P4C(char *, fmt, LVAL, stream, int, colon, int, atsign)
{
  LVAL tmp;
  LVAL lfmt;
  int ch, fcn;
  int lastspace = TRUE;

  xlstkcheck(2);
  xlsave(lfmt);
  xlsave(tmp);

  lfmt = cvstring(fmt);
  tmp = newustream();

  xlformat(lfmt, tmp);

  if (colon && atsign) fcn = 'U';
  else if (colon) fcn = 'C';
  else if (atsign) fcn = 'S';
  else fcn = 'D';

  while ((ch = xlgetc(tmp)) != EOF) {
    switch (fcn) {
    case 'U':	if (ISLOWER(ch)) ch = TOUPPER(ch); break;
    case 'D':	if (ISUPPER(ch)) ch = TOLOWER(ch); break;
    case 'C':   if (lastspace && ISLOWER(ch)) ch = TOUPPER(ch);
                if (!lastspace && ISUPPER(ch)) ch = TOLOWER(ch);
                lastspace = !ISLOWERA(ch) && !ISUPPER(ch);
                break;
    case 'S':   if (lastspace && ISLOWER(ch)) ch = TOUPPER(ch);
                if (!lastspace && ISUPPER(ch)) ch = TOLOWER(ch);
                if (ISUPPER(ch)) lastspace = FALSE;
                break;      
    }
    xlputc(stream, ch);
  }

  xlpopn(2);
}

LOCAL VOID conditional_print P5C(char *, fmt, LVAL, stream,
                                 FIXTYPE, count, int, colon, int, atsign)
{
  LVAL lfmt;
  char *oldfmt;

  xlsave1(lfmt);
  
  lfmt = cvstring(fmt);

  if (atsign) {
    if (! null(xlgetarg())) {
      xlargv--;
      xlargc++;
      xlformat(lfmt, stream);
    }
  }
  else if (colon) {
    if (! null(xlgetarg())) {
      fmt = skip_past_directive(fmt, ';', FALSE);
      if (fmt == NULL) xlerror("missing 'true' clause", lfmt);
      lfmt = cvstring(fmt);
    }
    xlformat(lfmt, stream);
  }
  else {
    if (count < 0) count = getfixnum(xlgafixnum());
    oldfmt = fmt;
    while (count-- > 0) {
      fmt = skip_past_directive(fmt, ';', FALSE);
      if (fmt == NULL) break;
    }
    if (fmt == NULL)
      fmt = skip_past_directive(oldfmt, ';', TRUE);
    if (fmt != NULL) {
      lfmt = cvstring(fmt);
      xlformat(lfmt, stream);
    }
  }
    
  xlpop();
}
  
#define MAXNPP  7

/* this does not support the termination directive ~^ */
LOCAL VOID iterative_print P5C(char *,  fmt,
                               LVAL,    stream,
                               FIXTYPE, count,
                               int,     colon,
                               int,     atsign)
{
  LVAL lfmt, args = NIL, alist;
  LVAL *oldargv = NULL, *oldsp = NULL;
  int oldargc = 0, once;
  int npp;            /* number of prefix parameters */
  FIXTYPE pp[MAXNPP];     /* list of prefix parameters */
  int tcolon, tatsign;

  xlsave1(lfmt);
  
  lfmt = cvstring(fmt);
  once = (skip_past_directive(fmt, '}', TRUE) == NULL) ? FALSE : TRUE;
  if (*fmt == '~' &&
      *decode_pp(fmt + 1, pp, MAXNPP, &npp, &tcolon, &tatsign, lfmt) == '}')
    lfmt = xlgastring();
  if (! atsign) args = xlgetarg();

  if (! atsign || colon) {
    oldargv = xlargv;
    oldargc = xlargc;
    oldsp = xlsp;
    xlargv = xlsp;
    xlargc = 0;
  }
  
  if (colon) {
    if (atsign) {
      for (; (oldargc > 0 || once) && count != 0; oldargc--, count--) {
	once = FALSE;
	alist = *oldargv++;
	xlargc = 0;
	xlargv = oldsp;
	xlsp = oldsp;
	for (; consp(alist); alist = cdr(alist)) {
	  pusharg(car(alist));
	  xlargc++;
	}
	xlformat(lfmt, stream);
      }
    }
    else {
      for (; (consp(args) || once) && count != 0; args = cdr(args), count--) {
	once = FALSE;
	alist = car(args);
	xlargc = 0;
	xlargv = oldsp;
	xlsp = oldsp;
	for (; consp(alist); alist = cdr(alist)) {
	  pusharg(car(alist));
	  xlargc++;
	}
	xlformat(lfmt, stream);
      }
    }
  }
  else {
    if (! atsign) {
      for (; consp(args); args = cdr(args)) {
	pusharg(car(args));
	xlargc++;
      }
    }
    while ((xlargc > 0 || once) && count-- != 0) {
      once = FALSE;
      if (--xlsample <= 0) {
	xlsample = SAMPLE;
	oscheck();
      }
      xlformat(lfmt, stream);
    }
  }

  if (! atsign || colon) {
    xlargv = oldargv;
    xlargc = oldargc;
    xlsp = oldsp;
  }
  xlpop();
}

/* skip prefix parameters and modifiers for a format directive */
LOCAL char *skip_pp P3C(char *,   fmt,
                        int *,    colon,  /* colon modifier given? */
                        int *,    atsign) /* atsign modifier given? */
{
  *colon = 0;
  *atsign= 0;
  do {
    if (*fmt == '\'') fmt += 2; /* character code */
    else if (*fmt == '#') fmt++; /* xlargc is value */
    else if (*fmt == 'v' || *fmt == 'V') fmt++; /* lisp arg is value */
    else if (isdigit(*fmt)) /* integer literal */
      do { fmt++; } while (isdigit(*fmt));
    else if (*fmt == ',') {     /* empty field */
    }
    else  break;                /* nothing to process */

    if (*fmt != ',') break;         /* no comma -- done */
    fmt++;                          /* toss comma */
  } while (TRUE);

  do {    /* pick up any colon or atsign modifier */
    if (*fmt == ':') *colon = 1;
    else if (*fmt == '@') *atsign = 1;
    else break;
    fmt++;
  } while (TRUE);
  return fmt;
}

LOCAL char *skip_past_directive P3C(char *, fmt,
                                    int,    tch,
                                    int,    want_colon)
{
  int ch;
  int colon, atsign;  /* : and @ modifiers given? */
  int nesting = 0;

  /* process the format string */
  while ((ch = *fmt++) != 0)
    if (ch == '~') {
      fmt = skip_pp(fmt, &colon, &atsign);
      ch = *fmt++;
      if (! nesting && (! want_colon || colon) && ch == tch)
	return(fmt);
      switch (ch) {
      case '[':
      case '(':
      case '{':
	nesting++;
	break;
      case ']':
      case ')':
      case '}':
	nesting--;
	break;
      }
      if (nesting < 0) break;
    }
  return (NULL);
}

/* xlformat - formatted output function */
/* TAA MOD 6/22/93 -- split out from xformat so routine can
   be called internally by xerror() and xcerror() */
VOID xlformat P2C(LVAL, lfmt, LVAL, stream)
{
  int ch;
  int npp;            /* number of prefix parameters */
  FIXTYPE pp[MAXNPP];     /* list of prefix parameters */
  int colon, atsign;  /* : and @ modifiers given? */
  char *fmt = getstring(lfmt);
  LVAL *oldargv;
  int oldargc;

  oldargv = xlargv;
  oldargc = xlargc;

  /* process the format string */
  while ((ch = *fmt++) != 0)
    if (ch == '~') {
      fmt = decode_pp( fmt, pp, MAXNPP, &npp, &colon, &atsign, lfmt);
      ch = *fmt++;
      if (ISLOWER7(ch)) ch = toupper(ch);
      switch (ch) {
      case '\0':
	xlerror("expecting a format directive",cvstring(fmt-1));
      case 'A':
	opt_print(stream,xlgetarg(),FALSE,pp,colon,atsign);
	break;
      case 'S':
	opt_print(stream,xlgetarg(),TRUE,pp,colon,atsign);
	break;
#ifdef BIGNUMS
      case 'R':
        if (npp > 3) toomanyopt(lfmt);
        integer_print(stream,xlgetarg(),ch,pp,atsign);
        break;
      case 'B':
#endif
      case 'D':
      case 'O':
      case 'X':
	if (npp > 4) toomanyopt(lfmt);
	integer_print(stream,xlgetarg(),ch,pp,atsign);
	break;
      case 'E':
	if (npp > 7) toomanyopt(lfmt);
	flonum_eprint(stream,xlgetarg(),pp,atsign);
	break;
      case 'F':
	if (npp > 5) toomanyopt(lfmt);
	flonum_fprint(stream,xlgetarg(),pp,atsign);
	break;
      case 'G':
	if (npp > 7) toomanyopt(lfmt);
	flonum_gprint(stream,xlgetarg(),pp,atsign);
	break;
      case '&':
	if ( pp[0] < 0 ) pp[0] = 1;
	if ((pp[0])-- > 0)
	  xlfreshline(stream);
	while( (pp[0])-- > 0 )
	  xlterpri(stream);
	break;
      case '*':
	if (npp > 1) toomanyopt(lfmt);
	if (atsign) {
	  if (pp[0] < 0) pp[0] = 0;
	  if (pp[0] > oldargc) xltoofew();
	  xlargc = oldargc - (int)pp[0];
	  xlargv = oldargv + (int)pp[0];
	}
	else if (colon) {
	  if (pp[0] < 0) pp[0] = 1;
	  if (pp[0] > oldargc - xlargc) xltoofew();
	  xlargc += (int)pp[0];
	  xlargv -= (int)pp[0];
	}
	else {
	  if (pp[0] < 0) pp[0] = 1;
	  if (pp[0] > xlargc) xltoofew();
	  xlargc -= (int)pp[0];
	  xlargv += (int)pp[0];
	}
	break;
      case 'T':
	tab_print(stream,pp,atsign);
	break;
      case '%':
	if( pp[0] < 0 ) pp[0] = 1;
	while( (pp[0])-- > 0 )
	  xlterpri(stream);
	break;
      case '~':
	if( pp[0] <= 0 ) pp[0] = 1;
	while( (pp[0])-- > 0 )
	  xlputc(stream,'~');
	break;
      case '\n':
	if( colon )
	  break;
	if( atsign )
	  xlterpri(stream);
	while (*fmt && *fmt != '\n' && isspace(*fmt))
	  ++fmt;
	break;
      case '?':
	indirect_print(stream, atsign);
	break;
      case '|':
	if (pp[0] < 0) pp[0] = 1;
	while ((pp[0])-- > 0)
	  xlputc(stream, '\f');
	break;
      case '(':
	case_convert_print(fmt, stream, colon, atsign);
	fmt = skip_past_directive(fmt, ')', FALSE);
	if (fmt == NULL) xlerror("incomplete ( directive", lfmt);
	break;
      case '[':
	conditional_print(fmt, stream, pp[0], colon, atsign);
	fmt = skip_past_directive(fmt, ']', FALSE);
	if (fmt == NULL) xlerror("incomplete [ directive", lfmt);
	break;
      case '{':
	iterative_print(fmt, stream, pp[0], colon, atsign);
	fmt = skip_past_directive(fmt, '}', FALSE);
	if (fmt == NULL) xlerror("incomplete { directive", lfmt);
	break;
      case ';':
      case ')':
      case ']':
      case '}':
	return;
      default:
	xlerror("unknown format directive",cvstring(fmt-1));
      }
    }
    else
      xlputc(stream,ch);
}

/* xformat - formatted output function */
LVAL xformat(V)
{
    LVAL stream,val;
    LVAL lfmt;

    xlsave1(val);                       /* TAA fix */

    /* get the stream and format string */
    stream = xlgetarg();
    if (null(stream)) {
	val = stream = newustream();
    }
    else {
	if (stream == s_true)
	    stream = getvalue(s_stdout);
                                        /* fix from xlispbug.417 */
        else if (streamp(stream)) {     /* copied from xlgetfile() */
                if (getfile(stream) == CLOSED)
                        xlfail("file not open");
        }
        else if (!ustreamp(stream))
	    xlbadtype(stream);
	val = NIL;
    }

    lfmt=xlgastring();
    
    /* go do it! */
    xlformat(lfmt, stream);

    /* get string if output to string */
    if (!null(val)) val = getstroutput(val);
        
    /* unprotect */
    xlpop();

    /* return the value */
    return val;
}


/* getstroutput - get the output stream string (internal) */
LVAL getstroutput P1C(LVAL, stream)
{
    char *str;
    LVAL next,val;
    unsigned len;           /* TAA MOD */
    int ch;

    /* compute the length of the stream */
    for (len = 0, next = gethead(stream); consp(next); next = cdr(next)) {
      ++len;
      /****if (len > MAXSLEN)
	xltoolong();*/   /* TAA MOD addition for overflow detect */
    }

    /* create a new string */
    xlprot1(stream);
    val = newstring(len);
    xlpop();
    
    /* copy the characters into the new string */
    str = getstring(val);
    while ((ch = xlgetc(stream)) != EOF)
	*str++ = ch;
    *str = '\0';

    /* return the string */
    return (val);
}
