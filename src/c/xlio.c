/* xlio - xlisp i/o routines */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* xlgetc - get a character from a file or stream */
int xlgetc P1C(LVAL, fptr)
{
    LVAL lptr,cptr=NULL;
    FILEP fp;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (ustreamp(fptr)) {
	if ((lptr = gethead(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) || (cptr = car(lptr)) == NIL || !charp(cptr))
		xlfail("bad stream");
	    sethead(fptr,lptr = cdr(lptr));
	    if (lptr == NIL)
		settail(fptr,NIL);
	    ch = getchcode(cptr);
	}
    }

    /* otherwise, check for a buffered character */
    else if ((ch = getsavech(fptr)) != 0)
	setsavech(fptr,'\0');

    /* otherwise, check for terminal input or file input */
    else {
	fp = getfile(fptr);
        if (fp == CLOSED)   /* TAA MOD -- give error */
            xlfail("can't read closed stream");
	else if (fp == CONSOLE)
            /* TAA MOD -- revamped for redirecting */
	    ch = ostgetc();
        else {
	  if ((fptr->n_sflags & S_FORREADING) == 0)
	    xlerror("can't read write-only file stream", fptr);
	  if ((fptr->n_sflags & S_READING) == 0) {
	    /* possible direction change*/
	    if (fptr->n_sflags & S_WRITING) {
	      OSSEEKCUR(fp,0L);
	    }
	    fptr->n_sflags |= S_READING;
	    fptr->n_sflags &= ~S_WRITING;
	  }
#ifdef OSAGETC
	  ch = (fptr->n_sflags & S_BINARY) ? OSGETC(fp) : OSAGETC(fp);
#else
	  ch = OSGETC(fp);
#endif
	}
    }

    /* return the character */
    return (ch);
}

/* xlungetc - unget a character */
VOID xlungetc P2C(LVAL, fptr, int, ch)
{
    LVAL lptr;
    
    /* check for ungetc from nil, or ungetc of EOF */
    if (fptr == NIL || ch == EOF)
	;
	
    /* otherwise, check for ungetc to a stream */
    else if (ustreamp(fptr)) {
	    lptr = cons(cvchar(ch),gethead(fptr));
	    if (gethead(fptr) == NIL)
		settail(fptr,lptr);
	    sethead(fptr,lptr);
    }

    /* otherwise, it must be a file */
    else
	setsavech(fptr,ch);
}

/* xlpeek - peek at a character from a file or stream */
int xlpeek P1C(LVAL, fptr)
{
    LVAL lptr,cptr=NULL;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (ustreamp(fptr)) {
	if ((lptr = gethead(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) || (cptr = car(lptr)) == NIL || !charp(cptr))
		xlfail("bad stream");
	    ch = getchcode(cptr);
	}
    }

    /* otherwise, get the next file character and save it */
    else {
	ch = xlgetc(fptr);
        if (ch != EOF) setsavech(fptr,ch);  /* TAA MOD -- don't save EOF! */
    }

    /* return the character */
    return (ch);
}

/* xlputc - put a character to a file or stream */
VOID xlputc P2C(LVAL, fptr, int, ch)
{
    LVAL lptr;
    FILEP fp;

    /* TAA MOD -- delete output to NIL and character counting 1/97 */
    /* check for output to an unnamed stream */
    if (ntype(fptr) == USTREAM) {	/* TAA MOD, was ustreamp() */
	lptr = consa(cvchar((unsigned char)ch));
	if (gettail(fptr)!=NIL)
	    rplacd(gettail(fptr),lptr);
	else
	    sethead(fptr,lptr);
	settail(fptr,lptr);
    }

    /* otherwise, check for terminal output or file output */
    else {
	fp = getfile(fptr);
        if (fp == CLOSED)   /* TAA MOD -- give error */
            xlfail("can't write closed stream");
	if (fp == CONSOLE)  /* TAA MOD -- for redirecting */
	    ostputc(ch);
	else {
	  if ((fptr->n_sflags & S_FORWRITING) == 0)
	    xlerror("can't write read-only file stream", fptr);
	  if ((fptr->n_sflags & S_WRITING) == 0) {
	    /* possible direction change*/
	    if (fptr->n_sflags & S_READING) {
	      OSSEEKCUR(fp,
                        (getsavech(fptr)?(setsavech(fptr,'\0'),-1L):0L));
	    }
	    fptr->n_sflags |= S_WRITING;
	    fptr->n_sflags &= ~S_READING;
#ifdef BIGNUMS
	    if ((fptr->n_sflags & S_BINARY) == 0)
#endif
	    fptr->n_cpos = 0;   /* best guess */
	  }
#ifdef BIGNUMS
	  if ((fptr->n_sflags & S_BINARY) == 0) {
#endif
	  if (ch == '\n') fptr->n_cpos = 0;
	  else fptr->n_cpos++;
#ifdef BIGNUMS
	}
#endif
#ifdef OSAGETC
	  if (((fptr->n_sflags & S_BINARY) ?
	       OSPUTC(ch,fp) : OSAPUTC(ch,fp)) == EOF)
	    /* TAA MOD to check for write to RO file */
	    xlerror("write failed", fptr);
#else
	  if (OSPUTC(ch,fp)==EOF) /* TAA MOD to check for write to RO file*/
	    xlerror("write failed", fptr);
#endif
        }
    }
}

/* xlflush - flush the input buffer */
VOID xlflush(V)
{
    setsavech(getvalue(s_termio), '\0');    /* TAA mod -- added 10/93 */
    osflush();
}

/* stdprint - print to *standard-output* */
VOID stdprint P1C(LVAL, expr)
{
    xlprint(getvalue(s_stdout),expr,TRUE);
    xlterpri(getvalue(s_stdout));
}

/* stdputstr - print a string to *standard-output* */
VOID stdputstr P1C(char *, str)
{
    xlputstr(getvalue(s_stdout),str);
}

/* errprint - print to *error-output* */
VOID errprint P1C(LVAL, expr)
{
    xlprint(getvalue(s_stderr),expr,TRUE);
    xlterpri(getvalue(s_stderr));
}

/* errputstr - print a string to *error-output* */
VOID errputstr P1C(char *, str)
{
    xlputstr(getvalue(s_stderr),str);
}

/* dbgprint - print to *debug-io* */
VOID dbgprint P1C(LVAL, expr)
{
    xlprint(getvalue(s_debugio),expr,TRUE);
    xlterpri(getvalue(s_debugio));
}

/* dbgputstr - print a string to *debug-io* */
VOID dbgputstr P1C(char *, str)
{
    xlputstr(getvalue(s_debugio),str);
}

/* trcprin1 - print to *trace-output* */
VOID trcprin1 P1C(LVAL, expr)
{
    xlprint(getvalue(s_traceout),expr,TRUE);
}

/* trcputstr - print a string to *trace-output* */
VOID trcputstr P1C(char *, str)
{
    xlputstr(getvalue(s_traceout),str);
}
