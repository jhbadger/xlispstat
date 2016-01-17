/* xlread - xlisp expression input routine */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* symbol parser modes */
#define DONE	0
#define NORMAL	1
#define ESCAPE	2

/* string constants */
#define WSPACE "\t \f\r\n"
#define CONST1 "!$%&*+-./0123456789:<=>?@[]^_{}~"
#define CONST2 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

/* forward declarations */
LOCAL LVAL callmacro P2H(LVAL, int);
LOCAL LVAL psymbol P1H(LVAL);
LOCAL LVAL punintern P1H(LVAL);
LOCAL LVAL pnumber P2H(LVAL, int);
LOCAL LVAL pquote P2H(LVAL, LVAL);
LOCAL LVAL plist P1H(LVAL);
LOCAL LVAL pvector P1H(LVAL);
LOCAL LVAL pstruct P1H(LVAL);
LOCAL LVAL readlist P2H(LVAL, int *);
LOCAL VOID pcomment P1H(LVAL);
LOCAL VOID badeof(V);/* TAA MOD to remove unnecessary arg 11/92 */
LOCAL VOID upcase P1H(char *);
LOCAL VOID storech P2H(int *, int);
LOCAL int  nextch P1H(LVAL);
LOCAL int  checkeof P1H(LVAL);
LOCAL int  readone P2H(LVAL, LVAL *);
#ifdef PACKAGES
LOCAL int  pname P3H(LVAL, int *, int *);
#else
LOCAL int  pname P2H(LVAL, int *);
#endif /* PACKAGES */
LOCAL VOID defmacro P3H(int, LVAL, int);
#ifdef BIGNUMS
LOCAL int isadigit P2H(char, int);
LOCAL LVAL convertnumber P2H(char *, int);
#endif
#ifdef PRINTCIRCLE
LOCAL LVAL findcircval P2H(int, LVAL);
LOCAL VOID cleancircle P2H(LVAL, LVAL);
LOCAL VOID circpush P3H(LVAL, LVAL, LVAL *);
LOCAL VOID registercirc P2H(LVAL, LVAL);
#endif /* PRINTCIRCLE */

/* xlload - load a file of xlisp expressions */
int xlload P3C(char *, fname, int, vflag, int, pflag)
{
    char fullname[STRMAX+1];
    LVAL fptr,expr;
    CONTEXT cntxt;
    FILEP fp;
    int sts, mask;
    LVAL oldrtable = getvalue(s_rtable);
#ifdef PACKAGES
    LVAL oldpack = getvalue(s_package);
#endif /* PACKAGES */

#if (! defined(XLISP_STAT) && ! defined(BYTECODE))
    /* protect some pointers */
    xlstkcheck(3);
    xlsave(fptr);
    xlsave(expr);
    xlprotect(oldrtable);
#ifdef PACKAGES
    xlprot1(oldpack);
#endif /* PACKAGES */

    /* default the extension */
    if (needsextension(fname)) {
	strcpy(fullname,fname);
	strcat(fullname,".lsp");
	fname = fullname;
    }

    /* allocate a file node */
    fptr = cvfile(CLOSED,S_FORREADING);

    /* open the file */
#ifdef PATHNAMES
    if ((fp = ospopen(fname,TRUE)) == CLOSED)
#else
    if ((fp = OSAOPEN(fname,OPEN_RO)) == CLOSED)
#endif
    {
	xlpopn(3);
#ifdef PACKAGES
	xlpop();
#endif /* PACKAGES */
	return (FALSE);
    }
    setfile(fptr,fp);
#else
    {
      char origname[STRMAX+1];
      int extend = needsextension(fname);
      int done, Try;
#ifdef XLISP_STAT
      extern LVAL s_default_path;
      LVAL dp = getvalue(s_default_path);
#endif /* XLISP_STAT */

      strcpy(origname, fname);
      fname = fullname;
      fp = CLOSED;

      for (Try = 1, done = FALSE; ! done; Try++) {
	switch (Try) {
	case 1:
	  if (extend) {
	    strcpy(fullname, origname);
	    strcat(fullname, ".fsl");
	    break;
	  }
	  else Try++;
	  /* fall through */
	case 2:
	  strcpy(fullname, origname);
	  if (extend)
	    strcat(fullname, ".lsp");
	  break;
#ifdef XLISP_STAT
	case 3:
	  if (extend && stringp(dp)) {
	    strcpy(fullname, getstring(dp));
	    strcat(fullname, origname);
	    strcat(fullname, ".fsl");
	    break;
	  }
	  else Try++;
	  /* fall through */
	case 4:
	  if (stringp(dp)) {
	    strcpy(fullname, getstring(dp));
	    strcat(fullname, origname);
	    if (extend)
	      strcat(fullname, ".lsp");
	    break;
	  }
	  else Try++;
	  /* fall through */
#endif /* XLISP_STAT */
	default: done = TRUE;
	}
	if (! done)
	  if ((fp = OSAOPEN(fname,OPEN_RO)) != CLOSED)
	    done = TRUE;
      }
    }

    if (fp == CLOSED)
	return(FALSE);

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(fptr);
    xlsave(expr);
    xlprotect(oldrtable);
#ifdef PACKAGES
    xlprot1(oldpack);
#endif /* PACKAGES */

    /* allocate a file node */
    fptr = cvfile(fp,S_FORREADING);
#endif /* XLISP_STAT */

    /* print the information line */
    if (vflag)  /* TAA MOD -- changed from printing to stdout */
	{ sprintf(buf,"; loading \"%s\"\n",fname); dbgputstr(buf); }

    /* read, evaluate and possibly print each expression in the file */
    xlbegin(&cntxt,CF_ERROR|CF_UNWIND,s_true); /*TAA mod so file gets closed*/
#ifdef CRAYCC
    mask = XL_SETJMP(cntxt.c_jmpbuf); /* TAA mod -- save mask */
    if (mask != 0)
#else
    if ((mask = XL_SETJMP(cntxt.c_jmpbuf)) != 0) /* TAA mod -- save mask */
#endif /* CRAYCC */
	sts = FALSE;
    else {
	while (xlread(fptr,&expr,FALSE,FALSE)) {
	    expr = xleval(expr);
	    if (pflag)
		stdprint(expr);
	}
	sts = TRUE;
    }
    xlend(&cntxt);

    /* restore the readtable and package */
    setvalue(s_rtable, oldrtable);
#ifdef PACKAGES
    setvalue(s_package, oldpack);
#endif /* PACKAGES */

    /* close the file */
    OSCLOSE(getfile(fptr));
    setfile(fptr,CLOSED);

    /* restore the stack */
    xlpopn(3);
#ifdef PACKAGES
    xlpop();
#endif /* PACKAGES */

    /* check for unwind protect TAA MOD */
    if ((mask & ~CF_ERROR) != 0)
        xljump(xltarget, xlmask, xlvalue);

    /* return status */
    return (sts);
}

#ifdef PRINTCIRCLE
#define PCHSIZE 31

#define circvalp(x) (consp(x) && car(x) == car(data))
#define circindex(x) (getfixnum(cdr(x)))

LOCAL LVAL findcircval P2C(int, n, LVAL, data)
{
  LVAL next;
  for (next = cdr(cdr(data)); consp(next); next = cdr(next)) {
    if (consp(car(next)) && fixp(car(car(next)))) {
      if (getfixnum(car(car(next))) == n)
	return cdr(car(next));
    }
  }
  xlerror("bad circle read index", cvfixnum((FIXTYPE) n));
  return(NIL);
}

LOCAL VOID circpush P3C(LVAL, val, LVAL, table, LVAL *, ptodo)
{
  LVAL next;
  int i;

  switch (ntype(val)) {
  case CONS:
  case DARRAY:
  case RNDSTATE:
  case ARRAY:
  case OBJECT:
  case VECTOR:
  case STRUCT:
#ifdef BYTECODE
  case CPSNODE:
  case BCODE:
#endif /* BYTECODE */
#ifdef HASHFCNS
    if (structp(val) && getelement(val,0) == a_hashtable)
	break;
#endif
    i = (int) (CVPTR(val) % PCHSIZE);
    for (next = getelement(table, i); consp(next); next = cdr(next))
      if (car(next) == val)
	return;
    *ptodo = cons(val, *ptodo);
  }
}
	 
LOCAL VOID registercirc P2C(LVAL, entry, LVAL, table)
{  
  int i = (int) (CVPTR(car(entry)) % PCHSIZE);
  rplacd(entry, getelement(table, i));
  setelement(table, i, entry);
}

LOCAL VOID cleancircle P2C(LVAL, val, LVAL, data)
{
  LVAL todo, table, entry, next;
  int i, changed;

  if (null(car(cdr(data)))) return;

  switch (ntype(val)) {
  case SUBR:
  case FSUBR:
  case FIXNUM:
  case FLONUM:
#ifdef BIGNUMS
  case RATIO:
  case BIGNUM:
#endif /* BIGNUMS */
  case STREAM:
  case CHAR:
  case USTREAM:
  case COMPLEX:
#ifdef BYTECODE
  case BCCLOSURE:
#endif /* BYTECODE */
  case CLOSURE:
  case STRING:
  case ADATA:
  case TVEC:
  case NATPTR:
  case WEAKBOX:
  case SYMBOL:
#ifdef PACKAGES
  case PACKAGE:
#endif /* PACKAGES */
    return;
  }

  xlstkcheck(2);
  xlsave(todo);
  xlsave(table);

  table = newvector(PCHSIZE);

  do {
    changed = FALSE;

    todo = consa(val);
    for (i = 0; i < PCHSIZE; i++)
      setelement(table, i, NIL);

    while (consp(todo)) {
      entry = todo;
      next = car(todo);
      todo = cdr(todo);
      switch (ntype(next)) {
      case CONS:
      case DARRAY:
      case RNDSTATE:
	registercirc(entry, table);
	if (circvalp(car(next))) {
	  rplaca(next, findcircval(circindex(car(next)), data));
	  changed = TRUE;
	}
	circpush(car(next), table, &todo);
	if (circvalp(cdr(next))) {
	  rplacd(next, findcircval(circindex(cdr(next)), data));
	  changed = TRUE;
	}
	circpush(cdr(next), table, &todo);
	break;
      case ARRAY:
      case OBJECT:
      case VECTOR:
      case STRUCT:
#ifdef BYTECODE
      case CPSNODE:
    case BCODE:
#endif /* BYTECODE */
#ifdef HASHFCNS
	if (structp(next) && getelement(next,0) == a_hashtable)
	  break;
#endif
	{
	  int i, n;
	  registercirc(entry, table);
	  for (i = 0, n = getsize(next); i < n; i++) {
	    if (circvalp(getelement(next, i))) {
	      setelement(next, i,
			 findcircval(circindex(getelement(next, i)), data));
	      changed = TRUE;
	    }
	    circpush(getelement(next, i), table, &todo);
	  }
	}
	break;
      }
    }
  } while (changed);

  xlpopn(2);
}
#endif /* PRINTCIRCLE */

/* xlread - read an xlisp expression */
int xlread P4C(LVAL, fptr, LVAL *, pval, int, rflag, int, pwflag)
{
    int sts;
    LVAL olddenv = xldenv;
#ifdef PRINTCIRCLE
    if (! rflag)
      xldbind(s_rdcircdat, cons(consa(NIL),consa(NIL)));
#endif /* PRINTCIRCLE */
    if (!rflag) xldbind(a_readpw, (pwflag ? s_true : NIL));

    /* read an expression */
    while ((sts = readone(fptr,pval)) == FALSE)
	;
#ifdef PRINTCIRCLE
    if (! rflag)
      cleancircle(*pval, getvalue(s_rdcircdat));
#endif /* PRINTCIRCLE */

    /* unbind a_readpw if necessary */
    xlunbind(olddenv);

    /* return status */
    return (sts == EOF ? FALSE : TRUE);
}

/* readone - attempt to read a single expression */
LOCAL int readone P2C(LVAL, fptr, LVAL *, pval)
{
    LVAL val,type;
    int ch;

#ifdef STSZ
    /* check the stack */
    stchck();
#endif

    /* get a character and check for EOF */
    if ((ch = xlgetc(fptr)) == EOF)
	return (EOF);

    /* handle white space */
    if ((type = tentry(ch)) == k_wspace)
	return (FALSE);

    /* handle symbol constituents */
    /* handle single and multiple escapes */  /* combined by TAA MOD */
    else if (type == k_const ||
	     type == k_sescape || type == k_mescape) {
	xlungetc(fptr,ch);
	*pval = psymbol(fptr);
	return (TRUE);	    
    }

    /* handle read macros */
    else if (consp(type)) {
	if (((val = callmacro(fptr,ch)) != NIL) && consp(val)) {
	    *pval = car(val);
	    return (TRUE);
	}
	else
	    return (FALSE);
    }

    /* handle illegal characters */
    else {
/*      xlerror("illegal character",cvfixnum((FIXTYPE)ch)); */
        xlerror("illegal character",cvchar(ch));    /* friendlier TAA MOD*/
        return (0);  /* compiler warning */
    }
}

/* rmhash - read macro for '#' */
LVAL rmhash(V)
{
    LVAL fptr,val;
    char *bufp;         /* TAA fix to allow control character literals */
        int i;
    int ch;
#ifdef __SASC__
    int testch;
#endif

    /* protect some pointers */
    xlsave1(val);

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* make the return value */
    val = consa(NIL);

    /* check the next character */
    switch (ch = xlgetc(fptr)) {
    case '\'':
		rplaca(val,pquote(fptr,s_function));
		break;
    case '(':
		xlungetc(fptr,ch);
		rplaca(val,pvector(fptr));
		break;
    case '.':
		if (! null(getvalue(s_read_suppress))) {
		  rplaca(val,NIL);
		  break;
		}
		if (readone(fptr,&car(val)) == EOF)
		  badeof(); /* Check added 3/98 */
		rplaca(val,xleval(car(val)));
		break;
    case 'b':
    case 'B':
		rplaca(val,pnumber(fptr,2));
		break;
    case 'o':
    case 'O':
		rplaca(val,pnumber(fptr,8));
		break;
    case 'd':	/* added for version 2.1h */
    case 'D':
		rplaca(val,pnumber(fptr,10));
		break;
    case 'x':
    case 'X':
    		rplaca(val,pnumber(fptr,16));
		break;
    case 's':
    case 'S':
		rplaca(val,pstruct(fptr));
		break;
    case '\\':
		for (i = 0; i < STRMAX-1; i++) {
		  ch = xlgetc(fptr);  /* TAA fix to scan at end of file */
		  if (ch == EOF ||
		      (buf[i] = ch,
		       ((tentry((unsigned char)(buf[i]))  != k_const) &&
			(i > 0) &&      /* TAA fix for left and right paren */
			buf[i] != '\\' && buf[i] != '|'))) {
		    xlungetc(fptr, buf[i]);
		    break;
		  }
		}
		if (! null(getvalue(s_read_suppress))) {
		  rplaca(val,NIL);
		  break;
		}
		buf[i] = 0;
		ch = (unsigned char)buf[0];
#ifdef __SASC__
	testch = etoa(ch);
#endif
		if (strlen(buf) > (unsigned)1) {  /* TAA Fixed */
		  i = buf[strlen(buf)-1]; /* Value of last character */
		  upcase(buf);
		  bufp = &buf[0];
#ifdef __SASC__ /* EBCDIC */
	    testch = 0;
	    if (strncmp(bufp,"M-",2) == 0) {
		testch = 128;
		bufp += 2;
	    }
	    if (strcmp(bufp,"NEWLINE") == 0)
		testch += 0x0a;
	    else if (strcmp(bufp,"SPACE") == 0)
		testch += 0x20;
	    else if (strcmp(bufp,"RUBOUT") == 0)
		ch += 127;
	    else if (strlen(bufp) == 1)
		ch += i;
	    else if (strncmp(bufp,"C-",2) == 0 && strlen(bufp) == 3)
		testch += etoa(bufp[2]) & 31;
	    else xlerror("unknown character name",cvstring(buf));
	    ch = testch;
#else
		  ch = 0;
		  if (strncmp(bufp,"M-",2) == 0) {
		    ch = 128;
		    bufp += 2;
		  }
		  if (strcmp(bufp,"NEWLINE") == 0)
		    ch += '\n';
		  else if (strcmp(bufp,"SPACE") == 0)
		    ch += ' ';
		  else if (strcmp(bufp,"RUBOUT") == 0)
		    ch += 127;
		  else if (strcmp(bufp,"PAGE") == 0)
		    ch += 12;
		  else if (strcmp(bufp,"TAB") == 0)
		    ch += '\t';
		  else if (strcmp(bufp,"BACKSPACE") == 0)
		    ch += 8;
		  else if (strcmp(bufp,"RETURN") == 0)
		    ch += 13;
		  else if (strcmp(bufp,"LINEFEED") == 0)
		    ch += 10;
		  else if (strcmp(bufp,"ESCAPE") == 0)
		    ch += 27;
		  else if (strlen(bufp) == 1) 
		    ch += i;
		  else if (strncmp(bufp,"C-",2) == 0 && strlen(bufp) == 3) 
		    ch += bufp[2] & 31;
		  /* for Macintosh check mark character */
		  else if (strcmp(bufp,"CHECK") == 0)
		    ch +=  0x12;
		  /* for Macintosh Apple character */
		  else if (strcmp(bufp,"APPLE") == 0)
		    ch +=  0x14;
		  else xlerror("unknown character name",cvstring(buf));
#endif
		}
#ifdef __SASC__
		rplaca(val, cvchar(atoe(testch)));
#else
		rplaca(val,cvchar(ch));
#endif
		break;
    case ':':
	        rplaca(val,punintern(fptr));
		break;
    case '|':
    		pcomment(fptr);
		val = NIL;
		break;
    case 'c':
    case 'C':  /* From XLISP-STAT, Copyright (c) 1988, Luke Tierney */
      {
        LVAL list;
        if (readone(fptr, &list) == EOF)
	  badeof(); /* check added 3/98 */
        if (! consp(list) || ! consp(cdr(list)) || cdr(cdr(list)) != NIL)
          xlerror("bad complex number specification", list);
        rplaca(val, newcomplex(car(list), car(cdr(list))));
        break;
      }
    case '+': /* From XLISP-STAT, Copyright (c) 1988, Luke Tierney */
    case '-':  
      {
        LVAL arg;
	int sts; /* added eof check 3/98 */
	LVAL olddenv;

        xlsave1(arg);

	olddenv = xldenv;
	xldbind(s_package, xlkeypack);
        while (! (sts = readone(fptr, &arg)));
	xlunbind(olddenv);

        if (sts == EOF) badeof();
        if (null(getvalue(s_read_suppress)) && checkfeatures(arg, ch)) {
          while (! (sts = readone(fptr, &arg)));
          if (sts==EOF) badeof();
	  rplaca(val, arg);
        }
        else {
	  olddenv = xldenv;
	  xldbind(s_read_suppress, s_true);
          while (! (sts = readone(fptr, &arg)));
          if (sts==EOF) badeof();
	  val = NIL;
	  xlunbind(olddenv);
        }
        xlpop();
        break;
      }
/*************************************************************************/
/*      Lines below added to allow for common lisp arrays                */
/*      Luke Tierney, March 1, 1988                                      */
/*************************************************************************/
	case '0':
	case '1':	
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		{
		  int rank = 0;

		  while (isdigit(ch)) {
		    rank = 10 * rank + ch - '0';
		    ch = xlgetc(fptr);
		  }
#ifdef PRINTCIRCLE
		  if (ch == '=') {
		    LVAL data;
		    if (! boundp(s_rdcircdat)) xlfail("no top level read");
		    if (!xlread(fptr,&val,TRUE,FALSE))
		      badeof();
		    data = getvalue(s_rdcircdat);
		    rplacd(cdr(data), cons(cons(cvfixnum((FIXTYPE) rank),val),
					   cdr(cdr(data))));
		    val = consa(val);
		  }
		  else if (ch == '#') {
		    LVAL next;
		    int found = FALSE;
		    if (! boundp(s_rdcircdat)) xlfail("no top level read");
		    for (next = cdr(cdr(getvalue(s_rdcircdat)));
			 consp(next);
			 next = cdr(next)) {
		      if (consp(car(next)) && fixp(car(car(next)))) {
			if (getfixnum(car(car(next))) == rank) {
			  found = TRUE;
			  val = consa(cdr(car(next)));
			  break;
			}
		      }
		    }
		    if (! found) {
		      rplaca(cdr(getvalue(s_rdcircdat)), s_true);
		      val = consa(cons(car(getvalue(s_rdcircdat)),
				       cvfixnum((FIXTYPE) rank)));
		    }
		  }
		  else			
#endif /* PRINTCIRCLE */
		  if ((ch == 'A') || (ch == 'a')) {
		    readone(fptr, &val);
		    val = nested_list_to_array(val, rank);
		    val = consa(val);
		  }
		  else if ((ch == 'r' || ch == 'R')) {
		    if (rank < 2 || rank > 36)
		      xlfail("bad radix specifier");
		    rplaca(val, pnumber(fptr, rank));
		  }
		  else
		    xlfail("incomplete array specification");
		}
		break;
/*************************************************************************/
/*      Lines above added to allow for common lisp arrays                */
/*      Luke Tierney, March 1, 1988                                      */
/*************************************************************************/
#ifdef BYTECODE
	      case 'k':
	      case 'K':
		{
		  LVAL olddenv = xldenv;
		  LVAL arg;

		  xlsave1(arg);
		  xldbind(s_rtcase, k_upcase);
		  xldbind(s_rtable, getvalue(s_stdrtable));
		  xldbind(s_read_suppress, NIL);
		  readone(fptr, &arg);
		  xlunbind(olddenv);
		  rplaca(val, xlapplysubr(xlmakebcode, arg));
		  xlpop();
		}
		break;
#endif /* BYTECODE */
	      case '$':
		{
		  LVAL arg, *oldargv, *oldsp;
		  int oldargc;
		  
		  xlsave1(arg);
		  oldargv = xlargv;
		  oldargc = xlargc;
		  oldsp = xlsp;
		  xlargv = xlsp;
		  readone(fptr, &arg);
		  pusharg(s_true);
		  xlargc = 1;
		  for (; consp(arg); arg = cdr(arg)) {
		    pusharg(car(arg));
		    xlargc++;
		  }
		  rplaca(val, xmkrndstate());
		  xlsp = oldsp;
		  xlargc = oldargc;
		  xlargv = oldargv;
		  xlpop();
		}
		break;		  
    case EOF:   /* added 3/98 */
        badeof();
    default:
/*      xlerror("illegal character after #",cvfixnum((FIXTYPE)ch)); */
	xlerror("illegal character after #",cvchar(ch)); /*TAA Mod */
    }

    /* restore the stack */
    xlpop();

    /* return the value */
    return (val);
}

/* rmquote - read macro for '\'' */
LVAL rmquote(V)
{
    LVAL fptr;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* parse the quoted expression */
    return (consa(pquote(fptr,s_quote)));
}

/* rmdquote - read macro for '"' */
LVAL rmdquote(V)
{
    char buf[STRMAX+1],*p, *sptr;
    LVAL fptr,str,newstr;
    int len,blen,ch,d2,d3;

    /* protect some pointers */
    xlsave1(str);

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* loop looking for a closing quote */
    len = blen = 0; p = buf;
    while ((ch = checkeof(fptr)) != '"') {

	/* handle escaped characters */
	switch (ch) {
	case '\\':
		switch (ch = checkeof(fptr)) {
		case 't':
			ch = '\011';
			break;
		case 'n':
			ch = '\012';
			break;
		case 'f':
			ch = '\014';
			break;
		case 'r':
			ch = '\015';
			break;
		default:
			if (ch >= '0' && ch <= '7') {
			    d2 = checkeof(fptr);
			    d3 = checkeof(fptr);
			    if (d2 < '0' || d2 > '7'
			     || d3 < '0' || d3 > '7')
				xlfail("invalid octal digit");
			    ch -= '0'; d2 -= '0'; d3 -= '0';
			    ch = (ch << 6) | (d2 << 3) | d3;
			}
			break;
		}
	}

	/* check for buffer overflow */
	if (blen >= STRMAX) {
	    newstr = newstring(len + STRMAX);
	    sptr = getstring(newstr);
	    if (str != NIL)
		MEMCPY(sptr, getstring(str), len);
	    *p = '\0';
	    MEMCPY(sptr+len, buf, blen+1);
	    p = buf;
	    blen = 0;
	    len += STRMAX;
	    str = newstr;
	}

	/* store the character */
	*p++ = ch; ++blen;
    }

    /* append the last substring */
    if (str == NIL || blen) {
	newstr = newstring(len + blen);
	sptr = getstring(newstr);
	if (str != NIL) MEMCPY(sptr, getstring(str), len);
	*p = '\0';
	MEMCPY(sptr+len, buf, blen+1);
	str = newstr;
    }

    /* restore the stack */
    xlpop();

    /* return the new string */
    return (consa(str));
}

/* rmbquote - read macro for '`' */
LVAL rmbquote(V)
{
    LVAL fptr;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* parse the quoted expression */
    return (consa(pquote(fptr,s_bquote)));
}

/* rmcomma - read macro for ',' */
LVAL rmcomma(V)
{
    LVAL fptr,sym;
    int ch;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* check the next character */
    if ((ch = xlgetc(fptr)) == '@' || ch == '.')
	sym = s_comat;
    else {
	xlungetc(fptr,ch);
	sym = s_comma;
    }

    /* make the return value */
    return (consa(pquote(fptr,sym)));
}

/* rmlpar - read macro for '(' */
LVAL rmlpar(V)
{
    LVAL fptr;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* make the return value */
    return (consa(plist(fptr)));
}

/* rmrpar - read macro for ')' */
LVAL rmrpar(V)
{
    xlfail("misplaced close paren");
    return (NIL);   /* never returns */
}

/* rmsemi - read macro for ';' */
LVAL rmsemi(V)
{
    LVAL fptr;
    int ch;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* skip to end of line */
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n')
	;

    /* return nil (nothing read) */
    return (NIL);
}

/* pcomment - parse a comment delimited by #| and |# */
LOCAL VOID pcomment P1C(LVAL, fptr)
{
    int lastch,ch,n;

    /* look for the matching delimiter (and handle nesting) */
    for (n = 1, lastch = -1; n > 0 && (ch = xlgetc(fptr)) != EOF; ) {
	if (lastch == '|' && ch == '#')
	    { --n; ch = -1; }
	else if (lastch == '#' && ch == '|')
	    { ++n; ch = -1; }
	lastch = ch;
    }
}

/* pnumber - parse a number */
#ifdef BIGNUMS
LOCAL LVAL convertnumber(buf, radix)
char *buf; int radix;
{
  if (radix==10 && strlen(buf) < 10) {
    /* take shortcut */
    return cvfixnum(ICNV(buf));
  }
  else {
    LVAL x;
    FIXTYPE temp;
    x = cvtstrbignum(buf, radix);
    return (cvtbigfixnum(x, &temp) ? cvfixnum(temp) : x);
  }
}

LOCAL LVAL pnumber P2C(LVAL, fptr, int, radix)
{
	int i=0;	/* index into buffer */
	int digits=0; /* number of digits so far */
	int ch;
	LVAL resulta = NULL, resultb;
	
	while ((ch = xlgetc(fptr)) != EOF && i < STRMAX) {
		if (i == 0 && ch == '+') { /* ignore leading + */
			i++;
			continue;
		}
		if (i == 0 && ch == '-') { /* negative number */
			buf[i++] = ch;
			continue;
		}
		if (ch == '/' && resulta==NULL) { /* a ratio */
			buf[i] = '\0';
			if (digits==0) xlfail("unrecognized number");
			digits = i = 0;
			resulta = cvtstrbignum(buf, radix); /* do numerator */
			continue;
		}
		if (isadigit(ch, radix)) { /* number constituent */
			buf[i++] = ch;
			digits++;
			continue;
		}
		break; /* invalid character terminates number */
	}
	xlungetc(fptr,ch);
	if (i == STRMAX) xlfail("number too long to process");
	if (digits==0) xlfail("unrecognized number");
	buf[i] = '\0';
	if (resulta) { /* finish up a ratio */
		xlprot1(resulta);
		resultb = cvtstrbignum(buf, radix);
		xlpop();
		if (zeropbignum(resultb)) xlfail("invalid ratio");
		return cvbratio(resulta, resultb);
	}
	return convertnumber(buf, radix);
}

#else
LOCAL LVAL pnumber P2C(LVAL, fptr, int, radix)
{
    int digit,ch;
    long num;
    
    for (num = 0L; (ch = xlgetc(fptr)) != EOF; ) {
	if (ISLOWER7(ch)) ch = toupper(ch);
	if (!('0' <= ch && ch <= '9') && !('A' <= ch && ch <= 'F'))
	    break;
	if ((digit = (ch <= '9' ? ch - '0' : ch - 'A' + 10)) >= radix)
	    break;
	num = num * (long)radix + (long)digit;
    }
    xlungetc(fptr,ch);
    return (cvfixnum((FIXTYPE)num));
}
#endif

/* plist - parse a list */
LOCAL LVAL plist P1C(LVAL, fptr)
{
    LVAL val,expr,lastnptr,nptr;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(val);
    xlsave(expr);

    /* keep appending nodes until a closing paren is found */
    for (lastnptr = NIL; nextch(fptr) != ')'; )

	/* get the next expression */
	switch (readone(fptr,&expr)) {
	case EOF:
	    badeof();
	case TRUE:

	    /* check for a dotted tail */
	    if (expr == s_dot) {

		/* make sure there's a node */
		if (lastnptr == NIL)
		    xlfail("invalid dotted pair");

		/* parse the expression after the dot */
		if (!xlread(fptr,&expr,TRUE,FALSE))
		    badeof();
		rplacd(lastnptr,expr);

		/* make sure its followed by a close paren */
		if (nextch(fptr) != ')')
		    xlfail("invalid dotted pair");
	    }

	    /* otherwise, handle a normal list element */
	    else {
		nptr = consa(expr);
		if (lastnptr == NIL)
		    val = nptr;
		else
		    rplacd(lastnptr,nptr);
		lastnptr = nptr;
	    }
	    break;
	}

    /* skip the closing paren */
    xlgetc(fptr);

    /* restore the stack */
    xlpopn(2);

    /* return successfully */
    return (val);
}

/* pvector - parse a vector */
LOCAL LVAL pvector P1C(LVAL, fptr)
{
    LVAL list,val;
    int len,i;

    /* protect some pointers */
    xlsave1(list);

    /* read the list */
    list = readlist(fptr,&len);

    /* make a vector of the appropriate length */
    val = newvector(len);

    /* copy the list into the vector */
    for (i = 0; i < len; ++i, list = cdr(list))
	setelement(val,i,car(list));

    /* restore the stack */
    xlpop();

    /* return successfully */
    return (val);
}

/* pstruct - parse a structure */
LOCAL LVAL pstruct P1C(LVAL, fptr)
{
    LVAL list,val;
    int len;

    /* protect some pointers */
    xlsave1(list);

    /* read the list */
    list = readlist(fptr,&len);

    /* make the structure */
    val = xlrdstruct(list);

    /* restore the stack */
    xlpop();

    /* return successfully */
    return (val);
}

/* pquote - parse a quoted expression */
LOCAL LVAL pquote P2C(LVAL, fptr, LVAL, sym)
{
    LVAL val,p;
    int sts; /* EOF checking added 3/98 */

    /* protect some pointers */
    xlsave1(val);

    /* allocate two nodes */
    val = consa(sym);
    rplacd(val,consa(NIL));

    /* initialize the second to point to the quoted expression */
    while ((sts = readone(fptr,&p)) == FALSE);
    if (sts == EOF)
        badeof();
    rplaca(cdr(val),p);

    /* restore the stack */
    xlpop();

    /* return the quoted expression */
    return (val);
}

/* psymbol - parse a symbol name */
#ifdef PACKAGES
LOCAL LVAL psymbol P1C(LVAL, fptr)
{
    int escflag, packindex;
    LVAL val, pack;
    int colons;
    char *p;

    pname(fptr,&escflag,&packindex);
    if (! null(getvalue(s_read_suppress))) return(NIL);
    if (escflag || packindex >= 0 || !xlisnumber(buf,&val)) {
      if (packindex >= 0) {
	/* check for zero-length name */
	if (buf[packindex+1] == 0) xlfail("zero length name after ':'");

	if (packindex == 0) {
	  /* count the colons */
	  for (p = buf + packindex + 1, colons = 1; *p == ':'; p++, colons++);
	  if (colons > 2) xlfail("too many :'s");
	  val = xlintern(p, xlkeypack);
	}
	else {
	  /* find the package */
	  buf[packindex] = 0;
	  pack = xlfindpackage(buf);
	  if (! packagep(pack))
	    xlerror("package not found", cvstring(buf));
	  
	  /* count the colons and switch */
	  for (p = buf + packindex + 1, colons = 1; *p == ':'; p++, colons++);
	  switch (colons) {
	  case 1:
	    if (xlfindsymbol(p, pack, &val) != SYM_EXTERNAL)
	      xlerror("external symbol not found", cvstring(p));
	    break;
	  case 2:
	    val = xlintern(p, pack);
	    break;
	  default: xlfail("too many :'s");
	  }
	}
      }
      else {
	pack = getvalue(s_package);
	return(goodpackagep(pack) ? xlintern(buf, pack) : NIL);
      }
    }
    return(val);
}
#else
LOCAL LVAL psymbol P1C(LVAL, fptr)
{
    int escflag;
    LVAL val;
    pname(fptr,&escflag);
    if (! null(getvalue(s_read_suppress))) return(NIL);
    return (escflag || !xlisnumber(buf,&val) ? xlenter(buf) : val);
}
#endif /* PACKAGES */

/* punintern - parse an uninterned symbol */
#ifdef PACKAGES
LOCAL LVAL punintern P1C(LVAL, fptr)
{
    int escflag,packindex;
    pname(fptr,&escflag,&packindex);
    return (xlmakesym(buf));
}
#else
LOCAL LVAL punintern P1C(LVAL, fptr)
{
    int escflag;
    pname(fptr,&escflag);
    return (xlmakesym(buf));
}
#endif /* PACKAGES */

/* pname - parse a symbol/package name */
#ifdef PACKAGES
LOCAL int pname P3C(LVAL, fptr, int *, pescflag, int *, ppackindex)
#else
LOCAL int pname P2C(LVAL, fptr, int *, pescflag)
#endif /* PACKAGES */
{
    int mode, ch = 0, i;
    LVAL type;
#ifdef READTABLECASE
    LVAL rtcase = getvalue(s_rtcase);
    int low=0, up=0;
#endif

    /* initialize */
    *pescflag = FALSE;
#ifdef PACKAGES
    *ppackindex = -1;
#endif /* PACKAGES */
    mode = NORMAL;
    i = 0;

    /* accumulate the symbol name */
    while (mode != DONE) {

	/* handle normal mode */
	while (mode == NORMAL)
	    if ((ch = xlgetc(fptr)) == EOF)
		mode = DONE;
	    else if ((type = tentry(ch)) == k_sescape) {
		storech(&i,checkeof(fptr));
		*pescflag = TRUE;
	    }
	    else if (type == k_mescape) {
		*pescflag = TRUE;
		mode = ESCAPE;
	    }
	    else if (type == k_const
		 ||  (consp(type) && car(type) == k_nmacro))
#ifdef PACKAGES
	      {
		if (ch == ':') {
		  if (*ppackindex < 0) *ppackindex = i;
		  storech(&i,ch);
		}
		else
#endif /* PACKAGES */
#ifdef READTABLECASE
            {
                if (rtcase == k_preserve)
                    storech(&i,ch);
                else if (rtcase == k_downcase)
                    storech(&i,ISUPPER(ch) ? TOLOWER(ch) : ch);
                else if (rtcase == k_invert)
                    storech(&i,ISLOWER(ch) ? (low++, TOUPPER(ch)) : 
                        (ISUPPER(ch) ? (up++, TOLOWER(ch)) : ch));
                else   /*  default upcase  */
                    storech(&i,ISLOWER(ch) ? TOUPPER(ch) : ch);
            }
#else
		storech(&i,ISLOWER(ch) ? TOUPPER(ch) : ch);
#endif
#ifdef PACKAGES
	    }
#endif /* PACKAGES */
	    else
		mode = DONE;

	/* handle multiple escape mode */
	while (mode == ESCAPE)
	    if ((ch = xlgetc(fptr)) == EOF)
		badeof();
	    else if ((type = tentry(ch)) == k_sescape)
		storech(&i,checkeof(fptr));
	    else if (type == k_mescape)
		mode = NORMAL;
	    else
		storech(&i,ch);
    }
    buf[i] = 0;

#ifdef READTABLECASE    /* TAA Mod, sorta fixing a bug */
    if (rtcase == k_invert && low != 0 && up != 0) {
        /* must undo inversion (ugh!). Unfortunately, we don't know if
           any characters are quoted, so we'll just label this bug as
           a feature in the manual. The problem will only occur in symbols
           with mixed case characters outside of quotes and at least one
           quoted alpha character -- not very likely, I hope. */
        int cnt, c;
        for (cnt = 0; cnt < i; cnt++ ) {
            c = buf[cnt];
            if (ISUPPER(c)) buf[cnt] = TOLOWER(c);
            else if (ISLOWER(c)) buf[cnt] = TOUPPER(c);
        }
    }
#endif

    /* check for a zero length name */
    if (i == 0)
        xlfail("zero length name");     /* TAA fix, Jeff Prothero improved*/

    /* unget the last character and return it */
    if (tentry(ch) != k_wspace || ! null(getvalue(a_readpw)))
      xlungetc(fptr, ch);
    return (ch);
}

/* readlist - read a list terminated by a ')' */
LOCAL LVAL readlist P2C(LVAL, fptr, int *, plen)
{
    LVAL list,expr,lastnptr,nptr;
    int ch;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(list);
    xlsave(expr);

    /* get the open paren */
    if ((ch = nextch(fptr)) != '(')
	xlfail("expecting an open paren");
    xlgetc(fptr);

    /* keep appending nodes until a closing paren is found */
    for (lastnptr = NIL, *plen = 0; (ch = nextch(fptr)) != ')'; ) {

	/* check for end of file */
	if (ch == EOF)
	    badeof();

	/* get the next expression */
	switch (readone(fptr,&expr)) {
	case EOF:
	    badeof();
	case TRUE:
	    nptr = consa(expr);
	    if (lastnptr == NIL)
		list = nptr;
	    else
		rplacd(lastnptr,nptr);
	    lastnptr = nptr;
	    ++(*plen);
	    break;
	}
    }

    /* skip the closing paren */
    xlgetc(fptr);

    /* restore the stack */
    xlpopn(2);

    /* return the list */
    return (list);
}

/* storech - store a character in the print name buffer */
/* TAA MOD -- since buffer is always global buf, it is no longer passed
   as argument. also return value is stored in i, so i is now address of
   the int rather than its value */
LOCAL VOID storech P2C(int *, i, int, ch)
{
    if (*i < STRMAX)
	buf[(*i)++] = ch;
}

/* tentry - get a readtable entry */
LVAL tentry P1C(int, ch)
{
    LVAL rtable;
    rtable = getvalue(s_rtable);
    if (!vectorp(rtable) || ch < 0 || ch >= getsize(rtable))
	return (NIL);
    return (getelement(rtable,ch));
}

/* nextch - look at the next non-blank character */
LOCAL int nextch P1C(LVAL, fptr)
{
    int ch;

    /* return and save the next non-blank character */
    while ((ch = xlgetc(fptr)) != EOF && isspace(ch))
	;
    xlungetc(fptr,ch);
    return (ch);
}

/* checkeof - get a character and check for end of file */
LOCAL int checkeof P1C(LVAL, fptr)
{
    int ch;

    if ((ch = xlgetc(fptr)) == EOF)
	badeof();
    return (ch);
}

/* badeof - unexpected eof */
LOCAL VOID badeof(V)
{
    xlfail("EOF reached before expression end");
}

/* xlisnumber - check if this string is a number */
#define DBL_DIGITS 50
#ifdef BIGNUMS
LOCAL int isadigit P2C(char, c, int, r)
{
	if (isdigit(c)) return ((c - '0') < r);
	else if (ISLOWER7(c)) return ((c + 10 - 'a') < r);
	else if (isupper(c)) return ((c + 10 - 'A') < r);
	return 0;
}

int xlisnumber P2C(char *, str, LVAL *, pval)
{
    int dl=0, dr=0;
    char *p = str;
    char *denp = NULL;		/* pointer to denominator string */
    int ratio=0;		/* flag */
    int badratio=0;		/* set if invalid ratio (upon conversion) */
    int radix = 10;
    FIXTYPE numbr;		/* converted integer */
    LVAL numerp, denomp;	/* in case it is a a bignum/ratio */
    int eoff = 0;               /* added to handle 'D' */

    numerp = getvalue(s_readbase);
    if (fixp(numerp)) {
      numbr = getfixnum(numerp);
      if (numbr <2 || numbr > 36) radix = 10;
      else radix = (int)numbr;
    }

    /* check for a sign */
    if (*p == '+' || *p == '-') p++;

    /* check for a string of constituent digits */
    if (radix==10) while (isdigit(*p)) p++, dl++;
    else while (isadigit(*p, radix)) p++, dl++;


    if (*p == '/') { 	/* check for a ratio */
      if (dl == 0) return FALSE;
      p++;
      denp = p; /* save start of denominator */
      if (radix == 10) {
	while (isdigit(*p)) {
	  if (*p++ != 0) ratio = 1;
	  dr++;
	}
      }
      else {
	while (isadigit(*p, radix)) {
	  if (*p++ != 0) ratio = 1;
	  dr++;
	}
      }
      if (dr == 0) return FALSE;
      badratio = !ratio;
      ratio = 1;	/* providing there was no junk at the end */
    }
    else if (*p != '\0') {	/* failed to complete scan */
      radix = 10;	
      if (*p == '.' && p[1] == '\0') {
	p++;			/*  a forced decimal number that scanned */
      }
      else {			/* force decimal and start all over */
	p = str;
	dr = dl = 0;
	/* check for a sign */
	if (*p == '+' || *p == '-')	p++;

	/* check for a string of constituent digits */
	while (isdigit(*p)) p++, dl++;

	/* check for a decimal point */
	if (*p == '.') {
	  p++;
	  while (isdigit(*p)) p++, dr++;
	}
	/* check for an exponent */
#ifdef READTABLECASE
	if ((dl || dr) && *p && strchr("esfdlESFDL", *p))
#else
	if ((dl || dr) && *p && strchr("ESFDL", *p)) 
#endif
	    {
	      eoff = p - str;
	      p++;

	      /* check for a sign */
	      if (*p == '+' || *p == '-') p++;

	      /* check for a string of digits */
	      while (isdigit(*p)) p++, dr++;
	    }
	  }
      }

      /* make sure there was at least one digit and this is the end */
      if ((dl == 0 && dr == 0) || *p) return (FALSE);

      /* convert the string to an integer and return successfully */
      if (pval != NULL) {
        if (*str == '+') ++str;
        if (str[strlen(str)-1] == '.') {
	  str[strlen(str)-1] = '\0';
	}
        if (ratio) {
	  if (badratio) xlerror ("invalid rational number", cvstring (str));
	  *(denp-1) = '\0';	/* delimit numerator string */
	  xlsave1(numerp);
	  numerp = cvtstrbignum(str, radix);
	  denomp = cvtstrbignum(denp, radix);
	  xlpop();
	  if (zeropbignum(denomp)) xlerror("invalid rational number", cvstring(str));
	  *pval = cvbratio(numerp, denomp);
        }
        else if (dr) {
	  char buf[DBL_DIGITS + 1];
	  strncpy(buf, str, DBL_DIGITS);
	  buf[DBL_DIGITS] = 0;
	  if (eoff && eoff < DBL_DIGITS)
	    buf[eoff] = 'E';
	  *pval = cvflonum(atof(buf));
	}
	else {
	  *pval = convertnumber(str, radix);
	}
      }
      return (TRUE);
    }
#else
int xlisnumber P2C(char *, str, LVAL *, pval)
{
  int dl=0, dr=0;
  char *p = str;
  int eoff = 0;               /* added to handle 'D' */

  /* check for a sign */
  if (*p == '+' || *p == '-')
    p++;

  /* check for a string of digits */
  while (isdigit(*p))
    p++, dl++;

  /* check for a decimal point */
  if (*p == '.') {
    p++;
    while (isdigit(*p))
      p++, dr++;
  }

  /* check for an exponent */
#ifdef READTABLECASE
	if ((dl || dr) && *p && strchr("esfdlESFDL", *p))
#else
	if ((dl || dr) && *p && strchr("ESFDL", *p)) 
#endif
    {
      eoff = p - str;
      p++;

      /* check for a sign */
      if (*p == '+' || *p == '-')
	p++;

      /* check for a string of digits */
      while (isdigit(*p))
	p++, dr++;
    }

  /* make sure there was at least one digit and this is the end */
  if ((dl == 0 && dr == 0) || *p) return (FALSE);

  /* convert the string to an integer and return successfully */
  if (pval != NULL) {
    if (*str == '+') ++str;
    if (str[strlen(str)-1] == '.') str[strlen(str)-1] = 0;
    if (dr) {
      char buf[DBL_DIGITS + 1];
      strncpy(buf, str, DBL_DIGITS);
      buf[DBL_DIGITS] = 0;
      if (eoff && eoff < DBL_DIGITS)
	buf[eoff] = 'E';
      *pval = cvflonum(atof(buf));
    }
    else
      *pval = cvfixnum(ICNV(str));
  }
  return (TRUE);
}
#endif

/* defmacro - define a read macro */
LOCAL VOID defmacro P3C(int, ch, LVAL, type, int, offset)
{
    LVAL subr;
    subr = cvsubr(funtab[offset].fd_subr,funtab[offset].fd_type,offset);
    setelement(getvalue(s_rtable),ch,cons(type,subr));
}

/* callmacro - call a read macro */
LOCAL LVAL callmacro P2C(LVAL, fptr, int, ch)
{
    FRAMEP newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(cdr(getelement(getvalue(s_rtable),ch)));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(fptr);
    pusharg(cvchar(ch));
    xlfp = newfp;
    return (xlapply(2));
}

/* upcase - translate a string to upper case */
LOCAL VOID upcase P1C(char *, str)
{
    for (; *str != '\0'; ++str)
	if (ISLOWER7(*str))
	    *str = toupper(*str);
}

/* xlrinit - initialize the reader */
VOID xlrinit(V)
{
    LVAL rtable;
    char *p;
    int ch;

    /* create the read table */
    rtable = newvector(256);
    setsvalue(s_rtable,rtable);

    /* initialize the readtable */
    for (p = WSPACE; (ch = *p++) != 0; )
	setelement(rtable,ch,k_wspace);
    for (p = CONST1; (ch = *p++) != 0; )
	setelement(rtable,ch,k_const);
    for (p = CONST2; (ch = *p++) != 0; )
	setelement(rtable,ch,k_const);

#ifdef ASCII8
/* TAA MOD (8/92) to make extended ASCII character constituent */
    for (ch=128; ch < 255; ch++)
        setelement(rtable,ch,k_const);
#endif

    /* setup the escape characters */
    setelement(rtable,'\\',k_sescape);
    setelement(rtable,'|', k_mescape);

    /* install the read macros */
    defmacro('#', k_nmacro,FT_RMHASH);
    defmacro('\'',k_tmacro,FT_RMQUOTE);
    defmacro('"', k_tmacro,FT_RMDQUOTE);
    defmacro('`', k_tmacro,FT_RMBQUOTE);
    defmacro(',', k_tmacro,FT_RMCOMMA);
    defmacro('(', k_tmacro,FT_RMLPAR);
    defmacro(')', k_tmacro,FT_RMRPAR);
    defmacro(';', k_tmacro,FT_RMSEMI);
#ifdef BYTECODE
    defconstant(s_stdrtable,copyvector(rtable));
#endif /* BYTECODE */
}
