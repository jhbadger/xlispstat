/* xlprint - xlisp print routine */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"
#ifdef XLISP_STAT
#include "xlstat.h"
#endif /* XLISP_STAT */

/* forward declarations */
LOCAL VOID putpacksym P3H(LVAL, LVAL, int);
LOCAL VOID putsymbol P3H(LVAL, char *, int);
LOCAL VOID putstring P2H(LVAL, LVAL);
LOCAL VOID putqstring P2H(LVAL, LVAL);
LOCAL VOID putatm P3H(LVAL, char *, LVAL);
LOCAL VOID putsubr P3H(LVAL, char *, LVAL);
LOCAL VOID putclosure P2H(LVAL, LVAL);
LOCAL VOID putfixnum P2H(LVAL, FIXTYPE);
#ifdef BIGNUMS
LOCAL VOID putbignum P2H(LVAL, LVAL);
#endif
LOCAL VOID putflonum P2H(LVAL, FLOTYPE);
LOCAL VOID putchcode P3H(LVAL, int, int);
LOCAL VOID putoct P2H(LVAL, int);
LOCAL VOID putarray P3H(LVAL, LVAL, int);
LOCAL VOID checkreadable P1H(LVAL);
#ifdef BYTECODE
LOCAL VOID putcpsnode P2H(LVAL, LVAL);
LOCAL VOID putbcode P3H(LVAL, LVAL, int);
LOCAL VOID putbcclosure P3H(LVAL, LVAL, int);
#endif /* BYTECODE */
#ifdef PACKAGES
LOCAL VOID putpackage P2H(LVAL, LVAL);
#endif /* PACKAGES */
LOCAL VOID putrndstate P3H(LVAL, LVAL, int);
#ifdef PRINTCIRCLE
LOCAL LVAL newcircdata(V);
LOCAL VOID addcircdat P2H(LVAL, LVAL);
LOCAL LVAL findcircdat P2H(LVAL, LVAL);
LOCAL VOID cleancircdat P1H(LVAL);
LOCAL LVAL makecircdat P1H(LVAL);
LOCAL int printcircle P2H(LVAL, LVAL);
LOCAL int checkcircle P2H(LVAL, LVAL);
#endif /* PRINTCIRCLE */

/* $putpatch.c$: "MODULE_XLPRIN_C_GLOBALS" */

/* xlprint - print an xlisp value */
VOID xlprint P3C(LVAL, fptr, LVAL, vptr, int, flag)
{
  LVAL temp;
  int readably;

  readably = null(getvalue(s_printreadably)) ? FALSE : TRUE;

  temp = getvalue(s_printlevel);
  if (! readably && fixp(temp) &&
      getfixnum(temp) <= MAXPLEV && getfixnum(temp) >= 0) {
    plevel = (int)getfixnum(temp);
  }
  else {
    plevel = MAXPLEV;     /* clamp to "reasonable" level */
  }
  temp = getvalue(s_printlength);
  if (! readably && fixp(temp) &&
      getfixnum(temp) <= MAXPLEN && getfixnum(temp) >= 0) {
    plength = (int)getfixnum(temp);
  }
  else
    plength = MAXPLEN;
	
  xlprintl(fptr,vptr,flag);
}

#ifdef PRINTCIRCLE
#define PCHSIZE 31

LOCAL LVAL newcircdata(V)
{
  return cons(cvfixnum((FIXTYPE) 0), newvector(PCHSIZE));
}

LOCAL VOID addcircdat P2C(LVAL, x, LVAL, data)
{
  int i = (int) (CVPTR(car(x)) % PCHSIZE);
  rplacd(x, NIL);
  setelement(cdr(data), i, cons(x,getelement(cdr(data), i)));
}

LOCAL LVAL findcircdat P2C(LVAL, x, LVAL, data)
{
  int i = (int) (CVPTR(x) % PCHSIZE);
  LVAL next;
  for (next = getelement(cdr(data), i); consp(next); next = cdr(next))
    if (x == car(car(next)))
      return(car(next));
  return(NIL);
}

LOCAL VOID cleancircdat P1C(LVAL, data)
{
  LVAL next, last;
  int i;

  for (i = 0; i < PCHSIZE; i++) {
    for (next = getelement(cdr(data), i), last = NIL;
	 consp(next);
	 next = cdr(next)) {
      if (! null(cdr(car(next)))) {
	if (null(last)) {
	  setelement(cdr(data), i, next);
	  last = next;
	}
	else {
	  rplacd(last, next);
	  last = next;
	}
      }
    }
    if (null(last))
      setelement(cdr(data), i, NIL);
    else
      rplacd(last, NIL);
  }
}

LOCAL LVAL makecircdat P1C(LVAL, val)
{
  LVAL data, todo, current, cval, entry;

  /* try to avoid consing if val is simple */
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
    return(NIL);
  }

  xlstkcheck(3);
  xlsave(data);
  xlsave(todo);
  xlsave(current);

  data = newcircdata();
  todo = consa(val);

  /* build up the data base of the value to be printed */
  while (consp(todo)) {
    current = todo;
    cval = car(current);
    todo = cdr(todo);
    switch (ntype(cval)) {
    case CONS:
    case DARRAY:
    case RNDSTATE:
      entry = findcircdat(cval,data);
      if (! null(entry))
	rplacd(entry, s_true);
      else {
	addcircdat(current,data);
	todo = cons(cdr(cval),cons(car(cval),todo));
      }
      break;
    case ARRAY:
    case OBJECT:
    case VECTOR:
    case STRUCT:
#ifdef BYTECODE
    case CPSNODE:
    case BCODE:
#endif /* BYTECODE */
      entry = findcircdat(cval,data);
      if (! null(entry))
	rplacd(entry, s_true);
      else {
	int i, n;
	addcircdat(current,data);
#ifdef HASHFCNS
	if (structp(cval) && getelement(cval,0) == a_hashtable)
	  break;
#endif
	for (i = 0, n = getsize(cval); i < n; i++)
	  todo = cons(getelement(cval,i),todo);
      }
      break;
    case SYMBOL:
#ifdef PACKAGES
      if (null(getpackage(cval))) {
	entry = findcircdat(cval,data);
	if (! null(entry))
	  rplacd(entry, s_true);
	else
	  addcircdat(current,data);
      }
#endif /* PACKAGES */
      break;
    case STRING:
    case ADATA:
    case TVEC:
#ifdef PACKAGES
    case PACKAGE:
#endif /* PACKAGES */
      entry = findcircdat(cval,data);
      if (! null(entry))
	rplacd(entry, s_true);
      else
	addcircdat(current,data);
      break;
    }
  }

  /* drop items used only once */
  cleancircdat(data);

  xlpopn(3);
  return(data);
}

LOCAL int printcircle P2C(LVAL, fptr, LVAL, entry)
{
  xlputc(fptr, '#');
  if (fixp(cdr(entry))) {
    putfixnum(fptr, getfixnum(cdr(entry)));
    xlputc(fptr, '#');
    return(TRUE);
  }
  else {
    LVAL data = getvalue(s_prcircdat);
    int n = getfixnum(car(data));
    if (null(cdr(entry))) xlfail("null entry!!");
    putfixnum(fptr, n);
    rplacd(entry, cvfixnum((FIXTYPE) n));
    rplaca(data, cvfixnum((FIXTYPE) n + 1));
    xlputc(fptr, '=');
    return(FALSE);
  }
}

LOCAL int checkcircle P2C(LVAL, fptr, LVAL, val)
{
  LVAL entry;

  if (!null(getvalue(s_printcircle))
      && !null(entry = findcircdat(val,getvalue(s_prcircdat))))
    return(printcircle(fptr, entry));
  else return(FALSE);
}
#endif /* PRINTCIRCLE */
    
VOID xlprintl P3C(LVAL, fptr, LVAL, vptr, int, flag)
{
    LVAL nptr,next;
    int n,i;
    int llength;
    LVAL olddenv;
#ifdef PRINTCIRCLE
    LVAL entry;
#endif /* PRINTCIRCLE */

#ifdef STSZ
    /* check the stack */
    stchck();
#endif

    olddenv = xldenv;
    if (flag ^ (! null(getvalue(s_printescape))))
      xldbind(s_printescape, flag ? s_true : NIL);

    if (! null(getvalue(s_printreadably))) {
      if (!null(getvalue(s_printlevel)))
	xldbind(s_printlevel, NIL);
      if (!null(getvalue(s_printlength)))
	xldbind(s_printlength, NIL);
      if (null(getvalue(s_printgensym)))
	xldbind(s_printgensym, s_true);
      if (null(getvalue(s_printescape))) {
	xldbind(s_printescape, s_true);
	flag = TRUE;
      }
#ifdef PRINTCIRCLE
      if (null(getvalue(s_printcircle)))
	xldbind(s_printcircle, s_true);
#endif /* PRINTCIRCLE */
#ifdef BIGNUMS
      if (!null(getvalue(s_printbase)))
	xldbind(s_printbase, cvfixnum((FIXTYPE) 10));
#endif /* BIGNUMS */
    }

#ifdef PRINTCIRCLE
    if (getvalue(s_printcircle) != NIL && ! boundp(s_prcircdat))
      xldbind(s_prcircdat,makecircdat(vptr));
#endif /* PRINTCIRCLE */

    /* check value type */
    switch (ntype(vptr)) {
    case SUBR:
	    putsubr(fptr,"Subr",vptr);
	    break;
    case FSUBR:
	    putsubr(fptr,"FSubr",vptr);
	    break;
    case CONS:
            if (plevel-- == 0) {            /* depth limitation */
		xlputc(fptr, '#');
                plevel++;
                break;
	      }
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr))
	      break;
#endif /* PRINTCIRCLE */

	    xlputc(fptr,'(');
            llength = plength;
	    for (nptr = vptr; nptr != NIL; nptr = next) {
	      if (llength-- == 0) { /* length limitiation */
		xlputstr(fptr,"...");
		break;
	      }
	      xlprintl(fptr,car(nptr),flag);
	      if ((next = cdr(nptr)) != NIL) {
#ifdef PRINTCIRCLE
		if (!null(getvalue(s_printcircle))
		    && !null(entry = findcircdat(next,getvalue(s_prcircdat)))) {
		  xlputstr(fptr, " . ");
		  xlprintl(fptr,next,flag);
		  break;
		}
		else
#endif /* PRINTCIRCLE */
		if (consp(next))
		  xlputc(fptr,' ');
		else {
		  xlputstr(fptr," . ");
		  xlprintl(fptr,next,flag);
		  break;
		}
	      }
	    }
	    xlputc(fptr,')');
            plevel++;
	    break;
    case SYMBOL:
	    if (vptr == s_unbound) {    /* TAA MOD for new unbound 1/92 */
	      checkreadable(vptr);
	      xlputstr(fptr, "#<Unbound>");
	      break;
	    }
	    putpacksym(fptr, vptr, flag);
	    break;
    case FIXNUM:
	    putfixnum(fptr,getfixnum(vptr));
	    break;
    case FLONUM:
	    putflonum(fptr,getflonum(vptr));
	    break;
    case CHAR:
	    putchcode(fptr,getchcode(vptr),flag);
	    break;
    case STRING:
	    if (flag)
#ifdef PRINTCIRCLE
	      {
		if (! checkcircle(fptr, vptr))
		  putqstring(fptr, vptr);
	      }
#else
                putqstring(fptr, vptr);
#endif /* PRINTCIRCLE */
	    else
		putstring(fptr,vptr);
	    break;
    case STREAM:
#ifdef FILETABLE
        {
            char *msg;
            FILEP fp = getfile(vptr);
	    checkreadable(vptr);
            if (fp == CLOSED)   xlputstr(fptr, "#<Closed-Stream>");
            else {
#ifndef BIGNUMS
	        char *msg2 = (vptr->n_sflags & S_BINARY)?"Binary":"Character";
#endif
                switch (vptr->n_sflags & (S_FORREADING | S_FORWRITING)) {
                    case S_FORREADING: msg = "Input-Stream"; break;
                    case S_FORWRITING: msg = "Output-Stream"; break;
                    default: msg = "IO-Stream"; break;
                }
#ifdef BIGNUMS
		if (vptr->n_sflags & S_BINARY)
		  sprintf(buf,"#<%s-Byte-%lu-%s %d:\"%s\">",
			  (vptr->n_sflags&S_UNSIGNED)?"Unsigned":"Signed",
			  vptr->n_bsiz*((unsigned long) 8L), msg, fp+1, filetab[fp].tname);
		else
		  sprintf(buf,"#<Character-%s %d:\"%s\">",
			  msg, fp+1, filetab[fp].tname);
#else
                sprintf(buf,"#<%s %s %d:\"%s\">", msg2, msg, fp+1, filetab[fp].tname);
#endif
                xlputstr(fptr,buf);
            }
        }
#else
        {
            char *msg;
            FILEP fp = getfile(vptr);
            if (fp == CLOSED)   msg = "Closed-Stream";
            else if (fp == STDIN) msg = "Stdin-Stream";
            else if (fp == STDOUT) msg = "Stdout-Stream";
            else if (fp == CONSOLE) msg = "Terminal-Stream";
            else switch (vptr->n_sflags & (S_FORREADING | S_FORWRITING | S_BINARY)) {
                case (S_FORREADING|S_BINARY): msg = "Binary Input-Stream"; break;
                case (S_FORWRITING|S_BINARY): msg = "Binary Output-Stream"; break;
                case S_BINARY: msg = "Binary IO-Stream"; break;
                case S_FORREADING: msg = "Input-Stream"; break;
                case S_FORWRITING: msg = "Output-Stream"; break;
                default: msg = "IO-Stream"; break;
            }
            putatm(fptr,msg,vptr);
        }
#endif
	    break;
    case USTREAM:
	    putatm(fptr,"Unnamed-Stream",vptr);
	    break;
    case OBJECT:
#ifdef PRINTCIRCLE
            if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
#ifdef XLISP_STAT
            print_mobject(vptr, fptr);
#else
            /* putobj fakes a (send obj :prin1 file) call */
            putobj(fptr,vptr);
#endif /* XLISP_STAT */
	    break;
    case VECTOR:
    case TVEC:
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    if (! null(getvalue(s_printreadably)) && tvecp(vptr)) {
	      LVAL tmp;

	      xlsave1(tmp);
	      n = gettvecsize(vptr);
	      tmp = gettvecetype(vptr);
	      
	      xlputstr(fptr, "#.(");
	      putpacksym(fptr, s_make_array, TRUE);
	      xlputc(fptr, ' ');
	      putfixnum(fptr, n);
	      xlputc(fptr, ' ');
	      putpacksym(fptr, k_elementtype, TRUE);
	      xlputstr(fptr, " '");
	      xlprintl(fptr, tmp, TRUE);
	      xlputc(fptr, ' ');
	      putpacksym(fptr, k_initcont, TRUE);
	      xlputstr(fptr, " '(");

	      for (i = 0; i < n; i++) {
		tmp = gettvecelement(vptr,i);
		xlprintl(fptr,tmp,flag);
                if (i < n - 1) xlputc(fptr,' ');
	      }
	      xlputstr(fptr, "))");
	      xlpop();
	    }
	    else {
	      LVAL item;

	      if (plevel-- == 0) {            /* depth limitation */
		xlputc(fptr, '#');
                plevel++;
                break;
	      }
	      xlputc(fptr,'#'); xlputc(fptr,'(');
	      llength = plength;
	      n = gettvecsize(vptr);
	      
	      xlsave1(item);
	      for (i = 0; i < n; i++) {
                if (llength-- == 0) {        /* length limitiation */
		  xlputstr(fptr,"... ");
		  break;
		}
		item = gettvecelement(vptr,i);
		xlprintl(fptr,item,flag);
                if (i < n - 1) xlputc(fptr,' ');
	      }
	      xlpop();
	      xlputc(fptr,')');
	      plevel++;
	    }
	    break;
	case STRUCT:
#ifdef HASHFCNS
            if (getelement(vptr,0) == a_hashtable) {
                putatm(fptr,"Hash-table",vptr);
	    break;
            }
#endif
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    xlprstruct(fptr,vptr,plevel,flag);
	    break;
    case CLOSURE:
	    putclosure(fptr,vptr);
	    break;
#ifdef BIGNUMS
    case BIGNUM:
	    putbignum(fptr, vptr);
	    break;
    case RATIO:
	    xlprintl(fptr,getnumer(vptr),flag);
	    xlputc(fptr,'/');
	    xlprintl(fptr,getdenom(vptr),flag);
	    break;
#endif
    case COMPLEX:
	    xlputstr(fptr, "#C(");
	    xlprintl(fptr,getreal(vptr),flag);
	    xlputc(fptr,' ');
	    xlprintl(fptr,getimag(vptr),flag);
	    xlputc(fptr, ')');
	    break;
    case ADATA:  /* L. Tierney */
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    putatm(fptr,"Data",vptr);
	    break;
    case NATPTR:  /* L. Tierney */
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    checkreadable(vptr);
	    if (flag) {
	      sprintf(buf, "#<%s: #", "Pointer");
	      xlputstr(fptr, buf);
	    }
	    sprintf(buf, AFMT, CVPTR(getnpaddr(vptr)));
	    xlputstr(fptr, buf);
	    if (flag)
	      xlputc(fptr, '>');
	    break;
    case WEAKBOX:
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    checkreadable(vptr);
	    putatm(fptr,"Weak Box",vptr);
	    break;
    case DARRAY:
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    if (plevel == 0)
	      xlputc(fptr, '#');
	    else
	      putarray(fptr, vptr, flag);
	    break;
#ifdef BYTECODE
    case BCCLOSURE:
	    putbcclosure(fptr, vptr, flag);
	    break;
    case CPSNODE:
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    putcpsnode(fptr,vptr);
	    break;
    case BCODE:
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    putbcode(fptr,vptr,flag);
	    break;
#endif /* BYTECODE */
#ifdef PACKAGES
    case PACKAGE:
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    putpackage(fptr,vptr);
	    break;
#endif /* PACKAGES */
    case RNDSTATE:
#ifdef PRINTCIRCLE
	    if (checkcircle(fptr, vptr)) break;
#endif /* PRINTCIRCLE */
	    putrndstate(fptr, vptr, flag);
	    break;
    case FREE:
	    putatm(fptr,"Free",vptr);
	    break;

    /* $putpatch.c$: "MODULE_XLPRIN_C_XLPRINT" */

    default:
	    putatm(fptr,"Unknown",vptr);        /* was 'Foo`   TAA Mod */
	    break;
    }
    xlunbind(olddenv);
}

/* xlterpri - terminate the current print line */
VOID xlterpri P1C(LVAL, fptr)
{
    xlputc(fptr,'\n');
}

/* xlgetcolumn -- find the current file column */

int xlgetcolumn P1C(LVAL, fptr)
{
    if (fptr == NIL) return 0;
    else if (ntype(fptr) == USTREAM) { /* hard work ahead :-( */
        LVAL ptr = gethead(fptr);
        int count = 0;

        while (ptr != NIL) {
            if (getchcode(car(ptr)) == '\n') count = 0 ;
            else count++;
            ptr = cdr(ptr);
        }
        return count;
    }
    else if (getfile(fptr) == CONSOLE)
        return lposition;
    else
        return ((fptr->n_sflags & S_WRITING)? fptr->n_cpos : 0);
}


/* xlfreshline -- start new line if not at beginning of line */
int xlfreshline P1C(LVAL, fptr)
{
    if (xlgetcolumn(fptr) != 0) {
        xlterpri(fptr);
        return TRUE;
    }
    return FALSE;
}

/* xlputstr - output a string */
VOID xlputstr P2C(LVAL, fptr, char *, str)
{
    /* solve reentrancy problems if gc prints messages and
       xlputstr output is directed to a string stream */
    if (ustreamp(fptr)) {
        int oplevel=plevel, oplength=plength;   /* save these variables */
        char nbuf[STRMAX+1];

        if (buf == str) {   /* copy to reentrant buffer if necessary */
            str = strcpy(nbuf, buf);
        }

        while (*str)        /* print string */
            xlputc(fptr, *str++);

        plevel = oplevel;   /* restore level and length */
        plength = oplength;
    }
    else
    while (*str)
	xlputc(fptr,*str++);
}

/* print package and symbol */
#ifdef PACKAGES
LOCAL VOID putpacksym P3C(LVAL, fptr, LVAL, sym, int, flag)
{
  LVAL pack, foundsym;
  char *pname;
  int full;

  pack = getvalue(s_package);
  pname = getstring(getpname(sym));
  
  full = (null(getvalue(s_printsympack))) ? FALSE : TRUE;

  if (!flag || !goodpackagep(pack))
    putsymbol(fptr, pname, flag);
  else if (keywordp(sym)) {
    xlputc(fptr, ':');
    putsymbol(fptr, pname, flag);
  }
  /* TAA MOD 10/96 -- if no home package, always print with #: */
  else if (! full && (!null(getpackage(sym))) &&
	   xlfindsymbol(pname, pack, &foundsym) && sym == foundsym)
    putsymbol(fptr, pname, flag);
  else {
    /* TAA modified to handle mixed cases when in :invert, 9/13/96 */
    LVAL olddenv = xldenv;
    pack = getpackage(sym);
    if (packagep(pack)) {
      if (getvalue(s_rtcase) == k_invert)  {
	char *cp, c;
	int up=0, low=0;
	cp = getstring(xlpackagename(pack));
	while ((c=*cp++)!='\0') {
	  if (ISUPPER(c)) up++;
	  else if (ISLOWER(c)) low++;
	}
	cp = getstring(getpname(sym));
	while ((c=*cp++)!='\0') {
	  if (ISUPPER(c)) up++;
	  else if (ISLOWER(c)) low++;
	}
	xldbind(s_rtcase, ((up!=0 && low!=0) ? k_preserve : k_invert));
      }
      putsymbol(fptr, getstring(xlpackagename(pack)), flag);
      if (! full && xlfindsymbol(pname, pack, &foundsym) == SYM_EXTERNAL)
	xlputc(fptr, ':');
      else
	xlputstr(fptr, "::");
      putsymbol(fptr, pname, flag);
      xlunbind(olddenv);
    }
    else if (flag && !null(getvalue(s_printgensym))) {
#ifdef PRINTCIRCLE
      if (! checkcircle(fptr, sym)) {
	xlputstr(fptr,"#:");
	putsymbol(fptr, pname, flag);
      }
#else
      xlputstr(fptr,"#:");
      putsymbol(fptr, pname, flag);
#endif /* PRINTCIRCLE */
    }
    else putsymbol(fptr, pname, flag);
  }
}
#else
LOCAL VOID putpacksym P3C(LVAL, fptr, LVAL, sym, int, flag)
{
  /* check for uninterned symbol */
  if (flag && !null(getvalue(s_printgensym)) && ! syminterned(sym))
    xlputstr(fptr,"#:");
  putsymbol(fptr, getstring(getpname(sym)), flag);
}
#endif /* PACKAGES */

#ifdef READTABLECASE
#define RUP  0      /* values for upcase, downcase, preserve, and invert */
#define RDWN 1
#define RPRE 2
#define RINV 3
#endif

/* putsymbol - output a symbol */
LOCAL VOID putsymbol P3C(LVAL, fptr, char *, stri, int, flag)
{
#ifdef READTABLECASE
    LVAL rtcase = getvalue(s_rtcase);
    int rcase,up,low;
    int mixcase;
#endif
    int downcase;
    int capitalize;
    LVAL type;
    unsigned char *p;
    unsigned char c;
#define str stri

#ifdef READTABLECASE
    /* check value of *readtable-case* */
    if      (rtcase == k_upcase)   rcase = RUP;
    else if (rtcase == k_invert)   rcase = RINV;
    else if (rtcase == k_downcase) rcase = RDWN;
    else if (rtcase == k_preserve) rcase = RPRE;
    else rcase = RUP;                           /* default is upcase */
#endif

    /* handle escaping if flag is true */
    if (flag) {
        /* check to see if symbol needs escape characters */
        for (p = (unsigned char *)str; (c = *p) != 0 ; ++p)
#ifdef READTABLECASE
            if    ((rcase == RUP && ISLOWER(c))
                || (rcase == RDWN && ISUPPER(c))
                ||  ((type = tentry(c)) != k_const
                    && (!consp(type) || car(type) != k_nmacro)))
#else
            if (ISLOWER(c)
                ||  ((type = tentry(c)) != k_const
                    && (!consp(type) || car(type) != k_nmacro)))
#endif
            {
		xlputc(fptr,'|');
		while (*str) {
		    if (*str == '\\' || *str == '|')
			xlputc(fptr,'\\');
		    xlputc(fptr,*str++);
		}
		xlputc(fptr,'|');
		return;
	    }
        /* check for the first character being '#'
            or string looking like a number */
        if (*str == '#' || xlisnumber(str,NULL))
            xlputc(fptr,'\\');
    }

    /* get the case translation flag -- default upcase */
    downcase = (getvalue(s_printcase) == k_downcase);
    /* use capitalize mode if RUP or RDWN and printcase is capitalize */
    capitalize = 
#ifdef READTABLECASE
        (rcase==RUP || rcase==RDWN) &&
#endif
        (getvalue(s_printcase) == k_capitalize);

#ifdef READTABLECASE
    /* we need to know if there is a mixed case symbol if reading :INVERT */
    if (rcase == RINV)  {
        up=FALSE;
        low=FALSE;
        mixcase = FALSE;
        for (p=(unsigned char *)str ; (c = *p) != 0 && !mixcase ; ++p)  {
            if (ISLOWER(c))
                low = TRUE;
            else if (ISUPPER(c))
                up = TRUE;
            mixcase = up&low;
        }
        if (mixcase) rcase = RPRE;  /* preserve if cases mixed */
    }
    low = (rcase == RINV) || (rcase == RUP && (downcase||capitalize));
    up  = (rcase == RINV) || (rcase == RDWN && !downcase);

#endif

    /* output each character */
    mixcase = TRUE; /* set at start of a "word */
    while ((c = (unsigned char) *str++) != 0) {
#ifdef PACKAGES
	if (flag && (c == '\\' || c == '|' || c == ':'))
#else
	if (flag && (c == '\\' || c == '|'))
#endif /* PACKAGES */
	    xlputc(fptr,'\\');
#ifdef READTABLECASE
        if (capitalize) {
            xlputc(fptr, (mixcase ? ((ISLOWER(c)&&up) ? TOUPPER(c) : c)
                                  : ((ISUPPER(c)&&low) ? TOLOWER(c) : c)));
            mixcase = !ISLOWERA(c) && !ISUPPER(c);
        }
        else if (ISUPPER(c)) xlputc(fptr, low ? TOLOWER(c) : c);
        else if (ISLOWER(c)) xlputc(fptr, up  ? TOUPPER(c) : c);
        else xlputc(fptr,c);
#else
        if (capitalize) {
            xlputc(fptr, (mixcase ? (ISLOWER(c) ? TOUPPER(c) : c)
                                  : (ISUPPER(c) ? TOLOWER(c) : c)));
            mixcase = !ISLOWERA(c) && !ISUPPER(c);
        }
	else xlputc(fptr,(downcase && ISUPPER(c) ? TOLOWER(c) : c));
#endif
    }
}

#undef str

/* putstring - output a string */
/* rewritten to  print strings containing nulls TAA mod*/
LOCAL VOID putstring P2C(LVAL, fptr, LVAL, str)
{
    char *p = getstring(str);
    unsigned len = getslength(str);

    /* output each character */
    while (len-- > 0) xlputc(fptr,*p++);
}

/* putqstring - output a quoted string */
/* rewritten to  print strings containing nulls TAA mod*/
LOCAL VOID putqstring P2C(LVAL, fptr, LVAL, str)
{
    char *p = getstring(str);
    unsigned len = getslength(str);
    int ch;

    /* output the initial quote */
    xlputc(fptr,'"');

    /* output each character in the string */
    while (len-- > 0) {
#ifdef __SASC__
	/* For IBM mainframe, Convert EBCDIC to ASCII for the tests below */
	int testch;

	ch = *(unsigned char *)p++;
	testch = etoa(ch);

	/* check for a control character */
	if (testch < 040 || testch == '\\' ||
	    testch == '"' || testch > 0176) /* TAA MOD quote quote */
#else /* __SASC__ */
        ch = *(unsigned char *)p++;

	/* check for a control character */
	/* removed newline - Luke Tierney */
#ifdef ASCII8   /* in this case, upper bit set characters are printable! */
                /* TAA MOD added 8/92 */
        if (ch != '\n' && (ch < 040 || ch == '\\' || ch == '"' || (ch&0177) == 0177))
#else
	if (ch != '\n' && (ch < 040 || ch == '\\' || ch == '"' || ch > 0176)) /* TAA MOD quote quote */
#endif
#endif
            {
	    xlputc(fptr,'\\');
#ifdef __SASC__
	    switch (testch)
#else
	    switch (ch)
#endif
	    {
	    case '\011':
		    xlputc(fptr,'t');
		    break;
	    case '\012':
		    xlputc(fptr,'n');
		    break;
	    case '\014':
		    xlputc(fptr,'f');
		    break;
	    case '\015':
		    xlputc(fptr,'r');
		    break;
	    case 0x5c:  /* is '\\' except for EBCDIC */
	    case 0x22:  /* is '"'  except for EBCDIC, so use int constant*/
		    xlputc(fptr,ch);
		    break;
	    default:
		    putoct(fptr,ch);
		    break;
	    }
	}

	/* output a normal character */
	else
	    xlputc(fptr,ch);
    }

    /* output the terminating quote */
    xlputc(fptr,'"');
}

/* putatm - output an atom */
LOCAL VOID putatm P3C(LVAL, fptr, char *, tag, LVAL, val)
{
    checkreadable(val);
    sprintf(buf, "#<%s: #", tag);
    xlputstr(fptr, buf);
    sprintf(buf, AFMT, CVPTR(val)); /* TAA Fix 2/94: was just val */
    xlputstr(fptr, buf);
    xlputc(fptr, '>');
}

/* putsubr - output a subr/fsubr */
LOCAL VOID putsubr P3C(LVAL, fptr, char *, tag, LVAL, val)
{
/*    sprintf(buf,"#<%s-%s: #",tag,funtab[getoffset(val)].fd_name); */
    char *str;      /* TAA mod */
    checkreadable(val);
    if ((str = funtab[getoffset(val)].fd_name) != NULL)
        sprintf(buf,"#<%s-%s: #",tag,str);
    else
        sprintf(buf,"#<%s: #",tag);
    xlputstr(fptr, buf);
    sprintf(buf, AFMT, CVPTR(val)); /* TAA Fix 2/94: was just val */
    xlputstr(fptr, buf);
    xlputc(fptr, '>');
}

/* putclosure - output a closure */
LOCAL VOID putclosure P2C(LVAL, fptr, LVAL, val)
{
    LVAL name;
    checkreadable(val);
    if ((name = getname(val)) != NIL)
	sprintf(buf,"#<Closure-%s: #",getstring(getpname(name)));
    else
	strcpy(buf,"#<Closure: #");
    xlputstr(fptr, buf);
    sprintf(buf, AFMT, CVPTR(val)); /* TAA Fix 2/94: was just val */
    xlputstr(fptr, buf);
    xlputc(fptr, '>');
}

/* print an array */
LOCAL VOID putarray P3C(LVAL, fptr, LVAL, array, int, flag)
{
  LVAL value, data;
  
  /* protect a pointer */
  xlsave1(value);
  
  data = getdarraydata(array);
  if (! null(getvalue(s_printreadably)) && (stringp(data) || tvecp(data))) {
    LVAL tmp;
    int i, n;

    xlsave1(tmp);
    xlputstr(fptr, "#.(");
    putpacksym(fptr, s_make_array, TRUE);

    value = getdarraydim(array);
    n = getsize(value);
    xlputstr(fptr, " '(");
    for (i = 0; i < n; i++) {
      tmp = getelement(value,i);
      xlprintl(fptr,tmp,flag);
      if (i < n - 1) xlputc(fptr,' ');
    }
    xlputstr(fptr, ") ");

    value = gettvecetype(data);
    putpacksym(fptr, k_elementtype, TRUE);
    xlputstr(fptr, " '");
    xlprintl(fptr, value, TRUE);

    value = array_to_nested_list(array);
    xlputc(fptr, ' ');
    putpacksym(fptr, k_initcont, TRUE);
    xlputstr(fptr, " '");
    if (value == NIL) {
      xlputc(fptr,'(');
    xlputc(fptr,')');
    }
    else
      xlprintl(fptr, value, flag);
    xlputc(fptr, ')');
    xlpop();
  }
  else {
    xlputc(fptr,'#');
    value = cvfixnum((FIXTYPE) getdarrayrank(array));
    xlprint(fptr, value, flag);
    xlputc(fptr, 'A');
    value = array_to_nested_list(array);
    if (value == NIL) {
      xlputc(fptr,'(');
      xlputc(fptr,')');
    }
    else
      xlprintl(fptr, value, flag);
  }
  
  /* restore the stack frame */
  xlpop();
}

#ifdef BYTECODE
/* putcpsnode - output a CPS node */
LOCAL VOID putcpsnode P2C(LVAL, fptr, LVAL, val)
{
    LVAL type;
    checkreadable(val);
    type = getcpstype(val);
    if (null(type) || ! symbolp(type))
	strcpy(buf,"#<CPS-Node NIL: #");
    else
	sprintf(buf,"#<CPS-Node %s: #",getstring(getpname(type)));
    xlputstr(fptr,buf);
    sprintf(buf,AFMT, CVPTR(val)); xlputstr(fptr,buf);
    xlputc(fptr,'>');
}

/* putbcode - output a byte code vector */
LOCAL VOID putbcode P3C(LVAL, fptr, LVAL, val, int, flag)
{
  if (null(getvalue(s_printreadably))) {
    sprintf(buf,"#<Byte-Code: #");
    xlputstr(fptr,buf);
    sprintf(buf,AFMT, CVPTR(val));
    xlputstr(fptr,buf);
    xlputc(fptr,'>');
  }
  else {
    LVAL olddenv = xldenv;
    if (getvalue(s_rtcase) != k_upcase)
      xldbind(s_rtcase, k_upcase);
    if (getvalue(s_printcase) != k_upcase)
      xldbind(s_printcase, k_upcase);
    xlputc(fptr,'#');
    xlputc(fptr, 'K');
    xlputc(fptr, '(');
    xlputc(fptr,'#');
    xlputc(fptr,'(');
    {
      int i, n;
      char *s;

      s = getstring(getbccode(val));
      n = getslength(getbccode(val)) - 1;

      for (i = 0; i < n; ++i) {
	putfixnum(fptr, (FIXTYPE) (unsigned char) s[i]);
	xlputc(fptr,' ');
      }
      putfixnum(fptr, (FIXTYPE) (unsigned char) s[n]);
      xlputc(fptr,')');
    }
    xlputc(fptr, ' ');
    xlprint(fptr, getbcjtab(val), flag);
    xlputc(fptr, ' ');
    xlprint(fptr, getbclits(val), flag);
    xlputc(fptr, ' ');
    xlprint(fptr, getbcidx(val), flag);
    xlputc(fptr, ' ');
    xlprint(fptr, getbcenv(val), flag);
    xlputc(fptr,')');
    xlunbind(olddenv);
  }
}
#endif /* BYTECODE */

/* putrndstate - output a random state */
LOCAL VOID putrndstate P3C(LVAL, fptr, LVAL, val, int, flag)
{
  xlputstr(fptr,"#$(");
  xlprintl(fptr, getrndgen(val), flag);
  xlputc(fptr, ' ');
  xlprintl(fptr, getrnddata(val), flag);
  xlputc(fptr,')');
}

#ifdef BYTECODE
/* putbcclosure - output a random state */
LOCAL VOID putbcclosure P3C(LVAL, fptr, LVAL, val, int, flag)
{
    LVAL name;
    checkreadable(val);
    if ((name = getbcname(getbcccode(val))) != NIL)
      sprintf(buf,"#<Byte-Code-Closure-%s: #",getstring(getpname(name)));
    else
      strcpy(buf,"#<Byte-Code-Closure: #");
    xlputstr(fptr,buf);
    sprintf(buf,AFMT, CVPTR(val)); xlputstr(fptr,buf);
    xlputc(fptr,'>');
}
#endif /* BYTECODE */

/* putfixnum - output a fixnum */
LOCAL VOID putfixnum P2C(LVAL, fptr, FIXTYPE, n)
{
#ifdef BIGNUMS
	if (getvalue(s_printbase) != NIL) {
		/* expect non decimal radix */
		putbignum(fptr, cvtfixbignum(n));
		return;
	}
	else {
		sprintf(buf, INTFMT, n);
	}
#else
    LVAL val;
    char *fmt;

    val = getvalue(s_ifmt);
    fmt = (stringp(val) ? getstring(val) : INTFMT);
    sprintf(buf,fmt,n);
#endif
    xlputstr(fptr,buf);
}

#ifdef PACKAGES
/* putpackage - output package */
LOCAL VOID putpackage P2C(LVAL, fptr, LVAL, val)
{
  LVAL names;
  checkreadable(val);
  names = getpacknames(val);
  if (consp(names) && stringp(car(names))) {
    sprintf(buf,"#<Package %s>",getstring(car(names)));
    xlputstr(fptr, buf);
  }
  else {
    xlputstr(fptr, "#<Package ???: #");
    sprintf(buf,AFMT, CVPTR(val)); /* TAA Fix 2/94, was (OFFTYPE)val */
    xlputstr(fptr,buf);
    xlputc(fptr,'>');
  }
}
#endif /* PACKAGES */

#ifdef BIGNUMS
/* putbignum - output a bignum */
LOCAL VOID putbignum P2C(LVAL, fptr, LVAL, n)
{
	LVAL val;
	FIXTYPE radix;
	char *pstring;

	if (zeropbignum(n)) {
	  /* skip all of this for zero */
	  xlputc(fptr, '0');
	  return;
	}

	val = getvalue(s_printbase);
	if (fixp(val)) {
		radix = getfixnum(val);
		if (radix < 2 || radix > 36) radix = 10;
	}
	else
		radix = 10;

	pstring = cvtbignumstr(n, (int)radix);
	xlputstr(fptr, pstring);
	MFREE(pstring);
}
#endif

int read_exponent P1C(char *, s)
{
  int i;

  i = 0;
  if (s[0] == '+' || s[0] == '-') i++;
  for (; s[i] != 0; i++)
    if (! isdigit(s[i]))
      return(0);
  return(atoi(s));
}

/* modified for consistency with CL -- L. Tierney */
/* putflonum - output a flonum */
LOCAL VOID putflonum P2C(LVAL, fptr, FLOTYPE, n)
{
#ifdef OLDPRINT
  char *fmt;
  LVAL val;

  val = getvalue(s_ffmt);
  fmt = (stringp(val) ? getstring(val) : "%g");
  sprintf(buf,fmt,n);
  xlputstr(fptr,buf);
#else
  if (stringp(getvalue(s_ffmt)) && null(getvalue(s_printreadably))) {
    sprintf(buf, getstring(getvalue(s_ffmt)), n);
    xlputstr(fptr,buf);
  }
  else {
#ifdef IEEEFP
    if (! is_finite(n)) {
      xlputstr(fptr,"#.");
      if (is_nan(n))
	putpacksym(fptr, s_notanumber, TRUE);
      else if (n > 0)
	putpacksym(fptr, s_posinfinity, TRUE);
      else
	putpacksym(fptr, s_neginfinity, TRUE);
      return;
    }
#endif
    /* print the sign */
    if (n < 0) {
      xlputc(fptr, '-');
      n = -n;
    }

    if (n == 0.0) xlputstr(fptr, "0.0");
    else if (1.0e-3 <= n && n < 1.0e7) {
      int e, f, i, m;
      char *ep;

      write_double_efmt(buf, n, 16);

      /* locate the exponent */
      ep = strchr(buf, 'e');

      if (ep != NULL) { /* exponent found -- should always be true */
	/* read exponent and terminate string */
	e = read_exponent(ep + 1);
	*ep = 0;

	/* trim trailing zeros and find lingth of fractional part */
	for (ep--; *ep == '0' && ep > buf; ep--)
	  *ep = 0;
	f = ep - buf - 1;

	if (e < 0) {
	  MEMMOVE(buf + 2 - e, buf + 2, f); /* shift fractional part */
	  buf[1 - e] = buf[0];              /* move leading digit */
	  buf[0] = '0';                     /* set leading digit to zero */
	  for (i = 2; i < 1 - e; i++)       /* insert zeros if needed */
	    buf[i] = '0';
	  buf[2 + f - e] = 0;               /* terminate string */
	}
	else {
	  m = e > f ? f : e;                /* shift the decimal point */
	  MEMMOVE(buf + 1, buf + 2, m);
	  f -= m;
	  buf[e + 1] = '.';
	  for (i = m + 1; i < e + 1; i++)   /* insert zeros */
	    buf[i] = '0';
	  if (f == 0) {                     /* add trailing zero if needed */
	    buf[e + 2] = '0';
	    buf[e + 3] = 0;
	  }
	}
      }
      xlputstr(fptr,buf);
    }
    else {
      int d, e, i, z;

      write_double_efmt(buf, n, 16);

      /* locate the decimal point */
      for (i = 0, d = -1; buf[i] != 0; i++) {
	if (buf[i] == '.') {
	  d = i;
	  break;
	}
      }

      if (d != -1) { /* decimal point found -- true unless Infinity or NaN */

	/* find the exponent marker */
	for (e = d + 1; buf[e] != 0 && ! isalpha(buf[e]); e++);

	/* find the first trailing zero, if any */
	for (z = e - 1; z > d + 1 && buf[z] == '0'; z--);
	z++;

	/* process the exponent */
	if (isalpha(buf[e])) {
	  i = read_exponent(buf + e + 1);
	  sprintf(buf + z, "E%d", i);
	}
      }
      xlputstr(fptr,buf);
    }
  }
#endif /* OLDPRINT */
}

/* putchcode - output a character */
/* modified to print control and meta characters TAA Mod */
LOCAL VOID putchcode P3C(LVAL, fptr, int, ch, int, escflag)
{
#ifdef __SASC__
    int testch = etoa((unsigned) ch);
#endif
    if (escflag) {
        xlputstr(fptr,"#\\");
#ifndef ASCII8  /* print graphics if not defined */
#if __SASC__
	if (testch > 127) {
	    testch -= 128;
	    xlputstr(fptr,"M-");
	}
#else
        if (ch > 127) {
            ch -= 128;
            xlputstr(fptr,"M-");
        }
#endif
#endif
#ifdef __SASC__
	switch (testch)
#else
	switch (ch)
#endif
	{
	case 0x0a:	/* ASCII '\n' */
	    xlputstr(fptr,"Newline");
	    break;
	case 0x20:	/* ASCII ' ' */
	    xlputstr(fptr,"Space");
	    break;
	case 127:
	    xlputstr(fptr,"Rubout");
	    break;
	case 12:
	    xlputstr(fptr,"Page");
	    break;
        case '\t':
	    xlputstr(fptr,"Tab");
	    break;
	case 8:
	    xlputstr(fptr,"Backspace");
	    break;
	case 13:
	    xlputstr(fptr,"Return");
	    break;
#ifdef ASCII8
	case 255:
	    xlputstr(fptr,"M-Rubout");
#endif
#ifdef MACINTOSH /* lines added by Luke Tierney, March 12, 1988 */
	case 0x12: xlputstr(fptr, "Check"); break;
	case 0x14: xlputstr(fptr, "Apple"); break;
#endif /* MACINTOSH */ /* lines added by Luke Tierney, March 12, 1988 */
	default:
#ifdef __SASC__
		if (testch < 32) {
		    testch += '@';
		    xlputstr(fptr,"C-");
		}
		/* Convert ASCII testch to EBCDIC for printing... */
		xlputc(fptr,atoe((unsigned)testch));
		break;
#else
	    if (ch < 32) {
	      ch += '@';
	      xlputstr(fptr,"C-");
	    }
	    xlputc(fptr,ch);
	    break;
#endif
	}
    }
    else xlputc(fptr,ch);
}

/* putoct - output an octal byte value */
LOCAL VOID putoct P2C(LVAL, fptr, int, n)
{
    sprintf(buf,"%03o",n);
    xlputstr(fptr,buf);
}

/* signal error if *print-readably* is true */
LOCAL VOID checkreadable P1C(LVAL, x)
{
  /* argument is ignored; eventually need to print unreadable version */
  if (!null(getvalue(s_printreadably)))
    xlfail("can't readably print an unreadable object");
}
