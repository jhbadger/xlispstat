/* xlpp.c - xlisp pretty printer */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* local variables */
static int pplevel,ppmargin,ppmaxlen;
static LVAL ppfile;

/* forward declarations */
LOCAL VOID pp P1H(LVAL);
LOCAL VOID pplist P1H(LVAL);
LOCAL VOID ppexpr P1H(LVAL);
LOCAL VOID ppputc P1H(int);
LOCAL VOID ppterpri(V);
LOCAL int  ppflatsize P1H(LVAL);

/* xpp - pretty-print an expression */
LVAL xpp(V)
{
    LVAL expr;

    /* get printlevel and depth values */
    expr = getvalue(s_printlevel);
    if (fixp(expr) && getfixnum(expr) <= MAXPLEV && getfixnum(expr) >= 0) {
        plevel = (int)getfixnum(expr);
    }
    else {
        plevel = MAXPLEV;
    }
    expr = getvalue(s_printlength);
    if (fixp(expr) && getfixnum(expr) <= MAXPLEN && getfixnum(expr) >= 0) {
        plength = (int)getfixnum(expr);
    }
    else
        plength = MAXPLEN;

    /* get expression to print and file pointer */
    expr = xlgetarg();
    ppfile = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* pretty print the expression */
    pplevel = ppmargin = 0; ppmaxlen = 40;
    pp(expr); ppterpri();

    /* return nil */
#ifdef MULVALS
    xlnumresults = 0;	/* no returned results if Multiple values */
    xlresults[0] = NIL;
#endif /* MULVALS */
    return (NIL);
}

/* pp - pretty print an expression */
LOCAL VOID pp P1C(LVAL, expr)
{
  if (consp(expr))
    pplist(expr);
  else if (darrayp(expr)) {
    LVAL value;

    ppputc('#');
    if (plevel == 0) return;

    /* protect a pointer */
    xlsave1(value);

    value = cvfixnum((FIXTYPE) getdarrayrank(expr));
    ppexpr(value);
    ppputc('A');
    value = array_to_nested_list(expr);
    if (null(value)) {
      ppputc('(');
      ppputc(')');
    }
    else
      pplist(value);
  
    /* restore the stack frame */
    xlpop();
  }
  else
    ppexpr(expr);
}

/* pplist - pretty print a list */
LOCAL VOID pplist P1C(LVAL, expr)
{
    int n;

    /* if the expression will fit on one line, print it on one */
    if ((n = ppflatsize(expr)) < ppmaxlen) {
	xlprintl(ppfile,expr,TRUE);
	pplevel += n;
    }

    /* otherwise print it on several lines */
    else {
        int llength = plength;

        if (plevel-- == 0) {
            ppputc('#');
            plevel++;
            return;
        }

	n = ppmargin;
	ppputc('(');
	if (atom(car(expr))) {
	    ppexpr(car(expr));
	    ppputc(' ');
	    ppmargin = pplevel;
	    expr = cdr(expr);
	}
	else
	    ppmargin = pplevel;
	for (; consp(expr); expr = cdr(expr)) {
            if (llength-- == 0) {
                xlputstr(ppfile,"... )");
                pplevel += 5;
                ppmargin =n;
                plevel++;
                return;
            }
	    pp(car(expr));
	    if (consp(cdr(expr)))
		ppterpri();
	}
	if (expr != NIL) {
	    ppputc(' '); ppputc('.'); ppputc(' ');
	    ppexpr(expr);
	}
	ppputc(')');
	ppmargin = n;
        plevel++;
    }
}

/* ppexpr - print an expression and update the indent level */
LOCAL VOID ppexpr P1C(LVAL, expr)
{
    xlprintl(ppfile,expr,TRUE);
    pplevel += ppflatsize(expr);
}

/* ppputc - output a character and update the indent level */
LOCAL VOID ppputc P1C(int, ch)
{
    xlputc(ppfile,ch);
    pplevel++;
}

/* ppterpri - terminate the print line and indent */
LOCAL VOID ppterpri(V)
{
    xlterpri(ppfile);
    for (pplevel = 0; pplevel < ppmargin; pplevel++)
	xlputc(ppfile,' ');
}

/* ppflatsize - compute the flat size of an expression */
LOCAL int ppflatsize P1C(LVAL, expr)
{
  LVAL ustream = newustream();
  int size;

  xlprot1(ustream);
    
  xlprint(ustream,expr,TRUE);

  /* calculate size */
  for (size = 0, ustream = gethead(ustream);
       !null(ustream);
       size++, ustream = cdr(ustream)) ;
  xlpop();

  return (size);
}
