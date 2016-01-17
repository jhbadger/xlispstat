/* sortdata - Sorting, ordering and ranking functions. Uses C qsort.   */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

/* forward declarations */
LOCAL LVAL lsort(V);
LOCAL int tiebreak P2H(LVAL *, LVAL *);

/* tiebreaker routine */
LOCAL int tiebreak P2C(LVAL *, px, LVAL *, py)
{
  int ix = getfixnum(px[1]);
  int iy = getfixnum(py[1]);

  if (ix < iy) return(-1);
  else return(1);
}

/* comparison routine for qsort */
#ifdef ANSI
LOCAL int lcomp(const void *px, const void *py)
#else
LOCAL int lcomp(px, py)
     ALLOCTYPE *px, *py;
#endif /* ANSI */
{
  LVAL x, y;	
  FIXTYPE ix, iy;
  double fx, fy;
  
  x = *((LVAL *) px);
  y = *((LVAL *) py);
  
  if (fixp(x) && fixp(y)) {
    ix = getfixnum(x);
    iy = getfixnum(y);
    if (ix < iy) return(-1);
    else if (ix > iy) return(1);
    else return tiebreak((LVAL *) px, (LVAL *) py);
  }
  else if (realp(x) && realp(y)) {
    fx = makefloat(x);
    fy = makefloat(y);
    if (fx < fy) return(-1);
    else if (fx > fy) return(1);
    else return tiebreak((LVAL *) px, (LVAL *) py);
  }
  else if (stringp(x) && stringp(y)) {
    ix = strcmp(getstring(x), getstring(y));
    return ix ? ix : tiebreak((LVAL *) px, (LVAL *) py);
  }
  else xlfail("can't compare these");
  return 0; /* not reached */
}

/* for defining SORT-DATA with SORT function */
LVAL xssortcmp(V)
{
  LVAL x, y;

  x = xlgetarg();
  y = xlgetarg();
  xllastarg();
  
  if (fixp(x) && fixp(y))
    return (getfixnum(x) > getfixnum(y)) ? NIL : s_true;
  else if (realp(x) && realp(y))
    return (makefloat(x) > makefloat(y)) ? NIL : s_true;
  else if (stringp(x) && stringp(y))
    return (strcmp(getstring(x), getstring(y)) > 0) ? NIL : s_true;
  else {
    xlfail("can't compare these");
    return 0; /* not reached */
  }
}

/* for defining ORDER with SORT function */
LVAL xsordercmp(V)
{
  LVAL x, y;

  x = car(xlgacons());
  y = car(xlgacons());
  xllastarg();
  
  if (fixp(x) && fixp(y))
    return (getfixnum(x) > getfixnum(y)) ? NIL : s_true;
  else if (realp(x) && realp(y))
    return (makefloat(x) > makefloat(y)) ? NIL : s_true;
  else if (stringp(x) && stringp(y))
    return (strcmp(getstring(x), getstring(y)) > 0) ? NIL : s_true;
  else {
    xlfail("can't compare these");
    return 0; /* not reached */
  }
}

/* internal sort and order routine. Returns list of list of sorted values */
/* and corresponding indices into original sequence (result of ORDER).    */
LOCAL LVAL lsort(V)
{
  LVAL x, sortx, result, nextx, nexti, 
  result_x, result_i;
  int i, n;
  
  /* protect some pointers */
  xlstkcheck(5);
  xlsave(x);
  xlsave(sortx);
  xlsave(result);
  xlsave(result_x);
  xlsave(result_i);
  
  x = xlgetarg();
  x = elementseq(x);
  x = coerce_to_list(x);
  xllastarg();
  
  n = llength(x);
  
  /* copy x and indices to sortx */
  sortx = newvector(2 * n);
  for (i = 0, nextx = x; i < n; i++, nextx = cdr(nextx)) {
    setelement(sortx, 2 * i, car(nextx));
    setelement(sortx, 2 * i + 1, cvfixnum((FIXTYPE) i));
  }
  
  /* sort the data and get the indices */
  qsort(&getelement(sortx, 0), n, 2 * sizeof(LVAL), lcomp);
  
  /* copy the arrays to lists */
  result_x = mklist(n, NIL);
  result_i = mklist(n, NIL);
  for (i = 0, nextx = result_x, nexti = result_i; i < n;
       i++, nextx = cdr(nextx), nexti = cdr(nexti)) {
    rplaca(nextx, getelement(sortx, 2 * i));
    rplaca(nexti, getelement(sortx, 2 * i + 1));
  }
  
  result = list2(result_x, result_i);
  
  /* restore the stack frame */
  xlpopn(5);
  
  return(result);
}

/* Built in SORT-DATA function */
LVAL xssortdata(V) { return(car(lsort())); }

/* Built in ORDER function */
LVAL xsorder(V) { return(car(cdr(lsort()))); }

/* Built in RANK function */
LVAL xsrank(V)
{
  LVAL x, result;
  
  /* create a new stack frame */
  xlstkcheck(2);
  xlsave(x);
  xlsave(result);
  
  x = peekarg(0);
  result = xsorder();
  result = xlcallsubr1(xsorder, result);
  result = makecompound(x, result);
  
  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}
