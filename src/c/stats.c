/* statistics - Basic statistical functions for XLISP-STAT.            */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

typedef LVAL (*subrfun)(V);

LOCAL LVAL datareduce1 P4H(subrfun, subrfun, LVAL, int);


LVAL xssum(V)  { return(datareduce1(xssum, xadd, cvfixnum((FIXTYPE) 0), FALSE)); }
LVAL xsprod(V) { return(datareduce1(xsprod, xmul, cvfixnum((FIXTYPE) 1), FALSE)); }
LVAL xsmin(V) { return(datareduce1(xsmin, xmin, NIL, FALSE)); }
LVAL xsmax(V) { return(datareduce1(xsmax, xmax, NIL, FALSE)); }
LVAL xscount(V)  { return(datareduce1(xscount, xadd, cvfixnum((FIXTYPE) 0), TRUE)); }

LOCAL LVAL datareduce1 P4C(subrfun, f, subrfun, bf, LVAL, nullval, int, count)
{
  LVAL fcn, x, result;
  
  switch (xlargc) {
  case 0: result = nullval; break;
  case 1: 
    if (compoundp(peekarg(0))) {
      xlstkcheck(2);
      xlsave(x);
      xlsave(fcn);
      fcn = cvsubr(bf, SUBR, 0);
      x = subr_map_elements(f);
      x = compounddataseq(x);
      result = reduce(fcn, x, FALSE, NIL);
      xlpopn(2);
    }
    else result = (count) ? cvfixnum((FIXTYPE) 1) : xlgetarg();
    break;
  default:
    xlsave1(x);
    x = makearglist(xlargc, xlargv);
    result = xlcallsubr1(f, x);
    xlpop();
  }
  return(result);
}

LOCAL int all_simple P1C(LVAL, x)
{
  int i, n;

  switch (ntype(x)) {
  case CONS:
    for (; consp(x); x = cdr(x)) 
      if (compoundp(car(x))) return(FALSE);
    break;
  case VECTOR:
    n = getsize(x);
    for (i = 0; i < n; i++) 
      if (compoundp(getelement(x, i))) return(FALSE);
    break;
  case TVEC:
    break;
  case SYMBOL:
    if (null(x)) break;
    /* else fall through */
  default:
    xlerror("not a sequence", x);
  }
  return(TRUE);
}

static LVAL lastcdr P1C(LVAL, x)
{
  LVAL last = NIL;
  
  for (; consp(x); x = cdr(x)) last = x;
  
  return(last);
}

static LVAL elementlist P1C(LVAL, x)
{
  LVAL next, last, result;
  
  if (!compoundp(x)) result = consa(x);
  else {
    xlprot1(x);
    x = compounddataseq(x);
    x = (listp(x)) ? copylist(x) : coerce_to_list(x);
    if (all_simple(x)) result = x;
    else {
      for (next = x; consp(next); next = cdr(next))
        rplaca(next, elementlist(car(next)));
      result = car(x);
      last = lastcdr(car(x));
      for (next = cdr(x); consp(next); next = cdr(next)) {
        rplacd(last, car(next));
        last = lastcdr(car(next));
      }
    }
    xlpop();
  }
  return(result);
}
      
LVAL elementseq P1C(LVAL, x)
{
  if (! compoundp(x)) xlerror("not a compound data item", x);
  x = compounddataseq(x);
  if (all_simple(x)) return(x);
  else return(elementlist(x));
}

LVAL xselement_seq(V) { return(elementseq(xlgetarg())); }

static LVAL base_ifelse(V)
{
  LVAL a, b, c;
  
  a = xlgetarg();
  b = xlgetarg();
  c = xlgetarg();
  xllastarg();
  
  return((a != NIL) ? b : c);
}

LVAL xsifelse(V) { return(subr_map_elements(base_ifelse)); }

typedef struct {
  double real, imag;
  int complex;
} Number;

static VOID make_number P2C(Number *, num, LVAL, x)
{
  if (realp(x)) {
    num->real = makefloat(x);
    num->imag = 0.0;
    num->complex = FALSE;
  }
  else if (complexp(x)) {
    num->real = makefloat(getreal(x));
    num->imag = makefloat(getimag(x));
    num->complex = TRUE;
  }
  else xlerror("not a number", x);
}

static VOID base_mean P3C(int *, count, Number *, mean, LVAL, x)
{
  LVAL y;
  Number num;
  double c, p, q;
  int i, n;

  if (! compoundp(x)) {
    c = *count; p = c / (c + 1.0); q = 1.0 - p;
    make_number(&num, x);
    mean->real = p * mean->real + q * num.real;
    mean->complex = mean->complex || num.complex;
    if (mean->complex) mean->imag = p * mean->imag + q * num.imag;
    (*count)++;
  }
  else {
    x = compounddataseq(x);
    n = seqlen(x);
    for (i = 0; i < n; i++) {
      y = getnextelement(&x, i);
      base_mean(count, mean, y);
    }
  }
}

LVAL xsmean(V)
{
  Number mean;
  int count;
  LVAL x;

  x = xlgetarg();
  xllastarg();

  mean.real = 0.0; mean.imag = 0.0; mean.complex = FALSE;
  count = 0;
  base_mean(&count, &mean, x);
  if (mean.complex) return(newdcomplex(mean.real,mean.imag));
  else return(cvflonum((FLOTYPE) mean.real));
}

LVAL xssample(V)
{
  LVAL x, result, temp, elem;
  int n, N, replace, i, j;
  
  x = xlgaseq();
  n = getfixnum(xlgafixnum());
  N = seqlen(x);
  replace = (moreargs()) ? (xlgetarg() != NIL) : FALSE;
  xllastarg();

  if (! replace && n > N) n = N;

  xlstkcheck(4);
  xlprotect(x);
  xlsave(result);
  xlsave(elem);
  xlsave(temp);
  x = (listp(x)) ? coerce_to_tvec(x, s_true) : copyvector(x);
  result = NIL;
  if (N > 0 && n > 0) {
    for (i = 0; i < n; i++) {
      j = (replace) ? osrand(N) : i + osrand(N - i);
      elem = gettvecelement(x, j);
      result = cons(elem, result);
      if (! replace) {           /* swap elements i and j */
        temp = gettvecelement(x, i);
        settvecelement(x, i, elem);
        settvecelement(x, j, temp);
      }
    }
  }
  xlpopn(4);
  return(result);
}
