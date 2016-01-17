/* xltvec.c - typed vectors */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

LOCAL int typecode P1H(LVAL);
LOCAL int typesize P1H(int);

LOCAL FIXTYPE getirealpart P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
    return getfixnum(x);
  case COMPLEX:
    if (fixp(getreal(x)))
      return getfixnum(getreal(x));
    else
      xlbadtype(x);
    break;
  default:
    xlbadtype(x);
  }
  return 0; /* not reached */
}

LOCAL FIXTYPE getiimagpart P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
    return 0;
  case COMPLEX:
    if (fixp(getimag(x)))
      return getfixnum(getimag(x));
    else
      xlbadtype(x);
    break;
  default:
    xlbadtype(x);
  }
  return 0; /* not reached */
}

LOCAL FLOTYPE getdrealpart P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
    return (FLOTYPE) getfixnum(x);
  case FLONUM:
    return getflonum(x);
  case COMPLEX:
    return makefloat(getreal(x));
  default:
    xlbadtype(x);
  }
  return 0; /* not reached */
}

LOCAL FLOTYPE getdimagpart P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
  case FLONUM:
    return 0.0;
  case COMPLEX:
    return makefloat(getimag(x));
  default:
    xlbadtype(x);
  }
  return 0; /* not reached */
}

LOCAL int typecode P1C(LVAL, x)
{
  x = xlparsetype(x);
  if (x == a_char) return(CD_CHARACTER);
  else if (x == a_fixnum) return(CD_FIXTYPE);
  else if (x == a_flonum) return(CD_FLOTYPE);
  else if (consp(x) && consp(cdr(x)) && car(x) == a_complex) {
    x = xlparsetype(car(cdr(x)));
    if (x == a_fixnum) return(CD_CXFIXTYPE);
    else if (x == a_flonum) return(CD_CXFLOTYPE);
    else return(CD_TRUE);
  }
  else if (x == s_c_char) return(CD_CHAR);
  else if (x == s_c_uchar) return(CD_UCHAR);
  else if (x == s_c_short) return(CD_SHORT);
  else if (x == s_c_int) return(CD_INT);
  else if (x == s_c_long) return(CD_LONG);
  else if (x == s_c_float) return(CD_FLOAT);
  else if (x == s_c_double) return(CD_DOUBLE);
  else if (x == s_c_complex) return(CD_COMPLEX);
  else if (x == s_c_dcomplex) return(CD_DCOMPLEX);
  else return(CD_TRUE);
}

LOCAL int typesize P1C(int, code)
{
  switch(code) {
  case CD_CHARACTER: return(sizeof(char));
  case CD_FIXTYPE:   return(sizeof(FIXTYPE));
  case CD_FLOTYPE:   return(sizeof(FLOTYPE));
  case CD_CXFIXTYPE: return(2 * sizeof(FIXTYPE));
  case CD_CXFLOTYPE: return(2 * sizeof(FLOTYPE));
  case CD_CHAR:      return(sizeof(char));
  case CD_SHORT:     return(sizeof(short));
  case CD_INT:       return(sizeof(int));
  case CD_LONG:      return(sizeof(long));
  case CD_FLOAT:     return(sizeof(float));
  case CD_DOUBLE:    return(sizeof(double));
  case CD_COMPLEX:   return(2 * sizeof(float));
  case CD_DCOMPLEX:  return(2 * sizeof(double));
  default:           return(1);
  }
}

LVAL mktvec P2C(int, n, LVAL, etype)
{
  LVAL val;
  int type;

  type = typecode(etype);
  if (type == CD_TRUE)
    val = newvector(n);
  else if (type == CD_CHARACTER) {
    int i;
    val = newstring(n);
    for (i = 0; i < n; i++)
      setstringch(val, i, ' ');
  }
  else {
    val = newtvec(n, typesize(type));
    settvectype(val, type);
  }
  return(val);
}

#define settvecdataelt(c, t, i, v) (((t *) (c))[i] = ((t) (v)))
#define gettvecdataelt(c, t, i) (((t *) (c))[i])

#define CVFIX(x) cvfixnum((FIXTYPE) (x))
#define CVFLO(x) cvflonum((FLOTYPE) (x))

#define u_char unsigned char
#define u_short unsigned short
#define u_int unsigned int
#define u_long unsigned long

int gettvecsize P1C(LVAL, x)
{
  switch(ntype(x)) {
  case VECTOR: return(getsize(x));
  case STRING: return(getslength(x));
  case TVEC:   return(gettlength(x) / typesize(gettvectype(x)));
  default: xlbadtype(x);
  }
  /* not reacched */
  return 0;
}

LVAL gettvecelement P2C(LVAL, x, int, i)
{
  double rval, ival;
  FIXTYPE irval, iival;
  ALLOCTYPE *v;
  int type;

  switch (ntype(x)) {
  case VECTOR: return(getelement(x, i));
  case STRING: return(cvchar(getstringch(x, i)));
  case TVEC:
    type = gettvectype(x);
    v = gettvecdata(x);
  
    switch (type) {
    case CD_CHARACTER:  return(cvchar(gettvecdataelt(v, char, i)));
    case CD_FIXTYPE:    return(CVFIX(gettvecdataelt(v, FIXTYPE, i)));
    case CD_FLOTYPE:    return(CVFLO(gettvecdataelt(v, FLOTYPE, i)));
    case CD_CXFIXTYPE:
      irval = gettvecdataelt(v, FIXTYPE, 2 * i);
      iival = gettvecdataelt(v, FIXTYPE, 2 * i + 1);
      return(newicomplex(irval, iival));
    case CD_CXFLOTYPE:
      rval = gettvecdataelt(v, FLOTYPE, 2 * i);
      ival = gettvecdataelt(v, FLOTYPE, 2 * i + 1);
      return(newdcomplex(rval, ival));
    case CD_CHAR:       return(CVFIX(gettvecdataelt(v, char, i)));
    case CD_UCHAR:      return(CVFIX(gettvecdataelt(v, unsigned char, i)));
    case CD_SHORT:      return(CVFIX(gettvecdataelt(v, short, i)));
    case CD_INT:        return(CVFIX(gettvecdataelt(v, int, i)));
    case CD_LONG:       return(CVFIX(gettvecdataelt(v, long, i)));
    case CD_FLOAT:      return(CVFLO(gettvecdataelt(v, float, i)));
    case CD_DOUBLE:     return(CVFLO(gettvecdataelt(v, double, i)));
    case CD_COMPLEX:
      rval = gettvecdataelt(v, float, 2 * i);
      ival = gettvecdataelt(v, float, 2 * i + 1);
      return(newdcomplex(rval, ival));
    case CD_DCOMPLEX:
      rval = gettvecdataelt(v, double, 2 * i);
      ival = gettvecdataelt(v, double, 2 * i + 1);
      return(newdcomplex(rval, ival));
    default:
      xlbadtype(x);
    }
  default:
    xlbadtype(x);
  }
  /* not reached */
  return(NIL);
}

VOID settvecelement P3C(LVAL, x, int, i, LVAL, item)
{
  ALLOCTYPE *v;
  int type;

  switch (ntype(x)) {
  case VECTOR: setelement(x, i, item); break;
  case STRING:
    if (! charp(item)) xlbadtype(item);
    setstringch(x, i, getchcode(item));
    break;
  case TVEC:
    type = gettvectype(x);
    v = gettvecdata(x);

    switch (type) {
    case CD_CHARACTER:
      if (! charp(item)) xlbadtype(item);
      settvecdataelt(v, char, i, getchcode(item));
      break;
    case CD_FIXTYPE:
      if (! fixp(item)) xlbadtype(item);
      settvecdataelt(v, FIXTYPE, i, getfixnum(item));
      break;
    case CD_FLOTYPE:
      settvecdataelt(v, FLOTYPE, i, MAKEFLOAT(item));
      break;
    case CD_CXFIXTYPE:
      settvecdataelt(v, FIXTYPE, 2 * i, getirealpart(item));
      settvecdataelt(v, FIXTYPE, 2 * i + 1, getiimagpart(item));
      break;
    case CD_CXFLOTYPE:
      settvecdataelt(v, FLOTYPE, 2 * i, getdrealpart(item));
      settvecdataelt(v, FLOTYPE, 2 * i + 1, getdimagpart(item));
      break;
    case CD_CHAR:
      if (! fixp(item)) xlbadtype(item);
      settvecdataelt(v, char, i, getfixnum(item));
      break;
    case CD_UCHAR:
      if (! fixp(item)) xlbadtype(item);
      settvecdataelt(v, unsigned char, i, getfixnum(item));
      break;
    case CD_SHORT: 
      if (! fixp(item)) xlbadtype(item);
      settvecdataelt(v, short, i, getfixnum(item));
      break;
    case CD_INT:
      if (! fixp(item)) xlbadtype(item);
      settvecdataelt(v, int, i, getfixnum(item));
      break;
    case CD_LONG:
      if (! fixp(item)) xlbadtype(item);
      settvecdataelt(v, long, i, getfixnum(item));
      break;
    case CD_FLOAT:
      settvecdataelt(v, float, i, MAKEFLOAT(item));
      break;
    case CD_DOUBLE:
      settvecdataelt(v, double, i, MAKEFLOAT(item));
      break;
    case CD_COMPLEX:
      settvecdataelt(v, float, 2 * i, getdrealpart(item));
      settvecdataelt(v, float, 2 * i + 1, getdimagpart(item));
      break;
    case CD_DCOMPLEX:
      settvecdataelt(v, double, 2 * i, getdrealpart(item));
      settvecdataelt(v, double, 2 * i + 1, getdimagpart(item));
      break;
    default:
      xlbadtype(x);
    }
    break;
  default:
    xlbadtype(x);
  }
}

LVAL gettvecetype P1C(LVAL, x)
{
  switch (ntype(x)) {
  case VECTOR: return(s_true);
  case STRING: return(a_char);
  case TVEC:
    switch (gettvectype(x)) {
    case CD_CHARACTER: return(a_char);
    case CD_FIXTYPE:   return(a_fixnum);
    case CD_FLOTYPE:   return(a_flonum);
    case CD_CXFIXTYPE: return(cons(a_complex, consa(a_fixnum)));
    case CD_CXFLOTYPE: return(cons(a_complex, consa(a_flonum)));
    case CD_CHAR:      return(s_c_char);
    case CD_UCHAR:     return(s_c_uchar);
    case CD_SHORT:     return(s_c_short);
    case CD_INT:       return(s_c_int);
    case CD_LONG:      return(s_c_long);
    case CD_FLOAT:     return(s_c_float);
    case CD_DOUBLE:    return(s_c_double);
    case CD_COMPLEX:   return(s_c_complex);
    case CD_DCOMPLEX:  return(s_c_dcomplex);
    default: xlbadtype(x);
    }
  default: xlbadtype(x);
  }
  /* not reached */
  return(NIL);
}

int gettveceltsize P1C(LVAL, x)
{
  switch (ntype(x)) {
  case VECTOR: return(sizeof(LVAL));
  case STRING: return(1);
  case TVEC:   return(typesize(gettvectype(x)));
  default:     xlbadtype(x);
  }
  /* not reached */
  return(0);
}

VOID xlreplace P6C(LVAL, x, LVAL, y,
		   int, start1, int, end1,
		   int, start2, int, end2)
{
  int n1, n2, i, j;

  n1 = listp(x) ? llength(x) : gettvecsize(x);
  n2 = listp(y) ? llength(y) : gettvecsize(y);
  if (! (0 <= start1 && start1 <= end1 && end1 <= n1) ||
      ! (0 <= start2 && start2 <= end2 && end2 <= n2))
    xlfail("range error");

  /* adjust list arguments */
  if (consp(x))
    while (start1 > 0) { start1--; end1--; x = cdr(x); }
  if (consp(y))
    while (start2 > 0) { start2--; end2--; y = cdr(y); }

  /* quick return for no copying */
  if (start1 >= end1 || start2 >= end2)
    return;

  /* do the replacement */
  switch (ntype(x)) {
  case CONS:
    switch (ntype(y)) {
    case CONS:
      for (; end1 > 0 && end2 > 0; x = cdr(x), y = cdr(y), end1--, end2--)
	rplaca(x, car(y));
      break;
    case VECTOR:
      for (i = start2; end1 > 0 && i < end2; x = cdr(x), end1--, i++)
	rplaca(x, getelement(y, i));
      break;
    case STRING:
    case TVEC:
      for (i = start2; end1 > 0 && i < end2; x = cdr(x), end1--, i++)
	rplaca(x, gettvecelement(y, i));
      break;
    default:
      xlbadtype(y);
    }
    break;
  case VECTOR:
    switch (ntype(y)) {
    case CONS:
      for (i = start1; i < end1 && end2 > 0; i++, y = cdr(y), end2--)
	setelement(x, i, car(y));
      break;
    case VECTOR:
      for (i = start1, j = start2; i < end1 && j < end2; i++, j++)
	setelement(x, i, getelement(y, j));
      break;
    case STRING:
    case TVEC:
      for (i = start1, j = start2; i < end1 && j < end2; i++, j++)
	setelement(x, i, gettvecelement(y, j));
      break;
    default:
      xlbadtype(y);
    }
    break;
  case STRING:
  case TVEC:
    switch (ntype(y)) {
    case CONS:
      for (i = start1; i < end1 && end2 > 0; i++, y = cdr(y), end2--)
	settvecelement(x, i, car(y));
      break;
    case VECTOR:
      for (i = start1, j = start2; i < end1 && j < end2; i++, j++)
	settvecelement(x, i, getelement(y, j));
      break;
    case STRING:
    case TVEC:
      if (gettvectype(x) == gettvectype(y)) {
	char *dx, *dy;
	int nbytes;
	dx = ((char *) gettvecdata(x)) + start1;
	dy = ((char *) gettvecdata(y)) + start2;
	end1 -= start1;
	end2 -= start2;
	nbytes = ((end1 < end2) ? end1 : end2) * typesize(gettvectype(x));
	MEMCPY(dx, dy, nbytes);
      }
      else
	for (i = start1, j = start2; i < end1 && j < end2; i++, j++)
	  settvecelement(x, i, gettvecelement(y, j));
      break;
    default:
      xlbadtype(y);
    }
    break;
  default:
    xlbadtype(x);
  }

  return;
}

LVAL xtveceltsize(V)
{
  LVAL x = xlgetarg();
  xllastarg();
  return cvfixnum(gettveceltsize(x));
}
