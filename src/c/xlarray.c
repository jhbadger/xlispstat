/* xlarray - Implementation of Common Lisp multi-dimensional arrays.   */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"

/**** check int/long/FIXNUM stuff; also signs */
/**** review for efficiency */
/**** check error messages */

/* forward declarations */
LOCAL int inboundsp P3H(LVAL, LVAL, int);
LOCAL FIXTYPE sizefordim P1H(LVAL);
LOCAL int rankfordim P1H(LVAL);
LOCAL LVAL getnextarg P2H(LVAL *, int);

#define getdim(x,d) getelement(getdarraydim(x),d)


/***************************************************************************/
/**                                                                       **/
/**                          Utility Functions                            **/
/**                                                                       **/
/***************************************************************************/

/* find length of a list */
FIXTYPE llength P1C(LVAL, x)
{
  FIXTYPE n;
  
  for (n = 0; consp(x); n++, x = cdr(x))
    if (n > nnodes) xlcircular();

  return(n);
}

LVAL coerce_to_list P1C(LVAL, x)
{
  LVAL next, result;
  int n, i;
  
  /* save the result pointer */
  xlsave1(result);
  
  if (darrayp(x))
    result = array_to_nested_list(x);
  else if (vectorp(x) || stringp(x) || tvecp(x)) {
    n = gettvecsize(x);
    result = mklist(n, NIL);
    for (i = 0, next = result; i < n; i++, next = cdr(next))
      rplaca(next, gettvecelement(x, i));
  }
  else if (objectp(x))
    result = NIL; /***** include standard coercion message later */
  else if (listp(x))
    result = x;
  else if (atom(x)) {
    result = consa(x);
  }
  else result = NIL;
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

LVAL coerce_to_tvec P2C(LVAL, x, LVAL, type)
{
  LVAL val;
  int n;

  xlsave1(val);
  switch (ntype(x)) {
  case SYMBOL:
    if (! null(x))
      xlbadtype(x);
    /* fall through */
  case CONS:
    n = llength(x);
    val = mktvec(n, type);
    xlreplace(val, x, 0, n, 0, n);
    break;
  case DARRAY:
    x = getdarraydata(x);
    /* fall through */
  case VECTOR:
  case STRING:
  case TVEC:
    if (gettvecetype(x) == type)
      val = x;
    else {
      n = gettvecsize(x);
      val = mktvec(n, type);
      xlreplace(val, x, 0, n, 0, n);
    }
    break;
  default:
    if (atom(x)) {
      val = newvector(1);
      setelement(val, 0, x);
    }
    else {
      xlbadtype(x);
      val = NIL; /* not reached */
    }
  }
  xlpop();
  return val;
}

LVAL split_list P2C(LVAL, list, int, len)
{
  LVAL result, sublist, next_r, next_s, next;
  int numlists, n;
  
  if (len < 1) xlfail("invalid length for sublists");
  
  /* protect some pointers */
  xlsave1(result);
  
  n = llength(list);
  if ((n % len) != 0)
    xlfail("list not divisible by this length");
  numlists = n / len;
  
  result = mklist(numlists, NIL);
  for (next = list, next_r = result; consp(next_r); next_r = cdr(next_r)) {
    sublist = mklist(len, NIL);
    rplaca(next_r, sublist);
    for (next_s = sublist; consp(next_s); 
	 next_s = cdr(next_s), next = cdr(next))
      rplaca(next_s, car(next));
  }

  /* restore the stack frame */
  xlpop();
  
  return(result);
}

/* Check for a nonnegative integer */
LVAL checknonnegint P1C(LVAL, x)
{
  if (! fixp(x) || getfixnum(x) < 0) xlerror("Not a nonnegative integer", x);
  return(x);
}

/* Flatten a nested list to depth rank */
LVAL nested_list_to_list P2C(LVAL, list, int, rank)
{
  LVAL result, temp, nextr, nexte, sublist;
  int i;
  
  if (rank > 1) {
    /* protect some pointers */
    xlstkcheck(3);
    xlsave(result);
    xlsave(temp);
    xlsave(sublist);
  
    for (i = 1, result = coerce_to_list(list); i < rank; i++) {
      /* flatten the lists in reverse order */
      for (temp = NIL, nextr = result; consp(nextr); nextr = cdr(nextr)) {
	sublist = coerce_to_list(car(nextr));
	for (nexte = sublist; consp(nexte); nexte = cdr(nexte)) {
	  temp = cons(car(nexte), temp);
	}
      }
      result = temp;
      /* nreverse the result */
      for (temp = NIL; consp(result);) {
	nextr = cdr(result);
	rplacd(result, temp);
	temp = result;
	result = nextr;
      }
      result = temp;
    }

    /* restore the previous stack frame */
    xlpopn(3);
  }
  else result = coerce_to_list(list);

  return (result);
}

/* Get the next argument from the list or the stack; cdr the list */
LOCAL LVAL getnextarg P2C(LVAL *, plist, int, from_stack)
{
  LVAL arg;
  if (from_stack) arg = xlgetarg();
  else if (consp(*plist)) {
    arg = car(*plist);
    *plist = cdr(*plist);
  }
  else {
    xlfail("no arguments left"); /**** xltoofew?? */
    arg = NIL; /* to keep compiler happy */
  }
  return(arg);
}

/* Compute the rank of an array with dimensions given by list or vector dim */
LOCAL int rankfordim P1C(LVAL, dim) 
{
  if (listp(dim)) return(llength(dim));
  else if (vectorp(dim)) return(getsize(dim));
  else xlerror("bad dimension specifier", dim);
  return(0); /* not reached */
}

/* Compute the size of an array with dimensions given by list or vector dim */
LOCAL FIXTYPE sizefordim P1C(LVAL, dim)
{
  int rank, i;
  FIXTYPE size;

  size = 1;

  if (vectorp(dim)) {
    rank = getsize(dim);
    for (i = 0; i < rank; i++)
      size *= getfixnum(checknonnegint(getelement(dim, i)));
  }
  else 
    for (; consp(dim); dim = cdr(dim))
      size *= getfixnum(checknonnegint(car(dim)));

  return(size);
}	

LVAL copylist P1C(LVAL, list)
{
  LVAL result, nextl, nextr;
  
  if (! listp(list)) xlbadtype(list);
  
  /* protect the result pointer */
  xlsave1(result);
  
  result = mklist(llength(list), NIL);
  for (nextl = list, nextr = result; consp(nextl);
       nextl = cdr(nextl), nextr = cdr(nextr)) {
    rplaca(nextr, car(nextl));
  }
  if (! null(nextl)) {
    for (nextr = result; consp(cdr(nextr)); nextr = cdr(nextr));
    rplacd(nextr, nextl);
  }
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

LVAL copyvector P1C(LVAL, v)
{
  LVAL result;
  int n;
  
  switch (ntype(v)) {
  case VECTOR:
  case STRING:
  case TVEC:
  
    /* protect the result pointer */
    xlsave1(result);

    n = gettvecsize(v);
    result = mktvec(n, gettvecetype(v));
    /*for (i = 0; i < n; i++)
      settvecelement(result, i, gettvecelement(v, i));*/
    xlreplace(result, v, 0, n, 0, n);

    /* restore the stack frame */
    xlpop();
    break;
  default: xlbadtype(v);
  }
  return(result);
}

/***************************************************************************/
/***************************************************************************/
/****                                                                   ****/
/****                      Internal Representation                      ****/
/****                                                                   ****/
/***************************************************************************/
/***************************************************************************/

/* Multidimensional arrays are implemented as displaced arrays.        */
/* Internally they are represented as a cons cell. The car component   */
/* is the dimension vector and the cdr is the data vector.             */

/***************************************************************************/
/**                                                                       **/
/**                            Basic Predicates                           **/
/**                                                                       **/
/***************************************************************************/

/* check if a subscript sequence is in array bounds */
LOCAL int inboundsp P3C(LVAL, x, LVAL, indices, int, from_stack)
{
  LVAL index;
  int i, rank;
  
  if (darrayp(x)) {
    rank = getdarrayrank(x);
    for (i = 0; i < rank; i++) {
      index = getnextarg(&indices, from_stack);
      if (! fixp(index) || getfixnum(index) < 0
	  || getfixnum(index) >= getfixnum(getdim(x, i)))
	return(FALSE);
    }
    xllastarg();
    return(TRUE);
  }
  else if (vectorp(x) || stringp(x) || tvecp(x)) {
    index = getnextarg(&indices, from_stack);
    xllastarg();
    return(fixp(index) && getfixnum(index) >= 0 && 
	   getfixnum(index) < gettvecsize(x));
  }
  else xlerror("not an array", x);
  return(0); /* not reached */
}

/***************************************************************************/
/**                                                                       **/
/**                            Basic Constructor                          **/
/**                                                                       **/
/***************************************************************************/

/* Form an array representation from dim sequence and data vector */
/* Both arguments should be protected from garbage collection     */
/**** should be in xldmem.c */
LVAL newdarray P2C(LVAL, dim, LVAL, data)
{
  LVAL dimvector, result;
  int rank;
  FIXTYPE size;

  rank = rankfordim(dim);

  /* Check dim and data for consistency */
  size = sizefordim(dim);
  if (! (vectorp(data) || stringp(data) || tvecp(data)))
    xlerror("bad data argument", data);
  if (size != gettvecsize(data))
    xlfail("dimensions do not match data length");

  if (rank == 1) {
    result = data;
  }
  else {
    /* protect some pointers */
    xlstkcheck(2);
    xlsave(dimvector);
    xlsave(result);

    dimvector = coerce_to_tvec(dim, s_true);

    result = cons(dimvector,data);
    setntype(result, DARRAY);
    
    xlpopn(2);
  }
  return(result);
}

/***************************************************************************/
/***************************************************************************/
/****                                                                   ****/
/****                     Implementation Independent Part               ****/
/****                                                                   ****/
/***************************************************************************/
/***************************************************************************/

/***************************************************************************/
/**                                                                       **/
/**                              Predicates                               **/
/**                                                                       **/
/***************************************************************************/

/* Common Lisp ARRAYP function */
LVAL xarrayp(V)
{
  LVAL x;
  
  x = xlgetarg();
  xllastarg();
  
  switch (ntype(x)) {
  case DARRAY:
  case VECTOR:
  case STRING:
  case TVEC:
    return(s_true);
  default:
    return(NIL);
  }
}

/****************************************************************************/
/**                                                                        **/
/**                              Selectors                                 **/
/**                                                                        **/
/****************************************************************************/

/* Common Lisp ARRAY-DIMENSIONS function */
LVAL xarraydimensions(V)
{
  LVAL x;
  LVAL result;
  
  x = xlgetarg();
  xllastarg();
  
  xlsave1(result);
  if (vectorp(x) || stringp(x) || tvecp(x)) {
    result = cvfixnum((FIXTYPE) gettvecsize(x));
    result = consa(result);
  }
  else if (darrayp(x))
    result = coerce_to_list(getdarraydim(x));
  else xlbadtype(x);

  xlpop();
  return(result);
}

/* Common Lisp ARRAY-RANK function */
LVAL xarrayrank(V)
{
  LVAL x;
  
  x = xlgetarg();
  xllastarg();
  
  if (vectorp(x) || stringp(x) || tvecp(x)) return(cvfixnum((FIXTYPE) 1));
  else if (darrayp(x)) return(cvfixnum((FIXTYPE) getdarrayrank(x)));
  else xlbadtype(x);
  return(NIL); /* not reached */
}

/* Common Lisp ARRAY-TOTAL-SIZE function */
LVAL xarraytotalsize(V)
{
  LVAL x;
  
  x = xlgetarg();
  xllastarg();

  if (darrayp(x)) x = getdarraydata(x);

  if (vectorp(x) || stringp(x) || tvecp(x))
    return(cvfixnum((FIXTYPE) gettvecsize(x)));
  else
    xlbadtype(x);
  return(NIL); /* not reached */
}

/* Common Lisp ARRAY-DIMENSION function */
LVAL xarraydimension(V)
{
  LVAL x, i;

  x = xlgetarg();
  i = checknonnegint(xlgafixnum());
  xllastarg();

  if (getfixnum(i) >= (darrayp(x) ? getdarrayrank(x) : 1))
    xlerror("dimension exceeds rank", i);
  else if (vectorp(x) || stringp(x) || tvecp(x))
    return(cvfixnum((FIXTYPE) gettvecsize(x)));
  else if (darrayp(x)) return(getdim(x, (int) getfixnum(i)));
  else xlbadtype(x);
  return(NIL); /* not reached */
}

/* Common Lisp ARRAY-IN-BOUNDS-P function */
LVAL xarrayinboundsp(V)
{
  return((inboundsp(xlgetarg(), NIL, TRUE)) ? s_true : NIL);
}

/* Compute row major index from indices list or array or from stack args */
FIXTYPE rowmajorindex P3C(LVAL, x, LVAL, indices, int, from_stack)
{
  LVAL dim=NIL, index=NIL;
  int rank, fsize, i;
  FIXTYPE k;
  
  if (vectorp(x) || stringp(x) || tvecp(x)) {
    index = checknonnegint(getnextarg(&indices, from_stack));
    if (getfixnum(index) >= gettvecsize(x))
      xlerror("index out of range", index);
    return(getfixnum(index));
  }
  else if (darrayp(x)) {
    
    dim = getdarraydim(x);
    
    rank = getdarrayrank(x);
    for (i = 0, k = 0; i < rank; i++) {
      index = checknonnegint(getnextarg(&indices, from_stack));
      fsize = getfixnum(getelement(dim, i));
      if (getfixnum(index) >= getfixnum(getdim(x, i)))
	xlerror("index out of range", index);
      k = fsize * k + getfixnum(index);
    }

    if (k >= gettvecsize(getdarraydata(x)))
      xlerror("index out of range", index);

    return(k);
  }
  else xlerror("not an array", x);
  return(0); /* not reached */
}

/* Common Lisp ARRAY-ROW-MAJOR-INDEX function */
LVAL xarrayrowmajorindex(V)
{
  LVAL x;
  
  x = xlgetarg();
  
  return(cvfixnum((FIXTYPE) rowmajorindex(x, NIL, TRUE)));
}

/* Common Lisp AREF function */
LVAL xaref(V)
{
  LVAL x, data, v;

  x = xlgetarg();
  
  data = darrayp(x) ? getdarraydata(x) : x;

  if (darrayp(x) || vectorp(x) || stringp(x) || tvecp(x))
    v = gettvecelement(data, rowmajorindex(x, NIL, TRUE));
  else {
    xlbadtype(x);
    v = NIL; /* to keep compiler happy */
  }
  return(v);
}

LVAL xrowmajoraref(V)
{
  LVAL x, v;
  LVAL index;
  FIXTYPE i;

  x = xlgetarg();
  index = xlgafixnum();
  i = getfixnum(index);
  xllastarg();
  
  if (darrayp(x)) x = getdarraydata(x);

  if (vectorp(x) || stringp(x) || tvecp(x)) {
    if (i < 0 || i >= gettvecsize(x))
      xlerror("array index out of bounds",index);
    v = gettvecelement(x, i);
  }
  else {
    xlbadtype(x);
    v = NIL; /* to keep compiler happy */
  }
  return(v);
}

LVAL xarrayelementtype(V)
{
  LVAL x;
  x = xlgetarg();
  xllastarg();

  if (darrayp(x)) x = getdarraydata(x);

  return(gettvecetype(x));
}


/****************************************************************************/
/**                                                                        **/
/**                            Constructors                                **/
/**                                                                        **/
/****************************************************************************/

/* Make a new array of dimension dim with contents specified by the keyword */
/* argument.                                                                 */
LVAL mkarray P4C(LVAL, dim, LVAL, key, LVAL, key_arg, LVAL, etype)
{
  LVAL data, contents, result;
  int rank, i;
  FIXTYPE size;
  
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(data);
  xlsave(contents);
  xlsave(result);
  
  /* make the array data vector */
  if (key == NIL) {
    data = mktvec(sizefordim(dim), etype);
  }
  else if (key == k_initelem) {
    size = sizefordim(dim);
    data = mktvec(size, etype);
    for (i = 0; i < size; i++)
      settvecelement(data, i, key_arg);
  }
  else if (key == k_initcont) {
    rank = rankfordim(dim);
    if (rank == 0) {
      data = mktvec(1, etype);
      settvecelement(data, 0, key_arg);
    }
    else if (rank == 1) {
      size = sizefordim(dim);
      data = mktvec(size, etype);
      xlreplace(data, key_arg, 0, size, 0, size);
    }
    else {
      size = sizefordim(dim);
      contents = nested_list_to_list(key_arg, rank);
      if (llength(contents) != size)
	xlerror("initial contents does not match dimensions", key_arg);
      data = mktvec(size, etype);
      for (i = 0; i < size && consp(contents); i++, contents = cdr(contents))
	settvecelement(data, i, car(contents));
    }
  }
  else if (key == k_displacedto)
    data = darrayp(key_arg) ? getdarraydata(key_arg) : key_arg;
  else
    xlerror("bad keyword", key);

  result = newdarray(dim, data);
  
  /* restore the stack frame */
  xlpopn(3);
  
  return (result);
}

/* convert nested list to array - used by read macro. Determines dimension */
/* from first list element, without checking others, then calls mkarray.   */
LVAL nested_list_to_array P2C(LVAL, list, int, rank)
{
  LVAL next, dim, data, result;
  int i;
  
  /* protect some pointers */
  xlstkcheck(2);
  xlsave(dim);
  xlsave(result);
  
  dim = mklist(rank, NIL);
  for (i = 0, data = list, next = dim; i < rank; i++, next = cdr(next)) {
    rplaca(next, cvfixnum((FIXTYPE) llength(data)));
    if ((i < rank) && (! listp(data)))
      xlerror("data does not match rank", list);
    data = consp(data) ? car(data) : NIL;
  }
  
  result = mkarray(dim, k_initcont, list, s_true);
  
  /* restore the stack frame */
  xlpopn(2);
  
  return (result);
}

/* Common Lisp MAKE-ARRAY function. Allows one of the keywords */
/* :INITIAL-ELEMENT, :INITIAL-CONTENTS, or :DISPLACED-TO       */
LVAL xmkarray(V)
{
  LVAL dim, key = NIL, key_arg = NIL, etype, result;
  
  /* protect some pointes */
  xlstkcheck(2);
  xlsave(dim);
  xlsave(result);
  
  dim = xlgetarg();
  if (xlgetkeyarg(k_initelem, &key_arg)) key = k_initelem;
  else if (xlgetkeyarg(k_initcont, &key_arg)) key = k_initcont;
  else if (xlgetkeyarg(k_displacedto, &key_arg)) key = k_displacedto;
  if (!xlgetkeyarg(k_elementtype, &etype)) etype = s_true;

  if (fixp(dim)) dim = consa(dim);
  if (! listp(dim)) xlerror("bad dimension argument", dim);
  
  result = mkarray(dim, key, key_arg, etype);
  
  /* restore the stack frame */
  xlpopn(2);
  
  return (result);
}

/*************************************************************************/
/**                                                                     **/
/**                             Print Array                             **/
/**                                                                     **/
/*************************************************************************/

/* Convert to a nested list for printing */
LVAL array_to_nested_list P1C(LVAL, array)
{
  int i, n;
  LVAL alist;
  
  if (! darrayp(array))
    xlerror("not a displaced array", array);

  /* protect the result pointer */
  xlsave1(alist);
  
  n = getdarrayrank(array);
  if (n == 0)
    alist = gettvecelement(getdarraydata(array), 0);
  else {
    alist = coerce_to_list(getdarraydata(array));
    if (alist != NIL)
      for (i = n - 1; i > 0; i--)
	alist = split_list(alist, (int) getfixnum(getdim(array, i)));
  }
  
  /* restore the stack frame */
  xlpop();
  
  return(alist);
}
