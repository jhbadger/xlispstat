/* mats1 - Elementary matrix operations                                */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

/* forward declarations */
LOCAL LVAL facelist P1H(int);
LOCAL LVAL xsbindfaces P1H(int);


/*************************************************************************/
/**                                                                     **/
/**           Matrix Construction and Decomposition Functions           **/
/**                                                                     **/
/*************************************************************************/

/* Built in DIAGONAL function */
LVAL xsdiagonal(V)
{
  LVAL arg, next, dim, val, data, result, result_data;
  int n, m, i;

  arg = xlgetarg();
  xllastarg();

  /* protect some pointers */
  xlstkcheck(3);
  xlsave(dim);
  xlsave(val);
  xlsave(result);
	
  if (matrixp(arg)) {

    /* extract diagonal from a matrix */
    n = (numrows(arg) < numcols(arg)) ? numrows(arg) : numcols(arg);
    m = numcols(arg);
    result = mklist(n, NIL);
    data = getdarraydata(arg);
    for (i = 0, next = result; i < n; i++, next = cdr(next))
      rplaca(next, gettvecelement(data, m * i + i));
  }
  else if (seqp(arg)) {

    /* construct a diagonal matrix */
    n = seqlen(arg);
    dim = cvfixnum((FIXTYPE) n);
    dim = list2(dim, dim);
    val = cvfixnum((FIXTYPE) 0);
    result = mkarray(dim, k_initelem, val, s_true);
    result_data = getdarraydata(result);
    for (i = 0; i < n; i++)
      setelement(result_data, n * i + i, getnextelement(&arg, i));
  }
  else xlbadtype(arg);
	
  /* restore the stack frame */
  xlpopn(3);

  return(result);
}

/* Return a list of rows or columns of a matrix read from the stack */
/***** this could be more efficient */
LOCAL LVAL facelist P1C(int, face)
{
  LVAL a, result, next, vect, data, type;
  int rows, cols, i, j;
	
  a = xlgamatrix();
  xllastarg();

  rows = numrows(a);
  cols = numcols(a);
	
  /* protect some pointers */
  xlsave1(result);

  data = getdarraydata(a);
  type = gettvecetype(data);
  switch(face) {
  case 0: /* rows */
    result = mklist(rows, NIL);
    for (next = result, i = 0; i < rows; i++, next = cdr(next)) {
      vect = mktvec(cols, type);
      rplaca(next, vect);
      for (j = 0; j < cols; j++) 
	settvecelement(vect, j, gettvecelement(data, cols * i + j));
    }
    break;
  case 1: /* columns */
    result = mklist(cols, NIL);
    for (next = result, j = 0; j < cols; j++, next = cdr(next)) {
      vect = mktvec(rows, type);
      rplaca(next, vect);
      for (i = 0; i < rows; i++) 
	settvecelement(vect, i, gettvecelement(data, cols * i + j));
    }
    break;
  default:
    xlfail(" bad face selector");
  }
    
  /* restore the stack frame */
  xlpop();

  return(result);
}

/* Built in ROW-LIST and COLUMN-LIST functions */
LVAL xsrowlist(V) { return(facelist(0)); }
LVAL xscolumnlist(V) { return(facelist(1)); }

/* Bind list of sequences or matrices along rows or columns */
LOCAL LVAL xsbindfaces P1C(int, face)
{
  LVAL next, data, dim, result, result_data;
  int totalsize, rows=0, cols=0, c=0, r=0, n, i, j;
  
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(data);
  xlsave(dim);
  xlsave(result);
  
  /* Check the first argument and establish size of the binding face */
  next = peekarg(0);
  switch (face) {
  case 0:
    if (matrixp(next)) cols = numcols(next);
    else if (seqp(next)) cols = seqlen(next);
    else if (! compoundp(next)) cols = 1;
    else xlbadtype(next);
    break;
  case 1:
    if (matrixp(next)) rows = numrows(next);
    else if (seqp(next)) rows = seqlen(next);
    else if (! compoundp(next)) rows = 1;
    else xlbadtype(next);
    break;
  }

  /* Pass through the arguments on the stack to determine the result size */
  n = xlargc;
  for (i = 0, totalsize = 0; i < n; i++) {
    next = peekarg(i);
    if (matrixp(next)) {
      c = numcols(next);
      r = numrows(next); 
    }
    else if (seqp(next))
      switch (face) {
      case 0:  c = seqlen(next); r = 1; break;
      case 1:  c = 1; r = seqlen(next); break;
      }
    else if (! compoundp(next)) {
      c = 1;
      r = 1;
    }
    else xlbadtype(next);

    switch (face) {
    case 0:
      if (c != cols) xlfail("dimensions do not match");
      else totalsize += r;
      break;
    case 1:
      if (r != rows) xlfail("dimensions do not match");
      else totalsize += c;
    }
  }
  
  /* set up the result matrix */
  dim = newvector(2);
  switch (face) {
  case 0:
    setelement(dim, 0, cvfixnum((FIXTYPE) totalsize));
    setelement(dim, 1, cvfixnum((FIXTYPE) cols));
    break;
  case 1:
    setelement(dim, 0, cvfixnum((FIXTYPE) rows));
    setelement(dim, 1, cvfixnum((FIXTYPE) totalsize));
    break;
  }
  result = mkarray(dim, NIL, NIL, s_true);
  result_data = getdarraydata(result);

  /* compute the result */
  for (r = 0, c = 0; moreargs();) {
    next = xlgetarg();
    if (matrixp(next)) {
      rows = numrows(next);
      cols = numcols(next);
      data = getdarraydata(next);
    }
    else {
      switch (face) {
      case 0: rows = 1; break;
      case 1: cols = 1; break;
      }
      data = (vectorp(next) || tvecp(next)) ?
	next : coerce_to_tvec(next, s_true);
    }
    switch (face) {
    case 0:
      for (i = 0; i < rows; i++, r++) 
	for (j = 0; j < cols; j++)
	  setelement(result_data, cols * r + j,
		     gettvecelement(data, cols * i + j));
      break;
    case 1:
      for (j = 0; j < cols; j++, c++)
	for (i = 0; i < rows; i++) 
	  setelement(result_data, totalsize * i + c,
		     gettvecelement(data, cols * i + j));
      break;
    }
  }
  
  /* restore the stack frame */
  xlpopn(3);
  
  return(result);
}

/* Built in BIND-ROWS and BIND-COLUMNS functions */
LVAL xsbindrows(V) { return(xsbindfaces(0)); }
LVAL xsbindcols(V) { return(xsbindfaces(1)); }

/* Built in TRANSPOSE-LIST function */
LVAL xstransposelist(V)
{
  LVAL list, result, nextr, row, nextl;
  int m, n;
  
  list = xlgalist();
  xllastarg();
  
  xlstkcheck(2);
  xlsave(result);
  xlprotect(list);
  
  list = copylist(list);
  m = llength(list);
  if (! consp(car(list))) xlerror("not a list", car(list));
  n = llength(car(list));
  
  result = mklist(n, NIL);
  for (nextr = result; consp(nextr); nextr = cdr(nextr)) {
    row = mklist(m, NIL);
    rplaca(nextr, row);
    for (nextl = list; consp(nextl); nextl = cdr(nextl)) {
      if (!consp(car(nextl))) xlerror("not a list", car(nextl));
      rplaca(row, car(car(nextl)));
      row = cdr(row);
      rplaca(nextl, cdr(car(nextl)));
    }
  }
  
  xlpopn(2);
  return(result);
}
