/* basics - Basic functions for manipulating compound data             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

/* forward declarations */
LOCAL int allfixargs P1H(void);
LOCAL VOID indices_from_rowmajor P3H(LVAL, int, LVAL);
LOCAL LVAL iseq P2H(int, int);
LOCAL LVAL lrepeat P2H(LVAL, int);
LOCAL int old_rowmajor_index P4H(int, LVAL, LVAL, LVAL);
LOCAL VOID permute_indices P4H(LVAL, LVAL, LVAL, int);
LOCAL VOID setcons P3H(LVAL, LVAL, LVAL);
LOCAL VOID setfixnum P2H(LVAL, FIXTYPE);
LOCAL LVAL makeargvec P2H(int, LVAL *);
LOCAL LVAL subarray P4H(LVAL, LVAL, int, LVAL);
LOCAL LVAL subsequence P4H(LVAL, LVAL, int, LVAL);
LOCAL int translate_index P7H(int, LVAL, LVAL, LVAL, LVAL, LVAL, LVAL);


/**************************************************************************/
/**                                                                      **/
/**                         Sequence Predicate                           **/
/**                                                                      **/
/**************************************************************************/

/* Built in SEQUENCEP */
LVAL xssequencep(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();
  return((seqp(x)) ? s_true : NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                           Copying Functions                          **/
/**                                                                      **/
/**************************************************************************/

/* Built in COPY-VECTOR function */
LVAL xscopyvector(V)
{
  LVAL v;
  
  v = xlgetarg();
  xllastarg();
  
  return(copyvector(v));
}

/* internal array copying function */
LVAL copyarray P1C(LVAL, array)
{
  LVAL dim, data, result;
  
  switch (ntype(array)) {
  case VECTOR:
  case STRING:
  case TVEC:
    result = copyvector(array);
    break;
  case DARRAY:
    /* protext some pointers */
    xlstkcheck(2);
    xlsave(dim);
    xlsave(data);
  
    dim = copyvector(getdarraydim(array));
    data = copyvector(getdarraydata(array));
    result = newdarray(dim, data);
  
    /* restore the stack frame */
    xlpopn(2);
    break;
  default:
    xlerror("not an array", array);
    result = NIL; /* not reached */
  }
  
  return(result);
}

LVAL xscopyarray(V)
{
  LVAL array;
  
  array = xlgetarg();
  xllastarg();
  
  return(copyarray(array));
}

/**************************************************************************/
/**                                                                      **/
/**                  Compound Data Decomposition Functions               **/
/**                                                                      **/
/**************************************************************************/

/* Built in SPLIT-LIST function */
LVAL xssplitlist(V)
{
  LVAL data;
  int n;
  
  data = xlgalist();
  n = getfixnum(xlgafixnum());
  xllastarg();
  
  return(split_list(data, n));
}

/**************************************************************************/
/**                                                                      **/
/**                         WHICH Function                               **/
/**                                                                      **/
/**************************************************************************/

/* Built in WHICH function. Generates indices in the data sequence of     */
/* a compound data item where argument elements are not nil. Should do    */
/* something more reasonable for non sequence compound data.              */
LVAL xswhich(V)
{
  LVAL x, result, data, index, tail = NIL;
  int i, n;
  
  /* protect the result pointer */
  xlstkcheck(3);
  xlsave(result);
  xlsave(index);
  xlsave(data);
  
  x = xlgetarg();
  xllastarg();
  
  if (compoundp(x)) {
    data = compounddataseq(x);
    n = compounddatalen(x);
    for (i = 0; i < n; i++)
      if (getnextelement(&x, i) != NIL) {
	index = cvfixnum((FIXTYPE) i);
	if (result == NIL) {
	  result = consa(index);
	  tail = result;
	}
	else {
	  rplacd(tail, consa(index));
	  tail = cdr(tail);
	}
      }
  }
  else xlbadtype(x);

  /* restore the stack frame */
  xlpopn(3);
  
  return(result);
}

/**************************************************************************/
/**                                                                      **/
/**                       List Construction Functions                    **/
/**                                                                      **/
/**************************************************************************/

/* internal version of ISEQ function */
LOCAL LVAL iseq P2C(int, m, int, n)
{
  int length, i;
  LVAL result, next;

  /* protect the result pointer */
  xlsave1(result);
  
  length = abs(n - m) + 1;
  result = mklist(length, NIL);
  
  for (next = result, i = m; consp(next); next = cdr(next), 
       (m <= n) ? i++ : i--) 
    rplaca(next, cvfixnum((FIXTYPE) i));
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

/* Built in ISEQ function. Generates a list of consecutive integers */
LVAL xsiseq(V)
{
  int n, m;
  
  m = getfixnum(xlgafixnum());
  if (moreargs()) n = getfixnum(xlgafixnum());
  else if (m > 0) {
    n = m - 1;
    m = 0;
  }
  else if (m < 0) {
    m = m + 1;
    n = 0;
  }
  else return(NIL);
  xllastarg();

  return(iseq(m, n));
}

/* Built in REPEAT function */
LOCAL LVAL lrepeat P2C(LVAL, arg, int, n)
{
  LVAL data, nextd, nextr, result;
  
  /* protect some pointers */
  xlstkcheck(2);
  xlsave(data);
  xlsave(result);
  
  if (compoundp(arg))
    data = coerce_to_list(arg);
  else
    data = consa(arg);
  
  /* make new data list */
  result = mklist(n * llength(data), NIL);
  
  /* insert values from data into list */
  for (nextr = result, nextd = data; consp(nextr); 
       nextr = cdr(nextr), nextd = cdr(nextd)) {
    if (nextd == NIL) nextd = data; /* cycle through the data */
    rplaca(nextr, car(nextd));
  }

  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}

LVAL xsrepeat(V)
{
  LVAL data, result;
  int reps;
 
  if (xlargc != 2) xlfail("wrong number of arguments");
  else if (compoundp(xlargv[1])) {
    xlsave1(result);
    result = subr_map_elements(xsrepeat);
    result = coerce_to_list(result);
    result = nested_list_to_list(result, 2);
    xlpop();
  }
  else {
    data = xlgetarg();
    reps = getfixnum(checknonnegint(xlgetarg()));
    xllastarg();
    result = lrepeat(data, reps);
  }
  return(result);
}

/**************************************************************************/
/**                                                                      **/
/**               Subset Selection and Mutation Functions                **/
/**                                                                      **/
/**************************************************************************/

/* select or set the subsequence corresponding to the specified indices */
LOCAL LVAL subsequence P4C(LVAL, x,
			   LVAL, indices,
			   int, set_values,
			   LVAL, values)
{
  int rlen, dlen, i, j, idx;
  LVAL result, nextx, nextr, index, elem;

  /* Check the input data */
  if (! seqp(x))
    xlbadtype(x);
  if (! seqp(indices) && indices != s_true)
    xlbadtype(indices);

  /* protect some pointers */
  xlsave1(result);

  dlen = seqlen(x);
  rlen = (indices == s_true) ? dlen : seqlen(indices);

  /* set up the result/value sequence */
  if (set_values) {
    if (! compoundp(values)) /**** could be more efficient */
      result = values = mklist(rlen, values);
    else {
      if (seqlen(values) != rlen) 
	xlfail("value and index sequences do not match");
      result = values;
    }
  }
  else if listp(x)
    result = mklist(rlen, NIL);
  else
    result = mktvec(rlen, gettvecetype(x));

  /* get or set the sequence elements */
  if (indices == s_true) /* do all indices */
    if (set_values)
      for (i = 0; i < dlen; i++)
	setnextelement(&x, i, getnextelement(&values, i));
    else
      result = x;
  else { 
    if (set_values) {
      for (nextx = x, nextr = result, i = 0, j = 0; i < rlen; i++) {
	index = getnextelement(&indices, i);
	if (! fixp(index)) xlerror("not an integer", index);
	idx = getfixnum(index);
	if (idx < 0 || dlen <= idx)
	  xlerror("index out of range", index);
	elem = getnextelement(&result, i);
	if (listp(x)) {
	  if (j > idx) {
	    j = 0;
	    nextx = x;
	  }
	  for (; j < idx && consp(nextx); j++, nextx = cdr(nextx));
	  rplaca(nextx, elem);
	}
	else 
	  settvecelement(x, idx, elem);
      }
    }
    else {
      for (nextx = x, nextr = result, i = 0, j = 0; i < rlen; i++) {
	index = getnextelement(&indices, i);
	if (! fixp(index)) xlerror("not an integer", index);
	idx = getfixnum(index);
	if (idx < 0 || dlen <= idx)
	  xlerror("index out of range", index);
	if (listp(x)) {
	  if (j > idx) {
	    j = 0;
	    nextx = x;
	  }
	  for (; j < idx && consp(nextx); j++, nextx = cdr(nextx));
	  elem = car(nextx);
	}
	else 
	  elem = gettvecelement(x, idx);
	setnextelement(&nextr, i, elem);
      }
    }
  }
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

/* translate row major index in resulting submatrix to row major index in */
/* the original matrix                                                    */
LOCAL int old_rowmajor_index P4C(int, index,
				 LVAL, indices,
				 LVAL, dim,
				 LVAL, olddim)
{
  int face, oldface, rank, i, oldindex;
  
  rank = getsize(dim);
  
  for (face = 1, oldface = 1, i = 0; i < rank; i++) {
    face *= getfixnum(getelement(dim, i));
    oldface *= getfixnum(getelement(olddim, i));
  }
  
  for (oldindex = 0, i = 0; i < rank; i++) {
    face /= getfixnum(getelement(dim, i));
    oldface /= getfixnum(getelement(olddim, i));
    oldindex +=
      oldface *
	getfixnum(getelement(getelement(indices, i), index / face));
    index = index % face;
  }
  return(oldindex);
}

/* make arguments into vector */
LOCAL LVAL makeargvec P2C(int, argc, LVAL *, argv)
{
  LVAL val;
  int i;

  val = newvector(argc);
  for (i = 0; i < argc; i++)
    setelement(val, i, argv[i]);
  return val;
}

/* extract or set subarray for the indices from a displaced array */
LOCAL LVAL subarray P4C(LVAL, a,
			LVAL, indices,
			int, set_values,
			LVAL, values)
{
  LVAL index, dim, vdim, data, result_data, olddim, result;
  int rank, m, n, i, j, k;
  
  /* protect some pointers */
  xlstkcheck(2);
  xlsave(dim);
  xlsave(result);

  if (! darrayp(a)) xlbadtype(a);
  if (getsize(indices) != getdarrayrank(a))
    xlfail("wrong number of indices");

  olddim = getdarraydim(a);

  /* compute the result dimension vector and fix up the indices */
  rank = getdarrayrank(a);
  dim = newvector(rank);
  for (i = 0; i < rank; i++) {
    index = getelement(indices, i);
    n = getfixnum(getelement(olddim, i));
    if (index == s_true) {
      index = newvector(n);
      setelement(indices, i, index);
      for (j = 0; j < n; j++)
	setelement(index, j, cvfixnum((FIXTYPE) j));
    }
    else {
      index = coerce_to_tvec(index, s_true);
      k = gettvecsize(index);
      for (j = 0; j < k; j++) 
	if (n <= getfixnum(checknonnegint(gettvecelement(index, j))))
	  xlerror("index out of bounds", gettvecelement(index, j));
      setelement(indices, i, index);
    }
    setelement(dim, i, cvfixnum((FIXTYPE) gettvecsize(index)));
  }
    
  /* set up the result or check the values*/
  if (set_values) {
    if (! compoundp(values)) /**** could be more efficient */
      result = mkarray(dim, k_initelem, values, s_true);
    else {
      if (! darrayp(values) || rank != getdarrayrank(values))
	xlbadtype(values);
      vdim = getdarraydim(values);
      for (i = 0; i < rank; i++) 
	if (getfixnum(getelement(vdim, i)) != getfixnum(getelement(dim, i)))
	  xlbadtype(values);
      result = values;
    }
  }
  else
    result = mkarray(dim, NIL, NIL, s_true);

  /* compute the result or set the values */
  data = getdarraydata(a);
  result_data = getdarraydata(result);
  m = gettvecsize(data);
  n = gettvecsize(result_data);
  for (i = 0; i < n; i++) {
    k = old_rowmajor_index(i, indices, dim, olddim);
    if (0 > k || k >= m)
      xlfail("index out of range");
    if (set_values)
      settvecelement(data, k, gettvecelement(result_data, i));
    else
      settvecelement(result_data, i, gettvecelement(data, k));
  }
  
  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}

/* are all arguments beyond the first fixnums? */
LOCAL int allfixargs(V)
{
  int i;
  
  for (i = 1; i < xlargc; i++) 
    if (! fixp(xlargv[i])) return(FALSE);
  return(TRUE);
}

/* Built in SELECT function */
LVAL xsselect(V)
{
  LVAL x, indices, result = NIL;
  
  if (allfixargs()) {
    if (darrayp(peekarg(0))) result = xaref();
    else result = xelt();
  }
  else if (seqp(peekarg(0))) {
    x = xlgetarg();
    indices = xlgetarg();
    result = subsequence(x, indices, FALSE, NIL);
  }
  else if (darrayp(peekarg(0))) {
    xlsave1(indices);
    x = xlgetarg();
    indices = makeargvec(xlargc, xlargv);
    result = subarray(x, indices, FALSE, NIL);
    xlpop();
  }
  else xlbadtype(xlgetarg());

  return(result);
}

/**** this could be dangerous */
LOCAL VOID setcons P3C(LVAL, x, LVAL, first, LVAL, rest)
{
  setntype(x, CONS);
#ifdef NEWGC
  Rplaca(x, first);
  Rplacd(x, rest);
#else
  rplaca(x, first);
  rplacd(x, rest);
#endif
}

/* Built in SET-SELECT (SETF method for SELECT) */
/* This function uses node data to avoid creating garbage nodes. */
/* This use of nodes *should* be safe, since there *should* be   */
/* no chance of a garbage collection during this operation.      */
LVAL xssetselect(V)
{
  LVAL x, indices, values;
  struct node index_node, value_node;
  LVAL i_list = &index_node, v_list = &value_node;
  
  xlsave1(indices);
  xlsave1(values);
  
  x = xlgetarg();
  if (xlargc < 1) xltoofew();
  indices = makeargvec(xlargc - 1, xlargv);
  values = xlargv[xlargc - 1];

  if (seqp(x)) {
    if (getsize(indices) != 1)
      xlerror("bad indices", indices);
    indices = getelement(indices, 0);
    if (fixp(indices)) {
      setcons(i_list, indices, NIL);
      setcons(v_list, values, NIL);
      subsequence(x, i_list, TRUE, v_list);
    }
    else
      subsequence(x, indices, TRUE, values);
  }
  else if (darrayp(x))
    subarray(x, indices, TRUE, values);
  else xlbadtype(x);

  xlpopn(2);

  return(values);
}

/**************************************************************************/
/**                                                                      **/
/**                     Array Permutation Function                       **/
/**                                                                      **/
/**************************************************************************/


/* permute x into y using perm; all should be vectors; If check is TRUE */
/* the routine will check to make sure no indices are reused, but x     */
/* will be destroyed.                                                   */
LOCAL VOID permute_indices P4C(LVAL, x, LVAL, y, LVAL, perm, int, check) 
{
  LVAL index;
  int rank, i, k;

  rank = getsize(x);
  for (i = 0; i < rank; i++) {
    index = getelement(perm, i);
    if (! fixp(index)) xlerror("bad permutation sequence", perm);
    k = getfixnum(index);
    if (k < 0 || k >= rank) xlerror("bad permutation sequence", perm);
    setelement(y, i, getelement(x, k));
    if (check)
      setelement(x, k, NIL); /* to insure dimensions are not re-used */
  }
}

/* compute indices in a from rowmajor index k, put in vector result */
/* The indices are stored in cons cells, which are treated locally  */
/* fixnums. This SEEMS to be safe since it is entirely local, but   */
/* it may be dangerous......                                        */
/**** this could be dangerous */
/* set a fixnum node */

LOCAL VOID setfixnum P2C(LVAL, node, FIXTYPE, val)
{
  node->n_fixnum = val;
  setntype(node, FIXNUM);
}

LOCAL VOID indices_from_rowmajor P3C(LVAL, a, int, k, LVAL, result)
{
  LVAL next, dim;
  int face, i, rank;
  
  if (0 > k || k >= gettvecsize(getdarraydata(a)))
    xlfail("index out of range");
  
  dim = getdarraydim(a);
  rank = getdarrayrank(a);
  
  for (i = 0, face = 1, next = dim; i < rank; i++)
    face *= getfixnum(getnextelement(&next, i));

  for (i = 0, next = dim; i < rank; i++) {
    face /= getfixnum(getnextelement(&next, i));
    setfixnum(gettvecelement(result, i),(FIXTYPE) k / face);
    k = k % face;
  }
}

/* Translate row major index in original array to row major index in new */
/* array. Use indices vectors and ilist for temporary storage.           */
LOCAL int translate_index P7C(int, i, LVAL, result, LVAL, x, LVAL, perm, LVAL, indices,
                              LVAL, oldindices, LVAL, ilist)
{
  LVAL next;
  int rank, k;

  rank = getdarrayrank(x);

  indices_from_rowmajor(x, i, oldindices); 
  permute_indices(oldindices, indices, perm, FALSE);

  for (next = ilist, k = 0; k < rank && consp(next); k++, next = cdr(next))
    rplaca(next, getelement(indices, k));

  return(rowmajorindex(result, ilist, FALSE));
}

/* Built in PERMUTE-ARRAY function */
LVAL xspermutearray(V)
{
  LVAL x, perm, result, data, result_data, dim, olddim, indices;
  LVAL oldindices, ilist;
  int rank, i, k, n;

  /* protect some pointers */
  xlstkcheck(7);
  xlsave(result);
  xlsave(dim);
  xlsave(olddim);
  xlsave(indices);
  xlsave(oldindices);
  xlsave(perm);
  xlsave(ilist);

  /* Get and check the arguments */
  x = xlgadarray();
  perm = xlgaseq();
  perm = coerce_to_tvec(perm, s_true);
  if (gettvecsize(perm) != getdarrayrank(x))
    xlerror("bad permutation sequence", perm);
  xllastarg();

  /* get old dimension vector */
  olddim = getdarraydim(x);
  rank = getdarrayrank(x);

  /* construct new dimension vector */
  dim = newvector(rank);
  olddim = copyvector(olddim); /* since permute_indices will destroy this */
  permute_indices(olddim, dim, perm, TRUE);

  /* make result array and the index vectors and lists */
  data = getdarraydata(x);
  n = gettvecsize(data);
  result = mktvec(gettvecsize(data), gettvecetype(data));
  result = newdarray(dim, result);
  indices = newvector(rank);
  oldindices = newvector(rank);
  for (i = 0; i < rank; i++)
    setelement(oldindices, i, consa(NIL));
  ilist = mklist(rank, NIL);

  /* fill in the result */
  result_data = getdarraydata(result);
  for (i = 0; i < n; i++) {
    k = translate_index(i, result, x, perm, indices, oldindices, ilist);
    settvecelement(result_data, k, gettvecelement(data, i));
  }

  /* restore stack */
  xlpopn(7);

  return(result);
}
