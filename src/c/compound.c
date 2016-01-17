/* compound - Compound data implementation and Elementwise mapping     */
/* functions.                                                          */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL s_compound_data_proto;
extern LVAL sk_data_length, sk_data_seq, sk_make_data;

/* forward declarations */
LOCAL LVAL checkcompound _((LVAL x));
LOCAL int findmaprlen _((LVAL args));
LOCAL VOID pushnextargs _((LVAL fcn, int n, LVAL args, int i));
LOCAL LVAL map _((LVAL type, LVAL fcn, LVAL args, int rlen));
LOCAL LVAL findcompound _((int skip_one));
LOCAL int findrlen _((LVAL args));
LOCAL VOID fixuparglist _((LVAL list));


/*************************************************************************/
/*************************************************************************/
/**                                                                     **/
/**                    Compound Data Implementation                     **/
/**                                                                     **/
/*************************************************************************/
/*************************************************************************/

/* Compound data items contain a data sequence and structural            */
/* information. The sequence can be extracted, the natural type of the   */
/* sequence can be determined, the length of the sequence can be         */
/* determined and a sequence of the appropriate length can be coerced to */
/* match the shape of an object.                                         */
/*                                                                       */
/* For the moment, x is compound if it is a cons or an array of positive */
/* size, or an object iheriting from COMPOUND-DATA-PROTO.                */
/*                                                                       */
/* If x is compound and y is a sequence then makecompound(x, seq) will   */
/* return a compound item of the same shape as x with data sequence seq. */
/* for sequences, same shape means same length. For arrays it means      */
/* equal dimensions. For objects it means whatever x thinks it means.    */

/* internal predicate */
int compoundp P1C(LVAL, x)
{
  switch (ntype(x)) {
  case FIXNUM:
  case FLONUM:
  case COMPLEX:
    return(FALSE);
  case CONS:
    return(TRUE);
  case DARRAY:
    x = getdarraydata(x);
    if (stringp(x))
      return(FALSE);
    /* fall through */
  case VECTOR:
  case TVEC:
    return(gettvecsize(x) > 0 ? TRUE :FALSE);
  case OBJECT:
    return(kind_of_p(x, getvalue(s_compound_data_proto)));
  default:
    return(FALSE);
  }
}

/* Built in COMPOUNDP */
LVAL xscompoundp(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();
  return((compoundp(x)) ? s_true : NIL);
}

/* Check for a compound data item; pass it through or signal an error */
LOCAL LVAL checkcompound P1C(LVAL, x)
{
  if (! compoundp(x)) xlerror("not a compound data item", x);
  return(x);
}

/**** is this needed? */
/* find length of a compound item's data sequence */
int compounddatalen P1C(LVAL, x)
{
  switch (ntype(x)) {
  case OBJECT:
    {
      LVAL n = send_message(x, sk_data_length);
      if (! fixp(n) || getfixnum(n) < 0) xlerror("bad length", n);
      return((int) getfixnum(n));
    }
  case CONS:
    return(llength(x));
  case DARRAY:
    x = getdarraydata(x);
    if (stringp(x))
      xlbadtype(x);
    /* fall through */
  case VECTOR:
  case TVEC:
    return(gettvecsize(x));
  case SYMBOL:
    if (null(x)) return(0);
  default:
    xlbadtype(x);
    return(0);
  }
}

/* Built in COMPOUND-DATA-LENGTH */
LVAL xscompound_length(V)
{
  LVAL x;
  
  x = checkcompound(xlgetarg());
  xllastarg();
  return(cvfixnum((FIXTYPE) compounddatalen(x)));
}

/* get compound item's data sequence */
LVAL compounddataseq P1C(LVAL, x) 
{
  switch (ntype(x)) {
  case OBJECT:
    {
      LVAL seq = send_message(x, sk_data_seq);
      if (! listp(seq) && ! vectorp(seq) && ! tvecp(seq))
	xlerror("not a sequence", seq);
      return(seq);
    }
  case DARRAY: return(getdarraydata(x));
  case CONS:
  case VECTOR:
  case TVEC:   return(x);
  case SYMBOL:
    if (null(x)) return(x);
    /* fall through */
  default: return(xlbadtype(x));
  }
}

/* Built in COMPOUND-DATA-SEQ */
LVAL xscompound_seq(V)
{
  LVAL x;
  
  x = checkcompound(xlgetarg());
  xllastarg();
  return(compounddataseq(x));
}

/* get 'natural' type of of compound item's data sequence */
#define compoundseqtype(x) (listp(x)) ? a_list : a_vector;

/* Make sequence into a compound item of the same shape as form */
LVAL makecompound P2C(LVAL, form, LVAL, seq)
{
  LVAL result;

  xlsave1(result);
  if (listp(form))
    result = coerce_to_list(seq);
  else if (vectorp(form) || tvecp(form))
    result = coerce_to_tvec(seq, s_true);
  else if (darrayp(form)) {
    result = coerce_to_tvec(seq, s_true);
    result = newdarray(getdarraydim(form), result);
  }
  else if (objectp(form)) {
    result = send_message_1L(form, sk_make_data, seq);
  }
  else xlerror("not a compound data item", form);

  xlpop();
  return(result);
}

/***********************************************************************/
/**                     REDUCE and MAP functions                      **/
/***********************************************************************/

/**** combine this stuff with in xlseq.c */

/* Common Lisp REDUCE function (internal version) */
LVAL reduce P4C(LVAL, fcn,LVAL,  sequence, int, has_init, LVAL, initial_value)
{
  LVAL next, result;
  int i, n;
  
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(next);
  xlsave(result);
  xlprotect(fcn);

  switch (ntype(sequence)) {
  case CONS:
    next = sequence;
    if (has_init) result = initial_value;
    else {
      result = car(next);
      next = cdr(next);
    }
    for (; consp(next); next = cdr(next)) 
      result = xsfuncall2(fcn, result, car(next));
    break;
  case VECTOR:
  case TVEC:
    n = gettvecsize(sequence);
    i = 0;
    if (has_init) result = initial_value;
    else {
      result = gettvecelement(sequence, 0);
      i = 1;
    }
    for (; i < n; i++) 
      result = xsfuncall2(fcn, result, gettvecelement(sequence, i));
    break;
  default:
    xlbadtype(sequence);
  }

  /* restore the stack frame */
  xlpopn(3);

  return(result);
}

/* compute the length of the result sequence */
LOCAL int findmaprlen P1C(LVAL, args)
{
  LVAL next, e;
  int len, rlen;

  for (rlen = -1, next = args; consp(next); next = cdr(next)) {
    e = car(next);
    if (! listp(e) && ! vectorp(e) && ! tvecp(e))
      xlbadtype(car(next));
    len = seqlen(e);
    if (rlen == -1)
      rlen = len;
    else
      rlen = (len < rlen) ? len : rlen;
  }
  return(rlen);
}

LOCAL VOID pushnextargs P4C(LVAL, fcn, int, n, LVAL, args, int, i)
{
  LVAL *newfp, next, value = NULL;

  /* build a new argument stack frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(fcn);
  pusharg(cvfixnum((FIXTYPE)n));
  
  /* push the arguments and shift the list pointers */
  for (next = args; consp(next); next = cdr(next)) {
    switch (ntype(car(next))) {
    case VECTOR:
      value = getelement(car(next), i);
      break;
    case TVEC:
      value = gettvecelement(car(next), i);
      break;
    case CONS:
      value = car(car(next));
      rplaca(next, cdr(car(next)));
      break;
    }
    pusharg(value);
  }

  /* establish the new stack frame */
  xlfp = newfp;
}
  
/* Internal version of Common Lisp MAP function */
LOCAL LVAL map P4C(LVAL, type, LVAL, fcn, LVAL, args, int, rlen)
{
  LVAL nextr, result;
  int nargs, i;

  /* protect some pointers */
  xlstkcheck(2);
  xlsave(result);
  xlprotect(fcn);
 
  if (rlen < 0) rlen = findmaprlen(args); 
  if (type == a_vector)
    result = newvector(rlen);
  else
    result = mklist(rlen, NIL);
  nargs = llength(args);

  for (i = 0, nextr = result; i < rlen; i++) {
    pushnextargs(fcn, nargs, args, i);
    setnextelement(&nextr, i, xlapply(nargs));
  }

  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}

/*************************************************************************/
/*************************************************************************/
/**                                                                     **/
/**                 Element-Wise Mapping Functions                      **/
/**                                                                     **/
/*************************************************************************/
/*************************************************************************/

/* MAP-ELEMENTS acts like FUNCALL if all arguments are simple (i. e. not */
/* compound). If one is compound all should be of the same shape. In     */
/* this case simple arguments are treates as constant compound items of  */
/* the appropriate shape. The function is applied elementwise and the    */
/* result is returned as a compound item of the same shape as its        */
/* arguments (in particular its first compound argument). If the         */
/* arguments are sequences the result is a sequence of the same type as  */
/* the first sequence argument.                                          */

/* Check the stack for a compound data argument and return it or NIL     */
LOCAL LVAL findcompound P1C(int, skip_one)
{
  LVAL *next;
  int n;
  
  n = xlargc;
  next = xlargv;
  
  if (skip_one) {
    n--;
    next++;
  }

  for (; n > 0; n--, next++) 
    /* pretesting to speed up non-compound case a bit */
    if (! numberp(*next) && ! stringp(*next) && compoundp(*next))
      return(*next);
  return(NIL);
}

/* find the length of the result sequence for map for the arguments in args */
LOCAL int findrlen P1C(LVAL, args)
{
  LVAL next;
  int len, rlen;

  for (rlen = -1, next = args; consp(next); next = cdr(next))
    if (compoundp(car(next))) {
      len = compounddatalen(car(next));
      if (rlen < 0) rlen = len;
      else if (len != rlen) xlfail("arguments not all the same length");
    }
  return(rlen);
}

/* replace displaced array arguments by their data vectors and simple */
/* arguments by circular lists of one element.                        */
LOCAL VOID fixuparglist P1C(LVAL, list)
{
  LVAL next;
  for (next = list; consp(next); next = cdr(next))
    if (! compoundp(car(next))) { 
      /* make circular list */
      rplaca(next, consa(car(next)));
      rplacd(car(next), car(next));
    }
    else
      rplaca(next, compounddataseq(car(next)));
}

typedef LVAL (*mapfun)(V);

/* MAP-ELEMENTS for internal subroutines */
LVAL subr_map_elements P1C(mapfun, f)
{
  LVAL arglist, result, fcn, first_compound, type;
  int rlen;

  first_compound = findcompound(FALSE);

  if (first_compound == NIL) result = (*f)();
  else {
    xlstkcheck(3);
    xlsave(arglist);
    xlsave(fcn);
    xlsave(result);
    fcn = cvsubr(f, SUBR, 0);
    type = compoundseqtype(first_compound);
    arglist = makearglist(xlargc, xlargv);
    rlen = findrlen(arglist);
    fixuparglist(arglist);
    result = map(type, fcn, arglist, rlen);
    result = makecompound(first_compound, result);
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = result;
#endif /* MULVALS */
    xlpopn(3);
  }
  return(result);
}

/* recursive MAP-ELEMENTS for internal subroutines */
LVAL recursive_subr_map_elements P2C(mapfun, bf, mapfun, f)
{
  if (findcompound(FALSE) == NIL) return((*bf)());
  else return(subr_map_elements(f));
}

/* Built in MAP-ELEMENTS */
LVAL xsmap_elements(V)
{
  LVAL arglist, result, fcn, first_compound, type;
  int rlen;

  if (xlargc < 2) xltoofew();
  first_compound = findcompound(TRUE);

  if (first_compound == NIL) result = xfuncall();
  else {
    xlstkcheck(2)
    xlsave(arglist);
    xlsave(result);
    fcn = xlgetarg();
    type = compoundseqtype(first_compound);
    arglist = makearglist(xlargc, xlargv);
    rlen = findrlen(arglist);
    fixuparglist(arglist);
    result = map(type, fcn, arglist, rlen);
    result = makecompound(first_compound,result);
    xlpopn(2);
  }
  return(result);
}
