/* utilities - basic utility functions                                 */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/****** ///// clean this stuff up */

#include "xlisp.h"
#include "xlstat.h"

typedef LVAL (*subrfun)(V);

/************************************************************************/
/**                           Basic Utilities                          **/
/************************************************************************/

/* return list of two elements */
LVAL list2 P2C(LVAL, x1, LVAL, x2)
{
  LVAL list, y1, y2;
  
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(list);
  xlsave(y1);
  xlsave(y2);
  
  y1 = x1;
  y2 = x2;
  list = consa(y2);
  list = cons(y1, list);
  
  /* restore the stack frame */
  xlpopn(3);
  
  return(list);
}

/* return list of three elements */
LVAL list3 P3C(LVAL, x1, LVAL, x2, LVAL, x3)
{
  LVAL list, y1, y2, y3;
  
  /* protect some pointers */
  xlstkcheck(4);
  xlsave(list);
  xlsave(y1);
  xlsave(y2);
  xlsave(y3);

  y1 = x1;
  y2 = x2;
  y3 = x3;
  list = consa(y3);
  list = cons(y2, list);
  list = cons(y1, list);
  
  /* restore the stack frame */
  xlpopn(4);
  
  return(list);
}

/* return the i-th argument, without popping it; signal an error if needed. */
LVAL peekarg P1C(int, i)
{
  if (xlargc <= i) xltoofew();
  return(xlargv[i]);
}

/* Get the next element in the sequence; cdr the pointer if it is a list */
LVAL getnextelement P2C(LVAL *, pseq, int, i)
{
  LVAL value;

  switch (ntype(*pseq)) {
  case VECTOR:
    value = getelement(*pseq, i);
    break;
  case TVEC:
  case STRING:
    value = gettvecelement(*pseq, i);
    break;
  case CONS:  
    value = car(*pseq);
    *pseq = cdr(*pseq);
    break;
  default:
    xlbadtype(*pseq);
    value = NIL;
  }
  return(value);
}

/* Set the next element in the sequence; cdr the pointer if it is a list */
VOID setnextelement P3C(LVAL *, pseq, int, i, LVAL, value)
{
  switch (ntype(*pseq)) {
  case VECTOR:
    setelement(*pseq, i, value);
    break;
  case TVEC:
  case STRING:
    settvecelement(*pseq, i, value);
    break;
  case CONS:
    rplaca(*pseq, value);
    *pseq = cdr(*pseq);
    break;
  default:
    xlbadtype(*pseq);
  }
}


/************************************************************************/
/**                  Function Applicaiton Utilities                    **/
/************************************************************************/

VOID pushargvec P3C(LVAL, fun, int, argc, LVAL *, argv)
{
  LVAL *newfp, *oldsp;
  int i;

  /* build a new argument stack frame */
  newfp = oldsp = xlsp;
  pusharg(NIL); /* place holder for stack frame increment */
  pusharg(fun);
  pusharg(NIL); /* place holder for argc */

  /* push the arguments */
  for (i = 0; i < argc; i++)
    pusharg(argv[i]);

  /* establish the new stack frame */
  oldsp[0] = cvfixnum((FIXTYPE)(newfp - xlfp));
  oldsp[2] = cvfixnum((FIXTYPE) argc);
  xlfp = newfp;
}

LVAL xsapplysubr P2C(subrfun, f, LVAL, args)
{
  LVAL *oldargv, val;
  int argc, oldargc;
   
  xlprot1(args); /* protect arguments while pushing */
  argc = pushargs(NIL, args);
  xlpop();       /* now they are protected since they are on the stack */

  oldargc = xlargc;
  oldargv = xlargv;
  xlargc = argc;
  xlargv = xlfp + 3;
  val = (*f)();
  xlargc = oldargc;
  xlargv = oldargv;

  /* remove the call frame */
  xlsp = xlfp;
  xlfp = xlfp - (int)getfixnum(*xlfp);
  return(val);
}

LVAL xscallsubrvec P3C(subrfun, f, int, argc, LVAL *, argv)
{
  LVAL *oldargv, val;
  int oldargc;
   
  pushargvec(NIL, argc, argv);
  oldargc = xlargc;
  oldargv = xlargv;
  xlargc = argc;
  xlargv = xlfp + 3;
  val = (*f)();
  xlargc = oldargc;
  xlargv = oldargv;

  /* remove the call frame */
  xlsp = xlfp;
  xlfp = xlfp - (int)getfixnum(*xlfp);
  return(val);
}

LVAL xsfuncall0 P1C(LVAL, fun)
{
  pushargvec(fun, 0, NULL);
  return(xlapply(0));
}

LVAL xsfuncall1 P2C(LVAL, fun, LVAL, x)
{
  pushargvec(fun, 1, &x);
  return(xlapply(1));
}

LVAL xsfuncall2 P3C(LVAL, fun, LVAL, x, LVAL, y)
{
  LVAL args[2];
  
  args[0] = x;
  args[1] = y;
  pushargvec(fun, 2, args);
  return(xlapply(2));
}

/* replicates a list n times */ 
int xsboolkey P2C(LVAL, key, int, dflt)
{
  LVAL val;
  int result = dflt;
  
  if (xlgetkeyarg(key, &val)) result = ((val != NIL) ? TRUE : FALSE);
  return(result);
}
