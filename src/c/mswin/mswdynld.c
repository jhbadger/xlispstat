/* mswdynld - Dynamic loading and C function calling routines.         */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/* Calling conventions are based on the conventions given in the New S */
/* book.                                                               */

#include "xlisp.h"
#include "xlstat.h"
#include "xlsx.h"

extern LVAL s_dll_list;

/************************************************************************/
/**                                                                    **/
/**                      DLL Handling Functions                        **/
/**                                                                    **/
/************************************************************************/

LVAL xsload_dll()
{
  char *dllname;
  HANDLE hdll;
  LVAL dllhandle, dll_list;
#ifdef DODO
{
  HANDLE h1,h2;
  h1 = LoadLibrary("foo.dll");
  h2 = LoadLibrary("simplex.dll");
  FreeLibrary(h1);
  FreeLibrary(h2);
  return(NIL);
}
#endif DODO

  dllname = (char *) getstring(xlgastring());
  xllastarg();
  hdll = LoadLibrary(dllname);

  if ((UINT) hdll >= 32) {
    xlsave1(dllhandle);
    dllhandle = cvfixnum((FIXTYPE) hdll);
    dll_list = consp(getvalue(s_dll_list)) ? getvalue(s_dll_list) : NIL;
    setvalue(s_dll_list, cons(dllhandle, dll_list));
    xlpop();
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = dllhandle;
#endif
    return(dllhandle);
  }
  else {
#ifdef MULVALS
    xlnumresults = 2;
    xlresults[0] = NIL;
    xlresults[1] = cvfixnum((FIXTYPE) hdll);
#endif
    return NIL;
  }
}

LVAL xsfree_dll()
{
  HANDLE hdll;
  LVAL last, list;

  hdll = (HANDLE) getfixnum(xlgafixnum());
  xllastarg();

//  SysBeep(10);
//  return(NIL);

  if ((UINT) hdll >= 32) {
    for (last = NIL, list = getvalue(s_dll_list);
	 consp(list);
	 last = list, list = cdr(list)) {
      //*** because of some bug, on a 386SX20 at least, frees MUST be done
      //*** on a last in-first out basis. So only the head of the list
      //*** can be freed. Hence the following line:
      if (consp(last)) break;
      if (hdll == (HANDLE) getfixnum(car(list))) {
	if (consp(last)) cdr(last) = cdr(list);
	else setvalue(s_dll_list, cdr(list));
	FreeLibrary(hdll);
	break;
      }
    }
  }
  return(NIL);
}

void MSWDLLCleanup()
{
  LVAL list;

  if (s_dll_list != NULL)
    for (list = getvalue(s_dll_list); consp(list); list = cdr(list))
      FreeLibrary((HANDLE) getfixnum(car(list)));
}

/************************************************************************/
/**                                                                    **/
/**               Allocation and Error Signalling Functions            **/
/**                                                                    **/
/************************************************************************/

static LVAL current_allocs = NULL;
#define fixup_current_allocs \
  { if (current_allocs == NULL) current_allocs = NIL; }

/* allocate space that will be garbage collected after return */
static char *xscall_alloc(int n, int m)
{
  LVAL adata;
  char *p;

  fixup_current_allocs;

  adata = newadata(n, m, FALSE);
  if (adata == NIL || (p = getadaddr(adata)) == NULL)
    xlfail("allocation failed");
  current_allocs = cons(adata, current_allocs);
  return(p);
}

/************************************************************************/
/**                                                                    **/
/**                Lisp to C/FORTRAN Data Conversion                   **/
/**                                                                    **/
/************************************************************************/

#define IN 0
#define RE 1

typedef struct {
  int type, size;
  char *addr;
} call_arg;

/* convert lisp argument to allocated pointer */
static call_arg lisp2arg(LVAL x)
{
  call_arg a;
  LVAL elem, data;
  int i;

  xlprot1(x);

  /* make sure x is a sequence and find its length */
  if (! seqp(x)) x = consa(x);
  a.size = seqlen(x);

  /* determine the mode of the data */
  for (i = 0, a.type = IN, data = x; i < a.size; i++) {
    elem = getnextelement(&data, i);
    if (floatp(elem)) a.type = RE;
#ifdef BIGNUMS
    else if (ratiop(elem)) a.type = RE;
#endif
    else if (! integerp(elem)) xlerror("not a real number", elem);
  }

  /* allocate space for the data */
  a.addr = xscall_alloc(a.size, (a.type == IN) ? sizeof(int) : sizeof(double));

  /* fill the space */
  for (i = 0, data = x; i < a.size; i++) {
    elem = getnextelement(&data, i);
    if (a.type == IN) ((int *) a.addr)[i] = getfixnum(elem);
    else ((double *) a.addr)[i] = makefloat(elem);
  }

  xlpop();
  return(a);
}

/* copy allocated pointer back to new lisp list */
static LVAL arg2lisp(call_arg a)
{
  LVAL x, next;
  int i;

  xlsave1(x);
  x = mklist(a.size, NIL);
  for (i = 0, next = x; i < a.size; i++, next = cdr(next)) {
    if (a.type == IN) rplaca(next, cvfixnum((FIXTYPE) ((int *) a.addr)[i]));
    else rplaca(next, cvflonum((FLOTYPE) ((double *) a.addr)[i]));
  }
  xlpop();

  return(x);
}

/************************************************************************/
/**                                                                    **/
/**                 Foreign Function Call Function                     **/
/**                                                                    **/
/************************************************************************/

typedef void VFUN(XLSXblock *);
typedef VFUN *VFPTR;

LVAL xscall_cfun()
{
  LVAL result, Lfun, old_allocs, next;
  call_arg *args, *pargs;
  int nargs, i;
  VFPTR routine;
  HANDLE hdll;
  char *name;
  XLSXblock params;

  fixup_current_allocs;

  // ### patch this to handle errors properly (reset allocs, etc) -- use dynamic scoping
  // ### also use allocation with free as in (new) linalg?
  xlstkcheck(3);
  xlsave(old_allocs);
  xlprotect(current_allocs);
  xlsave(result);
  old_allocs = current_allocs;
  current_allocs = NIL;

  /* get the routine pointer */
  hdll = (HANDLE) getfixnum(xlgafixnum());
  Lfun = xlgetarg();
  if (stringp(Lfun)) name = getstring(Lfun);
  else if (fixp(Lfun)) name = (char *) MAKEINTRESOURCE((int) getfixnum(Lfun));
  routine = (VFPTR) GetProcAddress(hdll, name);
  if (! routine) xlerror("can't find function address", Lfun);

  /* convert the arguments to allocated pointers */
  nargs = xlargc;
  if (nargs == 0) xlfail("too few arguments");
  args = (call_arg *) xscall_alloc(nargs, sizeof(call_arg));
  params.argc = nargs;
  params.argv = (char **) xscall_alloc(nargs, sizeof(char *));
  for (i = 0; i < nargs; i++) {
    args[i] = lisp2arg(xlgetarg());
    params.argv[i] = args[i].addr;
  }

  /* make the call */
  (*routine)(&params);

  /* convert the pointers back to lists, grouped in a list */
  result = (nargs > 0) ? mklist(nargs, NIL) : NIL;
  for (next = result, pargs = args; consp(next); next = cdr(next), pargs++)
    rplaca(next, arg2lisp(*pargs));

  current_allocs = old_allocs;
  xlpopn(3);

  return(result);
}
