/* xlshlib - Shared library support functions.                      */
/* XLISP-STAT 2.1 Copyright (c) 1990-1997, by Luke Tierney          */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz */
/* You may give out copies of this software; for conditions see the */
/* file COPYING included with this distribution.                    */

#include "xlisp.h"
#ifdef SHAREDLIBS
#include <dlfcn.h>
#define cvvoidptr(x) newnatptr(x, NIL)
#define setsubr(x,v) getsubr(x)=(v)
#define setoffset(x,v) getoffset(x)=(v)
#define xlgasubr()    (testarg(typearg(subrp)))

static void shlib_error()
{
  char *str = dlerror();
  xlfail(str != NULL ? str : "unknown shared library error");
}

/* SHLIB-OPEN path */
LVAL xshlibopen()
{
  char *name;
  void *handle;
  name = getstring(xlgastring());
  xllastarg();
  if ((handle = dlopen(name, RTLD_NOW)) == NULL)
    shlib_error();
  return newnatptr(handle, NIL);
}

/* SHLIB-SYMADDR lib name &optional error */
LVAL xshlibsymaddr()
{
  void *val;
  LVAL lib = xlganatptr();
  void *handle = getnpaddr(lib);
  char *name = getstring(xlgastring());
  int err = moreargs() ? null(xlgetarg()) : TRUE;
  xllastarg();
  if ((val = dlsym(handle, name)) == NULL) {
    if (err)
      shlib_error();
    else
      return NIL;
  }
  return newnatptr(val, lib);
}

/* SHLIB-CLOSE lib */
LVAL xshlibclose()
{
  void *lib = getnpaddr(xlganatptr());
  xllastarg();
  if (dlclose(lib) == -1)
    shlib_error();
  return NIL;
}

#define MAX_CALLADDR_ARGS 16

/* CALL-BY-ADDRESS &rest args */
LVAL xshlibcalladdr()
{
  void *(*f)() = (void *(*)()) getnpaddr(xlganatptr());
  void *a[MAX_CALLADDR_ARGS];
  int n, i;

  if (xlargc > MAX_CALLADDR_ARGS)
    xltoomany();

  for (n = xlargc, i = 0; i < n; i++) {
    LVAL arg = xlgetarg();
    if (fixp(arg))
      a[i] = (void *) getfixnum(arg);
    else if (natptrp(arg))
      a[i] = getnpaddr(arg);
    else
      xlbadtype(arg);
  }
  
  switch (n) {
  case 0: return cvvoidptr(f());
  case 1: return cvvoidptr(f(a[0]));
  case 2: return cvvoidptr(f(a[0],a[1]));
  case 3: return cvvoidptr(f(a[0],a[1],a[2]));
  case 4: return cvvoidptr(f(a[0],a[1],a[2],a[3]));
  case 5: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4]));
  case 6: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5]));
  case 7: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6]));
  case 8: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]));
  case 9: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
  case 10: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],
                              a[9]));
  case 11: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],
                              a[9],a[10]));
  case 12: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],
                              a[9],a[10],a[11]));
  case 13: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],
                              a[9],a[10],a[11],a[12]));
  case 14: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],
                              a[9],a[10],a[11],a[12],a[13]));
  case 15: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],
                              a[9],a[10],a[11],a[12],a[13],a[14]));
  case 16: return cvvoidptr(f(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],
                              a[9],a[10],a[11],a[12],a[13],a[14],a[15]));
  default: xlfail("too many arguments"); return NIL;
  }
}

#ifdef _Windows
typedef void * __stdcall (*stdfun0)(void);
typedef void * __stdcall (*stdfun1)(void *);
typedef void * __stdcall (*stdfun2)(void *, void *);
typedef void * __stdcall (*stdfun3)(void *, void *, void *);
typedef void * __stdcall (*stdfun4)(void *, void *, void *, void *);
typedef void * __stdcall (*stdfun5)(void *, void *, void *, void *, \
 void *);
typedef void * __stdcall (*stdfun6)(void *, void *, void *, void *, \
 void *, void *);
typedef void * __stdcall (*stdfun7)(void *, void *, void *, void *, \
 void *, void *, void *);
typedef void * __stdcall (*stdfun8)(void *, void *, void *, void *, \
 void *, void *, void *, void *);
typedef void * __stdcall (*stdfun9)(void *, void *, void *, void *, \
 void *, void *, void *, void *, void *);
typedef void * __stdcall (*stdfun10)(void *, void *, void *, void *, \
 void *, void *, void *, void *, void *, void *);
typedef void * __stdcall (*stdfun11)(void *, void *, void *, void *, \
 void *, void *, void *, void *, void *, void *, void *);
typedef void * __stdcall (*stdfun12)(void *, void *, void *, void *, \
 void *, void *, void *, void *, void *, void *, void *, void *);
typedef void * __stdcall (*stdfun13)(void *, void *, void *, void *, \
 void *, void *, void *, void *, void *, void *, void *, void *, \
 void *);
typedef void * __stdcall (*stdfun14)(void *, void *, void *, void *, \
 void *, void *, void *, void *, void *, void *, void *, void *, \
 void *, void *);
typedef void * __stdcall (*stdfun15)(void *, void *, void *, void *, \
 void *, void *, void *, void *, void *, void *, void *, void *, \
 void *, void *, void *);
typedef void * __stdcall (*stdfun16)(void *, void *, void *, void *, \
 void *, void *, void *, void *, void *, void *, void *, void *, \
 void *, void *, void *, void *);
                               
LVAL xshlibstdcalladdr()
{
  void *f = getnpaddr(xlganatptr());
  void *a[MAX_CALLADDR_ARGS];
  int n, i;

  if (xlargc > MAX_CALLADDR_ARGS)
    xltoomany();

  for (n = xlargc, i = 0; i < n; i++) {
    LVAL arg = xlgetarg();
    if (fixp(arg))
      a[i] = (void *) getfixnum(arg);
    else if (natptrp(arg))
      a[i] = getnpaddr(arg);
    else
      xlbadtype(arg);
  }
  
  switch (n) {
  case 0: return cvvoidptr(((stdfun0) f)());
  case 1: return cvvoidptr(((stdfun1) f)(a[0]));
  case 2: return cvvoidptr(((stdfun2) f)(a[0],a[1]));
  case 3: return cvvoidptr(((stdfun3) f)(a[0],a[1],a[2]));
  case 4: return cvvoidptr(((stdfun4) f)(a[0],a[1],a[2],a[3]));
  case 5: return cvvoidptr(((stdfun5) f)(a[0],a[1],a[2],a[3],
                                         a[4]));
  case 6: return cvvoidptr(((stdfun6) f)(a[0],a[1],a[2],a[3],
                                         a[4], a[5]));
  case 7: return cvvoidptr(((stdfun7) f)(a[0],a[1],a[2],a[3],
                                         a[4],a[5],a[6]));
  case 8: return cvvoidptr(((stdfun8) f)(a[0],a[1],a[2],a[3],
                                         a[4],a[5],a[6],a[7]));
  case 9: return cvvoidptr(((stdfun9) f)(a[0],a[1],a[2],a[3],
                                         a[4],a[5],a[6],a[7],
                                         a[8]));
  case 10: return cvvoidptr(((stdfun10) f)(a[0],a[1],a[2],a[3],
                                           a[4],a[5],a[6],a[7],
                                           a[8],a[9]));
  case 11: return cvvoidptr(((stdfun11) f)(a[0],a[1],a[2],a[3],
                                           a[4],a[5],a[6],a[7],
                                           a[8],a[9],a[10]));
  case 12: return cvvoidptr(((stdfun12) f)(a[0],a[1],a[2],a[3],
                                           a[4],a[5],a[6],a[7],
                                           a[8],a[9],a[10],a[11]));
  case 13: return cvvoidptr(((stdfun13) f)(a[0],a[1],a[2],a[3],
                                           a[4],a[5],a[6],a[7],
                                           a[8],a[9],a[10],a[11],
                                           a[12]));
  case 14: return cvvoidptr(((stdfun14) f)(a[0],a[1],a[2],a[3],
                                           a[4],a[5],a[6],a[7],
                                           a[8],a[9],a[10],a[11],
                                           a[12],a[13]));
  case 15: return cvvoidptr(((stdfun15) f)(a[0],a[1],a[2],a[3],
                                           a[4],a[5],a[6],a[7],
                                           a[8],a[9],a[10],a[11],
                                           a[12],a[13],a[14]));
  case 16: return cvvoidptr(((stdfun16) f)(a[0],a[1],a[2],a[3],
                                           a[4],a[5],a[6],a[7],
                                           a[8],a[9],a[10],a[11],
                                           a[12],a[13],a[14],a[15]));
  default: xlfail("too many arguments"); return NIL;
  }
}
#endif

/* ARRAY-DATA-ADDRESS array */
LVAL xarraydata_addr()
{
  LVAL x = xlgetarg();
  xllastarg();

  switch (ntype(x)) {
  case DARRAY: x = getdarraydata(x); /* and drop through */
  case VECTOR:
  case STRING:
  case TVEC: return newnatptr(gettvecdata(x), x);
  default: return xlbadtype(x);
  }
}

/* MAKE-SUBR addr &optional mulvalp */
LVAL xmakesubr()
{
  LVAL val;
  LVAL (*fun)(void) = (LVAL (*)(void)) getnpaddr(xlganatptr());
  int mv = moreargs() ? (null(xlgetarg()) ? FALSE : TRUE) : FALSE;
  xllastarg();
  val = cvsubr(fun, SUBR, 0);
  setmulvalp(val, mv);
  return val;
}

LOCAL LVAL errsubr() { xlfail("SUBR not available"); return NIL; }

/* CLEAR-SUBR subr */
LVAL xclearsubr()
{
  LVAL x = xlgasubr();
  xllastarg();
  setsubr(x, errsubr);
  setoffset(x, 0);
  setmulvalp(x, FALSE);
  return NIL;
}

#define MAKEVERSION(major,minor) ((1L<<16) * major + minor)
#define XLSHLIB_SYSVERSION {MAKEVERSION(0,1),MAKEVERSION(0,0)}
#define XLSHLIB_VERSION_INFO(maj_cur,min_cur,maj_old,min_old) \
  XLSHLIB_SYSVERSION, \
  {MAKEVERSION(maj_cur,min_cur),MAKEVERSION(maj_old,min_old)}

struct version_info { long current, oldest; };

typedef struct { char *name; FIXTYPE val; } FIXCONSTDEF;
typedef struct { char *name; FLOTYPE val; } FLOCONSTDEF;
typedef struct { char *name; char *val; } STRCONSTDEF;
typedef struct { char *name; unsigned long val; } ULONGCONSTDEF;

typedef struct {
  struct version_info sysversion;
  struct version_info modversion;
  FUNDEF *funs;
  FIXCONSTDEF *fixconsts;
  FLOCONSTDEF *floconsts;
  STRCONSTDEF *strconsts;
  ULONGCONSTDEF *ulongconsts;
} xlshlib_modinfo_t;

static struct version_info defsysversion = XLSHLIB_SYSVERSION;

static int check_version(struct version_info *req, struct version_info *imp)
{
  if (req->current == imp->current)
    return TRUE;
  else if (req->current > imp->current)
    return imp->current >= req->oldest ? TRUE : FALSE;
  else
    return req->current >= imp->oldest ? TRUE : FALSE;
}

/* SHLIB-INIT funtab &optional (version -1) (oldest version) */
LVAL xshlibinit()
{
  LVAL subr, val, sym;
  xlshlib_modinfo_t *info = getnpaddr(xlganatptr());
  FUNDEF *p = info->funs;
  FIXCONSTDEF *pfix = info->fixconsts;
  FLOCONSTDEF *pflo = info->floconsts;
  STRCONSTDEF *pstr = info->strconsts;
  struct version_info defversion;

  defversion.current = moreargs()?getfixnum(xlgafixnum()):-1;
  defversion.oldest = moreargs()?getfixnum(xlgafixnum()):defversion.current;
  xllastarg();

  if (! check_version(&defsysversion, &(info->sysversion)))
    xlfail("shared library not compatible with current system");
  if (defversion.current >= 0 &&
      ! check_version(&defversion, &(info->modversion)))
    xlfail("module not compatible with requested version");

  xlsave1(val);
  val = NIL;
  if (p != NULL)
    for (val = NIL; (p->fd_subr) != (LVAL(*)(void)) NULL; p++) {
      subr = cvsubr(p->fd_subr, p->fd_type & TYPEFIELD, 0);
      setmulvalp(subr, (p->fd_type & (TYPEFIELD + 1)) ? TRUE : FALSE);
      val = cons(subr, val);
      if (p->fd_name != NULL) {
        sym = xlenter(p->fd_name);
        setfunction(sym, subr);
      }
    }
  if (pfix != NULL)
    for (; pfix->name != NULL; pfix++) {
      sym = xlenter(pfix->name);
      defconstant(sym, cvfixnum(pfix->val));
    }
  if (pflo != NULL)
    for (; pflo->name != NULL; pflo++) {
      sym = xlenter(pflo->name);
      defconstant(sym, cvflonum(pflo->val));
    }
  if (pstr != NULL)
    for (; pstr->name != NULL; pstr++) {
      sym = xlenter(pstr->name);
      defconstant(sym, cvstring(pstr->val));
    }
  if (info->sysversion.current >= MAKEVERSION(0,1)) {
    ULONGCONSTDEF *pulong = info->ulongconsts;
    if (pulong != NULL)
      for (; pulong->name != NULL; pulong++) {
        sym = xlenter(pulong->name);
        defconstant(sym, ulong2lisp(pulong->val));
      }
  }
  xlpop();
  return xlnreverse(val);
}

/* SHLIB-INFO funtab */
LVAL xshlibinfo()
{
  LVAL val;
  xlshlib_modinfo_t *info = getnpaddr(xlganatptr());
  FUNDEF *p = info->funs;
  FIXCONSTDEF *pfix = info->fixconsts;
  FLOCONSTDEF *pflo = info->floconsts;
  STRCONSTDEF *pstr = info->strconsts;
  xllastarg();

  if (! check_version(&defsysversion, &(info->sysversion)))
    xlfail("shared library not compatible with current system");

  xlsave1(val);
  val = cons(cvfixnum((FIXTYPE) info->modversion.current), NIL);
  val = cons(cvfixnum((FIXTYPE) info->modversion.oldest), val);
  val = cons(NIL, val);
  if (p != NULL) {
    for (; (p->fd_subr) != (LVAL(*)(void)) NULL; p++)
      rplaca(val, cons(cvstring(p->fd_name), car(val)));
    rplaca(val, xlnreverse(car(val)));
  }
  val = cons(NIL, val);
  if (pfix != NULL)
    for (; pfix->name != NULL; pfix++)
      rplaca(val, cons(cvstring(pfix->name), car(val)));
  if (pflo != NULL)
    for (; pflo->name != NULL; pflo++)
      rplaca(val, cons(cvstring(pflo->name), car(val)));
  if (pstr != NULL)
    for (; pstr->name != NULL; pstr++)
      rplaca(val, cons(cvstring(pstr->name), car(val)));
  if (info->sysversion.current >= MAKEVERSION(0,1)) {
    ULONGCONSTDEF *pulong = info->ulongconsts;
    for (; pulong->name != NULL; pulong++)
      rplaca(val, cons(cvstring(pulong->name), car(val)));
  }
  rplaca(val, xlnreverse(car(val)));
  xlpop();
  return xlnreverse(val);
}

#endif /* SHAREDLIBS */
