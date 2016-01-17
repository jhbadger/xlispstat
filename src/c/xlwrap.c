/* xlwrap - Lisp wrappers for C code.                               */
/* XLISP-STAT 2.1 Copyright (c) 1990-1997, by Luke Tierney          */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz */
/* You may give out copies of this software; for conditions see the */
/* file COPYING included with this distribution.                    */

#include "xlisp.h"
#ifdef SHAREDLIBS
#include "xlwrap.h"

static LVAL s_types_registry = NULL;

LVAL xlw_lookup_type(char *tname)
{
  LVAL next, types;
  if (s_types_registry == NULL) {
    s_types_registry = xlenter("SYSTEM::*C-TYPES-REGISTRY*");
    setvalue(s_types_registry, NIL);
  }
  types = getvalue(s_types_registry);
  for (next = types; consp(next); next = cdr(next))
    if (stringp(car(next)) && strcmp(getstring(car(next)), tname) == 0)
      return car(next);
  types = cons(cvstring(tname), types);
  setvalue(s_types_registry, types);
  return car(types);
}

DECLARE_CPTR_TYPE(void)

LVAL xlgacptr(LVAL type, int null_ok)
{
  LVAL p = xlgetarg();
  if ((null(p) && null_ok) ||
      (cptr_type_p(p,type) && getcpaddr(p) != NULL))
    return p;
  else
    return xlbadtype(p);
}

void *xlgacptraddr(LVAL type, int null_ok)
{
  LVAL p = xlgetarg();
  if (null(p) && null_ok)
    return NULL;
  else if (cptr_type_p(p,type) && getcpaddr(p) != NULL)
    return getcpaddr(p);
  else
    return xlbadtype(p);
}

LVAL cvcptr(LVAL type, void *v, LVAL data)
{
  if (v == NULL)
    return NIL;
  else {
    LVAL ptr, val;
    xlprot1(data);
    xlsave1(ptr);
    ptr = newnatptr(v, data);
    val = newcptr(type,ptr);
    xlpopn(2);
    return val;
  }
}

LVAL xlw_make_cptr(LVAL type, size_t elsize)
{
  LVAL data, count;
  FIXTYPE n = 1;
  if (moreargs()) {
    count = xlgafixnum();
    n = getfixnum(count);
    if (n <= 0)
      xlbadtype(count);
  }
  xllastarg();
  data = mktvec(n * elsize, s_c_char);
  return cvcptr(type, gettvecdata(data), data);
}

LVAL xlw_cast_cptr(LVAL type)
{
  LVAL p = xlgetarg();
  xllastarg();
  if (null(p))
    return NIL;
  else if (cptrp(p)) /* won't be a NULL pointer */
    return newcptr(type, getcpptr(p));
  else if (natptrp(p)) /* need to check for NULL */
    return getnpaddr(p) == NULL ? NIL : newcptr(type, p);
  else
    return xlbadtype(p);
}

LVAL xlw_offset_cptr(LVAL type, size_t elsize)
{
  LVAL p = xlgetarg();
  size_t off = getfixnum(xlgafixnum()) * elsize;
  xllastarg();
  if (! cptr_type_p(p, type))
    xlbadtype(p);
  return cvcptr(type, (char *) getcpaddr(p) + off, getcpprot(p));
}

LVAL xcptrprotect(V)
{
  LVAL ptr, val, nptr, pval;

  ptr = xlgetarg();
  val = xlgetarg();
  xllastarg();

  if (! cptrp(ptr)) xlbadtype(ptr);
  nptr = getcpptr(ptr);
  pval = getnpprot(nptr);
  pval = null(pval) ? val : cons(val, pval);
  setnpprot(nptr, pval);
  return NIL;
}
#endif /* SHAREDLIBS */
