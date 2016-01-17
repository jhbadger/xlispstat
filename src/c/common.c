/* common - Additional Common Lisp functions not yet included in       */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

/* forward declarations */
LOCAL int is_sub_str P2H(char *, char *);
LOCAL int strck P3H(char *, char *, int);


/****************************************************************************/
/****************************************************************************/
/**                                                                        **/
/**                         Common Lisp Functions                          **/
/**                                                                        **/
/****************************************************************************/
/****************************************************************************/

/****************************************************************************/
/**                       APROPOS and APROPOS-LIST                         **/
/****************************************************************************/

/* check if s1 is a substring of s2; case insensitive */
LOCAL int is_sub_str P2C(char *, s1, char *, s2)
{
  int n, m, i;

  m = strlen(s1);
  n = strlen(s2) - m;
  
  for (i = 0; i <= n; i++)
    if (strck(s1, &s2[i], m)) return (0);
  return(1);
}

/* check if s1 and s2 agree up to character m; case insensitive */
LOCAL int strck P3C(char *, s1, char *, s2, int, m)
{
  char ch1, ch2;

  while (m-- > 0) {
    ch1 = ISUPPER(*s1) ? TOLOWER(*s1) : *s1;
    ch2 = ISUPPER(*s2) ? TOLOWER(*s2) : *s2;
    if (ch1 != ch2) return(0);
    s1++;
    s2++;
  }
  return(1);
}

LVAL xsstringsearch(V)
{
  char *s1, *s2;

  s1 = getstring(xlgastrorsym());
  s2 = getstring(xlgastrorsym());
  xllastarg();
  return((is_sub_str(s1, s2) == 0) ? s_true : NIL);
}

LVAL xsrcomplex(V)   { return (recursive_subr_map_elements(xcomplex, xsrcomplex));     }
LVAL xsrconjugate(V) { return (recursive_subr_map_elements(xconjugate, xsrconjugate)); }
LVAL xsrrealpart(V)  { return (recursive_subr_map_elements(xrealpart, xsrrealpart));   }
LVAL xsrimagpart(V)  { return (recursive_subr_map_elements(ximagpart, xsrimagpart));   }

/***********************************************************************/
/**                                                                   **/
/**                  Time and Environment Functions                   **/
/**                                                                   **/
/***********************************************************************/

LVAL xtime(V)
{
  LVAL result;
  unsigned long tm, gctm;
  double dtm, gcdtm;
  
  tm = run_tick_count();
  gctm = gc_tick_count();
  result = xleval(xlgetarg());
  tm = run_tick_count() - tm;
  gctm = gc_tick_count() - gctm;
  dtm = tm;
  gcdtm = gctm;
  
  sprintf(buf, "The evaluation took %.2f seconds; ", dtm / ticks_per_second());
  stdputstr(buf);
  sprintf(buf, "%.2f seconds in gc.\n", gcdtm / ticks_per_second());
  stdputstr(buf);
  return(result);
}

LVAL xruntime(V)
{
  return(cvfixnum((FIXTYPE) run_tick_count()));
}

LVAL xrealtime(V)
{
  return(cvfixnum((FIXTYPE) real_tick_count()));
}

LVAL xgctime(V)
{
  return(cvfixnum((FIXTYPE) gc_tick_count()));
}

LVAL xsgetenv(V)
{
  xllastarg();
  return(list3(xlenv, xlfenv, xldenv));
}
