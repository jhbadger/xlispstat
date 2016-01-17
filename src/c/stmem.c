/* stmem.c - memory allocation routines.                               */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

#ifndef ANSI
extern ALLOCTYPE *calloc(), *realloc();
#endif

/**************************************************************************/
/**                                                                      **/
/**             Error Message and Memory Allocation Functions            **/
/**                                                                      **/
/**************************************************************************/

ALLOCTYPE *StCalloc P2C(int, n, int, m)
{
  ALLOCTYPE *val;
  
  if ((val = calloc(n, m)) == NULL) {
    gc();
    if ((val = calloc(n, m)) == NULL)
      xlfail("allocation failed");
  }
  return(val);
}

VOID StFree P1C(ALLOCTYPE *, p)
{
  if (p != NULL) free(p);
}


/**************************************************************************/
/**                                                                      **/
/**                  Reallocatable Data Pointer Package                  **/
/**                                                                      **/
/**  (This is needed on the Mac because the realloc function does not    **/
/**          work and also to help reduce heap fragmentation.)           **/
/**                                                                      **/
/**************************************************************************/

#ifdef MACINTOSH
typedef ALLOCTYPE **StReallocData;
#else
typedef struct{
  int size;
  ALLOCTYPE *data;
} realloc_data, *StReallocData;
#endif /* MACINTOSH */

ALLOCTYPE *StRCalloc P2C(int, n, int, m)
{
#ifdef MACINTOSH
  Handle h;
  long size;
  char *p;
  
  size = ((long) n) * ((long) m);
  if ((h = NewHandle(size)) == NULL || MemError()) {
    gc();
    if ((h = NewHandle(size)) == NULL || MemError()) 
      xlfail("Allocation Failed");
  }
  for (p = (char *) *h; size > 0; *p++ = 0, size--);
  return ((ALLOCTYPE *) h);
#else
  StReallocData r;

  r = (StReallocData) StCalloc(sizeof(realloc_data), 1);
  r->size = n * m;
  r->data = StCalloc(r->size, 1);
  return((ALLOCTYPE *) r);
#endif /* MACINTOSH */
}

VOID StRFree P1C(ALLOCTYPE *, p)
{
  StReallocData d = (StReallocData) p;
#ifdef MACINTOSH
  if (d != NULL) DisposeHandle((Handle) d);
#else
  if (d != NULL) {
    StFree(d->data);
    StFree(d);
  }
#endif /* MACINTOSH */
}

ALLOCTYPE *StRRealloc P3C(ALLOCTYPE *, q, int, n, int, m)
{
  StReallocData d = (StReallocData) q;
#ifdef MACINTOSH
  long oldSize, newSize, i;
  char *p;
  
  oldSize = (d != NULL) ? GetHandleSize((Handle) d) : 0;
  newSize = (long) n * (long) m;
  
  if (d == NULL) d = (StReallocData) NewHandle(newSize);
  else SetHandleSize((Handle) d, newSize);
  if (d == NULL || MemError()) xlfail("Allocation Failed");
  
  for (p = (char *) *d, i = oldSize; i < newSize; p[i] = 0, i++);
  
  return ((ALLOCTYPE *) d);
#else
  int oldsize, size;
  
  if (d == NULL) {
    d = (StReallocData) StRCalloc(n, m);
    if (d == NULL) xlfail("Allocation Failed");
  }
  else {
    size = n * m;
    d->data = realloc(d->data, size);
    if (d->data == NULL) xlfail("Allocation Failed");
    oldsize = d->size;
    d->size = size;
    if (size > oldsize) {
      MEMSET((char *)d->data + oldsize, 0, size - oldsize);
    }
  }
  return((ALLOCTYPE *) d);
#endif /* MACINTOSH */
}

long StRSize P1C(ALLOCTYPE *, p)
{
  StReallocData d = (StReallocData) p;
#ifdef MACINTOSH
  return ((d != NULL) ? GetHandleSize((Handle) d) : 0L);
#else
  return((d != NULL) ? d->size : 0);
#endif /* MACINTOSH */
}

ALLOCTYPE *StRPtr P1C(ALLOCTYPE *, p)
{
  StReallocData d = (StReallocData) p;
#ifdef MACINTOSH
  return ((d != NULL) ? (ALLOCTYPE *) *d : NULL);
#else
  if (d != NULL && d->data == NULL) xlfail("bad relocatable data");
  return((d != NULL) ? d->data : NULL);
#endif /* MACINTOSH */
}

VOID StRLock P1C(ALLOCTYPE *, p)
{
#ifdef MACINTOSH
  HLock((Handle) p);
#endif /* MACINTOSH */
}

VOID StRUnlock P1C(ALLOCTYPE *, p)
{
#ifdef MACINTOSH
  HUnlock((Handle) p);
#endif /* MACINTOSH */
}

