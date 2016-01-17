/* xlsx.h - Include file for external Macintosh routines.              */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/* Calling conventions are based on the conventions given in the New S */
/* book.                                                               */
typedef struct {
  int argc;
  char **argv;
} XLSXblock;

#define XLSXargc(p)    ((p)->argc)
#define XLSXargv(p, i) ((p)->argv[(i)])
