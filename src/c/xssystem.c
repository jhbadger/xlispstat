/* xssystem - calling UNIX utilities                                   */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"

extern LVAL s_true, s_stdout;

LVAL xssystem()
{
  char *cmd;
  int status;
  LVAL stream = NIL;
  FILE *p;
  int ch;

  cmd = (char *) getstring(xlgastring());
  if (moreargs()) {
    stream = xlgetarg();
    if (stream == s_true)
      stream = getvalue(s_stdout);
    else if (!streamp(stream) && !ustreamp(stream))
      xlbadtype(stream);
  }
  
  if (stream == NIL) {
    status = system(cmd);
    if (status == 127) xlfail("shell could not execute command");
  }
  else {
    if ((p = popen(cmd, "r")) == NULL)
      xlfail("could not execute command");
    while ((ch = getc(p)) != EOF) xlputc(stream, ch);
    status = pclose(p);
  }
  return(cvfixnum((FIXTYPE) status));
}



