#include "xlisp.h"
#include "xlmodule.h"

VOID init___dummy _((int *pn, VOID (***pf) _((int))));

MODULE xlmodules[1];
int xlnummodules = 1;
int xlcurrentmodule = 0;

VOID init_modules(V)
{
  xlmodules[0].name = "dummy";
  init___dummy(&xlmodules[0].numfunctions, &xlmodules[0].functions);
}

