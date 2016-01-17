/* macxsgraph - Macintosh lisp low level graphics functions            */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

#define MULTIPLIER 65535

/* external variables */
extern LVAL k_initial;

static RGBColor ListToRGB(LVAL x)
{
  RGBColor color;

  if (! consp(x) || llength(x) != 3) xlerror("not a color list", x);
  color.red   = MULTIPLIER * makefloat(car(x)); x = cdr(x);
  color.green = MULTIPLIER * makefloat(car(x)); x = cdr(x);
  color.blue  = MULTIPLIER * makefloat(car(x));
  return(color);
}
  
static LVAL RGBToList(RGBColor color)
{
  LVAL result, rp;
  
  xlsave1(result);
  result = rp = mklist(3, NIL);
  rplaca(rp, cvflonum((FLOTYPE) (((double) color.red)   / MULTIPLIER)));
  rp = cdr(rp);
  rplaca(rp, cvflonum((FLOTYPE) (((double) color.green) / MULTIPLIER)));
  rp = cdr(rp);
  rplaca(rp, cvflonum((FLOTYPE) (((double) color.blue)  / MULTIPLIER)));
  xlpop();
  return(result);
}

LVAL xspick_color(void)
{
  Point where;
  char *prompt;
  RGBColor in_color, out_color;
  int ok;
  LVAL arg;
  Str255 pbuf;
  
  if (! StScreenHasColor()) return(NIL);
  
  in_color.red = 0; 
  in_color.green = 0;
  in_color.blue = 0;
  if (moreargs()) {
    prompt = (char *) getstring(xlgastring());
    if (xlgetkeyarg(k_initial, &arg)) in_color = ListToRGB(arg);
  }
  else prompt = "Pick a color";
  
  where.h = 0; where.v = 0;
  CintoPstring(prompt, pbuf, sizeof pbuf, FALSE);
  NotifyIfInBackground();
  ok = GetColor(where, pbuf, &in_color, &out_color);
  
  return((ok) ? RGBToList(out_color) : NIL);
}
