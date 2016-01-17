/* xsscatterplot - XLISP interface to IVIEW dynamic graphics package.  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL sk_draw, sk_resize, sk_redraw, s_true;
extern LVAL s_scale_type;

static VOID adjust_variable P4C(IVIEW_WINDOW, w, int, var, int *, ticks, int *, labeled) 
{
  double low, high;
  char *label;
  
  IViewGetRange(w, var, &low, &high);
  GetNiceRange(&low, &high, ticks);
  IViewSetRange(w, var, low, high);
  label = IViewVariableLabel(w, var);
  *labeled = (label != NULL && strlen(label) != 0) ? TRUE : FALSE;
}

LVAL iview_plot2d_adjust_to_data(V)
{
  LVAL object;
  IVIEW_WINDOW w;
  StGWWinInfo *gwinfo;
  int x, y, ticks, labeled, scaled;
  LVAL arg;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  gwinfo = StGWObWinInfo(object);
  
  if (! IVIEW_WINDOW_NULL(w)) {
    scaled = (slot_value(object, s_scale_type) != NIL) ? TRUE : FALSE;
    StGrGetContentVariables(gwinfo, &x, &y);
    StGrObAdjustToData(object, FALSE);
    ticks = 4;
    if (! scaled) adjust_variable(w, x, &ticks, &labeled);
    IViewSetXaxis(w, ! scaled, labeled, ticks);
    ticks = 4;
    if (! scaled) adjust_variable(w, y, &ticks, &labeled);
    IViewSetYaxis(w, ! scaled, labeled, ticks);

    if (! xlgetkeyarg(sk_draw, &arg)) arg = s_true;
    if (arg != NIL) send_message(object, sk_resize);
    if (arg != NIL) send_message(object, sk_redraw);  
  }
  return(NIL);
}

static LVAL plot2d_add_data P1C(int, which)
{
  IVIEW_WINDOW w;
  int old_n = 0, n = 0;
  LVAL x, y, data, object;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  
  if (IVIEW_WINDOW_NULL(w)) return(NIL);
    
  xlsave1(data);
  x = xlgetarg();
  if (fixp(x) || (consp(x) && seqp(car(x)))) data = x;
  else {
    y = xlgetarg();
    data = list2(x, y);
  }
  switch (which) {
  case 'P':
    old_n = IViewNumPoints(w);
    internal_iview_add_points(w, object, data);
    n = IViewNumPoints(w);
    break;
  case 'L':
    old_n = IViewNumLines(w);
    internal_iview_add_lines(w, object, data);
    n = IViewNumLines(w);
    break;
#ifdef USESTRINGS
  case 'S':
    old_n = IViewNumStrings(w);
    internal_iview_add_strings(w, object, data);
    n = IViewNumStrings(w);
    break;
#endif /* USESTRINGS */
  } 
  xlpop();
  
  check_add_to_screen(object, which, old_n, n, FALSE);
  
  return(NIL);
}

LVAL iview_plot2d_add_points(V)   { return(plot2d_add_data('P')); }
LVAL iview_plot2d_add_lines(V)    { return(plot2d_add_data('L')); }
#ifdef USESTRINGS
LVAL iview_plot2d_add_strings(V)  { return(plot2d_add_data('S')); }
#endif /* USESTRINGS */
