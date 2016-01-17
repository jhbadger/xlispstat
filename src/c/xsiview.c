/* xsiview - XLISP interface to IVIEW dynamic graphics package.        */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL s_true;
extern LVAL s_solid, sk_point_labels, sk_draw, sk_redraw,
  sk_redraw_background, sk_redraw_content, sk_redraw_overlays,
  sk_resize_overlays, sk_scale, sk_type, sk_color, sk_width, sk_symbol,
  sk_clear_content;


/**************************************************************************/
/**                                                                      **/
/**                    General IView Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

static int check_locations P2C(LVAL, data, int, vars)
{
  int i, n;
  
  if (fixp(data)) {
    if (getfixnum(data) <= 0) xlerror("too few points", data);
    else return(getfixnum(data));
  }
  
  if (! consp(data)/* || llength(data) != vars */)
    xlerror("bad variable list", data);
  
  if (! seqp(car(data))) xlerror("not a sequence", car(data));
  n = seqlen(car(data));
  
  for (i = 0; i < vars && consp(data); i++, data = cdr(data)) {
    if (! seqp(car(data))) xlerror("not a sequence", car(data));
    if (seqlen(car(data)) != n) xlfail("sequences of different lengths");
  }
  return(n);
}

static VOID set_locations P7C(IVIEW_WINDOW, w, LVAL, object, int, type, LVAL, data,
                              int, vars, int, oldn, int, n)
{
  LVAL seq, arg, val;
  int i, j;

  if (fixp(data)) return;
  if (! xlgetkeyarg(sk_scale, &arg)) arg = s_true;
  
  for (i = 0; i < vars && consp(data); i++, data = cdr(data)) {
    for (j = oldn, seq = car(data); j < n; j++) {
      val = getnextelement(&seq, j - oldn);
      switch (type) {
      case 'P':
        if (realp(val))
          IViewSetPointValue(w, i, j, makefloat(val));
        else {
          IViewSetPointValue(w, i, j, 0.0);
          IViewSetPointMask(w, j, TRUE);
        }
        break;
      case 'L':
        if (realp(val))
          IViewSetLineValue(w, i, j, makefloat(val));
        else {
          IViewSetLineValue(w, i, j, 0.0);
          IViewSetNextLine(w, j, -1);
          if (j > oldn) IViewSetNextLine(w, j - 1, -1);
        }
        break;
#ifdef USESTRINGS
      case 'S':
        IViewSetStringValue(w, i, j, realp(val) ? makefloat(val) : 0.0);
        break;
#endif /* USESTRINGS */
      }
    }
  }
}

static VOID check_strings P2C(int, n, LVAL, strings)
{
  int i;
  LVAL element;
  
  if (! seqp(strings)) xlerror("not a sequence", strings);
  if (n != seqlen(strings)) xlerror("wrong sequence length", strings);
  for (i = 0; i < n; i++) {
    element = getnextelement(&strings, i);
    if (! stringp(element)) xlerror("not a string", element);
  }
}

static LVAL clear_data P1C(int, which)
{
  IVIEW_WINDOW w;
  LVAL object;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) get_iview_address(object);
  
  switch(which) {
  case 'P': IViewClearPoints(w);  break;
  case 'L': IViewClearLines(w);   break;
#ifdef USESTRINGS
  case 'S': IViewClearStrings(w); break;
#endif /* USESTRINGS */
  }
  check_redraw(object, TRUE, TRUE);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                      IView Point Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

VOID internal_iview_add_points P3C(IVIEW_WINDOW, w, LVAL, object, LVAL, data)
{
  LVAL labels, arg;
  int i, n, oldn, vars, sym, hsym;
  char *str;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);

  if (! xlgetkeyarg(sk_point_labels, &labels)) labels = NIL;
  
  vars = IViewNumVariables(w);
  oldn = IViewNumPoints(w);
  n = check_locations(data, vars);
  
  IViewAddPoints(w, n);
  n = IViewNumPoints(w);
  set_locations(w, object, 'P', data, vars, oldn, n);
  
  if (labels != NIL) {
    check_strings(n - oldn, labels);
    for (i = oldn; i < n; i++) {
      str = (char *) getstring(getnextelement(&labels, i - oldn));
      IViewSetPointLabel(w, i, str);
    }
  }
  if (xlgetkeyarg(sk_color, &arg) && !null(arg)) {
    StGWSetUseColor(gwinfo, TRUE);
    for (i = oldn; i < n; i++)
      IViewSetPointColor(w, i, decode_lisp_color(arg));
  }
  if (xlgetkeyarg(sk_symbol, &arg)) {
    decode_point_symbol(arg, &sym, &hsym);
    for (i = oldn; i < n; i++)
      IViewSetPointSymbol(w, i, sym, hsym);
  }
}

LVAL iview_add_points(V)
{
  IVIEW_WINDOW w;
  LVAL object, data;
  int old_n, n;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) get_iview_address(object);
  data = xlgetarg();
  
  old_n = IViewNumPoints(w);
  internal_iview_add_points(w, object, data);
  n = IViewNumPoints(w);
  check_add_to_screen(object, 'P', old_n, n, FALSE);
  return(NIL);
}

LVAL iview_clear_points(V) { return(clear_data('P')); }

/**************************************************************************/
/**                                                                      **/
/**                      IView Line Data Functions                       **/
/**                                                                      **/
/**************************************************************************/

VOID internal_iview_add_lines P3C(IVIEW_WINDOW, w, LVAL, object, LVAL, data)
{
  int i, n, oldn, vars, width;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  LVAL arg;
  
  vars = IViewNumVariables(w);
  oldn = IViewNumLines(w);
  n = check_locations(data, vars);
  
  IViewAddLines(w, n);
  n = IViewNumLines(w);
  set_locations(w, object, 'L', data, vars, oldn, n);

  if (xlgetkeyarg(sk_type, &arg) && arg != s_solid)
    for (i = oldn; i < n; i++) IViewSetLineType(w, i, 1);
  if (xlgetkeyarg(sk_color, &arg) && !null(arg)) {
    StGWSetUseColor(gwinfo, TRUE);
    for (i = oldn; i < n; i++)
      IViewSetLineColor(w, i, decode_lisp_color(arg));
  }
  if (xlgetkeyarg(sk_width, &arg) && fixp(arg)) {
    width = getfixnum(arg);
    for (i = oldn; i < n; i++)
      IViewSetLineWidth(w, i, width);
  }
}

LVAL iview_add_lines(V)
{
  IVIEW_WINDOW w;
  LVAL object, data;
  int n, oldn;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) get_iview_address(object);
  data = xlgetarg();

  oldn = IViewNumLines(w);
  internal_iview_add_lines(w, object, data);
  n = IViewNumLines(w);
  check_add_to_screen(object, 'L', oldn, n, FALSE);
  return(NIL);
}
  
LVAL iview_clear_lines(V) { return(clear_data('L')); }

#ifdef USESTRINGS
/**************************************************************************/
/**                                                                      **/
/**                     IView String Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

internal_iview_add_strings P3C(IVIEW_WINDOW, w, LVAL, object, LVAL, data)
{
  LVAL strings;
  int i, n, oldn, vars;
  char *str;
  
  strings = xlgetarg();
  
  vars = IViewNumVariables(w);
  oldn = IViewNumStrings(w);
  n = check_locations(data, vars);
  check_strings(n, strings);
  
  IViewAddStrings(w, n);
  n = IViewNumStrings(w);
  set_locations(w, object, 'S', data, vars, oldn, n);
  
  for (i = oldn; i < n; i++) {
    str = (char *) getstring(getnextelement(&strings, i - oldn));
    IViewSetStringString(w, i, str);
  }
}

LVAL iview_add_strings(V)
{
  IVIEW_WINDOW w;
  LVAL object, data;
  int n, oldn;
  
  object = xlgaobject();
  w = get_iview_address(object);
  data = xlgetarg();

  oldn = IViewNumStrings(w);
  internal_iview_add_strings(w, object, data);
  n = IViewNumStrings(w);
  check_add_to_screen(object, 'S', oldn, n, FALSE);
  return(NIL);
}

LVAL iview_clear_strings(V) { return(clear_data('S')); }
#endif /* USESTRINGS */

/**************************************************************************/
/**                                                                      **/
/**                     Standard Callback Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_std_resize(V)
{
  IVIEW_WINDOW w;

  w = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  IViewStdResize(w);
  return(NIL);
}

LVAL iview_std_redraw(V)
{
  IVIEW_WINDOW w;

  w = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  IViewStdRedraw(w);
  return(NIL);
}

LVAL iview_std_redraw_background(V)
{
  IVIEW_WINDOW w;

  w = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  IViewStdRedrawBackground(w);
  return(NIL);
}

LVAL iview_std_clear_content(V)
{
  IVIEW_WINDOW w;

  w = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  IViewStdClearContent(w);
  return(NIL);
}

LVAL iview_std_redraw_content(V)
{
  IVIEW_WINDOW w;

  w = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  IViewStdRedrawContent(w);
  return(NIL);
}

VOID IViewRedrawBackground P1C(IVIEW_WINDOW, w)
{
  send_message(IViewWindowGetObject(w), sk_redraw_background);
}

VOID IViewClearContent P1C(IVIEW_WINDOW, w)
{
  send_message(IViewWindowGetObject(w), sk_clear_content);
}

VOID IViewRedrawContent P1C(IVIEW_WINDOW, w)
{
  send_message(IViewWindowGetObject(w), sk_redraw_content);
}

VOID IViewRedrawOverlays P1C(IVIEW_WINDOW, w)
{
  send_message(IViewWindowGetObject(w), sk_redraw_overlays);
}

VOID IViewResizeOverlays P1C(IVIEW_WINDOW, w)
{
  send_message(IViewWindowGetObject(w), sk_resize_overlays);
}


/**************************************************************************/
/**                                                                      **/
/**                        Miscellaneous Functions                       **/
/**                                                                      **/
/**************************************************************************/

VOID check_add_to_screen P5C(LVAL, object, int, which, int, old_n, int, n, int, redraw)
{
  IVIEW_WINDOW w;
  StGWWinInfo *gwinfo;
  int x, y;
  LVAL adjust;
  
  w = (IVIEW_WINDOW) get_iview_address(object);
  gwinfo = StGWObWinInfo(object);
  if (! xlgetkeyarg(sk_draw, &adjust)) adjust = s_true;
  
  if (adjust != NIL) {
    if (redraw || old_n == 0) send_message(object, sk_redraw_content);
    else {
      StGrGetContentVariables(gwinfo, &x, &y);
      switch(which) {
      case 'P': IViewDrawDataPoints(w, x, y, old_n, n);   break;
      case 'L': IViewDrawDataLines(w, x, y, old_n, n);    break;
#ifdef USESTRINGS
      case 'S': IViewDrawDataStrings(w, x, y, old_n, n);  break;
#endif /* USESTRINGS */
      }
    }
  }
}

VOID check_redraw P3C(LVAL, object, int, deflt, int, content_only)
{
  LVAL arg, msg;
  int redraw;
  
  if (xlgetkeyarg(sk_draw, &arg)) redraw = (arg != NIL);
  else redraw = deflt;
  
  msg = (content_only) ? sk_redraw_content : sk_redraw;
  if (redraw) send_message(object, msg);
}

int draw_key_arg P1C(int, deflt)
{
  int value = deflt, n, i;
  
  for (n = xlargc - 1, i = 0; i < n; i++) {
    if (sk_draw == xlargv[i]) {
	  value = (xlargv[i+1] != NIL) ? TRUE : FALSE;
	}
  }
  return(value);
}
