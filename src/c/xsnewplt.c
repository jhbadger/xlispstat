/* xsnewplots - XLISP interface to IVIEW dynamic graphics package.     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL s_true, sk_new, s_histogram_proto, sk_add_points, sk_add_lines,
  sk_draw, s_scatterplot_proto, sk_adjust_to_data, s_spin_proto,
  s_scatmat_proto, s_name_list_proto, sk_point_labels, s_fixed, s_variable,
  sk_resize, sk_redraw, s_scale_type, sk_show, sk_show_window;

static VOID set_scale_shift P4C(IVIEW_WINDOW, w, int, var, double, scale, double, shift)
{
  double old_scale, old_shift;

  old_scale = IViewScale(w, var);
  old_shift = IViewShift(w, var);
  if (scale != 0.0 && old_scale != 0.0) {
    scale = scale / old_scale;
    shift = shift - scale * old_shift;
    IViewApplyScaleShift(w, var, scale, shift);
  }
}

VOID StGrObAdjustToData P2C(LVAL, object, int, draw)
{
  IVIEW_WINDOW w;
  double low, high, range, center;
  int i, vars;
  LVAL scale_type;
  
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  if (! IVIEW_WINDOW_NULL(w)) {
    scale_type = slot_value(object, s_scale_type);
    vars = IViewNumVariables(w);
    high = 1.0; low = -high;
    if (scale_type == s_variable) {
      high = sqrt((double) vars); low = - high;
      for (i = 0; i < vars; i++) {
	IViewScaleToRange(w, i, -1.0, 1.0);
	IViewSetScaledRange(w, i, low, high);
      }
    }
    else if (scale_type == s_fixed) {
      if (vars > 0) {
	IViewGetVisibleRange(w, 0, &low, &high);
	set_scale_shift(w, 0, 1.0, -(high + low) / 2.0);
	range = high - low;
	for (i = 1; i < vars; i++) {
	  IViewGetVisibleRange(w, i, &low, &high);
	  set_scale_shift(w, i, 1.0, -(high + low) / 2.0);
	  if (high - low > range) range = high - low;
	}
	range = sqrt((double) vars) * range / 2;
	for (i = 0; i < vars; i++) {
	  center = -IViewShift(w, i);
	  IViewSetRange(w, i, center - range, center + range);
	}
      }
    }
    else {
      for (i = 0; i < vars; i++) {
	IViewApplyScaleShift(w, i, 1.0, 0.0);
	IViewGetVisibleRange(w, i, &low, &high);
	IViewSetRange(w, i, low, high);
      }
    }
    if (draw) {
      send_message(object, sk_resize);
      send_message(object, sk_redraw);
    }
  }
}

LVAL iview_adjust_to_data(V)
{
  LVAL object;
  LVAL arg;
  int draw;

  object = xlgaobject();
  if (! xlgetkeyarg(sk_draw, &arg)) arg = s_true;
  draw = (arg != NIL) ? TRUE : FALSE;
  StGrObAdjustToData(object, draw);
  return(NIL);
}

static LVAL make_iview_object P3C(int, which, int, vars, LVAL, rest)
{
  LVAL proto = NIL, object, args;
  
  switch (which) {
  case 'H': proto = getvalue(s_histogram_proto); break;
  case 'P': 
  case 'L': proto = getvalue(s_scatterplot_proto); break;
  case 'R': proto = getvalue(s_spin_proto); break;
  case 'S': proto = getvalue(s_scatmat_proto); break;
  case 'N': proto = getvalue(s_name_list_proto); break;
  default:  xlfail("unknown iview proto");
  }
  
  xlsave1(args);
  args = cons(NIL, rest);
  args = cons(sk_show, args);
  /* cons protects its arguments, so the new fixnum should be safe */
  args = cons(cvfixnum((FIXTYPE) vars), args);
  object = apply_send(proto, sk_new, args);
  xlpop();
  return(object);
}

static VOID get_data P5C(int, which, LVAL *, data, int *, vars, LVAL *, rest, int *, show)
{
  LVAL x, y;
  int n;
  
  if (data == NULL || vars == NULL) return;
  
  switch (which) {
  case 'H':
    *data = xlgetarg();
    *vars = (consp(*data) && seqp(car(*data))) ? seqlen(*data) : 1;
	*show = xsboolkey(sk_show, TRUE);
    *rest = makearglist(xlargc, xlargv);
    break;
  case 'P':
  case 'L':
    x = xlgetarg();
    if (consp(x) && seqp(car(x))) *data = x;
    else {
      y = xlgetarg();
      *data = list2(x, y);
    }
    *vars = (consp(*data) && seqp(car(*data))) ? seqlen(*data) : 1;
	*show = xsboolkey(sk_show, TRUE);
    *rest = makearglist(xlargc, xlargv);
    break;
  case 'R':
  case 'S':
    *data = xlgalist();
    *vars = llength(*data);
	*show = xsboolkey(sk_show, TRUE);
    *rest = makearglist(xlargc, xlargv);
    break;
  case 'N':
    *vars = 0;
    *data = xlgetarg();
	*show = xsboolkey(sk_show, TRUE);
    *rest = makearglist(xlargc, xlargv);
    if (seqp(*data)) {
      n = seqlen(*data);
      *rest = cons(*data, *rest);
      *rest = cons(sk_point_labels, *rest);
      *data = cvfixnum((FIXTYPE) n);
    }
    break;
  default:  xlfail("unknown iview proto");
  }    
}

static VOID check_data P2C(int, which, LVAL, data)
{
  switch (which) {
  case 'H': break;
  case 'P':
  case 'L':
  case 'R':
  case 'S': 
    if (! consp(data)) xlerror("not a list of sequences", data);
    for (; consp(data); data = cdr(data)) 
      if (! seqp(car(data))) xlerror("not a sequence", car(data));
    break;
  case 'N': break;
  default:  xlfail("unknown iview proto");
  }
}

static VOID add_data P4C(int, which, LVAL, object, LVAL, data, LVAL, rest)
{
  LVAL args, message = NIL;
  
  xlsave1(args);
  args = cons(NIL, rest);
  args = cons(sk_draw, args);
  args = cons(data, args);

  switch (which) {
  case 'H':
  case 'P': 
  case 'R': 
  case 'S':
  case 'N': message = sk_add_points; break;
  case 'L': message = sk_add_lines;  break;
  default:  xlfail("unknown iview proto");
  }
  
  apply_send(object, message, args);
  xlpop();
}
  
static VOID adjust_plot_to_data P2C(LVAL, object, LVAL, rest)
{
  LVAL args;
  
  xlsave1(args);
  args = cons(NIL, rest);
  args = cons(sk_draw, args);
  apply_send(object, sk_adjust_to_data, args);
  xlpop();
}

static LVAL newplot P1C(int, which)
{
  int vars, show;
  LVAL object, data, rest, args;
  
  if (! StHasWindows()) {
#ifdef UNIX
    extern LVAL gnupointplot _((void));
    extern LVAL gnulineplot _((void));
    switch (which) {
    case 'P': return(gnupointplot());
    case 'L': return(gnulineplot());
    }
#endif /* UNIX */
    xlfail("not available without windows");
  }

  xlstkcheck(4);
  xlsave(object);
  xlsave(data);
  xlsave(args);
  xlsave(rest);
  
  get_data(which, &data, &vars, &rest, &show);
  check_data(which, data);
  object = make_iview_object(which, vars, rest);
  add_data(which, object, data, rest);
  adjust_plot_to_data(object, rest);
  
  xlpopn(4);
  
  if (show) send_message(object, sk_show_window);
  
  return(object);
}

LVAL xshistogram(V)          { return(newplot('H')); }
LVAL xsplot_points(V)        { return(newplot('P')); }
LVAL xsplot_lines(V)         { return(newplot('L')); }
LVAL xsspin_plot(V)          { return(newplot('R')); }
LVAL xsscatterplot_matrix(V) { return(newplot('S')); }
LVAL xsnamelist(V)           { return(newplot('N')); }
