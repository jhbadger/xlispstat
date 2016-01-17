/* xsgraphics - XLISP interface to IVIEW dynamic graphics package.     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL s_true, sk_draw, sk_resize, sk_redraw;

/* static global variables */
static int point_state;
static IVIEW_WINDOW wind;
static int scale_type;
static LVAL plot_object;

static LVAL base_is_state(V)
{
  int point, set = FALSE, on = 0;
  PointState state;
  
  /* get the arguments */
  point = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    on = (xlgetarg() != NIL) ? TRUE : FALSE;
  }
  
  /* set the new state if value was supplied */
  if (set) {
    if (on) {
      switch (point_state) {
      case 'S': IViewSetPointState(wind, point, pointSelected); break;
      case 'H': IViewSetPointState(wind, point, pointHilited);  break;
      case 'V': IViewSetPointState(wind, point, pointNormal);   break;
      case 'M': IViewSetPointMask(wind, point, TRUE);           break;
      }
    }
    else {
      switch (point_state) {
      case 'S': 
      case 'H': IViewSetPointState(wind, point, pointNormal);   break;
      case 'V':
        if (IViewPointState(wind, point) != pointInvisible)
          IViewSetPointState(wind, point, pointInvisible);
        break;
      case 'M': IViewSetPointMask(wind, point, FALSE);          break;
      }
    }
  }
  
  /* get the current state */
  state = IViewPointState(wind, point);
  switch (point_state) {
  case 'S': on = (state == pointSelected);      break;
  case 'H': on = (state == pointHilited);       break;
  case 'V': on = (state != pointInvisible);     break;
  }
    
  /* return the current state */
  return((on) ? s_true : NIL);
}

static LVAL is_state(V)
{
  return(recursive_subr_map_elements(base_is_state, is_state));
}

static LVAL internal_is_state P1C(int, type)
{
  LVAL object, result;
  int set = (xlargc > 2);
  
  object = xlgaobject();
  wind = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  if (IVIEW_WINDOW_NULL(wind)) return(NIL);
  if (xlargc > 1) IViewCheckLinks(wind);
  point_state = type;
  result = is_state();
  
  if (set) {
    if (type == 'M') IViewRedrawContent(wind);
    else IViewAdjustScreens(wind);
  }
  return(result);
}

LVAL iview_point_selected(V)  { return(internal_is_state('S')); }
LVAL iview_point_hilited(V)   { return(internal_is_state('H')); }
LVAL iview_point_showing(V)   { return(internal_is_state('V')); }

#ifdef MACINTOSH
LVAL iview_window_copy_to_clip(V)
{
  StGWWinInfo *gwinfo;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  xllastarg();
  if (gwinfo != NULL) StGWCopyToClip(gwinfo);
  return(NIL);
}
#endif /* MACINTOSH */

static LVAL base_visible_range(V)
{
  int var;
  double low, high;
  
  var = getfixnum(xlgafixnum());
  xllastarg();
  
  IViewGetVisibleRange(wind, var, &low, &high);
  return(double_list_2(low, high));
}

static LVAL visible_range(V)
{
  return(recursive_subr_map_elements(base_visible_range, visible_range));
}

LVAL iview_visible_range(V)
{
  LVAL object;
  
  object = xlgaobject();
  wind = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  if (! IVIEW_WINDOW_NULL(wind)) return(visible_range());
  else return(NIL);
}

static LVAL base_scale_to_range(V)
{
  int var;
  double low, high;
  
  var = getfixnum(xlgafixnum());
  low = makefloat(xlgetarg());
  high = makefloat(xlgetarg());
  
  IViewScaleToRange(wind, var, low, high);  
  return(NIL);
}

static LVAL scale_to_range(V)
{
  return(recursive_subr_map_elements(base_scale_to_range, scale_to_range));
}

LVAL iview_scale_to_range(V)
{
  LVAL arg;
  
  plot_object = xlgaobject();
  wind = (IVIEW_WINDOW) GETIVIEWADDRESS(plot_object);
  if (! IVIEW_WINDOW_NULL(wind)) {
    int oldargc = xlargc;
    LVAL *oldargv = xlargv;
    scale_to_range();
    xlargc = oldargc - 3;
    xlargv = oldargv + 3;
  }
  if (! xlgetkeyarg(sk_draw, &arg)) arg = s_true;
  if (arg != NIL) send_message(plot_object, sk_resize);
  if (arg != NIL) send_message(plot_object, sk_redraw);
  return(NIL);
}

static LVAL base_scale_shift(V)
{
  int var, set = FALSE;
  double scale, shift, old_scale, old_shift, new = 0, new_scale, new_shift;
  
  var = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    new = makefloat(xlgetarg());
  }
  
  old_scale = IViewScale(wind, var);
  old_shift = IViewShift(wind, var);
  if (set) {
    if (scale_type == 'A') {
      if (new <= 0.0) xlfail("nonpositive scale factor");
      new_scale = 1.0 / new;
      new_shift = (old_scale != 0.0) ? old_shift * (new_scale / old_scale)
                                     : 0.0;
    }
    else {
      new_scale = old_scale;
      new_shift = - new * old_scale;
    }
    if (old_scale == 0.0 || new_scale == 0.0) return(NIL);
    scale = new_scale / old_scale;
    shift = new_shift - scale * old_shift;
  
    IViewApplyScaleShift(wind, var, scale, shift);
  }
  if (scale_type == 'A') {
    new = IViewScale(wind, var);
    new = (new > 0.0) ? 1.0 / new : 1.0;
  }
  else new = (old_scale != 0.0) ? (-IViewShift(wind, var) / old_scale) : 0.0;
  
  return(cvflonum((FLOTYPE) new));
}

static LVAL scale_shift(V)
{
  return(recursive_subr_map_elements(base_scale_shift, scale_shift));
}

static LVAL iview_scale_shift P1C(int, action)
{
  LVAL arg, result, *oldargv = NULL;
  int set = (xlargc > 2);
  int oldargc = 0;
  
  plot_object = xlgaobject();
  wind = (IVIEW_WINDOW) GETIVIEWADDRESS(plot_object);
  scale_type = action;
  
  if (IVIEW_WINDOW_NULL(wind)) return(NIL);
  
  xlsave1(result);
  if (set) {
    oldargc = xlargc;
    oldargv = xlargv;
  }
  result = scale_shift();
  if (set) {
    xlargc = oldargc - 2;
    xlargv = oldargv + 2;
    if (! xlgetkeyarg(sk_draw, &arg)) arg = s_true;
    if (!null(arg)) {
      send_message(plot_object, sk_resize);
      send_message(plot_object, sk_redraw);
    }
  }
  xlpop();  
  return(result);
}

LVAL iview_scale(V)      { return(iview_scale_shift('A')); }
LVAL iview_shift(V)      { return(iview_scale_shift('B')); }

LVAL iview_clear_masks(V)
{
  IVIEW_WINDOW w;
  int i, n;
  
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(xlgaobject());
  
  if (! IVIEW_WINDOW_NULL(w)) {
    n = IViewNumPoints(w);
    for (i = 0; i < n; i++) IViewSetPointMask(w, i, FALSE);
    n = IViewNumLines(w);
    for (i = 0; i < n; i++) IViewSetLineMask(w, i, FALSE);
#ifdef USESTRINGS
    n = IViewNumStrings(w);
    for (i = 0; i < n; i++) IViewSetStringMask(w, i, FALSE);
#endif /* USESTRINGS */
  }
  return(NIL);
}


static LVAL base_slice_variable(V)
{
  unsigned var;
  int i, n;
  double low, high, value;
  
  var = getfixnum(xlgafixnum());
  low = makefloat(xlgetarg());
  high = makefloat(xlgetarg());
  xllastarg();
  
  if (var >= IViewNumVariables(wind)) return(NIL);
    
  n = IViewNumPoints(wind);
  for (i = 0; i < n; i++) {
    value = IViewPointValue(wind, var, i);
    if (value < low || value > high) IViewSetPointMask(wind, i, TRUE);
  }
  n = IViewNumLines(wind);
  for (i = 0; i < n; i++) {
    value = IViewLineValue(wind, var, i);
    if (value < low || value > high) IViewSetLineMask(wind, i, TRUE);
  }
#ifdef USESTRINGS
  n = IViewNumStrings(wind);
  for (i = 0; i < n; i++) {
    value = IViewStringValue(wind, var, i);
    if (value < low || value > high) IViewSetStringMask(wind, i, TRUE);
  }
#endif /* USESTRINGS */
  return(NIL);
}

static LVAL slice_variable(V)
{
  return(recursive_subr_map_elements(base_slice_variable, slice_variable));
}

static LVAL internal_slice_variable(V)
{
  wind = (IVIEW_WINDOW) GETIVIEWADDRESS(xlgaobject());
  if (! IVIEW_WINDOW_NULL(wind)) return(slice_variable());
  else return(NIL);
}

LVAL iview_slice_variable(V)      { return(internal_slice_variable()); }

static LVAL iview_X_to_screen P1C(int, scaled)
{
  IVIEW_WINDOW w;
  int x, y, var1, var2, screen_low, screen_high, origin_x, origin_y;
  double dx, dy, low, high;
  StGWWinInfo *gwinfo;
  
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(xlgaobject());
  gwinfo = IViewWindowWinInfo(w);
  dx = makefloat(xlgetarg());
  dy = makefloat(xlgetarg());
  xllastarg();
  
  if (IVIEW_WINDOW_NULL(w)) return(NIL);
  
  StGrGetContentOrigin(gwinfo, &origin_x, &origin_y);
  StGrGetContentVariables(gwinfo, &var1, &var2);

  if (scaled) IViewGetScaledRange(w, var1, &low, &high);
  else IViewGetRange(w, var1, &low, &high);
  IViewGetScreenRange(w, var1, &screen_low, &screen_high);
  if (low >= high) return(NIL);
  x = (dx - low) * ((screen_high - screen_low) / (high - low)) + screen_low;
  x = x + origin_x;
   
  if (scaled) IViewGetScaledRange(w, var2, &low, &high);
  else IViewGetRange(w, var2, &low, &high);
  IViewGetScreenRange(w, var2, &screen_low, &screen_high);
  if (low >= high) return(NIL);
  y = (dy - low) * ((screen_high - screen_low) / (high - low)) + screen_low;
  y = origin_y - y;

  return(integer_list_2(x, y));
}

static LVAL iview_screen_to_X P1C(int, scaled)
{
  IVIEW_WINDOW w;
  int x, y, var1, var2, screen_low, screen_high, origin_x, origin_y;
  double dx, dy, low, high;
  StGWWinInfo *gwinfo;
  
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(xlgaobject());
  gwinfo = IViewWindowWinInfo(w);
  x = getfixnum(xlgafixnum());
  y = getfixnum(xlgafixnum());
  xllastarg();
  
  if (IVIEW_WINDOW_NULL(w)) return(NIL);
  
  StGrGetContentOrigin(gwinfo, &origin_x, &origin_y);
  StGrGetContentVariables(gwinfo, &var1, &var2);

  x = x - origin_x;
  if (scaled) IViewGetScaledRange(w, var1, &low, &high);
  else IViewGetRange(w, var1, &low, &high);
  IViewGetScreenRange(w, var1, &screen_low, &screen_high);
  if (screen_low >= screen_high) return(NIL);
  dx = (x - screen_low) * ((high - low) / (screen_high - screen_low)) + low;
   
  y = origin_y - y;
  if (scaled) IViewGetScaledRange(w, var2, &low, &high);
  else IViewGetRange(w, var2, &low, &high);
  IViewGetScreenRange(w, var2, &screen_low, &screen_high);
  if (screen_low >= screen_high) return(NIL);
  dy = (y - screen_low) * ((high - low) / (screen_high - screen_low)) + low;

  return(double_list_2(dx, dy));
}

LVAL iview_real_to_screen(V)   { return(iview_X_to_screen(FALSE)); }
LVAL iview_scaled_to_screen(V) { return(iview_X_to_screen(TRUE));  }

LVAL iview_screen_to_real(V)   { return(iview_screen_to_X(FALSE)); }
LVAL iview_screen_to_scaled(V) { return(iview_screen_to_X(TRUE));  }

LVAL iview_points_in_rect(V)
{
  IVIEW_WINDOW w;
  int left, top, width, height, i, n;
  LVAL object, result, temp;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());
  xllastarg();

  if (IVIEW_WINDOW_NULL(w)) return(NIL);
  
  xlstkcheck(2);
  xlsave(result);
  xlsave(temp);
  IViewClearPointMarks(w);
  IViewMarkPointsInRect(w, left, top, width, height);
  n = IViewNumPoints(w);
  for (i = 0, result = NIL; i < n; i++)
    if (IViewPointMarked(w, i)) {
      temp = cvfixnum((FIXTYPE) i);
      result = cons(temp, result);
    }
  IViewClearPointMarks(w);
  xlpopn(2);
  
  return(result);
}

static struct { 
  int left, top, width, height, off_x, off_y;
} grey_rect;

static VOID drag_action P3C(IVIEW_WINDOW, w, int, x, int, y)
{
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  StGWFrameRect(gwinfo, grey_rect.left, grey_rect.top,
                        grey_rect.width, grey_rect.height);
  grey_rect.left = x - grey_rect.off_x;
  grey_rect.top = y - grey_rect.off_y;
  StGWFrameRect(gwinfo, grey_rect.left, grey_rect.top,
                        grey_rect.width, grey_rect.height);
}
  
LVAL iview_window_drag_grey_rect(V)
{
  LVAL object;
  IVIEW_WINDOW w;
  int mode, type;
  StGWWinInfo *gwinfo;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) GETIVIEWWINDOWADDRESS(object);
  gwinfo = StGWObWinInfo(object);
  if (IVIEW_WINDOW_NULL(w) || gwinfo == NULL) return(NIL);
  
  grey_rect.left = getfixnum(xlgafixnum());
  grey_rect.top = getfixnum(xlgafixnum());
  grey_rect.width = getfixnum(xlgafixnum());
  grey_rect.height = getfixnum(xlgafixnum());
  grey_rect.off_x = (moreargs()) ? getfixnum(xlgafixnum()) : grey_rect.width;
  grey_rect.off_y = (moreargs()) ? getfixnum(xlgafixnum()) : grey_rect.height;
  xllastarg();

  mode = StGWDrawMode(gwinfo);
  StGWSetDrawMode(gwinfo, 1);
  type = StGWLineType(gwinfo);
  StGWSetLineType(gwinfo, 1);
  
  grey_rect.left -= grey_rect.off_x;
  grey_rect.top -= grey_rect.off_y;
  
  StGWFrameRect(gwinfo, grey_rect.left, grey_rect.top,
                        grey_rect.width, grey_rect.height);


  StGWWhileButtonDown(gwinfo, drag_action, TRUE);

  StGWFrameRect(gwinfo, grey_rect.left, grey_rect.top,
                        grey_rect.width, grey_rect.height);

  StGWSetDrawMode(gwinfo, mode);
  StGWSetLineType(gwinfo, type);

  return(integer_list_4(grey_rect.left, grey_rect.top,
                        grey_rect.width, grey_rect.height));
}

static LVAL iview_points_state P1C(int, which)
{
  IVIEW_WINDOW w;
  LVAL points, next;
  int set, i, n;
  PointState state = pointNormal;
  
  switch(which) {
  case 'V': state = pointInvisible; break;
  case 'H': state = pointHilited; break;
  case 'S': state = pointSelected; break;
  }
  
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(xlgaobject());
  set = moreargs();
  points = (set) ? xlgalist() : NIL;
  xllastarg();
  
  if (! IVIEW_WINDOW_NULL(w)) {
    IViewCheckLinks(w);
    n = IViewNumPoints(w);
	if (set) {
	  IViewClearPointMarks(w);
	  for (next = points; consp(next) && fixp(car(next)); next = cdr(next)) {
	     i = getfixnum(car(next));
		 if (0 <= i && i < n) IViewSetPointMark(w, i, TRUE);
	  }
	  switch(which) {
	  case 'V':
	    for (i = 0; i < n; i++) {
          if (! IViewPointMasked(w, i)) {
		    if (IViewPointMarked(w, i) && IViewPointState(w, i) == pointInvisible)
		      IViewSetPointState(w, i, pointNormal);
		    else if (! IViewPointMarked(w, i) && IViewPointState(w, i) != pointInvisible)
              IViewSetPointState(w, i, pointInvisible);
	      }
	    }
		break;
	  case 'H':
	    for (i = 0; i < n; i++) {
          if (! IViewPointMasked(w, i)) {
		    if (IViewPointMarked(w, i) && IViewPointState(w, i) == pointNormal)
		      IViewSetPointState(w, i, pointHilited);
		    else if (! IViewPointMarked(w, i) && IViewPointState(w, i) != pointHilited)
              IViewSetPointState(w, i, pointNormal);
	      }
	    }
		break;
	  case 'S':
	    for (i = 0; i < n; i++) {
          if (! IViewPointMasked(w, i) && IViewPointState(w, i) != pointInvisible) {
		    if (IViewPointMarked(w, i)) IViewSetPointState(w, i, pointSelected);
		    else IViewSetPointState(w, i, pointNormal);
	      }
	    }
		break;
	  }
	  IViewAdjustScreens(w);
	}
    else {
	  xlsave1(points);
	  switch(which) {
	  case 'V':
	    for (i = n - 1, points = NIL; i >= 0; i--)
	      if (IViewPointState(w, i) != pointInvisible)
		    points = cons(cvfixnum((FIXTYPE) i), points);
	    break;
      case 'H':
	  case 'S':
	    for (i = n - 1, points = NIL; i >= 0; i--)
	      if (IViewPointState(w, i) == state)
		    points = cons(cvfixnum((FIXTYPE) i), points);
	    break;
	  }
      xlpop();
	}
  }
  return(points);
}

LVAL iview_points_showing(V)  { return(iview_points_state('V')); }
LVAL iview_points_hilited(V)  { return(iview_points_state('H')); }
LVAL iview_points_selected(V) { return(iview_points_state('S')); }

