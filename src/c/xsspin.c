/* xsspin - XLISP interface to IVIEW dynamic graphics package.         */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

#define LABEL_OFFSET 3
#define AXIS_FRACTION 0.5

#define SPIN_CONTROL_HEIGHT 20
#define SPIN_CONTROL_SIZE 10
#define SPIN_CONTROL_LEFT 5
#define SPIN_CONTROL_TOP 5
#define SPIN_CONTROL_GAP 5
#define SPIN_PITCH_STRING "Pitch"
#define SPIN_ROLL_STRING  "Roll"
#define SPIN_YAW_STRING   "Yaw"
#define ALPHA 3.14159 / 72.0

#define Pitching    0
#define Rolling     1
#define Yawing      2
#define ApplyMatrix 3

/* external variables */
extern LVAL s_depth_cuing, s_showing_labels, s_showing_axes, 
  s_variable_labels, s_content_variables, s_true, s_rotation_type,
  s_rotation_angle, s_rotation_controls, sk_scale, sk_draw, sk_show,
  sk_resize, sk_redraw, sk_adjust_depth_cuing, sk_draw_axes, sk_show_window,
  sk_apply_transformation, s_pitching, s_rolling, s_yawing;
  
/* forward declarations */
LOCAL VOID set_content_variables P4H(LVAL, unsigned, unsigned, unsigned);
LOCAL VOID IViewSpinRotate P1H(IVIEW_WINDOW);
LOCAL int is_showing_axes P1H(LVAL);
LOCAL VOID set_showing_axes P2H(LVAL, int);
LOCAL int rotation_type P1H(LVAL);
LOCAL VOID set_rotation_type P2H(LVAL, int);
LOCAL VOID set_angle P2H(LVAL, double);
LOCAL double spin_angle _((LVAL object));
LOCAL int is_cuing P1H(LVAL);
LOCAL VOID set_cuing P2H(LVAL, int);
LOCAL VOID adjust_cuing P1H(LVAL);
LOCAL VOID cuing_off P1H(LVAL);

/**************************************************************************/
/**                                                                      **/
/**                      Spinner Creation Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_spin_allocate(V)
{
  LVAL object;
  int vars, i, show, ascent, height;
  IVIEW_WINDOW w;
  StGWWinInfo *gwinfo;

  object = xlgaobject();
  show = xsboolkey(sk_show, TRUE);

  gwinfo = StGWObWinInfo(object);
  get_iview_ivars(object, &vars);
  
  if (vars < 3) xlfail("too few variables");
  w = IViewNew(object);
  initialize_iview(w, object);
  
  for (i = 0; i < vars; i++)
    IViewSetScaledRange(w, i, -sqrt((double) vars), sqrt((double) vars));
  set_content_variables(object, 0, 1, 2);
  
  IViewSetIdentityTransformation(w);
  set_rotation_type(object, Rolling);
  set_angle(object, ALPHA);
  ascent = StGWTextAscent(gwinfo);
  height = (ascent > SPIN_CONTROL_SIZE) ? 2 * ascent : SPIN_CONTROL_HEIGHT;
  StGrSetMargin(gwinfo, 0, 0, 0, height);
  
  /* use StShowWindow to show (map) window but NOT send :resize or :redraw */
  if (show) StShowWindow(w);

  return(object);
}

/**************************************************************************/
/**                                                                      **/
/**                     State Accessors and Mutators                     **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_spin_content_variables(V)
{
  int x = 0, y = 0, z = 0, set = FALSE;
  LVAL object;
  
  object = xlgaobject();
  if (moreargs()) {
    set = TRUE;
    x = getfixnum(xlgafixnum());
    y = getfixnum(xlgafixnum());
    z = getfixnum(xlgafixnum());
  } 
  xllastarg();
  
  if (set) set_content_variables(object, x, y, z);

  return(slot_value(object, s_content_variables));
}

LOCAL VOID set_content_variables P4C(LVAL, object, unsigned, x, unsigned, y, unsigned, z)
{    
  StGWWinInfo *gwinfo = StGWObWinInfo(object);
  StGrSetContentVariables(gwinfo, x, y);
  set_slot_value(object, s_content_variables, integer_list_3(x, y, z));
}

static VOID get_content_variables P4C(LVAL, object, int *, x, int *, y, int *, z)
{
  LVAL val = slot_value(object, s_content_variables);
  if (consp(val) && fixp(car(val))) { 
    if (x != NULL) *x = getfixnum(car(val)); 
    val = cdr(val);
  }    
  if (consp(val) && fixp(car(val))) {
    if (y != NULL) *y = getfixnum(car(val));
    val = cdr(val);
  }    
  if (consp(val) && fixp(car(val))) {
    if (z != NULL) *z = getfixnum(car(val));
    val = cdr(val);
  }    
}

LVAL iview_spin_depth_cuing(V)
{
  LVAL object;
  
  object = xlgaobject();
  if (moreargs()) 
    set_cuing(object, (xlgetarg() != NIL) ? TRUE : FALSE);
  xllastarg();
  
  return((is_cuing(object)) ? s_true : NIL);
}

LOCAL int is_cuing P1C(LVAL, object)
{
  return((slot_value(object, s_depth_cuing) != NIL) ? TRUE : FALSE);
}

LOCAL VOID set_cuing P2C(LVAL, object, int, cuing)
{
  set_slot_value(object, s_depth_cuing, (cuing) ? s_true : NIL);    
  if (cuing) adjust_cuing(object);
  else cuing_off(object);
}

LVAL iview_spin_showing_axes(V)
{
  LVAL object;
  
  object = xlgaobject();
  if (moreargs())
    set_showing_axes(object, (xlgetarg() != NIL) ? TRUE : FALSE);
  xllastarg();
  
  return((is_showing_axes(object)) ? s_true : NIL);
}

LOCAL int is_showing_axes P1C(LVAL, object)
{
  return((slot_value(object, s_showing_axes) != NIL) ? TRUE : FALSE);
}

LOCAL VOID set_showing_axes P2C(LVAL, object, int, showing)
{
  set_slot_value(object, s_showing_axes, (showing) ? s_true : NIL);    
}

LOCAL int rotation_type P1C(LVAL, object)
{
  LVAL value = slot_value(object, s_rotation_type);

  if (symbolp(value)) {
    if (value == s_pitching) return(Pitching);
    else if (value == s_yawing) return(Yawing);
    else return(Rolling);
  }
  else if (matrixp(value)) return(ApplyMatrix);
  else return(Pitching);
}

LOCAL VOID set_rotation_type P2C(LVAL, object, int, type)
{
  LVAL value;

  switch (type) {
  case Pitching: value = s_pitching; break;
  case Rolling:  value = s_rolling;  break;
  case Yawing:   value = s_yawing;   break;
  default:       value = s_pitching; break;
  }
  set_slot_value(object, s_rotation_type, value);
}

LOCAL VOID set_angle P2C(LVAL, object, double, alpha)
{
  set_slot_value(object, s_rotation_angle, cvflonum((FLOTYPE) alpha));
}

LOCAL double spin_angle P1C(LVAL, object)
{
  LVAL value = slot_value(object, s_rotation_angle);
  
  if (floatp(value)) return(getflonum(value));
  else return(0.0);
}

LVAL iview_spin_angle(V)
{
  LVAL object;
  
  object = xlgaobject();
  if (moreargs()) set_angle(object, makefloat(xlgetarg()));
  xllastarg();
  
  return(slot_value(object, s_rotation_angle));
}

/**************************************************************************/
/**                                                                      **/
/**                            Data Functions                            **/
/**                                                                      **/
/**************************************************************************/

/**************************************************************************/
/**                                                                      **/
/**                    Drawing and Resizing Functions                    **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_spin_resize(V)
{
  IVIEW_WINDOW w;
  LVAL object;
  int vars, i, top, left, width, height, size;
  StGWWinInfo *gwinfo;
  object = xlgaobject();
  xllastarg();
  
  gwinfo = StGWObWinInfo(object);
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  if (IVIEW_WINDOW_NULL(w) || gwinfo == NULL) return(NIL);
  
  vars = IViewNumVariables(w);
  IViewSetFixedAspect(w, TRUE);
  IViewStdResize(w);
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  size = width;
  StGrSetContentOrigin(gwinfo, left + size / 2, top + size / 2);
    
  for (i = 0; i < vars; i++)
    IViewSetScreenRange(w, i, - size / 2, size / 2);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);
  StGWSetClipRect(gwinfo, TRUE, left, top, width, height);

  return(NIL);
}

static VOID redraw_content P2C(IVIEW_WINDOW, w, LVAL, object)
{
  int left, top, width, height;
  StGWWinInfo *gwinfo = StGWObWinInfo(object);
  
  if (is_cuing(object)) adjust_cuing(object);
  StGWStartBuffering(gwinfo);
  IViewStdRedrawContent(w);
  if (is_showing_axes(object)) send_message(object, sk_draw_axes);
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGWBufferToScreen(gwinfo, left, top, width, height);
}

LVAL iview_spin_draw_axes(V)
{
  IVIEW_WINDOW w;
  int x, y, xorig, yorig, size, xend, yend, vars, i, unit;
  char *s;
  double **a;
  StGWWinInfo *gwinfo;
  LVAL object;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  gwinfo = StGWObWinInfo(object);
  if (! IVIEW_WINDOW_NULL(w)) {
    StGrGetContentVariables(gwinfo, &x, &y);
    StGrGetContentOrigin(gwinfo, &xorig, &yorig);
    StGrGetContentRect(gwinfo, NULL, NULL, &size, NULL);
    vars = IViewNumVariables(w);
    a = IViewTransformation(w);

    unit = size / 2;
    for (i = 0; i < vars; i++) {
      xend = xorig + a[x][i] * AXIS_FRACTION * unit;
      yend = yorig - a[y][i] * AXIS_FRACTION * unit;
      StGWDrawLine(gwinfo, xorig, yorig, xend, yend);
      s = IViewVariableLabel(w, i);
      xend += LABEL_OFFSET;
      yend -= LABEL_OFFSET;
      StGWDrawString(gwinfo, s, xend, yend);
    }
  }
  return(NIL);
}

LVAL iview_spin_redraw_content(V)
{
  IVIEW_WINDOW w;
  LVAL object;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  xllastarg();
  
  if (! IVIEW_WINDOW_NULL(w)) redraw_content(w, object);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                           Mouse Functions                            **/
/**                                                                      **/
/**************************************************************************/

LOCAL VOID adjust_cuing P1C(LVAL, object)
{
  int vx, vy, vz;
  
  get_content_variables(object, &vx, &vy, &vz);
  send_message1(object, sk_adjust_depth_cuing, vz);
}

LOCAL VOID cuing_off P1C(LVAL, object)
{
  IVIEW_WINDOW w;
  int n, i;

  w = (IVIEW_WINDOW) GETIVIEWADDRESS(object);
  if (IVIEW_WINDOW_NULL(w)) return;

  n  = IViewNumPoints(w);
  for (i = 0; i < n; i++) IViewSetPointSymbol(w, i, 0, 5);
  n = IViewNumLines(w);
  for (i = 0; i < n; i++) IViewSetLineWidth(w, i, 1);
} 

/**************************************************************************/
/**                                                                      **/
/**                         Rotation Functions                           **/
/**                                                                      **/
/**************************************************************************/

LOCAL VOID IViewSpinRotate P1C(IVIEW_WINDOW, w)
{
  int x, y, z;
  LVAL object = IViewWindowGetObject(w);
  double alpha = spin_angle(object);
  
  get_content_variables(object, &x, &y, &z);
  
  switch (rotation_type(object)) {
  case Pitching:    IViewRotate2(w, y, z, alpha); break;
  case Rolling:     IViewRotate2(w, x, y, alpha); break;
  case Yawing:      IViewRotate2(w, x, z, alpha); break;
  case ApplyMatrix: send_message_1L(object, 
                                    sk_apply_transformation,
                                    slot_value(object, s_rotation_type));
  }
  IViewRedrawContent(w);
}

LVAL iview_spin_rotate(V)
{
  IVIEW_WINDOW w;
  
  w = (IVIEW_WINDOW) GETIVIEWADDRESS(xlgaobject());
  xllastarg();
  
  if (! IVIEW_WINDOW_NULL(w)) IViewSpinRotate(w);
  return(NIL);
}
