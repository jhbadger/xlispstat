/* X11slider - slider/scroll items for X11 dialogs                     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
/***********************************************************************/
/**                                                                   **/
/**                    General Includes and Definitions               **/
/**                                                                   **/
/***********************************************************************/

#include "dialogs.h"

extern Display *StX11Display();
extern Point DialogStringSize();
extern LVAL StX11ItemObject();
extern char *checkstring();
extern LVAL s_window_id;

typedef struct {
  unsigned long fore, back;
} ColorPair;

/* layout defines */
# define BUTTON_SIZE 16
# define MIN_SLIDER_HEIGHT  16
# define MIN_SLIDER_WIDTH  250
# define THUMB_WIDTH BUTTON_SIZE / 2

/* forward declarations */
LOCAL VOID check_value _((LVAL item));

/***********************************************************************/
/**                                                                   **/
/**                        Global Variables                           **/
/**                                                                   **/
/***********************************************************************/

/* configuration parameters - should be set using the defaults database */
extern XFontStruct *DialogFont;
extern unsigned long DialogBorderColor;
extern ColorPair DialogC;
extern unsigned int dialog_border_width;
extern int min_slider_width;

extern GC DialogGC, DialogRGC;

extern XContext ObjectContext, Button1Context,
  Button2Context, ThumbContext;

extern Pixmap LeftSliderPM, RightSliderPM;
extern Cursor DoubleArrowCursor, RightArrowCursor, LeftArrowCursor,
  ArrowCursor;

/***********************************************************************/
/**                                                                   **/
/**                          Slider Items                             **/
/**                                                                   **/
/***********************************************************************/

static VOID adjust_slider(item)
     LVAL item;
{
  Display *dpy = StX11Display();
  LVAL win_id;
  Window win, thumb;
  Point size;
  int left, low, high, val, width;
  double x;

  win_id = slot_value(item, s_window_id);
  if (fixp(win_id)) {
    win = (Window) getfixnum(win_id);
    size = ListToPoint(slot_value(item, s_size));
    low = getfixnum(slot_value(item, s_min_value));
    high = getfixnum(slot_value(item, s_max_value));
    val = getfixnum(slot_value(item, s_value));
    width = size.h - 2 * BUTTON_SIZE - THUMB_WIDTH;
    if (low < high) {
      x = ((double) (val - low)) / ((double) (high - low));
      left = BUTTON_SIZE + x * width;
      left = max(BUTTON_SIZE, min(left, width + BUTTON_SIZE));
    }
    else left = BUTTON_SIZE;
    if (XFindContext(dpy, win, ThumbContext, (caddr_t *) &thumb) != 0)
      xlfail("can't find thumb context");
    XMoveWindow(dpy, thumb, left, 0);
  }
}

static VOID track_thumb(dpy, win, report, item)
     Display *dpy;
     Window win;
     XEvent report;
     LVAL item;
{
  Point size;
  int low, high, val, old_val, width, page, up, done = FALSE;
  double x;

  size = ListToPoint(slot_value(item, s_size));
  low = getfixnum(slot_value(item, s_min_value));
  high = getfixnum(slot_value(item, s_max_value));
  width = size.h - 2 * BUTTON_SIZE - THUMB_WIDTH;

  switch (report.xbutton.button) {
  case Button1:
  case Button3:
    up = (report.xbutton.button == Button3) ? TRUE : FALSE;
    XDefineCursor(dpy, win, (up) ? RightArrowCursor : LeftArrowCursor); 
    old_val = low - 1;  /* an "impossible" value */
    while (! done) {
      if (XCheckTypedEvent(dpy, ButtonRelease, &report)) done = TRUE;
      else{
	val = getfixnum(slot_value(item, s_value));
	page = getfixnum(slot_value(item, s_page_increment));
	if (up) val += page;
	else val -= page;
	val = max(low, min(val, high));
	DialogScrollItemValue(item, TRUE, val);
	if (val != old_val) send_message(item, sk_scroll_action);
	old_val = val;
	XSync(dpy, FALSE);
      }
    }
    break; 
  case Button2: 
    if (width > 0)
      x = ((double) report.xbutton.x - BUTTON_SIZE) / ((double) width);
    else x = 0.0;
    val = low + x * (high - low);
    val = max(low, min(val, high));
    DialogScrollItemValue(item, TRUE, val);
#ifdef DODO
    XGrabPointer(dpy, win, TRUE, ButtonMotionMask,
                 GrabModeAsync, GrabModeAsync, win, 
		 DoubleArrowCursor, CurrentTime);
#endif /* DODO */
    while (! done) {
      XNextEvent(dpy, &report);
      switch (report.type) {
      case ButtonRelease:
	done = TRUE;
	break;
      case MotionNotify:
	x = ((double) report.xmotion.x - BUTTON_SIZE) / ((double) width);
	val = low + x * (high - low);
	val = max(low, min(val, high));
	DialogScrollItemValue(item, TRUE, val);
	break;
      default:
	break;
      }
    }
#ifdef DODO
    XUngrabPointer(dpy, CurrentTime);
#endif /* DODO */
    send_message(item, sk_do_action);
    XSync(dpy, FALSE);
    break;
  }

  XDefineCursor(dpy, win, DoubleArrowCursor);
}

static LVAL slider_handler(report, modal)
     XEvent report;
     int modal;
{
  Display *dpy = StX11Display();
  Window win;
  LVAL item;
  LVAL result = NIL;

  win = report.xany.window;
  item = StX11ItemObject(dpy, win);
  if (item != NIL) {
    switch (report.type) {
    case Expose:
      break;
    case ButtonPress:
      track_thumb(dpy, win, report, item);
/*
      if (! modal) {
	send_message(item, sk_do_action);
	XSync(dpy, FALSE);
      }
*/
/*      send_message(item, sk_do_action);*/
      XSync(dpy, FALSE);
      break;
    case ButtonRelease:
      break;
    default: 
      break;
    }
  }
  return(result);
}

static LVAL button_handler(report, is_right)
     XEvent report;
     int is_right;
{
  Display *dpy = StX11Display();
  Window win;
  LVAL item;
  LVAL result = NIL;
  int low, high, val, done, old_val;

  win = report.xany.window;
  item = StX11ItemObject(dpy, win);
  if (item != NIL) {
    switch (report.type) {
    case ButtonPress:
      done = FALSE;
      low = getfixnum(slot_value(item, s_min_value));
      high = getfixnum(slot_value(item, s_max_value));
      old_val = low - 1; /* an "impossible" value */
      while (! done) {
	if (XCheckTypedEvent(dpy, ButtonRelease, &report)) done = TRUE;
	else {
	  val = getfixnum(slot_value(item, s_value));
	  if (is_right) val++;
	  else val--;
	  val = max(low, min(val, high));
	  DialogScrollItemValue(item, TRUE, val);
	  if (val != old_val) send_message(item, sk_scroll_action);
	  old_val = val;
	  XSync(dpy, FALSE);
	}
      }
      /* send_message(item, sk_do_action); */
      break;
    default: 
      break;
    }
  }
  return(result);
}

static LVAL left_button_handler(report, modal)
     XEvent report;
     int modal;
{
  return(button_handler(report, FALSE));
}

static LVAL right_button_handler(report, modal)
     XEvent report;
     int modal;
{
  return(button_handler(report, TRUE));
}

VOID InstallScrollItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Point loc, size;
  Window slider, button1, button2, thumb;

  check_value(item);
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  size.v = BUTTON_SIZE;
  slider = XCreateSimpleWindow(dpy, win, loc.h, loc.v, size.h, size.v,
			       dialog_border_width, 
			       DialogBorderColor, DialogC.back);
  XSelectInput(dpy, slider, 
	       ExposureMask | ButtonMotionMask | 
	       ButtonPressMask | ButtonReleaseMask);
  XDefineCursor(dpy, slider, DoubleArrowCursor);

  button1 = XCreateSimpleWindow(dpy, slider, 
				-dialog_border_width, -dialog_border_width, 
				BUTTON_SIZE, BUTTON_SIZE,
				dialog_border_width,
				DialogBorderColor, DialogC.back);
  XSelectInput(dpy, button1,
               ExposureMask | ButtonPressMask | ButtonReleaseMask);
  XSetWindowBackgroundPixmap(dpy, button1, LeftSliderPM);
#ifdef DODO
  XDefineCursor(dpy, button1, LeftArrowCursor);
#endif /* DODO */
  XDefineCursor(dpy, button1, ArrowCursor);

  button2 = XCreateSimpleWindow(dpy, slider, 
				size.h - BUTTON_SIZE, -dialog_border_width, 
				BUTTON_SIZE, BUTTON_SIZE,
				dialog_border_width,
                                DialogBorderColor, DialogC.back);
  XSelectInput(dpy, button2,
               ExposureMask | ButtonPressMask | ButtonReleaseMask);
  XSetWindowBackgroundPixmap(dpy, button2, RightSliderPM);
#ifdef DODO
  XDefineCursor(dpy, button2, RightArrowCursor);
#endif /* DODO */
  XDefineCursor(dpy, button2, ArrowCursor);
  
  thumb = XCreateSimpleWindow(dpy, slider, BUTTON_SIZE, 0,
			      THUMB_WIDTH, BUTTON_SIZE,
			      0, DialogBorderColor, DialogC.fore);

  set_slot_value(item, s_window_id, cvfixnum((FIXTYPE) slider));

  install_dialog_item_handler(dpy, slider, slider_handler, item);
  if (XSaveContext(dpy, slider, ObjectContext, (caddr_t) item) != 0)
    xlfail("could not install object in window");
  if (XSaveContext(dpy, slider, ThumbContext, (caddr_t) thumb) != 0)
    xlfail("could not install thumb in slider");

  if (XSaveContext(dpy, slider, Button1Context, (caddr_t) button1) != 0)
    xlfail("could not install left button in slider");
  install_dialog_item_handler(dpy, button1, left_button_handler, item);
  if (XSaveContext(dpy, button1, ObjectContext, (caddr_t) item) != 0)
    xlfail("could not install object in window");

  if (XSaveContext(dpy, slider, Button2Context, (caddr_t) button2) != 0)
    xlfail("could not install right button in slider");
  install_dialog_item_handler(dpy, button2, right_button_handler, item);
  if (XSaveContext(dpy, button2, ObjectContext, (caddr_t) item) != 0)
    xlfail("could not install object in window");

  adjust_slider(item);

  XMapSubwindows(dpy, slider);
}

VOID DeleteScrollItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Window slider, button1, button2;

  slider = (Window) getfixnum(slot_value(item, s_window_id));

  delete_dialog_item_handler(dpy, slider);
  if (XDeleteContext(dpy, slider, ObjectContext) != 0)
    xlfail("could not delete object context");
  if (XDeleteContext(dpy, slider, ThumbContext) != 0)
    xlfail("could not delete thumb context");

  if (XFindContext(dpy, slider, Button1Context, (caddr_t *) &button1) != 0)
    xlfail("can't find left button context");
  if (XDeleteContext(dpy, slider, Button1Context) != 0)
    xlfail("could not delete left button context");
  delete_dialog_item_handler(dpy, button1);
  if (XDeleteContext(dpy, button1, ObjectContext) != 0)
    xlfail("could not delete object context");

  if (XFindContext(dpy, slider, Button2Context, (caddr_t *) &button2) != 0)
    xlfail("can't find right button context");
  if (XDeleteContext(dpy, slider, Button2Context) != 0)
    xlfail("could not delete right button context");
  delete_dialog_item_handler(dpy, button2);
  if (XDeleteContext(dpy, button2, ObjectContext) != 0)
    xlfail("could not delete object context");

  set_slot_value(item, s_window_id, NIL);
}

VOID DialogScrollGetDefaultSize(item, width, height)
     LVAL item;
     int *width, *height;
{
  if (width != NULL) *width = MIN_SLIDER_WIDTH;
  if (height != NULL) *height = MIN_SLIDER_HEIGHT;
}

LVAL DialogScrollItemValue(item, set, value)
     LVAL item;
     int set, value;
{
  if (set) {
    set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
    check_value(item);
    adjust_slider(item);
  }
  else check_value(item);

  return(slot_value(item, s_value));
}

LVAL DialogScrollItemMin(item, set, value) 
     LVAL item;
     int set, value;
{
  if (set) {
    set_slot_value(item, s_min_value, cvfixnum((FIXTYPE) value));
    check_value(item);
    adjust_slider(item);
  }
  return(slot_value(item, s_min_value));
}

LVAL DialogScrollItemMax(item, set, value)
     LVAL item;
     int set, value;
{
  if (set) {
    set_slot_value(item, s_max_value, cvfixnum((FIXTYPE) value));
    check_value(item);
    adjust_slider(item);
  }
  return(slot_value(item, s_max_value));
}

LOCAL VOID check_value(item)
     LVAL item;
{
  LVAL low, high, value;
  int ilow, ihigh, ivalue;

  low = slot_value(item, s_min_value);
  high = slot_value(item, s_max_value);
  value = slot_value(item, s_value);

  ilow = (fixp(low)) ? getfixnum(low): 0;
  ihigh = (fixp(high)) ? getfixnum(high) : 100;
  ivalue = (fixp(value)) ? getfixnum(value) : ilow;

  if (ilow >= ihigh) ihigh = ilow + 1;
  ivalue = max(ilow, min(ihigh, ivalue));

  if (! fixp(low) || ilow != getfixnum(low)) 
    set_slot_value(item, s_min_value, cvfixnum((FIXTYPE) ilow));
  if (! fixp(high) || ihigh != getfixnum(high)) 
    set_slot_value(item, s_max_value, cvfixnum((FIXTYPE) ihigh));
  if (! fixp(value) || ivalue != getfixnum(value)) 
    set_slot_value(item, s_value, cvfixnum((FIXTYPE) ivalue));
}
