/* X11toggle - toggle items for X11 dialogs                            */
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
# define TOGGLE_PAD 20
# define TOGGLE_MARK_HEIGHT 16

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
extern int min_toggle_height;

extern GC DialogGC, DialogRGC;

extern XContext ObjectContext;

extern Pixmap ToggleOffPM, ToggleOnPM;

/***********************************************************************/
/**                                                                   **/
/**                         Toggle Items                              **/
/**                                                                   **/
/***********************************************************************/

static VOID draw_toggle(dpy, win, item)
     Display *dpy;
     Window win;
     LVAL item;
{
  char *text;
  int x, y, len, ascent, on;
  Pixmap mark;

  ascent = DialogFont->max_bounds.ascent;

  text = checkstring(slot_value(item, s_text));
  on = (slot_value(item, s_value) != NIL) ? TRUE : FALSE;
  mark = (on) ? ToggleOnPM : ToggleOffPM;
  XCopyPlane(dpy, mark, win, DialogGC, 
	     0, 0, TOGGLE_MARK_HEIGHT, TOGGLE_MARK_HEIGHT,
	     0, max(0, ascent - TOGGLE_MARK_HEIGHT), 1);
  x = TOGGLE_PAD;
  y = max(TOGGLE_MARK_HEIGHT, ascent);
  len = strlen(text);
  XDrawString(dpy, win, DialogGC, x, y, text, len);
}
  
static LVAL toggle_handler(report, modal)
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
      draw_toggle(dpy, win, item);
      break;
    case ButtonPress:
      set_slot_value(item, s_value, 
		     (slot_value(item, s_value) != NIL) ? NIL : s_true);
      draw_toggle(dpy, win, item);
      if (! modal) send_message(item, sk_do_action);
      break;
    case ButtonRelease:
      break;
    default: 
      break;
    }
  }
  return(result);
}

VOID InstallToggleItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Point loc, size;
  Window toggle;

  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  toggle = XCreateSimpleWindow(dpy, win, loc.h, loc.v, size.h, size.v,
			       0, DialogBorderColor, DialogC.back);
  XSelectInput(dpy, toggle, 
	       ExposureMask | ButtonPressMask | ButtonReleaseMask);

  set_slot_value(item, s_window_id, cvfixnum((FIXTYPE) toggle));

  install_dialog_item_handler(dpy, toggle, toggle_handler, item);
  if (XSaveContext(dpy, toggle, ObjectContext, (caddr_t) item) != 0)
    xlfail("could not install object in window");
}

VOID DeleteToggleItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Window toggle;

  toggle = (Window) getfixnum(slot_value(item, s_window_id));

  delete_dialog_item_handler(dpy, toggle);
  if (XDeleteContext(dpy, toggle, ObjectContext) != 0)
    xlfail("could not delete object context");
  set_slot_value(item, s_window_id, NIL);
}

VOID DialogToggleGetDefaultSize(item, width, height)
     LVAL item;
     int *width, *height;
{
  Point sz;
  sz = DialogStringSize(checkstring(slot_value(item, s_text)));
  if (width != NULL) *width = sz.h + TOGGLE_PAD;
  if (height != NULL) *height = max(sz.v, min_toggle_height);
}

LVAL DialogToggleItemValue(item, set, value)
     LVAL item, value;
     int set;
{
  Display *dpy = StX11Display();
  LVAL win_id;

  if (set) {
    set_slot_value(item, s_value, (value != NIL) ? s_true : NIL);
    win_id = slot_value(item, s_window_id);
    if (fixp(win_id)) draw_toggle(dpy, (Window) getfixnum(win_id), item);
  }
  return(slot_value(item, s_value));
}
