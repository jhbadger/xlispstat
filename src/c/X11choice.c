/* X11choice - choice items for X11 dialogs                            */
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
# define CHOICE_PAD 20
# define CHOICE_MARK_HEIGHT 16
# define CHOICE_LEAD 5

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
extern int min_choice_height;

extern GC DialogGC, DialogRGC;

extern XContext ObjectContext;

extern Pixmap ChoiceOffPM, ChoiceOnPM;

/***********************************************************************/
/**                                                                   **/
/**                         Choice Items                              **/
/**                                                                   **/
/***********************************************************************/

static VOID draw_choice(dpy, win, item)
     Display *dpy;
     Window win;
     LVAL item;
{
  LVAL text, val;
  char *text_item;
  int x, y, len, ascent, descent, value, i, step;
  Pixmap mark;

  ascent = DialogFont->max_bounds.ascent;
  descent = DialogFont->max_bounds.descent;

  text = slot_value(item, s_text);
  val = slot_value(item, s_value);
  value = (fixp(val)) ? getfixnum(val) : 0;
  x = CHOICE_PAD;
  step = descent + max(CHOICE_MARK_HEIGHT, ascent) + CHOICE_LEAD;

  for (i = 0, y = max(ascent, CHOICE_MARK_HEIGHT);
       consp(text);
       i++, y += step, text = cdr(text)) {
    mark = (value == i) ? ChoiceOnPM : ChoiceOffPM;
    text_item = checkstring(car(text));
    XCopyPlane(dpy, mark, win, DialogGC, 
	       0, 0, CHOICE_MARK_HEIGHT, CHOICE_MARK_HEIGHT,
	       0, y - CHOICE_MARK_HEIGHT, 1);
    len = strlen(text_item);
    XDrawString(dpy, win, DialogGC, x, y, text_item, len);
  }
}
  
static LVAL choice_handler(report, modal)
     XEvent report;
     int modal;
{
  Display *dpy = StX11Display();
  Window win;
  LVAL item, result = NIL;
  int i, item_height, ascent, descent;

  ascent = DialogFont->max_bounds.ascent;
  descent = DialogFont->max_bounds.descent;

  win = report.xany.window;
  item = StX11ItemObject(dpy, win);
  if (item != NIL) {
    switch (report.type) {
    case Expose:
      draw_choice(dpy, win, item);
      break;
    case ButtonPress:
      item_height = descent + max(CHOICE_MARK_HEIGHT, ascent) + CHOICE_LEAD;
      i = report.xbutton.y / item_height;
      DialogChoiceItemValue(item, TRUE, i);
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

VOID InstallChoiceItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Point loc, size;
  Window choice;

  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  choice = XCreateSimpleWindow(dpy, win, loc.h, loc.v, size.h, size.v,
			       0, DialogBorderColor, DialogC.back);
  XSelectInput(dpy, choice, 
	       ExposureMask | ButtonPressMask | ButtonReleaseMask);

  set_slot_value(item, s_window_id, cvfixnum((FIXTYPE) choice));
  if (! fixp(slot_value(item, s_value)))
    set_slot_value(item, s_value, cvfixnum((FIXTYPE) 0));

  install_dialog_item_handler(dpy, choice, choice_handler, item);
  if (XSaveContext(dpy, choice, ObjectContext, (caddr_t) item) != 0)
    xlfail("could not install object in window");
}

VOID DeleteChoiceItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Window choice;

  choice = (Window) getfixnum(slot_value(item, s_window_id));

  delete_dialog_item_handler(dpy, choice);
  if (XDeleteContext(dpy, choice, ObjectContext) != 0)
    xlfail("could not delete object context");
  set_slot_value(item, s_window_id, NIL);
}

VOID DialogChoiceGetDefaultSize(item, width, height)
     LVAL item;
     int *width, *height;
{
  Point i_sz, s_sz;
  LVAL text = slot_value(item, s_text);

  for (i_sz.h = 0, i_sz.v = 0; consp(text); text = cdr(text)) {
    s_sz = DialogStringSize(getstring(car(text)));
    if (i_sz.v != 0) i_sz.v += CHOICE_LEAD; /* only add lead between lines */
    i_sz.v += max(s_sz.v, min_choice_height);
    i_sz.h = max(i_sz.h, s_sz.h);
  }
  if (width != NULL) *width = i_sz.h + CHOICE_PAD;
  if (height != NULL) *height = i_sz.v;
}

LVAL DialogChoiceItemValue(item, set, value)
     LVAL item;
     int set, value;
{
  Display *dpy = StX11Display();
  LVAL win_id, text;
  int n;

  if (set) {
    text = slot_value(item, s_text);
    n = (consp(text)) ? llength(text) : 0;
    value = max(0, min(value, n - 1));
    set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
    win_id = slot_value(item, s_window_id);
    if (fixp(win_id)) draw_choice(dpy, (Window) getfixnum(win_id), item);
  }
  return(slot_value(item, s_value));
}
