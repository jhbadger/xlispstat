/* X11buttons - buttons for X11 dialogs and windows                    */
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

#define NullWindow ((Window) 0)

typedef struct {
  unsigned long fore, back;
} ColorPair;

/* layout defines */
# define BUTTON_PAD 15
# define BUTTON_LEAD 5

# define CloseButton 1
# define MenuButton 2

# define CLOSE_TEXT "Close"
# define MENU_TEXT "Menu"

/* forward declarations */
LOCAL LVAL special_button_object _((Display *dpy, Window button));
LOCAL VOID draw_button _((Display *dpy, Window win, LVAL item, int reversed));
LOCAL VOID draw_special_button _((Display *dpy, Window win,
				  int type, int reversed));
LOCAL VOID delete_special_button _((Window win, int type));

/***********************************************************************/
/**                                                                   **/
/**                        Global Variables                           **/
/**                                                                   **/
/***********************************************************************/

/* configuration parameters - should be set using the defaults database */
extern XFontStruct *DialogFont;
extern unsigned long DialogBorderColor, ButtonBorderColor;
extern ColorPair DialogC, ButtonC;
extern unsigned int dialog_border_width, button_border_width;
extern int min_button_height, min_button_width, dialog_item_gap;

extern GC DialogGC, DialogRGC;

extern XContext EventContext, ObjectContext, CloseContext, MenuContext;

extern LVAL s_menu, sk_select;

/***********************************************************************/
/**                                                                   **/
/**                         Button Items                              **/
/**                                                                   **/
/***********************************************************************/

static LVAL track_button(dpy, win, item, modal)
     Display *dpy;
     Window win;
     LVAL item;
     int modal;
{
  int done = FALSE;
  LVAL result = NIL;
  XEvent report;

  draw_button(dpy, win, item, TRUE);

  while (! done) {
    XNextEvent(dpy, &report);
    switch (report.type) {
    case ButtonRelease:
      done = TRUE;
      result = item;
      break;
    case LeaveNotify:
      done = TRUE;
      if (report.xcrossing.window == win)
	XSetWindowBorderWidth(dpy, win, button_border_width);
      break;
    default:
      break;
    }
  }
  draw_button(dpy, win, item, FALSE);
      
  if (! modal && result != NIL) send_message(item, sk_do_action);

  return(result);
}

LOCAL VOID draw_button(dpy, win, item, reversed)
     Display *dpy;
     Window win;
     LVAL item;
     int reversed;
{
  Point ssz, bsz;
  char *text;
  int x, y, len;
  GC gc;
  unsigned long color;

  gc = (reversed) ? DialogRGC : DialogGC;
  color = (reversed) ? DialogC.fore : DialogC.back;

  XSetWindowBackground(dpy, win, color);
  XClearWindow(dpy, win);
  text = checkstring(slot_value(item, s_text));
  ssz = DialogStringSize(text);
  bsz = ListToPoint(slot_value(item, s_size));
  x = (bsz.h - ssz.h) / 2;
  y = (bsz.v - ssz.v) / 2 + DialogFont->max_bounds.ascent;
  len = strlen(text);
  XDrawString(dpy, win, gc, x, y, text, len);
  XSetWindowBackground(dpy, win, DialogC.back);
}
  
static LVAL button_handler(report, modal)
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
      draw_button(dpy, win, item, FALSE);
      break;
    case ButtonPress:
      result = track_button(dpy, win, item, modal);
      break;
    case ButtonRelease:
      break;
    case EnterNotify:
      XSetWindowBorderWidth(dpy,report.xcrossing.window, 
			    button_border_width + 1);
      break;
    case LeaveNotify:
      XSetWindowBorderWidth(dpy, report.xcrossing.window, button_border_width);
      break;
    default: 
      break;
    }
  }
  return(result);
}

VOID InstallButtonItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Point loc, size;
  Window button;

  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  button = XCreateSimpleWindow(dpy, win, loc.h, loc.v, size.h, size.v,
			       button_border_width,
                               ButtonBorderColor, ButtonC.back);
  XSelectInput(dpy, button, 
	       StructureNotifyMask | ExposureMask | EnterWindowMask |
               LeaveWindowMask | ButtonPressMask | ButtonReleaseMask);

  set_slot_value(item, s_window_id, cvfixnum((FIXTYPE) button));

  install_dialog_item_handler(dpy, button, button_handler, item);
  if (XSaveContext(dpy, button, ObjectContext, (caddr_t) item) != 0)
    xlfail("could not install object in window");
}

VOID DeleteButtonItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Window button;

  button = (Window) getfixnum(slot_value(item, s_window_id));

  delete_dialog_item_handler(dpy, button);
  if (XDeleteContext(dpy, button, ObjectContext) != 0)
    xlfail("could not delete object context");
  set_slot_value(item, s_window_id, NIL);
}

VOID DialogButtonGetDefaultSize(item, width, height)
     LVAL item;
     int *width, *height;
{
  Point sz;

  sz = DialogStringSize(checkstring(slot_value(item, s_text)));
  if (width != NULL) *width = max(sz.h + BUTTON_PAD, min_button_width);
  if (height != NULL) *height = max(sz.v + BUTTON_LEAD, min_button_height);
}

/***********************************************************************/
/**                                                                   **/
/**                          Close Buttons                            **/
/**                                                                   **/
/***********************************************************************/

int ClosePanelHeight()
{
  Point sz;

  sz = DialogStringSize(CLOSE_TEXT);
  return (sz.v + 4 * BUTTON_LEAD);
}

LOCAL LVAL special_button_object(dpy, button)
     Display *dpy;
     Window button;
{
  LVAL object;

  if (XFindContext(dpy, button, ObjectContext, (caddr_t *) &object) == 0 
      && objectp(object))
    return(object);
  else return(NIL);
}

static Point special_button_size(type)
     int type;
{
  char *text = NULL;
  Point bsz, ssz;

  switch (type) {
  case CloseButton: text = CLOSE_TEXT; break;
  case MenuButton:  text = MENU_TEXT;  break;
  default: break;
  }

  ssz = DialogStringSize(text);
  bsz.h = max(ssz.h + BUTTON_PAD, min_button_width);
  bsz.v = max(ssz.v + BUTTON_LEAD, min_button_height);
  return(bsz);
}

static VOID track_special_button(dpy, win, type, modal)
     Display *dpy;
     Window win;
     int type, modal;
{
  int done = FALSE, result = FALSE;
  XEvent report;
  LVAL object;

  draw_special_button(dpy, win, type, TRUE);

  while (! done) {
    XNextEvent(dpy, &report);
    switch (report.type) {
    case ButtonRelease:
      done = TRUE;
      result = TRUE;
      break;
    case LeaveNotify:
      done = TRUE;
      if (report.xcrossing.window == win)
	XSetWindowBorderWidth(dpy, win, button_border_width);
      break;
    default:
      break;
    }
  }
  draw_special_button(dpy, win, type, FALSE);

  if (! modal && result == TRUE) {
    switch (type) {
    case CloseButton: 
      object = special_button_object(dpy, win);
      if (object != NIL) send_message(object, sk_close);
      break;
    default:
      break;
    }
  }
}

LOCAL VOID draw_special_button(dpy, win, type, reversed)
     Display *dpy;
     Window win;
     int type, reversed;
{
  Point ssz, bsz;
  char *text = NULL;
  int x, y, len;
  GC gc;
  unsigned long color;

  switch (type) {
  case CloseButton: text = CLOSE_TEXT; break;
  case MenuButton:  text = MENU_TEXT; break;
  default: break;
  }

  gc = (reversed) ? DialogRGC : DialogGC;
  color = (reversed) ? DialogC.fore : DialogC.back;

  XSetWindowBackground(dpy, win, color);
  XClearWindow(dpy, win);
  ssz = DialogStringSize(text);
  bsz = special_button_size(type);
  x = (bsz.h - ssz.h) / 2;
  y = (bsz.v - ssz.v) / 2 + DialogFont->max_bounds.ascent;
  len = strlen(text);
  XDrawString(dpy, win, gc, x, y, text, len);
  XSetWindowBackground(dpy, win, DialogC.back);
}
  
static LVAL basic_special_button_handler(report, modal, type)
     XEvent report;
     int modal, type;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Window win, child;
  int x, y, item;
  LVAL object, menu;

  if (modal) return(NIL);

  win = report.xany.window;
  object = special_button_object(dpy, win);
  if (objectp(object) && GETWINDOWADDRESS(object) != NullWindow) {
    switch (report.type) {
    case Expose:
      draw_special_button(dpy, win, type, FALSE);
      break;
    case ButtonPress:
      switch(type) {
      case MenuButton:
	XTranslateCoordinates(dpy, win, RootWindow(dpy, screen),
			      report.xbutton.x, report.xbutton.y, &x, &y,
			      &child);
	menu = slot_value(object, s_menu);
	if (menu_p(menu)) {
	  draw_special_button(dpy, win, type, TRUE);
	  XSetWindowBorderWidth(dpy, report.xcrossing.window, 
				button_border_width);
	  item = StMObPopup(menu, x, y, NIL);
	  draw_special_button(dpy, win, type, FALSE);
	  if (item > 0) send_message1(menu, sk_select, item);
	}
	else 
	  XSetWindowBorderWidth(dpy, report.xcrossing.window, 
				button_border_width);
	break;
      default:
	track_special_button(dpy, win, type, modal);
	break;
      }
      break;
    case ButtonRelease:
      break;
    case EnterNotify:
      XSetWindowBorderWidth(dpy,report.xcrossing.window, 
			    button_border_width + 1);
      break;
    case LeaveNotify:
      XSetWindowBorderWidth(dpy, report.xcrossing.window, button_border_width);
      break;
    default: 
      break;
    }
  }
  return(NIL);
}

static LVAL close_button_handler(report, modal)
     XEvent report;
     int modal;
{
  return(basic_special_button_handler(report, modal, CloseButton));
}

static LVAL menu_button_handler(report, modal)
     XEvent report;
     int modal;
{
  return(basic_special_button_handler(report, modal, MenuButton));
}

static VOID install_special_button(win, object, type)
     Window win;
     LVAL object;
     int type;
{
  Display *dpy = StX11Display();
  Point loc, size;
  Window button;
  LVAL (*handler)() = NULL;
  int width, height, gravity = 0;
  XSetWindowAttributes winattr;

  switch (type) {
  case CloseButton:
    loc.h = BUTTON_LEAD;
    loc.v = BUTTON_LEAD;
    size = special_button_size(type);
    handler = close_button_handler;
    gravity = NorthWestGravity;
    break;
  case MenuButton:
    size = special_button_size(type);
    loc.v = BUTTON_LEAD;
    StWGetSize(win, &width, &height, TRUE);
    loc.h = width - size.h - BUTTON_LEAD;
    handler = menu_button_handler;
    gravity = NorthEastGravity;
  default:
    break;
  }

  button = XCreateSimpleWindow(dpy, win, loc.h, loc.v, size.h, size.v,
			       button_border_width,
                               ButtonBorderColor, ButtonC.back);
  XSelectInput(dpy, button, 
	       StructureNotifyMask | ExposureMask | EnterWindowMask |
               LeaveWindowMask | ButtonPressMask | ButtonReleaseMask);

  winattr.win_gravity = gravity;
  XChangeWindowAttributes(dpy, button, CWWinGravity, &winattr);

  switch (type) {
  case CloseButton:
    if (XSaveContext(dpy, win, CloseContext, (caddr_t) button) != 0)
      xlfail("could not install close button context");
    break;
  case MenuButton:
    if (XSaveContext(dpy, win, MenuContext, (caddr_t) button) != 0)
      xlfail("could not install menu button context");
    break;
  default:
    break;
  }
  if (XSaveContext(dpy, button, EventContext, (caddr_t) handler) != 0)
    xlfail("could not install event handler");
  if (XSaveContext(dpy, button, ObjectContext, (caddr_t) object) != 0)
    xlfail("could not install object in window");
}

LOCAL VOID delete_special_button(win, type)
     Window win;
     int type;
{
  Display *dpy = StX11Display();
  Window button;
  XContext context = (XContext) 0;

  switch (type) {
  case CloseButton: context = CloseContext; break;
  case MenuButton:  context = MenuContext;  break;
  default: break;
  }

  if (XFindContext(dpy, win, context, (caddr_t *) &button) == 0) {
    if (XDeleteContext(dpy, win, context) != 0)
      xlfail("could not delete buttont context");
    if (XDeleteContext(dpy, button, EventContext) != 0)
      xlfail("could not delete event context");
    if (XDeleteContext(dpy, button, ObjectContext) != 0)
      xlfail("could not delete object context");
  }
}

VOID InstallCloseButton(win, object)
     Window win;
     LVAL object;
{
  install_special_button(win, object, CloseButton);
}

VOID DeleteCloseButton(win)
     Window win;
{
  delete_special_button(win, CloseButton);
}

VOID InstallMenuButton(win, object)
     Window win;
     LVAL object;
{
  install_special_button(win, object, MenuButton);
}

VOID DeleteMenuButton(win)
     Window win;
{
  delete_special_button(win, MenuButton);
}
