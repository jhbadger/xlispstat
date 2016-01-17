/* X11text - text items for X11 dialogs                                */
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
extern LVAL s_window_id, s_text_length;

typedef struct {
  unsigned long fore, back;
} ColorPair;

# define STATIC_TEXT_LEAD 5
# define STATIC_TEXT_PAD 10
# define EDIT_TEXT_LEAD 10
# define EDIT_TEXT_PAD 20

/***********************************************************************/
/**                                                                   **/
/**                        Global Variables                           **/
/**                                                                   **/
/***********************************************************************/

/* configuration parameters - should be set using the defaults database */
extern XFontStruct *DialogFont;
extern unsigned long DialogBorderColor;
extern ColorPair DialogC;
extern unsigned int text_border_width;

extern GC DialogGC, DialogRGC;

extern XContext ObjectContext, TextCursorContext;

extern Cursor IBeamCursor;

/***********************************************************************/
/**                                                                   **/
/**                            Text Items                             **/
/**                                                                   **/
/***********************************************************************/

static int max_line_size(s)
	char *s;
{
  char *bp;
  int w;
  Point sz;
  
  for (w = 0; *s != '\0'; s++) {
    for (bp = buf; *s != '\0' && *s != '\r' && *s != '\n'; s++, bp++)
      *bp = *s;
    *bp = '\0';
    sz = DialogStringSize(buf);
    w = max(w, sz.h);
    if (*s == '\0') break;
  }
  return(w);
}

static VOID draw_text(dpy, win, item, erase, move_cursor)
     Display *dpy;
     Window win;
     LVAL item;
     int erase, move_cursor;
{
  char *text, *bp;
  int x, y, len, ascent, font_height, editable, cx, cy;
  Window cursor;

  ascent = DialogFont->max_bounds.ascent;
  font_height = ascent + DialogFont->max_bounds.descent;

  if (erase) XClearWindow(dpy, win);

  text = checkstring(slot_value(item, s_text));
  editable = (slot_value(item, s_editable) != NIL) ? TRUE : FALSE;

  x = (editable) ? EDIT_TEXT_LEAD : 0;
  y = (editable) ? EDIT_TEXT_LEAD + ascent : STATIC_TEXT_LEAD + ascent;
  if (editable) {
    len = strlen(text);
    if (move_cursor) {
      cy = EDIT_TEXT_LEAD;
      cx = EDIT_TEXT_LEAD + XTextWidth(DialogFont, text, len);
      if (XFindContext(dpy, win, TextCursorContext, (caddr_t *) &cursor) != 0)
	xlfail("can't find text cursor context");
      XMoveWindow(dpy, cursor, cx, cy);
    }
    XDrawString(dpy, win, DialogGC, x, y, text, len);
  }
  else {
    for(; *text != '\0'; text++, y += font_height + STATIC_TEXT_LEAD) {
      for (bp = buf;
	   *text != '\0' && *text != '\r' && *text != '\n';
	   text++, bp++)
	*bp = *text;
      *bp = '\0';
      len = strlen(buf);
      XDrawString(dpy, win, DialogGC, x, y, buf, len);
    }
  }
}

#define bufsize 20

static int do_key(report, dpy, win, item)
     XEvent report;
     Display *dpy;
     Window win;
     LVAL item;
{
  char buffer[bufsize], *text;
  int count, len, erase = FALSE, result = FALSE;
  KeySym keysym;
  /*XComposeStatus compose;*/
  
  text = checkstring(slot_value(item, s_text));
  strcpy(buf, text);

  count = XLookupString((XKeyEvent *) &report, buffer, bufsize, &keysym,
			NULL /*&compose*/);
  if (keysym == XK_Return || keysym == XK_KP_Enter || keysym == XK_Linefeed) {
    result = TRUE;
/*    XBell(dpy, 100);*/
  }
  else if ((keysym >= XK_KP_Space && keysym <= XK_KP_9) 
	   || (keysym >= XK_space && keysym <= XK_asciitilde)) {
    if ((int) strlen(buf) + count >= STRMAX) XBell(dpy, 100);
    else strncat(buf, buffer, count);
  }
  else if (keysym >= XK_Shift_L && keysym <= XK_Hyper_R)
    ; /* do nothing because it is a modifier key */
  else if (keysym >= XK_F1 && keysym <= XK_F35) {
    if (count == 0) XBell(dpy, 100);
    else if ((int) strlen(buf) + count >= STRMAX) XBell(dpy, 100);
    else strncat(buf, buffer, count);
  }
  else if (keysym == XK_BackSpace || keysym == XK_Delete) {
    if ((len = strlen(buf)) > 0) {
      buf[len - 1] = '\0';
      erase = TRUE;
    }
    else XBell(dpy, 100);
  }
  else XBell(dpy, 100);

  set_slot_value(item, s_text, cvstring(buf));
  draw_text(dpy, win, item, erase, TRUE);
  return(result);
}

static LVAL text_handler(report, modal)
     XEvent report;
     int modal;
{
  Display *dpy = StX11Display();
  Window win, cursor;
  LVAL item, result = NIL;
  int editable;

  win = report.xany.window;
  item = StX11ItemObject(dpy, win);
  editable = (slot_value(item, s_editable) != NIL) ? TRUE : FALSE;
  if (item != NIL) {
    switch (report.type) {
    case Expose:
      draw_text(dpy, win, item, FALSE, TRUE);
      break;
    case KeyPress:
      if (editable && do_key(report, dpy, win, item) && modal)
	result = slot_value(slot_value(item, s_dialog), s_default_button);
      break;
    case ButtonPress:
      break;
    case ButtonRelease:
      break;
    case EnterNotify:
      if (editable && report.xcrossing.subwindow == None
	  && report.xcrossing.focus) {
	if (XFindContext(dpy, win, TextCursorContext, (caddr_t *) &cursor)
	    != 0)
	  xlfail("can't find text cursor context");
	XSetWindowBackground(dpy, cursor, DialogC.fore);
	XClearWindow(dpy, cursor);
	XSetWindowBorderWidth(dpy, win, text_border_width + 1);
      }
      break;
    case LeaveNotify:
      if (editable && report.xcrossing.subwindow == None
	  && report.xcrossing.focus) {
	if (XFindContext(dpy, win, TextCursorContext, (caddr_t *) &cursor)
	    != 0)
	  xlfail("can't find text cursor context");
	XSetWindowBackground(dpy, cursor, DialogC.back);
	XClearWindow(dpy, cursor);
	XSetWindowBorderWidth(dpy, win, text_border_width);
      }
      break;
    default: 
      break;
    }
  }
  return(result);
}

VOID InstallTextItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Point loc, size;
  Window field, cursor;
  int editable, ascent;
    
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  editable = (slot_value(item, s_editable) != NIL) ? TRUE : FALSE;
  field = XCreateSimpleWindow(dpy, win, loc.h, loc.v, size.h, size.v,
			      (editable) ? text_border_width : 0,
                               DialogBorderColor, DialogC.back);
  if (editable)
    XSelectInput(dpy, field,
		 ExposureMask | EnterWindowMask | LeaveWindowMask | 
		 KeyPressMask | ButtonPressMask | ButtonReleaseMask);
  else XSelectInput(dpy, field, ExposureMask);
  if (editable) {
    XDefineCursor(dpy, field, IBeamCursor);
    ascent = DialogFont->max_bounds.ascent;
    cursor = XCreateSimpleWindow(dpy, field, 
				 EDIT_TEXT_LEAD, EDIT_TEXT_LEAD,
				 ascent / 2, ascent,
				 text_border_width, 
				 DialogBorderColor, DialogC.back);
    if (XSaveContext(dpy, field, TextCursorContext, (caddr_t) cursor) != 0)
      xlfail("can't install cursor for text field");
    XMapWindow(dpy, cursor);
  }

  set_slot_value(item, s_window_id, cvfixnum((FIXTYPE) field));

  install_dialog_item_handler(dpy, field, text_handler, item);
  if (XSaveContext(dpy, field, ObjectContext, (caddr_t) item) != 0)
    xlfail("could not install object in window");
}

VOID DeleteTextItem(win, item)
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Window field;

  field = (Window) getfixnum(slot_value(item, s_window_id));

  delete_dialog_item_handler(dpy, field);
  if (XDeleteContext(dpy, field, ObjectContext) != 0)
    xlfail("could not delete object context");
  if (slot_value(item, s_editable) != NIL 
      && XDeleteContext(dpy, field, TextCursorContext) != 0)
    xlfail("could not delete cursor context");
  set_slot_value(item, s_window_id, NIL);
}

VOID DialogTextGetDefaultSize(item, width, height)
     LVAL item;
     int *width, *height;
{
  Point sz, em_sz;
  LVAL text = slot_value(item, s_text);
  LVAL text_length = slot_value(item, s_text_length);
  int w = 0;
  char *s;
  
  em_sz = DialogStringSize("M");
  sz.v = em_sz.v + 2 * STATIC_TEXT_LEAD;
  if (stringp(text)) {
    w = max_line_size(getstring(text));
    s = (char *) getstring(text);
    for (sz.v = em_sz.v + 2 * STATIC_TEXT_LEAD; *s != '\0'; s++)
      if (*s == '\n' || *s == '\r') sz.v += em_sz.v + STATIC_TEXT_LEAD;
  }
  if (fixp(text_length)) {
    w = max((int) (getfixnum(text_length) * em_sz.h), w);
  }
  if (slot_value(item, s_editable) != NIL) {
    if (width != NULL) *width = w + EDIT_TEXT_PAD;
    if (height != NULL) *height = em_sz.v + 2 * EDIT_TEXT_LEAD;
  }
  else {
    if (width != NULL) *width = w + STATIC_TEXT_PAD;
    if (height != NULL) *height = sz.v;
  }
}

LVAL DialogTextItemText(item, set, text)
     LVAL item;
     int set;
     char *text;
{
  Display *dpy = StX11Display();
  LVAL win_id;

  if (set) {
    set_slot_value(item, s_text, cvstring(text));
    win_id = slot_value(item, s_window_id);
    if (fixp(win_id)) 
      draw_text(dpy, (Window) getfixnum(win_id), item, TRUE, TRUE);
  }
  return(slot_value(item, s_text));
}

#ifdef TODO
mouse editing support
#endif /* TODO */
