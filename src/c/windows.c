/* windows - general window functions                                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL s_true, s_title, s_size, s_location, sk_allocate;

/**************************************************************************/
/**                                                                      **/
/**                           Utility Functions                          **/
/**                                                                      **/
/**************************************************************************/

VOID get_window_bounds P5C(LVAL, object, int *, left, int *, top, int *, width, int *, height)
{
  LVAL size, location;
  
  size = slot_value(object, s_size);
  location = slot_value(object, s_location);
  if (consp(size) && fixp(car(size)) && consp(cdr(size)) && fixp(car(cdr(size)))) {
    *width = getfixnum(car(size));
    *height = getfixnum(car(cdr(size)));
  }
  if (consp(location) && fixp(car(location)) 
      && consp(cdr(location)) && fixp(car(cdr(location)))) {
    *left = getfixnum(car(location));
    *top = getfixnum(car(cdr(location)));
  }
}

/***********************************************************************/
/**                                                                   **/
/**                  General Window Methods Functions                 **/
/**                                                                   **/
/***********************************************************************/

LVAL xsshowwindow(V)
{
  LVAL object = xlgaobject();
  IVIEW_WINDOW w = (IVIEW_WINDOW) GETWINDOWADDRESS(object);
  
  if (IVIEW_WINDOW_NULL(w)) {
    send_message(object, sk_allocate);
    w = (IVIEW_WINDOW) GETWINDOWADDRESS(object);
  }
  if (! IVIEW_WINDOW_NULL(w)) StShowWindow(w);
  return(NIL);
}

LVAL xshidewindow(V)
{
  LVAL object = xlgaobject();
  IVIEW_WINDOW w = (IVIEW_WINDOW) GETWINDOWADDRESS(object);
  
  if (! IVIEW_WINDOW_NULL(w)) StHideWindow(w);
  return(NIL);
}

LVAL xswindow_title(V)
{
  IVIEW_WINDOW w;
  LVAL object, title;
  char *str;
  
  object = xlgaobject();
  w = (IVIEW_WINDOW) GETWINDOWADDRESS(object);
  if (moreargs()) {
    title = xlgastring();
    set_slot_value(object, s_title, title);
    if (! IVIEW_WINDOW_NULL(w)) {
      str = (char *) getstring(title);
      StWSetTitle(w, str);
    }
  }
  return(slot_value(object, s_title));
}
  
static LVAL window_dimensions P2C(int, which, int, frame)
{
  LVAL object, slot;
  IVIEW_WINDOW w;
  int a, b, set = FALSE;
  
  object = xlgaobject();
  if (moreargs()) {
    set = TRUE;
    a = getfixnum(xlgafixnum());
    b = getfixnum(xlgafixnum());
  }
  xllastarg();
  
  w = (IVIEW_WINDOW) GETWINDOWADDRESS(object);
  slot = (which == 'L') ? s_location : s_size;
  
  if (set) {
    if (! frame) set_slot_value(object, slot, integer_list_2(a, b));
    if (! IVIEW_WINDOW_NULL(w)) {
      switch (which) {
      case 'L': StWSetLocation(w, a, b, frame); break;
      case 'S': StWSetSize(w, a, b, frame);     break;
      }
    }
  }
  if (! IVIEW_WINDOW_NULL(w)) {
    switch (which) {
    case 'L': 
      StWGetLocation(w, &a, &b, FALSE);
      set_slot_value(object, slot, integer_list_2(a, b));
      if (frame) StWGetLocation(w, &a, &b, TRUE);
      break;
    case 'S': 
      StWGetSize(w, &a, &b, FALSE);
      set_slot_value(object, slot, integer_list_2(a, b));
      if (frame) StWGetSize(w, &a, &b, TRUE);
      break;
    }
    return(integer_list_2(a, b));
  }
  else return(slot_value(object, slot));
}

LVAL xswindow_location(V)       { return(window_dimensions('L', FALSE)); }
LVAL xswindow_size(V)           { return(window_dimensions('S', FALSE)); }
LVAL xswindow_frame_location(V) { return(window_dimensions('L', TRUE));  }
LVAL xswindow_frame_size(V)     { return(window_dimensions('S', TRUE));  }

/**************************************************************************/
/**                                                                      **/
/**                         Screen Info Functions                        **/
/**                                                                      **/
/**************************************************************************/

LVAL xsscreen_size(V)
{
  int width, height;

  StGetScreenSize(&width, &height);
  return(integer_list_2(width, height));
}

LVAL xsscreen_has_color(V)
{
  return((StScreenHasColor()) ? s_true : NIL);
}

LVAL xssystem_has_windows(V)
{
  return((StHasWindows()) ? s_true : NIL);
}

LVAL xsflush_graphics(V)
{
  StFlushGraphics();
  return(NIL);
}
