/* X11resizebr - brush resizing dialog for X11                         */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
/***********************************************************************/
/**                                                                   **/
/**                    General Includes and Definitions               **/
/**                                                                   **/
/***********************************************************************/

#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

extern Display *StX11Display();

typedef struct {
  unsigned long fore, back;
} ColorPair;

/* forward declarations */
LOCAL VOID draw_dialog _((Window win));
LOCAL VOID draw_button _((Window win, char *text));
LOCAL VOID drag_brush _((XEvent report, int *pwidth, int *pheight));

/***********************************************************************/
/**                                                                   **/
/**                        Global Variables                           **/
/**                                                                   **/
/***********************************************************************/

#define WindowWidth 250
#define WindowHeight 200

static char ResizeMessage1[] = "To resize brush click in";
static char ResizeMessage2[] = "this window and drag";
static char OK_String[] = "OK";
static char CANCEL_String[] = "Cancel";

/* configuration parameters - should be set using the defaults database */
extern XFontStruct *DialogFont;
extern unsigned long DialogBorderColor, ButtonBorderColor;
extern ColorPair DialogC, ButtonC;
extern unsigned int dialog_border_width, button_border_width;
extern int min_button_height, min_button_width, dialog_item_gap;

extern GC DialogGC, ResizeGC;
extern Cursor ArrowCursor;

/***********************************************************************/
/**                                                                   **/
/**                    Resize Brush Dialog Function                   **/
/**                                                                   **/
/***********************************************************************/

int IViewGetNewBrushSize(w, new_width, new_height)
     IVIEW_WINDOW w;
     int *new_width, *new_height;
{
  Window win, ok_win, cancel_win;
  unsigned int width = WindowWidth, height = WindowHeight;
  int left, top;
  XSetWindowAttributes setwinattr;
  unsigned long valuemask;
  XEvent report;
  int done = FALSE;
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  int brush_width, brush_height;
  int cancelled = FALSE;
  
  /* create opaque dialog window */
  left = (DisplayWidth(dpy, screen) - WindowWidth) / 2;
  top = (DisplayHeight(dpy, screen) - WindowHeight) / 3;
  win = XCreateSimpleWindow(dpy, RootWindow(dpy, screen),
			    left, top, width, height, dialog_border_width,
			    DialogBorderColor, DialogC.back);

  /* set the override_redirect and save_under attributes for a dialog */
  valuemask = CWOverrideRedirect | CWSaveUnder;
  setwinattr.override_redirect = TRUE;
  setwinattr.save_under = TRUE;
  XChangeWindowAttributes(dpy, win, valuemask, &setwinattr);
  XSelectInput(dpy, win, ExposureMask | ButtonPressMask | ButtonMotionMask);

  /* make the buttons */
  ok_win = XCreateSimpleWindow(dpy, win, 
			       dialog_item_gap, 
			       WindowHeight - dialog_item_gap
			       - min_button_height,
			       min_button_width, min_button_height, 
			       button_border_width,
			       ButtonBorderColor, ButtonC.back);
  XSelectInput(dpy, ok_win,
	       ExposureMask | EnterWindowMask |
	       LeaveWindowMask | ButtonPressMask | ButtonReleaseMask);
  cancel_win = XCreateSimpleWindow(dpy, win, 
				   WindowWidth - min_button_width 
				   - dialog_item_gap, 
				   WindowHeight - dialog_item_gap
				   - min_button_height,
				   min_button_width, min_button_height, 
				   button_border_width,
				   ButtonBorderColor, ButtonC.back);
  XSelectInput(dpy, cancel_win,
	       ExposureMask | EnterWindowMask |
	       LeaveWindowMask | ButtonPressMask | ButtonReleaseMask);

  /* set the cursor */
  XDefineCursor(dpy, win, ArrowCursor);

  /* Display (map) the windows */
  XMapSubwindows(dpy, win);
  XMapWindow(dpy, win);

  /* grap the pointer */
  StX11ReleaseButton();
  XGrabPointer(dpy, win, TRUE, 
	       ButtonPressMask | ButtonReleaseMask | ButtonMotionMask,
	       GrabModeAsync, GrabModeAsync, None, None, CurrentTime);

  /* Loop until button is released, examining each event */
  brush_width = 0;
  brush_height = 0;
  while (! done) {
    XNextEvent(dpy, &report);
    switch (report.type) {
    case Expose:
      if (report.xexpose.window == win) {
	draw_dialog(win);
      }
      else if (report.xexpose.window == ok_win)
	draw_button(ok_win, OK_String);
      else if (report.xexpose.window == cancel_win)
	draw_button(cancel_win, CANCEL_String);
      else StProcessEvent(dpy, report);
      break;
    case ButtonPress:
      if (report.xbutton.window == win)
	drag_brush(report, &brush_width, &brush_height);
      break;
    case ButtonRelease:
      if (report.xbutton.window == ok_win 
	  || report.xbutton.window == cancel_win) {
	if (report.xbutton.window == cancel_win) cancelled = TRUE;
	done = TRUE;
      }
      break;
    case EnterNotify:
      if (report.xcrossing.window == ok_win)
	XSetWindowBorderWidth(dpy, ok_win, button_border_width + 1);
      else if (report.xcrossing.window == cancel_win)
	XSetWindowBorderWidth(dpy, cancel_win, button_border_width + 1);
      break;
    case LeaveNotify:
      if (report.xcrossing.window == ok_win)
	XSetWindowBorderWidth(dpy, ok_win, button_border_width);
      else if (report.xcrossing.window == cancel_win)
	XSetWindowBorderWidth(dpy, cancel_win, button_border_width);
      break;
    default:
      break;
    }
  }

  /* clean up */
  XDestroyWindow(dpy, win);
  XFlush(dpy);

  if (new_width != NULL) *new_width = brush_width;
  if (new_height != NULL) *new_height = brush_height;
  return(! cancelled);
}

LOCAL VOID draw_dialog(win) 
     Window win;
{
  int font_height, margin, x, y, len, text_width;
  GC gc;
  Display *dpy = StX11Display();
  char *text;

  font_height = DialogFont->max_bounds.ascent
              + DialogFont->max_bounds.descent;
  margin = DialogFont->max_bounds.descent / 2;

  gc = DialogGC;

  text = ResizeMessage1;
  len = strlen(text);
  text_width = XTextWidth(DialogFont, text, len);
  x = (WindowWidth - text_width) / 2;
  y = DialogFont->max_bounds.ascent + margin;
  XDrawString(dpy, win, gc, x, y, text, len);      

  text = ResizeMessage2;
  len = strlen(text);
  text_width = XTextWidth(DialogFont, text, len);
  x = (WindowWidth - text_width) / 2;
  y = 2 * y;
  XDrawString(dpy, win, gc, x, y, text, len);      
}

LOCAL VOID draw_button(win, text)
     Window win;
     char *text;
{
  int font_height, x, y, len, text_width;
  GC gc;
  Display *dpy = StX11Display();
  
  len = strlen(text);
  text_width = XTextWidth(DialogFont, text, len);

  font_height = DialogFont->max_bounds.ascent
              + DialogFont->max_bounds.descent;

  x = (min_button_width - text_width) / 2;
  y = DialogFont->max_bounds.ascent + (min_button_height - font_height) / 2; 
  
  gc = DialogGC;
  XDrawString(dpy, win, gc, x, y, text, len);    
}

LOCAL VOID drag_brush(report, pwidth, pheight)
     XEvent report;
     int *pwidth, *pheight;
{
  Window win = report.xbutton.window, child;
  Display *dpy = StX11Display();
  int done = FALSE, newx, newy;
  int xinit = report.xbutton.x, yinit = report.xbutton.y, x = xinit, y = yinit;
  GC gc = ResizeGC;
  
  XDrawRectangle(dpy, win, gc, xinit, yinit, 0, 0); /* to draw */

  /* Loop until button is released, examining each event */
  while (! done) {
    XNextEvent(dpy, &report);
    switch (report.type) {
    case MotionNotify:
      if (win == report.xmotion.window) {
	newx = report.xmotion.x;
	newy = report.xmotion.y;
      }
      else {
	XTranslateCoordinates(dpy, report.xmotion.window, win,
			      report.xmotion.x, report.xmotion.y, &newx, &newy,
			      &child);
      }
      XDrawRectangle(dpy, win, gc, 
		     (xinit < x) ? xinit : x,
		     (yinit < y) ? yinit : y, 
		     (xinit < x) ? x - xinit : xinit - x, 
		     (yinit < y) ? y - yinit : yinit - y); /* to erase */
      x = newx;
      y = newy;
      XDrawRectangle(dpy, win, gc, 
		     (xinit < x) ? xinit : x,
		     (yinit < y) ? yinit : y, 
		     (xinit < x) ? x - xinit : xinit - x, 
		     (yinit < y) ? y - yinit : yinit - y); /* to draw */
      break;
    case ButtonRelease:
      done = TRUE;
      break;
    default:
      break;
    }
  }

  XDrawRectangle(dpy, win, gc, 
		 (xinit < x) ? xinit : x,
		 (yinit < y) ? yinit : y, 
		 (xinit < x) ? x - xinit : xinit - x, 
		 (yinit < y) ? y - yinit : yinit - y); /* to erase */

  *pwidth = (x < xinit) ? xinit - x : x - xinit;
  *pheight = (y < yinit) ? yinit - y : y - yinit;
}

#ifdef TODO
try to leave brush up after it has been set
use dashed lines
#endif /* TODO */
