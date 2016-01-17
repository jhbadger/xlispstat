/* X11graph - X11 support for XLISP-STAT                               */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"
#include "StX11options.h"

#define NullWindow ((Window) 0)
#define NullPixmap ((Pixmap) 0)
#define NullDrawable ((Drawable) 0)
#define NullFile ((FILEP) 0)

extern VOID ShowScrollBar P5H(Window, int, int, int, int);
extern VOID HideScrollBar P1H(Window);
extern VOID psputinit P4H(FILEP, int, int, double);
extern VOID psputbit P1H(int);
extern VOID psputrest(V);
extern VOID InstallMenuButton P2H(Window, LVAL);
extern VOID DeleteMenuButton P1H(Window);
extern VOID StX11FinishMenus(V);
extern VOID StX11FinishDialogs(V);
extern StX11InitMenus(V);
extern StX11InitDialogs(V);

extern char *getenv();

extern char *progname, buf[];
extern LVAL s_true, sk_close, s_go_away, s_event_queue, sk_fast_lines,
  sk_fast_symbols, sk_motion_sync, sk_do_clipping, sk_use_icccm,
  sk_wait_for_map;

/* forward declarations */
LOCAL int x_error_handler _((Display *dpy, XErrorEvent *err));
LOCAL VOID MakeCursors _((void));
LOCAL VOID upcase _((char *str));
LOCAL VOID SetWRefCon _((Window w, long rc));
LOCAL long GetWRefCon _((Window w));
LOCAL unsigned long xor_color _((int index));
LOCAL LVAL window_object _((Display *dpy, Window w));
LOCAL VOID get_panel_size P5H(StGWWinInfo *, int, int, int *, int *);
LOCAL VOID make_new_gc _((StGWWinInfo *gwinfo));
LOCAL VOID do_key _((XEvent report, Display *dpy, Window win, LVAL object));
LOCAL VOID do_motion _((XEvent report, Display *dpy, Window win, LVAL object));
LOCAL VOID do_button _((XEvent report, Display *dpy, Window win, LVAL object));
LOCAL LVAL frame_handler _((XEvent report, int modal));
LOCAL LVAL panel_handler _((XEvent report, int modal));
LOCAL Drawable get_drawable _((StGWWinInfo *gwinfo));
LOCAL VOID fill_rect P6H(StGWWinInfo *, int, int, int, int, int);
LOCAL VOID draw_arc P7H(StGWWinInfo *, int, int, int, int, int, int);
LOCAL VOID fill_arc _((StGWWinInfo *gwinfo,
		       int left, int top, int width, int height,
		       int angle1, int angle2, int which));
LOCAL VOID fill_poly _((StGWWinInfo *gwinfo, int n, short *p,
			int from_origin, int which));
LOCAL VOID draw_char_up _((StGWWinInfo *gwinfo, char *s, int x, int y));
LOCAL VOID MakeSymbols _((void));
LOCAL VOID FreeSymbols _((void));
#ifdef DRAWPOINTSBUG
LOCAL VOID set_tmpsym _((XPoint *sym, int n, int x, int y));
#endif /* DRAWPOINTSBUG */
LOCAL VOID cheat_syms _((Display *dpy, StGWWinInfo *gwinfo, unsigned sym,
			 int x, int y));
LOCAL VOID MakeWorkPort _((void));
LOCAL VOID allocate_named_color _((Display *dpy, int screen, int index,
				   char *name));
LOCAL VOID MakeColors _((void));
#ifndef SERVER_COLOR_FREE_PROBLEM
LOCAL VOID FreeColors _((void));
#endif /* SERVER_COLOR_FREE_PROBLEM */
LOCAL unsigned long get_color _((ColorCode index));
LOCAL VOID MakeGraphCursors _((void));
LOCAL VOID FreeGraphCursors _((void));
LOCAL Pixmap make_pixmap _((int width, int height, char *image));
LOCAL VOID set_gc_clip_regions _((StGWWinInfo *gwinfo));
LOCAL int is_allocated_graph _((LVAL object));
LOCAL LVAL entry_object _((LVAL entry));
LOCAL VOID do_idle_actions _((void));
LOCAL VOID background_tasks _((void));
LOCAL int any_idle_tasks _((void));
LOCAL VOID StX11ObScrollAction _((LVAL object, Window s,
				  int which, int x, int y));
LOCAL VOID adjust_scroll_bars _((StGWWinInfo *gwinfo, int adjust_showing));
LOCAL VOID set_has_scroll _((StGWWinInfo *gwinfo,
			     int which, int has, int size));
LOCAL VOID StX11FinishGW(V);
LOCAL VOID StX11InitGW(V);


/**************************************************************************/
/**                                                                      **/
/**                            Global Variables                          **/
/**                                                                      **/
/**************************************************************************/

/* Global display and screen  variables for use in Xlib calls */
static struct {
  Display *dpy;
  int screen;
  int has_windows;
  XrmDatabase db;
} StX11Globals;

XContext EventContext, ObjectContext, CloseContext, MenuContext, 
  Button1Context, Button2Context, ThumbContext, TextCursorContext,
  WRefConContext, ListFieldContext, ScrollActionContext;
Cursor MenuCursor, ArrowCursor, IBeamCursor;

static int pointer_button_down = FALSE;

/**************************************************************************/
/**                                                                      **/
/**                       Globals Access Functions                       **/
/**                                                                      **/
/**************************************************************************/

Display *StX11Display() { return(StX11Globals.dpy); }
int StX11Screen() { return(StX11Globals.screen); }

/**************************************************************************/
/**                                                                      **/
/**                       Initialization Functions                       **/
/**                                                                      **/
/**************************************************************************/

LOCAL int x_error_handler(dpy, err)
     Display *dpy;
     XErrorEvent *err;
{
  char msg[80];

  XGetErrorText(dpy, err->error_code, msg, 80);
  fprintf(stderr, "X error code %s\n", msg);
  xlfail("You may want to save what you can and exit.");
  /* not reached */
  return(0);
}

VOID StInitGraphics()
{
  char *display_name = NULL;
  XrmDatabase db, tmpdb;
  char *xdefs, *xdefdir;

  /* connect to X server */
  if ((StX11Globals.dpy = XOpenDisplay(display_name)) == NULL) {
    fprintf(stderr, "%s: can't connect to X server %s\n", 
	    progname, XDisplayName(display_name));
    StX11Globals.has_windows = FALSE;
    return;
  }
  else StX11Globals.has_windows = TRUE;

  XrmInitialize();

  /* read standard application default database */
  db = XrmGetFileDatabase("/usr/lib/X11/app-defaults/Xlisp");

  /* read application default database from directory given in environment */
  xdefdir = getenv("XAPPLRESDIR");
  if (xdefdir != NULL) {
    sprintf(buf, "%s/%s", xdefdir, "Xlisp");
    tmpdb = XrmGetFileDatabase(buf);
    if (tmpdb != NULL) {
      if (db != NULL)
	XrmMergeDatabases(tmpdb, &db);
      else
	db = tmpdb;
    }
  }

  /* read display's resources */
#ifdef PBaseSize /* Cheap way to test for X11R4 or later -- from emacs. */
  xdefs = XResourceManagerString(StX11Globals.dpy);
#else
  xdefs = StX11Globals.dpy->xdefaults;
#endif
  if (xdefs != NULL) {
    tmpdb = XrmGetStringDatabase(xdefs);
    if (tmpdb != NULL) {
      if (db != NULL)
	XrmMergeDatabases(tmpdb, &db);
      else
	db = tmpdb;
    }
  }
  StX11Globals.db = db;

  /* get context ids for objects and callbacks */
  ObjectContext =  XUniqueContext();
  EventContext = XUniqueContext();
  CloseContext = XUniqueContext();
  MenuContext = XUniqueContext();
  Button1Context = XUniqueContext();
  Button2Context = XUniqueContext();
  ThumbContext = XUniqueContext();
  TextCursorContext = XUniqueContext();
  WRefConContext = XUniqueContext();
  ListFieldContext = XUniqueContext();
  ScrollActionContext = XUniqueContext();

  /* get the default screen and the font to use */
  StX11Globals.screen = DefaultScreen(StX11Globals.dpy);
  
  /* initialize other modules */
  StX11InitMenus();
  StX11InitDialogs();
  StX11InitGW();

  XSetErrorHandler (x_error_handler);

  MakeCursors();
}

LOCAL VOID MakeCursors()
{
  Display *dpy = StX11Display();

  ArrowCursor = XCreateFontCursor(dpy, XC_left_ptr);
  IBeamCursor = XCreateFontCursor(dpy, XC_xterm);
}

VOID StX11Finish() 
{ 
  Display *dpy = StX11Display();

  if (StHasWindows()) {
    XFreeCursor(dpy, ArrowCursor);
    XFreeCursor(dpy, IBeamCursor);
    
    StX11FinishMenus();
    StX11FinishDialogs();
    StX11FinishGW();
    XCloseDisplay(dpy); 
  }
}

LOCAL VOID upcase(str)
     char *str;
{
  for (; *str != '\0'; ++str)
    if (islower(*str))
      *str = toupper(*str);
}

int is_option_on(s)
     char *s;
{
  strcpy(buf, s);
  upcase(buf);
  return((strcmp(buf, "ON")==0) || (strcmp(buf,"YES")==0)
	 || (strcmp(buf,"1")==0) || (strcmp(buf,"TRUE") == 0));
}

/**************************************************************************/
/**                                                                      **/
/**                       Event Polling Functions                        **/
/**                                                                      **/
/**************************************************************************/

VOID StPollEvent()
{
  Display *dpy = StX11Display();
  XEvent report;
  int (*callback)();
  
  if (StHasWindows() && XEventsQueued(dpy, QueuedAfterFlush) > 0) {
    XNextEvent(dpy, &report);
    if (XFindContext(dpy, report.xany.window, EventContext,
		     (caddr_t *) &callback) == 0
	&& callback != NULL)
      (*callback)(report, FALSE);
    if (report.type == MappingNotify)
      XRefreshKeyboardMapping((XMappingEvent *) &report);
  }
  do_idle_actions();
}

VOID StProcessEvent(dpy, report)
     Display *dpy;
     XEvent report;
{
  int (*callback)();
  
  if (XFindContext(dpy, report.xany.window, EventContext,
		   (caddr_t *) &callback) == 0
      && callback != NULL)
    (*callback)(report, FALSE);
  if (report.type == MappingNotify)
    XRefreshKeyboardMapping((XMappingEvent *) &report);
}

/* If no events are pending this function enters a select that blocks  */
/* until X input is available or new input occurs on stdin. It returns */
/* TRUE if the block is broken by character input to sdtin, otherwise  */
/* it returns FALSE.                                                   */
/*                                                                     */
/* It is not clear how well this will work on non BSD systems.         */

int StBlockForInput()
{
  Display *dpy = StX11Display();
  fd_set readmask;
  int result, maxfd;

  if (! any_idle_tasks()
      && (! StHasWindows() || XEventsQueued(dpy, QueuedAfterFlush) == 0)) {
    FD_ZERO(&readmask);
    FD_SET(fileno(stdin), &readmask);
    maxfd = fileno(stdin);
    if (StHasWindows()) {
      int tmp = ConnectionNumber(dpy);
      if (tmp > maxfd) maxfd = tmp;
      FD_SET(maxfd, &readmask);
    }
    result = select(maxfd+1, (int *) &readmask, NULL, NULL, NULL);
    return((FD_ISSET(fileno(stdin), &readmask)) ? TRUE : FALSE);
  }
  else return(FALSE);
}

/**************************************************************************/
/**                                                                      **/
/**                    Screen Information Functions                      **/
/**                                                                      **/
/**************************************************************************/

int StScreenHasColor()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();

  /* 
   * This only checks for the depth; it does not distinguish between
   * gray-scale monitors and true color. It is not clear whether
   * this is the right decision or not. The usual hack of looking at
   * the visual class seems kind of gross.
   */
  if (StHasWindows()) return((DefaultDepth(dpy, screen) > 1) ? TRUE : FALSE);
  else return(FALSE);
}

VOID StGetScreenSize(width, height)
     int *width, *height;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();

  if (StHasWindows()) {
    if (width != NULL) *width = DisplayWidth(dpy, screen);
    if (height != NULL) *height = DisplayHeight(dpy, screen);
  }
  else {
    if (width != NULL) *width = 0;
    if (height != NULL) *height = 0;
  }
}

int StHasWindows(V) { return (StX11Globals.has_windows); }

VOID StFlushGraphics()
{
  Display *dpy = StX11Display();
  if (StHasWindows())
    XFlush(dpy);
}

/**************************************************************************/
/**                                                                      **/
/**                       Simple Window Functions                        **/
/**                                                                      **/
/**************************************************************************/

VOID StWSetTitle(win, title)
     Window win;
     char *title;
{
  Display *dpy = StX11Display();

  XStoreName(dpy, win, title);
}

VOID StHideWindow(win)
     Window win;
{
  Display *dpy = StX11Display();

  XUnmapWindow(dpy, win);
}

VOID StShowWindow(win)
     Window win;
{
  Display *dpy = StX11Display();

  /* XMapSubwindows(dpy, win); */
  XMapRaised(dpy, win);
  XFlush(dpy);
}

LOCAL VOID SetWRefCon(w, rc)
     Window w;
     long rc;
{
  Display *dpy = StX11Display();

  if (XSaveContext(dpy, w, WRefConContext, (caddr_t) rc) != 0)
    xlfail("could not install WRefCon");
}

LOCAL long GetWRefCon(w)
     Window w;
{
  Display *dpy = StX11Display();
  long rc;

  if (XFindContext(dpy, w, WRefConContext, (caddr_t *) &rc) == 0) return(rc);
  else return 0;
}

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                        Graph Window Functions                        **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

/**************************************************************************/
/**                                                                      **/
/**                         Graph Window Globls                          **/
/**                                                                      **/
/**************************************************************************/

static int use_fast_lines, use_fast_symbols, motion_sync, do_clipping,
  use_icccm, wait_for_map;

static unsigned int gw_border_width;
static unsigned long GWBorderColor;

static char *GraphFontName = "9x15";
static XFontStruct *GraphFont;

# define ScrollWidth 20

static int buffering = FALSE;
static int bufflevel = 0;
static Pixmap WorkPort, CharPM;
static GC CharGC, CharEraseGC;
static GC copy_gc;   /* used to deal with the cfb clipping bug in R3 */
GC ResizeGC;

static unsigned long get_color();

extern LVAL slot_value(), mklist();
extern LVAL s_true;

static Atom wm_delete_window, wm_protocols;

/**************************************************************************/
/**                                                                      **/
/**               Graph Window Initialization and Cleanup                **/
/**                                                                      **/
/**************************************************************************/

/* 
   This is a hack to figure out what color to use for xor drawing. The
   idea is to check where white and black differ and look at the result
   of xor'ing black on white. If the result is not white, black should
   work as a drawing color in xor mode. Otherwise use white.
*/

LOCAL unsigned long xor_color(index)
     int index;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  unsigned long mask, white, black;

  black = BlackPixel(dpy, screen);
  white = WhitePixel(dpy, screen);
  mask = black ^ white;  /* mask where black and white differ */
  
  if ((white ^ black) != (white & mask)) return(black);
  else return(white);
}

char *StX11GetDefault(name)
     char *name;
{
  XrmValue v;
  char *type;

  if (StX11Globals.db != NULL) {
    XrmGetResource(StX11Globals.db, name, name, &type, &v);
    if (type != NULL && ! strcmp(type, "String"))
      return((char *) v.addr);
  }
  return(NULL);
}
  
LOCAL VOID StX11InitGW(V)
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  XGCValues values;
  unsigned long valuemask;
  int font_height, font_width;
  char *font;
  char *option;

  MakeColors();

  gw_border_width = 1;
  GWBorderColor = BlackPixel(dpy, screen);

  /**** fast line drawing - use line width 0 for speed. ****/
  option = StX11GetDefault("xlisp.graph.fastlines");
  if (option == NULL) use_fast_lines = USE_FAST_LINES_DEFAULT;
  else use_fast_lines = is_option_on(option);

  /**** fast symbols - draw them with DrawPoints instead of as Pixmaps ****/
  option = StX11GetDefault("xlisp.graph.fastsymbols");
  if (option == NULL) use_fast_symbols = USE_FAST_SYMBOLS_DEFAULT;
  else use_fast_symbols = is_option_on(option);

  /**** use XSync calls within mouse motion events (no button) ****/
  option = StX11GetDefault("xlisp.graph.motionsync");
  if (option == NULL) motion_sync = MOTION_SYNC_DEFAULT;
  else motion_sync = is_option_on(option);

  /**** use clipping calls (don't work on pmax?) ****/
  option = StX11GetDefault("xlisp.graph.doclipping");
  if (option == NULL) do_clipping = DO_CLIPPING_DEFAULT;
  else do_clipping = is_option_on(option);

  /**** try to be more ICCCM-compliant - may help in OpenLook ****/
  option = StX11GetDefault("xlisp.graph.icccm");
  if (option == NULL) use_icccm = USE_ICCCM_DEFAULT;
  else use_icccm = is_option_on(option);

  /**** wait for MapNotify event in initial draw - may help or hurt ****/
  option = StX11GetDefault("xlisp.graph.waitformap");
  if (option == NULL) wait_for_map = WAIT_FOR_MAP_DEFAULT;
  else wait_for_map = is_option_on(option);

  font = (char *) StX11GetDefault("xlisp.graph.font");
  if (font == NULL) font = (char *) StX11GetDefault("xlisp.font");
  if (font == NULL) font = GraphFontName;
  if ((GraphFont = XLoadQueryFont(dpy, font)) == NULL) {
    fprintf(stderr, "xlisp: Can't open %s font\n", font);
    if ((GraphFont = XLoadQueryFont(dpy, GraphFontName)) == NULL) {
      fprintf(stderr, "xlisp: Can't open %s font\n", GraphFontName);
      if ((GraphFont = XLoadQueryFont(dpy, "fixed")) == NULL) {
	fprintf(stderr, "xlisp: Can't open %s font\n", "fixed");
	exit(-1);
      }
    }
  }
  valuemask = 0; /* ignore XGCValues and use defaults */
  CharGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, CharGC, GraphFont->fid);
  XSetForeground(dpy, CharGC, BlackPixel(dpy, screen));
  XSetBackground(dpy, CharGC, WhitePixel(dpy, screen));
  CharEraseGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, CharEraseGC, GraphFont->fid);
  XSetForeground(dpy, CharEraseGC, WhitePixel(dpy, screen));
  XSetBackground(dpy, CharEraseGC, WhitePixel(dpy, screen));
  valuemask = 0;
  values.function = GXxor;
  ResizeGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFunction(dpy, ResizeGC, GXxor);
  XSetForeground(dpy, ResizeGC, xor_color(1));
  XSetBackground(dpy, ResizeGC, WhitePixel(dpy, screen));


  font_height = GraphFont->max_bounds.ascent + GraphFont->max_bounds.descent;
  font_width = GraphFont->max_bounds.width;
  CharPM = XCreatePixmap(dpy, RootWindow(dpy, screen),
			 font_width, font_height, DefaultDepth(dpy, screen));

  /* the following is used to deal with the cfb clipping bug in R3 */
  valuemask = 0; /* ignore XGCValues and use defaults */
  copy_gc = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);

  MakeWorkPort();
  MakeSymbols();
  MakeGraphCursors();

  wm_protocols = XInternAtom (dpy, "WM_PROTOCOLS", FALSE);
  wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", FALSE);
}

LOCAL VOID StX11FinishGW(V)
{
  Display *dpy = StX11Display();

  XUnloadFont(dpy, GraphFont->fid);
#ifndef SERVER_COLOR_FREE_PROBLEM
  FreeColors();
#endif /* SERVER_COLOR_FREE_PROBLEM */
  XFreeGC(dpy, CharGC);
  XFreeGC(dpy, CharEraseGC);
  XFreeGC(dpy, ResizeGC);
  XFreePixmap(dpy, CharPM);
  FreeSymbols();
  FreeGraphCursors();
}

LVAL xsx11_options()
{
  LVAL arg, result, next;

  if (xlgetkeyarg(sk_fast_lines, &arg))
    use_fast_lines = (arg != NIL) ? TRUE : FALSE;
  if (xlgetkeyarg(sk_fast_symbols, &arg))
    use_fast_symbols = (arg != NIL) ? TRUE : FALSE;
  if (xlgetkeyarg(sk_motion_sync, &arg))
    motion_sync = (arg != NIL) ? TRUE : FALSE;
  if (xlgetkeyarg(sk_do_clipping, &arg))
    do_clipping = (arg != NIL) ? TRUE : FALSE;
  if (xlgetkeyarg(sk_use_icccm, &arg))
    use_icccm = (arg != NIL) ? TRUE : FALSE;
  if (xlgetkeyarg(sk_wait_for_map, &arg))
    wait_for_map = (arg != NIL) ? TRUE : FALSE;

  result = mklist(12, NIL);
  next = result;
  rplaca(next, sk_fast_lines); next = cdr(next);
  rplaca(next, (use_fast_lines) ? s_true : NIL); next = cdr(next);
  rplaca(next, sk_fast_symbols); next = cdr(next);
  rplaca(next, (use_fast_symbols) ? s_true : NIL); next = cdr(next);
  rplaca(next, sk_motion_sync); next = cdr(next);
  rplaca(next, (motion_sync) ? s_true : NIL); next = cdr(next);
  rplaca(next, sk_do_clipping); next = cdr(next);
  rplaca(next, (do_clipping) ? s_true : NIL); next = cdr(next);
  rplaca(next, sk_use_icccm); next = cdr(next);
  rplaca(next, (use_icccm) ? s_true : NIL); next = cdr(next);
  rplaca(next, sk_wait_for_map); next = cdr(next);
  rplaca(next, (wait_for_map) ? s_true : NIL); next = cdr(next);

  return(result);
}

int StX11UseICCCM() { return(use_icccm); }

/***********************************************************************/
/**                                                                   **/
/**             Constructing and Removing Graph Windows               **/
/**                                                                   **/
/***********************************************************************/

LOCAL LVAL window_object(dpy, w)
     Display *dpy;
     Window w;
{
  LVAL object;

  if (XFindContext(dpy, w, ObjectContext, (caddr_t *) &object) == 0 
      && objectp(object))
    return(object);
  else return(NIL);
}

LOCAL VOID get_panel_size(gwinfo, width, height, pwidth, pheight)
     StGWWinInfo *gwinfo;
     int width, height, *pwidth, *pheight;
{
  height -= ClosePanelHeight();
  if (gwinfo->hasHscroll) height -= ScrollWidth;
  if (gwinfo->hasVscroll) width -= ScrollWidth;
  if (pwidth != NULL) *pwidth = width;
  if (pheight != NULL) *pheight = height;
}

LOCAL VOID make_new_gc(gwinfo)
     StGWWinInfo *gwinfo;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  unsigned long valuemask;
  XGCValues values;

  valuemask = GCFunction | GCLineWidth | GCLineStyle;
  values.function = GXcopy;
  if (gwinfo->lineWidth == 1 && use_fast_lines) values.line_width = 0;
  else values.line_width = gwinfo->lineWidth;
  values.line_style = (gwinfo->lineType == 0) ? LineSolid : LineOnOffDash;

  gwinfo->gc = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, gwinfo->gc, GraphFont->fid);
  XSetForeground(dpy, gwinfo->gc, get_color(gwinfo->drawColor));
  XSetBackground(dpy, gwinfo->gc, get_color(gwinfo->backColor));

  valuemask = 0; /* ignore XGCValues and use defaults */
  gwinfo->erase_gc = XCreateGC(dpy, RootWindow(dpy, screen), 
			       valuemask, &values);
  XSetFont(dpy, gwinfo->erase_gc, GraphFont->fid);
  XSetForeground(dpy, gwinfo->erase_gc, get_color(gwinfo->backColor));
  XSetBackground(dpy, gwinfo->erase_gc, get_color(gwinfo->backColor));

  valuemask = GCFunction | GCLineWidth | GCLineStyle;
  values.function = GXxor;
  gwinfo->xor_gc = XCreateGC(dpy, RootWindow(dpy, screen), 
			       valuemask, &values);
  XSetFont(dpy, gwinfo->xor_gc, GraphFont->fid);
  XSetForeground(dpy, gwinfo->xor_gc, xor_color(gwinfo->backColor));
  XSetBackground(dpy, gwinfo->xor_gc, get_color(gwinfo->backColor));

  set_gc_clip_regions(gwinfo);
}

#define bufsize 20
#define NO_CHARS 0
#define IN_KEY 1
#define IN_BUFFER 2

LOCAL VOID do_key(report, dpy, win, object)
     XEvent report;
     Display *dpy;
     Window win;
     LVAL object;
{
  char buffer[bufsize];
  KeySym keysym;
  int count, result = NO_CHARS;
  /*XComposeStatus compose;*/
  char key = 0;
  int shift, opt, i;

  count = XLookupString((XKeyEvent *) &report, buffer, bufsize,
			&keysym, NULL /*&compose*/);
  if (keysym == XK_Return || keysym == XK_KP_Enter || keysym == XK_Linefeed) {
    result = IN_KEY;
    key = '\n';
  }
  else if ((keysym >= XK_KP_Space && keysym <= XK_KP_9) 
	   || (keysym >= XK_space && keysym <= XK_asciitilde)) {
    result = IN_BUFFER;
  }
  else if (keysym >= XK_Shift_L && keysym <= XK_Hyper_R)
    ; /* do nothing because it is a modifier key */
  else if (keysym >= XK_F1 && keysym <= XK_F35) {
    result = IN_BUFFER;
  }
  else if (keysym == XK_BackSpace || keysym == XK_Delete) {
    result = IN_KEY;
    key = '\b';
  }
  else XBell(dpy, 100);

  if (result != NO_CHARS) {
    shift = report.xkey.state & ShiftMask;
    opt = report.xkey.state & ControlMask;

    if (result == IN_KEY) {
      buffer[0] = key;
      count = 1;
    }
    for (i = 0; i < count; i++)  StGWObDoKey(object, buffer[i], shift, opt);
  }
}

LOCAL VOID do_motion(report, dpy, win, object)
     XEvent report;
     Display *dpy;
     Window win;
     LVAL object;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
  int x, y;

  if (XCheckWindowEvent(dpy, win, ButtonPressMask, &report)) {
    do_button(report, dpy, win, object);
  }
  else {
    /* compress motion events */
    do {
      x = report.xmotion.x + gwinfo->view_h;
      y = report.xmotion.y + gwinfo->view_v;
    } while (XCheckMaskEvent(dpy, PointerMotionMask, &report));

    /* only act if pointer has really moved */
    if (x != gwinfo->mouse_x || y != gwinfo->mouse_y) {
      gwinfo->mouse_x = x;
      gwinfo->mouse_y = y;
      StGWObDoMouse(object, x, y, MouseMove, (MouseClickModifier) 0);
      if (motion_sync) XSync(dpy, FALSE);
      else XFlush(dpy);
    }
  }
}

LOCAL VOID do_button(report, dpy, win, object)
     XEvent report;
     Display *dpy;
     Window win;
     LVAL object;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
  int x, y;
  int extend, option, mods;

  extend = ShiftMask & report.xbutton.state;
  option = ControlMask & report.xbutton.state;
  mods = (int) ((extend) ? ExtendModifier : NoModifiers);
  if (option) mods += 2;
  x = report.xbutton.x + gwinfo->view_h;
  y = report.xbutton.y + gwinfo->view_v;
  gwinfo->mouse_x = x;
  gwinfo->mouse_y = y;
  StX11PressButton();
  StGWObDoMouse(object, x, y, MouseClick, (MouseClickModifier) mods);
  StX11ReleaseButton();
  XSync(dpy, FALSE);
}

LOCAL LVAL frame_handler(report, modal)
     XEvent report;
     int modal;
{
  Display *dpy = StX11Display();
  StGWWinInfo *gwinfo;
  LVAL object;
  int width, height;

  if (modal) return(NIL);

  switch(report.type) {
  case ConfigureNotify:
    object = window_object(dpy, report.xany.window);
    if (objectp(object) 
	&& (gwinfo = (StGWWinInfo *) StGWObWinInfo(object)) != NULL
	&&(gwinfo->frame_width != report.xconfigure.width
	   || gwinfo->frame_height != report.xconfigure.height)) {
      gwinfo->frame_width = report.xconfigure.width;
      gwinfo->frame_height = report.xconfigure.height;
      get_panel_size(gwinfo, gwinfo->frame_width, gwinfo->frame_height,
		     &width, &height);
      XResizeWindow(dpy, gwinfo->panel, width, height);
      XMapWindow(dpy, gwinfo->panel);  
      if (! gwinfo->hasVscroll) gwinfo->canvasHeight = height;
      if (! gwinfo->hasHscroll) gwinfo->canvasWidth = width;
      adjust_scroll_bars(gwinfo, TRUE);
      gwinfo->initialized = TRUE;
      StGWObResize(object);
    }
    break;
  case Expose:  /* added to remap panel after resize on sun X11 (I hope) */
    object = window_object(dpy, report.xany.window);
    if (objectp(object) 
	&& (gwinfo = (StGWWinInfo *) StGWObWinInfo(object)) != NULL) {
      XMapWindow(dpy, gwinfo->panel);  
    }
    break;
  case ClientMessage:
    StX11HandleClientMessage(report);
    break;
  default:
    break;
  }
  return(NIL);
}

LOCAL LVAL panel_handler(report, modal)
     XEvent report;
     int modal;
{
  Display *dpy = StX11Display();
  LVAL object;
  Window win;

  if (modal) return(NIL);

  win = report.xany.window;
  object = window_object(dpy, win);
  if (objectp(object)) {
    switch(report.type) {
    case Expose:
      if (report.xexpose.count == 0) StGWObRedraw(object);
      break;
    case KeyPress:    
      do_key(report, dpy, win, object);
      break;
    case ButtonPress:
      do_button(report, dpy, win, object);
      break;
    case MotionNotify:
      do_motion(report, dpy, win, object);
      break;
    default:
      break;
    }
  }
  return(NIL);
}

VOID StX11HandleClientMessage(report)
     XEvent report;
{
  Display *dpy = StX11Display();
  LVAL object;

  /*
   * This sends the :close message to windows with a go_away in response
   * to a window manager delete mindow action (e.g. choosing the Quit
   * item from an olwm frame menu).
   */
  if (report.xclient.data.l[0] == wm_delete_window) {
    object = window_object(dpy, report.xany.window);
    if (objectp(object) && slot_value(object, s_go_away) != NIL)
      send_message(object, sk_close);
    else SysBeep(10);
  }
}

int StGWWinInfoSize() { return(sizeof(StGWWinInfo)); }

VOID StGWInitWinInfo(object)
     LVAL object;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) StGWObWinInfo((LVAL) object);

  gwinfo->Object = object;
  gwinfo->idleOn = FALSE;
  gwinfo->window = NullWindow;
  gwinfo->panel = NullWindow;
  gwinfo->FreeMem = NULL;
  gwinfo->backColor = 0;
  gwinfo->drawColor = 1;
  gwinfo->canvasWidth = 0;
  gwinfo->canvasHeight = 0;
  gwinfo->lineType = 0;
  gwinfo->drawMode = 0;
  gwinfo->lineWidth = 1;
  gwinfo->RefCon = 0L;
  gwinfo->use_color = FALSE;
  gwinfo->hasHscroll = FALSE;
  gwinfo->hasVscroll = FALSE;
  gwinfo->view_h = 0;
  gwinfo->view_v = 0;
  gwinfo->v_scroll_inc[0] = 1; gwinfo->v_scroll_inc[1] = 50;
  gwinfo->h_scroll_inc[0] = 1; gwinfo->h_scroll_inc[1] = 50;
  gwinfo->hscroll = NullWindow;
  gwinfo->vscroll = NullWindow;
  gwinfo->cursor = 0;
  gwinfo->clipped = FALSE;
  gwinfo->clip_left = 0;
  gwinfo->clip_top = 0;
  gwinfo->clip_width = 0;
  gwinfo->clip_height = 0;
  gwinfo->frame_width = -1;
  gwinfo->frame_height = -1;
  gwinfo->gc = NULL;
  gwinfo->erase_gc = NULL;
  gwinfo->xor_gc = NULL;
  gwinfo->initialized = FALSE;
  gwinfo->go_away = TRUE;
  gwinfo->has_menu_button = TRUE;
}

Window IViewWindowNew(object, is_GW)
     LVAL object;
     int is_GW;
{
  char *title;
  int left, top, width, height, go_away, w_height, w_width;
  StGWWinInfo *gwinfo;
  Window win;
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  XSetWindowAttributes winattr;

  StGWGetAllocInfo(object, &title, &left, &top, &width, &height, &go_away);
  if (title == (char *) 0 || strlen(title) == 0) title = "Graph Window";
  
  /* adjust for scroll bars also */
  gwinfo = (StGWWinInfo *) StGWObWinInfo((LVAL) object);
  w_height = height + ClosePanelHeight();
  if (gwinfo->hasHscroll) height += ScrollWidth;
  if (gwinfo->hasVscroll) width += ScrollWidth;
  w_width = width;
  
  win = XCreateSimpleWindow(dpy, RootWindow(dpy, screen),
			    left, top, w_width, w_height, gw_border_width,
			    GWBorderColor, WhitePixel(dpy, screen));
  XSelectInput(dpy, win, StructureNotifyMask);
  StX11SetStandardHints(dpy, win);
  StX11SetWindowClass(dpy, win);
  SetWRefCon(win, (long) gwinfo);
  gwinfo->window = win;
  gwinfo->frame_width = -1;
  gwinfo->frame_height = -1;
  gwinfo->go_away = go_away;
  gwinfo->initialized = FALSE;

  XStoreName(dpy, win, title);
  StX11SetNormalHints(dpy, win, left, top, w_width, w_height);

  left = 0;
  top = ClosePanelHeight();
  get_panel_size(gwinfo, w_width, w_height, &width, &height);
  gwinfo->panel = XCreateSimpleWindow(dpy, win,
				      left, top, width, height,
				      gw_border_width,
				      GWBorderColor, 
				      get_color(gwinfo->backColor));
  winattr.win_gravity = UnmapGravity;
  XSelectInput(dpy, gwinfo->panel, 
	       ExposureMask | KeyPressMask | PointerMotionMask | 
	       ButtonPressMask | ButtonReleaseMask);
  XChangeWindowAttributes(dpy, gwinfo->panel, CWWinGravity, &winattr);
  if (! gwinfo->hasVscroll) gwinfo->canvasHeight = height;
  if (! gwinfo->hasHscroll) gwinfo->canvasWidth = width;
	       
  if (go_away) InstallCloseButton(win, object);
  InstallMenuButton(win, object);
  make_new_gc(gwinfo);

  InstallScrollBar(win, object, 0, top + height, width, ScrollWidth,
		   &gwinfo->hscroll, StX11ObScrollAction);
  InstallScrollBar(win, object, left + width, top, ScrollWidth, height, 
		   &gwinfo->vscroll, StX11ObScrollAction);

  if (XSaveContext(dpy, win, EventContext, (caddr_t) frame_handler) != 0)
    xlfail("could not install event handler");
  if (XSaveContext(dpy, win, ObjectContext, (caddr_t) object) != 0)
    xlfail("could not install object in window");
  if (XSaveContext(dpy, gwinfo->panel, EventContext, (caddr_t) panel_handler)
      != 0)
    xlfail("could not install event handler");
  if (XSaveContext(dpy, gwinfo->panel, ObjectContext, (caddr_t) object) != 0)
    xlfail("could not install object in window");

  if (is_GW) set_iview_window_address((CPTR) win, object);
  else set_iview_address((CPTR) win, object);

  XDefineCursor(dpy, win, ArrowCursor);
  StGWSetCursor(gwinfo, gwinfo->cursor);

  /* Display (map) the panel and buttons windows */
  XMapSubwindows(dpy, win);

  gwinfo->mouse_x = -1;
  gwinfo->mouse_y = -1;
  return(win);
}

VOID StX11SetWindowClass(dpy, win)
     Display *dpy;
     Window win;
{
  XClassHint class_hints;
  
  class_hints.res_name  = "xlisp";
  class_hints.res_class = "Xlisp";
  XSetClassHint(dpy, win, &class_hints);
}

VOID StX11SetNormalHints(dpy, win, left, top, width, height)
     Display *dpy;
     Window win;
     int left, top, width, height;
{
  XSizeHints hints;

  hints.x = left;
  hints.y = top;
  hints.width = width;
  hints.height = height;
  hints.flags = USPosition | USSize;
  XSetNormalHints(dpy, win, &hints);
}

VOID StX11SetTransientHint(dpy, win)
     Display *dpy;
     Window win;
{
  int screen = StX11Screen();

  XSetTransientForHint(dpy, win, RootWindow(dpy, screen));
}

VOID StX11SetStandardHints(dpy, w)
     Display *dpy;
     Window w;
{
  XWMHints hints;

  hints.input = TRUE;
  hints.flags = InputHint;
  XSetWMHints(dpy, w, &hints);

  /* 
   * According to the ICCCM, placing the WM_DELETE_WINDOW atom in the
   * protocols property tells a compliant window manager not to blast
   * the application when a `delete window' action occurs (e.g. by
   * choosing Quit from an olwm frame menu). Instead, a ClientMessage
   * is generated. If the window is to have a go_away, the frame_handler
   * sends the window the :close message in response to such an event.
   */
  XChangeProperty(dpy, w, wm_protocols, XA_ATOM, 32, PropModeReplace,
		  (unsigned char *) &wm_delete_window, 1);
}

/**************************************************************************/
/**                                                                      **/
/**                      Window Management Functions                     **/
/**                                                                      **/
/**************************************************************************/

VOID StGWShowWindow(gwinfo)
     StGWWinInfo *gwinfo;
{
  Window w;

  if (gwinfo == NULL || (w = gwinfo->window) == NullWindow) return;
  else {
    StShowWindow(w);
    if (! gwinfo->initialized) StGWInitialDraw(gwinfo);
  }
}

VOID StGWRemove(gwinfo)
     StGWWinInfo *gwinfo;
{
  Window w;
  Display *dpy = StX11Display();
  
  if (gwinfo == NULL || (w = gwinfo->window) == NullWindow) return;
  if (IViewInternalIsLinked(w)) IViewUnlinkWindow(w);
  StGWObDoClobber((LVAL) gwinfo->Object);
  if (gwinfo->FreeMem != NULL) (*gwinfo->FreeMem)(w);

  if (XDeleteContext(dpy, w, EventContext) != 0)
    xlfail("could not delete event context");
  if (XDeleteContext(dpy, w, ObjectContext) != 0)
    xlfail("could not delete object context");
  if (XDeleteContext(dpy, gwinfo->panel, EventContext) != 0)
    xlfail("could not delete event context");
  if (XDeleteContext(dpy, gwinfo->panel, ObjectContext) != 0)
    xlfail("could not delete object context");

  if (gwinfo->gc != NULL) XFreeGC(dpy, gwinfo->gc);
  gwinfo->gc = NULL;
  if (gwinfo->erase_gc != NULL) XFreeGC(dpy, gwinfo->erase_gc);
  gwinfo->erase_gc = NULL;
  if (gwinfo->xor_gc != NULL) XFreeGC(dpy, gwinfo->xor_gc);
  gwinfo->xor_gc = NULL;

  DeleteScrollBar(gwinfo->vscroll);
  DeleteScrollBar(gwinfo->hscroll);
  gwinfo->vscroll = NullWindow;
  gwinfo->hscroll = NullWindow;

  gwinfo->window = NullWindow;
  gwinfo->panel = NullWindow;
  gwinfo->frame_width = -1;
  gwinfo->frame_height = -1;
  if (gwinfo->go_away) DeleteCloseButton(w);
  if (gwinfo->has_menu_button) DeleteMenuButton(w);
  XDestroyWindow(dpy, w);
  XFlush(dpy);

  if (XDeleteContext(dpy, w, WRefConContext) != 0)
    xlfail("could not delete WRefCon context");
}

VOID StWSetLocation(w, left, top, frame)
     Window w;
     int left, top, frame;
{
  Display *dpy = StX11Display();
  XSizeHints hints;
  int width, height;

  /**** Should allow for borders. Assumes all windows have close ***/
  /**** panel (true except for modal dialogs)                    ***/
  if (! frame) top -= ClosePanelHeight();
  if (w != NullWindow) {
    XMoveWindow(dpy, w, left, top);
    StWGetSize(w, &width, &height, TRUE);
    hints.x = left;
    hints.y = top;
    hints.width = width;
    hints.height = height;
    hints.flags = USPosition | USSize;
    XSetNormalHints(dpy, w, &hints);
    XFlush(dpy);
  }
}

VOID StWGetLocation(w, left, top, frame)
     Window w;
     int *left, *top, frame;
{
  Display *dpy = StX11Display();
  Window root, child, *children, parent;
  int lx, ly, x, y;
  unsigned int width, height, b_width, depth, nch;

  /**** Should allow for borders. Assumes all windows have close ***/
  /**** panel (true except for madal dialogs)                    ***/
  if (w != NullWindow) {
    XGetGeometry(dpy, w, &root, &lx, &ly, &width, &height, &b_width, &depth);
    XQueryTree(dpy, w, &root, &parent, &children, &nch);
    if (children != NULL) XFree((char *) children);
    XTranslateCoordinates(dpy, parent, root, lx, ly, &x, &y, &child);
    if (left != NULL) *left = x;
    if (top != NULL) *top = (frame) ? y : y + ClosePanelHeight();
  }
  else {
    if (left != NULL) *left = 0;
    if (top != NULL) *top = 0;
  }
}

VOID StWSetSize(w, width, height, frame)
     Window w;
     int width, height;
{
  Display *dpy = StX11Display();
  StGWWinInfo *gwinfo;
  XSizeHints hints;
  int left, top;

  /**** Should allow for borders. Assumes all windows have close ***/
  /**** panel (true except for modal dialogs)                   ***/
  if ((gwinfo = (StGWWinInfo *) GetWRefCon(w)) != NULL) {
    gwinfo->frame_width = -1;
    gwinfo->frame_height = -1;
  }
  if (! frame) height += ClosePanelHeight();
  if (w != NullWindow) {
    XResizeWindow(dpy, w, width, height);
    StWGetLocation(w, &left, &top, TRUE);
    hints.x = left;
    hints.y = top;
    hints.width = width;
    hints.height = height;
    hints.flags = USPosition | USSize;
    XSetNormalHints(dpy, w, &hints);
    if (gwinfo != NULL) adjust_scroll_bars(gwinfo, TRUE);
    XFlush(dpy);
  }
}

VOID StWGetSize(w, pwidth, pheight, frame)
     Window w;
     int *pwidth, *pheight;
     int frame;
{
  Display *dpy = StX11Display();
  Window root;
  int x, y;
  unsigned int width, height, b_width, depth;
  StGWWinInfo *gwinfo;

  /**** Should allow for borders. Assumes all windows have close ***/
  /**** panel (true except for madal dialogs)                    ***/
  if (w != NullWindow) {
    if ((gwinfo = (StGWWinInfo *) GetWRefCon(w)) != (StGWWinInfo *) NULL
	&& gwinfo->frame_width >= 0 && gwinfo->frame_height >= 0) {
      width = gwinfo->frame_width;
      height = gwinfo->frame_height;
      if (! frame) get_panel_size(gwinfo, width, height,
				  (int *) &width,
				  (int *) &height);
    }
    else {
      XGetGeometry(dpy, w, &root, &x, &y, &width, &height, &b_width, &depth);
      if (! frame) height -= ClosePanelHeight();
    }
    if (pwidth != NULL) *pwidth = width;
    if (pheight != NULL) *pheight = height;
  }
  else {
    if (pwidth != NULL) *pwidth = 1;
    if (pheight != NULL) *pheight = 1;
  }    
}

VOID StGWSetSize(gwinfo, width, height, frame)
     StGWWinInfo *gwinfo;
     int width, height;
{
  Window w;
  if (gwinfo == NULL || (w = gwinfo->window) == NullWindow) return;
  else StWSetSize(w, width, height, frame);
}

/**************************************************************************/
/**                                                                      **/
/**             Window State Access and Mutation Functions               **/
/**                                                                      **/
/**************************************************************************/

int StGWUseColor(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(0);
  else return(gwinfo->use_color);
}

int StGWCanvasWidth(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(0);
  else return(gwinfo->canvasWidth);
}

int StGWCanvasHeight(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(0);
  else return(gwinfo->canvasHeight);
}

int StGWLineType(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(0);
  else return(gwinfo->lineType);
}

int StGWDrawMode(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(0);
  else return(gwinfo->drawMode);
}

int StGWDrawColor(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(0);
  else return(gwinfo->drawColor);
}

int StGWBackColor(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(0);
  else return(gwinfo->backColor);
}

VOID StGWGetLineWidth(gwinfo, width)
     StGWWinInfo *gwinfo;
     int *width;
{
  if (gwinfo == NULL) return;
  else if (width != NULL) *width = gwinfo->lineWidth;
}

VOID StGWGetViewRect(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int *left, *top, *width, *height;
{
  if (gwinfo == NULL) return;
  if (gwinfo->window != NullWindow) {
    StWGetSize(gwinfo->window, width, height, FALSE);
    if (left != NULL) *left = gwinfo->view_h;
    if (top != NULL) *top = gwinfo->view_v;
  }
  else {
    if (left != NULL) *left = 0;
    if (top != NULL) *top = 0;
    if (width != NULL) *width = gwinfo->canvasWidth;
    if (height != NULL) *height = gwinfo->canvasHeight;
  }
}

int StGWIdleOn(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(FALSE);
  else return(gwinfo->idleOn);
}

VOID StGWSetIdleOn(gwinfo, on)
        StGWWinInfo *gwinfo;
        int on;
{
  if (gwinfo != NULL) gwinfo->idleOn = on;
}

VOID StGWSetDrawMode(gwinfo, mode)
     StGWWinInfo *gwinfo;
     int mode;
{
  if (gwinfo != NULL && gwinfo->drawMode != mode)
    gwinfo->drawMode = mode;
}

VOID StGWSetLineWidth(gwinfo, width)
     StGWWinInfo *gwinfo;
     int width;
{
  XGCValues values;

  if (width < 0) width = 0;
  if (gwinfo != NULL && gwinfo->lineWidth != width) {
    gwinfo->lineWidth = width;
    if (width == 1 && use_fast_lines) width = 0;
    values.line_width = width;
    if (gwinfo->gc != NULL)
      XChangeGC(StX11Display(), gwinfo->gc, GCLineWidth, &values);
    if (gwinfo->xor_gc != NULL)
      XChangeGC(StX11Display(), gwinfo->xor_gc, GCLineWidth, &values);
  }
}

VOID StGWSetLineType(gwinfo, type)
     StGWWinInfo *gwinfo;
     int type;
{
  XGCValues values;

  if (gwinfo != NULL && gwinfo->lineType != type) {
    gwinfo->lineType = type;
    values.line_style = (type == 0) ? LineSolid : LineOnOffDash;
    if (gwinfo->gc != NULL)
      XChangeGC(StX11Display(), gwinfo->gc, GCLineStyle, &values);
    if (gwinfo->xor_gc != NULL)
      XChangeGC(StX11Display(), gwinfo->xor_gc, GCLineStyle, &values);
  }
}

VOID StGWSetDrawColor(gwinfo, index) 
     StGWWinInfo *gwinfo;
     int index;
{
  if (index < 0) index = 1;
  if (gwinfo != NULL && gwinfo->drawColor != index) {
    gwinfo->drawColor = index;
    if (gwinfo->gc != NULL)
      XSetForeground(StX11Display(), gwinfo->gc, get_color(gwinfo->drawColor));
  }
}

VOID StGWSetBackColor(gwinfo, index)
     StGWWinInfo *gwinfo;
     int index;
{
  if (index < 0) index = 0;
  if (gwinfo != NULL && gwinfo->backColor != index) {
    gwinfo->backColor = index;
    if (gwinfo->gc != NULL)
      XSetBackground(StX11Display(), gwinfo->gc, get_color(gwinfo->backColor));
    if (gwinfo->xor_gc != NULL)
      XSetBackground(StX11Display(), gwinfo->xor_gc, 
		     xor_color(gwinfo->backColor));
    if (gwinfo->erase_gc != NULL) {
      XSetForeground(StX11Display(), gwinfo->erase_gc, 
		     get_color(gwinfo->backColor));
      XSetBackground(StX11Display(), gwinfo->erase_gc, 
		     get_color(gwinfo->backColor));
    }
  }
}

VOID StGWReverseColors(gwinfo)
     StGWWinInfo *gwinfo;
{
  ColorCode backColor, drawColor;
  
  if (gwinfo == NULL) return;
  backColor = StGWBackColor(gwinfo);
  drawColor = StGWDrawColor(gwinfo);
  if (backColor != drawColor) {
    StGWSetBackColor(gwinfo, drawColor);
    StGWSetDrawColor(gwinfo, backColor);
    StGWObRedraw((LVAL) gwinfo->Object);
  }
}
      
VOID StGWSetUseColor(gwinfo, use)
     StGWWinInfo *gwinfo;
     int use;
{
  if (gwinfo != NULL && gwinfo->use_color != use) {
    gwinfo->use_color = use; 
    /*** do something with plane masks??? ***/
  }
}

/**************************************************************************/
/**                                                                      **/
/**                           Drawing Functions                          **/
/**                                                                      **/
/**************************************************************************/

LOCAL Drawable get_drawable(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return NullDrawable;
  else if (buffering) return(WorkPort);
  else return(gwinfo->panel);
}

# define is_allocated(gwinfo) ((gwinfo) != NULL && (gwinfo)->gc != NULL)
# define draw_gc(gwinfo) \
  ((gwinfo)->drawMode == 0) ? (gwinfo)->gc : (gwinfo)->xor_gc

#define NUMTEST 100

VOID StGWDrawPoint(gwinfo, x, y)
     StGWWinInfo *gwinfo;
     int x, y;
{
  Display *dpy = StX11Display();
  XDrawPoint(dpy, get_drawable(gwinfo), draw_gc(gwinfo),
	     x - gwinfo->view_v, y - gwinfo->view_v);
}
  
VOID StGWDrawLine(gwinfo, x1, y1, x2, y2)
     StGWWinInfo *gwinfo;
     int x1, y1, x2, y2;
{
  Display *dpy = StX11Display();

  if (is_allocated(gwinfo))
    XDrawLine(dpy, get_drawable(gwinfo), draw_gc(gwinfo), 
	      x1 - gwinfo->view_h, y1 - gwinfo->view_v, 
	      x2 - gwinfo->view_h, y2 - gwinfo->view_v);
}

VOID StGWFrameRect(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  Display *dpy = StX11Display();

  if (is_allocated(gwinfo))
    XDrawRectangle(dpy, get_drawable(gwinfo), draw_gc(gwinfo),
		   left - gwinfo->view_h, top - gwinfo->view_v,
		   width - gwinfo->lineWidth, height - gwinfo->lineWidth);
}

LOCAL VOID fill_rect(gwinfo, left, top, width, height, which)
     StGWWinInfo *gwinfo;
     int left, top, width, height, which;
{
  Display *dpy = StX11Display();

  if (is_allocated(gwinfo))
    XFillRectangle(dpy, get_drawable(gwinfo), 
		   (which == 'P') ? gwinfo->gc : gwinfo->erase_gc,
                   left - gwinfo->view_h, top - gwinfo->view_v,
		   width, height);
}

VOID StGWPaintRect(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  fill_rect(gwinfo, left, top, width, height, 'P');
}

VOID StGWEraseRect(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  fill_rect(gwinfo, left, top, width, height, 'E');
}

LOCAL VOID draw_arc(gwinfo, left, top, width, height, angle1, angle2)
     StGWWinInfo *gwinfo;
     int left, top, width, height, angle1, angle2;
{
  Display *dpy = StX11Display();

  if (is_allocated(gwinfo))
    XDrawArc(dpy, get_drawable(gwinfo), draw_gc(gwinfo),
             left - gwinfo->view_h, top - gwinfo->view_v,
             width - gwinfo->lineWidth, height - gwinfo->lineWidth,
             angle1, angle2);
}

LOCAL VOID fill_arc(gwinfo, left, top, width, height, angle1, angle2, which)
     StGWWinInfo *gwinfo;
     int left, top, width, height, angle1, angle2, which;
{
  Display *dpy = StX11Display();

  if (is_allocated(gwinfo))
    XFillArc(dpy, get_drawable(gwinfo), 
	     (which == 'P') ? gwinfo->gc : gwinfo->erase_gc,
	     left - gwinfo->view_h, top - gwinfo->view_v,
	     width, height, angle1, angle2);
}

VOID StGWFrameOval(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  draw_arc(gwinfo, left, top, width, height, 0, 360 * 64);
}

VOID StGWPaintOval(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  fill_arc(gwinfo, left, top, width, height, 0, 360 * 64, 'P');
}

VOID StGWEraseOval(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  fill_arc(gwinfo, left, top, width, height, 0, 360 * 64, 'E');
}

VOID StGWFrameArc(gwinfo, left, top, width, height, a1, a2)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
     double a1, a2;
{
  draw_arc(gwinfo, left, top, width, height, (int) a1 * 64, (int) a2 * 64);
}

VOID StGWPaintArc(gwinfo, left, top, width, height, a1, a2)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
     double a1, a2;
{
  fill_arc(gwinfo, left, top, width, height, 
	   (int) a1 * 64, (int) a2 * 64, 'P');
}

VOID StGWEraseArc(gwinfo, left, top, width, height, a1, a2)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
     double a1, a2;
{
  fill_arc(gwinfo, left, top, width, height,
           (int) a1 * 64, (int) a2 * 64, 'E');
}

VOID StGWFramePoly(gwinfo, n, p, from_origin)
        StGWWinInfo *gwinfo;
        int n, from_origin;
        short *p;
{
  Display *dpy = StX11Display();
  int i;

  if (is_allocated(gwinfo) && n > 1 && p != NULL) {
    if (from_origin)
      for (i = 0; i < n; i++) {
	p[2 * i] -= gwinfo->view_h;
	p[2 * i + 1] -= gwinfo->view_v;
      }
    else {
      p[0] -= gwinfo->view_h;
      p[1] -= gwinfo->view_v;
    }
    XDrawLines(dpy, get_drawable(gwinfo), draw_gc(gwinfo), (XPoint *) p, n,
	       (from_origin) ? CoordModeOrigin : CoordModePrevious);
  }
}

LOCAL VOID fill_poly(gwinfo, n, p, from_origin, which)
        StGWWinInfo *gwinfo;
        int n, from_origin, which;
        short *p;
{
  Display *dpy = StX11Display();
  int i;

  if (is_allocated(gwinfo) && n > 1 && p != NULL) {
    if (from_origin)
      for (i = 0; i < n; i++) {
	p[2 * i] -= gwinfo->view_h;
	p[2 * i + 1] -= gwinfo->view_v;
      }
    else {
      p[0] -= gwinfo->view_h;
      p[1] -= gwinfo->view_v;
    }
    XFillPolygon(dpy, get_drawable(gwinfo), 
		 (which == 'P') ? gwinfo->gc : gwinfo->erase_gc,
		 (XPoint *) p, n, Complex, 
		 (from_origin) ? CoordModeOrigin : CoordModePrevious);
  }  
}

VOID StGWPaintPoly(gwinfo, n, p, from_origin)
        StGWWinInfo *gwinfo;
        int n, from_origin;
        short *p;
{
  fill_poly(gwinfo, n, p, from_origin, 'P');
}

VOID StGWErasePoly(gwinfo, n, p, from_origin)
        StGWWinInfo *gwinfo;
        int n, from_origin;
        short *p;
{
  fill_poly(gwinfo, n, p, from_origin, 'E');
}

/**************************************************************************/
/**                                                                      **/
/**                            Text Functions                            **/
/**                                                                      **/
/**************************************************************************/

int StGWTextAscent(gwinfo)
     StGWWinInfo *gwinfo;
{
  return(GraphFont->max_bounds.ascent);
}

int StGWTextDescent(gwinfo)
     StGWWinInfo *gwinfo;
{
  return(GraphFont->max_bounds.descent);
}

int StGWTextWidth(gwinfo, text)
     StGWWinInfo *gwinfo;
     char *text;
{
  return((text != NULL) ? XTextWidth(GraphFont, text, strlen(text)) : 0);
}

VOID StGWDrawString(gwinfo, s, x, y)
     StGWWinInfo *gwinfo;
     char *s;
     int x, y;
{
  Display *dpy = StX11Display();
  
  if (is_allocated(gwinfo) && s != NULL) {
    XDrawString(dpy, get_drawable(gwinfo), draw_gc(gwinfo),
		x - gwinfo->view_h, y - gwinfo->view_v, s, strlen(s));
  }
}

LOCAL VOID draw_char_up(gwinfo, s, x, y)
     StGWWinInfo *gwinfo;
     char *s;
     int x, y;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  unsigned long bp = BlackPixel(dpy, screen);
  int ascent = GraphFont->max_bounds.ascent;
  int width = GraphFont->max_bounds.width;
  int height = GraphFont->max_bounds.ascent + GraphFont->max_bounds.descent;
  Drawable d;
  XImage *xi;
  int i, j;
  static XPoint *p = NULL;
  int n;

  if (p == NULL) p = (XPoint *) StCalloc(width * height, sizeof(XPoint));

  if (is_allocated(gwinfo)) {
    XFillRectangle(dpy, CharPM, CharEraseGC, 0, 0, width, height);
    XDrawString(dpy, CharPM, CharGC, 0, ascent, s, 1);
    xi = XGetImage(dpy, CharPM, 0, 0, width, height, AllPlanes, ZPixmap);
    d = get_drawable(gwinfo);
    for (i = 0, n = 0; i < width; i++)
      for (j = 0; j < height; j++)
	if (bp == XGetPixel(xi, i, j)) {
	  p[n].x = x - ascent + j;
	  p[n].y = y - i;
	  n++;
	}
    XDrawPoints(dpy, d, draw_gc(gwinfo), p, n, CoordModeOrigin);
    XDestroyImage(xi);
  }
}

VOID StGWDrawText(gwinfo, text, x, y, h, v)
     StGWWinInfo *gwinfo;
     char *text;
     int x, y, h, v;
{
  int FontAscent, string_width;

  FontAscent = StGWTextAscent(gwinfo);
  string_width = StGWTextWidth(gwinfo, text);

  if (v == 1) y += FontAscent;
  if (h == 1) x -= string_width / 2;
  if (h == 2) x -= string_width;

  StGWDrawString(gwinfo, text, x, y);
}

VOID StGWDrawStringUp(gwinfo, s, x, y)
     StGWWinInfo *gwinfo;
     char *s;
     int x, y;
{
  char str[2];
  int n;
  
  str[1] = '\0';
  
  if (s == NULL || gwinfo == NULL) return;

  x -= gwinfo->view_h;
  y -= gwinfo->view_v;
  for (n = strlen(s); n > 0; n--, s++) {
  	draw_char_up(gwinfo, s, x, y);
  	str[0] = *s;
  	y -= StGWTextWidth(gwinfo, str);
  }
}
 
VOID StGWDrawTextUp(gwinfo, text, x, y, h, v)
     StGWWinInfo *gwinfo;
     char *text;
     int x, y, h, v;
{
  int FontAscent, string_width;

  FontAscent = StGWTextAscent(gwinfo);
  string_width = StGWTextWidth(gwinfo, text);

  if (v == 1) x -= FontAscent;
  if (h == 1) y += string_width / 2;
  if (h == 2) y += string_width;

  StGWDrawStringUp(gwinfo, text, x, y);
}

/**************************************************************************/
/**                                                                      **/
/**                           Symbol Functions                           **/
/**                                                                      **/
/**************************************************************************/

#define NUMSYMBOLS 18
#define NUMFASTSYMBOLS 18
#define SYMROWS 5

typedef struct {
  int left, top, width, height;
  char image[SYMROWS];
  Pixmap pm;
  long refcon;
} Symbol;

typedef struct {
  XPoint *black, *white;
  int nblack, nwhite, blkleft, blktop, wleft, wtop;
} FastSymbol;

#define NPM NullPixmap
static Symbol Symbols[] = {
  {0, 0, 1, 1, {0x01, 0x00, 0x00, 0x00, 0x00}, NPM, 0L}, /* dot */
  {0, 0, 2, 1, {0x03, 0x00, 0x00, 0x00, 0x00}, NPM, 0L}, /* double dot */
  {1, 1, 2, 2, {0x03, 0x01, 0x00, 0x00, 0x00}, NPM, 0L}, /* triple dot */
  {1, 1, 2, 2, {0x03, 0x03, 0x00, 0x00, 0x00}, NPM, 0L}, /* quadruple dot */
  {2, 2, 4, 4, {0x06, 0x09, 0x09, 0x06, 0x00}, NPM, 0L}, /* disk */
  {2, 2, 4, 4, {0x06, 0x0f, 0x0f, 0x06, 0x00}, NPM, 0L},
  {3, 3, 5, 5, {0x04, 0x0a, 0x11, 0x0a, 0x04}, NPM, 0L}, /* diamond */
  {3, 3, 5, 5, {0x04, 0x0e, 0x1f, 0x0e, 0x04}, NPM, 0L},
  {3, 3, 5, 5, {0x04, 0x04, 0x1f, 0x04, 0x04}, NPM, 0L}, /* cross */
  {3, 3, 5, 5, {0x0a, 0x1b, 0x00, 0x1b, 0x0a}, NPM, 0L},
  {2, 2, 4, 4, {0x0f, 0x09, 0x09, 0x0f, 0x00}, NPM, 0L}, /* square */
  {2, 2, 4, 4, {0x0f, 0x0f, 0x0f, 0x0f, 0x0f}, NPM, 0L},
  {3, 3, 5, 5, {0x0e, 0x11, 0x11, 0x0a, 0x04}, NPM, 0L}, /* wedge 1 */
  {3, 3, 5, 5, {0x0e, 0x1f, 0x1f, 0x0e, 0x04}, NPM, 0L},
  {3, 3, 5, 5, {0x04, 0x0a, 0x11, 0x11, 0x0e}, NPM, 0L}, /* wedge 2 */
  {3, 3, 5, 5, {0x04, 0x0e, 0x1f, 0x1f, 0x0e}, NPM, 0L},
  {3, 3, 5, 5, {0x11, 0x0a, 0x04, 0x0a, 0x11}, NPM, 0L}, /* X */
  {3, 3, 5, 5, {0x1b, 0x1b, 0x04, 0x1b, 0x1b}, NPM, 0L}
};
#undef NPM

static FastSymbol fast_syms[] = {
  {NULL, NULL,  0,  0, 0, 0, 0, 0},
  {NULL, NULL,  0,  0, 0, 0, 0, 0},
  {NULL, NULL,  0,  0, 0, 0, 0, 0},
  {NULL, NULL,  0,  0, 0, 0, 0, 0},
  {NULL, NULL,  8,  4, 1, 0, 1, 1},         /* disk */
  {NULL, NULL, 12,  0, 1, 0, 1, 1},
  {NULL, NULL,  8,  5, 2, 0, 2, 1},         /* diamond */
  {NULL, NULL, 13,  0, 2, 0, 0, 0},
  {NULL, NULL,  9, 12, 2, 0, 1, 0},         /* cross */
  {NULL, NULL, 12,  9, 1, 0, 2, 0},
  {NULL, NULL, 12,  4, 0, 0, 1, 1},         /* square */
  {NULL, NULL, 16,  0, 0, 0, 0, 0},
  {NULL, NULL, 10,  7, 1, 0, 1, 1},         /* wedge 1 */
  {NULL, NULL, 17,  0, 1, 0, 0, 0},
  {NULL, NULL, 10,  7, 2, 0, 2, 1},         /* wedge 2 */
  {NULL, NULL, 17,  0, 2, 0, 0, 0},
  {NULL, NULL,  9,  8, 0, 0, 1, 0},         /* X */
  {NULL, NULL, 17,  0, 0, 0, 0, 0}
};

/* disk */
static XPoint sym4black[] = {{0, 0}, {1, 0}, {-2, 1}, {3, 0},
			       {-3, 1}, {3, 0}, {-2, 1}, {1, 0}};
static XPoint sym4white[] = {{0, 0}, {1, 0}, {-1, 1}, {1, 0}};
static XPoint sym5black[] = {{0, 0}, {1, 0},
                                {-2, 1}, {1, 0}, {1, 0}, {1, 0},
				{-3, 1}, {1, 0}, {1, 0}, {1, 0},
				{-2, 1}, {1, 0}};
static XPoint *sym5white = NULL;

/* diamond */
static XPoint sym6black[] = {{0, 0}, {-1, 1}, {2, 0}, {-3, 1}, 
			       {4, 0}, {-3, 1}, {2, 0}, {-1, 1}};
static XPoint sym6white[] = {{0, 0}, {-1, 1}, {1, 0}, {1, 0}, {-1, 1}};
static XPoint sym7black[] = {{0, 0}, {-1, 1}, {1, 0}, {1, 0},
			       {-3, 1},  {1, 0}, {1, 0}, {1, 0}, {1, 0},
			       {-3, 1}, {1, 0}, {1, 0}, {-1, 1}};
static XPoint *sym7white = NULL;

/* cross */
static XPoint sym8black[] = {{0, 0}, {0, 1}, {-2, 1}, {1, 0}, {1, 0}, 
			       {1, 0}, {1, 0}, {-2, 1}, {0, 1}};
static XPoint sym8white[] = {{0, 0}, {2, 0}, {-3, 1}, {1, 0},
			       {2, 0}, {1, 0}, {-4, 2}, {1, 0},
			       {2, 0}, {1, 0}, {-3, 1}, {2, 0}};
static XPoint sym9black[] = {{0, 0}, {2, 0}, {-3, 1}, {1, 0},
                               {2, 0}, {1, 0}, {-4, 2}, {1, 0},
                               {2, 0}, {1, 0}, {-3, 1}, {2, 0}};
static XPoint sym9white[] = {{0, 0}, {0, 1}, {-2, 1}, {1, 0}, {1, 0},
			       {1, 0}, {1, 0}, {-2, 1}, {0, 1}};

/* square */
static XPoint sym10black[] = {{0, 0}, {1, 0}, {1, 0}, {1, 0},
				{-3, 1}, {3, 0}, {-3, 1}, {3, 0},
				{-3, 1}, {1, 0}, {1, 0}, {1, 0}};
static XPoint sym10white[] = {{0, 0}, {1, 0}, {-1, 1}, {1, 0}};
static XPoint sym11black[] = {{0, 0}, {1, 0}, {1, 0}, {1, 0},
				{-3, 1}, {1, 0}, {1, 0}, {1, 0},
				{-3, 1}, {1, 0}, {1, 0}, {1, 0},
				{-3, 1}, {1, 0}, {1, 0}, {1, 0}};
static XPoint *sym11white = NULL;

/* wedge 1 */
static XPoint sym12black[] = {{0, 0}, {1, 0}, {1, 0}, {-3, 1}, {4, 0},
				{-4, 1}, {4, 0}, {-3, 1}, {2, 0}, {-1, 1}};
static XPoint sym12white[] = {{0, 0}, {1, 0}, {1, 0}, {-2, 1}, {1, 0},
				{1, 0}, {-1, 1}};
static XPoint sym13black[] = {{0, 0}, {1, 0}, {1, 0}, {-3, 1}, {1, 0}, {1, 0},
				{1, 0}, {1, 0}, {-4, 1}, {1, 0}, {1, 0},
				{1, 0}, {1, 0}, {-3, 1},  {1, 0}, {1, 0},
				{-1, 1}};
static XPoint *sym13white = NULL;

/* wedge 2 */
static XPoint sym14black[] = {{0, 0}, {-1, 1}, {2, 0}, {-3, 1}, {4, 0},
				{-4, 1}, {4, 0}, {-3, 1},  {1, 0}, {1, 0}};
static XPoint sym14white[] = {{0, 0}, {-1, 1}, {1, 0}, {1, 0}, {-2, 1},
				{1, 0}, {1, 0}};
static XPoint sym15black[] = {{0, 0}, {-1, 1}, {1, 0}, {1, 0}, {-3, 1},
				{1, 0}, {1, 0}, {1, 0}, {1, 0}, {-4, 1},
				{1, 0}, {1, 0}, {1, 0}, {1, 0}, {-3, 1},
				{1, 0}, {1, 0}};
static XPoint *sym15white = NULL;

/* X */
static XPoint sym16black[] = {{0, 0}, {4, 0}, {-3, 1}, {2, 0}, {-1, 1}, 
				{-1, 1}, {2, 0}, {-3, 1}, {4, 0}};
static XPoint sym16white[] = {{0, 0}, {2, 0}, {-3, 1}, {4, 0}, {-4, 2}, {4, 0},
				{-3, 1}, {2, 0}};
static XPoint sym17black[] = {{0, 0}, {1, 0}, {2, 0}, {1, 0}, {-4, 1}, {1, 0},
				{2, 0}, {1, 0}, {-2, 1}, {-2, 1}, {1, 0}, 
				{2, 0}, {1, 0}, {-4, 1}, {1, 0}, {2, 0}, 
				{1, 0}};
static XPoint *sym17white = NULL;

LOCAL VOID MakeSymbols()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  int i;

  for (i = 0; i < NUMSYMBOLS; i++)
    Symbols[i].pm = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
					  Symbols[i].image,
					  Symbols[i].width,
					  Symbols[i].height);
  
  fast_syms[4].white = sym4white;
  fast_syms[4].black = sym4black;
  fast_syms[5].white = sym5white;
  fast_syms[5].black = sym5black;

  fast_syms[6].white = sym6white;
  fast_syms[6].black = sym6black;
  fast_syms[7].white = sym7white;
  fast_syms[7].black = sym7black;

  fast_syms[8].white = sym8white;
  fast_syms[8].black = sym8black;
  fast_syms[9].white = sym9white;
  fast_syms[9].black = sym9black;

  fast_syms[10].white = sym10white;
  fast_syms[10].black = sym10black;
  fast_syms[11].white = sym11white;
  fast_syms[11].black = sym11black;

  fast_syms[12].white = sym12white;
  fast_syms[12].black = sym12black;
  fast_syms[13].white = sym13white;
  fast_syms[13].black = sym13black;

  fast_syms[14].white = sym14white;
  fast_syms[14].black = sym14black;
  fast_syms[15].white = sym15white;
  fast_syms[15].black = sym15black;

  fast_syms[16].white = sym16white;
  fast_syms[16].black = sym16black;
  fast_syms[17].white = sym17white;
  fast_syms[17].black = sym17black;
}

#ifdef DRAWPOINTSBUG
static XPoint tmpsym[SYMROWS * SYMROWS];
#endif /* DRAWPOINTSBUG */

VOID StGWSetSymRefCon(index, rc)
     unsigned int index;
     long rc;
{
  if (index < NUMSYMBOLS) Symbols[index].refcon = rc;
}

long StGWGetSymRefCon(index)
	unsigned int index;
{	
  if (index < NUMSYMBOLS) return(Symbols[index].refcon);
  else return 0;
}

VOID StGWGetSymbolSize(sym, width, height)
     int sym, *width, *height;
{
  *width = Symbols[sym].width;
  *height = Symbols[sym].height;
}

LOCAL VOID FreeSymbols()
{
  Display *dpy = StX11Display();
  int i;

  for (i = 0; i < NUMSYMBOLS; i++) {
    if (Symbols[i].pm != NullPixmap) XFreePixmap(dpy, Symbols[i].pm);
  }
}

VOID StGWDrawSymbol(gwinfo, sym, x, y)
     StGWWinInfo *gwinfo;
     int sym;
     int x, y;
{
  Display *dpy = StX11Display();
  
  if (sym < 0) return;
  if (use_fast_symbols && (sym < NUMFASTSYMBOLS) && is_allocated(gwinfo)) {
    cheat_syms(dpy, gwinfo, sym, x, y);
    return;
  }
  if (sym < NUMSYMBOLS &&
      Symbols[sym].pm != NullPixmap &&
      is_allocated(gwinfo)) {
    XCopyPlane(dpy, Symbols[sym].pm, get_drawable(gwinfo), draw_gc(gwinfo),
	       0, 0, Symbols[sym].width, Symbols[sym].height,
	       x - gwinfo->view_h - Symbols[sym].left, 
	       y - gwinfo->view_v - Symbols[sym].top,
	       1);
  }
}

#ifdef DRAWPOINTSBUG
LOCAL VOID set_tmpsym(sym, n, x, y)
     XPoint *sym;
     int n, x, y;
{
  int i;

  tmpsym[0].x = x;
  tmpsym[0].y = y;
  for (i = 1; i < n; i++) {
    x += sym[i].x;
    y += sym[i].y;
    tmpsym[i].x = x;
    tmpsym[i].y = y;
  }
}
#endif /* DRAWPOINTSBUG */

LOCAL VOID cheat_syms(dpy, gwinfo, sym, x, y)
     Display *dpy;
     StGWWinInfo *gwinfo;
     unsigned sym;
     int x, y;
{
  Drawable d;
  GC black_gc, white_gc;
  register int left, top;

  d = get_drawable(gwinfo);
  black_gc = draw_gc(gwinfo);
  white_gc = gwinfo->erase_gc;
  left = x - gwinfo->view_h - Symbols[sym].left;
  top = y - gwinfo->view_v - Symbols[sym].top;

  switch (sym) {
  case 0:
    XDrawPoint(dpy, d, black_gc, left, top);
    break;
  case 1:
    XDrawPoint(dpy, d, black_gc, left, top);
    XDrawPoint(dpy, d, black_gc, left + 1, top);
    break;
  case 2:
    XDrawPoint(dpy, d, black_gc, left, top);
    XDrawPoint(dpy, d, black_gc, left + 1, top);
    XDrawPoint(dpy, d, black_gc, left, top + 1);
    break;
  case 3:
    XDrawPoint(dpy, d, black_gc, left, top);
    XDrawPoint(dpy, d, black_gc, left + 1, top);
    XDrawPoint(dpy, d, black_gc, left, top + 1);
    XDrawPoint(dpy, d, black_gc, left + 1, top + 1);
    break;
  default:
    if (fast_syms[sym].nblack > 0) {
#ifdef DRAWPOINTSBUG
      set_tmpsym(fast_syms[sym].black, fast_syms[sym].nblack,
		 left + fast_syms[sym].blkleft, top + fast_syms[sym].blktop);
      XDrawPoints(dpy, d, black_gc, tmpsym, fast_syms[sym].nblack,
		  CoordModeOrigin);
#else
      fast_syms[sym].black[0].x = left + fast_syms[sym].blkleft;
      fast_syms[sym].black[0].y = top + fast_syms[sym].blktop;
      XDrawPoints(dpy, d, black_gc, 
		  fast_syms[sym].black, fast_syms[sym].nblack,
		  CoordModePrevious);
#endif
    }
    if (fast_syms[sym].nwhite > 0) {
#ifdef DRAWPOINTSBUG
      set_tmpsym(fast_syms[sym].white, fast_syms[sym].nwhite,
		 left + fast_syms[sym].wleft, top + fast_syms[sym].wtop);
      XDrawPoints(dpy, d, white_gc, tmpsym, fast_syms[sym].nwhite,
		  CoordModeOrigin);
#else
      fast_syms[sym].white[0].x = left + fast_syms[sym].wleft;
      fast_syms[sym].white[0].y = top + fast_syms[sym].wtop;
      XDrawPoints(dpy, d, white_gc, 
		  fast_syms[sym].white, fast_syms[sym].nwhite,
		  CoordModePrevious);
#endif
    }
  }
}

VOID StGWReplaceSymbol(gwinfo, oldsym, newsym, x, y)
     StGWWinInfo *gwinfo;
     unsigned oldsym, newsym;
     int x, y;
{
  int oldwidth, oldheight, newwidth, newheight;
  
  if (oldsym >= NUMSYMBOLS || newsym >= NUMSYMBOLS) return;
  
  StGWGetSymbolSize(oldsym, &oldwidth, &oldheight);
  StGWGetSymbolSize(newsym, &newwidth, &newheight);
  if (oldwidth > newwidth || oldheight > newheight)
    StGWEraseRect(gwinfo, x - Symbols[oldsym].left, y - Symbols[oldsym].top,
		  oldwidth, oldheight);
  StGWDrawSymbol(gwinfo, newsym, x, y);
}

/**************************************************************************/
/**                                                                      **/
/**                         Buffering Functions                          **/
/**                                                                      **/
/**************************************************************************/

LOCAL VOID MakeWorkPort()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  int width, height;

  width = DisplayWidth(dpy, screen);
  height = DisplayHeight(dpy, screen);
  WorkPort = XCreatePixmap(dpy, RootWindow(dpy, screen),
			   width, height, DefaultDepth(dpy, screen));

  /* it is not clear whether this is the proper error check */
  if (WorkPort == NullPixmap) {
    fprintf(stderr, "work port allocation failed");
    exit(-1);
  }
}

VOID StGWStartBuffering(gwinfo)
     StGWWinInfo *gwinfo;
{
  buffering = TRUE;
  bufflevel++;
}

VOID StGWBufferToScreen(gwinfo, left, top, width, height)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
{
  Display *dpy = StX11Display();
  GC gc;

  if (is_allocated(gwinfo)) {
    if (bufflevel > 0) bufflevel--;
    if (bufflevel > 0) return;
    if (! buffering) return;
    buffering = FALSE;
    left -= gwinfo->view_h;
    top -= gwinfo->view_v;
    
    /* some checking in case rectangle is bad */
    if (left < 0) width += left;
    if (top < 0) height += height;
    if (width < 0 || height < 0) return;

    /* the following is used to deal with the cfb clipping bug in R3 */
    /* gc = gwinfo->gc */
    gc = copy_gc;

    XCopyArea(dpy, WorkPort, gwinfo->panel, gc, left, top,
	      width, height, left, top);

    XSync(dpy, FALSE);
  }
}

VOID StGWResetBuffer(V)
{
  bufflevel = 0;
  buffering = FALSE;  
}

VOID StGWDumpImage(gwinfo, file, scale)
     StGWWinInfo *gwinfo;
     FILEP file;
     double scale;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  int left, top, width, height;
  static XImage *xi = NULL;
  unsigned long bc;
  int back_bit, fore_bit;
  int x, y, padright;

  if (scale <= 0) scale = 1.0;
  if (gwinfo == NULL || gwinfo->window == NullWindow || file == NullFile)
    return;

  /* clear image left from possibly interrupted previous call */
  if (xi != NULL) XDestroyImage(xi);

  /* get the image into the buffer */
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);
  StGWStartBuffering(gwinfo);
  StGWObRedraw((LVAL) gwinfo->Object);
  StGWResetBuffer();

  /* compute foreground and backgroung bit colors. Use white background */
  /* if the screen background is white, black otherwise.                */
  bc = get_color(gwinfo->backColor);
  back_bit = (bc == WhitePixel(dpy, screen)) ? 0 : 1;
  fore_bit = ! back_bit;

  /* Compute padding to round cols up to the nearest multiple of 8. */
  padright = ((width + 7) / 8) * 8 - width;

  /* read in the image a line at a time and write it out.               */
  /* The line at a time stuff may slow things down a little (probably   */
  /* not a lot, though) but it avoids huge allocations, especially on   */
  /* color screens. Taking 10 lines at a time would probably be better  */
  /* but this is simpler and seems to work.                             */
  psputinit(file, width, height, scale);
  for (y = 0; y < height; y++) {
    xi = XGetImage(dpy, WorkPort, 0, y, width, 1, AllPlanes, ZPixmap);
    if (xi == NULL) xlfail("Could not allocate image");
    for (x = 0; x < width; x++)
      psputbit((bc == XGetPixel(xi, x, 0)) ? back_bit : fore_bit);
    for (x = 0; x < padright; x++)
      psputbit( 0 );
    XDestroyImage(xi);
    xi = NULL;
  }
  psputrest();
}

/**************************************************************************/
/**                                                                      **/
/**                           Color Functions                            **/
/**                                                                      **/
/**************************************************************************/

# define NumBasicColors 8
# define MULTIPLIER 65535

static int NumColors;

typedef struct {
  int allocated;
  unsigned long value;
  long refcon;
} ctab_entry;

static ctab_entry *ctable;

LOCAL VOID allocate_named_color(dpy, screen, index, name)
     Display *dpy;
     int screen;
     int index;
     char *name;
{
  Colormap cmap = DefaultColormap(dpy, screen);
  XColor exact, color;

  if (StScreenHasColor()
      && XAllocNamedColor(dpy, cmap, name, &exact, &color) != 0) {
    ctable[index].allocated = TRUE;
    ctable[index].value = color.pixel;
  }
  else {
    ctable[index].allocated = FALSE;
    ctable[index].value = BlackPixel(dpy, screen);
  }
}
  
LOCAL VOID MakeColors()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();

  NumColors = NumBasicColors;
  ctable = (ctab_entry *) StCalloc(NumColors, sizeof(ctab_entry));

  ctable[0].value = WhitePixel(dpy, screen);
  ctable[1].value = BlackPixel(dpy, screen);
  allocate_named_color(dpy, screen, 2, "red");
  allocate_named_color(dpy, screen, 3, "green");
  allocate_named_color(dpy, screen, 4, "blue");
  allocate_named_color(dpy, screen, 5, "cyan");
  allocate_named_color(dpy, screen, 6, "magenta");
  allocate_named_color(dpy, screen, 7, "yellow");
}

#ifndef SERVER_COLOR_FREE_PROBLEM
LOCAL VOID FreeColors()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Colormap cmap = DefaultColormap(dpy, screen);
  int i;

  for (i = 2; i < NumColors; i++)
    if (ctable[i].allocated)
      XFreeColors(dpy, cmap, &ctable[i].value, 1, 0);
}
#endif /* SERVER_COLOR_FREE_PROBLEM */

VOID StGWSetColRefCon(index, rc)
     unsigned int index;
     long rc;
{
  if (index < NumColors) ctable[index].refcon = rc;
}

long StGWGetColRefCon(index)
	unsigned int index;
{	
  if (index < NumColors) return(ctable[index].refcon);
  else return 0;
}

LOCAL unsigned long get_color(index)
     ColorCode index;
{
  if (0 <= index && index < NumColors) return(ctable[index].value);
  else return(ctable[1].value);
}

int StGWMakeColor(red, green, blue, refcon)
        double red, green, blue;
        long refcon;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Colormap cmap = DefaultColormap(dpy, screen);
  XColor color;
  int index;
  char *temp;

  if (! StScreenHasColor()) return(-1);

  for (index = 0;
       index < NumColors && StGWGetColRefCon(index) != 0;
       index++);
  if (index >= NumColors) {
    temp = realloc(ctable, (NumColors + 1) * sizeof(ctab_entry));
    if (temp == NULL) return(-1);
    ctable = (ctab_entry *) temp;
    NumColors++;
    ctable[index].allocated = FALSE;
    ctable[index].refcon = 0;
    ctable[index].value = BlackPixel(dpy, screen);
  }
  color.red = MULTIPLIER * red;
  color.green = MULTIPLIER * green;
  color.blue = MULTIPLIER * blue;
  if (XAllocColor(dpy, cmap, &color) == 0) return(-1);
  else {
    ctable[index].allocated = TRUE;
    ctable[index].refcon = refcon;
    ctable[index].value = color.pixel;
    return(index);
  }
}

VOID StGWFreeColor(index)
        unsigned int index;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Colormap cmap = DefaultColormap(dpy, screen);

  if (index >= NumBasicColors && index < NumColors) {
    XFreeColors(dpy, cmap, &ctable[index].value, 1, 0);
    ctable[index].allocated = FALSE;
    ctable[index].refcon = 0;
    ctable[index].value = BlackPixel(dpy, screen);
  }
  else xlfail("can't free standard color");
}

LVAL xsparse_color()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Colormap cmap = DefaultColormap(dpy, screen);
  char *string;
  XColor color;
  LVAL result;

  string = (char *) getstring(xlgastring());
  xllastarg();

  if (XParseColor(dpy, cmap, string, &color) == 0) result = NIL;
  else {
    xlsave1(result);
    result = consa(cvflonum((FLOTYPE) ((double) color.blue) / MULTIPLIER));
    result = cons(cvflonum((FLOTYPE) ((double) color.green) / MULTIPLIER),
		  result);
    result = cons(cvflonum((FLOTYPE) ((double) color.red) / MULTIPLIER),
                  result);
    xlpop();
  }
  return(result);
}

/**************************************************************************/
/**                                                                      **/
/**                          Cursor Functions                            **/
/**                                                                      **/
/**************************************************************************/

#define NumBasicCursors 9
static int NumCursors;

typedef struct {
  Cursor curs;
  long refcon;
} cursor_entry;

static cursor_entry *curstab;

#define brush_width 16
#define brush_height 16
#define brush_x_hot 10
#define brush_y_hot 0
static unsigned char brush_bits[] = {
  0xfe, 0x0f, 0x54, 0x15, 0xa8, 0x2a, 0x08, 0x20, 0xf8, 0x3f, 0x08, 0x20,
  0x08, 0x20, 0x08, 0x20, 0xf8, 0x3f, 0x40, 0x04, 0x40, 0x04, 0x40, 0x04,
  0x40, 0x05, 0x40, 0x05, 0x40, 0x04, 0x80, 0x03};
#define brush_mask_width 16
#define brush_mask_height 16
static unsigned char brush_mask_bits[] = {
   0xfe, 0x0f, 0xfc, 0x1f, 0xf8, 0x3f, 0xf8, 0x3f, 0xf8, 0x3f, 0xf8, 0x3f,
   0xf8, 0x3f, 0xf8, 0x3f, 0xf8, 0x3f, 0xc0, 0x07, 0xc0, 0x07, 0xc0, 0x07,
   0xc0, 0x07, 0xc0, 0x07, 0xc0, 0x07, 0x80, 0x03};
static Pixmap BrushPM, BrushMaskPM;

#define hand_width 16
#define hand_height 16
#define hand_x_hot 8
#define hand_y_hot 0
static unsigned char hand_bits[] = {
  0x00, 0x01, 0xc0, 0x06, 0xa0, 0x1a, 0xa0, 0x2a, 0xa0, 0x2a, 0xa0, 0x2a,
  0xa2, 0x2a, 0x25, 0x20, 0x29, 0x20, 0x32, 0x20, 0x04, 0x20, 0x08, 0x20,
  0x10, 0x20, 0x20, 0x20, 0xc0, 0x3f, 0x00, 0x00};
#define hand_mask_width 16
#define hand_mask_height 16
static unsigned char hand_mask_bits[] = {
  0x00, 0x01, 0xc0, 0x07, 0xe0, 0x1f, 0xe0, 0x3f, 0xe0, 0x3f, 0xe0, 0x3f,
  0xe2, 0x3f, 0xe7, 0x3f, 0xef, 0x3f, 0xfe, 0x3f, 0xfc, 0x3f, 0xf8, 0x3f,
  0xf0, 0x3f, 0xe0, 0x3f, 0xc0, 0x3f, 0x00, 0x00};
static Pixmap HandPM, HandMaskPM;

#define finger_width 16
#define finger_height 16
#define finger_x_hot 6
#define finger_y_hot 0
static unsigned char finger_bits[] = {
  0x40, 0x00, 0xa0, 0x00, 0xa0, 0x00, 0xa0, 0x00, 0xa0, 0x05, 0xa0, 0x1a,
  0xb0, 0x2a, 0x28, 0x20, 0x24, 0x20, 0x24, 0x20, 0x04, 0x20, 0x08, 0x20,
  0x10, 0x20, 0x20, 0x20, 0xc0, 0x3f, 0x00, 0x00};
#define finger_mask_width 16
#define finger_mask_height 16
static unsigned char finger_mask_bits[] = {
  0x40, 0x00, 0xe0, 0x00, 0xe0, 0x00, 0xe0, 0x00, 0xe0, 0x05, 0xe0, 0x1f,
  0xf0, 0x3f, 0xf8, 0x3f, 0xfc, 0x3f, 0xfc, 0x3f, 0xfc, 0x3f, 0xf8, 0x3f,
  0xf0, 0x3f, 0xe0, 0x3f, 0xc0, 0x3f, 0x00, 0x00};
static Pixmap FingerPM, FingerMaskPM;

LOCAL VOID MakeGraphCursors()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Colormap cmap = DefaultColormap(dpy, screen);
  XColor black, white;

  NumCursors = NumBasicCursors;
  curstab = (cursor_entry *) StCalloc(NumCursors, sizeof(cursor_entry));

  /*** should check for errors here ***/
  XParseColor(dpy, cmap, "white", &white);
  XParseColor(dpy, cmap, "black", &black);

  curstab[ARROW_CURSOR].curs = 0;
  curstab[WATCH_CURSOR].curs = XCreateFontCursor(dpy, XC_watch);
  curstab[CROSS_CURSOR].curs = XCreateFontCursor(dpy, XC_cross);

  BrushPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				  (char *) brush_bits,
				  brush_width, brush_height);
  BrushMaskPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				      (char *) brush_mask_bits, 
				      brush_mask_width, brush_mask_height);
  curstab[BRUSH_CURSOR].curs = XCreatePixmapCursor(dpy, BrushPM, BrushMaskPM,
						   &black, &white,
						   brush_x_hot, brush_y_hot);
  XFreePixmap(dpy, BrushPM);
  XFreePixmap(dpy, BrushMaskPM);

  HandPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				 (char *) hand_bits, hand_width, hand_height);
  HandMaskPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				     (char *) hand_mask_bits, 
				     hand_mask_width, hand_mask_height);
  curstab[HAND_CURSOR].curs = XCreatePixmapCursor(dpy, HandPM, HandMaskPM,
						  &black, &white,
						  hand_x_hot, hand_y_hot);
  XFreePixmap(dpy, HandPM);
  XFreePixmap(dpy, HandMaskPM);

  FingerPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				   (char *) finger_bits,
				   finger_width, finger_height);
  FingerMaskPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				       (char *) finger_mask_bits, 
				       finger_mask_width, finger_mask_height);
  curstab[FINGER_CURSOR].curs = 
    XCreatePixmapCursor(dpy, FingerPM, FingerMaskPM,
			&black, &white, finger_x_hot, finger_y_hot);
  XFreePixmap(dpy, FingerPM);
  XFreePixmap(dpy, FingerMaskPM);
}

LOCAL VOID FreeGraphCursors()
{
  Display *dpy = StX11Display();
  int i;

  for (i = NumBasicCursors; i < NumCursors; i++)
    if (curstab[i].curs != 0)
      XFreeCursor(dpy, curstab[i].curs);
}

VOID StGWSetCursRefCon(index, rc)
     unsigned int index;
     long rc;
{
  if (index < NumCursors) curstab[index].refcon = rc;
}

long StGWGetCursRefCon(index)
	unsigned int index;
{	
  if (index < NumCursors) return(curstab[index].refcon);
  else return 0;
}

VOID StGWSetCursor(gwinfo, cursor) 
     StGWWinInfo *gwinfo;
     int cursor;
{
  Display *dpy = StX11Display();

  if (cursor < 0) cursor = 0;
  if (gwinfo != NULL) {
    gwinfo->cursor = cursor;
    if (gwinfo->panel != NullWindow) {
      if (cursor < NumCursors && curstab[cursor].curs != 0)
	XDefineCursor(dpy, gwinfo->panel, curstab[cursor].curs);
      else XDefineCursor(dpy, gwinfo->panel, ArrowCursor);
    }
  }
}

int StGWCursor(gwinfo)
	StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return(ARROW_CURSOR);
  return(gwinfo->cursor);
}

/* This routine is adapted from the code in Jef Poskanzer's pbm package. */
LOCAL Pixmap make_pixmap(width, height, image)
     int width, height;
     char *image;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  int row_bytes, i, j, bit, val;
  char *data;
  Pixmap pm;

  row_bytes = (width % 8 == 0) ? width / 8 : width / 8 + 1;

  if (height * row_bytes <= 0) return NullPixmap;
  data = StCalloc(height * row_bytes, 1);
  if (data == NULL) return NullPixmap;

  for (i = 0; i < height; i++) {
    for (j = 0, val = 0; j < 8 * row_bytes; j++) {
      bit = (j < width) ? image[i * width + j] : 0;
      if (bit) val += 1 << (j % 8);
      if (j % 8 == 7) {
	data[i * row_bytes + j / 8] = val;
	val = 0;
      }
    }
  }
  pm = XCreateBitmapFromData(dpy, RootWindow(dpy, screen), 
			     data, width, height);

  StFree(data);
  return(pm);
}
  
int StGWMakeCursor(n, image, mask, h, v, refcon)
     int n, h, v;
     char *image, *mask;
     long refcon;
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Colormap cmap = DefaultColormap(dpy, screen);
  int index;
  char *temp;
  Pixmap CursPM, MaskPM;
  XColor black, white;

  /**** check for negative n, h, v? */

  if (XParseColor(dpy, cmap, "white", &white) == 0) return(-1);
  if (XParseColor(dpy, cmap, "black", &black) == 0) return(-1);

  if (image == NULL) return(-1);
  for (index = 0;
       index < NumCursors && StGWGetCursRefCon(index) != 0;
       index++);
    if (index >= NumCursors) {
    temp = realloc(curstab, (NumCursors + 1) * sizeof(cursor_entry));
    if (temp == NULL) return(-1);
    curstab = (cursor_entry *) temp;
    NumCursors++;
    curstab[index].curs = 0;
    curstab[index].refcon = 0;
  }
  if (curstab[index].curs != 0) XFreeCursor(dpy, curstab[index].curs);
  
  if (h >= n) h = n - 1;
  if (v >= n) v = n - 1;
  CursPM = make_pixmap(n, n, image);
  MaskPM = (mask != NULL) ? make_pixmap(n, n, image) : None;

  if (CursPM != NullPixmap && (mask == NULL || MaskPM != NullPixmap)) {
    curstab[index].curs = XCreatePixmapCursor(dpy, CursPM, MaskPM,
					      &black, &white, h, v);
    curstab[index].refcon = refcon;
  }
  else index = -1;

  if (CursPM != NullPixmap) XFreePixmap(dpy, CursPM);
  if (mask != NULL && MaskPM != NullPixmap) XFreePixmap(dpy, MaskPM);

  return(index);
}

int StGWMakeResCursor(name, num, refcon)
     char *name;
     int num;
     long refcon;
{ 
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  Colormap cmap = DefaultColormap(dpy, screen);
  unsigned int width, height;
  int index, v, h;
  char *temp, *mname;
  Pixmap CursPM, MaskPM;
  XColor black, white;

  for (index = 0;
       index < NumCursors && StGWGetCursRefCon(index) != 0;
       index++);
    if (index >= NumCursors) {
    temp = realloc(curstab, (NumCursors + 1) * sizeof(cursor_entry));
    if (temp == NULL) return(-1);
    curstab = (cursor_entry *) temp;
    NumCursors++;
    curstab[index].curs = 0;
    curstab[index].refcon = 0;
  }
  if (curstab[index].curs != 0) XFreeCursor(dpy, curstab[index].curs);
  
  if (name != NULL) {
    mname = (moreargs()) ? (char *) getstring(xlgastring()) : NULL;

    if (XParseColor(dpy, cmap, "white", &white) == 0) return(-1);
    if (XParseColor(dpy, cmap, "black", &black) == 0) return(-1);

    if (XReadBitmapFile(dpy, RootWindow(dpy, screen), name, 
			&width, &height, &CursPM, &h, &v) != BitmapSuccess)
      xlfail("can't read bitmap file");
    if (h >= width) h = width - 1; if (h < 0) h = 0;
    if (v >= height) v = height - 1; if (v < 0) v = 0;

    if (mname != NULL) {
      if (XReadBitmapFile(dpy, RootWindow(dpy, screen), mname,
			  &width, &height, &MaskPM, 0, 0) != BitmapSuccess) {
	XFreePixmap(dpy, CursPM);
	xlfail("can't read mask file");
      }
    }
    else MaskPM = None;

    curstab[index].curs = XCreatePixmapCursor(dpy, CursPM, MaskPM,
					      &black, &white, h, v);
    curstab[index].refcon = refcon;

    XFreePixmap(dpy, CursPM);
    if (MaskPM != None) XFreePixmap(dpy, MaskPM);
  }
  else {
    if (num < 0 || num > 127) index = -1;
    else {
      curstab[index].curs = XCreateFontCursor(dpy, num);
      if (curstab[index].curs != 0) curstab[index].refcon = refcon;
      else index = -1;
    }
  }
  return(index);
}

VOID StGWFreeCursor(index)
     unsigned int index;
{
  Display *dpy = StX11Display();

  if (index < NumCursors && index >= NumBasicCursors) {
    if (curstab[index].curs != 0)
      XFreeCursor(dpy, curstab[index].curs);
    curstab[index].curs = 0;
    curstab[index].refcon = 0;
  }
  else xlfail("can't free standard cursor");
}

LVAL xsbest_cursor_size()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  int width, height;
  unsigned int rwidth, rheight;

  if (moreargs()) {
    width = getfixnum(xlgafixnum());
    height = getfixnum(xlgafixnum());
  }
  else {
    width = 16;
    height = 16;
  }
  xllastarg();

  XQueryBestCursor(dpy, RootWindow(dpy, screen), 
		   width, height, &rwidth, &rheight);
  return(integer_list_2((int) rwidth, (int) rheight));
}

LVAL xsbitmap_from_file() { xlfail("not supported"); return NIL; }

/**************************************************************************/
/**                                                                      **/
/**                         Bitmap Functions                             **/
/**                                                                      **/
/**************************************************************************/

VOID StGWDrawBitmap(gwinfo, left, top, width, height, image)
     StGWWinInfo *gwinfo;
     int left, top, width, height;
     char *image;
{
  Display *dpy = StX11Display();
  Pixmap PM;

  PM = make_pixmap(width, height, image);
  if (PM != NullPixmap) {
    XCopyPlane(dpy, PM, get_drawable(gwinfo), draw_gc(gwinfo),
               0, 0, width, height,
	       left - gwinfo->view_h, top - gwinfo->view_v, 1);
    XFreePixmap(dpy, PM);
  }
}

/**************************************************************************/
/**                                                                      **/
/**                        Button Down Action                            **/
/**                                                                      **/
/**************************************************************************/

VOID StX11PressButton(V)   { pointer_button_down = TRUE; }
VOID StX11ReleaseButton(V) { pointer_button_down = FALSE; }
int  StX11ButtonIsDown(V)  { return(pointer_button_down); }

VOID StGWWhileButtonDown(gwinfo, action, motionOnly)
     StGWWinInfo *gwinfo;
     VOID (*action) _((IVIEW_WINDOW, int, int));
     int motionOnly;
{
  Display *dpy = StX11Display();
  XEvent report;
  Window win;
  int x, y;
  int moved;

  if (StX11ButtonIsDown() && is_allocated(gwinfo)) {
    win = gwinfo->window;
    while (! XCheckMaskEvent(dpy, ButtonReleaseMask, &report)) {
      x = gwinfo->mouse_x;
      y = gwinfo->mouse_y;
      while (XCheckMaskEvent(dpy, ButtonMotionMask, &report)) {
	x = report.xmotion.x + gwinfo->view_h;
	y = report.xmotion.y + gwinfo->view_v;
      }
      moved = (gwinfo->mouse_x != x || gwinfo->mouse_y != y) ? TRUE : FALSE;
      if (moved || ! motionOnly) {
	if (action != NULL) (*action)(win, x, y);
	if (moved) XSync(dpy, FALSE);
      }
      gwinfo->mouse_x = x;
      gwinfo->mouse_y = y;
    }
  }
}

/**************************************************************************/
/**                                                                      **/
/**                          Clipping Functions                          **/
/**                                                                      **/
/**************************************************************************/

int StGWGetClipRect(gwinfo, a, b, c, d) 
     StGWWinInfo *gwinfo;
     int *a, *b,*c, *d;
{
  if (gwinfo != NULL) {
    if (a != NULL) *a = gwinfo->clip_left;
    if (b != NULL) *b = gwinfo->clip_top;
    if (c != NULL) *c = gwinfo->clip_width;
    if (d != NULL) *d = gwinfo->clip_height;
  }
  return(gwinfo->clipped);
}

LOCAL VOID set_gc_clip_regions(gwinfo)
     StGWWinInfo *gwinfo;
{
  Display *dpy = StX11Display();
  XRectangle r;
  
  if (! do_clipping) return;

  if (gwinfo->clipped) {
    r.x = gwinfo->clip_left - gwinfo->view_h;
    r.y = gwinfo->clip_top - gwinfo->view_v;
    r.width = gwinfo->clip_width;
    r.height = gwinfo->clip_height;
    XSetClipRectangles(dpy, gwinfo->gc, 0, 0, &r, 1, Unsorted);
    XSetClipRectangles(dpy, gwinfo->erase_gc, 0, 0, &r, 1, Unsorted);
    XSetClipRectangles(dpy, gwinfo->xor_gc, 0, 0, &r, 1, Unsorted);
  }
  else {
    XSetClipMask(dpy, gwinfo->gc, None);
    XSetClipMask(dpy, gwinfo->erase_gc, None);
    XSetClipMask(dpy, gwinfo->xor_gc, None);
  }
}

VOID StGWSetClipRect(gwinfo, clipped, left, top, width, height)
        StGWWinInfo *gwinfo;
        int clipped, left, top, width, height;
{
  if (gwinfo != NULL) {
    gwinfo->clipped = clipped;
    if (clipped) {
      gwinfo->clip_left = left;
      gwinfo->clip_top = top;
      gwinfo->clip_width = width;
      gwinfo->clip_height = height;
    }
    if (gwinfo->gc != NULL) set_gc_clip_regions(gwinfo);
  }
}

/**************************************************************************/
/**                                                                      **/
/**                             Idle Action                              **/
/**                                                                      **/
/**************************************************************************/

extern LVAL s_hardware_objects, s_hardware_address, xlenv, xlfenv;

#define IDLE_FREQUENCY 15

LOCAL int is_allocated_graph(object)
     LVAL object;
{
  return(valid_iview_window_address(slot_value(object, s_hardware_address)));
}

LOCAL LVAL entry_object(entry)
     LVAL entry;
{
  if (consp(entry) && consp(cdr(entry)) && consp(cdr(cdr(entry))))
    return(car(cdr(cdr(entry))));
  else return(NIL);
}

LOCAL VOID do_idle_actions()
{
  LVAL object, list = getvalue(s_hardware_objects);
  StGWWinInfo *gwinfo;
  static int count = IDLE_FREQUENCY;

  if (count > 0) count--;
  else {
    count = IDLE_FREQUENCY;
    for (; consp(list); list = cdr(list)) {
      object = entry_object(car(list));
      if (is_allocated_graph(object)) {
	gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
	if (gwinfo != NULL && gwinfo->idleOn) StGWObDoIdle(object);
      }
    }
    background_tasks();
  }
}

LOCAL VOID background_tasks()
{
  LVAL task, queue, oldenv, oldfenv;
    
  queue = getvalue(s_event_queue);
  if (consp(queue)) {
  
    /* set the lexical environment to null */
    xlstkcheck(2);
    xlsave(oldenv);
    xlsave(oldfenv);
    oldenv = xlenv; xlenv = NIL;
    oldfenv = xlfenv; xlfenv = NIL;

    task = car(queue);
    setvalue(s_event_queue, cdr(queue));
    xleval(task);

    /* reset the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;
    xlpopn(2);
  }
}

LOCAL int any_idle_tasks()
{
  LVAL object, list = getvalue(s_hardware_objects);
  LVAL queue = getvalue(s_event_queue);
  StGWWinInfo *gwinfo;

  if (consp(queue)) return(TRUE);

  for (; consp(list); list = cdr(list)) {
    object = entry_object(car(list));
    if (is_allocated_graph(object)) {
      gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
      if (gwinfo != NULL && gwinfo->idleOn) return(TRUE);
    }
  }
  return(FALSE);
}  

/**************************************************************************/
/**                                                                      **/
/**                       Miscellaneous Functions                        **/
/**                                                                      **/
/**************************************************************************/

VOID StGWSetRefCon(gwinfo, x)
     StGWWinInfo *gwinfo;
     long x;
{
  if (gwinfo == NULL) return;
  else gwinfo->RefCon = x;
}

long StGWGetRefCon(gwinfo)
     StGWWinInfo *gwinfo;
{
  if (gwinfo == NULL) return 0;
  else return(gwinfo->RefCon);
}

VOID StGWSetObject(gwinfo, x)
     StGWWinInfo *gwinfo;
     LVAL x;
{
  if (gwinfo == NULL) return;
  else gwinfo->Object = x;
}

LVAL IViewWindowGetObject(w)
     Window w;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(w);
  if (gwinfo == NULL) return(NIL);
  else return(gwinfo->Object);
}

StGWWinInfo *IViewWindowWinInfo(w)
	Window w;
{
  return((w != NullWindow) ? (StGWWinInfo *) GetWRefCon(w) : NULL);
}

VOID StGWSetFreeMem(gwinfo, FreeMem)
     StGWWinInfo *gwinfo;
     VOID (*FreeMem) _((IVIEW_WINDOW));
{
  if (gwinfo == NULL) return;
  else gwinfo->FreeMem = FreeMem;
}

VOID StGWInitialDraw(gwinfo)
     StGWWinInfo *gwinfo;
{
  int width, height;
  XEvent report;
  Display *dpy = StX11Display();

  if (is_allocated(gwinfo) && ! gwinfo->initialized) {
    if (wait_for_map) {
      do {
	XNextEvent(dpy, &report);
	StProcessEvent(dpy, report);
      } while (report.xany.window != gwinfo->window
	       || report.type != MapNotify);
    }
    StWGetSize(gwinfo->window, &width, &height, FALSE);
    if (! gwinfo->hasVscroll) gwinfo->canvasHeight = height;
    if (! gwinfo->hasHscroll) gwinfo->canvasWidth = width;
    adjust_scroll_bars(gwinfo, TRUE);

    StGWObResize((LVAL) gwinfo->Object);
    StGWObRedraw((LVAL) gwinfo->Object);
    gwinfo->initialized = TRUE;
  }
}

/**************************************************************************/
/**                                                                      **/
/**                       Window Scrolling Functions                     **/
/**                                                                      **/
/**************************************************************************/

LOCAL VOID StX11ObScrollAction(object, s, which, x, y)
     LVAL object;
     Window s;
     int which, x, y;
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
  int is_h_scroll, val, max, page, width, height, pos, v, h, inc;

  if (is_allocated(gwinfo)) {
    StWGetSize(gwinfo->window, &width, &height, FALSE);
    is_h_scroll = (s == gwinfo->hscroll) ? TRUE : FALSE;
    val = (is_h_scroll) ? gwinfo->view_h : gwinfo->view_v;
    max = (is_h_scroll) ? gwinfo->canvasWidth : gwinfo->canvasHeight;
    page = (is_h_scroll) ? width : height;
    pos = (is_h_scroll) ? x : y;
    inc = (is_h_scroll) ? gwinfo->h_scroll_inc[1] : gwinfo->v_scroll_inc[1];
    
    switch (which) {
    case 'M':
      val = (page > 0) ? ((double) pos / (double) page) * max : val;
      break;
    case 'L': val += inc; break;
    case 'R': val -= inc; break;
    }

    h = (is_h_scroll) ? val : gwinfo->view_h;
    v = (is_h_scroll) ? gwinfo->view_v : val;
    StGWSetScroll(gwinfo, h, v, FALSE);
    StGWObRedraw(object);
  }
}
  
LOCAL VOID adjust_scroll_bars(gwinfo, adjust_showing)
     StGWWinInfo *gwinfo;
     int adjust_showing;
{
  int width, height, top;

  StWGetSize(gwinfo->window, &width, &height, FALSE);
  
  if (adjust_showing) {
    top = ClosePanelHeight();
    if (gwinfo->hasVscroll)
      ShowScrollBar(gwinfo->vscroll, width, top, ScrollWidth, height);
    else HideScrollBar(gwinfo->vscroll);
    if (gwinfo->hasHscroll)
      ShowScrollBar(gwinfo->hscroll, 0, top + height, width, ScrollWidth);
    else HideScrollBar(gwinfo->hscroll);
  }
  
  gwinfo->view_h = max(0, min(gwinfo->view_h, gwinfo->canvasWidth - width));
  gwinfo->view_v = max(0, min(gwinfo->view_v, gwinfo->canvasHeight - height));
  if (gwinfo->hasVscroll) 
    AdjustScrollBar(gwinfo->vscroll, 
		    gwinfo->view_v, height, gwinfo->canvasHeight);
  if (gwinfo->hasHscroll)
    AdjustScrollBar(gwinfo->hscroll,
                    gwinfo->view_h, width, gwinfo->canvasWidth);
}

     
LOCAL VOID set_has_scroll(gwinfo, which, has, size)
        StGWWinInfo *gwinfo;
        int which, has, size;
{
  Display *dpy = StX11Display();
  int view_size, old_has, width, height;

  if (is_allocated(gwinfo)) {
    if (has && size <= 0) xlfail("size must be positive");

    old_has = (which == 'H') ? gwinfo->hasHscroll : gwinfo->hasVscroll;
    if (which == 'H') gwinfo->hasHscroll = has;
    else gwinfo->hasVscroll = has;
    
    if (has) {
      if (which == 'H') {
	if (! old_has) gwinfo->canvasHeight -= ScrollWidth;
	gwinfo->canvasWidth = size;
	StGWGetViewRect(gwinfo, NULL, NULL, &view_size, NULL);
      }
      else {
	if (! old_has) gwinfo->canvasWidth -= ScrollWidth;
	gwinfo->canvasHeight = size;
	StGWGetViewRect(gwinfo, NULL, NULL, NULL, &view_size);
      }
    }
    else {
      if (which == 'H') {
	StGWGetViewRect(gwinfo, NULL, NULL, &gwinfo->canvasWidth, NULL);
	if (old_has) gwinfo->canvasHeight += ScrollWidth;
	StGWSetScroll(gwinfo, 0, gwinfo->view_v, TRUE);
      }
      else {
	StGWGetViewRect(gwinfo, NULL, NULL, NULL, &gwinfo->canvasHeight);
	if (old_has) gwinfo->canvasWidth += ScrollWidth;
	StGWSetScroll(gwinfo, gwinfo->view_h, 0, TRUE);
      }
    }
    StWGetSize(gwinfo->window, &width, &height, FALSE);
    XResizeWindow(dpy, gwinfo->panel, width, height);
    XFlush(dpy);
    set_gc_clip_regions(gwinfo);
    adjust_scroll_bars(gwinfo, TRUE);
    StGWObResize((LVAL) gwinfo->Object);
  }
}

VOID StGWSetHasHscroll(gwinfo, has, size)
     StGWWinInfo *gwinfo;
     int has, size;
{
  set_has_scroll(gwinfo, 'H', has, size);
}

VOID StGWSetHasVscroll(gwinfo, has, size)
     StGWWinInfo *gwinfo;
     int has, size;
{
  set_has_scroll(gwinfo, 'V', has, size);
}

int StGWHasHscroll(gwinfo)
	StGWWinInfo *gwinfo;
{
  if (is_allocated(gwinfo)) return(gwinfo->hasHscroll);
  else return(FALSE);
}

int StGWHasVscroll(gwinfo)
	StGWWinInfo *gwinfo;
{
  if (is_allocated(gwinfo)) return(gwinfo->hasVscroll);
  else return(FALSE);
}

VOID StGWSetScroll(gwinfo, h, v, move)
     StGWWinInfo *gwinfo;
     int h, v, move;
{
  if (is_allocated(gwinfo)) {
    gwinfo->view_h = (gwinfo->hasHscroll) ? h : 0;
    gwinfo->view_v = (gwinfo->hasVscroll) ? v : 0;

    set_gc_clip_regions(gwinfo);
    adjust_scroll_bars(gwinfo, FALSE);
  }
}

VOID StGWGetScroll(gwinfo, h, v)
     StGWWinInfo *gwinfo;
     int *h, *v;
{
  
  if (is_allocated(gwinfo)) {
    if (h != NULL) *h = gwinfo->view_h;
    if (v != NULL) *v = gwinfo->view_v;
  }
}

VOID StGWSetHscrollIncs(gwinfo, inc, pageInc)
     StGWWinInfo *gwinfo;
     int inc, pageInc;
{
  if (is_allocated(gwinfo)) {
    gwinfo->h_scroll_inc[0] = inc;
    gwinfo->h_scroll_inc[1] = pageInc;
  }
}

VOID StGWGetHscrollIncs(gwinfo, inc, pageInc)
     StGWWinInfo *gwinfo;
     int *inc, *pageInc;
{
  if (is_allocated(gwinfo)) {
    if (inc != 0) *inc = gwinfo->h_scroll_inc[0];
    if (pageInc != 0) *pageInc = gwinfo->h_scroll_inc[1];
  }
}

VOID StGWSetVscrollIncs(gwinfo, inc, pageInc)
     StGWWinInfo *gwinfo;
     int inc, pageInc;
{
  if (is_allocated(gwinfo)) {
    gwinfo->v_scroll_inc[0] = inc;
    gwinfo->v_scroll_inc[1] = pageInc;
  }
}

VOID StGWGetVscrollIncs(gwinfo, inc, pageInc)
     StGWWinInfo *gwinfo;
     int *inc, *pageInc;
{
  if (is_allocated(gwinfo)) {
    if (inc != 0) *inc = gwinfo->v_scroll_inc[0];
    if (pageInc != 0) *pageInc = gwinfo->v_scroll_inc[1];
  }
}

#ifdef TODO
close/menu buttons at top of X modeless dialog and graphics windows
menus for Macintosh modeless dialogs/sun modeless dialogs
X protocol error trapping
#endif /* TODO */
