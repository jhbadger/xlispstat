/* X11menus - Low Level Menu Objects for X11                           */
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

#define NullWindow ((Window) 0)

extern Display *StX11Display();

extern LVAL slot_value();
extern caddr_t get_menu_address();

extern LVAL s_items, s_title, s_enabled, s_mark, sk_update;

typedef struct {
  char *title;
  int len, enabled, checked;
  Window pane;
} ItemEntry;

typedef struct {
  unsigned long fore, back;
} ColorPair;

/***********************************************************************/
/**                                                                   **/
/**                      Static Global Variables                      **/
/**                                                                   **/
/***********************************************************************/

/* gray stipple pattern bitmap data for disabled items */
#define gray_width 16
#define gray_height 16
static unsigned char gray_bits[] = {
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa};
static Pixmap GrayPM;

/* check mark bitmap data */
#define check_offset 2
#define check_width 9
#define check_height 8
static unsigned char check_bits[] = {
  0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
  0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
};
static Pixmap CheckPM;

/* configuration parameters - should be set using the defaults database */
static char *MenuFontName = "9x15";
static XFontStruct *MenuFont;
static unsigned long BorderColor;
static ColorPair MenuC, MenuItemC, MenuTitleC;
static unsigned int border_width, item_border_width;
static int show_menu_title;

/* graphics contexts used for menu items */
static GC NormalGC, ReversedGC, TitleGC, GrayGC;

/* current menu item's window pane - used in event handling */
static Window current_item;

static Cursor MenuCursor;

/* forward declarations */
LOCAL VOID LoadMenuFont _((void));
LOCAL VOID MakeMenuGC _((void));
LOCAL VOID get_menu_size _((LVAL menu, LVAL items,
			    ItemEntry *entries,
			    unsigned int *pwidth, unsigned int *pheight));
LOCAL VOID draw_pane _((int n, ItemEntry *entries, Window win));

/***********************************************************************/
/**                                                                   **/
/**               Menu System Initialization and Cleanup              **/
/**                                                                   **/
/***********************************************************************/

VOID StX11InitMenus()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  char *option;

  border_width = 1;
  item_border_width = 0;

  option = StX11GetDefault("xlisp.menu.titles");
  if (option == NULL) option = "off";
  show_menu_title = is_option_on(option);

  MenuC.fore = BlackPixel(dpy, screen);
  MenuC.back = WhitePixel(dpy, screen);
  MenuItemC.fore = BlackPixel(dpy, screen);
  MenuItemC.back = WhitePixel(dpy, screen);
  MenuTitleC.fore = WhitePixel(dpy, screen);
  MenuTitleC.back = BlackPixel(dpy, screen);
  BorderColor = BlackPixel(dpy, screen);
  
  GrayPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen), 
				 (char *) gray_bits,
				 gray_width, gray_height);
  CheckPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen), 
				  (char *) check_bits,
				  check_width, check_height);

  LoadMenuFont();
  MakeMenuGC();

  MenuCursor = XCreateFontCursor(dpy, XC_sb_left_arrow);
}

LOCAL VOID LoadMenuFont()
{
  Display *dpy = StX11Display();
  char *font;

  font = StX11GetDefault("xlisp.menu.font");
  if (font == NULL) font = StX11GetDefault("xlisp.font");
  if (font == NULL) font = MenuFontName;
  if ((MenuFont = XLoadQueryFont(dpy, font)) == NULL) {
    fprintf(stderr, "xlisp: Can't open %s font\n", font);
    if ((MenuFont = XLoadQueryFont(dpy, MenuFontName)) == NULL) {
      fprintf(stderr, "xlisp: Can't open %s font\n", MenuFontName);
      if ((MenuFont = XLoadQueryFont(dpy, "fixed")) == NULL) {
	fprintf(stderr, "xlisp: Can't open %s font\n", "fixed");
	exit(-1);
      }
    }
  }
}

LOCAL VOID MakeMenuGC()
{
  unsigned long valuemask;
  XGCValues values;
  Display *dpy = StX11Display();
  int screen = StX11Screen();

  valuemask = 0; /* ignore XGCValues and use defaults */
  NormalGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, NormalGC, MenuFont->fid);
  XSetForeground(dpy, NormalGC, MenuItemC.fore);
  XSetBackground(dpy, NormalGC, MenuItemC.back);

  valuemask = 0; /* ignore XGCValues and use defaults */
  ReversedGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, ReversedGC, MenuFont->fid);
  XSetForeground(dpy, ReversedGC, MenuItemC.back);
  XSetBackground(dpy, ReversedGC, MenuItemC.fore);

  valuemask = GCStipple+GCFillStyle;
  values.stipple = GrayPM;
  values.fill_style = FillStippled;
  GrayGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, GrayGC, MenuFont->fid);
  XSetForeground(dpy, GrayGC, MenuItemC.fore);
  XSetBackground(dpy, GrayGC, MenuItemC.back);

  valuemask = 0; /* ignore XGCValues and use defaults */
  TitleGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, TitleGC, MenuFont->fid);
  XSetForeground(dpy, TitleGC, MenuTitleC.fore);
  XSetBackground(dpy, TitleGC, MenuTitleC.back);
}

VOID StX11FinishMenus()
{
  Display *dpy = StX11Display();

  XUnloadFont(dpy, MenuFont->fid);
  XFreeGC(dpy, NormalGC);
  XFreeGC(dpy, ReversedGC);
  XFreeGC(dpy, GrayGC);
  XFreeGC(dpy, TitleGC);
  XFreePixmap(dpy, GrayPM);
  XFreePixmap(dpy, CheckPM);
  XFreeCursor(dpy, MenuCursor);
}

/***********************************************************************/
/**                                                                   **/
/**                       MENU-PROTO Definitions                      **/
/**                                                                   **/
/***********************************************************************/

int StMObInstalled(m) LVAL m; { return(FALSE); }

/***********************************************************************/
/**                                                                   **/
/**                       MENU-PROTO Definitions                      **/
/**                                                                   **/
/***********************************************************************/

FORWARD char *get_item_string();

/***********************************************************************/
/**                                                                   **/
/**                      Public Menu Functions                        **/
/**                                                                   **/
/***********************************************************************/

/* unused routines in popup system */
VOID StMObDisposeMach (m) LVAL m; {}
VOID StMObAllocateMach (m) LVAL m; {}
VOID StMObDeleteItem (m, item) LVAL m, item; {}
VOID StMObSetItemProp (m, i) LVAL m; int i; {}
VOID StMObAppendItems (m, a) LVAL m, a; {}
VOID StMObRemove (m) LVAL m; {}
VOID StMObEnable (m, i) LVAL m; int i; {}
VOID StMObInstall (m) LVAL m; {}

int StMObPopup (menu,x, y, window)
     LVAL menu, window;
     int x, y;
{
  Window win, w;
  unsigned int width, height;
  XSetWindowAttributes setwinattr;
  unsigned long valuemask;
  XEvent report;
  int done = FALSE;
  LVAL items;
  int i, n, pane_height, left, top;
  ItemEntry *entries;
  int item_selected;
  Display *dpy = StX11Display();
  int screen = StX11Screen();

  /* adjust coordinates to the root window */
  if (window != NIL && (w = (Window) GETWINDOWADDRESS(window)) != NullWindow) {
    StWGetLocation(w, &left, &top, FALSE);
    x += left;
    y += top;
  }

  /* have the menu update itself */
  send_message(menu, sk_update);

  /* get the item list and make sure there are some items to use */
  items = slot_value(menu, s_items);
  n = (consp(items)) ? llength(items) : 0;
  if (n == 0) return(0);
  if (show_menu_title) n++;

  /* set up the internal item entry array */
  entries = (ItemEntry *) StCalloc(n, sizeof(ItemEntry));
  get_menu_size(menu, items, entries, &width, &height);

  /* create opaque menu window */
  x = max(0, min(x, DisplayWidth(dpy, screen) - width));
  y = max(0, min(y, DisplayHeight(dpy, screen) - height));
  win = XCreateSimpleWindow(dpy, RootWindow(dpy, screen),
			    x, y, width, height, border_width,
			    BorderColor, MenuC.back);

  /* set the override_redirect and save_under attributes for a menu */
  valuemask = CWOverrideRedirect | CWSaveUnder;
  setwinattr.override_redirect = TRUE;
  setwinattr.save_under = TRUE;
  XChangeWindowAttributes(dpy, win, valuemask, &setwinattr);
  XDefineCursor(dpy, win, MenuCursor);
  
  /* create the title and item windows */
  pane_height = height / n;
  for (i = 0; i < n; i++) {
    entries[i].pane = XCreateSimpleWindow(dpy, win, 0, pane_height * i, 
					  width, pane_height,
					  item_border_width, 
					  BorderColor, MenuItemC.back);
    XSelectInput(dpy, entries[i].pane, 
		 ExposureMask | EnterWindowMask | LeaveWindowMask);
  }
				      
  /* Display (map) the windows */
  XMapSubwindows(dpy, win);
  XMapWindow(dpy, win);

  /* grap the pointer */
  StX11ReleaseButton();
  XGrabPointer(dpy, win, TRUE, 
	       ButtonReleaseMask,
	       GrabModeAsync, GrabModeAsync, None, None, CurrentTime);

  /* Loop until button is released, examining each event */
  current_item = NullWindow;
  while (! done) {
    XNextEvent(dpy, &report);
    switch (report.type) {
    case Expose:
      draw_pane(n, entries, report.xexpose.window);
      break;
    case ButtonPress:
      break;
    case ButtonRelease:
      done = TRUE;
      break;
    case EnterNotify:
      current_item = report.xcrossing.window;
      draw_pane(n, entries, current_item);
      break;
    case LeaveNotify:
      current_item = NullWindow;
      draw_pane(n, entries, report.xcrossing.window);
      break;
    default:
      break;
    }
  }

  /* find the item selected */
  for (item_selected = 0, i = 0; i < n; i++) {
    if (current_item == entries[i].pane) {
      if (entries[i].enabled) item_selected =  (show_menu_title) ? i : i + 1;
      break;
    }
  }

  /* clean up */
  XDestroyWindow(dpy, win);
  XFlush(dpy);
  StFree(entries);
  entries = NULL;

  return(item_selected);
}

LOCAL VOID get_menu_size(menu, items, entries, pwidth, pheight)
     LVAL menu, items;
     ItemEntry *entries;
     unsigned int *pwidth, *pheight;
{
  int font_height, margin, text_width, len;
  LVAL title;
  char *str;

  font_height = MenuFont->max_bounds.ascent
              + MenuFont->max_bounds.descent;
  margin = MenuFont->max_bounds.descent / 2;
  
  if (show_menu_title) {
    title = slot_value(menu, s_title);
    if (! stringp(title)) xlerror("not a string", title);
    str = getstring(title);
    len = strlen(str);
    text_width = XTextWidth(MenuFont, str, len);
    entries->title = str;
    entries->len = len;
    entries->enabled = FALSE;
    entries->checked = FALSE;
    *pwidth = text_width;
    *pheight = font_height + 2 * margin;
    entries++;
  }
  else {
    *pwidth = 0;
    *pheight = 0;
  }
  for (; consp(items); items = cdr(items), entries++) {
    *pheight += font_height + 2 * margin;
    title = slot_value(car(items), s_title);
    if (! stringp(title)) xlerror("not a string", title);
    str = getstring(title);
    len = strlen(str);
    text_width = XTextWidth(MenuFont, str, len);
    if (*pwidth < text_width) *pwidth = text_width;
    entries->title = str;
    entries->len = len;
    entries->enabled = (slot_value(car(items), s_enabled) != NIL);
    entries->checked = (slot_value(car(items), s_mark) != NIL);
  }
  *pwidth += margin + check_width + 2 * check_offset;
}

LOCAL VOID draw_pane(n, entries, win)
     int n;
     ItemEntry *entries;
     Window win;
{
  int margin, x, y, i;
  GC gc;
  Display *dpy = StX11Display();

  margin = MenuFont->max_bounds.descent / 2;

  x = check_width + 2 * check_offset;
  y = MenuFont->max_bounds.ascent + margin; 
  
  for (i = 0; i < n; i++) {
    if (entries[i].pane == win) {
      if (show_menu_title && i == 0) {
	XSetWindowBackground(dpy, win, MenuTitleC.back);
	gc = TitleGC;
      }
      else if (win == current_item && entries[i].enabled) {
	XSetWindowBackground(dpy, win, MenuItemC.fore);
	gc = ReversedGC;
      }
      else {
	XSetWindowBackground(dpy, win, MenuItemC.back);
	gc = (entries[i].enabled) ? NormalGC : GrayGC;
      }
      XClearWindow(dpy, win);
      if (entries[i].checked)
	XCopyPlane(dpy, CheckPM, win, gc, 
		   0, 0, check_width, check_height,
		   check_offset, y - check_height, 1);
      XDrawString(dpy, win, gc, x, y, entries[i].title, entries[i].len);    
      break;
    }
  }
}

#ifdef TODO
  special dividing lines
  use resource database to get defaults
#endif /* TODO */
