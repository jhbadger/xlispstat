/* X11dialogs - Low level dialog objects for X11                       */
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

#define NullWindow ((Window) 0)

extern Display *StX11Display();
extern LVAL s_window_id, sk_show_window;

typedef struct {
  unsigned long fore, back;
} ColorPair;

/***********************************************************************/
/**                                                                   **/
/**                        Global Variables                           **/
/**                                                                   **/
/***********************************************************************/

/* configuration parameters - should be set using the defaults database */
static char *DialogFontName = "9x15";
XFontStruct *DialogFont;
unsigned long DialogBorderColor, ButtonBorderColor;
ColorPair DialogC, ButtonC;
unsigned int dialog_border_width, button_border_width, text_border_width,
  list_border_width;
int min_button_height, min_button_width, min_toggle_height, min_choice_height,
  dialog_item_gap;

GC DialogGC, DialogRGC;

extern XContext EventContext, ObjectContext;

/* forward declarations */
LOCAL int FindItemType _((LVAL item));
LOCAL LVAL frame_handler _((XEvent report, int modal));
LOCAL VOID InstallDialogItems _((Window win, LVAL dialog));
LOCAL VOID InstallItemList _((Window win, LVAL items));
LOCAL VOID InstallItem _((Window win, LVAL item));
LOCAL VOID DeleteDialogItems _((LVAL dialog));
LOCAL VOID DeleteItemList _((Window win, LVAL items));
LOCAL VOID DeleteItem _((Window win, LVAL item));
LOCAL int is_item_window _((Display *dpy, Window event_win, LVAL dialog));

#define TOGGLE_MARK_HEIGHT 16
#define CHOICE_MARK_HEIGHT 16

/* gray stipple pattern bitmap data for scroll bar thumb items */
#define gray_width 16
#define gray_height 16
static unsigned char gray_bits[] = {
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa,
   0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa};
Pixmap ScrollThumbPM;

/* toggle item bitmaps */
#define toggle_off_width TOGGLE_MARK_HEIGHT
#define toggle_off_height TOGGLE_MARK_HEIGHT
static unsigned char toggle_off_bits[] = {
   0xff, 0xff, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80,
   0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80,
   0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0xff, 0xff};
Pixmap ToggleOffPM;
#define toggle_on_width CHOICE_MARK_HEIGHT
#define toggle_on_height CHOICE_MARK_HEIGHT
static unsigned char toggle_on_bits[] = {
   0xff, 0xff, 0x03, 0xc0, 0x05, 0xa0, 0x09, 0x90, 0x11, 0x88, 0x21, 0x84,
   0x41, 0x82, 0x81, 0x81, 0x81, 0x81, 0x41, 0x82, 0x21, 0x84, 0x11, 0x88,
   0x09, 0x90, 0x05, 0xa0, 0x03, 0xc0, 0xff, 0xff};
Pixmap ToggleOnPM;

/* choice item bitmaps */
#define choice_off_width 16
#define choice_off_height 16
static unsigned char choice_off_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x0f, 0x08, 0x10,
   0xe4, 0x27, 0x14, 0x28, 0x14, 0x28, 0x14, 0x28, 0x14, 0x28, 0x14, 0x28,
   0x14, 0x28, 0xe4, 0x27, 0x08, 0x10, 0xf0, 0x0f};
Pixmap ChoiceOffPM;
#define choice_on_width 16
#define choice_on_height 16
static unsigned char choice_on_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x0f, 0x08, 0x10,
   0xe4, 0x27, 0xf4, 0x2f, 0xf4, 0x2f, 0xf4, 0x2f, 0xf4, 0x2f, 0xf4, 0x2f,
   0xf4, 0x2f, 0xe4, 0x27, 0x08, 0x10, 0xf0, 0x0f};
Pixmap ChoiceOnPM;

/* slider button pixmap */
#define left_slider_width 16
#define left_slider_height 16
static unsigned char left_slider_bits[] = {
   0x00, 0x00, 0x40, 0x00, 0x20, 0x00, 0x10, 0x00, 0x08, 0x00, 0xfc, 0xff,
   0xfe, 0xff, 0x01, 0x00, 0xfe, 0xff, 0xfc, 0xff, 0x08, 0x00, 0x10, 0x00,
   0x20, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00};
Pixmap LeftSliderPM;
#define right_slider_width 16
#define right_slider_height 16
static unsigned char right_slider_bits[] = {
   0x00, 0x00, 0x00, 0x02, 0x00, 0x04, 0x00, 0x08, 0x00, 0x10, 0xff, 0x3f,
   0xff, 0x7f, 0x00, 0x80, 0xff, 0x7f, 0xff, 0x3f, 0x00, 0x10, 0x00, 0x08,
   0x00, 0x04, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00};
Pixmap RightSliderPM;

extern Cursor ArrowCursor;
Cursor DoubleArrowCursor, RightArrowCursor, LeftArrowCursor;
Cursor UpDownArrowCursor, UpArrowCursor, DownArrowCursor;

/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

Point DialogStringSize(s)
	char *s;
{
  Point pt;
  
  pt.v = DialogFont->max_bounds.ascent + DialogFont->max_bounds.descent;
  pt.h = XTextWidth(DialogFont,s, strlen(s));

  return(pt);
}

char *checkstring(s)
     LVAL s;
{
  if (! stringp(s)) xlerror("not a string", s);
  return((char *) getstring(s));
}

LVAL StX11ItemObject(dpy, win)
     Display *dpy;
     Window win;
{
  LVAL item, win_id;

  win_id = NIL;
  if (XFindContext(dpy, win, ObjectContext, (caddr_t *) &item) == 0
      && objectp(item)) {
    win_id = slot_value(item, s_window_id);
    if (! fixp(win_id)) item = NIL;
  }
  else item = NIL;
  return(item);
}

LOCAL int FindItemType(item)
	LVAL item;
{
  if (consp(item)) return(ITEM_LIST);
  else if (button_item_p(item)) return(BUTTON_ITEM);
  else if (toggle_item_p(item)) return(TOGGLE_ITEM);
  else if (text_item_p(item)) return(TEXT_ITEM);
  else if (choice_item_p(item)) return(CHOICE_ITEM);
  else if (scroll_item_p(item)) return(SCROLL_ITEM);
  else if (list_item_p(item)) return(LIST_ITEM);
  else xlfail("item of unknown type");
  /* not reached */
  return(0);
}

/***********************************************************************/
/**                                                                   **/
/**              Dialog System Initialization and Cleanup             **/
/**                                                                   **/
/***********************************************************************/

VOID StX11InitDialogs()
{
  Display *dpy = StX11Display();
  int screen = StX11Screen();
  unsigned long valuemask;
  XGCValues values;
  int font_height, margin;
  char *font;

  dialog_border_width = 1;
  button_border_width = 1;
  list_border_width = 1;
  text_border_width = 1;
  min_button_height = 20;
  min_button_width = 100;
  dialog_item_gap = 5;

  DialogC.fore = BlackPixel(dpy, screen);
  DialogC.back = WhitePixel(dpy, screen);
  ButtonC.fore = BlackPixel(dpy, screen);
  ButtonC.back = WhitePixel(dpy, screen);
  DialogBorderColor = BlackPixel(dpy, screen);
  ButtonBorderColor = BlackPixel(dpy, screen);

  font = StX11GetDefault("xlisp.dialog.font");
  if (font == NULL) font = StX11GetDefault("xlisp.font");
  if (font == NULL) font = DialogFontName;
  if ((DialogFont = XLoadQueryFont(dpy, font)) == NULL) {
    fprintf(stderr, "xlisp: Can't open %s font\n", font);
    if ((DialogFont = XLoadQueryFont(dpy, DialogFontName)) == NULL) {
      fprintf(stderr, "xlisp: Can't open %s font\n", DialogFontName);
      if ((DialogFont = XLoadQueryFont(dpy, "fixed")) == NULL) {
	fprintf(stderr, "xlisp: Can't open %s font\n", "fixed");
	exit(-1);
      }
    }
  }

  min_toggle_height = TOGGLE_MARK_HEIGHT + DialogFont->max_bounds.descent;
  min_choice_height = CHOICE_MARK_HEIGHT + DialogFont->max_bounds.descent;

  ScrollThumbPM = XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, screen),
					     (char *) gray_bits,
					     gray_width, gray_height,
					     BlackPixel(dpy, screen),
					     WhitePixel(dpy, screen),
					     DefaultDepth(dpy, screen));
  ToggleOffPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				      (char *) toggle_off_bits, 
				      toggle_off_width, toggle_off_height);
  ToggleOnPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				     (char *) toggle_on_bits,
				     toggle_on_width, toggle_on_height);

  ChoiceOffPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				      (char *) choice_off_bits, 
				      choice_off_width, choice_off_height);
  ChoiceOnPM = XCreateBitmapFromData(dpy, RootWindow(dpy, screen),
				     (char *) choice_on_bits,
				     choice_on_width, choice_on_height);

  LeftSliderPM = XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, screen),
					     (char *) left_slider_bits,
					     left_slider_width, 
					     left_slider_height,
					     BlackPixel(dpy, screen),
					     WhitePixel(dpy, screen),
					     DefaultDepth(dpy, screen));
  RightSliderPM = XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, screen),
					      (char *) right_slider_bits,
					      right_slider_width, 
					      right_slider_height,
					      BlackPixel(dpy, screen),
					      WhitePixel(dpy, screen),
					      DefaultDepth(dpy, screen));

  valuemask = 0; /* ignore XGCValues and use defaults */
  DialogGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, DialogGC, DialogFont->fid);
  XSetForeground(dpy, DialogGC, DialogC.fore);
  XSetBackground(dpy, DialogGC, DialogC.back);

  valuemask = 0; /* ignore XGCValues and use defaults */
  DialogRGC = XCreateGC(dpy, RootWindow(dpy, screen), valuemask, &values);
  XSetFont(dpy, DialogRGC, DialogFont->fid);
  XSetForeground(dpy, DialogRGC, DialogC.back);
  XSetBackground(dpy, DialogRGC, DialogC.fore);

  font_height = DialogFont->max_bounds.ascent
              + DialogFont->max_bounds.descent;
  margin = DialogFont->max_bounds.descent / 2;
  if (min_button_height < font_height + 2 * margin)
    min_button_height = font_height + 2 * margin;
  
  DoubleArrowCursor = XCreateFontCursor(dpy, XC_sb_h_double_arrow);
  RightArrowCursor = XCreateFontCursor(dpy, XC_sb_right_arrow);
  LeftArrowCursor = XCreateFontCursor(dpy, XC_sb_left_arrow);
  UpDownArrowCursor = XCreateFontCursor(dpy, XC_sb_v_double_arrow);
  UpArrowCursor = XCreateFontCursor(dpy, XC_sb_up_arrow);
  DownArrowCursor = XCreateFontCursor(dpy, XC_sb_down_arrow);
}

VOID StX11FinishDialogs()
{
  Display *dpy = StX11Display();

  XUnloadFont(dpy, DialogFont->fid);
  XFreeGC(dpy, DialogGC);
  XFreePixmap(dpy, ScrollThumbPM);
  XFreePixmap(dpy, ToggleOffPM);
  XFreePixmap(dpy, ToggleOnPM);
  XFreePixmap(dpy, ChoiceOffPM);
  XFreePixmap(dpy, ChoiceOnPM);
  XFreePixmap(dpy, LeftSliderPM);
  XFreePixmap(dpy, RightSliderPM);
  XFreeCursor(dpy, DoubleArrowCursor);
  XFreeCursor(dpy, RightArrowCursor);
  XFreeCursor(dpy, LeftArrowCursor);
  XFreeCursor(dpy, UpDownArrowCursor);
  XFreeCursor(dpy, UpArrowCursor);
  XFreeCursor(dpy, DownArrowCursor);
}

VOID StX11DialogReset(V)
{
  Display *dpy = StX11Display();

  XUngrabPointer(dpy, CurrentTime);
  XSync(dpy, TRUE);
}

/***********************************************************************/
/**                                                                   **/
/**               Constructing and Removing Dialogs                   **/
/**                                                                   **/
/***********************************************************************/

LOCAL LVAL frame_handler(report, modal)
     XEvent report;
     int modal;
{
  if (modal) return(NIL);

  switch(report.type) {
  case ClientMessage:
    StX11HandleClientMessage(report);
    break;
  default:
    break;
  }
  return(NIL);
}

VOID DialogAllocate(dialog) 
     LVAL dialog;
{
  char *title;
  Point loc, size;
  int go_away, modeless;
  Window win, panel;
  unsigned int width, height, wheight;
  int left, top;
  XSetWindowAttributes setwinattr;
  unsigned long valuemask;
  Display *dpy = StX11Display();
  int screen = StX11Screen();

  if (check_dialog_address(dialog)) DialogRemove(dialog);
    
  if (! stringp(slot_value(dialog, s_title))) 
    xlerror("not a string", slot_value(dialog, s_title));
  title = (char *) getstring(slot_value(dialog, s_title));

  loc = ListToPoint(slot_value(dialog, s_location));
  size = ListToPoint(slot_value(dialog, s_size));
  go_away = (slot_value(dialog, s_go_away) != NIL) ? TRUE : FALSE;
  
  modeless = (slot_value(dialog, s_type) == s_modeless) ? TRUE : FALSE;

  /* create opaque dialog window */
  left = loc.h; top = loc.v;
  width = size.h; height = size.v;
  wheight = (modeless && go_away) ? height + ClosePanelHeight() : height;
  win = XCreateSimpleWindow(dpy, RootWindow(dpy, screen),
			    left, top, width, wheight, dialog_border_width,
			    DialogBorderColor, DialogC.back);
  if (modeless) StX11SetWindowClass(dpy, win);
  StX11SetNormalHints(dpy, win, left, top, width, wheight);

  if (XSaveContext(dpy, win, EventContext, (caddr_t) frame_handler) != 0)
    xlfail("could not install event handler");
  if (XSaveContext(dpy, win, ObjectContext, (caddr_t) dialog) != 0)
    xlfail("could not install object in window");

  if (! modeless) {
    if (StX11UseICCCM()) StX11SetTransientHint(dpy, win);
    else {
      /* set override_redirect and save_under attributes for a modal dialog */
      valuemask = CWOverrideRedirect | CWSaveUnder;
      setwinattr.override_redirect = TRUE;
      setwinattr.save_under = TRUE;
      XChangeWindowAttributes(dpy, win, valuemask, &setwinattr);
    }
  }
  XStoreName(dpy, win, title);
  set_dialog_address((CPTR) win, dialog);
  XDefineCursor(dpy, win, ArrowCursor);
  StX11SetStandardHints(dpy, win);

  left = 0;
  top = (modeless && go_away) ? ClosePanelHeight() : 0;
  panel = XCreateSimpleWindow(dpy, win,
			      left, top, width, height, dialog_border_width,
			      DialogBorderColor, DialogC.back);

  InstallDialogItems(panel, dialog);
  if (modeless && go_away) InstallCloseButton(win, dialog);

  /* Display (map) the windows */
  XMapSubwindows(dpy, panel);
  XMapSubwindows(dpy, win);
  XMapWindow(dpy, win);  
}

VOID DialogRemove(dialog)
     LVAL dialog;
{
  Window win;
  Display *dpy = StX11Display();

  if (check_dialog_address(dialog) 
      && (win = (Window) GETDIALOGADDRESS(dialog)) != NullWindow) {
    if (XDeleteContext(dpy, win, EventContext) != 0)
      xlfail("could not delete event context");
    if (XDeleteContext(dpy, win, ObjectContext) != 0)
      xlfail("could not delete object context");
    DeleteDialogItems(dialog);
    DeleteCloseButton(win);
    XDestroyWindow(dpy, win);
    XFlush(dpy);
  }
  if (objectp(dialog)) standard_hardware_clobber(dialog);
}

VOID DialogSetDefaultButton(dialog, item) 
     LVAL dialog, item;
{
  if (item == NIL || button_item_p(item))
    set_slot_value(dialog, s_default_button, item);
}

LOCAL VOID InstallDialogItems(win, dialog)
     Window win;
     LVAL dialog;
{
  LVAL items;

  items = slot_value(dialog, s_items);
  InstallItemList(win, items);
}

LOCAL VOID InstallItemList(win, items)
	Window win;
	LVAL items;
{
  for (; consp(items); items = cdr(items))
    if (consp(car(items))) InstallItemList(win, car(items));
    else InstallItem(win, car(items));
}
  
LOCAL VOID InstallItem(win, item)
     Window win;
     LVAL item;
{
  int type;
  
  if (! dialog_item_p(item)) xlerror("not a dialog item", item);
  
  type = FindItemType(item);
  
  switch (type) {
  case BUTTON_ITEM: InstallButtonItem(win, item); break;
  case TOGGLE_ITEM: InstallToggleItem(win, item); break;
  case CHOICE_ITEM: InstallChoiceItem(win, item); break;
  case TEXT_ITEM:   InstallTextItem(win, item); break;
  case SCROLL_ITEM: InstallScrollItem(win, item); break;
  case LIST_ITEM: InstallListItem(win, item); break;
  default: xlfail("unkown item type");
  }
}

LOCAL VOID DeleteDialogItems(dialog)
     LVAL dialog;
{
  Window win;
  LVAL items;

  win = (Window) GETDIALOGADDRESS(dialog);
  if (win != NullWindow) {
    items = slot_value(dialog, s_items);
    DeleteItemList(win, items);
  }
}

LOCAL VOID DeleteItemList(win, items)
	Window win;
	LVAL items;
{
  for (; consp(items); items = cdr(items))
    if (consp(car(items))) DeleteItemList(win, car(items));
    else DeleteItem(win, car(items));
}
  
LOCAL VOID DeleteItem(win, item)
     Window win;
     LVAL item;
{
  int type;
  
  if (! dialog_item_p(item)) xlerror("not a dialog item", item);
  
  type = FindItemType(item);
  
  switch (type) {
  case BUTTON_ITEM: DeleteButtonItem(win, item); break;
  case TOGGLE_ITEM: DeleteToggleItem(win, item); break;
  case CHOICE_ITEM: DeleteChoiceItem(win, item); break;
  case TEXT_ITEM:   DeleteTextItem(win, item); break;
  case SCROLL_ITEM: DeleteScrollItem(win, item); break;
  case LIST_ITEM: DeleteListItem(win, item); break;
  default: xlfail("unkown item type");
  }
}

/***********************************************************************/
/**                                                                   **/
/**                       Modal Dialog Loop                           **/
/**                                                                   **/
/***********************************************************************/

LVAL DialogGetModalItem(dialog)
     LVAL dialog;
{
  Window win, event_win, old_focus_win;
  Display *dpy = StX11Display();
  XEvent report;
  LVAL result = NIL;
  LVAL (*callback)();
  int old_revert_to, use_icccm;

  send_message(dialog, sk_show_window);
  win = (Window) GETDIALOGADDRESS(dialog);
  if (win != NullWindow) {

    StX11ReleaseButton();
    use_icccm = StX11UseICCCM();
    if (! use_icccm) {
      /* set the input focus and grap the pointer */
      XGetInputFocus(dpy, &old_focus_win, &old_revert_to);
      XSetInputFocus(dpy, win, RevertToPointerRoot, CurrentTime);
      XGrabPointer(dpy, win, TRUE, ButtonReleaseMask,
		   GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
    }

    /* Loop until button is released, examining each event */
    while (result == NIL) {
      XNextEvent(dpy, &report);
      event_win = report.xany.window;
      if (is_item_window(dpy, event_win, dialog)
	  && XFindContext(dpy, event_win, EventContext, (caddr_t *) &callback)
	     == 0
	  && callback != NULL)
	result = (*callback)(report, TRUE);
      else if (report.type == Expose)
	StProcessEvent(dpy, report);
      else if (report.type == MappingNotify)
	XRefreshKeyboardMapping((XMappingEvent *) &report);
    }
    if (! use_icccm) {
      XUngrabPointer(dpy, CurrentTime);
      XSetInputFocus(dpy, old_focus_win, old_revert_to, CurrentTime);
    }
    else StX11FlushStdin();
  }
  return(result);
}

LOCAL int is_item_window(dpy, event_win, dialog)
     Display *dpy;
     Window event_win;
     LVAL dialog;
{
  LVAL item;

  if (XFindContext(dpy, event_win, ObjectContext, (caddr_t *) &item) != 0)
    return(FALSE);
  else if (! objectp(item) || ! dialog_item_p(item)) return(FALSE);
  else if (slot_value(item, s_dialog) != dialog) return(FALSE);
  else return(TRUE);
}

VOID install_dialog_item_handler(dpy, win, handler, item)
     Display *dpy;
     Window win;
     LVAL (*handler)(), item;
{
  if (XSaveContext(dpy, win, EventContext, (caddr_t) handler) != 0)
    xlfail("could not install event handler");
}
  
VOID delete_dialog_item_handler(dpy, win)
     Display *dpy;
     Window win;
{
  if (XDeleteContext(dpy, win, EventContext) != 0)
    xlfail("could not delete event context");
}

#ifdef TODO
register dialog with all subwindows; use in modal loop
make hitting return envoke default button for all dialogs
#endif /* TODO */
