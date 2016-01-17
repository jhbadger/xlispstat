/* X11listitem - list items for X11 dialogs                            */
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

typedef struct {
  unsigned long fore, back;
} ColorPair;

extern LVAL s_internals, s_window_id;

/* forward declarations */
LOCAL VOID draw_field_content _((Display *dpy, Window win,
				 char *text, int reversed));
LOCAL VOID draw_fields _((LVAL item));

/***********************************************************************/
/**                                                                   **/
/**                        Global Variables                           **/
/**                                                                   **/
/***********************************************************************/

/* configuration parameters - should be set using the defaults database */
extern XFontStruct *DialogFont;
extern unsigned long DialogBorderColor;
extern ColorPair DialogC;
extern unsigned int list_border_width;
extern int min_button_width;

extern GC DialogGC, DialogRGC;

extern XContext ObjectContext, ListFieldContext;

/***********************************************************************/
/**                                                                   **/
/**                           List Items                              **/
/**                                                                   **/
/***********************************************************************/

# define LIST_TEXT_PATTERN "MMMMMMMMMMMMMMM"
# define LIST_LEAD 5
# define LIST_PAD 15
# define MAX_LIST_ROWS 12
# define SCROLL_WIDTH 20
# define DOUBLE_CLICK_TIME 1000
# define DOUBLE_CLICK_MOVE 10

/***********************************************************************/
/**                                                                   **/
/**                     Double Click Detection                        **/
/**                                                                   **/
/***********************************************************************/

static int click_x, click_y, click_inited = FALSE;
static Time click_time;
Window click_win;

static int is_double_click(report)
     XEvent *report;
{
  int result, x, y, del_x, del_y, del_time;
  Time time;
  Window win;
  
  x = report->xbutton.x;
  y = report->xbutton.y;
  time = report->xbutton.time;
  win = report->xbutton.window;
  if (click_inited) {
    del_x = (click_x < x) ? x - click_x : click_x - x;
    del_y = (click_y < y) ? y - click_y : click_y - y;
    del_time = (click_time < time) ? time - click_time : click_time - time;
    result = (click_win == win
	      && del_x < DOUBLE_CLICK_MOVE 
	      && del_y < DOUBLE_CLICK_MOVE 
	      && del_time < DOUBLE_CLICK_TIME) ? TRUE : FALSE;
  }
  else result = FALSE;

  click_inited = ! result;
  click_x = x;
  click_y = y;
  click_time = time;
  click_win = win;

  return(result);
}

/***********************************************************************/
/**                                                                   **/
/**                    Basic Size Calculations                        **/
/**                                                                   **/
/***********************************************************************/

static Point data_size(data)
     LVAL data;
{
  Point sz;
  if (seqp(data)) { 
    sz.v = seqlen(data); 
    sz.h = 1;
  }
  else if (matrixp(data)) {
    sz.v = numrows(data);
    sz.h = numcols(data);
  }
  else xlerror("bad list item data", data);
  return(sz);
}

static Point cell_size()
{
  Point cellsz;

  cellsz = DialogStringSize(LIST_TEXT_PATTERN);
  cellsz.v += LIST_LEAD;
  cellsz.h += LIST_PAD;
  return(cellsz);
}

static int max_cols(item)
     LVAL item;
{
  LVAL columns = slot_value(item, s_columns);

  return((fixp(columns)) ? getfixnum(columns) : 1);
}

/***********************************************************************/
/**                                                                   **/
/**                        Index Conversion                           **/
/**                                                                   **/
/***********************************************************************/

static int field_index(dpy, win, vdims, ddims, offset)
     Display *dpy;
     Window win;
     Point vdims, ddims, offset;
{
  int vis_index, index;
  Point vpoint, dpoint;

  if (XFindContext(dpy, win, ListFieldContext, (caddr_t *) &vis_index) != 0)
    xlfail("could not find field index");
  vis_index--; /* needed to avoid storing a zero - confuses som X's */

  vpoint.v = vis_index / vdims.h;
  vpoint.h = vis_index % vdims.h;
  dpoint.v = offset.v + vpoint.v;
  dpoint.h = offset.h + vpoint.h;
  index = dpoint.v * ddims.h + dpoint.h;

  return(index);
}

/***********************************************************************/
/**                                                                   **/
/**                  Internal Data Represenation                      **/
/**                                                                   **/
/***********************************************************************/

static VOID make_internals(item)
     LVAL item;
{
  LVAL internals, data;
  int cols, num_fields;
  Point dsize, vsize;
  
  data = slot_value(item, s_list_data);
  cols = max_cols(item);
  dsize = data_size(data);
  vsize.h = (dsize.h > cols) ? cols : dsize.h;
  vsize.v = (dsize.v > MAX_LIST_ROWS) ? MAX_LIST_ROWS : dsize.v;
  num_fields = vsize.h * vsize.v;

  internals = newvector(8);
  set_slot_value(item, s_internals, internals);
  setelement(internals, 0, PointToList(vsize));
  setelement(internals, 1, PointToList(dsize));
  setelement(internals, 2,
	     matrixp(data) ? data : coerce_to_tvec(data, s_true));
  setelement(internals, 3, newvector(num_fields));
  setelement(internals, 4, integer_list_2(0, 0));
  setelement(internals, 5, NIL);
  setelement(internals, 6, NIL);
  setelement(internals, 7, NIL);
}

static Point visible_dims(internals)
     LVAL internals;
{
  return(ListToPoint(getelement(internals, 0)));
}

static Point data_dims(internals)
     LVAL internals;
{
  return(ListToPoint(getelement(internals, 1)));
}

static LVAL get_data(internals)
     LVAL internals;
{
  return(getelement(internals, 2));
}

static LVAL get_fields(internals)
     LVAL internals;
{
  return(getelement(internals, 3));
}

static Point get_offset(internals)
     LVAL internals;
{
  return(ListToPoint(getelement(internals, 4)));
}

static VOID set_offset(internals, offset)
     LVAL internals;
     Point offset;
{
  setelement(internals, 4, PointToList(offset));
}

static LVAL get_selection(internals)
     LVAL internals;
{
  return(getelement(internals, 5));
}

static int selection_index(internals)
     LVAL internals;
{
  LVAL sel = get_selection(internals);
  Point ddims, psel;

  if (sel == NIL) return(-1);
  else if (fixp(sel)) return(getfixnum(sel));
  else {
    ddims = data_dims(internals);
    psel = ListToPoint(sel);
    return(ddims.h * psel.h + psel.v);
  }
}

static VOID set_selection(item, val, index, use_val)
     LVAL item, val;
     int index, use_val;
{
  Point ddims, p;
  LVAL internals;

  internals = slot_value(item, s_internals);
  if (use_val) setelement(internals, 5, val);
  else if (vectorp(get_data(internals))) 
    setelement(internals, 5, cvfixnum((FIXTYPE) index));
  else {
    ddims = data_dims(internals);
    p.h = index / ddims.h;
    p.v = index % ddims.h;
    setelement(internals, 5, PointToList(p));
  }
  draw_fields(item);
}    

static VOID set_vscroll(internals, w, has)
     LVAL internals;
     Window w;
     int has;
{
  setelement(internals, 6, (has) ? cvfixnum((FIXTYPE) w) : NIL);
}

static VOID set_hscroll(internals, w, has)
     LVAL internals;
     Window w;
     int has;
{
  setelement(internals, 7, (has) ? cvfixnum((FIXTYPE) w) : NIL);
}

static int has_vscroll(internals)
     LVAL internals;
{
  return(getelement(internals, 6) != NIL);
}

static int has_hscroll(internals)
     LVAL internals;
{
  return(getelement(internals, 7) != NIL);
}

static Window get_vscroll(internals)
     LVAL internals;
{
  LVAL val = getelement(internals, 6);
  return(fixp(val) ? (Window) getfixnum(val) : None);
}

static Window get_hscroll(internals)
     LVAL internals;
{
  LVAL val = getelement(internals, 7);
  return(fixp(val) ? (Window) getfixnum(val) : None);
}

/***********************************************************************/
/**                                                                   **/
/**                       Drawing Routines                            **/
/**                                                                   **/
/***********************************************************************/

LOCAL VOID draw_field_content(dpy, win, text, reversed)
     Display *dpy;
     Window win;
     char *text;
     int reversed;
{
  Point ssz;
  int x, y, len;
  GC gc;
  unsigned long color;

  gc = (reversed) ? DialogRGC : DialogGC;
  color = (reversed) ? DialogC.fore : DialogC.back;

  XSetWindowBackground(dpy, win, color);
  XClearWindow(dpy, win);
  ssz = DialogStringSize(text);
  x = LIST_PAD / 2;
  y = LIST_LEAD / 2 + DialogFont->max_bounds.ascent;
  len = strlen(text);
  XDrawString(dpy, win, gc, x, y, text, len);
  XSetWindowBackground(dpy, win, DialogC.back);
}

LOCAL VOID draw_fields(item)
     LVAL item;
{
  Display *dpy = StX11Display();
  Window win;
  LVAL fields, internals, data;
  Point vdims, ddims, offset;
  char *text;
  int sel, i, j, k, index, num_fields;

  internals = slot_value(item, s_internals);
  data = get_data(internals);
  if (darrayp(data)) data = getdarraydata(data);
  fields = get_fields(internals);
  vdims = visible_dims(internals);
  ddims = data_dims(internals);
  offset = get_offset(internals);
  sel = selection_index(internals);
  num_fields = getsize(fields);

  for (i = 0, k = 0; i < vdims.v; i++) {
    for (j = 0; j < vdims.h && k < num_fields; j++, k++) {
      win = getfixnum(gettvecelement(fields, k));
      index = (i + offset.v) * ddims.h + j + offset.h;
      text = checkstring(gettvecelement(data, index));
      draw_field_content(dpy, win, text, sel == index);
    }
  }
}

/***********************************************************************/
/**                                                                   **/
/**                        Event Handlers                             **/
/**                                                                   **/
/***********************************************************************/

static LVAL field_handler(report, modal)
     XEvent report;
     int modal;
{
  Display *dpy = StX11Display();
  Window win;
  LVAL item, internals, data;
  LVAL result = NIL;
  Point vdims, ddims, offset;
  char *text;
  int sel, index;

  win = report.xany.window;
  item = StX11ItemObject(dpy, win);
  internals = slot_value(item, s_internals);
  data = get_data(internals);
  if (darrayp(data)) data = getdarraydata(data);
  vdims = visible_dims(internals);
  ddims = data_dims(internals);
  offset = get_offset(internals);
  sel = selection_index(internals);

  if (item != NIL) {
    switch (report.type) {
    case Expose:
      index = field_index(dpy, win, vdims, ddims, offset);
      text = checkstring(gettvecelement(data, index));
      draw_field_content(dpy, win, text, sel == index);
      break;
    case ButtonPress:
      index = field_index(dpy, win, vdims, ddims, offset);
      set_selection(item, NIL, index, FALSE);
      if (is_double_click(&report)) 
        send_message_1L(item, sk_do_action, s_true);
      else 
	send_message(item, sk_do_action);
      break;
    default: 
      break;
    }
  }
  return(result);
}

static VOID scroll_action(item, s, which, x, y)
     LVAL item;
     Window s;
     int which, x, y;
{
  int is_h_scroll, val, max, page, pos, inc;
  Point size, offset, ddims, vdims, cellsz;
  LVAL internals;
  Window hscroll, vscroll;
  double side;

  internals = slot_value(item, s_internals);
  offset = get_offset(internals);
  ddims = data_dims(internals);
  vdims = visible_dims(internals);
  cellsz = cell_size();
  size.v = vdims.v * cellsz.v;
  size.h = vdims.h * cellsz.h;
  hscroll = get_hscroll(internals);
  vscroll = get_vscroll(internals);

  is_h_scroll = (s == hscroll) ? TRUE : FALSE;
  val = (is_h_scroll) ? offset.h : offset.v;
  max = (is_h_scroll) ? ddims.h : ddims.v;
  page = (is_h_scroll) ? vdims.h : vdims.v;
  side = (is_h_scroll) ? size.h : size.v;
  pos = (is_h_scroll) ? x * (max / side) : y * (max / side);
  inc = 1;
    
  switch (which) {
  case 'M': val = pos;  break;
  case 'L': val += inc; break;
  case 'R': val -= inc; break;
  }
  if (val + page > max) val = max - page;
  if (val < 0) val = 0;

  if (is_h_scroll) offset.h = val;
  else offset.v = val;
  set_offset(internals, offset);
  draw_fields(item);
  if (hscroll != None) AdjustScrollBar(hscroll, offset.h, vdims.h, ddims.h);
  if (vscroll != None) AdjustScrollBar(vscroll, offset.v, vdims.v, ddims.v);
}

/***********************************************************************/
/**                                                                   **/
/**                       Public Routines                             **/
/**                                                                   **/
/***********************************************************************/

VOID DialogListGetDefaultSize(item, width, height)
     LVAL item;
     int *width, *height;
{
  LVAL data = slot_value(item, s_list_data);
  Point sz, cellsz;
  int cols, m, n;

  cellsz = cell_size();

  cols = max_cols(item);
  sz = data_size(data);
  m = sz.v;
  n = sz.h;

  *height = (m <= MAX_LIST_ROWS) ? m * cellsz.v : MAX_LIST_ROWS * cellsz.v;
  *width = (n <= cols) ? n * cellsz.h : cols * cellsz.h;
  if (m > MAX_LIST_ROWS) *width +=  SCROLL_WIDTH;
  if (n > cols) *height += SCROLL_WIDTH;
}

VOID InstallListItem(win, item) 
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Point loc, vsize, size, cellsz, dsize;
  Window panel, newfield, scroll;
  LVAL internals, fields;
  int num_fields, i, j, k;

  make_internals(item);

  internals = slot_value(item, s_internals);
  cellsz = cell_size();
  loc = ListToPoint(slot_value(item, s_location));
  vsize = visible_dims(internals);
  dsize = data_dims(internals);
  size.v = vsize.v * cellsz.v;
  size.h = vsize.h * cellsz.h;
  fields = get_fields(internals);
  num_fields = getsize(fields);

  panel = XCreateSimpleWindow(dpy, win, loc.h, loc.v, size.h, size.v,
			      list_border_width,
			      DialogBorderColor, DialogC.back);
  
  set_slot_value(item, s_window_id, cvfixnum((FIXTYPE) panel));
  
  if (XSaveContext(dpy, panel, ObjectContext, (caddr_t) item) != 0)
    xlfail("could not install object in window");

  if (dsize.h > vsize.h) {
    InstallScrollBar(win, item, loc.h, loc.v + size.v, size.h, SCROLL_WIDTH,
		     &scroll, scroll_action);
    set_hscroll(internals, scroll, TRUE);
    AdjustScrollBar(scroll, 0, vsize.h, dsize.h);
  }
  if (dsize.v > vsize.v) {
    InstallScrollBar(win, item, loc.h + size.h, loc.v, SCROLL_WIDTH, size.v,
		     &scroll, scroll_action);
    set_vscroll(internals, scroll, TRUE);
    AdjustScrollBar(scroll, 0, vsize.v, dsize.v);
  }

  for (i = 0, k = 0; i < vsize.v; i++) {
    for (j = 0; j < vsize.h && k < num_fields; j++, k++) {
      newfield = XCreateSimpleWindow(dpy, panel, cellsz.h * j, cellsz.v * i,
				     cellsz.h, cellsz.v, 0,
				     DialogBorderColor, DialogC.back);

      XSelectInput(dpy, newfield, ExposureMask | ButtonPressMask);
      install_dialog_item_handler(dpy, newfield, field_handler, item);
      if (XSaveContext(dpy, newfield, ObjectContext, (caddr_t) item) != 0)
	xlfail("could not install object in window");

      /* add 1 to index to avoid confusing context manager with zeros */
      if (XSaveContext(dpy, newfield, ListFieldContext, (caddr_t) (k + 1)) != 0)
	xlfail("could not install field index in window");
      setelement(fields, k, cvfixnum((FIXTYPE) newfield));
    }
  }
  XMapSubwindows(dpy, panel);
}

VOID DeleteListItem(win, item) 
     Window win;
     LVAL item;
{
  Display *dpy = StX11Display();
  Window panel, thefield;
  LVAL internals, fields;
  int k, num_fields;

  panel = (Window) getfixnum(slot_value(item, s_window_id));
  internals = slot_value(item, s_internals);
  fields = get_fields(internals);

  num_fields = getsize(fields);
  for (k = 0; k < num_fields; k++) {
    thefield = getfixnum(getelement(fields, k));
    delete_dialog_item_handler(dpy, thefield);
    if (XDeleteContext(dpy, thefield, ObjectContext) != 0)
      xlfail("could not delete object context");
    if (XDeleteContext(dpy, thefield, ListFieldContext) != 0)
      xlfail("could not delete list field context");
    setelement(fields, k, NIL);
  }

  if (has_hscroll(internals)) DeleteScrollBar(get_hscroll(internals));
  if (has_vscroll(internals)) DeleteScrollBar(get_vscroll(internals));

  if (XDeleteContext(dpy, panel, ObjectContext) != 0)
    xlfail("could not delete object context");
  set_slot_value(item, s_window_id, NIL);
}

VOID DialogListItemSetText(item, index, text)
     LVAL item, index;
     char *text;
{
#ifdef DODO
/* 

  this is not needed since the matrix in the internals is eq to the
  one in the list-data slot already modified by the portable part of
  the code.  Besides, this code is wrong since it permutes the intex
  (as on the Mac).

*/
  LVAL internals, data;
  Point p, ddims;
  int i;

  internals = slot_value(item, s_internals);
  data = get_data(internals);
  if (darrayp(data)) data = getdarraydata(data);
  if (fixp(index)) i = getfixnum(index);
  else {
    p = ListToPoint(index);
    ddims = data_dims(internals);
    i = p.v * ddims.h + p.h;
  }
  if (0 <= i && i < getsize(data))
    settvecelement(data, i, cvstring(text));
  else xlerror("index out of range", index);
#endif
  draw_fields(item);
}

LVAL DialogListItemSelection(item, set, index) 
     LVAL item, index;
     int set;
{
  if (set) set_selection(item, index, 0, TRUE);
  return(get_selection(slot_value(item, s_internals)));
}
