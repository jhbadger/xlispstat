/* xsiviewwin - XLISP interface to IVIEW dynamic graphics package.     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

#define IVIEW_WINDOW_TITLE  "Graph Window"
#ifdef MACINTOSH
#define IVIEW_WINDOW_LEFT 10
#define IVIEW_WINDOW_TOP 20
#define IVIEW_WINDOW_WIDTH  250
#define IVIEW_WINDOW_HEIGHT 250
#else
#ifdef _Windows
#define IVIEW_WINDOW_LEFT 0
#define IVIEW_WINDOW_TOP 0
#define IVIEW_WINDOW_WIDTH 250
#define IVIEW_WINDOW_HEIGHT 250
#define IVIEW_WINDOW_LEFT 50
#else
#define IVIEW_WINDOW_LEFT 50
#ifdef AMIGA
#define IVIEW_WINDOW_TOP 0
#else
#define IVIEW_WINDOW_TOP 50
#endif /* AMIGA */
#define IVIEW_WINDOW_WIDTH  400
#define IVIEW_WINDOW_HEIGHT 400
#endif /* _Windows */
#endif /* MACINTOSH */

/* external variables */
extern LVAL s_true;
extern LVAL sk_allocate, sk_dispose, sk_resize, sk_redraw, sk_do_idle,
  sk_do_click, sk_do_motion, sk_do_key, sk_install, sk_remove, s_title,
  s_go_away, s_menu, s_hardware_address, s_black_on_white, s_has_h_scroll,
  s_has_v_scroll, s_internals, sk_show, sk_show_window;
extern LVAL s_in_callback;
  
/**************************************************************************/
/**                                                                      **/
/**                       Window Creation Functions                      **/
/**                                                                      **/
/**************************************************************************/

/* :ISNEW message for IVIEW-WINDOW-CLASS */
LVAL iview_window_isnew(V)
{
  LVAL object = xlgaobject();
  int show = xsboolkey(sk_show, TRUE);

  object_isnew(object);
  initialize_graph_window(object);
  if (show) send_message(object, sk_allocate);
  return(object);
}

/* :ALLOCATE message for IVIEW-WINDOW-CLASS */
LVAL iview_window_allocate(V)
{
  LVAL object;
  IVIEW_WINDOW w;
  
  object = xlgaobject();
  
  w = IViewWindowNew(object, TRUE);
  /* use StShowWindow to show (map) window but NOT send :resize or :redraw */
  if (xsboolkey(sk_show, TRUE)) StShowWindow(w);
  
  return(object);
}

VOID StGWGetAllocInfo P7C(LVAL, object, char **, title,
                          int *, left, int *, top, int *, width, int *, height, int *, goAway)
{
  LVAL window_title;
  
  if (slot_value(object, s_hardware_address) != NIL)
  	send_message(object, sk_dispose);
  
  window_title = slot_value(object, s_title);
  if (!stringp(window_title)) {
    window_title = cvstring(IVIEW_WINDOW_TITLE);
    set_slot_value(object, s_title, window_title);
  }
  *title = (char *) getstring(window_title);
  
  *left = IVIEW_WINDOW_LEFT;
  *top = IVIEW_WINDOW_TOP;
  *width = IVIEW_WINDOW_WIDTH;
  *height = IVIEW_WINDOW_HEIGHT;
  get_window_bounds(object, left, top, width, height);
  
  *goAway = slot_value(object, s_go_away) != NIL;
}

VOID StGWObDoClobber P1C(LVAL, object)
{
  standard_hardware_clobber(object);
}

VOID StGWObResize P1C(LVAL, object)
{
  send_callback_message(object, sk_resize);
}

VOID StGWObRedraw P1C(LVAL, object)
{
  send_callback_message(object, sk_redraw);
}
	

/* idle action. incall is used to detect longjmp's on errors and to    */
/* turn off idle calling if the call is generating an error.           */
VOID StGWObDoIdle P1C(LVAL, object)
{
  static int incall = FALSE;
  
  if (incall) {
    StGWSetIdleOn(StGWObWinInfo(object), FALSE);
    incall = FALSE;
    return;
  }
  else {
    incall = TRUE;
    send_callback_message(object, sk_do_idle);
    incall = FALSE;
  }
}

VOID StGWObDoMouse P5C(LVAL, object, int, x, int, y, MouseEventType, type, MouseClickModifier, mods)
{
  LVAL Lx, Ly, argv[6], olddenv;
  int extend, option;
  
  xlstkcheck(2);
  xlsave(Lx);
  xlsave(Ly);
  argv[0] = object;
  argv[2] = Lx = cvfixnum((FIXTYPE) x);
  argv[3] = Ly = cvfixnum((FIXTYPE) y);

  olddenv = xldenv;
  xldbind(s_in_callback, s_true);
  if (type == MouseClick) {
	extend = ((int) mods) % 2;
	option = ((int) mods) / 2;
    argv[1] = sk_do_click;
	argv[4] = (extend) ? s_true : NIL;
	argv[5] = (option) ? s_true : NIL;
    xscallsubrvec(xmsend, 6, argv);
  }
  else {
    argv[1] = sk_do_motion;
    xscallsubrvec(xmsend, 4, argv);
  }
  xlpopn(2);
  xlunbind(olddenv);
}

VOID StGWObDoKey P4C(LVAL, object, int, key, int, shift, int, opt)
{
  LVAL argv[5], ch, olddenv;
  
  olddenv = xldenv;
  xldbind(s_in_callback, s_true);
  xlsave1(ch);
  ch = cvchar(key);
  argv[0] = object;
  argv[1] = sk_do_key;
  argv[2] = ch;
  argv[3] = shift ? s_true : NIL;
  argv[4] = opt ? s_true : NIL;
  xscallsubrvec(xmsend, 5, argv);
  xlpop();
  xlunbind(olddenv);
}
  
StGWWinInfo *StGWObWinInfo P1C(LVAL, object)
{
  LVAL internals = slot_value(object, s_internals);
  
  if (! consp(internals) || ! adatap(car(internals)) 
      || getadaddr(car(internals)) == NULL) 
    xlfail("bad internal data");
  return((StGWWinInfo *) getadaddr(car(internals)));
}

VOID initialize_graph_window P1C(LVAL, object)
{
  LVAL internals, value;
  int v, width, height, size;
  StGWWinInfo *gwinfo;
  ColorCode bc,dc; /* added JKL */
  
  internals = newadata(StGWWinInfoSize(), 1, FALSE);
  set_slot_value(object, s_internals, consa(internals));
  StGWInitWinInfo(object);
  
  gwinfo = StGWObWinInfo(object);
  if (gwinfo == NULL) return;
  
  StGWSetObject(gwinfo, object);
  
  if (slot_value(object, s_black_on_white) == NIL) {
    bc = StGWBackColor(gwinfo);         /* this seems better for color */
    dc = StGWDrawColor(gwinfo);         /* machines - 0 and 1 are not  */
    StGWSetDrawColor(gwinfo, bc);       /* the default draw and back   */
    StGWSetBackColor(gwinfo, dc);       /* colors on the Amiga   JKL   */
  }
  
  StGetScreenSize(&width, &height);
  size = (width > height) ? width : height;
  if ((value = slot_value(object, s_has_h_scroll)) != NIL) {
    v =  (fixp(value)) ? getfixnum(value) : size;
    StGWSetHasHscroll(gwinfo, TRUE, v);
  }
  if ((value = slot_value(object, s_has_v_scroll)) != NIL) {
    v =  (fixp(value)) ? getfixnum(value) : size;
    StGWSetHasVscroll(gwinfo, TRUE, v);
  }
}

LVAL xsiview_window_update(V)
{
#ifdef MACINTOSH
  LVAL object;
  int resized;
  
  object = xlgaobject();
  resized = (xlgetarg() != NIL);
  xllastarg();
  
  graph_update_action(StGWObWinInfo(object), resized);
#endif /* MACINTOSH */
  return(NIL);
}

LVAL xsiview_window_activate(V)
{
#ifdef MACINTOSH
  LVAL object, menu;
  int active;
  
  object = xlgaobject();
  active = (xlgetarg() != NIL);
  xllastarg();
  
  graph_activate_action(StGWObWinInfo(object), active);
  menu = slot_value(object, s_menu);
  if (menu_p(menu)) {
    if (active) send_message(menu, sk_install);
    else send_message(menu, sk_remove);
  } 
#endif /* MACINTOSH */
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                     Idle Installation Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_window_idle_on(V)
{
  StGWWinInfo *gwinfo;
  int on = 0, set = FALSE;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == NULL) return(NIL);
  
  if (moreargs()) {
    set = TRUE;
    on = (xlgetarg() != NIL) ? TRUE : FALSE;
  }
  xllastarg();

  if (set) StGWSetIdleOn(gwinfo, on);
  return((StGWIdleOn(gwinfo)) ? s_true : NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                 Menu Installation and Access Functions               **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_window_menu(V)
{
  LVAL object, menu = NULL;
  int set = FALSE;
  
  object = xlgaobject();
  if (moreargs()) {
    set = TRUE;
    menu = xlgetarg();
  }
  xllastarg();

  if (set) {
    if (menu_p(menu)) set_slot_value(object, s_menu, menu);
    else if (menu == NIL) set_slot_value(object, s_menu, NIL);
    else xlerror("not a menu", menu);
  }
  
  return(slot_value(object, s_menu));
}
