/* macmenus - Low Level Menu Objects for Macintosh                     */
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
#include "version.h"

#define IVIEW_MENU MenuHandle
#define IVIEW_WINDOW WindowPtr

/* external variables */
extern LVAL s_true, s_title, s_items, s_enabled, s_id, s_menu_list, s_key,
  s_mark, s_style, s_action, s_menu, s_menu_proto, s_apple_menu_proto,
  s_menu_item_proto, sk_select, sk_update, sk_do_action, s_bold, s_italic,
  s_underline, s_outline, s_shadow, s_condense, s_extend, sk_enabled,
  s_hardware_address, sk_allocate, sk_dispose;
extern int hasAppleEvents;

/* forward declarations */
LOCAL char *get_item_string _((LVAL item));
LOCAL Style get_item_style _((LVAL item));
LOCAL pascal void LispMenuSelect _((short i, short m));


/***********************************************************************/
/**                                                                   **/
/**                       MENU-PROTO Definitions                      **/
/**                                                                   **/
/***********************************************************************/

# define get_menu_id(m) ((int) getfixnum(slot_value(m, s_id)))

LOCAL Style get_item_style();

/***********************************************************************/
/**                                                                   **/
/**                     MENU-ITEM-PROTO Definitions                   **/
/**                                                                   **/
/***********************************************************************/

LOCAL char *get_item_string();

/***********************************************************************/
/**                                                                   **/
/**                        Support Function                           **/
/**                                                                   **/
/***********************************************************************/

LOCAL LVAL GetMenuList(void)
{
  return(slot_value(getvalue(s_menu_proto), s_menu_list));
}

/* find the position of the item in the menu */
LOCAL int get_item_position(LVAL menu, LVAL item)
{
  int i;
  LVAL items;
  
  for (items = slot_value(menu, s_items), i = 1;
       consp(items) && car(items) != item; i++, items = cdr(items))
    ;
  if (item != car(items)) xlfail("item not in the menu");
  return(i);
}

/***********************************************************************/
/**                                                                   **/
/**                            Menu Functions                         **/
/**                                                                   **/
/***********************************************************************/

int StMObInstalled(LVAL m)
{
  return(StMObAllocated(m) && GetMenuHandle(get_menu_id(m)) != nil);
}

/* find menu object with given hardware address */
LVAL get_menu_by_hardware(IVIEW_MENU m)
{
  LVAL menu = NIL, next;
  
  for (next = GetMenuList();
       menu == NIL && consp(next); next = cdr(next)) 
    if (StMObAllocated(car(next)) && m == (IVIEW_MENU) get_menu_address(car(next)))
      menu = car(next);
  
  if (menu == NIL) xlfail("can't find menu with this handle");
  return(menu);
}

/* find lisp menu with a specified macintosh menuID */
LOCAL LVAL get_menu_by_id(int m)
{
  return(get_menu_by_hardware(GetMenuHandle(m)));
}

/* menu select function for SkelMenu. Sends :SELECT message to the menu. */
LOCAL pascal void LispMenuSelect(short i, short m)
{
  /* Unhilite the menu bar */
  HiliteMenu(0);
  
  send_message1(get_menu_by_id(m), sk_select, i);
}

/* send an installed menu the :UPDATE message */
extern pascal void UpdateLispMenus(void)
{
  LVAL list;
  for (list = GetMenuList(); consp(list); list = cdr(list))
    send_message(car(list), sk_update);
}

/* allocate a macintosh internal menu */
LOCAL id_in_use(int id)
{
  LVAL next;
  
  for (next = GetMenuList(); consp(next); next = cdr(next)) {
    if (id == get_menu_id(car(next))) return(TRUE);
  }
  return(FALSE);
}
  
LOCAL unique_id(void)
{
  static int id = 2000;
  
  if (id > 32000) id = 2000;
  id++;
  
  while (id_in_use(id)) id++;
  return(id);
}

VOID StMObAllocateMach(LVAL menu)
{
  MenuHandle theMenu;
  LVAL title;
  int menuID;
  
  title = slot_value(menu, s_title);
  
  menuID = unique_id();
  
  CtoPstr(getstring(title));
  theMenu = NewMenu(menuID, (StringPtr) getstring(title));
  PtoCstr((StringPtr) getstring(title));
  if (theMenu == NULL) xlfail("menu allocation failed");
  set_menu_address((CPTR) theMenu, menu);
  set_slot_value(menu, s_id, cvfixnum((FIXTYPE) menuID));
  
  if (kind_of_p(menu, getvalue(s_apple_menu_proto))) {
    if (! hasAppleEvents) InsertMenuItem(theMenu, "\p(-", 0);
    AppendResMenu (theMenu, 'DRVR');
  }
}

/* dispose of a macintosh menu */
VOID StMObDisposeMach(LVAL menu)
{
  if (StMObAllocated(menu)) SkelRmveMenu((MenuHandle) get_menu_address(menu));
  if (StMObAllocated(menu)) DisposeMenu((MenuHandle) get_menu_address(menu));
}

/* add items to a macintosh internal menu */
VOID StMObAppendItems(LVAL menu, LVAL items)
{
  LVAL item;
  char *s;
  int i;
  MenuHandle theMenu;
  
  if (StMObAllocated(menu)) {
    theMenu = (MenuHandle) get_menu_address(menu);
    i = llength(slot_value(menu, s_items)) - llength(items);
    if (i < 0) xlfail("append list should not exceed item list");
    
    for (; consp(items); items = cdr(items), i++) {
      item = car(items);
      s = get_item_string(item);
      CtoPstr(s);
      InsertMenuItem(theMenu, (StringPtr) s, i);
      PtoCstr((StringPtr) s);
      SetItemStyle(theMenu, i, get_item_style(item));
    }
  }
}

/* remove item from a macintosh menu */
VOID StMObDeleteItem(LVAL menu, LVAL item)
{
  if (StMObAllocated(menu)) 
    DeleteMenuItem((MenuHandle) get_menu_address(menu), get_item_position(menu, item));
}

/* install a macintosh menu */
VOID StMObInstall(LVAL menu)
{
  if (! StMObInstalled(menu)) {
    if (! StMObAllocated(menu)) StMObAllocate(menu);
    if (! SkelMenu((MenuHandle) get_menu_address(menu), LispMenuSelect, nil, false, true))
      xlfail("menu installation failed");;
  }
}

/* remove a macintosh menu */
VOID StMObRemove(LVAL menu)
{
  if (StMObAllocated(menu)) SkelRmveMenu((MenuHandle) get_menu_address(menu));
  if (StMObAllocated(menu)) StMObDispose(menu);
}

/* enable or disable a macintosh menu */
VOID StMObEnable(LVAL menu, int enable)
{
  if (StMObAllocated(menu)) {
    if (enable) EnableItem((MenuHandle) get_menu_address(menu), 0);
    else DisableItem((MenuHandle) get_menu_address(menu), 0);
    if (StMObInstalled(menu)) DrawMenuBar();
  }
  set_slot_value(menu, s_enabled, (enable) ? s_true : NIL);
}

int StMObPopup(LVAL menu, int left, int top, LVAL window)
{
  IVIEW_MENU theMenu;
  IVIEW_WINDOW w;
  int item, menuID;
  GrafPtr SavePort;
  Point pt;
  
  StMObAllocate(menu);
  theMenu = (IVIEW_MENU) get_menu_address(menu);
  menuID = get_menu_id(menu);
  if (window != NIL && (w = (IVIEW_WINDOW) GETWINDOWADDRESS(window)) != nil) {
    GetPort(&SavePort);
    SetPort(w);
    pt.h = left; pt.v = top;
    LocalToGlobal(&pt);
    left = pt.h; top = pt.v;
    SetPort(SavePort);
  }
  if (! StillDown()) {
    while (! Button()) ;
    FlushEvents(mDownMask | mUpMask, 0);
  }
  InsertMenu(theMenu, -1);
  item = LoWord(PopUpMenuSelect(theMenu, top, left, 1));
  DeleteMenu(menuID);
  StMObDispose(menu);
  return(item);
}
  
/***********************************************************************/
/**                                                                   **/
/**                         Menu Item Functions                       **/
/**                                                                   **/
/***********************************************************************/

/* Get a string for use by AppendMenu. Style info is not encoded. */
LOCAL char *get_item_string(LVAL item)
{
  LVAL title, key, mark, enabled;
  static char *s;
    
  if (! menu_item_p(item)) xlerror("not a menu item", item);
  
  title = slot_value(item, s_title);
  if (! stringp(title)) xlerror("title is not a string", title);
  key = slot_value(item, s_key);
  mark = slot_value(item, s_mark);
  enabled = slot_value(item, s_enabled);
  
  s = buf;
  if (enabled == NIL)
    s += sprintf(s, "(");
  if (charp(key))
    s += sprintf(s, "/%c", getchcode(key));
  if (mark == s_true)
    s += sprintf(s, "!%c", 0x12);
  else if (charp(mark))
    s += sprintf(s, "!%c", getchcode(key));
  sprintf(s, "%s", getstring(title));
  return(buf);
}

/* Convert style symbol to Style value */
static Style style_value(LVAL sym)
{
  if (sym == NIL) return(0);
  else if (! symbolp(sym)) xlerror("not a symbol", sym);
  else if (sym == s_bold) return(bold);
  else if (sym == s_italic) return(italic);
  else if (sym == s_underline) return(underline);
  else if (sym == s_outline) return(outline);
  else if (sym == s_shadow) return(shadow);
  else if (sym == s_condense) return(condense);
  else if (sym == s_extend) return(extend);
  else xlerror("unknown style symbol", sym);
  return 0; /* not reached */
}

/* compute the style value for a style symbol or list using bit-or */
LOCAL Style get_item_style(LVAL item)
{
  LVAL style;
  Style s;
  
  style = slot_value(item, s_style);
  if (consp(style)) {
    for (s = 0; consp(style); style = cdr(style))
      s = s | style_value(car(style));
    return(s);
  }
  else return (style_value(style));
}
	
/* adjust internal implementation of allocated menu to new instance value */ 
VOID StMObSetItemProp(LVAL item, int which)
{
  char *s, ch;
  MenuHandle theMenu;
  LVAL menu;
  int i;
  
  menu = slot_value(item, s_menu);
  if (menu != NIL && StMObAllocated(menu)) {
    theMenu = (MenuHandle) get_menu_address(menu);
    i = get_item_position(menu, item);
    switch (which) {
    case 'T': {
                LVAL title = slot_value(item, s_title);
                if (! stringp(title))
                  xlerror("title is not a string", title);
                s = (char *) getstring(title); 
                CtoPstr(s);
                SetMenuItemText(theMenu, i, (StringPtr) s);
                PtoCstr((StringPtr) s);
                break;
              }
    case 'K': DeleteMenuItem(theMenu, i);
              s = get_item_string(item);
              CtoPstr(s);
              InsertMenuItem(theMenu, (StringPtr) s, i - 1);
              PtoCstr((StringPtr) s);
              SetItemStyle(theMenu, i, get_item_style(item));
              break;
    case 'M': {
                LVAL mark = slot_value(item, s_mark);
                CheckItem(theMenu, i, FALSE);
                if (mark == s_true) ch = 0x12;
                else if (charp(mark)) ch = getchcode(mark);
                else break; 
                SetItemMark(theMenu, i, ch);
                break;
              }
    case 'S': SetItemStyle(theMenu, i, get_item_style(item)); break;
    case 'A': break;
    case 'E': if (slot_value(item, s_enabled) != NIL) 
                EnableItem(theMenu, i);
              else DisableItem(theMenu, i);
              break;
    default:  xlfail("unknown item instance variable");
    }
  }
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                    APPLE-MENU-PROTO Methods                       **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

LVAL xsapple_menu_isnew(void) { return(xsmenu_isnew()); }

LVAL xsapple_menu_select(void)
{
  LVAL menu = peekarg(0), item = peekarg(1);
  int i, n;
  GrafPtr SavePort;
  
  if (! menu_p(menu)) xlerror("not a menu", menu);
  if (! fixp(item)) xlerror("not an integer", item);

  i = getfixnum(item);
  n = llength(slot_value(menu, s_items));
  
  if (i <= n) return(xsmenu_select());
  else {
    menu = xlgetarg();
    i = getfixnum(xlgetarg());
    xllastarg();
    
    if (StMObAllocated(menu)) {
      GetPort (&SavePort);
      GetMenuItemText ((MenuHandle) get_menu_address(menu), i, (StringPtr) buf);  /* get DA name */
      OpenDeskAcc((StringPtr) buf);                          /* open it     */
      SetPort (SavePort);
    }
    return(NIL);
  }
}

/* about alert for the */
# define	aboutAlrt		1000
#ifdef applec
#define COMPILER "\pMPW C, V3.2"
#endif /* applec */
#ifdef THINK_C
#define COMPILER "\pThink C, V7.0"
#endif /* THINK_C */
#ifdef __MWERKS__
#define COMPILER "\pMetroWerks CodeWarrior"
#endif /* __MWERKS__ */
LVAL xsabout_xlisp_stat(void) 
{
  xllastarg();
  sprintf(buf, "Release %d.%d.%d%s.",
	  XLS_MAJOR_RELEASE, XLS_MINOR_RELEASE, XLS_SUBMINOR_RELEASE,
	  XLS_RELEASE_STATUS);
  CtoPstr(buf);
  ParamText((StringPtr) buf, COMPILER, "\p", "\p");
  Alert (aboutAlrt, nil);
  return(NIL);
}
