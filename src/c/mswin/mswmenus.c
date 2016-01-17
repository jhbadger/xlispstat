/* mswmenus - Low Level Menu Objects for Microsoft Windows             */
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

/* external variables */
extern LVAL s_title, s_items, s_enabled, s_id, s_menu_list,
  s_mark, s_menu, s_menu_proto, s_menu_item_proto, sk_select,
  sk_update, sk_do_action, s_hardware_address;

extern HWND hWndFrame, hWndClient;
extern HMENU hMainMenu, hWinMenu;

/* static variables */
static int InPopup = FALSE, PopupItem = 0;

#define get_menu_id(m) ((int) getfixnum(slot_value(m, s_id)))

#define FIRSTMENU 32
#define MAXITEMS  128
#define LASTMENU  256
#define MENUITEMMENU(x) ((x)/MAXITEMS)
#define MENUITEMITEM(x) ((x)%MAXITEMS)
#define MAKEITEMINDEX(m,i) (((m)*MAXITEMS)+(i))

/* forward declaration */
static char *get_item_string(LVAL item);

/***********************************************************************/
/**                                                                   **/
/**                       Support Functions                           **/
/**                                                                   **/
/***********************************************************************/

static LVAL GetMenuList(void)
{
  return(slot_value(getvalue(s_menu_proto), s_menu_list));
}

/* find the position of the item in the menu */
static int get_item_position(LVAL menu, LVAL item)
{
  int i;
  LVAL items;
  
  for (items = slot_value(menu, s_items), i = 0;
       consp(items) && car(items) != item; i++, items = cdr(items))
    ;
  if (item != car(items)) xlfail("item not in the menu");
  return(i);
}

/* find index in menu bar; returns -1 if not installed */
static int get_menu_position(HMENU addr)
{
  int n, i;

  if (addr == NULL) return -1;  
  n = GetMenuItemCount(hMainMenu);
  for (i = 0; i < n; i++)
    if (addr == GetSubMenu(hMainMenu, i))
      return(i);
  return(-1);
}

void MSWResetMenus(void)
{
  InPopup = FALSE;
  PopupItem = 0;
}

/* ### this should be in the hardware objects file ### */
static void zero_hardware_address(LVAL menu)
{
  LVAL addr = slot_value(menu, s_hardware_address);
  if (consp(addr) && consp(cdr(addr)))
    car(cdr(addr)) = cvfixnum((FIXTYPE) 0);
}

/***********************************************************************/
/**                                                                   **/
/**                      Event Loop Interface                         **/
/**                                                                   **/
/***********************************************************************/

BOOL IsLispMenuItem(WORD wParam)
{
  LVAL next;
  int m;

  m = MENUITEMMENU(wParam);
  for (next = GetMenuList(); consp(next); next = cdr(next))
    if (StMObAllocated(car(next)) && m == get_menu_id(car(next)))
      return(TRUE);
  return(FALSE);
}

BOOL IsLispMenuHandle(HMENU wParam)
{
  LVAL next;

  for (next = GetMenuList(); consp(next); next = cdr(next))
    if (StMObAllocated(car(next)) && wParam == get_menu_address(car(next)))
      return(TRUE);
  return(FALSE);
}

void LispMenuSelect(WORD wParam)
{
  LVAL next;
  int m, i;

  m = MENUITEMMENU(wParam);
  i = MENUITEMITEM(wParam);
  for (next = GetMenuList(); consp(next); next = cdr(next)) {
    if (StMObAllocated(car(next)) && m == get_menu_id(car(next))) {
      if (InPopup) PopupItem = i + 1;
      else send_callback_message1(car(next), sk_select, i + 1);
      return;
    }
  }
}

void LispMenuUpdate(HMENU wParam)
{
  if (! InPopup) send_callback_message(get_menu_by_hardware(wParam), sk_update);
}

/***********************************************************************/
/**                                                                   **/
/**                            Menu Functions                         **/
/**                                                                   **/
/***********************************************************************/

int StMObInstalled(LVAL m)
{
  return(StMObAllocated(m) && get_menu_position(get_menu_address(m)) != -1);
}

/* find menu object with given hardware address */
LVAL get_menu_by_hardware(HMENU m)
{
  LVAL menu = NIL, next;

  for (next = GetMenuList();
       menu == NIL && consp(next); next = cdr(next))
    if (StMObAllocated(car(next)) && m == get_menu_address(car(next)))
      menu = car(next);

  if (menu == NIL) xlfail("can't find menu with this handle");
  return(menu);
}

/* allocate an internal menu */
static int id_in_use(int id)
{
  LVAL next;
  
  for (next = GetMenuList(); consp(next); next = cdr(next)) {
    if (id == get_menu_id(car(next))) return(TRUE);
  }
  return(FALSE);
}
  
static int unique_id(void)
{
  int id;

  for (id = FIRSTMENU; id < LASTMENU; id++) {
    if (! id_in_use(id)) return(id);
  }
  xlfail("too many menus");
  return(0);
}

void StMObAllocateMach(LVAL menu)
{
  HMENU theMenu;
  int menuID;

  menuID = unique_id();
  theMenu = CreatePopupMenu();
  if (! theMenu) xlfail("menu allocation failed");
  set_menu_address(theMenu, menu);
  set_slot_value(menu, s_id, cvfixnum((FIXTYPE) menuID));
}

/* dispose of a macintosh menu */
void StMObDisposeMach(LVAL menu)
{
  HWND addr;
  if (StMObInstalled(menu)) StMObRemove(menu);
  if (StMObAllocated(menu)) {
    /* test is needed because menu is destroyed after deleting from menu bar */
    addr = get_menu_address(menu);
    if (addr) DestroyMenu((HMENU) addr);
  }
}

/* add items to a macintosh internal menu */
void StMObAppendItems(LVAL menu, LVAL items)
{
  LVAL item;
  char *s;
  int i, flags, id;
  HMENU theMenu;
  
  if (StMObAllocated(menu)) {
    theMenu = get_menu_address(menu);
    id = get_menu_id(menu);
    i = llength(slot_value(menu, s_items)) - llength(items);
    if (i < 0) xlfail("append list should not exceed item list");
    
    for (; consp(items); items = cdr(items), i++) {
      item = car(items);
      s = get_item_string(item);
      if (s[0] == '-') AppendMenu((HMENU) theMenu, MF_SEPARATOR, 0, NULL);
      else {
	flags = MF_STRING;
	if (slot_value(item, s_mark) != NIL) flags |= MF_CHECKED;
	if (slot_value(item, s_enabled) == NIL) flags |= MF_GRAYED;
	AppendMenu((HMENU) theMenu, flags, MAKEITEMINDEX(id, i), s);
      }
    }
  }
}

/* remove item from a menu */
/* this thing is so involved because the item id's have to be fixed up */
/* to reflect the shift in position */
void StMObDeleteItem(LVAL menu, LVAL item)
{
  HMENU addr;
  int n, i, j, id, flags;
  LVAL items;
  char *s;

  if (StMObAllocated(menu)) {
    addr = get_menu_address(menu);
    id = get_menu_id(menu);
    i = get_item_position(menu, item);
    for (j = 0, items = slot_value(menu, s_items);
	 j < i && consp(items);
	 j++, items = cdr(items));
    n = GetMenuItemCount((HMENU) addr);
    for (; i < n; n--) DeleteMenu((HMENU) addr, i, MF_BYPOSITION);
    if (consp(items)) items = cdr(items);
    for (; consp(items); items = cdr(items), i++) {
      item = car(items);
      s = get_item_string(item);
      if (s[0] == '-') AppendMenu((HMENU) addr, MF_SEPARATOR, 0, NULL);
      else {
	flags = MF_STRING;
	if (slot_value(item, s_mark) != NIL) flags |= MF_CHECKED;
	if (slot_value(item, s_enabled) == NIL) flags |= MF_GRAYED;
	AppendMenu((HMENU) addr, flags, MAKEITEMINDEX(id, i), s);
      }
    }
  }
}

/* install a menu */
void StMObInstall(LVAL menu)
{
  int pos, enabled;
  LVAL title;

  title = slot_value(menu, s_title);
  if (! stringp(title)) xlfail("title must be a string");
  pos = get_menu_position(hWinMenu);
  if (pos != -1 && ! StMObInstalled(menu)) {
    if (! StMObAllocated(menu)) StMObAllocate(menu);
    InsertMenu(hMainMenu,
	       pos,
	       MF_POPUP|MF_BYPOSITION,
	       (UINT) get_menu_address(menu),
	       (char *) getstring(title));
    enabled = (slot_value(menu, s_enabled) != NIL) ? TRUE : FALSE;
    EnableMenuItem(hMainMenu,
		   pos,
		   (enabled ? MF_ENABLED : MF_GRAYED) | MF_BYPOSITION);
    SendMessage(hWndClient, WM_MDISETMENU, (WPARAM)hMainMenu,(LPARAM)hWinMenu);
    DrawMenuBar(hWndFrame);
  }
}

/* remove a menu */
void StMObRemove(LVAL menu)
{
  int m;

  if (! StMObAllocated(menu)) return;
  m = get_menu_position(get_menu_address(menu));
  if (m != -1) {
    DeleteMenu(hMainMenu, m, MF_BYPOSITION);
    SendMessage(hWndClient, WM_MDISETMENU, (WPARAM)hMainMenu,(LPARAM)hWinMenu);
    DrawMenuBar(hWndFrame);
    /*
     * DeleteMenu disposes of the popup menu, so this is added to avoid
     * disposing of an invalid handle -- hopefully it does not cause
     * other problems.
     */
    zero_hardware_address(menu);
  }
  if (StMObAllocated(menu)) StMObDispose(menu);
}

/* enable or disable a menu */
void StMObEnable(LVAL menu, int enable)
{
  int m;
  HMENU addr;

  if (StMObAllocated(menu) && StMObInstalled(menu)) {
    addr = get_menu_address(menu);
    m = get_menu_position(addr);
    EnableMenuItem(hMainMenu,
		   m,
		   (enable ? MF_ENABLED : MF_GRAYED) | MF_BYPOSITION);
    DrawMenuBar(hWndFrame);
  }
  set_slot_value(menu, s_enabled, (enable) ? s_true : NIL);
}

int StMObPopup(LVAL menu, int left, int top, LVAL window)
{
  HMENU theMenu;
  HWND w;
  POINT pt;

  if (window == NIL || (w = GETWINDOWADDRESS(window)) == 0)
    w = hWndFrame;

  pt.x = left; pt.y = top;
  ClientToScreen((HWND) w, &pt);
  left = pt.x; top = pt.y;

  InPopup = TRUE;
  PopupItem = 0;
  StMObAllocate(menu);
  theMenu = get_menu_address(menu);
  if (TrackPopupMenu((HMENU) theMenu, 0, left, top, 0, (HWND) w, NULL)) {
    MSG msg;
    extern HWND hWndClient;
    extern HACCEL hAccel;
    if (PeekMessage(&msg, w, WM_COMMAND, WM_COMMAND, PM_REMOVE) &&
        ! TranslateMDISysAccel(hWndClient, &msg) &&
        ! TranslateAccelerator(hWndFrame, hAccel, &msg)) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }
  StMObDispose(menu);
  InPopup = FALSE;

  return(PopupItem);
}
  
/***********************************************************************/
/**                                                                   **/
/**                         Menu Item Functions                       **/
/**                                                                   **/
/***********************************************************************/

/* Get a string for use by AppendMenu. */
static char *get_item_string(LVAL item)
{
  LVAL title;

  if (! menu_item_p(item)) xlerror("not a menu item", item);
  title = slot_value(item, s_title);
  if (! stringp(title)) xlerror("title is not a string", title);
  return((char *) getstring(title));
}

/* adjust internal implementation of allocated menu to new instance value */
void StMObSetItemProp(LVAL item, int which)
{
  char *s;
  HMENU theMenu;
  LVAL menu, title;
  int i, flags, id;

  menu = slot_value(item, s_menu);
  if (menu != NIL && StMObAllocated(menu)) {
    theMenu = get_menu_address(menu);
    id = get_menu_id(menu);
    i = get_item_position(menu, item);
    title = slot_value(item, s_title);
    if (! stringp(title))
      xlerror("title is not a string", title);
    s = (char *) getstring(title);
    if (s[0] == '-' && which != 'T') return;
    switch (which) {
    case 'T':
      if (s[0] == '-') {
	flags = MF_SEPARATOR | MF_BYPOSITION;
	ModifyMenu((HMENU) theMenu, i, flags, 0, NULL);
      }
      else {
	flags = MF_STRING | MF_BYPOSITION;
	flags |= (slot_value(item, s_mark) != NIL) ? MF_CHECKED : MF_UNCHECKED;
	flags |= (slot_value(item, s_enabled) != NIL) ? MF_ENABLED : MF_GRAYED;
	ModifyMenu((HMENU) theMenu, i, flags, MAKEITEMINDEX(id, i), s);
      }
      break;
    case 'K': break;
    case 'M':
      flags = (slot_value(item, s_mark) != NIL) ? MF_CHECKED : MF_UNCHECKED;
      flags |= MF_BYPOSITION;
      CheckMenuItem((HMENU) theMenu, i, flags);
      break;
    case 'A': break;
    case 'E':
      flags = (slot_value(item, s_enabled) != NIL) ? MF_ENABLED : MF_GRAYED;
      flags |= MF_BYPOSITION;
      EnableMenuItem((HMENU) theMenu, i, flags);
      break;
    default:  xlfail("unknown item instance variable");
    }
  }
}
