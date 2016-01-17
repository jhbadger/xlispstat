/* mswdlg - Low Level Dialog Objects for Microsoft Windows             */
/* XLISP-STAT 2.1 Copyright (c) 1990, 1991, by Luke Tierney            */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "dialogs.h"
#include "wxlisp.h"
#include "ledit.h"

/* external variables */
extern HWND hWndFrame, hWndClient;
extern HINSTANCE hInst;
extern HANDLE hAccel;
extern LVAL s_text_length;

/* static variables */
static char szXLSDlgClass[] = "XLSDialog";
static char szXLSMDIDlgClass[] = "XLSMDIDialog";
static char szXLSMDlgClass[] = "XLSModalDialog";
static BOOL InModalDialog = FALSE;
static LVAL ModalItem = NIL;
#ifdef WIN32
static WNDPROC pDefBtnProc = NULL;
static WNDPROC pDefEditTextProc = NULL;
static WNDPROC pDefScrollProc = NULL;
static WNDPROC pDefListProc = NULL;
#endif /* WIN32 */

/* function prototypes */
LRESULT CALLBACK XLSDlgProc(HWND, UINT, WPARAM, LONG);
LRESULT CALLBACK XLSMDIDlgProc(HWND, UINT, WPARAM, LONG);
LRESULT CALLBACK XLSMDlgProc(HWND, UINT, WPARAM, LONG);
static void InstallItem(HWND, LVAL);
static void InstallButtonItem(HWND, LVAL);
static void InstallToggleItem(HWND, LVAL);
static void InstallChoiceItem(HWND, LVAL);
static void InstallTextItem(HWND, LVAL);
static void InstallScrollItem(HWND, LVAL);
static void InstallListItem(HWND, LVAL);
static void SetClusterValue(HWND, int);
#ifdef WIN32
static void DialogDoDefaultButton(LVAL dialog);
static LRESULT CALLBACK CustomButtonProc(HWND hWnd,UINT message,WPARAM wPrm,LPARAM lPrm);
static LRESULT CALLBACK CustomEditTextProc(HWND hWnd,UINT message,WPARAM wPrm,LPARAM lPrm);
static LRESULT CALLBACK CustomScrollProc(HWND hWnd,UINT message,WPARAM wPrm,LPARAM lPrm);
static LRESULT CALLBACK CustomListProc(HWND hWnd,UINT message,WPARAM wPrm,LPARAM lPrm);
#endif /* WIN32 */

#ifdef WIN32
#  define DIALOG_FONT DEFAULT_GUI_FONT
#  define DIALOG_BACKGROUND COLOR_MENU
#  define AVE_CHAR_PAD 2
#  define CHOICE_WIDTH_FUZZ 6
#else
#  define DIALOG_FONT SYSTEM_FIXED_FONT
#  define DIALOG_BACKGROUND COLOR_WINDOW
#  define AVE_CHAR_PAD 0
#  define CHOICE_WIDTH_FUZZ 4
#endif
#define LIST_USE_LEADING FALSE  /**** inline this */
#define REAL_LIST_ITEM_PAD 0 /**** drop LIST_ITEM_PAD */
/**** need to rethink size calculations for non-fixed-width fonts */


/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

static void check_alloc(HANDLE p)
{
  if (! p) xlfail("allocation failed");
}

static void check_lock(void *p)
{
  if (! p) xlfail("lock failed");
}

static int FindItemType(LVAL item)
{
  if (consp(item)) return(ITEM_LIST);
  else if (button_item_p(item)) return(BUTTON_ITEM);
  else if (toggle_item_p(item)) return(TOGGLE_ITEM);
  else if (text_item_p(item)) return(TEXT_ITEM);
  else if (choice_item_p(item)) return(CHOICE_ITEM);
  else if (scroll_item_p(item)) return(SCROLL_ITEM);
  else if (list_item_p(item)) return(LIST_ITEM);
  else xlfail("item of unknown type");
  return(0);
}

static int count_hardware_items(LVAL items)
{
  LVAL temp;
  int n;

  if (! consp (items)) return(0);
  else {
    for (n = 0; consp(items); items = cdr(items)) {
      switch(FindItemType(car(items))) {
      case CHOICE_ITEM: 
	temp = slot_value(car(items), s_text);
	if (! consp(temp)) xlerror("not a list", temp);
	n += (int) llength(temp);
        break;
      case ITEM_LIST: n += count_hardware_items(car(items)); break;
      default: n += 1;
      }
    }
  }
  return(n);
}

static Point AvCharSize(BOOL leaded)
{
  Point pt;
  HDC hDC;
  TEXTMETRIC tm;

  hDC = GetDC(hWndFrame);
  SelectObject(hDC, GetStockObject(DIALOG_FONT));
  GetTextMetrics(hDC, &tm);
  pt.h = tm.tmAveCharWidth + AVE_CHAR_PAD; /* to correct font width problems */
  pt.v = tm.tmHeight;
  if (leaded) pt.v += tm.tmExternalLeading;
  ReleaseDC(hWndFrame, hDC);
  return(pt);
}

static Point text_size(char *s)
{
  char *bp;
  int w;
  Point sz;
  
  for (w = 0, sz.h = 0, sz.v = 0; *s != '\0'; s++) {
    for (bp = buf; *s != '\0' && *s != '\r' && *s != '\n'; s++, bp++)
      *bp = *s;
    *bp = '\0';
    w = strlen(buf);
    if (sz.h < w) sz.h = w;
    sz.v++;
    if (*s == '\0') break;
  }
  return(sz);
}

static void truncateListEntry(char *s)
{
  if (strlen(s) > MAX_ENTRY_LENGTH) {
    s = s + MAX_ENTRY_LENGTH - 3;
    s[0] = '.'; s[1] = '.'; s[2] = '.'; s[3] = '\0';
  }
}

static BOOL FindItemData(HWND theDialog,
			 int wParam,
			 DialogItemData *di)
{
  HANDLE hd;
  DialogData *dd;
  DialogItemData *did;
  int index, result;

  index = wParam - ITEM_INDEX_BASE;
  if (index < 0) return(FALSE);

  hd = GETDIALOGDATA(theDialog);
  dd = (DialogData *) GlobalLock(hd);
  check_lock(dd);
  did = GETITEMDATA(dd);
  if (index < dd->count) {
    *di = did[index];
    result = TRUE;
  }
  else result = FALSE;
  GlobalUnlock(hd);
  return(result);
}

static BOOL FindItemObjectData(LVAL item, DialogItemData *di)
{
  LVAL dialog;
  HANDLE hd;
  DialogData *dd;
  DialogItemData *did;
  int i, found;

  dialog = slot_value(item, s_dialog);
  if (dialog == NIL || ! check_dialog_address(dialog)) return(FALSE);

  hd = GETDIALOGDATA(GETDIALOGADDRESS(dialog));
  dd = (DialogData *) GlobalLock(hd);
  check_lock(dd);
  did = GETITEMDATA(dd);
  for (i = 0, found = FALSE; i < dd->count && ! found; i++) {
     if (did[i].object == item) {
       *di = did[i];
      found = TRUE;
    }
  }
  GlobalUnlock(hd);
  return(found);
}

/***********************************************************************/
/**                                                                   **/
/**                    Internal Dialog Data Functions                 **/
/**                                                                   **/
/***********************************************************************/

static HANDLE MakeDialogData(LVAL dialog)
{
  int numItems;
  HANDLE hItems;
  DialogData *dd;

  numItems = count_hardware_items(slot_value(dialog, s_items));
  hItems = GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT,
		       sizeof(DialogData) + numItems * sizeof(DialogItemData));
  check_alloc(hItems);
  dd = (DialogData *) GlobalLock(hItems);
  check_lock(dd);
  dd->count = 0;
  dd->object = dialog;
  dd->dflt = -1;
  GlobalUnlock(hItems);
  return(hItems);
}

static void FreeDialogData(HANDLE hItems)
{
  GlobalFree(hItems);
}

static void InstallItemList(HWND theDialog, LVAL items)
{
  for (; consp(items); items = cdr(items))
    if (consp(car(items))) InstallItemList(theDialog, car(items));
    else InstallItem(theDialog, car(items));
}

static void InstallItem(HWND theDialog, LVAL item)
{
  if (! dialog_item_p(item)) xlerror("not a dialog item", item);
  switch (FindItemType(item)) {
  case BUTTON_ITEM: InstallButtonItem(theDialog, item); break;
  case TOGGLE_ITEM: InstallToggleItem(theDialog, item); break;
  case CHOICE_ITEM: InstallChoiceItem(theDialog, item); break;
  case TEXT_ITEM:   InstallTextItem(theDialog, item);   break;
  case SCROLL_ITEM: InstallScrollItem(theDialog, item); break;
  case LIST_ITEM:   InstallListItem(theDialog, item);   break;
  default: xlfail("unkown item type");
  }
}

/***********************************************************************/
/**                                                                   **/
/**                Initialization and Callback Functions              **/
/**                                                                   **/
/***********************************************************************/

BOOL InitApplDialogs(HANDLE hInstance)
{
  WNDCLASS wc;

  /* class structure for modeless dialog windows */
  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = (WNDPROC) XLSDlgProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof(LONG) + sizeof(LONG);
  wc.hInstance = hInstance;
  wc.hIcon = LoadIcon(hInstance, "WXLSIcon");
  wc.hCursor = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH) (DIALOG_BACKGROUND + 1);
  wc.lpszMenuName = NULL;
  wc.lpszClassName = szXLSDlgClass;
  if (! RegisterClass(&wc)) return(FALSE);

  /* class structure for modeless MDI dialog windows */
  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = (WNDPROC) XLSMDIDlgProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof(LONG) + sizeof(LONG);
  wc.hInstance = hInstance;
  wc.hIcon = LoadIcon(hInstance, "WXLSIcon");
  wc.hCursor = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH) (DIALOG_BACKGROUND + 1);
  wc.lpszMenuName = NULL;
  wc.lpszClassName = szXLSMDIDlgClass;
  if (! RegisterClass(&wc)) return(FALSE);

  /* class structure for modal dialog windows */
  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = (WNDPROC) XLSMDlgProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof(LONG) + sizeof(LONG);
  wc.hInstance = hInstance;
  wc.hIcon = NULL;
  wc.hCursor = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH) (DIALOG_BACKGROUND + 1);
  wc.lpszMenuName = NULL;
  wc.lpszClassName = szXLSMDlgClass;
  if (! RegisterClass(&wc)) return(FALSE);
#ifdef WIN32
  //
  // these procs are used to pass the ENTER key on to the parent window to
  // mimic default button modal dialog behavior
  //
  GetClassInfo(hInstance, "button", &wc);
  pDefBtnProc = wc.lpfnWndProc;

  GetClassInfo(hInstance, "edit", &wc);
  pDefEditTextProc = wc.lpfnWndProc;

  GetClassInfo(hInstance, "scrollbar", &wc);
  pDefScrollProc = wc.lpfnWndProc;

  GetClassInfo(hInstance, "listbox", &wc);
  pDefListProc = wc.lpfnWndProc;
#endif /* WIN32 */
  return(TRUE);
}

int InitInstDialogs(HANDLE hInstance)
{
  return(TRUE);
}

static LRESULT ProcessDialogEvent(HWND hWnd,
  		                  UINT message,
			          WPARAM wPrm,
			          LONG lPrm,
                                  BOOL modeless,
				  BOOL mdidialog)
{

  LVAL dialog;
  DialogItemData di;
  int val, wid;

  switch(message) {
#ifdef WIN32
    /* Set the background color for WIN32 */
  case WM_CTLCOLORDLG:
  case WM_CTLCOLORSTATIC:
  case WM_CTLCOLORBTN:
    val = SetBkColor((HDC) wPrm, GetSysColor(DIALOG_BACKGROUND));
    SelectObject((HDC) wPrm, GetStockObject(DIALOG_FONT));
    return ((LRESULT) GetSysColorBrush(DIALOG_BACKGROUND));
#endif
  case WM_CREATE:
    if (mdidialog) {
      LPMDICREATESTRUCT mdic;
      mdic = (LPMDICREATESTRUCT) ((LPCREATESTRUCT) lPrm)->lpCreateParams;
      dialog = (LVAL) mdic->lParam;
    }
    else
      dialog = (LVAL) ((LPCREATESTRUCT) lPrm)->lpCreateParams;
    SETDIALOGOBJECT(hWnd, dialog);
    SETDIALOGDATA(hWnd, MakeDialogData(dialog));
    InstallItemList(hWnd, slot_value(dialog, s_items));
    DialogSetDefaultButton(dialog, slot_value(dialog, s_default_button));
    break;
  case WM_COMMAND:
    switch (GET_WM_COMMAND_ID(wPrm, lPrm)) {
    case IDC_SHOWWINDOW:
      if (modeless) {
	if (mdidialog) {
	  if (IsIconic(hWnd))
	    MDIRestoreWindow(hWndClient, hWnd);
	  MDIActivateWindow(hWndClient, hWnd);
	}
	else {
	  if (IsIconic(hWnd))
	    ShowWindow(hWnd,SW_RESTORE);
	}
        return(0);
      }
      break;
    case IDC_HIDEWINDOW:
      if (modeless) {
        if (! IsIconic(hWnd))
	  ShowWindow(hWnd, SW_MINIMIZE);
        return(0);
      }
      break;
    case IDC_DESTROY:
      if (mdidialog)
        MDIDestroyWindow(hWndClient, hWnd);
      else
        DestroyWindow(hWnd);
      return 0;
    default:
      if (FindItemData(hWnd, GET_WM_COMMAND_ID(wPrm, lPrm), &di)) {
	if (di.object && di.object != NIL) {
	  switch (di.type) {
	  case TOGGLE_ITEM:
	    val = ! Button_GetCheck(di.itemHandle);
	    Button_SetCheck(di.itemHandle, val);
	    set_slot_value(di.object, s_value, (val) ? s_true : NIL);
	    break;
	  case CHOICE_ITEM:
	    SetClusterValue(hWnd, di.itemNumber);
	    set_slot_value(di.object, s_value,
			   cvfixnum((FIXTYPE) di.itemNumber - di.clusterLeader));
	    break;
	  }
	  if (di.type == LIST_ITEM) {
	    switch (GET_WM_COMMAND_CMD(wPrm, lPrm)) {
	    case LBN_DBLCLK:
	      send_callback_message_1L(di.object, sk_do_action, s_true);
	      break;
	    case LBN_SELCHANGE:
	    case LBN_SELCANCEL:
	      send_callback_message(di.object, sk_do_action);
	      break;
	    }
	  }
	  else {
	    if (InModalDialog) ModalItem = di.object;
	    else send_callback_message(di.object, sk_do_action);
	  }
	}
	return(0);
      }
    }
    break;
  case WM_VSCROLL:
  case WM_HSCROLL:
    wid = GetWindowID(GET_WM_HSCROLL_HWND(wPrm, lPrm));
    switch (GET_WM_HSCROLL_CODE(wPrm, lPrm)) {
    case SB_PAGEDOWN:
    case SB_LINEDOWN:
    case SB_PAGEUP:
    case SB_LINEUP:
    case SB_TOP:
    case SB_BOTTOM:
      if (FindItemData(hWnd, wid, &di) && di.object && di.object != NIL) {
	int low, high, value, page;
	LVAL temp, item = di.object;

	temp = slot_value(item, s_min_value);
	low = fixp(temp) ? (int) getfixnum(temp) : SCROLL_MIN;
	temp = slot_value(item, s_max_value);
	high = fixp(temp) ? (int) getfixnum(temp) : SCROLL_MAX;
	temp = slot_value(item, s_value);
	value = (fixp(temp)) ? (int) getfixnum(temp) : low;
	temp = slot_value(item, s_page_increment);
	page = (fixp(temp)) ? (int) getfixnum(temp) : SCROLL_PAGE;
	switch (GET_WM_HSCROLL_CODE(wPrm, lPrm)) {
	case SB_PAGEDOWN: value += page; break;
	case SB_LINEDOWN: value++;       break;
	case SB_PAGEUP:   value -= page; break;
	case SB_LINEUP:   value--;       break;
	case SB_TOP:      value = low;   break;
	case SB_BOTTOM:   value = high;  break;
	}
	if (value < low) value = low;
	if (value > high) value = high;
	set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
	SetScrollPos(di.itemHandle, SB_CTL, value, TRUE);
	send_callback_message(item, sk_scroll_action);
      }
      return(0);
    case SB_THUMBPOSITION:
      val = GET_WM_HSCROLL_POS(wPrm, lPrm);
      if (FindItemData(hWnd, wid, &di) && di.object && di.object != NIL) {
	set_slot_value(di.object, s_value, cvfixnum((FIXTYPE) val));
	SetScrollPos(di.itemHandle, SB_CTL, val, TRUE);
	send_callback_message(di.object, sk_do_action);
      }
      return(0);
    }
    break;
#ifdef WIN32
  case WM_CHAR:
    switch (wPrm) {
    case VK_RETURN:
      // do the default button thing
      DialogDoDefaultButton(GETDIALOGOBJECT(hWnd));
      break;
    case VK_ESCAPE:
      DestroyWindow(hWnd);
      break;
    }
    break;
#endif /* WIN32 */
  case WM_QUERYENDSESSION:
  case WM_CLOSE:
    if (! InModalDialog)
      send_callback_message((LVAL) GETDIALOGOBJECT(hWnd), sk_close);
    return(0);
  case WM_DESTROY:
    FreeDialogData(GETDIALOGDATA(hWnd));
    if (mdidialog) return 0;
  }
  if (mdidialog)
    return(DefMDIChildProc(hWnd, message, wPrm, lPrm));
  else
    return(DefWindowProc(hWnd, message, wPrm, lPrm));
}

LRESULT CALLBACK XLSDlgProc(HWND hWnd, UINT message, WPARAM wPrm, LONG lPrm)
{
  return ProcessDialogEvent(hWnd, message, wPrm, lPrm, TRUE, FALSE);
}

LRESULT CALLBACK XLSMDIDlgProc(HWND hWnd, UINT message, WPARAM wPrm, LONG lPrm)
{
  return ProcessDialogEvent(hWnd, message, wPrm, lPrm, TRUE, TRUE);
}

LRESULT CALLBACK XLSMDlgProc(HWND hWnd, UINT message, WPARAM wPrm, LONG lPrm)
{
  return ProcessDialogEvent(hWnd, message, wPrm, lPrm, FALSE, FALSE);
}

void MSWResetDialogs(void)
{
  ModalItem = NIL;
  InModalDialog = FALSE;
}

/***********************************************************************/
/**                                                                   **/
/**                Allocation and Deallocation Functions              **/
/**                                                                   **/
/***********************************************************************/

void DialogAllocate(LVAL dialog)
{
  HWND theDialog;
  Point loc, size;
  char *title;
  BOOL goAway, modeless;
  DWORD flags;

  if (check_dialog_address(dialog)) DialogRemove(dialog);

  if (! stringp(slot_value(dialog, s_title)))
    xlerror("not a string", slot_value(dialog, s_title));
  title = (char *) getstring(slot_value(dialog, s_title));

  goAway = (slot_value(dialog, s_go_away) != NIL) ? TRUE : FALSE;
  modeless = (slot_value(dialog, s_type) != s_modal) ? TRUE : FALSE;

  loc = ListToPoint(slot_value(dialog, s_location));
  size = ListToPoint(slot_value(dialog, s_size));
  if (size.h < MIN_DLG_WIDTH) size.h = MIN_DLG_WIDTH;
  if (size.v < MIN_DLG_HEIGHT) size.v = MIN_DLG_HEIGHT;
  if (modeless) { // ### fix if manage to get proper frame
    /**** This is a quick hack.  It would probably be better to use a slot
	  to set the MDI/non-MDI option */
    LVAL topsym = xlenter("SYSTEM::*TOP-LEVEL-DIALOGS*");
    int mdidialog = (! boundp(topsym)) || null(getvalue(topsym));

    size.h += 2 * GetSystemMetrics(SM_CXBORDER);
    size.v += GetSystemMetrics(SM_CYBORDER);
    size.v += GetSystemMetrics(SM_CYCAPTION);

    // ### kill border so dlgframe works?
    flags = WS_POPUP | WS_DLGFRAME | WS_VISIBLE | WS_CAPTION;
    if (goAway) flags |= WS_SYSMENU;

    if (mdidialog) {
      MDICREATESTRUCT mdicreate;

      mdicreate.szClass = szXLSMDIDlgClass;
      mdicreate.szTitle = title;
      mdicreate.hOwner = hInst;
      mdicreate.x = loc.h;
      mdicreate.y = loc.v;
      mdicreate.cx = size.h;
      mdicreate.cy = size.v;
      mdicreate.style = WS_MINIMIZE;
      mdicreate.lParam = (LONG) dialog;

      theDialog = MDICreateWindow(hWndClient, &mdicreate);
      SetWindowStyle(theDialog, GetWindowStyle(theDialog)
		     & ~WS_MAXIMIZEBOX & ~WS_MINIMIZEBOX);
      MDIRestoreWindow(hWndClient, theDialog);
    }
    else {
      theDialog = CreateWindowEx(WS_EX_DLGMODALFRAME, szXLSDlgClass,
				 title, flags,
				 loc.h, loc.v, size.h, size.v,
				 NULL, NULL, hInst, (LPVOID) dialog);
    }
  }
  else {
    size.h += 2 * GetSystemMetrics(SM_CXDLGFRAME);
#ifdef WIN32
    size.v += GetSystemMetrics(SM_CYBORDER);
    size.v += GetSystemMetrics(SM_CYCAPTION);
    flags = WS_POPUP | WS_VISIBLE |WS_CAPTION;      //FC 7/20/99     
    theDialog = CreateWindowEx(WS_EX_DLGMODALFRAME, szXLSMDlgClass,
			       title, flags,
			       loc.h, loc.v, size.h, size.v,
			       NULL, NULL, hInst, (LPVOID) dialog);
#else
    size.v += 2 * GetSystemMetrics(SM_CYDLGFRAME);
    flags = WS_POPUP | WS_DLGFRAME | WS_VISIBLE;
    theDialog = CreateWindow(szXLSMDlgClass,
			     title, flags,
			     loc.h, loc.v, size.h, size.v,
			     hWndFrame, NULL, hInst, (LPVOID) dialog);
#endif
  }

  check_alloc(theDialog);
  set_dialog_address(theDialog, dialog);
}

void DialogRemove(LVAL dialog)
{
  if (check_dialog_address(dialog))
    XLSDestroyWindow(GETDIALOGADDRESS(dialog));
  if (objectp(dialog)) standard_hardware_clobber(dialog);
}

/***********************************************************************/
/**                                                                   **/
/**                       Dialog Item Functions                       **/
/**                                                                   **/
/***********************************************************************/

static void InstallBtnItem(HWND theDialog,
			   LVAL item,
			   DWORD flags,
			   int type,
			   DialogItemData *di)
{
  HANDLE hd;
  DialogItemData *data;
  DialogData *dialogData;
  HWND theItem;
  int itemIndex;
  char *text;
  Point loc, size;

  hd = GETDIALOGDATA(theDialog);
  dialogData = (DialogData *) GlobalLock(hd);
  check_lock(dialogData);

  itemIndex = (dialogData->count)++;

  if (! stringp(slot_value(item, s_text)))
    xlerror("not a string", slot_value(item, s_text));
  text = (char *) getstring(slot_value(item, s_text));

  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));

  theItem = CreateWindow("button", text,
			 WS_CHILD | WS_VISIBLE | flags,
			 loc.h, loc.v, size.h, size.v,
			 (HWND) theDialog,
                         (HMENU) (itemIndex + ITEM_INDEX_BASE),
			 hInst, NULL);
  check_alloc(theItem);

  data = GETITEMDATA(dialogData);
  data[itemIndex].type = type;
  data[itemIndex].itemNumber = itemIndex;
  data[itemIndex].itemHandle = theItem;
  data[itemIndex].object = item;
  if (di) *di = data[itemIndex];
#ifdef WIN32
  SetWindowLong(theItem, GWL_WNDPROC, (LPARAM)CustomButtonProc);
#endif /* WIN32 */
  GlobalUnlock(hd);
}

static void InstallButtonItem(HWND theDialog, LVAL item)
{
  InstallBtnItem(theDialog, item, BS_PUSHBUTTON, BUTTON_ITEM, NULL);
}

#ifdef WIN32
static LRESULT CALLBACK CustomButtonProc(HWND hWnd, UINT message,
					 WPARAM wPrm, LPARAM lPrm)
{
  if ((message == WM_CHAR) && (wPrm == VK_RETURN)) {
    SendMessage(GetParent(hWnd), message, wPrm, lPrm);
    return 0;
  }
  return CallWindowProc(pDefBtnProc, hWnd, message, wPrm, lPrm);
}

static LRESULT CALLBACK CustomScrollProc(HWND hWnd, UINT message,
					 WPARAM wPrm, LPARAM lPrm)
{
  if ((message == WM_CHAR) && (wPrm == VK_RETURN)) {
    SendMessage(GetParent(hWnd), message, wPrm, lPrm);
    return 0;
  }
  return CallWindowProc(pDefScrollProc, hWnd, message, wPrm, lPrm);
}

static LRESULT CALLBACK CustomEditTextProc(HWND hWnd, UINT message,
					   WPARAM wPrm, LPARAM lPrm)
{
  if ((message == WM_CHAR) && (wPrm == VK_RETURN)) {
    SendMessage(GetParent(hWnd), message, wPrm, lPrm);
    return 0;
  }
  return CallWindowProc(pDefEditTextProc, hWnd, message, wPrm, lPrm);
}

static LRESULT CALLBACK CustomListProc(HWND hWnd, UINT message,
				       WPARAM wPrm, LPARAM lPrm)
{
  if ((message == WM_CHAR) && (wPrm == VK_RETURN)) {
    SendMessage(GetParent(hWnd), message, wPrm, lPrm);
    return 0;
  }
  return CallWindowProc(pDefListProc, hWnd, message, wPrm, lPrm);
}
#endif /* WIN32 */

void DialogButtonGetDefaultSize(LVAL item, int *pwidth, int *pheight)
{
  LVAL text;
  Point csz;
  int n;

  text = slot_value(item, s_text);
  if (! stringp(text)) xlerror("not a string", text);
  csz = AvCharSize(TRUE);
  n = strlen((char *) getstring(text));
  if (n < MIN_BUTTON_STRLEN) n = MIN_BUTTON_STRLEN;

  if (pwidth != NULL) *pwidth = (n + 2) * csz.h;
  if (pheight != NULL) *pheight = (7 * csz.v) / 4;
}

static void InstallToggleItem(HWND theDialog, LVAL item)
{
  DialogItemData di;

  InstallBtnItem(theDialog, item, BS_CHECKBOX, TOGGLE_ITEM, &di);
  Button_SetCheck(di.itemHandle,
                  (slot_value(item, s_value) != NIL) ? TRUE : FALSE);
}

void DialogToggleGetDefaultSize(LVAL item, int *pwidth, int *pheight)
{
  LVAL text;
  Point csz;
  int n;

  text = slot_value(item, s_text);
  if (! stringp(text)) xlerror("not a string", text);
  csz = AvCharSize(TRUE);
  n = strlen((char *) getstring(text));

  if (pwidth != NULL) *pwidth = (n + 4) * csz.h;
  if (pheight != NULL) *pheight = (3 * csz.v) / 2;
}

LVAL DialogToggleItemValue(item, set, value)
	LVAL item, value;
	int set;
{
  DialogItemData itemData;

  if (set) {
    set_slot_value(item, s_value, (value != NIL) ?  s_true : NIL);
    if (FindItemObjectData(item, &itemData)) {
      Button_SetCheck(itemData.itemHandle,
		      (value != NIL) ? TRUE : FALSE);
    }
  }
  return(slot_value(item, s_value));
}

static void InstallChoiceItem(HWND theDialog, LVAL item)
{
  LVAL titles, temp;
  DialogItemData *data;
  DialogData *dialogData;
  HWND theItem;
  int itemIndex, clusterLeader, clusterSize, initial;
  char *text;
  Point loc, size;
  HANDLE hd;

  titles = slot_value(item, s_text);
  if (! consp(titles)) xlerror("not a list", titles);

  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  clusterSize = (int) llength(titles);
  size.v /= clusterSize;

  hd = GETDIALOGDATA(theDialog);
  dialogData = (DialogData *) GlobalLock(hd);
  check_lock(dialogData);

  clusterLeader = dialogData->count;
  for (; consp(titles); titles = cdr(titles)) {
    if (! stringp(car(titles))) xlerror("not a string", car(titles));
    text = (char *) getstring(car(titles));

    itemIndex = (dialogData->count)++;
    theItem = CreateWindow("button", text,
			   WS_CHILD | WS_VISIBLE | BS_RADIOBUTTON,
			   loc.h, loc.v, size.h, size.v,
			   (HWND) theDialog,
  		          (HMENU) (itemIndex + ITEM_INDEX_BASE),
			   hInst, NULL);
    check_alloc(theItem);
    loc.v += size.v;

    data = GETITEMDATA(dialogData);
    data[itemIndex].type = CHOICE_ITEM;
    data[itemIndex].itemNumber = itemIndex;
    data[itemIndex].itemHandle = theItem;
    data[itemIndex].object = item;
    data[itemIndex].clusterLeader = clusterLeader;
    data[itemIndex].clusterSize = clusterSize;
#ifdef WIN32
    SetWindowLong(theItem, GWL_WNDPROC, (LPARAM)CustomButtonProc);
#endif /* WIN32 */
  }
  temp = slot_value(item, s_value);
  initial = (fixp(temp)) ? (int) getfixnum(temp) : 0;
  if (initial < 0 || initial >= clusterSize) initial = 0;
  initial += clusterLeader;
  Button_SetCheck(data[initial].itemHandle, 1);
  GlobalUnlock(hd);
}

void DialogChoiceGetDefaultSize(LVAL item, int *pwidth, int *pheight)
{
  Point csz;
  LVAL text = slot_value(item, s_text);
  int n, k, h;
  
  csz = AvCharSize(TRUE);
  for (h = 0, n = 0; consp(text); text = cdr(text)) {
    k = strlen((char *) getstring(car(text)));
    if (n < k) n = k;
    h += (3 * csz.v) / 2;
  }
  if (pwidth != NULL) *pwidth = (n + CHOICE_WIDTH_FUZZ) * csz.h;
  if (pheight != NULL) *pheight = h;
}

static void SetClusterValue(HWND theDialog, int index)
{
  int i, n, leader;
  HANDLE hd;
  DialogData *dialogData;
  DialogItemData *data;

  if (index < 0) return;

  hd = GETDIALOGDATA(theDialog);
  dialogData = (DialogData *) GlobalLock(hd);
  check_lock(dialogData);

  if (index < dialogData->count) {
    data = GETITEMDATA(dialogData);
    leader = data[index].clusterLeader;
    n = data[index].clusterSize + leader;

    for (i = leader; i < n; i++)
      Button_SetCheck(data[i].itemHandle, 0);
    Button_SetCheck(data[index].itemHandle, 1);
  }

  GlobalUnlock(hd);
}

LVAL DialogChoiceItemValue(LVAL item, int set, int value)
{
  DialogItemData itemData;
  LVAL dialog, textlist;

  if (set) {
    textlist = slot_value(item, s_text);
    if (! consp(textlist) || value < 0 || value >= llength(textlist))
      xlfail("Value out of range");
    set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
    if (FindItemObjectData(item, &itemData)) {
      if (value < 0 || value >= itemData.clusterSize)
	xlfail("value out of range");
      dialog = slot_value(item, s_dialog);
      SetClusterValue(GETDIALOGADDRESS(dialog),
		      itemData.clusterLeader + value);
    }
  }
  return(slot_value(item, s_value));
}

static void InstallTextItem(HWND theDialog, LVAL item)
{
  HANDLE hd;
  DialogItemData *data;
  DialogData *dialogData;
  HWND theItem;
  int editable, itemIndex;
  char *text;
  Point loc, size;
  
  if (! stringp(slot_value(item, s_text)))
    xlerror("not a string", slot_value(item, s_text));
  text = (char *) getstring(slot_value(item, s_text));
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  editable = (slot_value(item, s_editable) != NIL) ? TRUE : FALSE;

  hd = GETDIALOGDATA(theDialog);
  dialogData = (DialogData *) GlobalLock(hd);
  check_lock(dialogData);
  itemIndex = (dialogData->count)++;

  strcpy(buf, text);
  if (editable) {
    theItem = CreateWindow("edit", buf, WS_CHILD | WS_VISIBLE | WS_BORDER,
			   loc.h, loc.v, size.h, size.v,
			   (HWND) theDialog,
                           (HMENU) (itemIndex + ITEM_INDEX_BASE),
			   hInst, NULL);
  }
  else {
    theItem = CreateWindow("static", buf, WS_CHILD | WS_VISIBLE,
			   loc.h, loc.v, size.h, size.v,
			   (HWND) theDialog,
                           (HMENU) (itemIndex + ITEM_INDEX_BASE),
			   hInst, NULL);
  }
  check_alloc(theItem);
#ifdef WIN32  
  if (editable) {
   SetWindowLong(theItem, GWL_WNDPROC, (LPARAM)CustomEditTextProc);
  }
#endif /* WIN32 */
  data = GETITEMDATA(dialogData);
  data[itemIndex].type = TEXT_ITEM;
  data[itemIndex].itemNumber = itemIndex;
  data[itemIndex].itemHandle = theItem;
  data[itemIndex].object = item;
  GlobalUnlock(hd);
}

void DialogTextGetDefaultSize(LVAL item, int *pwidth, int *pheight)
{
  Point csz, tsz;
  LVAL text = slot_value(item, s_text);
  LVAL text_length = slot_value(item, s_text_length);

  tsz.h = 0; tsz.v = 0;
  csz = AvCharSize(TRUE);
  if (stringp(text)) {
    tsz = text_size((char *) getstring(text));
  }
  if (fixp(text_length)) {
    if (tsz.h < getfixnum(text_length)) tsz.h = (int) getfixnum(text_length);
    if (tsz.v < 1) tsz.v = 1;
  }
  if (slot_value(item, s_editable) != NIL) {
    if (pwidth != NULL) *pwidth = csz.h * tsz.h + EDIT_TEXT_PAD;
    if (pheight != NULL) *pheight = csz.v * tsz.v + EDIT_TEXT_PAD;
  }
  else {
    if (pwidth != NULL) *pwidth = csz.h * tsz.h + STATIC_TEXT_PAD;
    if (pheight != NULL) *pheight = csz.v * tsz.v + STATIC_TEXT_PAD;
  }
}

LVAL DialogTextItemText(LVAL item, int set, char *text)
{
  DialogItemData di;
  int n;

  if (set) set_slot_value(item, s_text, cvstring(text));
  if (FindItemObjectData(item, &di)) {
    if (set) {
      strcpy(buf, text);
      SetWindowText(di.itemHandle, buf);
    }
    n = GetWindowText(di.itemHandle, buf, STRMAX);
    buf[n] = '\0';
    set_slot_value(item, s_text, cvstring(buf));
  }
  return(slot_value(item, s_text));
}

static void InstallScrollItem(HWND theDialog, LVAL item)
{
  HANDLE hd;
  DialogItemData *data;
  DialogData *dialogData;
  HWND theItem;
  int low, high, value;
  int itemIndex;
  Point loc, size;
  LVAL temp;
  DWORD flags;
  
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  flags = (size.h > size.v) ? SBS_HORZ : SBS_VERT;

  temp = slot_value(item, s_min_value);
  low = fixp(temp) ? (int) getfixnum(temp) : SCROLL_MIN;
  temp = slot_value(item, s_max_value);
  high = fixp(temp) ? (int) getfixnum(temp) : SCROLL_MAX;
  temp = slot_value(item, s_value);
  value = (fixp(temp)) ? (int) getfixnum(temp) : low;

  hd = GETDIALOGDATA(theDialog);
  dialogData = (DialogData *) GlobalLock(hd);
  check_lock(dialogData);

  itemIndex = (dialogData->count)++;
  theItem = CreateWindow("scrollbar", NULL,
			 WS_CHILD | WS_VISIBLE | WS_TABSTOP | flags,
			 loc.h, loc.v, size.h, size.v,
			 (HWND) theDialog,
                         (HMENU) (itemIndex + ITEM_INDEX_BASE),
			 hInst, NULL);
  check_alloc(theItem);
  SetScrollRange(theItem, SB_CTL, low, high, FALSE);
  SetScrollPos(theItem, SB_CTL, value, TRUE);

  data = GETITEMDATA(dialogData);
  data[itemIndex].type = SCROLL_ITEM;
  data[itemIndex].itemNumber = itemIndex;
  data[itemIndex].itemHandle = theItem;
  data[itemIndex].object = item;
  GlobalUnlock(hd);
#ifdef WIN32
  SetWindowLong(theItem, GWL_WNDPROC, (LPARAM)CustomScrollProc);
#endif /* WIN32 */
}

void DialogScrollGetDefaultSize(LVAL item, int *pwidth, int *pheight)
{
  int h, w;

  h = GetSystemMetrics(SM_CYHSCROLL);
  w = SCROLL_WIDTH; // ### 10 * h
  if (pwidth != NULL) *pwidth = w;
  if (pheight != NULL) *pheight = h;
}

static LVAL scroll_item_value(LVAL item, int set, int value, int which)
{
  LVAL slot, temp;
  DialogItemData di;
  int low, high;

  switch (which) {
  case 'V': slot = s_value;     break;
  case 'H': slot = s_max_value; break;
  case 'L': slot = s_min_value; break;
  }

  if (set) {
    if (FindItemObjectData(item, &di)) {
      switch (which) {
      case 'V':
	SetScrollPos(di.itemHandle, SB_CTL, value, TRUE);
	break;
      case 'H':
	temp = slot_value(item, s_min_value);
	low = fixp(temp) ? (int) getfixnum(temp) : SCROLL_MIN;
	SetScrollRange(di.itemHandle, SB_CTL, low, value, TRUE);
	break;
      case 'L':
	temp = slot_value(item, s_max_value);
	high = fixp(temp) ? (int) getfixnum(temp) : SCROLL_MAX;
	SetScrollRange(di.itemHandle, SB_CTL, value, high, TRUE);
	break;
      }
    }
    set_slot_value(item, slot, cvfixnum((FIXTYPE) value));
    temp = slot_value(item, s_min_value);
    low = fixp(temp) ? (int) getfixnum(temp) : SCROLL_MIN;
    temp = slot_value(item, s_max_value);
    high = fixp(temp) ? (int) getfixnum(temp) : SCROLL_MAX;
    temp = slot_value(item, s_value);
    value = (fixp(temp)) ? (int) getfixnum(temp) : low;
    if (value < low || value > high)
      set_slot_value(item, s_value, cvfixnum((FIXTYPE) low));
  }
  return(slot_value(item, slot));
}

LVAL DialogScrollItemValue(LVAL item, int set, int value)
{
  return(scroll_item_value(item, set, value, 'V'));
}

LVAL DialogScrollItemMax(LVAL item, int set, int value)
{
  return(scroll_item_value(item, set, value, 'H'));
}

LVAL DialogScrollItemMin(LVAL item, int set, int value)
{
  return(scroll_item_value(item, set, value, 'L'));
}

static Point ListItemDims(LVAL item)
{
  LVAL listData = slot_value(item, s_list_data);
  Point sz;

  if (seqp(listData)) {
    sz.v = (short) seqlen(listData);
    sz.h = 1;
  }
  else if (matrixp(listData)) {
    sz.v = numrows(listData);
    sz.h = numcols(listData);
  }
  else xlerror("this form of data is not yet supported", listData);
  return(sz);
}

static void InstallListItem(HWND theDialog, LVAL item)
{
  HANDLE hd;
  DialogItemData *data;
  DialogData *dialogData;
  HWND theItem;
  int itemIndex, columns, n, m, i, j, k;
  Point loc, size, csz, lsz;
  LVAL listData, next, temp;
  BOOL vscroll, hscroll;
  DWORD flags;
  char *s;

  csz = AvCharSize(LIST_USE_LEADING);
  csz.v += REAL_LIST_ITEM_PAD;
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));

  xlsave1(listData);
  listData = slot_value(item, s_list_data);
  lsz = ListItemDims(item);
  n = lsz.v;
  m = lsz.h;
  temp = slot_value(item, s_columns);
  if (! fixp(temp) || getfixnum(temp) < 1) columns = 1;
  else columns = (int) getfixnum(temp);

  hscroll = (columns < m) ? TRUE : FALSE;
  vscroll = (n * csz.v > size.v - ((hscroll) ? GetSystemMetrics(SM_CYHSCROLL) : 0)) ? TRUE : FALSE;
  flags = WS_CHILD | WS_VISIBLE | WS_BORDER | LBS_NOTIFY | LBS_NOINTEGRALHEIGHT;
  if (hscroll) flags |= WS_HSCROLL;
  if (vscroll) flags |= WS_VSCROLL;
  if (m > 1) flags |= LBS_MULTICOLUMN;

  hd = GETDIALOGDATA(theDialog);
  dialogData = (DialogData *) GlobalLock(hd);
  check_lock(dialogData);

  itemIndex = (dialogData->count)++;

  theItem = CreateWindow("listbox", NULL, flags,
			 loc.h, loc.v, size.h, size.v,
			 (HWND) theDialog,
                         (HMENU) (itemIndex + ITEM_INDEX_BASE),
                         hInst, NULL);
  check_alloc(theItem);
  SendMessage(theItem,WM_SETFONT,(WPARAM) GetStockObject(DIALOG_FONT),0);

  ListBox_SetColumnWidth(theItem, LIST_COL_CHARS * csz.h);
  if (darrayp(listData))
    listData = getdarraydata(listData);
  else if (listp(listData)) listData = coerce_to_tvec(listData, s_true);
  SetWindowRedraw(theItem, FALSE);
  for (j = 0; j < m; j++) {
    for (i = 0, k = j; i < n; i++, k += m) {
      next = gettvecelement(listData, k);
      s = (stringp(next)) ? (char *) getstring(next) : "";
      strcpy(buf, s);
      truncateListEntry(buf);
      if (ListBox_AddString(theItem, buf) < 0)
	xlfail("list allocation failed");
    }
  }
  SetWindowRedraw(theItem, TRUE);

  data = GETITEMDATA(dialogData);
  data[itemIndex].type = LIST_ITEM;
  data[itemIndex].itemNumber = itemIndex;
  data[itemIndex].itemHandle = theItem;
  data[itemIndex].object = item;
  GlobalUnlock(hd);
  xlpop();
#ifdef WIN32
  SetWindowLong(theItem, GWL_WNDPROC, (LPARAM)CustomListProc);
#endif /* WIN32 */
}

void DialogListGetDefaultSize(LVAL item, int *pwidth, int *pheight)
{
  LVAL columns = slot_value(item, s_columns);
  LVAL data = slot_value(item, s_list_data);
  Point csz, sz;

  csz = AvCharSize(LIST_USE_LEADING);
  sz.h = LIST_COL_CHARS * (short) getfixnum(columns);
  if (seqp(data))
    sz.v = (short) seqlen(data);
  else if (matrixp(data))
    sz.v = (int) numrows(data);

  csz.v += REAL_LIST_ITEM_PAD;
  sz.h *= csz.h;
  sz.v *= csz.v;
  if (sz.v > csz.v * MAX_LIST_ROWS) {
    sz.v = csz.v * MAX_LIST_ROWS;
    sz.h += GetSystemMetrics(SM_CXVSCROLL);
  }
  if (matrixp(data) && numrows(data) > getfixnum(columns))
    sz.v += GetSystemMetrics(SM_CYHSCROLL);
  if (pwidth != NULL) *pwidth = sz.h;
  if (pheight != NULL) *pheight = sz.v;
}

LVAL DialogListItemSelection(LVAL item, int set, LVAL index)
{
  LVAL result;
  DialogItemData di;
  Point lsz, cell;
  int i;
  BOOL twodim;

  lsz = ListItemDims(item);

  if (FindItemObjectData(item, &di)) {
    if (set) {
      if (index == NIL) i = -1;
      else if (fixp(index)) i = (int) getfixnum(index);
      else if (consp(index)) {
	cell = ListToPoint(index);
	i = cell.h + cell.v * lsz.h;
      }
      else xlbadtype(index);
      if (i < 0 || i >= lsz.h * lsz.v) i = -1;
      (void) ListBox_SetCurSel(di.itemHandle, i);
    }
    twodim = matrixp(slot_value(item, s_list_data)) ? TRUE : FALSE;
    i = (int) ListBox_GetCurSel(di.itemHandle);
    if (i == LB_ERR || lsz.h <= 0) result = NIL;
    else if (twodim) {
      cell.h = i % lsz.h;
      cell.v = i / lsz.h;
      result = integer_list_2(cell.h, cell.v);
    }
    else result = cvfixnum((FIXTYPE) i);
  }
  else result = NIL;

  return(result);
}

void DialogListItemSetText(LVAL item, LVAL index, char *text)
{
  DialogItemData di;
  Point lsz, cell;
  int i, j;

  lsz = ListItemDims(item);
  if (FindItemObjectData(item, &di)) {
    if (fixp(index)) i = (int) getfixnum(index);
    else if (consp(index)) {
      cell = ListToPoint(index);
      i = cell.h + cell.v * lsz.h;
    }
    else xlbadtype(index);
    if (i >= 0 && i < lsz.h * lsz.v) {
      j = ListBox_GetCurSel(di.itemHandle);
      (void) ListBox_DeleteString(di.itemHandle, i);
      (void) ListBox_InsertString(di.itemHandle, i, text);
      (void) ListBox_SetCurSel(di.itemHandle, j);
    }
  }
}

LVAL DialogGetModalItem(LVAL dialog)
{
  HWND theDialog;
  LVAL item, oldModalItem;
  BOOL oldInModalDialog;
  MSG msg;

  theDialog = GETDIALOGADDRESS(dialog);
  if (theDialog == NULL) xlfail("the dialog is not visible");

  oldModalItem = ModalItem;
  oldInModalDialog = InModalDialog;
  ModalItem = NIL;
  InModalDialog = TRUE;

  while (ModalItem == NIL && GetMessage(&msg, /*theDialog*/NULL, 0, 0)) {
    if(! TranslateMDISysAccel(hWndClient, &msg) &&
       ! TranslateAccelerator(hWndFrame, hAccel, &msg)) {
      TTYFlushOutput();
      TranslateMessage(&msg);
      DispatchMessage(&msg);
      SetActiveWindow(theDialog);
    }
  }

  item = ModalItem;
  ModalItem = oldModalItem;
  InModalDialog = oldInModalDialog;

  if (item == NIL) item = slot_value(dialog, s_default_button);
  return(item);
}

void DialogSetDefaultButton(LVAL dialog, LVAL item)
{
  HANDLE hd;
  DialogData *dd;
  DialogItemData di;
  HWND theDialog;

  if (item != NIL && ! button_item_p(item))
    xlerror("not a button item", item);

  set_slot_value(dialog, s_default_button, item);
    
  theDialog = GETDIALOGADDRESS(dialog);
  if (theDialog != NULL) {
    hd = GETDIALOGDATA(theDialog);
    dd = (DialogData *) GlobalLock(hd);
    check_lock(dd);

    if (FindItemData(theDialog, ITEM_INDEX_BASE + dd->dflt, &di)) {
      dd->dflt = -1;
      Button_SetStyle(di.itemHandle, BS_PUSHBUTTON, TRUE);
    }
    if (item != NIL && FindItemObjectData(item, &di)) {
      dd->dflt = di.itemNumber;
      Button_SetStyle(di.itemHandle, BS_DEFPUSHBUTTON, TRUE);
      SetFocus(di.itemHandle);
    }
    GlobalUnlock(hd);
  }
}

#ifdef WIN32
static void DialogDoDefaultButton(LVAL dialog)
{
  HANDLE hd;
  DialogData *dd;
  DialogItemData di;
  HWND theDialog;
  LVAL item;

  item = slot_value(dialog, s_default_button);
    
  if (item != NIL && ! button_item_p(item))
    xlerror("not a button item", item);

  theDialog = GETDIALOGADDRESS(dialog);
  if (theDialog != NULL) {
    hd = GETDIALOGDATA(theDialog);
    dd = (DialogData *) GlobalLock(hd);
    check_lock(dd);

    if (FindItemData(theDialog, ITEM_INDEX_BASE + dd->dflt, &di))
      dd->dflt = -1;
    if (item != NIL && FindItemObjectData(item, &di)) {
      HWND hwndDefault = di.itemHandle;
      if (IsWindow(hwndDefault))
	PostMessage(hwndDefault, BM_CLICK, 0, 0);
    }
    GlobalUnlock(hd);
  }
}
#endif /* WIN32 */

#ifdef DODO
Add keyboard support:
subclass all items to handle tabs
handle return as hitting default item
#endif DODO
