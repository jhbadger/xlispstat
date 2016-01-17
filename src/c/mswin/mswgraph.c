#include "xlisp.h"
#include "xlstat.h"
#include "wxlisp.h"
#include "ledit.h"
#include "winutils.h"

/* external variables */
extern HWND hWndFrame, hWndClient;
extern HANDLE hInst, hAccel;
extern LVAL s_menu, sk_allocate, sk_close, sk_install, sk_remove;
extern LVAL s_hardware_objects;
extern int Exiting;
extern int xldebug;
extern int xlisp_mono;

/* global variables */
static char szIVWinClass[]  = "MdiIVWinChild";
static char graphicsSection[] = "Graphics";
extern char *iniFile;
static HCURSOR hSaveCursor;
static HCURSOR hGCCursor, hArrowCursor, hWaitCursor, hCrossCursor,
  hHandCursor, hBrushCursor, hFingerCursor;
static BOOL MouseCaptured = FALSE, MouseMotionOnly;
static HWND MouseWnd = 0;
static void (*MouseAction)(HWND, int, int);
static TEXTMETRIC tm;
static HFONT hGraphFont, hGraphFontUp;

/* buffer variables */
static HBITMAP monoBuffBits = 0, colorBuffBits = 0;
static HDC currentDC = 0, bufferDC = 0;
static int buffering = FALSE;
static int bufflevel = 0;

void StGWCopyToClip(StGWWinInfo *);
long CALLBACK IVWinProc(HWND hWnd, UINT message, WPARAM wParam, LONG lParam);

static HCURSOR get_cursor(unsigned int index);
static void init_msw_cursors(void);
static void init_msw_colors(void);
static unsigned long get_color(unsigned int index);
void init_msw_buffer(void);
static void init_msw_text(void);
static void cleanup_msw_text(void);
static void init_msw_symbols(void);
static void cleanup_msw_symbols(void);
static void StGWPrint(StGWWinInfo *gwinfo);

/**************************************************************************/
/**                                                                      **/
/**                       Internal Data Functions                        **/
/**                                                                      **/
/**************************************************************************/

int StGWWinInfoSize(void) { return(sizeof(StGWWinInfo)); }

void StGWInitWinInfo(LVAL object)
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) StGWObWinInfo(object);

  gwinfo->Object = object;
  gwinfo->initialized = FALSE;
  gwinfo->mouse_x = 0;
  gwinfo->mouse_y = 0;
  gwinfo->canvasWidth = 0;
  gwinfo->canvasHeight = 0;
  gwinfo->hasHscroll = FALSE;
  gwinfo->hasVscroll = FALSE;
  gwinfo->view_h = 0;
  gwinfo->view_v = 0;
  gwinfo->h_scroll_inc[0] = 1; gwinfo->h_scroll_inc[1] = 50;
  gwinfo->v_scroll_inc[0] = 1; gwinfo->v_scroll_inc[1] = 50;
  gwinfo->lineType = 0;
  gwinfo->drawMode = 0;
  gwinfo->backColor = 0;
  gwinfo->drawColor = 1;
  gwinfo->lineWidth = 1;
  gwinfo->window = NULL;
  gwinfo->idleOn = FALSE;
  gwinfo->use_color = FALSE;
  gwinfo->cursor = 0;
  gwinfo->RefCon = 0L;
  gwinfo->drawPen.lopnStyle = PS_SOLID;
  gwinfo->drawPen.lopnWidth.x = 1;
  gwinfo->drawPen.lopnColor = 0;
  gwinfo->rect_offset = 0;
}

void StGWSetRefCon(StGWWinInfo *gwinfo, long x)
{
  if (gwinfo) gwinfo->RefCon = x;
}

long StGWGetRefCon(StGWWinInfo *gwinfo)
{
  return(gwinfo ? gwinfo->RefCon : 0);
}

void StGWSetObject(StGWWinInfo *gwinfo, LVAL x)
{
  if (gwinfo) gwinfo->Object = x;
}

LVAL IViewWindowGetObject(HWND w)
{
  StGWWinInfo *gwinfo = GETGWINFO(w);
  return(gwinfo ? gwinfo->Object : NIL);
}

LVAL StGWGetObject(StGWWinInfo *gwinfo)
{
  return(gwinfo ? gwinfo->Object : NIL);
}

static void SetHardwareState(StGWWinInfo *gwinfo)
{
  HDC hDC;
  HWND w;

  if (gwinfo && (w = gwinfo->window) != 0) {
    hDC = GetDC((HWND) w);
    SetBkMode(hDC, TRANSPARENT);
    SetBkColor(hDC, get_color(gwinfo->backColor));
    SetTextColor(hDC, get_color(gwinfo->drawColor));
    SelectObject(hDC, hGraphFont);
    SetROP2(hDC, gwinfo->drawMode ? R2_NOT : R2_COPYPEN);
    SetPolyFillMode(hDC, WINDING);
    ReleaseDC((HWND) w, hDC);
  }
}

/**************************************************************************/
/**                                                                      **/
/**                       Initialization Functions                       **/
/**                                                                      **/
/**************************************************************************/

BOOL InitApplGraphics(HANDLE hInstance)
{
  WNDCLASS wc;

  /* class structure for the IVIEW window */
  wc.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
  wc.lpfnWndProc = IVWinProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof(LVAL) + sizeof(char *);
  wc.hInstance = hInstance;
  wc.hIcon = LoadIcon(hInstance, "GraphIcon");
  wc.hCursor = (HCURSOR) NULL;
  wc.hbrBackground = GetStockObject(WHITE_BRUSH);
  wc.lpszMenuName = NULL;
  wc. lpszClassName = szIVWinClass;
  if (! RegisterClass(&wc)) return(FALSE);

  if (! InitApplResizeBrush(hInstance)) return(FALSE);
  return(TRUE);
}

BOOL InitInstGraphics(HANDLE hInstance)
{
  hGCCursor = LoadCursor(hInstance, "GCCursor");
  hHandCursor = LoadCursor(hInstance, "HandCursor");
  hFingerCursor = LoadCursor(hInstance, "FingerCursor");
  hBrushCursor = LoadCursor(hInstance, "BrushCursor");
  hArrowCursor = LoadCursor((HWND) NULL, IDC_ARROW);
  hWaitCursor = LoadCursor((HWND) NULL, IDC_WAIT);
  hCrossCursor = LoadCursor((HWND) NULL, IDC_CROSS);
  return(TRUE);
}

void StInitGraphics(void)
{
  init_msw_buffer();
  init_msw_symbols();
  init_msw_colors();
  init_msw_cursors();
  init_msw_text();
}

void MSWGraphCleanup(void)
{
  if (bufferDC) {
    DeleteDC(bufferDC);
    bufferDC = 0;
  }
  if (colorBuffBits) {
    DeleteObject(colorBuffBits);
    colorBuffBits = 0;
  }
  if (monoBuffBits) {
    DeleteObject(monoBuffBits);
    monoBuffBits = 0;
  }
  cleanup_msw_symbols();
  cleanup_msw_text();
}

/**************************************************************************/
/**                                                                      **/
/**         Window Creation, Destruction and Callback Functions          **/
/**                                                                      **/
/**************************************************************************/

HWND IViewWindowNew(LVAL object, int is_GW)
{
  char *title;
  int left, top, width, height, goAway;
  StGWWinInfo *gwinfo;
  HWND wind;
  MDICREATESTRUCT mdicreate;

  StGWGetAllocInfo(object, &title, &left, &top, &width, &height, &goAway);
  if (title == NULL || strlen(title) <= 0) title = "Graph Window";
  width += 2 * GetSystemMetrics(SM_CXFRAME);
  height += 2 * GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CYCAPTION);

  mdicreate.szClass = szIVWinClass;
  mdicreate.szTitle = title;
  mdicreate.hOwner = hInst;
  mdicreate.x = left;
  mdicreate.y = top;
  mdicreate.cx = width;
  mdicreate.cy = height;
  mdicreate.style = WS_MINIMIZE | WS_HSCROLL | WS_VSCROLL;
  mdicreate.lParam = (LONG) object;
  wind = MDICreateWindow(hWndClient, &mdicreate);
  if (! wind) xlfail("allocation Failed");

  gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
  gwinfo->window = wind;
  SetScrollRange(wind, SB_VERT, 0, 0, TRUE);
  SetScrollRange(wind, SB_HORZ, 0, 0, TRUE);
  if (! gwinfo->hasHscroll) gwinfo->canvasWidth = width;
  if (! gwinfo->hasVscroll) gwinfo->canvasHeight = height;
  gwinfo->initialized = FALSE;
  SETGWINFO(wind, gwinfo);
  SETWINOBJECT(wind, object);
  SetHardwareState(gwinfo);
  StGWSetClipRect(gwinfo, FALSE, 0, 0, 0, 0);

  if (is_GW) set_iview_window_address(wind, object);
  else set_iview_address(wind, object);

  return(wind);
}

void StGWRemove(StGWWinInfo *gwinfo)
{
  HWND w;
  
  if (gwinfo == NULL || (w = gwinfo->window) == NULL) return;
  else MDIDestroyWindow(hWndClient, w);
}

void ReleaseMouse(void)
{
  if (MouseCaptured) {
    ReleaseCapture();
    MouseCaptured = FALSE;
    MouseAction = NULL;
  }
  MouseWnd = 0; // indicates mouse is up
}

void CaptureMouse(HWND hWnd)
{
  ReleaseMouse();
  SetCapture(hWnd);
  MouseCaptured = TRUE;
  MouseWnd = hWnd;
}

long CALLBACK IVWinProc(HWND hWnd, UINT message, WPARAM wParam, LONG lParam)
{
  StGWWinInfo *gwinfo;
  static int activating = FALSE;

  switch (message) {
  case WM_CREATE:
    ReleaseMouse();
    return(0);
  case WM_COMMAND:
    switch (GET_WM_COMMAND_ID(wParam, lParam)) {
    case IDC_SHOWWINDOW:
      if (IsIconic(hWnd))
	MDIRestoreWindow(hWndClient, hWnd);
      MDIActivateWindow(hWndClient, hWnd);
      return(0);
    case IDC_HIDEWINDOW:
      if (! IsIconic(hWnd))
	ShowWindow(hWnd, SW_MINIMIZE);
      return(0);
    case IDM_COPY:
      gwinfo = GETGWINFO(hWnd);
      StGWCopyToClip(gwinfo);
      return(0);
    case IDM_PRINT:
      gwinfo  = GETGWINFO(hWnd);
      StGWPrint(gwinfo);
    break;
      break;
    }
    break;
  case WM_SIZE:
    if (! IsIconic(hWnd)) {
      gwinfo = GETGWINFO(hWnd);
      if (gwinfo->hasHscroll) {
	SetScrollRange(hWnd, SB_HORZ, 0, gwinfo->canvasWidth - LOWORD(lParam), TRUE);
      }
      else gwinfo->canvasWidth = LOWORD(lParam);
      if (gwinfo->hasVscroll) {
	SetScrollRange(hWnd, SB_VERT, 0, gwinfo->canvasHeight - HIWORD(lParam), TRUE);
      }
      else gwinfo->canvasHeight = HIWORD(lParam);
      if (gwinfo->hasVscroll || gwinfo->hasHscroll)
	StGWSetScroll(gwinfo, gwinfo->view_h, gwinfo->view_v, TRUE);
      StGWObResize(GETWINOBJECT(hWnd));
    }
    // ### fix slot value
    break;
  case WM_MOVE:
    // ### fix slot value
    break;
  case WM_VSCROLL:
  case WM_HSCROLL:
    {
      int val, hval, vval, *incs;

      gwinfo = GETGWINFO(hWnd);
      val = (message == WM_HSCROLL) ? gwinfo->view_h : gwinfo->view_v;
      incs = (message == WM_HSCROLL) ? gwinfo->h_scroll_inc : gwinfo->v_scroll_inc;
      switch (GET_WM_VSCROLL_CODE(wParam,lParam)) {
      case SB_LINEUP:        val -= incs[0];       break;
      case SB_LINEDOWN:      val += incs[0];       break;
      case SB_PAGEUP:        val -= incs[1];       break;
      case SB_PAGEDOWN:      val += incs[1];       break;
      case SB_TOP:
	{
	  int nMin,nMax;
	  int scroll = (message == WM_HSCROLL) ? SB_HORZ : SB_VERT;
	  GetScrollRange(hWnd, scroll, &nMin, &nMax);
	  val = nMin;
	  break;
	}
      case SB_BOTTOM:
	{
	  int nMin,nMax;
	  int scroll = (message == WM_HSCROLL) ? SB_HORZ : SB_VERT;
	  GetScrollRange(hWnd, scroll, &nMin, &nMax);
	  val = nMax;
	  break;
	}
      case SB_THUMBPOSITION: val = GET_WM_VSCROLL_POS(wParam,lParam); break;
      }
      hval = (message == WM_HSCROLL) ? val: gwinfo->view_h;
      vval = (message == WM_HSCROLL) ? gwinfo->view_v : val;
      StGWSetScroll(gwinfo, hval, vval, TRUE);
    }
    return(0);
  case WM_CHAR:
    StGWObDoKey(GETWINOBJECT(hWnd),
		(unsigned char) wParam,
		GetKeyState(VK_SHIFT),
		GetKeyState(VK_CONTROL));
    return(0);
#ifdef DODO
    /**** These don't make any sense.  Something like sending a symbol
      or a logical key code to the application is needed */
    case WM_KEYDOWN:
     switch (wParam) {
     case VK_UP:
     case VK_DOWN:
     case VK_LEFT:
     case VK_RIGHT:
     case VK_END:
     case VK_HOME:
     case VK_PRIOR:
     case VK_NEXT:
       StGWObDoKey(GETWINOBJECT(hWnd),
		  (unsigned char) wParam,
		   GetKeyState(VK_SHIFT),
		   GetKeyState(VK_CONTROL));
       break;
     }
     return(0);
#endif
  case WM_MOUSEMOVE:
    gwinfo = GETGWINFO(hWnd);
    SetCursor(get_cursor(gwinfo->cursor));
    if (! MouseCaptured) {
      gwinfo->mouse_x = LOWORD(lParam);
      gwinfo->mouse_y = HIWORD(lParam);
      StGWObDoMouse(GETWINOBJECT(hWnd),
		    gwinfo->mouse_x + gwinfo->view_h,
		    gwinfo->mouse_y + gwinfo->view_v,
		    MouseMove,
		    (MouseClickModifier) 0);
    }
    else if (MouseWnd == hWnd && xldebug <= 0) { // ### move to WhileButtonDown?
      if (gwinfo->mouse_x != LOWORD(lParam)
	  || gwinfo->mouse_y != HIWORD(lParam)
	  || ! MouseMotionOnly) {
	gwinfo->mouse_x = LOWORD(lParam);
	gwinfo->mouse_y = HIWORD(lParam);
	if (MouseAction != NULL)
	  (*MouseAction)(hWnd,
			 gwinfo->mouse_x + gwinfo->view_h,
			 gwinfo->mouse_y + gwinfo->view_v);
      }
    }
    return(0);
  case WM_LBUTTONDOWN:
  case WM_MBUTTONDOWN:
  case WM_RBUTTONDOWN:
    {
      int mods = 2 * ((MK_CONTROL & wParam) ? 1 : 0) + ((MK_SHIFT & wParam) ? 1 : 0);
      gwinfo = GETGWINFO(hWnd);
      gwinfo->mouse_x = LOWORD(lParam);
      gwinfo->mouse_y = HIWORD(lParam);
      ReleaseMouse();
      MouseWnd = hWnd; // to indicate button is down
      StGWObDoMouse(GETWINOBJECT(hWnd),
		    gwinfo->mouse_x + gwinfo->view_h,
		    gwinfo->mouse_y + gwinfo->view_v,
		    MouseClick,
		    (MouseClickModifier) mods);

    }
    return(0);
  case WM_LBUTTONUP:
  case WM_MBUTTONUP:
  case WM_RBUTTONUP:
    ReleaseMouse();
    return(0);
  case WM_PAINT:
    {
      PAINTSTRUCT ps;
      int left, top, width, height;

      gwinfo = GETGWINFO(hWnd);
      BeginPaint(hWnd, &ps);
      EndPaint(hWnd, &ps);
      if (! gwinfo->initialized) {
	gwinfo->initialized = TRUE;
	SetHardwareState(gwinfo);
      }
      StGWGetViewRect(gwinfo, &left, &top, &width, &height);
      StGWStartBuffering(gwinfo);
      StGWObRedraw(GETWINOBJECT(hWnd));
      StGWBufferToScreen(gwinfo, left, top, width, height);
      return(0);
    }
  case WM_MDIACTIVATE:
    {
      LVAL menu = slot_value(GETWINOBJECT(hWnd), s_menu);
      activating = GET_WM_MDIACTIVATE_FACTIVATE(hWnd, wParam, lParam);
      if (menu != NIL) {
	if (activating) send_callback_message(menu, sk_install);
	else send_callback_message(menu, sk_remove);
      }
    }
    return(0);
  case WM_NCACTIVATE:
    ReleaseMouse();
    return(DefMDIChildProc(hWnd, message, wParam, lParam));
  case WM_MOUSEACTIVATE:
    if (activating && HTCLIENT == LOWORD(lParam))
      return MA_ACTIVATEANDEAT;
    else
      return MA_ACTIVATE;
  case WM_SETCURSOR:
    activating = FALSE;
    break;
  case WM_CLOSE:
    send_callback_message(GETWINOBJECT(hWnd), sk_close);
    return(0);
  case WM_DESTROY:
    if (! Exiting) {
      gwinfo = GETGWINFO(hWnd);
      if (gwinfo) {
	if (IViewInternalIsLinked(hWnd))
	  IViewUnlinkWindow(hWnd);
	StGWObDoClobber(gwinfo->Object);
	if (gwinfo->FreeMem)
	  (*gwinfo->FreeMem)(hWnd);
	gwinfo->window = NULL;
      }
    }
    return(0);
  }
  return(DefMDIChildProc(hWnd, message, wParam, lParam));
}

void StGWWhileButtonDown(StGWWinInfo *gwinfo, void (*action)(), int motionOnly)
{
  MSG msg;

  if (! gwinfo || ! gwinfo->window) return;
  if (! MouseWnd) return; // checks if mouse is down
  MouseAction = action;
  MouseMotionOnly = motionOnly;
  CaptureMouse((HWND) gwinfo->window);
  while (MouseCaptured) {
    if (PeekMessage(&msg, (HWND) gwinfo->window, 0, 0, PM_REMOVE)) {
      if(! TranslateMDISysAccel(hWndClient, &msg)
	 && ! TranslateAccelerator(hWndFrame, hAccel, &msg)) {
	TTYFlushOutput();
	TranslateMessage(&msg);
	DispatchMessage(&msg);
      }
    }
    else if (! MouseMotionOnly && MouseAction && MouseCaptured) {
      (*MouseAction)(gwinfo->window,
		     gwinfo->mouse_x + gwinfo->view_h,
		     gwinfo->mouse_y + gwinfo->view_v);
      TTYFlushOutput();
    }
  }
  ReleaseMouse(); // should have heppened already
}

void MSWResetGraphics(void)
{
  ReleaseMouse();
  StGWResetBuffer();
}

/**************************************************************************/
/**                                                                      **/
/**                        Idle Action Functions                         **/
/**                                                                      **/
/**************************************************************************/

int MSWAnyIdleActions(void)
{
  LVAL next, object;
  StGWWinInfo *gwinfo;

  for (next = getvalue(s_hardware_objects); consp(next); next = cdr(next)) {
    if (valid_iview_window_address(car(next))) {
      object = car(cdr(cdr(car(next))));
      gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
      if (gwinfo && gwinfo->window
	  && gwinfo->idleOn && ! IsIconic((HWND) gwinfo->window)) {
	return TRUE;
      }
    }
  }
  return FALSE;
}

void MSWDoIdleActions(void)
{
  LVAL next, object;
  StGWWinInfo *gwinfo;

  for (next = getvalue(s_hardware_objects); consp(next); next = cdr(next)) {
    if (valid_iview_window_address(car(next))) {
      object = car(cdr(cdr(car(next))));
      gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
      if (gwinfo && gwinfo->window
	  && gwinfo->idleOn && ! IsIconic((HWND) gwinfo->window)) {
	StGWObDoIdle(object);
      }
    }
  }
}

int StGWIdleOn(StGWWinInfo *gwinfo)
{
  return(gwinfo ? gwinfo->idleOn : FALSE);
}

void StGWSetIdleOn(StGWWinInfo *gwinfo, int on)
{
  if (gwinfo) gwinfo->idleOn = on;
}

/**************************************************************************/
/**                                                                      **/
/**                   Scrolling and Canvas Functions                     **/
/**                                                                      **/
/**************************************************************************/

//**** not sure this is needed -- it probably duplicates stuff
static void ResetDCClipRect(HDC hDC, StGWWinInfo *gwinfo)
{
  if (gwinfo->clipped) {
    HRGN rgn;
    rgn = CreateRectRgn(gwinfo->clip_rect.left - gwinfo->view_h,
                        gwinfo->clip_rect.top - gwinfo->view_v,
		        gwinfo->clip_rect.right - gwinfo->view_h,
		        gwinfo->clip_rect.bottom) - gwinfo->view_v;
    SelectClipRgn(hDC, rgn);
    DeleteObject(rgn);
  }
}

int StGWCanvasWidth(StGWWinInfo *gwinfo) { return (gwinfo ? gwinfo->canvasWidth : 0); }
int StGWCanvasHeight(StGWWinInfo *gwinfo) { return (gwinfo ? gwinfo->canvasHeight : 0); }
int StGWHasVscroll(StGWWinInfo *gwinfo) { return (gwinfo ? gwinfo->hasVscroll : FALSE); }
int StGWHasHscroll(StGWWinInfo *gwinfo) { return (gwinfo ? gwinfo->hasHscroll : FALSE); }

void StGWSetHasVscroll(StGWWinInfo *gwinfo, int has, int size)
{
  RECT r;
  HWND w;
  int height;

  if (gwinfo && (w = gwinfo->window) != 0) {
    GetClientRect((HWND) w, &r);
    height = r.bottom - r.top;
    gwinfo->hasVscroll = has;
    gwinfo->canvasHeight = has ? size : height;
    SetScrollRange((HWND) w, SB_VERT, 0, gwinfo->canvasHeight - height, TRUE);
    StGWSetScroll(gwinfo, gwinfo->view_h, gwinfo->view_v, TRUE);
  }
}

void StGWSetHasHscroll(StGWWinInfo *gwinfo, int has, int size)
{
  RECT r;
  HWND w;
  int width;

  if (gwinfo && (w = gwinfo->window) != 0) {
    GetClientRect((HWND) w, &r);
    width = r.right - r.left;
    gwinfo->hasHscroll = has;
    gwinfo->canvasWidth = has ? size : width;
    SetScrollRange((HWND) w, SB_HORZ, 0, gwinfo->canvasWidth - width, TRUE);
    StGWSetScroll(gwinfo, gwinfo->view_h, gwinfo->view_v, TRUE);
  }
}

void StGWSetScroll(StGWWinInfo *gwinfo, int h, int v, int move)
#pragma argsused gwinfo h v
{
  RECT r;
  HWND w;
  int width, height, hmax, vmax;
  HDC hDC;

  if (gwinfo && (w = gwinfo->window) != 0) {
    GetClientRect((HWND) w, &r);
    width = r.right - r.left;
    height = r.bottom - r.top;
    hmax = gwinfo->canvasWidth - width;
    vmax = gwinfo->canvasHeight - height;
    if (hmax < 0) hmax = 0;
    if (vmax < 0) vmax = 0;
    if (h < 0) h = 0; if (h > hmax) h = hmax;
    if (v < 0) v = 0; if (v > vmax) v = vmax;
    gwinfo->view_h = (gwinfo->hasHscroll) ? h : 0;
    gwinfo->view_v = (gwinfo->hasVscroll) ? v : 0;
    if (gwinfo->view_h != GetScrollPos((HWND) w, SB_HORZ))
      SetScrollPos((HWND) w, SB_HORZ, gwinfo->view_h, TRUE);
    if (gwinfo->view_v != GetScrollPos((HWND) w, SB_VERT))
      SetScrollPos((HWND) w, SB_VERT, gwinfo->view_v, TRUE);
    hDC = GET_DC(w);
    SetViewportOrgEx(hDC, -gwinfo->view_h, -gwinfo->view_v, NULL);
    ResetDCClipRect(hDC, gwinfo);
    RELEASE_DC(w, hDC);
    if (currentDC) {
      SetViewportOrgEx(currentDC, -gwinfo->view_h, -gwinfo->view_v, NULL);
      ResetDCClipRect(currentDC, gwinfo);
    }
    InvalidateRect((HWND) w, &r, FALSE);
  }
}

void StGWGetScroll(StGWWinInfo *gwinfo, int *h, int *v)
{
  if (gwinfo) {
    if (h) *h = gwinfo->view_h;
    if (v) *v = gwinfo->view_v;
  }
}

void StGWSetHscrollIncs(StGWWinInfo *gwinfo, int inc, int pageInc)
{
  if (gwinfo) {
    gwinfo->h_scroll_inc[0] = inc;
    gwinfo->h_scroll_inc[1] = pageInc;
  }
}

void StGWGetHscrollIncs(StGWWinInfo *gwinfo, int *inc, int *pageInc)
{
  if (gwinfo) {
    if (inc) *inc = gwinfo->h_scroll_inc[0];
    if (pageInc) *pageInc = gwinfo->h_scroll_inc[1];
  }
}

void StGWSetVscrollIncs(StGWWinInfo *gwinfo, int inc, int pageInc)
{
  if (gwinfo) {
    gwinfo->v_scroll_inc[0] = inc;
    gwinfo->v_scroll_inc[1] = pageInc;
  }
}

void StGWGetVscrollIncs(StGWWinInfo *gwinfo, int *inc, int *pageInc)
{
  if (gwinfo) {
    if (inc) *inc = gwinfo->v_scroll_inc[0];
    if (pageInc) *pageInc = gwinfo->v_scroll_inc[1];
  }
}

void StGWGetViewRect(StGWWinInfo *gwinfo, int *left, int *top, int *width, int *height)
{
  HWND w;
  RECT r;

  if (gwinfo && (w = gwinfo->window) != 0) {
    GetClientRect((HWND) w, &r);
    if (left) *left = r.left + gwinfo->view_h;
    if (top) *top = r.top + gwinfo->view_v;
    if (width) *width = r.right - r.left;
    if (height) *height = r.bottom - r.top;
  }
  else {
    if (left) *left = 0;
    if (top) *top = 0;
    if (width) *width = 1;
    if (height) *height = 1;
  }
}

/**************************************************************************/
/**                                                                      **/
/**                            Cursor Functions                          **/
/**                                                                      **/
/**************************************************************************/

#define NumBasicCursors 9
static int NumCursors;

typedef struct {
  HCURSOR curs;
  int created;
  long refcon;
} cursor_entry;

static cursor_entry *curstab;

static HCURSOR get_cursor(unsigned int index)
{
  if (index < NumCursors) return(curstab[index].curs);
  else return(hArrowCursor);
}

void set_gc_cursor(int set)
{
  if (set) hSaveCursor = SetCursor(hGCCursor);
  else SetCursor(hSaveCursor);
}

static void init_msw_cursors(void)
{
  NumCursors = NumBasicCursors;
  curstab = (cursor_entry *) StCalloc(NumCursors, sizeof(cursor_entry));
  curstab[ARROW_CURSOR].curs = hArrowCursor;
  curstab[WATCH_CURSOR].curs = hWaitCursor;
  curstab[CROSS_CURSOR].curs = hCrossCursor;
  curstab[BRUSH_CURSOR].curs = hBrushCursor;
  curstab[HAND_CURSOR].curs = hHandCursor;
  curstab[FINGER_CURSOR].curs = hFingerCursor;
  curstab[HOUR_GLASS_CURSOR].curs = hWaitCursor;
  curstab[TRASH_BAG_CURSOR].curs = hGCCursor;
  curstab[TRASH_CAN_CURSOR].curs = hGCCursor;
}

void StGWSetCursRefCon(unsigned int index, long rc)
{
  if (index < NumCursors) curstab[index].refcon = rc;
}

long StGWGetCursRefCon(unsigned int index)
{
  if (index < NumCursors) return(curstab[index].refcon);
  else return(0L);
}

void StGWSetCursor(StGWWinInfo *gwinfo, int cursor)
{
  if (gwinfo) gwinfo->cursor = cursor;
}

int StGWCursor(StGWWinInfo *gwinfo)
{
  return(gwinfo ? gwinfo->cursor : 0);
}

static int get_free_cursor_index(void)
{
  char *temp;
  int index;

  for (index = 0;
       index < NumCursors && curstab[index].curs != NULL;
       index++);
  if (index >= NumCursors) {
    temp = realloc((char *) curstab, (NumCursors + 1) * sizeof(cursor_entry));
    if (temp == NULL) return(-1);
    curstab = (cursor_entry *) temp;
    NumCursors++;
    curstab[index].curs = NULL;
    curstab[index].created = FALSE;
    curstab[index].refcon = 0L;
  }
  return(index);
}

static char *make_bitmap_bits(char *image,
			      int width, int height,
			      int cwidth, int cheight,
			      int mask)
{
  HBITMAP hbm = 0, oldbm;
  BITMAP bm;
  HDC bmDC, hDC;
  char *bits = NULL;
  int i, j, size;

  hDC = GetDC(hWndFrame);
  bmDC = CreateCompatibleDC(hDC);
  ReleaseDC(hWndFrame, hDC);

  if (bmDC) {
    hbm = CreateBitmap(cwidth, cheight, 1, 1, NULL);
    if (hbm) {
      oldbm = SelectObject(bmDC, hbm);
      PatBlt(bmDC, 0, 0, cwidth, cheight, WHITENESS);
      if (image) {
	PatBlt(bmDC, 0, 0, cwidth, cheight, mask ? WHITENESS : BLACKNESS);
	PatBlt(bmDC, 0, 0, width, height, WHITENESS);
	for (i = 0; i < height && i < cheight; i++)
	  for (j = 0; j < width && j < cwidth; j++)
	    if (image[i * width + j])
	      SetPixel(bmDC, j, i, 0);
      }
      else PatBlt(bmDC, 0, 0, width, height, BLACKNESS);

      GetObject(hbm, sizeof(BITMAP), (LPSTR) &bm);
      size = (int) bm.bmWidthBytes * bm.bmHeight * bm.bmPlanes;
      bits = StCalloc(size, 1);
      if (bits) GetBitmapBits(hbm, size, bits);
      SelectObject(bmDC, oldbm);
      DeleteObject(hbm);
    }
    DeleteDC(bmDC);
  }
  return(bits);
}

static void free_bitmap_bits(char *bits)
{
  if (bits) StFree(bits);
}

int StGWMakeCursor(int n, char *image, char *mask, int h, int v, long refcon)
{
  int index, cwidth, cheight, i;
  char *imageBits, *maskBits;
  HCURSOR hc = 0;

  index = get_free_cursor_index();
  if (index < 0) return(-1);

  cwidth = GetSystemMetrics(SM_CXCURSOR);
  cheight = GetSystemMetrics(SM_CYCURSOR);
  if (mask && image)
    for (i = 0; i < n * n; i++)
      if (! mask[i]) image[i] = 1;
  imageBits = make_bitmap_bits(image, n, n, cwidth, cheight, FALSE);
  maskBits = make_bitmap_bits(mask, n, n, cwidth, cheight, TRUE);
  if (imageBits && maskBits)
    hc = CreateCursor(hInst, h, v, cwidth, cheight, maskBits, imageBits);
  free_bitmap_bits(imageBits);
  free_bitmap_bits(maskBits);
  if (! hc) return(-1);

  curstab[index].curs = hc;
  curstab[index].created = TRUE;
  curstab[index].refcon = refcon;
  return(index);
}

int StGWMakeResCursor(char *name, int num, long refcon)
{
  int index;
  char *cname = NULL;
  long resnum = 0;
  LVAL arg;

  if (name) return(-1);
  else if (! num) return(-1);
  else {
    arg = xlgetarg();
    if (stringp(arg)) cname = (char *) getstring(arg);
    else if (fixp(arg)) resnum = getfixnum(arg);
    else return(-1);

    index = get_free_cursor_index();
    if (index < 0) return(-1);

    curstab[index].curs = LoadCursor((HINSTANCE) num, cname ? cname : MAKEINTRESOURCE(resnum));
    if (curstab[index].curs) curstab[index].refcon = refcon;
    else return(-1);
  }
  return(index);
}

void StGWFreeCursor(unsigned int index)
{
  if (index < NumCursors && index >= NumBasicCursors) {
    if (curstab[index].curs != NULL && curstab[index].created)
      DestroyCursor(curstab[index].curs);
    curstab[index].curs = 0;
    curstab[index].refcon = 0;
    curstab[index].created = FALSE;
  }
  else xlfail("can't free standard cursor");
}

/**************************************************************************/
/**                                                                      **/
/**                            Color Functions                           **/
/**                                                                      **/
/**************************************************************************/

#define NumBasicColors 8
#define NumRGBColors 256
# define CMULT 255

static int NumColors;

typedef struct {
  unsigned long rgb;
  long refcon;
} ctab_entry;

static ctab_entry *ctable;

static void init_msw_colors(void)
{
  NumColors = NumBasicColors;
  if (StScreenHasColor()) NumColors += NumRGBColors;
  ctable = (ctab_entry *) StCalloc(NumColors, sizeof(ctab_entry));
  
  ctable[0].rgb = RGB(255, 255, 255); // white
  ctable[1].rgb = RGB(  0,   0,   0); // black
  ctable[2].rgb = RGB(255,   0,   0); // red
  ctable[3].rgb = RGB(  0, 255,   0); // green
  ctable[4].rgb = RGB(  0,   0, 255); // blue
  ctable[5].rgb = RGB(  0, 255, 255); // cyan
  ctable[6].rgb = RGB(255,   0, 255); // magenta
  ctable[7].rgb = RGB(255, 255,   0); // yellow
}

void StGWSetColRefCon(unsigned int index, long rc)
{
  if (index < NumColors) ctable[index].refcon = rc;
}

long StGWGetColRefCon(unsigned int index)
{
  if (index < NumColors) return(ctable[index].refcon);
  else return(0L);
}

int StGWMakeColor(double red, double green, double blue, long refcon)
{
  int index;
  
  for (index = NumBasicColors; 
       index < NumColors && StGWGetColRefCon(index) != 0;
       index++);
  if (index >= NumColors) return(-1);
  else {
    StGWSetColRefCon(index, refcon);
    ctable[index].rgb = RGB(CMULT * red, CMULT * green, CMULT * blue);
    return(index);
  }
}

void StGWFreeColor(unsigned int index)
{
  if (index < NumColors && index >= NumBasicColors)
    StGWSetColRefCon(index, 0);
  else xlfail("can't free standard color");
}

static unsigned long get_color(unsigned int index)
{
  if (index < NumColors) return(ctable[index].rgb);
  else return(ctable[1].rgb);
}

int StGWUseColor(StGWWinInfo *gwinfo)
{
  return(gwinfo ? gwinfo->use_color : FALSE);
}

ColorCode StGWDrawColor(StGWWinInfo *gwinfo)
{
  return ((ColorCode) (gwinfo ? gwinfo->drawColor : 1));
}

ColorCode StGWBackColor(StGWWinInfo *gwinfo)
{
  return ((ColorCode) (gwinfo ? gwinfo->backColor : 0));
}

void StGWSetUseColor(StGWWinInfo *gwinfo, int use)
{
  if (gwinfo) {
    gwinfo->use_color = use;
    if (! colorBuffBits) gwinfo->use_color = FALSE;
  }
}

void StGWSetDrawColor(StGWWinInfo *gwinfo, ColorCode color)
{
  HDC hDC;

  if (gwinfo) {
    if (color < NumColors) gwinfo->drawColor = color;
    gwinfo->drawPen.lopnColor = get_color(color);
    if (gwinfo->window) {
      hDC = GetDC((HWND) gwinfo->window);
      SetTextColor(hDC, get_color(color));
      ReleaseDC((HWND) gwinfo->window, hDC);
      if (currentDC) SetTextColor(currentDC, get_color(color));
    }
  }
}

void StGWSetBackColor(StGWWinInfo *gwinfo, ColorCode color)
{
  HDC hDC;

  if (gwinfo) {
    if (color < NumColors) gwinfo->backColor = color;
    if (gwinfo->window) {
      hDC = GetDC((HWND) gwinfo->window);
      SetBkColor(hDC, get_color(color));
      ReleaseDC((HWND) gwinfo->window, hDC);
      if (currentDC) SetBkColor(currentDC, get_color(color));
    }
  }
}

/**************************************************************************/
/**                                                                      **/
/**                        Drawing State Functions                       **/
/**                                                                      **/
/**************************************************************************/

int StGWDrawMode(StGWWinInfo *gwinfo)
{
  return(gwinfo ? gwinfo->drawMode : 0);
}

int StGWLineType(StGWWinInfo *gwinfo)
{
  return(gwinfo ? gwinfo->lineType : 0);
}

void StGWGetLineWidth(StGWWinInfo *gwinfo, int *width)
{
  if (gwinfo && width) *width = gwinfo->lineWidth;
}

void StGWSetDrawMode(StGWWinInfo *gwinfo, int mode)
{
  HDC hDC;

  if (gwinfo) {
    gwinfo->drawMode = mode;
    if (gwinfo->window) {
      hDC = GetDC((HWND) gwinfo->window);
      SetROP2(hDC, gwinfo->drawMode ? R2_NOT : R2_COPYPEN);
      ReleaseDC((HWND) gwinfo->window, hDC);
      if (currentDC)
	SetROP2(currentDC, gwinfo->drawMode ? R2_NOT : R2_COPYPEN);
    }
  }
}

void StGWSetLineType(StGWWinInfo *gwinfo, int type)
{
  if (gwinfo) {
    gwinfo->lineType = type ? 1 : 0;
    gwinfo->drawPen.lopnStyle = type ? PS_DOT : PS_SOLID;
  }
}

void StGWSetLineWidth(StGWWinInfo *gwinfo, int width)
{
  if (gwinfo) {
    gwinfo->lineWidth = (width > 0) ? width : 1;
    gwinfo->rect_offset = width / 2;
    gwinfo->drawPen.lopnWidth.x = (width > 1) ? width : 1;
  }
}

int StGWGetClipRect(StGWWinInfo *gwinfo,
		    int *pleft, int *ptop, int *pwidth, int *pheight)
{
  if (gwinfo == NULL) return(FALSE);
  if (gwinfo->clipped) {
    if (pleft) *pleft = gwinfo->clip_rect.left;
    if (ptop) *ptop = gwinfo->clip_rect.top;
    if (pwidth) *pwidth = gwinfo->clip_rect.right - gwinfo->clip_rect.left;
    if (pheight) *pheight = gwinfo->clip_rect.bottom - gwinfo->clip_rect.top;
  }
  return(gwinfo->clipped);
}

// ### fix
void StGWSetClipRect(StGWWinInfo *gwinfo, int clipping,
		     int left, int top, int width, int height)
{
  HDC hDC;
  HRGN rgn;
  int right, bottom;

  if (gwinfo) {
    if (clipping) {
      right = left + width;
      bottom = top + height;
    }
    else {
      left = 0;
      top = 0;
      right = gwinfo->canvasWidth;
      right = gwinfo->canvasHeight;
    }
    gwinfo->clipped = clipping;
    gwinfo->clip_rect.left = left;
    gwinfo->clip_rect.top = top;
    gwinfo->clip_rect.right = right;
    gwinfo->clip_rect.bottom = bottom;
    left -= gwinfo->view_h;
    top -= gwinfo->view_v;
    right -= gwinfo->view_h;
    bottom -= gwinfo->view_v;
    hDC = GetDC((HWND) gwinfo->window);
    rgn = CreateRectRgn(left, top, right, bottom); /***** error check */
    SelectClipRgn(hDC, rgn);
    ReleaseDC((HWND) gwinfo->window, hDC);
    if (currentDC) SelectClipRgn(currentDC, rgn);
    DeleteObject(rgn);
  }
}

/**************************************************************************/
/**                                                                      **/
/**                           Buffer Functions                           **/
/**                                                                      **/
/**************************************************************************/

void init_msw_buffer(void)
{
  HDC hDC;
  int width, height;

  StGetScreenSize(&width, &height);
  hDC = GetDC(hWndFrame);
  if (StScreenHasColor() && ! xlisp_mono) {
    colorBuffBits = CreateCompatibleBitmap(hDC, width, height);
    if (! colorBuffBits) {
      WarningBox("Can't allocate color buffer");
    }
  }
  ReleaseDC(hWndFrame, hDC);
  monoBuffBits = CreateBitmap(width, height, 1, 1, NULL);
  if (! monoBuffBits) {
    WarningBox("Can't allocate monochrome buffer");
    xexit();
  }
}

static void init_dc(StGWWinInfo *gwinfo, HDC bufferDC, int buffer)
{
  HBRUSH hbr;
  RECT r;
  SetBkMode(bufferDC, TRANSPARENT);
  SetBkColor(bufferDC, get_color(gwinfo->backColor));
  SetTextColor(bufferDC, get_color(gwinfo->drawColor));
  SelectObject(bufferDC, hGraphFont);
  SetROP2(bufferDC, gwinfo->drawMode ? R2_NOT : R2_COPYPEN);
  SetPolyFillMode(bufferDC, WINDING);
  if (buffer)
    SetViewportOrgEx(bufferDC, -gwinfo->view_h, -gwinfo->view_v, NULL);
  ResetDCClipRect(bufferDC, gwinfo);
  if (buffer) {
    GetClientRect((HWND) gwinfo->window, &r);
    r.left += gwinfo->view_h; r.top += gwinfo->view_v;
    r.right += gwinfo->view_h; r.bottom += gwinfo->view_v;
  }
  else {
    r.left = r.top = 0;
    r.right = gwinfo->canvasWidth;
    r.bottom = gwinfo->canvasHeight;
  }
  hbr = GET_ERASE_BRUSH(gwinfo);
  FillRect(bufferDC, &r, hbr);
  RELEASE_BRUSH(hbr);
}

void StGWStartBuffering(StGWWinInfo *gwinfo)
{
  HDC hDC;

  if (gwinfo && gwinfo->window) {
    if (! buffering) {
      StGWResetBuffer();
      buffering = TRUE;
      hDC = GetDC((HWND) gwinfo->window);
      bufferDC = CreateCompatibleDC(hDC);
      ReleaseDC((HWND) gwinfo->window, hDC);
      SelectObject(bufferDC, (gwinfo->use_color && colorBuffBits) ? colorBuffBits : monoBuffBits);
      init_dc(gwinfo, bufferDC, TRUE);
      currentDC = bufferDC;
    }
    bufflevel++;
  }
}

void StGWBufferToScreen(StGWWinInfo *gwinfo,
			int left, int top, int width, int height)
{
  HDC hDC;

  if (gwinfo && gwinfo->window) {
    if (--bufflevel <= 0 && buffering) {
      hDC = GetDC((HWND) gwinfo->window);
      if (! gwinfo->use_color) {
	SetBkColor(hDC, get_color(0));
	SetTextColor(hDC, get_color(1));
      }
      BitBlt(hDC, left, top, width, height, currentDC, left, top, SRCCOPY);
      if (! gwinfo->use_color) {
	SetBkColor(hDC, get_color(gwinfo->backColor));
	SetTextColor(hDC, get_color(gwinfo->drawColor));
      }
      ReleaseDC((HWND) gwinfo->window, hDC);
      StGWResetBuffer();
    }
  }
}

void StGWResetBuffer(void)
{
  bufflevel = 0;
  buffering = FALSE;
  currentDC = 0;
  if (bufferDC) {
    DeleteDC(bufferDC);
    bufferDC = 0;
  }
}

// ### fix
void StGWDumpImage(StGWWinInfo *gwinfo, FILEP file, double scale)
{
  xlfail("not supported yet");
}

#ifdef WIN32
static int GetPrinterDC(HDC *pdc)
{
  static PRINTDLG pd = { sizeof (PRINTDLG) } ;

  /* Show the Print dialog box and get printer DC */
  pd.Flags = PD_RETURNDC | PD_NOPAGENUMS | PD_NOSELECTION
    | PD_USEDEVMODECOPIESANDCOLLATE;
  if (!PrintDlg (&pd))
    return FALSE;
  else {
    *pdc = pd.hDC;
    return TRUE;
  }
}

static int PrintEMF(HDC printerDC, HENHMETAFILE hemf)
{
  static DOCINFO di = { sizeof (DOCINFO), TEXT ("EmfView: Printing") } ;
  ENHMETAHEADER emh;
  int play_success, print_success, width, height, dwidth, dheight;
  RECT r;
  double hScale, vScale, scale;

  /* get the metafile header */
  GetEnhMetaFileHeader(hemf, sizeof(emh), &emh);
  
  /* Calculate the scale factor */
  dwidth = GetDeviceCaps(printerDC, HORZRES);
  width = emh.rclBounds.right - emh.rclBounds.left; 
  if (width > 0) hScale = (double) dwidth / width;
  else hScale = 1.0;
  dheight = GetDeviceCaps(printerDC, VERTRES);
  height = emh.rclBounds.bottom - emh.rclBounds.top;
  if (height > 0) vScale = (double) dheight/ height;
  else vScale = 1.0;
  scale = hScale > vScale ? vScale : hScale;
  if (scale > 1.0) scale = floor(scale);

  /* Set the display rectangle */
  /***** add margins? */
  width = scale * width;
  height = scale * height;
  r.left = (dwidth - width) / 2;
  r.top = (dheight - height) / 2;
  r.right = r.left + width;
  r.bottom = r.top + height;

  /* Play the EMF to the printer */
  /**** save current cursor and set up wait cursor */
  print_success = FALSE;
  if ((StartDoc(printerDC, &di) > 0) && (StartPage(printerDC) > 0)) {
    play_success = PlayEnhMetaFile (printerDC, hemf, &r);
    if (EndPage(printerDC) > 0) {
      print_success = TRUE;
      EndDoc(printerDC) ;
    }
  }
  /**** restore original cursor */
  return play_success;
}
#endif /* WIN32 */

static void StGWPrint(StGWWinInfo *gwinfo)
{
#ifdef WIN32
  /***** need to figure out how to disable for win32s */
  if (gwinfo && gwinfo->window) {
    HDC printerDC;
    HENHMETAFILE hEMF;
    int success;

    /* Show the Print dialog box and get printer DC */
    if (! GetPrinterDC(&printerDC))
      return;
    if (printerDC == NULL)
      xlfail("Can't get printer DC");
  
    /* Draw the graph into an enhanced metafile */
    {
      CONTEXT cntxt, *target;
      int jumping, mask;
      LVAL val;

      xlbegin(&cntxt,CF_UNWIND,NIL);
      if (XL_SETJMP(cntxt.c_jmpbuf)) {
	target = xltarget;
	mask = xlmask;
	val = xlvalue;
	jumping = TRUE;
      }
      else {
	target = NULL;
	mask = 0;
	val = NIL;
	jumping = FALSE;

	/* protected code */
	if (buffering) xlfail("already buffering");
	StGWResetBuffer();
	bufferDC = CreateEnhMetaFile(NULL, NULL, NULL, NULL);
	if (bufferDC == NULL) xlfail("can't create meta file");
	init_dc(gwinfo, bufferDC, FALSE);
	currentDC = bufferDC;
	buffering = TRUE;
	bufflevel++;
	StGWObRedraw(GETWINOBJECT(gwinfo->window));
      }
      xlend(&cntxt);

      /* cleanup code */
      if (bufferDC != NULL) hEMF = CloseEnhMetaFile(bufferDC);
      bufferDC = 0;
      StGWResetBuffer();
      if (jumping) DeleteDC(printerDC);
      if (hEMF == NULL) xlfail("failed to close metafile properly");
      if (jumping) xljump(target, mask, val);
    }

    success = PrintEMF(printerDC, hEMF);
    DeleteDC (printerDC);
    DeleteEnhMetaFile(hEMF);
    if (! success) xlfail("printing failed");
  }
#endif /* WIN32 */
}

/**************************************************************************/
/**                                                                      **/
/**                       Basic Drawing Functions                        **/
/**                                                                      **/
/**************************************************************************/

void StGWDrawPoint(StGWWinInfo *gwinfo, int x, int y)
{
  HWND w;
  HDC hDC;
  HPEN oldPen;
  int oldWidth;

  if (gwinfo && (w = gwinfo->window) != 0) {
    oldWidth = gwinfo->drawPen.lopnWidth.x;
    gwinfo->drawPen.lopnWidth.x = 1;
    hDC = GET_DC(w);
    oldPen = SET_PEN(hDC, gwinfo);
    MoveToEx(hDC, x, y, NULL);
    LineTo(hDC, x + 1, y + 1);
    RESTORE_PEN(hDC, oldPen);
    RELEASE_DC(w, hDC);
    gwinfo->drawPen.lopnWidth.x = oldWidth;
  }
}

void StGWDrawLine(StGWWinInfo *gwinfo, int x1, int y1, int x2, int y2)
{
  HWND w;
  HDC hDC;
  HPEN oldPen;

  if (gwinfo && (w = gwinfo->window) != 0) {
    hDC = GET_DC(w);
    oldPen = SET_PEN(hDC, gwinfo);
    MoveToEx(hDC, x1, y1, NULL);
    LineTo(hDC, x2, y2);
    RESTORE_PEN(hDC, oldPen);
    RELEASE_DC(w, hDC);
  }
}

void StGWFrameRect(StGWWinInfo *gwinfo, int left, int top, int width, int height)
{
  HDC hDC;
  HPEN oldPen;
  HBRUSH oldBrush;

  if (gwinfo && gwinfo->window) {
    hDC = GET_DC(gwinfo->window);
    oldPen = SET_PEN(hDC, gwinfo);
    oldBrush = SelectObject(hDC, GetStockObject(NULL_BRUSH));
    if (gwinfo->lineWidth != 1) {
      left += gwinfo->rect_offset;
      top += gwinfo->rect_offset;
      width += 1 - gwinfo->lineWidth;
      height += 1 - gwinfo->lineWidth;
    }
    Rectangle(hDC, left, top, left + width, top + height);
    SelectObject(hDC, oldBrush);
    RESTORE_PEN(hDC, oldPen);
    RELEASE_DC(gwinfo->window, hDC);
  }
}

static void fill_rect(StGWWinInfo *gwinfo, int which,
		      int left, int top, int width, int height)
{
  HDC hDC;
  RECT r;
  HBRUSH hbr;

  if (gwinfo && gwinfo->window) {
    r.left = left; r.top = top;
    r.right = left + width; r.bottom = top + height;
    hDC = GET_DC(gwinfo->window);
    hbr = (which == 'P') ? GET_DRAW_BRUSH(gwinfo) : GET_ERASE_BRUSH(gwinfo);
    FillRect(hDC, &r, hbr);
    RELEASE_BRUSH(hbr);
    RELEASE_DC(gwinfo->window, hDC);
  }
}

void StGWPaintRect(StGWWinInfo *gwinfo,
		   int left, int top, int width, int height)
{
  fill_rect(gwinfo, 'P', left, top, width, height);
}

void StGWEraseRect(StGWWinInfo *gwinfo,
		   int left, int top, int width, int height)
{
  fill_rect(gwinfo, 'E', left, top, width, height);
}

static void draw_oval(StGWWinInfo *gwinfo, int which,
		      int left, int top, int width, int height)
{
  HDC hDC;
  HPEN oldPen;
  HBRUSH oldBrush;

  if (gwinfo && gwinfo->window) {
    hDC = GET_DC(gwinfo->window);
    if (which == 'F') {
      oldPen = SET_PEN(hDC, gwinfo);
      oldBrush = SelectObject(hDC, GetStockObject(NULL_BRUSH));
      if (gwinfo->lineWidth != 1) {
	left += gwinfo->rect_offset;
	top += gwinfo->rect_offset;
	width += 1 - gwinfo->lineWidth;
	height += 1 - gwinfo->lineWidth;
      }
    }
    else {
      oldPen = SelectObject(hDC, GetStockObject(NULL_PEN));
      oldBrush = SelectObject(hDC, (which == 'P') ? GET_DRAW_BRUSH(gwinfo) : GET_ERASE_BRUSH(gwinfo));
      width++;  // make up for some deficiency in filling
      height++; // make up for some deficiency in filling
    }
    Ellipse(hDC, left, top, left + width, top + height);
    if (which == 'F') {
      SelectObject(hDC, oldBrush);
      RESTORE_PEN(hDC, oldPen);
    }
    else {
      RESTORE_BRUSH(hDC, oldBrush);
      SelectObject(hDC, oldPen);
    }
    RELEASE_DC(gwinfo->window, hDC);
  }
}

void StGWFrameOval(StGWWinInfo *gwinfo,
		   int left, int top, int width, int height)
{
  draw_oval(gwinfo, 'F', left, top, width, height);
}

void StGWPaintOval(StGWWinInfo *gwinfo,
		   int left, int top, int width, int height)
{
  draw_oval(gwinfo, 'P', left, top, width, height);
}

void StGWEraseOval(StGWWinInfo *gwinfo,
		   int left, int top, int width, int height)
{
  draw_oval(gwinfo, 'E', left, top, width, height);
}

static POINT angle_to_point(double angle,
			    int left, int top, int width, int height)
{
  static double deg2rad = 3.14159 / 180.0;
  double c = cos(deg2rad * angle), s = sin(deg2rad * angle);
  int radius = (width > height) ? width : height;
  POINT pt;

  pt.x = left + width / 2 + radius * c;
  pt.y = top + height / 2 - radius * s;
  return(pt);
}

static void draw_arc(StGWWinInfo *gwinfo, int which,
		     int left, int top, int width, int height,
		     double angle1, double angle2)
{
  HDC hDC;
  HPEN oldPen;
  HBRUSH oldBrush;
  POINT a1, a2;

  a1 = angle_to_point(angle1, left, top, width, height);
  a2 = angle_to_point(angle1 + angle2, left, top, width, height);
  if (gwinfo && gwinfo->window) {
    hDC = GET_DC(gwinfo->window);
    if (which == 'F') {
      oldPen = SET_PEN(hDC, gwinfo);
      oldBrush = SelectObject(hDC, GetStockObject(NULL_BRUSH));
      if (gwinfo->lineWidth != 1) {
	left += gwinfo->rect_offset;
	top += gwinfo->rect_offset;
	width += 1 - gwinfo->lineWidth;
	height += 1 - gwinfo->lineWidth;
      }
    }
    else {
      oldPen = SelectObject(hDC, GetStockObject(NULL_PEN));
      oldBrush = SelectObject(hDC, (which == 'P') ? GET_DRAW_BRUSH(gwinfo) : GET_ERASE_BRUSH(gwinfo));
      width++;  // make up for some deficiency in filling
      height++; // make up for some deficiency in filling
    }
    if (which == 'F')
      Arc(hDC, left, top, left + width, top + height, a1.x, a1.y, a2.x, a2.y);
    else
      Pie(hDC, left, top, left + width, top + height, a1.x, a1.y, a2.x, a2.y);
    if (which == 'F') {
      SelectObject(hDC, oldBrush);
      RESTORE_PEN(hDC, oldPen);
    }
    else {
      RESTORE_BRUSH(hDC, oldBrush);
      SelectObject(hDC, oldPen);
    }
    RELEASE_DC(gwinfo->window, hDC);
  }
}

void StGWFrameArc(StGWWinInfo *gwinfo,
		  int left, int top, int width, int height,
		  double angle1, double angle2)
{
  draw_arc(gwinfo, 'F', left, top, width, height, angle1, angle2);
}

void StGWPaintArc(StGWWinInfo *gwinfo,
		  int left, int top, int width, int height,
		  double angle1, double angle2)
{
  draw_arc(gwinfo, 'P', left, top, width, height, angle1, angle2);
}

void StGWEraseArc(StGWWinInfo *gwinfo,
		  int left, int top, int width, int height,
		  double angle1, double angle2)
{
  draw_arc(gwinfo, 'E', left, top, width, height, angle1, angle2);
}

static void draw_poly(StGWWinInfo *gwinfo, int which, int n, short *p,
		      int from_origin)
{
  HDC hDC;
  HPEN oldPen;
  HBRUSH oldBrush;
  int i;
  POINT *P;

  if (! from_origin) {
    for (i = 1; i < n; i++) {
      p[2 * i] += p[2 * (i - 1)];
      p[2 * i + 1] += p[2 * (i - 1) + 1];
    }
  }

  if (gwinfo && gwinfo->window) {
    P = (POINT *) ((n > 0) ? StCalloc(n, sizeof(POINT)) : NULL);
    for (i = 0; i < n; i++) {
      P[i].x = p[2 * i];
      P[i].y = p[2 * i + 1];
    }

    hDC = GET_DC(gwinfo->window);
    if (which == 'F') {
      oldPen = SET_PEN(hDC, gwinfo);
      oldBrush = SelectObject(hDC, GetStockObject(NULL_BRUSH));
    }
    else {
      oldPen = SelectObject(hDC, GetStockObject(NULL_PEN));
      oldBrush = SelectObject(hDC, (which == 'P') ? GET_DRAW_BRUSH(gwinfo) : GET_ERASE_BRUSH(gwinfo));
    }
    if (which == 'F') Polyline(hDC, P, n);
    else Polygon(hDC, P, n);
    if (which == 'F') {
      SelectObject(hDC, oldBrush);
      RESTORE_PEN(hDC, oldPen);
    }
    else {
      RESTORE_BRUSH(hDC, oldBrush);
      SelectObject(hDC, oldPen);
    }
    RELEASE_DC(gwinfo->window, hDC);
    StFree(P);
  }
}

void StGWFramePoly(StGWWinInfo *gwinfo, int n, short *p, int from_origin)
{
  draw_poly(gwinfo, 'F', n, p, from_origin);
}

void StGWPaintPoly(StGWWinInfo *gwinfo, int n, short *p, int from_origin)
{
  draw_poly(gwinfo, 'P', n, p, from_origin);
}

void StGWErasePoly(StGWWinInfo *gwinfo, int n, short *p, int from_origin)
{
  draw_poly(gwinfo, 'E', n, p, from_origin);
}

/**************************************************************************/
/**                                                                      **/
/**                           Text Functions                             **/
/**                                                                      **/
/**************************************************************************/

static HDC charDC = 0, upcharDC = 0;
static HBITMAP charBits = 0, upcharBits = 0;
static int charSize = 0;

static void init_msw_text(void)
{
  HFONT oldFont;
  HDC hDC;
  LOGFONT lf;
  POINT pt;

  memset(&lf, 0, sizeof(LOGFONT));
  GetPrivateProfileString(graphicsSection, "Font", "Courier New",
                          lf.lfFaceName, LF_FACESIZE, iniFile);
  pt.y = -GetPrivateProfileInt(graphicsSection, "FontSize", 11, iniFile);

  hDC = GetDC(hWndFrame);
  
  /* convert specified point size to pixels */
  pt.y = (pt.y * GetDeviceCaps(hDC, LOGPIXELSY)) / 72;
  DPtoLP(hDC, &pt, 1);
  lf.lfHeight = pt.y;

  hGraphFont = CreateFontIndirect(&lf);
  oldFont = SelectObject(hDC, hGraphFont);
  GetTextMetrics(hDC, &tm);
  SelectObject(hDC, oldFont);

  if (tm.tmPitchAndFamily & TMPF_TRUETYPE) {
    /* rotate 90 degrees for drawing up */
    lf.lfEscapement = lf.lfOrientation = 900;
    hGraphFontUp = CreateFontIndirect(&lf);
  }
  else hGraphFontUp = NULL;

  charDC = CreateCompatibleDC(hDC);
  upcharDC = CreateCompatibleDC(hDC);
  ReleaseDC(hWndFrame, hDC);

  charSize = (tm.tmHeight >  tm.tmAveCharWidth) ? tm.tmHeight :  tm.tmAveCharWidth;
  charBits = CreateBitmap(charSize, charSize, 1, 1, NULL);
  upcharBits = CreateBitmap(charSize, charSize, 1, 1, NULL);
  SelectObject(charDC, charBits);
  SelectObject(charDC, hGraphFont);
  SelectObject(upcharDC, upcharBits);
  SelectObject(upcharDC, hGraphFont);
}

static void cleanup_msw_text(void)
{
  if (charDC) {
    DeleteDC(charDC);
    charDC = 0;
  }
  if (upcharDC) {
    DeleteDC(upcharDC);
    upcharDC = 0;
  }
  if (charBits) {
    DeleteObject(charBits);
    charBits = 0;
  }
  if (upcharBits) {
    DeleteObject(upcharBits);
    upcharBits = 0;
  }
  if (hGraphFont)
    DeleteObject(hGraphFont);
  if (hGraphFontUp)
    DeleteObject(hGraphFontUp);
}

static void draw_char(char ch, int x, int y, HDC hDC, int mode)
{
  if (mode) {
    PatBlt(charDC, 0, 0, charSize, charSize, WHITENESS);
    TextOut(charDC, 0, 0, &ch, 1);
    BitBlt(hDC, x, y, charSize, charSize, charDC, 0, 0,  0x990066L);
  }
  else {
    int bkm = SetBkMode(hDC, TRANSPARENT);
    TextOut(hDC, x, y, &ch, 1);
    SetBkMode(hDC, bkm);
  }
}

static void draw_char_up(char ch, int x, int y, HDC hDC, int mode)
{
  if (hGraphFontUp) {
    HFONT hfontOld;
    /* offsets of 1 seem to be needed to mach bitmap results */
    x++;
    y++;
    if (mode) {
      hfontOld = SelectObject(charDC, hGraphFontUp);
      PatBlt(charDC, 0, 0, charSize, charSize, WHITENESS);
      TextOut(charDC, 0, tm.tmAveCharWidth, &ch, 1);
      BitBlt(hDC, x, y, charSize, charSize, charDC, 0, 0,  0x990066L);
      SelectObject(charDC, hfontOld);
    }
    else {
      hfontOld = SelectObject(hDC, hGraphFontUp);
      draw_char(ch, x, y, hDC, mode);
      SelectObject(hDC, hfontOld);
    }
  }
  else {
    int i, j;
    HDC targetDC;
    COLORREF dc;

    targetDC = mode ? upcharDC : hDC;
    dc = mode ? 0 : GetTextColor(hDC);
    PatBlt(charDC, 0, 0, charSize, charSize, WHITENESS);
    TextOut(charDC, 0, 0, &ch, 1);
    if (mode)
      PatBlt(upcharDC, 0, 0, charSize, charSize, WHITENESS);
    for (i = 0; i < tm.tmAveCharWidth; i++)
      for (j = 0; j < tm.tmHeight; j++)
	if (GetPixel(charDC, i, j) == 0)
	  SetPixel(targetDC, x + j, y + tm.tmAveCharWidth - i, dc);
    if (mode)
      BitBlt(hDC, x, y, charSize, charSize, upcharDC, 0, 0,  0x990066L);
  }
}

void StGWDrawString(StGWWinInfo *gwinfo, char *s, int x, int y)
{
  HDC hDC;
  HWND w;

  if (s && gwinfo && (w = gwinfo->window) != 0) {
    hDC = GET_DC(w);
    y -= tm.tmAscent;
    SelectObject(hDC, hGraphFont); // **** why is this needed??
    if (gwinfo->drawMode) {
      SetBkColor(hDC, get_color(0));
      SetTextColor(hDC, get_color(1));
      for (; *s != '\0'; s++, x += tm.tmAveCharWidth)
	draw_char(*s, x, y, hDC, gwinfo->drawMode);
      SetBkColor(hDC, get_color(gwinfo->backColor));
      SetTextColor(hDC, get_color(gwinfo->drawColor));
    }
    else {
      int bkm = SetBkMode(hDC, TRANSPARENT);
      TextOut(hDC, x, y, s, strlen(s));
      SetBkMode(hDC, bkm);
    }
    RELEASE_DC(w, hDC);
  }
}

void StGWDrawStringUp(StGWWinInfo *gwinfo, char *s, int x, int y)
{
  HDC hDC;
  HWND w;

  if (s && gwinfo && (w = gwinfo->window) != 0) {
    hDC = GET_DC(w);
    x -= tm.tmAscent;
    if (gwinfo->drawMode || hGraphFontUp == NULL)
      y -= tm.tmAveCharWidth;
    if (gwinfo->drawMode) {
      SetBkColor(hDC, get_color(0));
      SetTextColor(hDC, get_color(1));
    }
    for (; *s != '\0'; s++, y -= tm.tmAveCharWidth)
      draw_char_up(*s, x, y, hDC, gwinfo->drawMode);
    if (gwinfo->drawMode) {
      SetBkColor(hDC, get_color(gwinfo->backColor));
      SetTextColor(hDC, get_color(gwinfo->drawColor));
    }
    RELEASE_DC(w, hDC);
  }
}

int StGWTextAscent(StGWWinInfo *gwinfo)
#pragma argsused
{
  return(tm.tmAscent);
}

int StGWTextDescent(StGWWinInfo *gwinfo)
#pragma argsused
{
  return(tm.tmDescent);
}

int StGWTextWidth(StGWWinInfo *gwinfo, char *text)
#pragma argsused text
{
  return(text ? strlen(text) * tm.tmAveCharWidth : 0);
}

void StGWDrawText(StGWWinInfo *gwinfo,
		  char *text,
		  int x, int y, int h, int v)
{
  int FontAscent, string_width;

  if (text && gwinfo) {
    FontAscent = StGWTextAscent(gwinfo);
    string_width = StGWTextWidth(gwinfo, text);
    if (v == 1) y += FontAscent;
    if (h == 1) x -= string_width / 2;
    if (h == 2) x -= string_width;
    StGWDrawString(gwinfo, text, x, y);
  }
}

void StGWDrawTextUp(StGWWinInfo *gwinfo,
		    char *text,
		    int x, int y, int h, int v)
{
  int FontAscent, string_width;

  if (text && gwinfo) {
    FontAscent = StGWTextAscent(gwinfo);
    string_width = StGWTextWidth(gwinfo, text);
    if (v == 1) x += FontAscent;
    if (h == 1) y += string_width / 2;
    if (h == 2) y += string_width;
    StGWDrawStringUp(gwinfo, text, x, y);
  }
}

/**************************************************************************/
/**                                                                      **/
/**                          Symbol Functions                            **/
/**                                                                      **/
/**************************************************************************/

#define NUMSYMBOLS 18
#define SYMROWS 5

typedef struct {
  HBITMAP map;
  HDC hDC;
  int left, top, width, height;
  long refcon;
} Symbol;

static Symbol Symbols[NUMSYMBOLS];

void StGWSetSymRefCon(unsigned int index, long rc)
{
  if (index < NUMSYMBOLS) Symbols[index].refcon = rc;
}

long StGWGetSymRefCon(unsigned int index)
{	
  if (index < NUMSYMBOLS) return(Symbols[index].refcon);
  else return(0);
}

static void InitSymbol(int sym, int left, int top, int width, int height)
{
  HDC hDC;

  hDC = GetDC(hWndFrame);
  Symbols[sym].hDC = CreateCompatibleDC(hDC);
  ReleaseDC(hWndFrame, hDC);

  Symbols[sym].map = CreateBitmap(width, height, 1, 1, NULL);
  SelectObject(Symbols[sym].hDC, Symbols[sym].map);
  PatBlt(Symbols[sym].hDC, 0, 0, width, height, WHITENESS);
  Symbols[sym].left = left;
  Symbols[sym].top = top;
  Symbols[sym].width = width;
  Symbols[sym].height = height;
}

void StGWGetSymbolSize(int sym, int *pwidth, int *pheight)
{
  if (pwidth) *pwidth = Symbols[sym].width;
  if (pheight) *pheight = Symbols[sym].height;
}

static void SetSymbolData(int sym, int row,
			  int bit0, int bit1, int bit2, int bit3, int bit4)
{
  if (bit0) SetPixel(Symbols[sym].hDC, row, 0, 0);
  if (bit1) SetPixel(Symbols[sym].hDC, row, 1, 0);
  if (bit2) SetPixel(Symbols[sym].hDC, row, 2, 0);
  if (bit3) SetPixel(Symbols[sym].hDC, row, 3, 0);
  if (bit4) SetPixel(Symbols[sym].hDC, row, 4, 0);
}

static void init_msw_symbols(void)
{
  InitSymbol(0, 0, 0, 2, 2);
  SetSymbolData(0, 0, 1, 0, 0, 0, 0);
  SetSymbolData(0, 1, 0, 0, 0, 0, 0);
  SetSymbolData(0, 2, 0, 0, 0, 0, 0);
  SetSymbolData(0, 3, 0, 0, 0, 0, 0);
  SetSymbolData(0, 4, 0, 0, 0, 0, 0);

  InitSymbol(1, 0, 0, 2, 2);
  SetSymbolData(1, 0, 1, 1, 0, 0, 0);
  SetSymbolData(1, 1, 0, 0, 0, 0, 0);
  SetSymbolData(1, 2, 0, 0, 0, 0, 0);
  SetSymbolData(1, 3, 0, 0, 0, 0, 0);
  SetSymbolData(1, 4, 0, 0, 0, 0, 0);

  InitSymbol(2, 0, 0, 2, 2);
  SetSymbolData(2, 0, 1, 1, 0, 0, 0);
  SetSymbolData(2, 1, 1, 0, 0, 0, 0);
  SetSymbolData(2, 2, 0, 0, 0, 0, 0);
  SetSymbolData(2, 3, 0, 0, 0, 0, 0);
  SetSymbolData(2, 4, 0, 0, 0, 0, 0);

  InitSymbol(3, 0, 0, 2, 2);
  SetSymbolData(3, 0, 1, 1, 0, 0, 0);
  SetSymbolData(3, 1, 1, 1, 0, 0, 0);
  SetSymbolData(3, 2, 0, 0, 0, 0, 0);
  SetSymbolData(3, 3, 0, 0, 0, 0, 0);
  SetSymbolData(3, 4, 0, 0, 0, 0, 0);

  InitSymbol(4, 2, 2, 4, 4);
  SetSymbolData(4, 0, 0, 1, 1, 0, 0);
  SetSymbolData(4, 1, 1, 0, 0, 1, 0);
  SetSymbolData(4, 2, 1, 0, 0, 1, 0);
  SetSymbolData(4, 3, 0, 1, 1, 0, 0);
  SetSymbolData(4, 4, 0, 0, 0, 0, 0);

  InitSymbol(5, 2, 2, 4, 4);
  SetSymbolData(5, 0, 0, 1, 1, 0, 0);
  SetSymbolData(5, 1, 1, 1, 1, 1, 0);
  SetSymbolData(5, 2, 1, 1, 1, 1, 0);
  SetSymbolData(5, 3, 0, 1, 1, 0, 0);
  SetSymbolData(5, 4, 0, 0, 0, 0, 0);

  InitSymbol(6, 3, 3, 5, 5);
  SetSymbolData(6, 0, 0, 0, 1, 0, 0);
  SetSymbolData(6, 1, 0, 1, 0, 1, 0);
  SetSymbolData(6, 2, 1, 0, 0, 0, 1);
  SetSymbolData(6, 3, 0, 1, 0, 1, 0);
  SetSymbolData(6, 4, 0, 0, 1, 0, 0);

  InitSymbol(7, 3, 3, 5, 5);
  SetSymbolData(7, 0, 0, 0, 1, 0, 0);
  SetSymbolData(7, 1, 0, 1, 1, 1, 0);
  SetSymbolData(7, 2, 1, 1, 1, 1, 1);
  SetSymbolData(7, 3, 0, 1, 1, 1, 0);
  SetSymbolData(7, 4, 0, 0, 1, 0, 0);

  InitSymbol(8, 3, 3, 5, 5);
  SetSymbolData(8, 0, 0, 0, 1, 0, 0);
  SetSymbolData(8, 1, 0, 0, 1, 0, 0);
  SetSymbolData(8, 2, 1, 1, 1, 1, 1);
  SetSymbolData(8, 3, 0, 0, 1, 0, 0);
  SetSymbolData(8, 4, 0, 0, 1, 0, 0);

  InitSymbol(9, 3, 3, 5, 5);
  SetSymbolData(9, 0, 0, 1, 1, 1, 0);
  SetSymbolData(9, 1, 1, 0, 1, 0, 1);
  SetSymbolData(9, 2, 1, 1, 1, 1, 1);
  SetSymbolData(9, 3, 1, 0, 1, 0, 1);
  SetSymbolData(9, 4, 0, 1, 1, 1, 0);

  InitSymbol(10, 2, 2, 4, 4);
  SetSymbolData(10, 0, 1, 1, 1, 1, 0);
  SetSymbolData(10, 1, 1, 0, 0, 1, 0);
  SetSymbolData(10, 2, 1, 0, 0, 1, 0);
  SetSymbolData(10, 3, 1, 1, 1, 1, 0);
  SetSymbolData(10, 4, 0, 0, 0, 0, 0);

  InitSymbol(11, 2, 2, 4, 4);
  SetSymbolData(11, 0, 1, 1, 1, 1, 0);
  SetSymbolData(11, 1, 1, 1, 1, 1, 0);
  SetSymbolData(11, 2, 1, 1, 1, 1, 0);
  SetSymbolData(11, 3, 1, 1, 1, 1, 0);
  SetSymbolData(11, 4, 0, 0, 0, 0, 0);

  InitSymbol(12, 3, 3, 5, 5);
  SetSymbolData(12, 0, 0, 1, 1, 1, 0);
  SetSymbolData(12, 1, 1, 0, 0, 0, 1);
  SetSymbolData(12, 2, 1, 0, 0, 0, 1);
  SetSymbolData(12, 3, 0, 1, 0, 1, 0);
  SetSymbolData(12, 4, 0, 0, 1, 0, 0);

  InitSymbol(13, 3, 3, 5, 5);
  SetSymbolData(13, 0, 0, 1, 1, 1, 0);
  SetSymbolData(13, 1, 1, 1, 1, 1, 1);
  SetSymbolData(13, 2, 1, 1, 1, 1, 1);
  SetSymbolData(13, 3, 0, 1, 1, 1, 0);
  SetSymbolData(13, 4, 0, 0, 1, 0, 0);

  InitSymbol(14, 3, 3, 5, 5);
  SetSymbolData(14, 0, 0, 0, 1, 0, 0);
  SetSymbolData(14, 1, 0, 1, 0, 1, 0);
  SetSymbolData(14, 2, 1, 0, 0, 0, 1);
  SetSymbolData(14, 3, 1, 0, 0, 0, 1);
  SetSymbolData(14, 4, 0, 1, 1, 1, 0);

  InitSymbol(15, 3, 3, 5, 5);
  SetSymbolData(15, 0, 0, 0, 1, 0, 0);
  SetSymbolData(15, 1, 0, 1, 1, 1, 0);
  SetSymbolData(15, 2, 1, 1, 1, 1, 1);
  SetSymbolData(15, 3, 1, 1, 1, 1, 1);
  SetSymbolData(15, 4, 0, 1, 1, 1, 0);

  InitSymbol(16, 3, 3, 5, 5);
  SetSymbolData(16, 0, 1, 0, 0, 0, 1);
  SetSymbolData(16, 1, 0, 1, 0, 1, 0);
  SetSymbolData(16, 2, 0, 0, 1, 0, 0);
  SetSymbolData(16, 3, 0, 1, 0, 1, 0);
  SetSymbolData(16, 4, 1, 0, 0, 0, 1);

  InitSymbol(17, 3, 3, 5, 5);
  SetSymbolData(17, 0, 1, 1, 0, 1, 1);
  SetSymbolData(17, 1, 1, 1, 0, 1, 1);
  SetSymbolData(17, 2, 0, 0, 1, 0, 0);
  SetSymbolData(17, 3, 1, 1, 0, 1, 1);
  SetSymbolData(17, 4, 1, 1, 0, 1, 1);
}

static void cleanup_msw_symbols(void)
{
  int i;

  for (i = 0; i < NUMSYMBOLS; i++) {
    if (Symbols[i].hDC) {
      DeleteDC(Symbols[i].hDC);
      Symbols[i].hDC = 0;
    }
    if (Symbols[i].map) {
      DeleteObject(Symbols[i].map);
      Symbols[i].map = 0;
    }
  }
}

void StGWDrawSymbol(StGWWinInfo *gwinfo, int sym, int x, int y)
{
  HDC hDC;
  if (gwinfo && gwinfo->window && sym >= 0 && sym < NUMSYMBOLS) {
    hDC = GET_DC(gwinfo->window);
    x -= Symbols[sym].left;
    y -= Symbols[sym].top;
    if (gwinfo->drawMode) {
      SetBkColor(hDC, get_color(0));
      SetTextColor(hDC, get_color(1));
      BitBlt(hDC, x, y, Symbols[sym].width, Symbols[sym].height,
	     Symbols[sym].hDC, 0, 0,  0x990066L);
      SetBkColor(hDC, get_color(gwinfo->backColor));
      SetTextColor(hDC, get_color(gwinfo->drawColor));
    }
    else if (! buffering || gwinfo->use_color || gwinfo->backColor == 0)
      BitBlt(hDC, x, y, Symbols[sym].width, Symbols[sym].height,
	     Symbols[sym].hDC, 0, 0, SRCCOPY);
    else
      BitBlt(hDC, x, y, Symbols[sym].width, Symbols[sym].height,
	     Symbols[sym].hDC, 0, 0, NOTSRCCOPY);
    RELEASE_DC(gwinfo->window, hDC);
  }
}

void StGWReplaceSymbol(StGWWinInfo *gwinfo,
		       unsigned oldsym, unsigned newsym,
		       int x, int y)
{
  int oldwidth, oldheight, newwidth, newheight;
  
  if (oldsym < NUMSYMBOLS && newsym < NUMSYMBOLS) {
    StGWGetSymbolSize(oldsym, &oldwidth, &oldheight);
    StGWGetSymbolSize(newsym, &newwidth, &newheight);
    if (oldwidth > newwidth || oldheight > newheight)
      StGWEraseRect(gwinfo,
		    x - Symbols[oldsym].left, y - Symbols[oldsym].top,
		    oldwidth, oldheight);
    StGWDrawSymbol(gwinfo, newsym, x, y);
  }
}

/**************************************************************************/
/**                                                                      **/
/**                       Miscellaneous Functions                        **/
/**                                                                      **/
/**************************************************************************/

void StGWSetFreeMem(StGWWinInfo *gwinfo, void (*FreeMem)(HWND))
{
  if (gwinfo) gwinfo->FreeMem = FreeMem;
}

void StGWShowWindow(StGWWinInfo *gwinfo)
{
  HWND w;

  if (gwinfo && (w = gwinfo->window) != 0) {
    StShowWindow(w);
    if (! gwinfo->initialized) StGWInitialDraw(gwinfo);
  }
}

void StGWSetSize(StGWWinInfo *gwinfo, int width, int height, int frame)
{
  if (gwinfo && gwinfo->window)
    StWSetSize(gwinfo->window, width, height, frame);
}

StGWWinInfo *IViewWindowWinInfo(HWND w)
{
  return(w ? (StGWWinInfo *) GETGWINFO(w) : NULL);
}

void StGWInitialDraw(StGWWinInfo *gwinfo)
{
  int left, top, width, height;

  if (gwinfo) {
    gwinfo->initialized = TRUE;
    SetHardwareState(gwinfo);
    StGWObResize(gwinfo->Object);
    StGWGetViewRect(gwinfo, &left, &top, &width, &height);
    StGWStartBuffering(gwinfo);
    StGWObRedraw(gwinfo->Object);
    StGWBufferToScreen(gwinfo, left, top, width, height);
  }
}

void StGWReverseColors(StGWWinInfo *gwinfo)
{
  ColorCode backColor, drawColor;
  LVAL object;

  object = StGWGetObject(gwinfo);
  backColor = StGWBackColor(gwinfo);
  drawColor = StGWDrawColor(gwinfo);
  if (backColor != drawColor) {
    StGWSetBackColor(gwinfo, drawColor);
    StGWSetDrawColor(gwinfo, backColor);
    StGWObRedraw(object);
  }
}

void StGWDrawBitmap(StGWWinInfo *gwinfo,
		    int left, int top, int width, int height,
		    char *image)
{
  HBITMAP hbm, oldbm;
  HDC bmDC, hDC;
  int i, j;

  if (gwinfo && gwinfo->window) {
    hDC = GET_DC(gwinfo->window);
    bmDC = CreateCompatibleDC(hDC);
    if (bmDC) {
      hbm = CreateBitmap(width, height, 1, 1, NULL);
      if (hbm) {
	oldbm = SelectObject(bmDC, hbm);
	PatBlt(bmDC, 0, 0, width, height, WHITENESS);
	for (i = 0; i < height; i++)
	  for (j = 0; j < width; j++)
	    if (image[i * width + j])
	      SetPixel(bmDC, j, i, 0);
	if (gwinfo->drawMode) {
	  SetBkColor(hDC, get_color(0));
	  SetTextColor(hDC, get_color(1));
	  BitBlt(hDC, left, top, width, height, bmDC, 0, 0,  0x990066L);
	  SetBkColor(hDC, get_color(gwinfo->backColor));
	  SetTextColor(hDC, get_color(gwinfo->drawColor));
	}
	else BitBlt(hDC, left, top, width, height, bmDC, 0, 0, SRCCOPY);
	SelectObject(bmDC, oldbm);
	DeleteObject(hbm);
      }
      DeleteDC(bmDC);
    }
    RELEASE_DC(gwinfo->window, hDC);
  }
}

void StGWCopyToClip(StGWWinInfo *gwinfo)
{
  HDC hDC, hDCMem;
  HBITMAP hBM, hOldBM;
  int left, top, width, height;

  if (gwinfo && gwinfo->window) {
    StGWGetViewRect(gwinfo, &left, &top, &width, &height);
    StGWStartBuffering(gwinfo);
    StGWObRedraw(GETWINOBJECT(gwinfo->window));

    hDC = GetDC((HWND) gwinfo->window);
    hDCMem = CreateCompatibleDC(hDC);
    hBM = CreateCompatibleBitmap(hDC, width, height);
    if (hBM) {
      hOldBM = SelectObject(hDCMem, hBM);

      if (! gwinfo->use_color) {
        SetBkColor(hDC, get_color(0));
        SetTextColor(hDC, get_color(1));
      }
      BitBlt(hDCMem, left, top, width, height, currentDC, left, top, SRCCOPY);
      if (! gwinfo->use_color) {
        SetBkColor(hDC, get_color(gwinfo->backColor));
        SetTextColor(hDC, get_color(gwinfo->drawColor));
      }

      OpenClipboard(gwinfo->window);
      EmptyClipboard();
      SetClipboardData(CF_BITMAP, hBM);
      CloseClipboard();

      SelectObject(hDCMem, hOldBM);
    }
    else SysBeep(10);

    DeleteDC(hDCMem);
    ReleaseDC((HWND) gwinfo->window, hDC);
    StGWResetBuffer();
  }
}
