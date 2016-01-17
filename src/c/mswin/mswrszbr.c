#include "xlisp.h"
#include "xlstat.h"
#include "wxlisp.h"

/* external variables */
extern HWND hWndFrame, hWndClient;
extern HANDLE hInst, hAccel;

/* global variables */
static char szRSZWinClass[]  = "RSZWindowClass";
static int done, changed, mouseDown, brleft, brtop, brwidth, brheight;

/* function prototyles */
long CALLBACK RSZWinProc(HWND, UINT, WPARAM, LONG);

/* defines */
#define RZTOP 100
#define RZHEIGHT 100
#define RZWIDTH 200
#define RZMARGIN 10
#define RZBTNWIDTH 85

BOOL InitApplResizeBrush(HANDLE hInstance)
{
  WNDCLASS wc;

  /* class structure for the IVIEW window */
  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = RSZWinProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = hInstance;
  wc.hIcon = (HICON) NULL;
  wc.hCursor = LoadCursor((HWND) NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH) (COLOR_WINDOW + 1);
  wc.lpszMenuName = NULL;
  wc. lpszClassName = szRSZWinClass;
  if (! RegisterClass(&wc)) return(FALSE);
  return(TRUE);
}

int IViewGetNewBrushSize(HWND w, int *pwidth, int *pheight)
#pragma argsused pwidth pheight
{
  HWND hWnd;
  int scwidth, scheight, btnHeight;
  int left, top, width, height;
  HDC hDC;
  TEXTMETRIC tm;
  MSG msg;

  hDC = GetDC(hWndFrame);
  SelectObject(hDC, GetStockObject(SYSTEM_FIXED_FONT));
  GetTextMetrics(hDC, &tm);
  btnHeight = (7 * tm.tmHeight) / 4;
  ReleaseDC(hWndFrame, hDC);

  StGetScreenSize(&scwidth, &scheight);
  width = RZWIDTH + 2 * GetSystemMetrics(SM_CXDLGFRAME);
  height = RZHEIGHT + 2 * GetSystemMetrics(SM_CYDLGFRAME);
  top = RZTOP;
  left = (scwidth - width) / 2;

  hWnd = CreateWindow(szRSZWinClass,
		      "Resize Brush", WS_POPUP | WS_DLGFRAME | WS_VISIBLE,
		      left, top, width, height, hWndFrame, 0, hInst, 0);
  CreateWindow("static", "Click in this window and drag.",
	       WS_CHILD | SS_CENTER | WS_VISIBLE,
	       RZMARGIN, RZMARGIN,
	       RZWIDTH - RZMARGIN, RZHEIGHT - RZMARGIN,
	       hWnd, 0, hInst, NULL);
  CreateWindow("button", "OK",
	       WS_CHILD | WS_VISIBLE | BS_PUSHBUTTON,
	       RZMARGIN, RZHEIGHT - RZMARGIN - btnHeight,
	       RZBTNWIDTH, btnHeight,
	       hWnd, (HMENU) IDOK, hInst, NULL);
  CreateWindow("button", "Cancel",
	       WS_CHILD | WS_VISIBLE | BS_PUSHBUTTON,
	       2 * RZMARGIN + RZBTNWIDTH,
	       RZHEIGHT - RZMARGIN - btnHeight,
	       RZBTNWIDTH, btnHeight, hWnd, (HMENU) IDCANCEL,
	       hInst, NULL);

  done = FALSE;
  changed = FALSE;
  mouseDown = FALSE;
  brwidth = pwidth ? *pwidth : 10;
  brheight = pheight ? *pheight : 10;

  while (! done && GetMessage(&msg, hWnd, 0, 0)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
  DestroyWindow(hWnd);

  if (changed) {
    if (pwidth) *pwidth = (brwidth > 0) ? brwidth : -brwidth;
    if (pheight) *pheight = (brheight > 0) ? brheight : -brheight;
  }

  return(changed);
}

static void set_brush(int x, int y, int initial)
{
  if (initial) {
    brleft = x;
    brtop = y;
    brwidth = 0;
    brheight = 0;
  }
  else {
    brwidth = x - brleft;
    brheight = y - brtop;
  }
}

static void draw_brush(HWND hWnd)
{
  HDC hDC;
  int left, top, width, height;

  left = (brwidth > 0) ? brleft : brleft + brwidth;
  top = (brheight > 0) ? brtop : brtop + brheight;
  width = (brwidth > 0) ? brwidth : -brwidth;
  height = (brheight > 0) ? brheight : -brheight;

  hDC = GetDC(hWnd);
  SetROP2(hDC, R2_NOT);
  SelectObject(hDC, GetStockObject(NULL_BRUSH));
  Rectangle(hDC, left, top, left + width, top + height);
  ReleaseDC(hWnd, hDC);
}

long CALLBACK RSZWinProc(HWND hWnd, UINT message, WPARAM wParam, LONG lParam)
{
  switch(message) {
  case WM_COMMAND:
    switch (GET_WM_COMMAND_ID(wParam, lParam)) {
    case IDOK:     done = TRUE; changed = TRUE;  return(TRUE);
    case IDCANCEL: done = TRUE; changed = FALSE; return(FALSE);
    }
    break;
  case WM_MOUSEMOVE:
    if (mouseDown) {
      draw_brush(hWnd); // to erase
      set_brush(LOWORD(lParam), HIWORD(lParam), FALSE);
      draw_brush(hWnd); // to draw
    }
    return(0);
  case WM_LBUTTONDOWN:
  case WM_MBUTTONDOWN:
  case WM_RBUTTONDOWN:
    mouseDown = TRUE;
    SetCapture(hWnd);
    set_brush(LOWORD(lParam), HIWORD(lParam), TRUE);
    draw_brush(hWnd);
    return(0);
  case WM_LBUTTONUP:
  case WM_MBUTTONUP:
  case WM_RBUTTONUP:
    ReleaseCapture();
    mouseDown = FALSE;
    draw_brush(hWnd); // to erase
    set_brush(LOWORD(lParam), HIWORD(lParam), FALSE);
    return(0);
  }
  return(DefWindowProc(hWnd, message, wParam, lParam));
}
