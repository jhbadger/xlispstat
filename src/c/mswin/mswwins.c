#include "xlisp.h"
#include "xlstat.h"
#include "wxlisp.h"

extern HWND hWndFrame, hWndClient;
extern LVAL s_true, s_location, s_size, s_msw_help_file, k_context, k_help,
  k_index, k_key, k_quit;

extern LVAL integer_list_2(int, int);

static LVAL MSWMESSAGE(int which)
{
  xllastarg();
  SendMessage(hWndFrame, WM_COMMAND, which, 0);
  return(NIL);
}

LVAL xsabout_xlisp_stat() { return(MSWMESSAGE(IDM_ABOUT)); }

LVAL msw_cut() {  return(MSWMESSAGE(IDM_CUT)); }
LVAL msw_copy() { return(MSWMESSAGE(IDM_COPY)); }
LVAL msw_paste() { return(MSWMESSAGE(IDM_PASTE)); }
LVAL msw_clear() { return(MSWMESSAGE(IDM_CLEAR)); }
LVAL msw_copy_paste() { return(MSWMESSAGE(IDM_COPYPASTE)); }

LVAL msw_tile() { return(MSWMESSAGE(IDM_TILE)); }
LVAL msw_cascade() { return(MSWMESSAGE(IDM_CASCADE)); }
LVAL msw_closeall() { return(MSWMESSAGE(IDM_CLOSEALL)); }
LVAL msw_arrange_icons() { return(MSWMESSAGE(IDM_ARRANGE)); }

LVAL msw_exit() {  return(MSWMESSAGE(IDM_EXIT)); }

LVAL msw_print() {  return(MSWMESSAGE(IDM_PRINT)); }
LVAL msw_pagesetup() {  return(MSWMESSAGE(IDM_PAGESETUP)); }

LVAL msw_win_exec()
{
  LPSTR s;
  LVAL lshow;
  int show, result;

  s = (LPSTR) getstring(xlgastring());
  if (moreargs()) {
    lshow = xlgetarg();
    if (lshow == s_true) show = SW_SHOWNORMAL;
    else if (lshow == NIL) show = SW_SHOWMINIMIZED;
    else if (fixp(lshow)) show = (int) getfixnum(xlgafixnum());
    else xlbadtype(lshow);
  }
  else show = SW_SHOWNORMAL;

  result = WinExec(s, show);
#ifdef MULVALS
  if (result > 32) {
    xlnumresults = 1;
    xlresults[0] = s_true;
  }
  else {
    xlnumresults = 2;
    xlresults[0] = NIL;
    xlresults[1] = cvfixnum((FIXTYPE) result);
  }
  return xlresults[0];
#else
  return(result > 32 ? s_true : cvfixnum((FIXTYPE) result));
#endif /* MULVALS */
}

void MSWCloseHelp(void)
{
  LVAL fname;

  if (s_msw_help_file != NULL && stringp(fname = getvalue(s_msw_help_file))) {
    WinHelp(hWndFrame, getstring(fname), HELP_QUIT, 0);
    setvalue(s_msw_help_file, NIL);
  }
}

LVAL msw_win_help()
{
  LVAL cmd, fname, data;
  int result;

  fname = xlgastring();
  cmd = xlgasymbol();

  if (cmd == k_context) {
    data = xlgafixnum();
    result = WinHelp(hWndFrame, getstring(fname), HELP_CONTEXT, getfixnum(data));
    setvalue(s_msw_help_file, fname);
  }
  else if (cmd == k_help) {
    result = WinHelp(hWndFrame, getstring(fname), HELP_HELPONHELP, 0);
    setvalue(s_msw_help_file, fname);
  }
  else if (cmd == k_index) {
    result = WinHelp(hWndFrame, getstring(fname), HELP_INDEX, 0);
    setvalue(s_msw_help_file, fname);
  }
  else if (cmd == k_key) {
    data = xlgastring();
    result = WinHelp(hWndFrame, getstring(fname), HELP_KEY, (DWORD) getstring(data));
    setvalue(s_msw_help_file, fname);
  }
  else if (cmd == k_quit) {
    result = WinHelp(hWndFrame, getstring(fname), HELP_QUIT, 0);
    setvalue(s_msw_help_file, NIL);
  }
  else xlfail("unknown help command");

  return(result ? s_true : NIL);
}

LVAL msw_free_mem(void)
{
  xllastarg();
  return(cvfixnum((FIXTYPE) GetFreeSpace(0)));
}

LVAL msw_cursor_size(void)
{
  xllastarg();
  return(integer_list_2(GetSystemMetrics(SM_CXCURSOR),
			GetSystemMetrics(SM_CYCURSOR)));
}

void StShowWindow(HWND w)
{
  SendMessage((HWND) w, WM_COMMAND, IDC_SHOWWINDOW, 0);
}

void StHideWindow(HWND w)
{
  SendMessage((HWND) w, WM_COMMAND, IDC_HIDEWINDOW, 0);
}

void StWSetTitle(HWND w, char *str)
{
  SetWindowText((HWND) w, str);
}

void StGetScreenSize(int *width, int *height)
{
  if (width) *width = GetSystemMetrics(SM_CXSCREEN);
  if (height) *height = GetSystemMetrics(SM_CYSCREEN);
}

int StScreenHasColor(void)
{
  static int inited = FALSE, hasColor = FALSE;
  if (! inited) {
    HDC hDC;
    int nPlanes, nBitsPixel;
    hDC = GetDC(hWndFrame);
    nPlanes = GetDeviceCaps(hDC, PLANES);
    nBitsPixel = GetDeviceCaps(hDC, BITSPIXEL);
    ReleaseDC(hWndFrame, hDC);
    hasColor = (nPlanes > 1 || nBitsPixel > 1) ? TRUE : FALSE;
    inited = TRUE;
  }
  return(hasColor);
}

int StHasWindows(void)
{
  return(TRUE);
}

void StFlushGraphics(void) {}

static void xlbadwinplacement() { xlfail("window placement error"); }

void StWSetLocation(HWND w, int left, int top, int frame)
{
  WINDOWPLACEMENT wp;
  int width, height;

  if (! frame) {
    int capheight = GetSystemMetrics(SM_CYCAPTION);
    int framewidth = GetSystemMetrics(SM_CXFRAME);
    int frameheight = GetSystemMetrics(SM_CYFRAME);
    left -= framewidth;
    top -= capheight + frameheight;
  }

  wp.length = sizeof(WINDOWPLACEMENT);
  if (! GetWindowPlacement(w, &wp)) xlbadwinplacement();
  width = wp.rcNormalPosition.right - wp.rcNormalPosition.left;
  height = wp.rcNormalPosition.bottom - wp.rcNormalPosition.top;
  wp.rcNormalPosition.left = left;
  wp.rcNormalPosition.right = left + width;
  wp.rcNormalPosition.top = top;
  wp.rcNormalPosition.bottom = top + height;
  if (! SetWindowPlacement(w, &wp)) xlbadwinplacement();
}

/* assumes border width and border height are the same */
void StWGetLocation(HWND w, int *pleft, int *ptop, int frame)
{
  WINDOWPLACEMENT wp;
  int left, top;

  wp.length = sizeof(WINDOWPLACEMENT);
  if (! GetWindowPlacement(w, &wp)) xlbadwinplacement();
  left = wp.rcNormalPosition.left;
  top = wp.rcNormalPosition.top;
  if (! frame) {
    int capheight = GetSystemMetrics(SM_CYCAPTION);
    int framewidth = GetSystemMetrics(SM_CXFRAME);
    int frameheight = GetSystemMetrics(SM_CYFRAME);
    left += framewidth;
    top += capheight + frameheight;
  }
  if (pleft) *pleft = left;
  if (ptop) *ptop = top;
}

void StWSetSize(HWND w, int width, int height, int frame)
{
  WINDOWPLACEMENT wp;

  if (! frame) {
    int capheight = GetSystemMetrics(SM_CYCAPTION);
    int framewidth = GetSystemMetrics(SM_CXFRAME);
    int frameheight = GetSystemMetrics(SM_CYFRAME);
    width += 2 * framewidth;
    height += capheight + 2 * frameheight;
  }

  wp.length = sizeof(WINDOWPLACEMENT);
  if (! GetWindowPlacement(w, &wp)) xlbadwinplacement();
  wp.rcNormalPosition.right = wp.rcNormalPosition.left + width;
  wp.rcNormalPosition.bottom = wp.rcNormalPosition.top + height;
  if (! SetWindowPlacement(w, &wp)) xlbadwinplacement();
}

void StWGetSize(HWND w, int *pwidth, int *pheight, int frame)
{
  WINDOWPLACEMENT wp;
  int width, height;

  wp.length = sizeof(WINDOWPLACEMENT);
  if (! GetWindowPlacement(w, &wp)) xlbadwinplacement();
  width = wp.rcNormalPosition.right - wp.rcNormalPosition.left;
  height = wp.rcNormalPosition.bottom - wp.rcNormalPosition.top;

  if (! frame) {
    int capheight = GetSystemMetrics(SM_CYCAPTION);
    int framewidth = GetSystemMetrics(SM_CXFRAME);
    int frameheight = GetSystemMetrics(SM_CYFRAME);
    width -= 2 * framewidth;
    height -= capheight + 2 * frameheight;
  }

  if (pwidth) *pwidth = width;
  if (pheight) *pheight = height;
}

LVAL msw_main_frame_visible(void)
{
  if (moreargs()) {
    if (xlgetarg() != NIL)
      ShowWindow(hWndFrame, IsIconic(hWndFrame) ? SW_SHOWNORMAL : SW_SHOW);
    else
      ShowWindow(hWndFrame, SW_HIDE);
  }
  xllastarg();
  return IsWindowVisible(hWndFrame) ? s_true : NIL;
}
