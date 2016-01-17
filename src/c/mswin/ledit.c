/* Lisp Editor and Listener Window Class */

#include <windows.h>
#include <windowsx.h>
#include <string.h>
#include <ctype.h>
#ifdef NOTTY
#include <dde.h>
#endif /* NOTTY */
#include "ledit.h"
#include "winutils.h"

static HFONT hFixedFont;
static HWND hTTYWnd = NULL;
static void (*MainLoop)(void);
static WNDPROC fpOldEditProc = NULL, fpLEditProc = NULL;

/* input and output buffers */
#define BUFSIZE 255
static char obuf[BUFSIZE];
#ifdef NOTTY
#define instart 0
#else
static char *obp = obuf;
static int instart = 0, inend = 0;
#endif /* NOTTY */

/* TTY Trimming defines */
#define MAXTTYBUF 25000
#define TRIMTTYTO 20000

LONG CALLBACK LEditWndProc(HWND, UINT, WPARAM, LONG);
#ifndef NOTTY
static void waitforline(void);
static BOOL check_parens(char *, int);
static BOOL at_text_end(char *, int);
static BOOL has_return(char *, int);
static BOOL input_complete(int);
static void check_trim_buffer(int);
#endif /* NOTTY */
static BOOL flash_matching_paren(HWND, int);
static void pardelay(void);
static void do_tab(HWND, int);
static void fix_blanks(HWND, int, int);
static int num_to_skip(char *, int);
static BOOL is_special(char *, int);
static void edit_getsel(HWND, UINT *, UINT *);

static char *LockText(void);
static void UnlockText(void);

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                         Public Routines                              **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

void InitLEditClass(void (*f)(), HFONT font)
{
  MainLoop = f;

  hFixedFont = (font) ? font : GetStockObject(ANSI_FIXED_FONT);
}

HWND CreateLEditWindow(HWND hWndParent, HMENU hMenu, HANDLE hInstance)
{
  RECT Rect;
  HWND hWnd;
  HANDLE h;

  /* This generates a global handle tobe used in place of hInstance. */
  /* This forces the edit control to use the global hewp with this   */
  /* handle for its text. It looks like this call works in Win32s    */
  /* too, but is should not be necessary there, so I have ifdefed it */
  /* out for now.                                                    */
#ifdef WIN32
  h = hInstance;
#else
  h = GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, 1024L),
#endif /* WIN32 */

  GetClientRect(hWndParent, (LPRECT) &Rect);
  hWnd = CreateWindow("Edit",
		      NULL,
		      WS_CHILD | WS_VISIBLE | WS_VSCROLL |
		      ES_MULTILINE | ES_AUTOVSCROLL,
		      0,
		      0,
		      (Rect.right - Rect.left),
		      (Rect.bottom - Rect.top),
		      hWndParent,
		      hMenu,
		      h,
		      NULL);

  if (hWnd) {
    if (fpOldEditProc == NULL) {
      fpOldEditProc = XLSGetWindowProc(hWnd);
      fpLEditProc = (WNDPROC) MakeProcInstance((FARPROC) LEditWndProc,
    		                               hInstance);
    }
    (void) SubclassWindow(hWnd, fpLEditProc);

    /* "First" window created is TTY */
    if (hTTYWnd == NULL) hTTYWnd = hWnd;
    SetWindowFont(hWnd, hFixedFont, FALSE);
  }

  return(hWnd);
}

#ifndef NOTTY
BOOL TTYHasInput(void)
{
  return(instart < inend ? TRUE : FALSE);
}

void TTYPutStr(char *s)
{
  while (*s != '\0') TTYPutC((int) *s++);
  TTYFlushOutput();
}

int TTYPutC(int c)
{
  if (obp >= obuf + BUFSIZE - 3) TTYFlushOutput();
  if (c == '\n' || c == '\r') {
    *obp++ = '\r';
    *obp++ = '\n';
    TTYFlushOutput();
  }
  else *obp++ = c;
  return(c);
}

void TTYResetInput(void)
{
  instart = Edit_GetTextLength(hTTYWnd);
  inend = instart;
  Edit_SetSel(hTTYWnd, instart, instart);
}

void TTYFlushOutput(void)
{
  int selstart, selend;

  if (obp > obuf) {
    *obp = '\0';
    obp = obuf;
    selstart = Edit_GetTextLength(hTTYWnd);
    selend = selstart + strlen(obuf);
    Edit_SetSel(hTTYWnd, selstart, selend);
    Edit_ReplaceSel(hTTYWnd, obuf);
    TTYResetInput();
    check_trim_buffer(TRUE);
  }
}

void TTYFlush(void)
{
  TTYFlushOutput();
  TTYResetInput();
}

int TTYGetC(void)
{
  int c;
  char *pText;

  /* wait for input if there is none */
  if (! TTYHasInput()) waitforline();

  /* get the text */
  pText = LockText();
  if (! pText) { SysBeep(10); return('\0'); }

  /* skip linefeeds (or whatever they are) */
  while (pText[instart] == '\r') instart++;

  if (instart < inend)
    c = pText[instart++];
  else {
    SysBeep(10);
    c = '\0';
  }

  /* release the text buffer */
  UnlockText();

  /* reset the enput at the end of the last line */
  if (instart >= inend) TTYResetInput();

  return((c == '\r') ? '\n' : c);
}
#endif /* NOTTY */

BOOL TTYHasSelection(void)
{
  UINT start, end;

  edit_getsel(hTTYWnd, &start, &end);
  return(start < end ? TRUE : FALSE);
}

void TTYSelToClip(void)
{
  XLSEditCopy(hTTYWnd);
}

void TTYClearSel(void)
{
#ifndef NOTTY
  UINT selstart;

  edit_getsel(hTTYWnd, &selstart, NULL);

  if (selstart < instart)
    SysBeep(10);
  else
#endif /* NOTTY */
    XLSEditClear(hTTYWnd);
}

void TTYPasteFromClip(void)
{
#ifndef NOTTY
  UINT selstart;

  edit_getsel(hTTYWnd, &selstart, NULL);

  /* move the insertion point to the end if it is */
  /* before the input text start                  */
  if (selstart < instart) {
    int text_end = Edit_GetTextLength(hTTYWnd);
    Edit_SetSel(hTTYWnd, text_end, text_end);
  }
#endif /* NOTTY */

  XLSEditPaste(hTTYWnd);

#ifndef NOTTY
  /* check for a return and a complete expression */
  if (input_complete(TRUE)) {
    inend = Edit_GetTextLength(hTTYWnd);
  }
#endif /* NOTTY */
}

#ifdef NOTTY
/**** This relies on being able to modify the buffer */
/**** and on the fact that an unlock does nothing */
char *TTYSelectionStr()
{
  UINT selstart, selend;
  char *p;

  edit_getsel(hTTYWnd, &selstart, &selend);
  p = LockText();
  p += selstart;
  p[(int)(selend - selstart)] = '\0';
  return p;
}
#else
void TTYTrimBuffer(void)
{
  check_trim_buffer(FALSE);
}
#endif /* NOTTY */

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                    Subclass Callback Function                        **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

/* window function for LEdit class -- filters returns */
LONG CALLBACK LEditWndProc(HWND hWnd, UINT message, WPARAM wParam, LONG lParam)
{
  if (message == WM_CHAR) {
#ifndef NOTTY
    UINT selstart, selend;

    edit_getsel(hTTYWnd, &selstart, &selend);

    /* ignore backspaces at the start of the input region */
    if (selstart == instart && selstart == selend && wParam == '\b') {
      SysBeep(10);
      return(FALSE);
    }

    /* move the insertion point to the end if it is before the */
    /* input text start                                        */
    if (selstart < instart) {
      int text_end = Edit_GetTextLength(hTTYWnd);
      Edit_SetSel(hTTYWnd, text_end, text_end);
    }
#endif /* NOTTY */

    /* adjust spacing on tab */
    if (wParam == '\t') {
      do_tab(hTTYWnd, instart);
      return(FALSE);
    }

#ifndef NOTTY
    /* jump to end of input on shift-return */
    if (wParam == '\r' && HIBIT(GetKeyState(VK_SHIFT))) {
      selstart = selend = Edit_GetTextLength(hTTYWnd);
      Edit_SetSel(hTTYWnd, selstart, selend);
      return(FALSE);
    }
#endif /* NOTTY */

    /* insert the character using the inherited method */
    CallWindowProc(fpOldEditProc, hWnd, message, wParam, lParam);

    /* flash matching parenthesis */
    if (wParam == ')') flash_matching_paren(hTTYWnd, instart);

#ifndef NOTTY
    /* check the character for a return */
    if (wParam == '\r' && input_complete(FALSE)) {
      inend = Edit_GetTextLength(hTTYWnd);
      return(TRUE);
    }
    else return(FALSE);
#else
    return(TRUE);
#endif /* NOTTY */
  }
  else return(CallWindowProc(fpOldEditProc, hWnd, message, wParam, lParam));
}

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                         Internal Routines                            **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

#ifndef NOTTY
static void waitforline(void)
{
  TTYFlushOutput();
  TTYResetInput();
  (*MainLoop)();
}

static BOOL input_complete(int check_return)
{
  char *pText;
  BOOL result;
  UINT text_end, selstart;
  BOOL checkstart;

  text_end = Edit_GetTextLength(hTTYWnd);
  pText = LockText();
  if (! pText) { SysBeep(10); return(FALSE); }
  edit_getsel(hTTYWnd, &selstart, NULL);
  checkstart = (selstart > 0) ? selstart - 1 : selstart;

  result = check_parens(pText + instart, text_end - instart)
	   && at_text_end(pText + selstart, text_end - selstart)
	   && (! check_return
	       || has_return(pText + checkstart, text_end - checkstart));

  UnlockText();
  return(result);
}

static BOOL at_text_end(char *s, int n)
{
  int i, result = TRUE;

  for (i = 0; result && i < n; i++)
    if (! isspace(s[i]) && s[i] != '\n' && s[i] != '\r')
      result = FALSE;
  return(result);
}

static BOOL has_return(char *s, int n)
{
  int i, result = FALSE;

  for (i = 0; ! result && i < n; i++)
    if (s[i] == '\n' || s[i] == '\r')
      result = TRUE;
  return(result);
}

static BOOL check_parens(s, n)
	char *s;
	int n;
{
  int parcount = 0, inquotes = FALSE, incomment = FALSE;
  char ch;
   
  while (n-- > 0) {
    ch = *s++;
    switch (ch) {
    case  '"': inquotes = ! inquotes; break;
    case  ';': if (! inquotes) incomment = TRUE; break;
    case '\r':
    case '\n': incomment = FALSE; break;
    case  '(': if (! inquotes && ! incomment) parcount++; break;
    case  ')': if (! inquotes && ! incomment) parcount--; break;
    }
  }
  return (parcount <= 0 ? TRUE : FALSE);
}
#endif /* NOTTY */

static BOOL flash_matching_paren(HWND hWnd, int start)
{
  BOOL inquotes = FALSE;
  UINT parcount = 0, sel, par;
  char ch, *s;

  edit_getsel(hWnd, &sel, NULL);

  s = LockText();
  if (! s) { SysBeep(10); return(FALSE); }
  s = s + sel - 1;

  par = sel;
  do {
    par--;
    ch = *s--;
    switch (ch) {
    case  '"': inquotes = ! inquotes; break;
    case  '(': if (! inquotes) parcount--; break;
    case  ')': if (! inquotes) parcount++; break;
    }
  } while (par > start && parcount > 0);

  UnlockText();

  if (ch == '(' && parcount == 0) {
    Edit_SetSel(hWnd, par, par + 1);
    pardelay();
    Edit_SetSel(hWnd, sel, sel);
  }

  return (parcount <= 0 ? TRUE : FALSE);
}

static void pardelay(void)
{
  Delay(250);
}

#define ISRETURNCHAR(c) ((c) == '\r' || (c) == '\n')

static void do_tab(HWND hWnd, int start)
{
  BOOL inquote;
  UINT sel, curline, lastline, nblanks, length;
  int parcount, pos;
  char *s, ch;

  edit_getsel(hWnd, &sel, NULL);

  s = LockText();
  if (! s) { SysBeep(10); return; }
  length = Edit_GetTextLength(hWnd);

  /* find beginning of the line */
  curline = sel;
  while (curline > start && ! ISRETURNCHAR(s[curline - 1])) curline--;
  if (curline == start) return;

  /* find unmatched paren */
  parcount = 0;
  inquote = FALSE;
  pos = curline;
  while (parcount >= 0 && --pos >= start) {
    ch = s[pos];
    switch (ch) {
    case ')': if (! inquote) parcount++; break;
    case '(': if (! inquote) parcount--; break;
    case '"': inquote = ! inquote; break;
    }
  }
  if (parcount == 0) return;
  
  /* find beginning of the line containing the expression start */
  lastline = pos;
  while (lastline > 0 && ! ISRETURNCHAR(s[lastline - 1])) lastline--;

  /* skip forward an s-expression or to first non blank */
  pos += num_to_skip(s + pos, curline - pos);

  if (pos > curline) pos = curline;
  nblanks = pos - lastline;

  /* adjust for the number of blanks already present, replace tabs by blanks */
  for (pos = curline; 
       pos < length && (s[pos] == ' ' || s[pos] == '\t');
       nblanks--, pos++)
    if (s[pos] == '\t') s[pos] = ' ';

  UnlockText();

  /* insert or delete the appropriate number of blanks */
  if (nblanks == 0) return;

  sel += nblanks;
  if (pos > length) pos = length;
  Edit_SetSel(hWnd, pos, pos);
  fix_blanks(hWnd, nblanks, curline);
  length = Edit_GetTextLength(hWnd);
  if (pos > length) pos = length;
  Edit_SetSel(hWnd, sel, sel);
}

static void fix_blanks(HWND hWnd, int nblanks, int curline)
{
  int i;

  if (nblanks > 0) {
    for (i = 0; i < nblanks && i < BUFSIZE -3; i++) obuf[i] = ' ';
    obuf[i] = '\0';
    Edit_SetSel(hWnd, curline, curline);
    Edit_ReplaceSel(hWnd, obuf);
  }
  else {
    obuf[0] = '\0';
    Edit_SetSel(hWnd, curline, curline - nblanks);
    Edit_ReplaceSel(hWnd, obuf);
  }
}

static int num_to_skip(char *s, int n)
{
  char str[4];
  int i, pos, oldpos;

  pos = 0;
  
  if (n <= 0) pos = 0;
  else if (*s == '(') {
  
    s++; n--; pos = 1;
    
    /* skip blanks */
    while (n > 0 && (*s == ' ' || *s == '\t')) { s++; n--; pos++; }
    
    /* check for end of line or list or lisp comment*/
    if (n > 0 && ! ISRETURNCHAR(*s) && *s != ';' && *s != '(') {
    
      /* check for special symbols */
      for (i = 0; i < 3 && i < n; i++)
        str[i] = islower(s[i]) ? (char) toupper(s[i]) : s[i];
      str[i] = '\0';
      if (is_special(s, n) /* strcmp(str, "DEF") == 0 || strcmp(str, "LET") == 0 
          || strcmp(str, "FLE") == 0 */ )
        pos = 2;
      else {
        
        /* skip over the s-expression */
        oldpos = pos;
	while (n > 0 && *s != ' ' && *s != '\t' && ! ISRETURNCHAR(*s))
          { s++; n--; pos++; }
          
        /* check for another s expression */
        for (i = 0; n > 0 && (*s == ' ' || *s == '\t'); s++, n--, i++) ;
	if (n == 0 || ISRETURNCHAR(*s))
	  pos = (oldpos == pos) ? oldpos + 1 : oldpos;
        else pos += i;
      }
    }
  }
  else {
    
    /* skip over any blanks */
    for (i = 0; n > 0 && (*s == ' ' || *s == '\t'); s++, n--, i++) ;
    if (n > 0 && ! ISRETURNCHAR(*s)) pos += i;
  }
  return(pos);
}

static BOOL is_special(char *s, int n)
{
  char str[10];
  int i;

  for (i = 0; i < n && i < 9; i++)
    str[i] = islower(s[i]) ? (char) toupper(s[i]) : s[i];
  str[i] = '\0';

  if (n >= 5 && strncmp(str, "DEFUN", 5) == 0) return(TRUE);
  if (n >= 8 && strncmp(str, "DEFMACRO", 8) == 0) return(TRUE);
  if (n >= 7 && strncmp(str, "DEFMETH", 7) == 0) return(TRUE);
  if (n >= 8 && strncmp(str, "DEFPROTO", 8) == 0) return(TRUE);
  if (n >= 3 && strncmp(str, "LET", 3) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "FLET", 4) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "COND", 4) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "CASE", 4) == 0) return(TRUE);
  if (n >= 6 && strncmp(str, "LABELS", 6) == 0) return(TRUE);
  if (n >= 6 && strncmp(str, "LAMBDA", 6) == 0) return(TRUE);
  return(FALSE);
}

#ifndef NOTTY
static void check_trim_buffer(int maxonly)
{
  int length, start;
  UINT selstart, selend;

  length = Edit_GetTextLength(hTTYWnd);

  if (length > (maxonly ? MAXTTYBUF : TRIMTTYTO)) {
    start = length - TRIMTTYTO;
    edit_getsel(hTTYWnd, &selstart, &selend);
    obuf[0] = '\0';
    Edit_SetSel(hTTYWnd, 0, start);
    Edit_ReplaceSel(hTTYWnd, obuf);
    instart -= start;
    inend -= start;
    selstart -= start;
    selend -= start;
    Edit_SetSel(hTTYWnd, selstart, selend);
  }
}
#endif /* NOTTY */

static void edit_getsel(HWND hWnd, UINT *pstart, UINT *pend)
{
  long sel;

  /**** this doesn't make sense for edit records larget than 64K */
  sel = Edit_GetSel(hWnd);
  if (pstart != NULL) *pstart = LOWORD(sel);
  if (pend != NULL) *pend = HIWORD(sel);
}

#define TBUFSIZ 31000
static char *tbuf = NULL;

static char *LockText(void)
{
  size_t length;

  if (tbuf == NULL) {
    HANDLE h;
    h = GlobalAlloc(GMEM_MOVEABLE, TBUFSIZ + 1);
    tbuf = h ? GlobalLock(h) : 0;
    if (tbuf == NULL) return NULL;
  }

  length = Edit_GetTextLength(hTTYWnd);
  GetWindowText(hTTYWnd, tbuf, TBUFSIZ);
  tbuf[length] = '\0';
  return tbuf;
}

static void UnlockText(void) {}
