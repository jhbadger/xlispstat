/* Lisp Editor and Listener Window Class */

void InitLEditClass(void (*) (), HFONT);
HWND CreateLEditWindow(HWND, HMENU, HANDLE);
BOOL TTYHasInput(void);
void TTYPutStr(char *);
int TTYPutC(int);
void TTYResetInput(void);
void TTYFlushOutput(void);
void TTYFlush(void);
int TTYGetC(void);
BOOL TTYHasSelection(void);
void TTYSelToClip(void);
void TTYClearSel(void);
void TTYPasteFromClip(void);
#ifdef NOTTY
char *TTYSelectionStr(void);
#else
void TTYTrimBuffer(void);
#endif /* NOTTY */

#define XLSGetWindowProc(w) ((WNDPROC) GetWindowLong(w, GWL_WNDPROC))
#define XLSEditCopy(w) SendMessage(w, WM_COPY, 0, 0)
#define XLSEditClear(w) SendMessage(w, WM_CLEAR, 0, 0)
#define XLSEditPaste(w) SendMessage(w, WM_PASTE, 0, 0)
