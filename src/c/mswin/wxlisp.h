#define IDM_ABOUT	10
#define IDM_NEWPOINTS	11
#define IDM_NEWLINES	12
#define IDM_CLOSE	13
#define IDM_EXIT	14
#define IDM_TOPLEVEL    15
#define IDM_PRINT	16
#define IDM_PAGESETUP   17

#define IDM_UNDO	20
#define IDM_CUT		21
#define	IDM_COPY	22
#define IDM_PASTE	23
#define IDM_CLEAR	24
#define IDM_COPYPASTE	25

#define IDM_TILE	30
#define IDM_CASCADE	31
#define	IDM_ARRANGE	32
#define	IDM_CLOSEALL	33

#define IDC_EDIT	50
#define IDC_FILENAME	51
#define IDC_EDITNAME	52
#define IDC_FILES	53
#define IDC_PATH	54
#define IDC_LISTBOX	55

#define IDC_SHOWWINDOW	60
#define IDC_HIDEWINDOW	61
#define IDC_DESTROY     62

#ifndef VK_C
#define VK_C 0x43
#endif

#define IDM_FIRSTCHILD	100

#define IDS_DIALOGFAILURE     1
#define IDS_STRUCTSIZE        2
#define IDS_INITIALIZATION    3
#define IDS_NOTEMPLATE        4
#define IDS_NOHINSTANCE       5
#define IDS_LOADSTRFAILURE    6
#define IDS_FINDRESFAILURE    7
#define IDS_LOADRESFAILURE    8
#define IDS_LOCKRESFAILURE    9
#define IDS_MEMALLOCFAILURE  10
#define IDS_MEMLOCKFAILURE   11
#define IDS_NOHOOK           12
#define IDS_SETUPFAILURE     13
#define IDS_PARSEFAILURE     14
#define IDS_RETDEFFAILURE    15
#define IDS_LOADDRVFAILURE   16
#define IDS_GETDEVMODEFAIL   17
#define IDS_INITFAILURE      18
#define IDS_NODEVICES        19
#define IDS_NODEFAULTPRN     20
#define IDS_DNDMMISMATCH     21
#define IDS_CREATEICFAILURE  22
#define IDS_PRINTERNOTFOUND  23
#define IDS_NOFONTS          24
#define IDS_SUBCLASSFAILURE  25
#define IDS_INVALIDFILENAME  26
#define IDS_BUFFERTOOSMALL   27
#define IDS_FILTERSTRING     28
#define IDS_UNKNOWNERROR     29

#ifndef SetWindowStyle
#define SetWindowStyle(w,s) SetWindowLong(w, GWL_STYLE, s)
#endif
#define MDIDestroyWindow(c,w) SendMessage(c, WM_MDIDESTROY, (WPARAM) w, 0)
#define MDICreateWindow(c,s) \
  ((HWND) SendMessage(hWndClient, WM_MDICREATE, \
                      0, (LONG) (LPMDICREATESTRUCT) s))
#define MDIRestoreWindow(c,w) SendMessage(c, WM_MDIRESTORE, (WPARAM) w, 0)
#define MDIActivateWindow(c,w) SendMessage(c, WM_MDIACTIVATE, (WPARAM) w, 0)
#ifdef WIN32
#define MDIGetActiveWindow(c) \
  ((HWND) SendMessage(hWndClient, WM_MDIGETACTIVE, 0, 0))
#else
#define MDIGetActiveWindow(c) \
  ((HWND) LOWORD(SendMessage(hWndClient, WM_MDIGETACTIVE, 0, 0)))
#endif /* WIN32 */
#define XLSDestroyWindow(w) SendMessage(w, WM_COMMAND, IDC_DESTROY, 0)

#ifndef	RC_INVOKED
extern void ExitXLS(void);
extern void malloc_cleanup(void);
extern void MSWResetGraphics(void);
extern void MSWGraphCleanup(void);
extern void MSWResetMenus(void);
extern void MSWResetDialogs(void);
extern void MSWDLLCleanup(void);
#endif
