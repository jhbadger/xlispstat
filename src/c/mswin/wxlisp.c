#include "xlisp.h"
#include "xlstat.h"
#include <ddeml.h>
#ifndef MinGW32
#  include <alloc.h>
#endif
#include "ledit.h"
#include "winutils.h"
#include "wxlisp.h"
#include "version.h"

/* command line arguments */
static int argc;
static char **argv;
#ifdef WIN32
#define APPNAME "WXLS32"
#else
#define APPNAME "WXLS"
#endif

/* global variables */
static char szFrameClass[] = "MdiFrame";
static char szListenerClass[] = "MdiListenerChild";
static char listenerSection[] = "Listener";
static char xlispSection[] = "Xlisp";
#ifdef WIN32
long win32stsz;
char *iniFile = "wxls32.ini";
#else
char *iniFile = "wxls.ini";
#endif /* WIN32 */
HINSTANCE hInst;
HWND hWndFrame, hWndClient;
static HWND hEditWnd, hEditFrame;
HMENU hMainMenu;
HMENU hWinMenu;
HANDLE hAccel;
static HFONT hListenerFont = NULL;
int Exiting = FALSE;
extern char *defaultpath, *resfile;
extern char buf[];
#ifdef WIN32
BOOL win32s;
#endif
extern LVAL s_in_callback;

static DWORD ddeInst = 0;
static HSZ service, systopic;
static XL_JMP_BUF exit_tag;

/* forward declarations */
static BOOL CheckSystem(void);
static BOOL InitApplication(HANDLE);
static BOOL InitInstance(HANDLE, int);
BOOL InitApplDialogs(HANDLE);
BOOL InitInstDialogs(HANDLE);
BOOL InitApplGraphics(HANDLE);
BOOL InitInstGraphics(HANDLE);
LONG CALLBACK FrameWndProc(HWND, UINT, WPARAM, LONG);
BOOL CALLBACK CloseEnumProc(HWND, LONG);
LONG CALLBACK ListenerWndProc(HWND, UINT, WPARAM, LONG);
HDDEDATA CALLBACK DdeProc(UINT, UINT, HCONV, HSZ, HSZ, HDDEDATA, DWORD, DWORD);
static void mainloop(void);
static void close_all_conversations(void);
static void set_dde_result(HCONV, LVAL);
static LVAL get_dde_result(HCONV);
static void delete_dde_result(HCONV);

int MSWAnyEventsQueued(void);

#define LSGR_LINES 0
#define LSGR_POINTS 1
#define LSGR_MARGIN 10

int WINAPI WinMain(HINSTANCE hInstance,
		   HINSTANCE hPrevInstance,
		   LPSTR lpCmdLine,
		   int nCmdShow)
{
  MSG msg;
  PFNCALLBACK lpDdeProc;

  /* check system characteristics */
  if (! CheckSystem()) return(FALSE);

  /* Try to cooperate with other running instances of the application */
  if (! hPrevInstance)
    if (! InitApplication(hInstance))
      return(FALSE);

  /* check for the COM embedding flag; start invisible if it is there */
  if (strstr(lpCmdLine, "-Embedding") != NULL)
    nCmdShow = SW_HIDE;

  /* initialize the specific instance */
  if (! InitInstance(hInstance, nCmdShow))
    return(FALSE);

  /* initialize DDEML */
  lpDdeProc = (PFNCALLBACK) MakeProcInstance((FARPROC) DdeProc, hInst);
  DdeInitialize(&ddeInst, lpDdeProc,
                APPCLASS_STANDARD |
                CBF_FAIL_ADVISES |
		/***** CBF_FAIL_POKES | CBF_SKIP_CONNECT_CONFIRMS |*/
                CBF_SKIP_REGISTRATIONS | CBF_SKIP_UNREGISTRATIONS,
                0L);

  service = DdeCreateStringHandle(ddeInst, "XLISP-STAT", CP_WINANSI);
  systopic = DdeCreateStringHandle(ddeInst, "SYSTEM", CP_WINANSI);
  if (systopic == NULL) {
    DdeFreeStringHandle(ddeInst, service);
    service = NULL;
  }
  if (service != NULL)
    if (! DdeNameService(ddeInst, service, NULL, DNS_REGISTER)) {
      DdeFreeStringHandle(ddeInst, service);
      service = NULL;
    }

  /* parse the argument list into argv */
  {
    extern char buf[];
    int i=0, j, argcount;

    /* get number of arguments */
    /* The Win32 documentaion on-line at Microsoft says that lpCmdLine
       contains the parameters, not the entire command line. Borland's
       documentation with 5.0 (Programmers Manual p. 186) claims
       otherwise. The runtime in the old version of Borland C I use,
       with __BORLANDC__ = 0x452, actually does pass the whole command
       line. The runtime under 5.0 does not. Hence this weird
       conditional. */
#if defined(WIN32) && defined(__BORLANDC__) && (__BORLANDC__ <= 0x452)
    argc = 0;
#else
    argc = 1;
#endif /* WIN32 */
    while (sscanf(&lpCmdLine[i], "%128s%n", buf, &j) == 1) {
      i += j;
      argc++;
    }

    /* now fill in arguments */
    argv = (char **) calloc(argc, sizeof(char *));
    i = 0;
#if defined(WIN32) && defined(__BORLANDC__) && (__BORLANDC__ <= 0x452)
    argcount = 0;
#else
    argv[0] = APPNAME;
    argcount = 1;
#endif /* WIN32 */
    while (sscanf(&lpCmdLine[i],"%128s%n", buf, &j)==1) {
      i += j;
      argv[argcount] = (char *) malloc(strlen(buf) + 1);
      strcpy(argv[argcount++], buf);
    }
  }

  if (XL_SETJMP(exit_tag) == 0)
    xlsmain(argc, argv);

  /* process orderly shutdown */
  close_all_conversations();
  Exiting = TRUE;
  while (GetMessage(&msg, 0, 0, 0)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }

  /* drop DDEML */
  if (service != NULL) DdeFreeStringHandle(ddeInst, service);
  if (systopic != NULL) DdeFreeStringHandle(ddeInst, systopic);
  DdeNameService(ddeInst, NULL, NULL, DNS_UNREGISTER);
  DdeUninitialize(ddeInst);
  FreeProcInstance((FARPROC) lpDdeProc);

  MSWCloseHelp();
  MSWGraphCleanup();
  MSWDLLCleanup();

  if (hListenerFont)
    DeleteObject(hListenerFont);

#ifndef WIN32
  malloc_cleanup();
#endif /* WIN32 */
  return(msg.wParam);
}

void ExitXLS(void)
{
  XL_LONGJMP(exit_tag, 1);
}

static BOOL CheckSystem(void)
{
  DWORD vinfo = GetVersion();
#ifndef WIN32
  LONG flags = GetWinFlags();
  if (! flags & WF_PMODE) {
    MessageBox((HWND) NULL,
	       "XLISP-STAT must be run in protected mode.",
	       NULL,
	       MB_ICONHAND);
    return(FALSE);
  }
#else
  win32s = ((1<<15) & HIWORD(vinfo)) ? TRUE : FALSE;
  win32stsz = win32s ? WIN32S_STSZ : WIN32NT_STSZ;
#endif /* WIN32 */
  if (! ((LOBYTE(LOWORD(vinfo)) > 3) ||
         (LOBYTE(LOWORD(vinfo)) == 3 && HIBYTE(LOWORD(vinfo)) >= 10))) {
    MessageBox((HWND) NULL,
               "XLISP-STAT requires Windows 3.1 or higher",
               NULL,
               MB_ICONHAND);
    return(FALSE);
  }
  return(TRUE);
}

static BOOL InitApplication(HANDLE hInstance)
{
  WNDCLASS wc;

  /* class structure for the MDI frame window */
  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = FrameWndProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = hInstance;
  wc.hIcon = LoadIcon(hInstance, "WXLSIcon");
  wc.hCursor = LoadCursor((HWND) NULL, IDC_ARROW);
  wc.hbrBackground = (HBRUSH) (COLOR_APPWORKSPACE + 1);
  wc.lpszMenuName = NULL;
  wc.lpszClassName = szFrameClass;
  if (! RegisterClass(&wc)) return(FALSE);

  /* class structure for the listener frame window */
  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = ListenerWndProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = hInstance;
  wc.hIcon = LoadIcon(hInstance, "LEditIcon");
  wc.hCursor = LoadCursor((HWND) NULL, IDC_ARROW);
  wc.hbrBackground = GetStockObject(WHITE_BRUSH);
  wc.lpszMenuName = NULL;
  wc. lpszClassName = szListenerClass;
  if (! RegisterClass(&wc)) return(FALSE);

  if (! InitApplDialogs(hInstance)) return(FALSE);
  if (! InitApplGraphics(hInstance)) return(FALSE);

  return(TRUE);
}

static BOOL InitInstance(HANDLE hInstance, int nCmdShow)
{
  MDICREATESTRUCT mdicreate;
  RECT r;
  DWORD flags;

  hInst = hInstance;

  InitInstDialogs(hInstance);
  InitInstGraphics(hInstance);

  if (GetPrivateProfileString(xlispSection, "Libdir", "", buf, 128, iniFile)) {
    int n;
    n = strlen(buf);
    if (n > 0 && buf[n - 1] != '\\')
      strcat(buf, "\\");
    defaultpath = malloc(strlen(buf) + 1);
    strcpy(defaultpath, buf);
  }
#ifdef WIN32
  else {
    char buf[_MAX_PATH+1];
    char drive[_MAX_PATH+1];
    char dir[_MAX_PATH+1];
    GetModuleFileName(NULL, buf, sizeof(buf));
    _splitpath(buf, drive, dir, NULL, NULL);
    if ((defaultpath = malloc(strlen(drive) + strlen(dir) + 1)) != NULL) {
      strcpy(defaultpath, drive);
      strcat(defaultpath, dir);
    }
  }
#endif /* WIN32 */

  if (GetPrivateProfileString(xlispSection, "Workspace", "", buf, 128, iniFile)) {
    resfile = malloc(strlen(buf) + 1);
    strcpy(resfile, buf);
  }
  else if (defaultpath != NULL) {
    FILE *fp;
    if ((fp = fopen(resfile, "r")) != NULL)
      fclose(fp);
    else {
      strcpy(buf, defaultpath);
      strcat(buf, resfile);
      if ((fp = fopen(buf, "r")) != NULL) {
        fclose(fp);
        resfile = malloc(strlen(buf) + 1);
        strcpy(resfile, buf);
      }
    }
  }

  hAccel = LoadAccelerators(hInst, "MdiAccel");

  hMainMenu = LoadMenu(hInst, "MdiMenu");
  hWinMenu = GetSubMenu(hMainMenu, GetMenuItemCount(hMainMenu) - 1);

  /* create MDI frame for this instance */
  flags = WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | MDIS_ALLCHILDSTYLES;
  if (nCmdShow == SW_HIDE) flags |= WS_MINIMIZE;
  hWndFrame = CreateWindow(szFrameClass,
			   "XLISP-STAT",
			   flags,
			   CW_USEDEFAULT,
			   CW_USEDEFAULT,
			   CW_USEDEFAULT,
			   CW_USEDEFAULT,
			   (HWND) NULL,
			   hMainMenu,
			   hInstance,
			   NULL);
  if (! hWndFrame) return(FALSE);

  hWndClient = GetWindow(hWndFrame, GW_CHILD);
  if (! hWndClient) {
    DestroyWindow(hWndFrame);
    return(FALSE);
  }

  ShowWindow(hWndFrame, nCmdShow);
  UpdateWindow(hWndFrame);

  {
    LOGFONT lf;
    HDC hDC;
    POINT pt;
    memset(&lf, 0, sizeof(LOGFONT));
    GetPrivateProfileString(listenerSection, "Font", "Courier New",
			    lf.lfFaceName, LF_FACESIZE, iniFile);
    pt.y = -GetPrivateProfileInt(listenerSection, "FontSize", 11, iniFile);

    /* convert specified point size to pixels */
    hDC = GetDC(hWndFrame);
    pt.y = (pt.y * GetDeviceCaps(hDC, LOGPIXELSY)) / 72;
    DPtoLP(hDC, &pt, 1);
    lf.lfHeight = pt.y;
    ReleaseDC(hWndFrame, hDC);

    hListenerFont = CreateFontIndirect(&lf);
    InitLEditClass(mainloop, hListenerFont);
  }

  /* create the listener frame */
  GetClientRect(hWndClient, (LPRECT) &r);
  mdicreate.szClass = szListenerClass;
  mdicreate.szTitle = "Listener";
  mdicreate.hOwner = hInst;
  mdicreate.x = CW_USEDEFAULT;
  mdicreate.y = CW_USEDEFAULT;
  mdicreate.cx = r.right - r.left;
  mdicreate.cy = CW_USEDEFAULT;
  mdicreate.style = WS_MINIMIZE;
  mdicreate.lParam = 0;
  hEditFrame = MDICreateWindow(hWndClient, &mdicreate);
  if (! hEditFrame) {
    DestroyWindow(hWndClient);
    DestroyWindow(hWndFrame);
    return(FALSE);
  }

  /* create the listener editor window pane */
  hEditWnd = CreateLEditWindow(hEditFrame, (HMENU) IDC_EDIT, hInst);
  if (! hEditWnd) {
    DestroyWindow(hEditFrame);
    DestroyWindow(hWndClient);
    DestroyWindow(hWndFrame);
    return(FALSE);
  }

  MDIRestoreWindow(hWndClient, hEditFrame);
  ShowWindow(hEditFrame, SW_SHOW);
  UpdateWindow(hEditFrame);
  SetFocus(hEditWnd);

  return(TRUE);
}

LONG CALLBACK FrameWndProc(HWND hWnd, UINT message, WPARAM wParam, LONG lParam)
{
  static HWND hWndClient;
  WNDENUMPROC lpfnEnum;
  HWND hWndChild;
  CLIENTCREATESTRUCT clientcreate;

  switch (message) {
  case WM_CREATE:
    clientcreate.hWindowMenu = hWinMenu;
    clientcreate.idFirstChild = IDM_FIRSTCHILD;
    hWndClient = CreateWindow("MDICLIENT",
			      NULL,
			      WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE,
			      0,
			      0,
			      0,
			      0,
			      hWnd,
			      (HMENU) 1,
			      hInst,
			      (LPSTR) &clientcreate);
    return(0);
  case WM_COMMAND:
    switch (GET_WM_COMMAND_ID(wParam, lParam)) {
    case IDM_ABOUT:
      sprintf(buf, "XLISP-STAT Release %d.%d.%d (%s)",
              XLS_MAJOR_RELEASE,
              XLS_MINOR_RELEASE,
              XLS_SUBMINOR_RELEASE,
#ifdef WIN32
              "Win32"
#else
              "Win16"
#endif
              );
      MessageBox(NULL, buf, "XLISP-STAT", MB_TASKMODAL);
      break;
    case IDM_PRINT:
      hWndChild = MDIGetActiveWindow(hWndClient);
      SendMessage(hWndChild, WM_COMMAND, wParam, lParam);
      return 0;
    case IDM_CLOSE:
      hWndChild = MDIGetActiveWindow(hWndClient);
      SendMessage(hWndChild, WM_CLOSE, 0, 0);
      return(0);
    case IDM_EXIT:
      SendMessage(hWnd, WM_CLOSE, 0, 0);
      return(0);
    case IDM_TILE:
      SendMessage(hWndClient, WM_MDITILE, 0, 0);
      return(0);
    case IDM_CASCADE:
      SendMessage (hWndClient, WM_MDICASCADE, 0, 0);
      return(0);
    case IDM_ARRANGE:
      SendMessage(hWndClient, WM_MDIICONARRANGE, 0, 0);
      return(0);
    case IDM_CLOSEALL:
      lpfnEnum = (WNDENUMPROC) MakeProcInstance((FARPROC) CloseEnumProc, hInst);
      EnumChildWindows(hWndClient, lpfnEnum, 0);
      FreeProcInstance((FARPROC) lpfnEnum);
      return(0);
    default:
      if (IsLispMenuItem(GET_WM_COMMAND_ID(wParam, lParam))) {
	LispMenuSelect(GET_WM_COMMAND_ID(wParam, lParam));
	return(0);
      }
      hWndChild = MDIGetActiveWindow(hWndClient);
      if (IsWindow(hWndChild))
	SendMessage(hWndChild, WM_COMMAND, wParam, lParam);
      break;
    }
    break;
  case WM_INITMENUPOPUP:
    if (IsLispMenuHandle((HMENU) wParam)) LispMenuUpdate((HMENU) wParam);
    break;
  case WM_QUERYENDSESSION:
  case WM_CLOSE:
    if (Exiting)
      break;
    if (OKorCancelBox("Do you really want to quit?") == IDCANCEL)
      return(0);
    do_exits();
    Exiting = TRUE;
    break;
  case WM_DESTROY:
    xexit();
    //PostQuitMessage(0);
    return(0);
  }

  /* pass unprocessed messages to DefFrameProc (not DefWindowProc) */
  return(DefFrameProc(hWnd, hWndClient, message, wParam, lParam));
}

static LVAL StringHandle2LispString(HSZ hsz)
{
  if (hsz == NULL)
    return NIL;
  else {
    LVAL val;
    DdeQueryString(ddeInst, hsz, buf, STRMAX, CP_WINANSI);
    xlsave1(val);
    val = cvstring(buf);
    val = xlcallsubr1(xnupcase, val);
    xlpop();
    return val;
  }
}

static LVAL DdeData2LispString(HDDEDATA hdata, int binary)
{
  LVAL val;
  if (hdata != NULL) {
    DWORD size;
    char *p = (char *) DdeAccessData(hdata, &size);
    if (p != NULL) {
      if (! binary) {
	char *q = memchr(p, 0, size);
	if (q != NULL) size = q - p;
      }
      val = newstring(size);
      memcpy(getstring(val), p, size);
    }
    else val = NIL;
    DdeUnaccessData(hdata);
    DdeFreeDataHandle(hdata);
  }
  else val = NIL;
  return val;
}

HDDEDATA CALLBACK DdeProc(UINT type, UINT fmt, HCONV hconv,
                          HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
                          DWORD dw1, DWORD dw2)
{
  static int inited = FALSE;
  static LVAL callbacksym;
  static LVAL k_connect, k_connect_confirm, k_wildconnect, k_disconnect;
  static LVAL k_execute, k_request, k_poke, k_text;

  if (! inited) {
    callbacksym = xlenter("SYSTEM::DDE-SERVER-CALLBACK");
    k_connect = xlenter(":CONNECT");
    k_connect_confirm = xlenter(":CONNECT-CONFIRM");
    k_wildconnect = xlenter(":WILDCONNECT");
    k_disconnect = xlenter(":DISCONNECT");
    k_execute = xlenter(":EXECUTE");
    k_request = xlenter(":REQUEST");
    k_poke = xlenter(":POKE");
    k_text = xlenter(":TEXT");
    inited = TRUE;
  }

  if (fboundp(callbacksym)) {
    LVAL typesym, fmtsym;
    LVAL val = NIL;
    int success;
    int binary = TRUE;

    /* Standardize the arguments so they can be converted to Lisp.
       In particular, for transactions that do not use hdata, hdata is
       set to NULL in case Windows passes something else in the hope
       that it won't be used. */
    switch (type) {
    case XTYP_CONNECT:
      typesym = k_connect;
      fmt = 0;
      hdata = NULL;
      break;
    case XTYP_CONNECT_CONFIRM:
      typesym = k_connect_confirm;
      fmt = 0;
      hdata = NULL;
      break;
    case XTYP_WILDCONNECT:
      typesym = k_wildconnect;
      fmt = 0;
      hdata = NULL;
      break;
    case XTYP_DISCONNECT:
      typesym = k_disconnect;
      fmt = 0;
      hsz1 = NULL;
      hsz2 = NULL;
      hdata = NULL;
      break;
    case XTYP_EXECUTE:
      typesym = k_execute;
      fmt = 0;
      hsz2 = NULL;
      binary = FALSE;
      break;
    case XTYP_REQUEST:
      typesym = k_request;
      hdata = NULL;
      break;
    case XTYP_POKE:
      typesym = k_poke;
      break;
    default: typesym = NIL;
    }
    fmtsym = fmt ==  CF_TEXT ? k_text : NIL;

    /* Call the Lisp callback function.  This code traps any
       non-local exit attempts -- the unwind stops here. I think
       this is safe as far as XLISP is concerned; it is necessary
       for self sends to work properly.  */
    {
      CONTEXT cntxt;
      xlbegin(&cntxt, CF_UNWIND | CF_ERROR, NIL);
      if (XL_SETJMP(cntxt.c_jmpbuf))
	success = FALSE;
      else {
	FRAMEP newfp;
	int argc;
	LVAL olddenv = xldenv;
	
	xldbind(s_breakenable, NIL);
	newfp = xlsp;
	pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	pusharg(callbacksym);
	pusharg(NIL);  /* will become argc */
	argc = 0;
	pusharg(typesym); argc++;
	pusharg(fmtsym); argc++;
	pusharg(cvfixnum((FIXTYPE) hconv)); argc++;
	pusharg(StringHandle2LispString(hsz1)); argc++;
	pusharg(StringHandle2LispString(hsz2)); argc++;
	pusharg(DdeData2LispString(hdata, binary)); argc++;
	pusharg(cvfixnum((FIXTYPE) dw1)); argc++;
	pusharg(cvfixnum((FIXTYPE) dw2)); argc++;
	newfp[2] = cvfixnum((FIXTYPE)argc);
	xlfp = newfp;
	val = xlapply(argc);
	xlunbind(olddenv);
	success = TRUE;
      }
      xlend(&cntxt);
    }

    /* Build the result.  What is expected by DDE depends on the
       transaction type. */
    switch (type) {
    case XTYP_CONNECT:
      return (HDDEDATA) ((success && ! null(val)) ? TRUE : FALSE);
    case XTYP_CONNECT_CONFIRM:
      return NULL;
    case XTYP_WILDCONNECT:
      if (success) {
	LVAL tmp;
	HDDEDATA data = NULL;
	int n;
	for (n = 0, tmp = val;
	     consp(tmp) && consp(car(tmp)) && stringp(car(car(tmp))) &&
	       stringp(car(cdr(car(tmp))));
	     n++, tmp = cdr(tmp));
	if (n > 0) {
	  int offset;
	  HSZPAIR p;
	  data = DdeCreateDataHandle(ddeInst, NULL, (n+1) * sizeof(HSZPAIR),
				     0, NULL, 0, 0);
	  for (tmp = val, offset = 0; n-- > 0; tmp = cdr(tmp)) {
	    char *service = getstring(car(car(tmp)));
	    char *topic = getstring(car(cdr(car(tmp))));
	    p.hszSvc =  DdeCreateStringHandle(ddeInst, service, CP_WINANSI);
	    p.hszTopic =  DdeCreateStringHandle(ddeInst, topic, CP_WINANSI);
	    DdeAddData(data, (LPBYTE) &p, sizeof(p), offset);
	    offset += sizeof(p);
	  }
	  p.hszSvc = p.hszTopic = NULL;
	  DdeAddData(data, (LPBYTE) &p, sizeof(p), offset);
	}
	return data;
      }
      else return NULL;
    case XTYP_EXECUTE:
      if (null(val)) success = FALSE;
      return (HDDEDATA) (success ? DDE_FACK : DDE_FNOTPROCESSED);
    case XTYP_DISCONNECT:
      return NULL;
    case XTYP_REQUEST:
      if (fmt == CF_TEXT && success && stringp(val)) {
	unsigned char *p = (unsigned char *) getstring(val);
	int size = getslength(val) + 1;
	return DdeCreateDataHandle(ddeInst, p, size, 0, hsz2, CF_TEXT, 0);
      }
      else return NULL;
    case XTYP_POKE:
      if (null(val)) success = FALSE;
      return (HDDEDATA) (success ? DDE_FACK : DDE_FNOTPROCESSED);
    default:
      return NULL;
    }
  }
  else {
    switch (type) {
    case XTYP_CONNECT:
      if (! DdeCmpStringHandles(service, hsz2) &&
	  (! DdeCmpStringHandles(service, hsz1) ||
	   ! DdeCmpStringHandles(systopic, hsz1)))
	return (HDDEDATA) TRUE;
      else return FALSE;
    case XTYP_CONNECT_CONFIRM:
      return NULL;
    case XTYP_WILDCONNECT:
      {
	HSZPAIR p[3];
	p[0].hszSvc = service;
	p[0].hszTopic = service;
	p[1].hszSvc = service;
	p[1].hszTopic = systopic;
	p[2].hszSvc = NULL;
	p[2].hszTopic = NULL;
	return DdeCreateDataHandle(ddeInst, (LPBYTE) p, sizeof(p),
				   0, NULL, 0, 0);
      }
    case XTYP_EXECUTE:
      if (! DdeCmpStringHandles(service, hsz1) ||
	  ! DdeCmpStringHandles(systopic, hsz1)) {
	int success;
	char *p = (char *) DdeAccessData(hdata, NULL);
	CONTEXT cntxt;
	/* This code traps any non-local exit attempts -- the unwind
	 stops here. I think this is safe as far as XLISP is
	 concerned; it is necessary for self sends to work properly.  */
	xlbegin(&cntxt, CF_UNWIND | CF_ERROR, NIL);
	if (XL_SETJMP(cntxt.c_jmpbuf))
	  success = FALSE;
	else {
	  LVAL olddenv = xldenv;
	  xldbind(s_breakenable, NIL);
	  set_dde_result(hconv, readevalstream(string2stream(p)));
	  xlunbind(olddenv);
	  success = TRUE;
	}
	xlend(&cntxt);
	DdeUnaccessData(hdata);
	DdeFreeDataHandle(hdata);
	return (HDDEDATA) (success ? DDE_FACK : DDE_FNOTPROCESSED);
      }
      else return DDE_FNOTPROCESSED;
    case XTYP_DISCONNECT:
      delete_dde_result(hconv);
      return NULL;
    case XTYP_REQUEST:
      if (fmt != CF_TEXT)
	return NULL;
      else if (! DdeCmpStringHandles(service, hsz1) ||
	       ! DdeCmpStringHandles(systopic, hsz1)) {
	CONTEXT cntxt;
	LVAL val, stream;
	HDDEDATA result;

	DdeQueryString(ddeInst, hsz2, buf, STRMAX, CP_WINANSI);
	if (stricmp(buf, "VALUE"))
	  return NULL;

	/* This code traps any non-local exit attempts -- the unwind
	   stops here. I think this is safe as far as XLISP is
	   concerned; it is necessary for self sends to work properly.  */
	xlbegin(&cntxt, CF_UNWIND | CF_ERROR, NIL);
	if (XL_SETJMP(cntxt.c_jmpbuf))
	  result = NULL;
	else {
	  LVAL olddenv = xldenv;
	  xldbind(s_breakenable, NIL);
	  val = get_dde_result(hconv);
	  xlsave1(stream);
	  stream = newustream();
	  xlcallsubr2(xprin1, val, stream);
	  stream = xlcallsubr1(xgetstroutput, stream);
	  result = DdeCreateDataHandle(ddeInst,
				       (unsigned char *) getstring(stream),
				       getslength(stream) + 1, 0,
				       hsz2, CF_TEXT, 0);
	  xlpop();
	  xlunbind(olddenv);
	}
	xlend(&cntxt);
	return result;
      }
      else return NULL;
    case XTYP_POKE:
      return DDE_FNOTPROCESSED;
    default:
      return NULL;
    }
  }
}

/**** fix this up ****/
BOOL CALLBACK CloseEnumProc(HWND hWnd, LONG lParam)
#pragma argsused hWnd
{
  if (GetWindow(hWnd, GW_OWNER)) // check for icon title
    return(1);
  if (hWnd == hEditFrame)
    return(1);
  MDIRestoreWindow(GetParent(hWnd), hWnd);
  if (! SendMessage(hWnd, WM_QUERYENDSESSION, 0, 0))
    return(1);
  MDIDestroyWindow(GetParent(hWnd), hWnd);
  return(1);
}

LONG CALLBACK ListenerWndProc(HWND hWnd, UINT message, WPARAM wParam, LONG lParam)
{
  switch (message) {
  case WM_CREATE:
    return(0);
  case WM_SIZE:
    MoveWindow(hEditWnd, 0, 0, LOWORD(lParam), HIWORD(lParam), TRUE);
    break;
  case WM_SETFOCUS:
    SetFocus(hEditWnd);
    break;
  case WM_COMMAND:
    switch (GET_WM_COMMAND_ID(wParam, lParam)) {
    case IDM_UNDO:
      WarningBox("Command Not Implemented");
      break;
    case IDM_CUT:
      TTYSelToClip();
      TTYClearSel();
      break;
    case IDM_COPY:
      TTYSelToClip();
      break;
    case IDM_PASTE:
      TTYPasteFromClip();
      break;
    case IDM_CLEAR:
      TTYClearSel();
      break;
    case IDM_COPYPASTE:
      TTYSelToClip();
      TTYPasteFromClip();
      break;
    case IDC_EDIT:
      if (GET_WM_COMMAND_CMD(wParam, lParam) == EN_ERRSPACE) {
	TTYTrimBuffer();
      	xlfail("listener window out of memory");
      }
      break;
    case IDM_TOPLEVEL:
      xlsigint();
      break;
    }
    return(0);
  case WM_MDIACTIVATE:
    return(0);
  case WM_QUERYENDSESSION:
    break;
  case WM_CLOSE:
    ShowWindow(hWnd, SW_MINIMIZE);
    return(0);
  case WM_DESTROY:
    return(0);
  }
  return(DefMDIChildProc(hWnd, message, wParam, lParam));
}

static int handle_message(MSG *pmsg)
{
  if(! TranslateMDISysAccel(hWndClient, pmsg)
     && ! TranslateAccelerator(hWndFrame, hAccel, pmsg)) {
    TTYFlushOutput();
    TranslateMessage(pmsg);
    DispatchMessage(pmsg);
    if (TTYHasInput()) return TRUE;
    else return FALSE;
  }
  else return FALSE;
}

static void mainloop(void)
{
  MSG msg;

  while (1) {
    if (PeekMessage(&msg, 0, 0, 0, PM_REMOVE)) {
      if (handle_message(&msg)) break;
    }
    else {
      MSWDoIdleActions();
      MSWDoEventQueue();
    }
    if (! Exiting && ! MSWAnyIdleActions() && ! MSWAnyEventsQueued()
        && GetMessage(&msg, 0, 0, 0)) {
      if (handle_message(&msg)) break;
    }
    if (Exiting) xexit();
  }
}

void oscheck()
{
#ifdef DODO
  if (HIBIT(GetAsyncKeyState(VK_CONTROL))
      && HIBIT(GetAsyncKeyState(VK_CANCEL))) {
    FlushAllEvents();
    xlsigint();
  }
#endif /* DODO */
  MSG msg;

  if (PeekMessage(&msg, NULL, WM_KEYDOWN, WM_KEYDOWN, PM_NOREMOVE)) {
    while (PeekMessage(&msg, NULL, WM_KEYDOWN, WM_KEYDOWN, PM_REMOVE)) {
      TranslateAccelerator(hWndFrame, hAccel, &msg);
    }
  }
  if (null(getvalue(s_in_callback))) {
    while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE)) {
      if(! TranslateMDISysAccel(hWndClient, &msg) &&
         ! TranslateAccelerator(hWndFrame, hAccel, &msg)) {
        TTYFlushOutput();
        TranslateMessage(&msg);
        DispatchMessage(&msg);
      }
    }
  }
}

// access string resources created by spp
char *GetStringResource(int id)
{
  static char gsr_buffer[256];

  if (LoadString(hInst, id, (LPSTR)gsr_buffer, 255) < 1) {
    SysBeep(10);
    WarningBox("LoadString Error -- bailing out.");
    xexit();
  }
  return gsr_buffer;
}

static HSZ Lisp2StringHandle(LVAL str)
{
  if (null(str))
    return NULL;
  else {
    HSZ hsz;
    char *p;
    if (! stringp(str)) xlbadtype(str);
    p = getstring(str);
    hsz = DdeCreateStringHandle(ddeInst, p, CP_WINANSI);
    if (hsz == NULL && p[0] != 0)
      xlfail("string handle creation failed");
    return hsz;
  }
}
  
#ifndef xlgastrornil
#define xlgastrornil() (testarg(null(*xlargv) ? nextarg() : typearg(stringp)))
#endif

LVAL dde_name_service(void)
{
  LVAL servarg;
  HSZ service;
  int flag, success;

  servarg = xlgastrornil();
  flag = (! moreargs()) || (! null(xlgetarg())) ? 
    DNS_REGISTER : DNS_UNREGISTER;
  xllastarg();

  service = Lisp2StringHandle(servarg);
  success = DdeNameService(ddeInst, service, NULL, flag) ? TRUE : FALSE;
  if (service != NULL) DdeFreeStringHandle(ddeInst, service);
  return success ? s_true : NIL;
}


/***
 *** DDE Client Interface
 ***/

#define MAXCONV 30

static HCONV hconvs[MAXCONV] = { NULL };

static int find_conv_index(void)
{
  int i;
  for (i = 0; i < MAXCONV; i++)
    if (hconvs[i] == NULL)
      return i;
  return -1;
}

static int get_conversation_index(void)
{
  int hcd = (int) getfixnum(xlgafixnum());
  if (0 <= hcd && hcd < MAXCONV && hconvs[hcd] != NULL)
    return hcd;
  else
    return -1;
}

static void close_all_conversations(void)
{
  int i;
  for (i = 0; i < MAXCONV; i++) {
    if (hconvs[i] != NULL) {
      DdeDisconnect(hconvs[i]);
      hconvs[i] = NULL;
    }
  }
}

LVAL dde_services(void)
{
  LVAL arg1, arg2, val, entry, string;
  HSZ service = NULL, topic = NULL;
  HCONVLIST hcl;
  HCONV hconv;
  CONVINFO ci;
  CONTEXT cntxt,*target;
  int mask, jumping;

  arg1 = moreargs() ? xlgastrornil() : NIL;
  arg2 = moreargs() ? xlgastrornil() : NIL;
  xllastarg();

  xlbegin(&cntxt,CF_UNWIND,NIL);
  if (XL_SETJMP(cntxt.c_jmpbuf)) {
    jumping = TRUE;
    target = xltarget;
    mask = xlmask;
    val = xlvalue;
  }
  else {
    jumping = FALSE;
    service = Lisp2StringHandle(arg1);
    topic = Lisp2StringHandle(arg2);

    xlstkcheck(3);
    xlsave(val);
    xlsave(entry);
    xlsave(string);
    hconv = NULL;
    val = NIL;
    hcl = DdeConnectList(ddeInst, service, topic, 0, NULL);
    while ((hconv = DdeQueryNextServer(hcl, hconv)) != NULL) {
      DdeQueryConvInfo(hconv, QID_SYNC, &ci);
      string = StringHandle2LispString(ci.hszTopic);
      entry = consa(string);
      string = StringHandle2LispString(ci.hszSvcPartner);
      entry = cons(string, entry);
      val = cons(entry, val);    
    }
    xlpopn(3);
  }
  xlend(&cntxt);

  if (hcl != NULL) DdeDisconnectList(hcl);
  if (service != NULL) DdeFreeStringHandle(ddeInst, service);
  if (topic != NULL) DdeFreeStringHandle(ddeInst, topic);

  if (jumping) xljump(target,mask,val);
  
  return xlnreverse(val);
}

LVAL dde_connect(void)
{
  LVAL servarg, toparg;
  HSZ service, topic;
  HCONV hconv;
  int hcd;

  servarg = moreargs() ? xlgastrornil() : NIL;
  toparg = moreargs() ? xlgastrornil() : servarg;
  xllastarg();

  if ((hcd = find_conv_index()) < 0)
    xlfail("too many conversations in progress");

  service = Lisp2StringHandle(servarg);
  topic = Lisp2StringHandle(toparg);
  hconv = DdeConnect(ddeInst, service, topic, NULL);
  DdeFreeStringHandle(ddeInst, service);
  DdeFreeStringHandle(ddeInst, topic);

  if (hconv != NULL) {
    hconvs[hcd] = hconv;
    return cvfixnum((FIXTYPE) hcd);
  }
  else
    return NIL;
}

LVAL dde_client_transaction(void)
{
  LVAL tmp;
  char *data, *item;
  int hcd, binary;
  UINT type;
  DWORD timeout;
  static int inited = FALSE;
  static LVAL k_execute, k_poke, k_binary;

  if (! inited) {
    k_execute = xlenter(":EXECUTE");
    k_poke = xlenter(":POKE");
    k_binary = xlenter(":BINARY");
    inited = TRUE;
  }

  hcd = get_conversation_index();

  if (xlgetkeyarg(k_data, &tmp) && stringp(tmp))
    data = getstring(tmp);
  else
    data = NULL;

  if (xlgetkeyarg(k_type, &tmp)) {
    if (tmp == k_request)
      type = XTYP_REQUEST;
    else if (tmp == k_execute)
      type = XTYP_EXECUTE;
    else if (tmp == k_poke)
      type = XTYP_POKE;
    else
      xlbadtype(tmp);
  }
  else
    type = XTYP_EXECUTE;

  if (xlgetkeyarg(k_item, &tmp) && stringp(tmp))
    item = getstring(tmp);
  else
    item = NULL;

  if (xlgetkeyarg(k_timeout, &tmp) && fixp(tmp) && getfixnum(tmp) > 0)
    timeout = getfixnum(tmp);
  else
    timeout = 60000L;

  if (xlgetkeyarg(k_binary, &tmp) && tmp != NIL)
    binary = TRUE;
  else
    binary = FALSE;

  xllastkey();

  if (hcd >= 0) {
    CONTEXT cntxt,*target;
    int mask, jumping;
    LVAL val;
    HSZ hsz;
    HDDEDATA hdata;
    DWORD dsize = data != NULL ? strlen(data) + 1 : 0;
    
    if (item != NULL) hsz = DdeCreateStringHandle(ddeInst, item, CP_WINANSI);
    else hsz = NULL;

    xlbegin(&cntxt,CF_UNWIND,NIL);
    if (XL_SETJMP(cntxt.c_jmpbuf)) {
      jumping = TRUE;
      target = xltarget;
      mask = xlmask;
      val = xlvalue;
    }
    else {
      jumping = FALSE;
      hdata = DdeClientTransaction((LPVOID) data, dsize, hconvs[hcd], hsz,
				   CF_TEXT, type, timeout, NULL);
      if (type == XTYP_REQUEST)
	val = DdeData2LispString(hdata, binary);
      else val = hdata != NULL ? s_true : NIL;
    }
    xlend(&cntxt);

    if (hsz != NULL) DdeFreeStringHandle(ddeInst, hsz);

    /* if unwinding, continue unwinding */
    if (jumping) xljump(target,mask,val);

    return val;
  }
  else return NIL;
}

LVAL dde_disconnect(void)
{
  int hcd;
  if (moreargs()) {
    hcd = get_conversation_index();
    if (hcd >= 0) {
      DdeDisconnect(hconvs[hcd]);
      hconvs[hcd] = NULL;
      return s_true;
    }
    else
      return NIL;
  }
  else {
    LVAL val = NIL;
    for (hcd = 0; hcd < MAXCONV; hcd++) {
      if (hconvs[hcd] != NULL) {
        DdeDisconnect(hconvs[hcd]);
        hconvs[hcd] = NULL;
        val = s_true;
      }
    }
    return val;
  }
}

static void set_dde_result(HCONV hconv, LVAL val)
{
  LVAL s_dde_values = xlintern("*DDE-VALUES*", xlisppack);
  LVAL tmp1, tmp2, list;

  xlstkcheck(3);
  xlsave(tmp1);
  xlsave(tmp2);
  xlprotect(val);
  tmp1 = cvfixnum((FIXTYPE) hconv);
  list = boundp(s_dde_values) ? getvalue(s_dde_values) : NIL;
  tmp2 = xlcallsubr2(xassoc, tmp1, list);
  if (null(tmp2)) {
    tmp2 = consa(tmp1);
    setvalue(s_dde_values, cons(tmp2, list));
  }
  rplacd(tmp2, val);
  xlpopn(3);
}

static LVAL get_dde_result(HCONV hconv)
{
  LVAL s_dde_values = xlintern("*DDE-VALUES*", xlisppack);
  LVAL list, pair;
  list = boundp(s_dde_values) ? getvalue(s_dde_values) : NIL;
  pair = xlcallsubr2(xassoc, cvfixnum((FIXTYPE) hconv), list);
  return consp(pair) ? cdr(pair) : NIL;
}

static void delete_dde_result(HCONV hconv)
{
  LVAL s_dde_values = xlintern("*DDE-VALUES*", xlisppack);
  LVAL list, pair, tmp;

  tmp = cvfixnum((FIXTYPE) hconv);
  list = boundp(s_dde_values) ? getvalue(s_dde_values) : NIL;
  pair = xlcallsubr2(xassoc, tmp, list);
  setvalue(s_dde_values, xlcallsubr2(xdelete, pair, list));
}


/***
 *** User Profile Interface
 ***/

#define stringornil(x) (stringp(x) || null(x))
#define xlgastringornil() (testarg(typearg(stringornil)))

LVAL msw_get_profile_string()
{
  char *section, *item, *file;
  LVAL arg;

  section = getstring(xlgastring());
  item = getstring(xlgastring());
  file = ! null(arg = xlgastringornil()) ? getstring(arg) : NULL;
  xllastarg();

  if (file == NULL) {
    if (GetProfileString(section, item, "", buf, STRMAX))
      return cvstring(buf);
    else
      return NIL;
  }
  else {
    if (GetPrivateProfileString(section, item, "", buf, STRMAX, file))
      return cvstring(buf);
    else
      return NIL;
  }
}

LVAL msw_write_profile_string()
{
  char *section, *item, *value, *file;
  LVAL arg;

  section = ! null(arg = xlgastringornil()) ? getstring(arg) : NULL;
  item = ! null(arg = xlgastringornil()) ? getstring(arg) : NULL;
  value = ! null(arg = xlgastringornil()) ? getstring(arg) : NULL;
  file = ! null(arg = xlgastringornil()) ? getstring(arg) : NULL;
  xllastarg();

  if (file == NULL) {
    if (WriteProfileString(section, item, value))
      return s_true;
    else
      return NIL;
  }
  else {
    if (WritePrivateProfileString(section, item, value, file))
      return s_true;
    else
      return NIL;
  }
}
