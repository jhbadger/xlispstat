/***************************************************************************/
/***************************************************************************/
/**                                                                       **/
/**                       Simple Lisp File Editor                         **/
/**      Adapted from examples in MS Windows Guide to Programming         **/
/**                                                                       **/
/***************************************************************************/
/***************************************************************************/
// clean up file opening, closing, fstat, etc.
// try to allow for multiple windows
// search facility

#include <windows.h>
#include <windowsx.h>
#include <commdlg.h>
#include <dir.h>
#include <ddeml.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <io.h>
#include <sys\types.h>
#include "winutils.h"
#include "ledit.h"
#include "lspedit.h"

HINSTANCE hInst;
HWND hWnd;
HWND hEditWnd;
HACCEL hAccTable;

#define NAMESIZE 129
char FileName[NAMESIZE];
BOOL bChanges = FALSE;

char buf[255];

HCURSOR hHourGlass;
char Untitled[] = "LSPEdit - (untitled)";

DWORD ddeInst;

static void OpenInitialFile(char *);
static BOOL OpenDlg(void);
static BOOL SaveAsDlg(void);
static void ReanInFile(FILE *);
static BOOL InitApplication(HINSTANCE);
static BOOL InitInstance(HINSTANCE, int);
static BOOL SaveFile(void);
static BOOL QuerySaveFile(void);
static void SetNewBuffer(HANDLE, PSTR);

#define OkOrCancelBox(s) \
  MessageBox(GetFocus(), s, "LSPEdit", MB_YESNOCANCEL | MB_ICONEXCLAMATION)

int PASCAL WinMain(HINSTANCE hInstance,
		   HINSTANCE hPrevInstance,
		   LPSTR lpCmdLine,
		   int nCmdShow)
{
  MSG msg;

  /* Try to cooperate with other running instances of the application */
  if (!hPrevInstance)
    if (!InitApplication(hInstance))
      return(FALSE);

  /* initialize the specific instance */
  if (!InitInstance(hInstance, nCmdShow))
    return(FALSE);

  /* initialize DDEML */
  /**** ought to check thisis available */
  DdeInitialize(&ddeInst, NULL,
                APPCLASS_STANDARD | APPCMD_CLIENTONLY |
                CBF_SKIP_ALLNOTIFICATIONS,
                0L);

  /* open an initial file if there is a command line argument */
#if defined(WIN32) && defined(__BORLANDC__) && (__BORLANDC__ <= 0x452)
  {
    int i;
    if (sscanf(lpCmdLine, "%128s%n", buf, &i) == 1)
      lpCmdLine += i;
  }
#endif
  if (sscanf(lpCmdLine, "%128s", buf) == EOF)
    buf[0] = '\0';
  OpenInitialFile(buf);

  /* process messages until a WM_QUIT message */
  while (GetMessage(&msg, NULL, 0, 0)) {
    if (! TranslateAccelerator(hWnd, hAccTable, &msg)) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }

  /* drop DDEML */
  DdeUninitialize(ddeInst);

  return(msg.wParam);
}

BOOL InitApplication(HINSTANCE hInstance)
{
  WNDCLASS wc;

  /* fill in the class structure for the main window */
  wc.style = 0;
  wc.lpfnWndProc = MainWndProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = hInstance;
  wc.hIcon = LoadIcon(hInstance, "LEditIcon");
  wc.hCursor = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground = GetStockObject(WHITE_BRUSH);
  wc.lpszMenuName = "LSPEditMenu";
  wc.lpszClassName = "LSPEditWClass";

  /* register the window class and return result code */
  return(RegisterClass(&wc));
}

BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
  hInst = hInstance;

  _fmode = O_BINARY;

  hHourGlass = LoadCursor(hInstance, IDC_WAIT);
  InitLEditClass(NULL, NULL);

  /* create a main window for this instance */
  hWnd = CreateWindow("LSPEditWClass",
		      Untitled,
		      WS_OVERLAPPEDWINDOW,
		      CW_USEDEFAULT,
		      CW_USEDEFAULT,
		      CW_USEDEFAULT,
		      CW_USEDEFAULT,
		      NULL,
		      NULL,
		      hInstance,
		      NULL);

  hEditWnd = CreateLEditWindow(hWnd, (HMENU) IDC_EDIT, hInstance);
  if (! hEditWnd) {
    DestroyWindow(hWnd);
    return(FALSE);
  }

  hAccTable = LoadAccelerators(hInst, "LSPEdit");

  /* if the window could not be created, return failure code */
  if (! hWnd) return(FALSE);

  /* make the window visible, update it, and return success */
  ShowWindow(hWnd, nCmdShow);
  UpdateWindow(hWnd);
  return(TRUE);
}

LONG WINAPI MainWndProc(HWND hWnd, UINT message, WPARAM wPrm, LONG lPrm)
{
  DLGPROC lpProcAbout;
  FILE *fp;

  switch(message) {
  case WM_COMMAND:
    switch (GET_WM_COMMAND_ID(wPrm, lPrm)) {
    case IDM_ABOUT:
      lpProcAbout = (DLGPROC) MakeProcInstance((FARPROC) About, hInst);
      DialogBox(hInst, "AboutBox", hWnd, lpProcAbout);
      (void) FreeProcInstance((FARPROC) lpProcAbout);
      break;
    case IDM_NEW:
      if (! QuerySaveFile()) return(FALSE);
      bChanges = FALSE;
      FileName[0] = 0;
      SetNewBuffer(NULL, Untitled);
      break;
    case IDM_OPEN:
      if (! QuerySaveFile())
        return FALSE;
      if (! OpenDlg() || (fp = fopen(FileName, "r")) == NULL)
        return FALSE;
      ReanInFile(fp);
      return FALSE;
    case IDM_SAVE:
      if (! FileName[0]) goto saveas;
      if (bChanges) SaveFile();
      break;
    case IDM_SAVEAS:
saveas:
      if (SaveAsDlg()) {
	sprintf(buf, "LSPEdit - %s", FileName);
	if (SaveFile())
          SetWindowText(hWnd, buf);
        else {
          FileName[0] = 0;
          SetWindowText(hWnd, Untitled);
        }
      }
      break;
    case IDM_PRINT:
      WarningBox("Command Not Implemented");
      break;
    case IDM_EXIT:
      if (! QuerySaveFile()) return FALSE;
      DestroyWindow(hWnd);
      break;
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
    case IDM_EVAL:
      {
        HSZ service, topic;
        HCONV hconv;

        service = DdeCreateStringHandle(ddeInst, "XLISP-STAT", CP_WINANSI);
        topic = DdeCreateStringHandle(ddeInst, "XLISP-STAT", CP_WINANSI);
        if ((hconv = DdeConnect(ddeInst, service, topic, NULL)) == NULL)
          WarningBox("Can't connect to XLISP-STAT");
        else {
          /**** switch to allocated buffer? */
          char *data = TTYSelectionStr();
          if (! DdeClientTransaction((LPVOID) data, strlen(data) + 1,
                                     hconv, NULL, CF_TEXT, XTYP_EXECUTE,
                                     60000L, NULL))
            WarningBox("Transaction failed");
          DdeDisconnect(hconv);
        }
        DdeFreeStringHandle(ddeInst, service);
        DdeFreeStringHandle(ddeInst, topic);
      }
      break;
    case IDC_EDIT:
      if (GET_WM_COMMAND_CMD(wPrm, lPrm) == EN_ERRSPACE) {
	WarningBox("Out of memory");
      }
      if (GET_WM_COMMAND_CMD(wPrm, lPrm) == EN_CHANGE) bChanges = TRUE;
      break;
    default: return(DefWindowProc(hWnd, message, wPrm, lPrm));
    }
    break;
  case WM_SETFOCUS:
    SetFocus(hEditWnd);
    break;
  case WM_SIZE:
    MoveWindow(hEditWnd, 0, 0, LOWORD(lPrm), HIWORD(lPrm), TRUE);
    break;
  case WM_QUERYENDSESSION:
    return(QuerySaveFile());
  case WM_CLOSE:
    if (QuerySaveFile()) DestroyWindow(hWnd);
    break;
  case WM_DESTROY:
    PostQuitMessage(0);
    break;
  default:
    return(DefWindowProc(hWnd, message, wPrm, lPrm));
  }
  return FALSE;
}

BOOL CALLBACK About(HWND hDlg, UINT message, WPARAM wPrm, LONG lPrm)
#pragma argsused hDlg message wPrm
{
  UINT which;

  switch(message) {
  case WM_INITDIALOG:
    return(TRUE);
  case WM_COMMAND:
    which = GET_WM_COMMAND_ID(wPrm, lPrm);
    if (which == IDOK || which == IDCANCEL) {
      EndDialog(hDlg, TRUE);
      return(TRUE);
    }
    break;
  }
  return(FALSE);
}

#define FILTERSIZE 255
#define STRMAX 255
static char szFilter[FILTERSIZE + 2];
static char szDfltFilter[] = "Lisp Files(*.LSP)|*.lsp|All Files(*.*)|*.*";
static char szDirName[256];

static BOOL OpenDlg(void)
{
  int i, n;
  OPENFILENAME ofn;

  strcpy(szFilter, szDfltFilter);

  n = strlen(szFilter);
  for (i = 0; i < n; i++)
    if (szFilter[i] == '|')
      szFilter[i] = '\0';
  szFilter[n] = '\0';
  szFilter[n + 1] = '\0';

  if (! getcwd(szDirName, sizeof(szDirName)))
    return FALSE;
  buf[0] = '\0';

  memset(&ofn, 0, sizeof(OPENFILENAME));
  ofn.lStructSize = sizeof(OPENFILENAME);
  ofn.lpstrFilter = szFilter;
  ofn.nFilterIndex = 1;
  ofn.lpstrFile = buf;
  ofn.nMaxFile = STRMAX;
  ofn.lpstrFileTitle = FileName;
  ofn.nMaxFileTitle = sizeof(FileName);
  ofn.lpstrInitialDir = szDirName;
  ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;

  if (GetOpenFileName(&ofn))
    return TRUE;
  else
    return FALSE;
}

static BOOL SaveAsDlg(void)
{
  int i, n;
  OPENFILENAME ofn;

  strcpy(szFilter, szDfltFilter);

  n = strlen(szFilter);
  for (i = 0; i < n; i++)
    if (szFilter[i] == '|')
      szFilter[i] = '\0';
  szFilter[n] = '\0';
  szFilter[n + 1] = '\0';

  if (! getcwd(szDirName, sizeof(szDirName)))
    return FALSE;
  buf[0] = '\0';

  memset(&ofn, 0, sizeof(OPENFILENAME));
  ofn.lStructSize = sizeof(OPENFILENAME);
  ofn.lpstrFilter = szFilter;
  ofn.nFilterIndex = 1;
  ofn.lpstrFile = buf;
  ofn.nMaxFile = STRMAX;
  ofn.lpstrFileTitle = FileName;
  ofn.nMaxFileTitle = sizeof(FileName);
  ofn.lpstrInitialDir = szDirName;
  ofn.Flags = OFN_OVERWRITEPROMPT;

  if (GetSaveFileName(&ofn))
    return TRUE;
  else
    return FALSE;
}

static BOOL SaveFile(void)
{
  BOOL bSuccess;
  HANDLE hEditBuffer;
  HCURSOR hSaveCursor;
  PSTR pEditBuffer;
  int IOStatus, length;
  FILE *fp;

  if ((fp = fopen(FileName, "w")) == NULL) {
    sprintf(buf, "Cannot write to %s.", FileName);
    WarningBox(buf);
    return(FALSE);
  }
  hSaveCursor = SetCursor(hHourGlass);
  length = Edit_GetTextLength(hEditWnd);
  hEditBuffer = LocalAlloc(LMEM_MOVEABLE | LMEM_ZEROINIT, length + 1);
  if (! hEditBuffer) {
    WarningBox("Not enough memory.");
    return FALSE;
  }
  pEditBuffer = LocalLock(hEditBuffer);
  GetWindowText(hEditWnd, pEditBuffer, length + 1);
  IOStatus = fwrite(pEditBuffer, 1, length, fp);
  fclose(fp);
  SetCursor(hSaveCursor);
  if (IOStatus != length) {
    sprintf(buf, "Error writing to %s.", FileName);
    WarningBox(buf);
    bSuccess = FALSE;
  }
  else {
    bSuccess = TRUE;
    bChanges = FALSE;
  }
  LocalUnlock(hEditBuffer);
  LocalFree(hEditBuffer);
  return(bSuccess);
}

BOOL QuerySaveFile()
{
  int Response;

  if (bChanges) {
    sprintf(buf, "Save current changes: %s", FileName);
    Response = OkOrCancelBox(buf);
    if (Response == IDYES) {
      if (! SaveAsDlg())
        return FALSE;
      else
        return SaveFile();
    }
    else if (Response == IDCANCEL)
      return FALSE;
  }
  return(TRUE);
}

void SetNewBuffer(HANDLE hNewBuffer, PSTR Title)
{
  if (hNewBuffer) {
    char *p = LocalLock(hNewBuffer);
    SetWindowText(hEditWnd, p);
    LocalUnlock(hNewBuffer);
  }
  else
    SetWindowText(hEditWnd, "");

  SetWindowText(hWnd, Title);
  bChanges = FALSE;
}

static void OpenInitialFile(char *name)
{
  FILE *fp;

  if (strchr(name, '*') || strchr(name, '?')) name[0] = 0;
  if (! name[0]) return;
  if (! strrchr(name, '.'))
    strcat(name, ".lsp");
  if ((fp = fopen(name, "r")) == NULL) {
    sprintf(buf, "Error opening %s.", name);
    WarningBox(buf);
  }
  else {
    strcpy(FileName, name);
    ReanInFile(fp);
  }
}

static void ReanInFile(FILE *fp)
{
  HCURSOR hSaveCursor;
  unsigned long IOStatus, FileSize;
  HANDLE hEditBuffer;
  PSTR pEditBuffer;

  FileSize = (size_t) filelength(fileno(fp));
  if (FileSize > MAXFILESIZE) {
    sprintf(buf, "Not enough memory to load %s.\n%s exceeds %d bytes",
            FileName, FileName, MAXFILESIZE);
	    WarningBox(buf);
      	    return;
  }
  hEditBuffer = LocalAlloc(LMEM_MOVEABLE | LMEM_ZEROINIT,
                           (size_t) FileSize + 1);
  if (! hEditBuffer) {
    WarningBox("Not enough memory.");
    return;
  }
  hSaveCursor = SetCursor(hHourGlass);
  pEditBuffer = LocalLock(hEditBuffer);
  IOStatus = fread(pEditBuffer, 1, (size_t) FileSize, fp);
  fclose(fp);
  if (IOStatus != FileSize) {
    sprintf(buf, "Error reading %s.", FileName);
    SetCursor(hSaveCursor);
    WarningBox(buf);
    pEditBuffer[0] = '\0';
    FileName[0] = '\0';
  }
  LocalUnlock(hEditBuffer);
  sprintf(buf, "LSPEdit - %s", FileName);
  SetNewBuffer(hEditBuffer, buf);
  LocalFree(hEditBuffer);
  SetCursor(hSaveCursor);
}
