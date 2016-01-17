#include <windows.h>
#include <windowsx.h>
#include <string.h>
#include <stdio.h>
#ifdef WIN32
#include <commdlg.h>
#endif /* WIN32 */
#include "winutils.h"
#include <dos.h>
#ifdef WIN32
#include "wxlisp.h"
#endif /* WIN32 */

#ifdef MinGW32
void Delay(unsigned n)
{
  Sleep(n);
}
#else
static double dbltime(struct time *tm)
{
  return(3600.0*tm->ti_hour+60.0*tm->ti_min+tm->ti_sec+0.01*tm->ti_hund);
}

void Delay(unsigned n)
{
  double first, second;
  struct time tm;

  gettime(&tm);
  first = 1000.0 * dbltime(&tm);
  do {
    gettime(&tm);
    second = 1000.0 * dbltime(&tm);
  } while (second - first < n);
}
#endif

void SysBeep(int n)
{
  for (; n > 0; n -= 10)
    MessageBeep(0);
}

void FlushAllEvents(void)
{
  MSG msg;

  while (PeekMessage(&msg, NULL, WM_KEYFIRST, WM_MOUSELAST, PM_REMOVE | PM_NOYIELD));
}

int WarningBox(char *msg)
{
  SysBeep(10);
  return(MessageBox(GetFocus(),
		    (LPSTR) msg,
		    "XLISP-STAT",
		    MB_ICONASTERISK | MB_OK));
}

int OKorCancelBox(char *msg)
{
  return(MessageBox(GetFocus(),
		    (LPSTR) msg,
		    "XLISP-STAT",
		    MB_ICONQUESTION | MB_OKCANCEL));
}

#ifdef WIN32
/****************************************************************************
*
*    FUNCTION: ProcessCDError(DWORD)
*
*    PURPOSE:  Processes errors from the common dialog functions.
*
*    COMMENTS:
*
*        This function is called whenever a common dialog function
*        fails.  The CommonDialogExtendedError() value is passed to
*        the function which maps the error value to a string table.
*        The string is loaded and displayed for the user.
*
*    RETURN VALUES:
*        void.
*
****************************************************************************/
void ProcessCDError(DWORD dwErrorCode, HWND hWnd)
{
  WORD  wStringID;
  char  buf[MAX_PATH];

  switch (dwErrorCode) {
  case CDERR_STRUCTSIZE:      wStringID=IDS_STRUCTSIZE;      break;
  case CDERR_INITIALIZATION:  wStringID=IDS_INITIALIZATION;  break;
  case CDERR_NOTEMPLATE:      wStringID=IDS_NOTEMPLATE;      break;
  case CDERR_NOHINSTANCE:     wStringID=IDS_NOHINSTANCE;     break;
  case CDERR_LOADSTRFAILURE:  wStringID=IDS_LOADSTRFAILURE;  break;
  case CDERR_FINDRESFAILURE:  wStringID=IDS_FINDRESFAILURE;  break;
  case CDERR_LOADRESFAILURE:  wStringID=IDS_LOADRESFAILURE;  break;
  case CDERR_LOCKRESFAILURE:  wStringID=IDS_LOCKRESFAILURE;  break;
  case CDERR_MEMALLOCFAILURE: wStringID=IDS_MEMALLOCFAILURE; break;
  case CDERR_MEMLOCKFAILURE:  wStringID=IDS_MEMLOCKFAILURE;  break;
  case CDERR_NOHOOK:          wStringID=IDS_NOHOOK;          break;
  case PDERR_PARSEFAILURE:    wStringID=IDS_PARSEFAILURE;    break;
  case PDERR_RETDEFFAILURE:   wStringID=IDS_RETDEFFAILURE;   break;
  case PDERR_LOADDRVFAILURE:  wStringID=IDS_LOADDRVFAILURE;  break;
  case PDERR_GETDEVMODEFAIL:  wStringID=IDS_GETDEVMODEFAIL;  break;
  case PDERR_INITFAILURE:     wStringID=IDS_INITFAILURE;     break;
  case PDERR_NODEVICES:       wStringID=IDS_NODEVICES;       break;
  case PDERR_NODEFAULTPRN:    wStringID=IDS_NODEFAULTPRN;    break;
  case PDERR_DNDMMISMATCH:    wStringID=IDS_DNDMMISMATCH;    break;
  case PDERR_CREATEICFAILURE: wStringID=IDS_CREATEICFAILURE; break;
  case PDERR_PRINTERNOTFOUND: wStringID=IDS_PRINTERNOTFOUND; break;
  case CFERR_NOFONTS:         wStringID=IDS_NOFONTS;         break;
  case FNERR_SUBCLASSFAILURE: wStringID=IDS_SUBCLASSFAILURE; break;
  case FNERR_INVALIDFILENAME: wStringID=IDS_INVALIDFILENAME; break;
  case FNERR_BUFFERTOOSMALL:  wStringID=IDS_BUFFERTOOSMALL;  break;

  default:
    wStringID=IDS_UNKNOWNERROR;
    break;

  case 0:     /* user might have clicked Cancel, */
    return;   /* or this is a very random error  */
  }

  LoadString(NULL, wStringID, buf, sizeof(buf));
  MessageBox(hWnd, buf, NULL, MB_OK);
  return;
}


/****************************************************************************
*
*    FUNCTION: PageSetup(HWND)
*
*    PURPOSE:  Invokes Page Setup common dialog function.
*
*    COMMENTS:
*
*        This function initializes the PAGESETUPDLG structure for all modes
*        possible: standard, using a hook or using a customized template.
*
*    RETURN VALUES:
*        void.
*
****************************************************************************/
void PageSetup( HWND hWnd, PAGESETUPDLG *psDlg, BOOL bGetOnly )
{
  /* Initialize the PAGESETUPDLG structure. */
  if (psDlg->lStructSize == 0) {
    psDlg->lStructSize = sizeof(PAGESETUPDLG);
    psDlg->hwndOwner = hWnd;
    psDlg->hDevMode = (HANDLE)NULL;
    psDlg->hDevNames = (HANDLE)NULL;
    psDlg->hInstance = (HANDLE)GetWindowLong(hWnd, GWL_HINSTANCE);
    psDlg->lCustData = (LPARAM)NULL;
    psDlg->hPageSetupTemplate = (HGLOBAL)NULL;
    psDlg->Flags = PSD_DEFAULTMINMARGINS;
  }
  else
    psDlg->Flags = PSD_MARGINS;

  if (bGetOnly)
    psDlg->Flags |= PSD_RETURNDEFAULT;
  else
    psDlg->Flags &= ~PSD_RETURNDEFAULT;

#ifdef IDM_STANDARD
  switch (wMode) {
  case IDM_STANDARD:
    psDlg->lpfnPageSetupHook = (LPPAGESETUPHOOK)(FARPROC)NULL;
    psDlg->lpPageSetupTemplateName = (LPTSTR)NULL;
    psDlg->lpfnPagePaintHook = (LPPAGEPAINTHOOK)(FARPROC)NULL;
    break;

  case IDM_HOOK:
    psDlg->Flags |= PSD_ENABLEPAGESETUPHOOK;
    psDlg->lpfnPageSetupHook = (LPPAGESETUPHOOK)(FARPROC)PageSetupHook;
    psDlg->lpPageSetupTemplateName = (LPTSTR)NULL;
    psDlg->lpfnPagePaintHook = (LPPAGEPAINTHOOK)(FARPROC)NULL;
    break;

  case IDM_CUSTOM:
    psDlg->Flags |= PSD_ENABLEPAGESETUPHOOK | PSD_ENABLEPAGESETUPTEMPLATE;
    psDlg->lpfnPageSetupHook = (LPPAGESETUPHOOK)(FARPROC)PageSetupHook;
    psDlg->lpPageSetupTemplateName = (LPTSTR)PRNSETUPDLGORD95;
    psDlg->lpfnPagePaintHook = (LPPAGEPAINTHOOK)(FARPROC)NULL;
    break;

  }
#else
  psDlg->lpfnPageSetupHook = (LPPAGESETUPHOOK)(FARPROC)NULL;
  psDlg->lpPageSetupTemplateName = (LPTSTR)NULL;
  psDlg->lpfnPagePaintHook = (LPPAGEPAINTHOOK)(FARPROC)NULL;
#endif

  /* Call the Page Setup common dialog procedure. */
  if (PageSetupDlg(psDlg) == FALSE)
    ProcessCDError(CommDlgExtendedError(), hWnd );
}


/****************************************************************************
*
*    FUNCTION: PrintDialog(HWND, PRINTDLG *)
*
*    PURPOSE:  Invokes common dialog function to print.
*
*    COMMENTS:
*
*        This function initializes the PRINTDLG structure for all modes
*        possible: standard, using a hook or using a customized template.
*        When hook mode is chosen, a hook is installed for both the
*        Print dialog and the Print Setup dialog.  When custom mode is
*        chosen, the templates are enabled for both the print dialog and
*        the Print Setup dialog boxes.
*
*
*    RETURN VALUES:
*        TRUE if successful.
*
****************************************************************************/
int PrintDialog(HWND hWnd, PRINTDLG *pd, HANDLE hDevMode, HANDLE hDevNames )
{
  /* Initialize PRINTDLG structure. */
  pd->lStructSize = sizeof(PRINTDLG);
  pd->hwndOwner = hWnd;
  pd->hDevMode = hDevMode;
  pd->hDevNames = hDevNames;
  pd->nFromPage = 1;
  pd->nToPage = 0;
  pd->nMinPage = 0;
  pd->nMaxPage = 0;
  pd->nCopies = 0;
  pd->hInstance = (HANDLE)GetWindowLong(hWnd, GWL_HINSTANCE);
  pd->Flags = PD_RETURNDC | PD_USEDEVMODECOPIESANDCOLLATE | PD_NOSELECTION | PD_NOPAGENUMS | PD_ALLPAGES;

#ifdef IDM_STANDARD
  switch (wMode) {
  case IDM_STANDARD:
    pd->lpfnSetupHook = (LPSETUPHOOKPROC)(FARPROC)NULL;
    pd->lpSetupTemplateName = (LPTSTR)NULL;
    pd->lpfnPrintHook = (LPPRINTHOOKPROC)(FARPROC)NULL;
    pd->lpPrintTemplateName = (LPTSTR)NULL;
    break;

  case IDM_HOOK:
    pd->Flags |= PD_ENABLEPRINTHOOK | PD_ENABLESETUPHOOK;
    pd->lpfnSetupHook = (LPSETUPHOOKPROC)PrintSetupHookProc;
    pd->lpSetupTemplateName = (LPTSTR)NULL;
    pd->lpfnPrintHook = (LPPRINTHOOKPROC)PrintDlgHookProc;
    pd->lpPrintTemplateName = (LPTSTR)NULL;
    break;

  case IDM_CUSTOM:
    pd->Flags |= PD_ENABLEPRINTHOOK | PD_ENABLEPRINTTEMPLATE |
      PD_ENABLESETUPHOOK | PD_ENABLESETUPTEMPLATE;
    pd->lpfnSetupHook = (LPSETUPHOOKPROC)PrintSetupHookProc;
    pd->lpfnPrintHook = (LPPRINTHOOKPROC)PrintDlgHookProc;
    pd->lpPrintTemplateName = (LPTSTR)MAKEINTRESOURCE(PRINTDLGORD);
    pd->lpSetupTemplateName = (LPTSTR)MAKEINTRESOURCE(PRNSETUPDLGORD);
    break;
  }
#else
  pd->lpfnSetupHook = (LPSETUPHOOKPROC)(FARPROC)NULL;
  pd->lpSetupTemplateName = (LPTSTR)NULL;
  pd->lpfnPrintHook = (LPPRINTHOOKPROC)(FARPROC)NULL;
  pd->lpPrintTemplateName = (LPTSTR)NULL;
#endif

  /* Print if successful. */
  if (PrintDlg(pd) != 0)
    return TRUE;
  else {
    ProcessCDError(CommDlgExtendedError(), hWnd );
    return FALSE;
  }
}
#endif /* WIN32 */
