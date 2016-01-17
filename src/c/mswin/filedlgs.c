// *** merge with lspedit code

#include "xlisp.h"
#include <commdlg.h>
#include <dir.h>

#define FILTERSIZE 255
static char szFilter[FILTERSIZE + 2];
static char szDfltFilter[] = "Lisp Files(*.LSP)|*.lsp|All Files(*.*)|*.*";
static char szDirName[256];

LVAL xsopenfiledialog()
{
  int i, n;
  OPENFILENAME ofn;
  BOOL changedir;
  LVAL usrfilter;

  changedir = moreargs() ? ! null(xlgetarg()) : TRUE;
  if (moreargs()) {
    usrfilter = xlgastring();
    if (getslength(usrfilter) > FILTERSIZE)
      xlbadtype(usrfilter);
    strcpy(szFilter, getstring(usrfilter));
  }
  else
    strcpy(szFilter, szDfltFilter);

  n = strlen(szFilter);
  for (i = 0; i < n; i++)
    if (szFilter[i] == '|')
      szFilter[i] = '\0';
  szFilter[n] = '\0';
  szFilter[n + 1] = '\0';

  if (! getcwd(szDirName, sizeof(szDirName)))
    return NIL;
  buf[0] = '\0';

  memset(&ofn, 0, sizeof(OPENFILENAME));
  ofn.lStructSize = sizeof(OPENFILENAME);
  ofn.lpstrFilter = szFilter;
  ofn.nFilterIndex = 1;
  ofn.lpstrFile = buf;
  ofn.nMaxFile = STRMAX;
  ofn.lpstrInitialDir = szDirName;
  ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;
  if (! changedir) ofn.Flags |= OFN_NOCHANGEDIR;

  if (GetOpenFileName(&ofn))
    return cvstring(buf);
  else
    return NIL;
}

LVAL xssetfiledialog()
{
  int i, n;
  OPENFILENAME ofn;
  BOOL changedir;
  LVAL usrfilter;
  char *title;

  title = getstring(xlgastring());
  changedir = moreargs() ? ! null(xlgetarg()) : TRUE;
  if (moreargs()) {
    usrfilter = xlgastring();
    if (getslength(usrfilter) > FILTERSIZE)
      xlbadtype(usrfilter);
    strcpy(szFilter, getstring(usrfilter));
  }
  else
    strcpy(szFilter, szDfltFilter);

  n = strlen(szFilter);
  for (i = 0; i < n; i++)
    if (szFilter[i] == '|')
      szFilter[i] = '\0';
  szFilter[n] = '\0';
  szFilter[n + 1] = '\0';

  if (! getcwd(szDirName, sizeof(szDirName)))
    return NIL;
  buf[0] = '\0';

  memset(&ofn, 0, sizeof(OPENFILENAME));
  ofn.lStructSize = sizeof(OPENFILENAME);
  ofn.lpstrFilter = szFilter;
  ofn.nFilterIndex = 1;
  ofn.lpstrFile = buf;
  ofn.nMaxFile = STRMAX;
  ofn.lpstrInitialDir = szDirName;
  ofn.Flags = OFN_OVERWRITEPROMPT;
  if (! changedir) ofn.Flags |= OFN_NOCHANGEDIR;
  ofn.lpstrTitle = title;

  if (GetSaveFileName(&ofn))
    return cvstring(buf);
  else
    return NIL;
}
