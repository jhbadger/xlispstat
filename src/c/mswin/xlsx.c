#include <windows.h>
#include <Windowsx.h>

#ifdef WIN32
BOOL WINAPI DllEntryPoint(HINSTANCE hInst, DWORD reason, LPVOID rsrvd)
#pragma argsused
{
  return TRUE;
}
#else
int FAR PASCAL LibMain(HINSTANCE hInst, WORD wDS, WORD wHS, LPSTR lpszCmd)
#pragma argsused wHS
{
  if (wHS > 0) UnlockData(0);
  return(1);
}

int FAR PASCAL WEP(int nParam)
#pragma argsused
{
  return(1);
}
#endif

