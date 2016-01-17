#include <windows.h>

int APIENTRY DllMain(HANDLE hdll, DWORD  reason, LPVOID reserved )
{
  switch( reason ) {
  case DLL_THREAD_ATTACH: break;
  case DLL_THREAD_DETACH: break;
  case DLL_PROCESS_ATTACH: break;
  case DLL_PROCESS_DETACH: break;
  }
  return( 1 );
}
