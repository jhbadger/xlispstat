void Delay(unsigned);
void SysBeep(int);
void FlushAllEvents(void);
int OKorCancelBox(char *);
int WarningBox(char *);
#ifdef WIN32
int PrintDialog(HWND hWnd, PRINTDLG *pd, HANDLE hDevMode, HANDLE hDevNames);
#endif

#define HIBIT(x) ((x) & 0x8000)
