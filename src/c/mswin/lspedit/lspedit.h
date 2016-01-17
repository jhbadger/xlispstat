#define IDM_ABOUT 100

#define IDM_NEW		101
#define IDM_OPEN	102
#define	IDM_SAVE	103
#define IDM_SAVEAS	104
#define IDM_PRINT	105
#define IDM_EXIT	106

#define IDM_UNDO	200
#define IDM_CUT		201
#define	IDM_COPY	202
#define IDM_PASTE	203
#define IDM_CLEAR	204
#define IDM_EVAL	205

#define IDC_EDIT	300

#define IDC_FILENAME	400
#define IDC_EDITNAME	401
#define IDC_FILES	402
#define IDC_PATH	403
#define IDC_LISTBOX	404

#define MAXFILESIZE 31000 /**** about max a win16 edit control can handle */

#ifndef RC_INVOKED
int WINAPI	WinMain(HINSTANCE, HINSTANCE, LPSTR, int);
LONG CALLBACK	MainWndProc(HWND, UINT, WPARAM, LONG);
BOOL CALLBACK	About(HWND, UINT, WPARAM, LONG);
#endif
