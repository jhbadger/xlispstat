#ifndef XLGRAPH_H
#define XLGRAPH_H

/* xlgraph.h - External declarations and defines for XLISP-STAT.       */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#ifdef MACINTOSH
/* macstuff.c */
typedef struct {
  short vRefNum;
  unsigned long parID;
} DirSpec;

typedef struct {
  char name[256];
  OSType type;
  DirSpec dir;
} FileSpec;

extern DirSpec default_dir;
extern short editFontNum, editFontSize;
extern short listenerFontNum, listenerFontSize;
extern short graphFontNum, graphFontSize;
extern Style editFontStyle, listenerFontStyle, graphFontStyle;

extern OSErr GetDir(DirSpec *dir);
extern OSErr SetDir(DirSpec *dir);

/* editwindows.c */
extern VOID set_edit_window_procs _((WindowPtr w));
extern VOID adjust_insert _((WindowPtr w));
extern VOID return_action _((TEHandle te));
extern VOID TtyPutc _((int c));
extern VOID TtyPrint _((char *s));
extern VOID TtyFlush _((void));
extern VOID make_listener_window _((Rect r));
extern Boolean edit_key_filter _((WindowPtr w, int c));

/* macdialogs2.c */
extern pascal VOID doDialog _((DialogPtr dlog, short theItem));
extern pascal VOID closeDialog _((void));
extern pascal VOID clobberDialog _((void));

/* maciviewwindow.c */
extern VOID init_mac_cursors _((void));
extern Rect scroll_bar_bounds _((WindowPtr w, int which));
extern VOID DrawGWGrowBox _((StGWWinInfo *gwinfo));
extern VOID mac_do_cursor _((StGWWinInfo *gwinfo));

/* maciviewwindow2.c */
extern VOID init_mac_colors _((void));
extern VOID SetHardwareState _((StGWWinInfo *gwinfo));
extern VOID set_fore_color _((StGWWinInfo *gwinfo));
extern VOID set_back_color _((StGWWinInfo *gwinfo));

/* maciviewwindow3.c */
extern VOID initialize_static_globals _((void));
extern VOID reset_clip_rect _((StGWWinInfo *gwinfo));
extern VOID adjust_graph_workport _((StGWWinInfo *gwinfo));

/* macstuff.c */
extern VOID getttyline _((LVAL s));
extern VOID MyShowWindow  _((WindowPtr wind));
extern int CintoPstring(const char *cp, StringPtr pp, int ppsize, int cvtnl);
extern OSType string_to_type _((char *s));
extern void NotifyIfInBackground _((void));

/* macwindows.c */
LVAL get_window_object _((WindowPtr w));
pascal VOID mac_update_action _((Boolean resized));
pascal VOID mac_activate_action _((Boolean active));
pascal VOID mac_close_action _((void));

#include "TransSkel1.h"
#endif /* MACINTOSH */

#ifdef X11WINDOWS
/* X11BSDstuff.c */
extern VOID StX11FlushStdin _((void));

/* X11buttons.c */
extern VOID InstallButtonItem _((Window win, LVAL item));
extern VOID DeleteButtonItem _((Window win, LVAL item));
extern int ClosePanelHeight _((void));
extern VOID InstallCloseButton _((Window win, LVAL object));
extern VOID DeleteCloseButton _((Window win));

/* X11choice.c */
extern VOID InstallChoiceItem _((Window win, LVAL item));
extern VOID DeleteChoiceItem _((Window win, LVAL item));

/* X11dialogs.c */
extern VOID install_dialog_item_handler _((Display *dpy,
					   Window win,
					   LVAL (*handler)(),
					   LVAL item));
extern VOID delete_dialog_item_handler _((Display *dpy, Window win));
extern VOID DialogAllocate _((LVAL dialog));
extern VOID DialogRemove _((LVAL dialog));

/* X11graph.c */
extern char *StX11GetDefault _((char *name));
extern VOID StProcessEvent _((Display *dpy, XEvent report));
extern Display *StX11Display _((void));
extern int StX11Screen _((void));
extern VOID StX11PressButton _((void));
extern VOID StX11ReleaseButton _((void));
extern int StX11ButtonIsDown _((void));
extern VOID StX11HandleClientMessage _((XEvent report));
extern VOID StX11SetWindowClass _((Display *dpy, Window win));
extern VOID StX11SetNormalHints _((Display *dpy, Window win,
				   int left, int top, int width, int height));
extern VOID StX11SetTransientHint _((Display *dpy, Window win));
extern VOID StX11SetStandardHints _((Display *dpy, Window w));
extern int StX11UseICCCM _((void));
extern VOID StWGetSize _((Window w, int *pwidth, int *pheight, int frame));
extern VOID StWGetLocation _((Window w, int *left, int *top, int frame));
extern int is_option_on _((char *s));

/* X11listitem.c */
extern VOID InstallListItem _((Window win, LVAL item));
extern VOID DeleteListItem _((Window win, LVAL item));

/* X11menus.c */
extern int StMObPopup _((LVAL menu, int x, int y, LVAL window));

/* X11scroll.c */
extern VOID InstallScrollBar _((Window w,
				LVAL object,
				int left, int top, int width, int height,
				Window *ps,
				VOID (*action)()));
extern VOID DeleteScrollBar _((Window s));
extern VOID AdjustScrollBar _((Window s, int val, int page, int max));

/* X11slider.c */
extern VOID InstallScrollItem _((Window win, LVAL item));
extern VOID DeleteScrollItem _((Window win, LVAL item));

/* X11text.c */
extern VOID InstallTextItem _((Window win, LVAL item));
extern VOID DeleteTextItem _((Window win, LVAL item));

/* X11toggle.c */
extern VOID InstallToggleItem _((Window win, LVAL item));
extern VOID DeleteToggleItem _((Window win, LVAL item));
#endif /* X11WINDOWS */

#endif /* XLGRAPH_H */

