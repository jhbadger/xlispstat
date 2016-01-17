#define BASIC_IVIEW 0

typedef int ColorCode;

typedef enum {
  MouseClick = 0,
  MouseMove = 1
} MouseEventType;

typedef enum {
  NoModifiers = 0,
  ExtendModifier = 1,
  OptionModifier = 2,
  OptionExtendModifier = 3
} MouseClickModifier;

typedef enum {
  pointInvisible,
  pointNormal,
  pointHilited,
  pointSelected
} PointState;

typedef enum {
  selecting,
  brushing,
  usermode
} MouseMode;

#ifdef MACINTOSH
#define thePort qd.thePort
#define arrow qd.arrow
#ifdef THINK_C
#define RETURNCHAR '\r'
#endif /* THINK_C */
#ifdef applec
#define RETURNCHAR '\n'
#endif /* applec */
#ifdef __MWERKS__
#define RETURNCHAR '\r'
#endif /* __MWERKS__ */
#else
#ifdef SUNVIEW
typedef struct {
  int h, v;
} Point;
#else
typedef struct {
  int h, v;
} Point;
typedef struct {
  int left, right, width, height;
} Rect;
#endif /* SUNVIEW */
#endif /* MACINTOSH */

#ifdef MACINTOSH
#define IVIEW_WINDOW WindowPtr
#define IVIEW_MENU MenuHandle

#define InitialTop (20 + GetMBarHeight())
#define InitialLeft 10
#define MOUSE_TOLERANCE 0

# define gray qd.gray
# define black qd.black
# define thePort qd.thePort
# define screenBits qd.screenBits
# define arrow qd.arrow
# undef extend
#ifdef applec
VOID PtoCstr _((StringPtr s));
VOID CtoPstr _((char * s));
#endif /* applec */

typedef struct {
  LVAL Object;                                /* elements of window_data */
  int idleOn, frontOnly;                      /* elements of window_data */
  int mouse_x, mouse_y;                       /* elements of window_data */
  WindowPtr window;
  void (*FreeMem) _((IVIEW_WINDOW));
  ColorCode backColor, drawColor;
  int canvasWidth, canvasHeight;
  int lineType, drawMode, lineWidth;
  long RefCon;
  int use_color;
  int hasHscroll, hasVscroll, view_h, view_v;
  int v_scroll_inc[2], h_scroll_inc[2];
  ControlHandle hscroll, vscroll;
  int initialized;
  int symbolMode;
  int cursor;
  Rect clip_rect;
  int clipped;
} StGWWinInfo;

extern Rect scroll_bar_bounds();

#define GetWRefCon(w) (((WindowPeek)(w))->refCon)

#define BAG_RES    1800
#define GLASS_RES  1801
#define CAN_RES    1802
#define BRUSH_RES  1803
#define HAND_RES   1804
#define FINGER_RES 1805
#endif /* MACINTOSH */

#ifdef UNIX
#ifdef GNUPLOT
#define Window long
#define Menu long
#define StGWWinInfo VOID
#define StGWWinInfo VOID
#endif /* GNUPLOT */
#define IVIEW_WINDOW Window
#define IVIEW_MENU Menu
#endif /* UNIX */

#ifdef SUNVIEW
typedef struct {
  long Object;                                /* elements of window_data */
  int idleOn, frontOnly;                      /* elements of window_data */
  int mouse_x, mouse_y;                       /* elements of window_data */
  IVIEW_WINDOW window;
  int (*FreeMem)();
  IVIEW_MENU menu;
  int (*MenuAction)(), (*MenuShow)(), (*MenuUpdate)();
  ColorCode backColor, drawColor;
  int canvasWidth, canvasHeight;
  int lineType, drawMode, lineWidth;
  Canvas content_canvas;
  long RefCon;
  int use_color;
  int hasHscroll, hasVscroll, view_h, view_v;
  int v_scroll_inc[2], h_scroll_inc[2];
/*  ControlHandle hscroll, vscroll; +++ */
  int initialized;
  int symbolMode;
  int cursor;
  Rect clip_rect;
  int clipped;
} StGWWinInfo;
#endif /* SUNVIEW */

#ifdef X11WINDOWS
typedef struct {
  LVAL Object;                                /* elements of window_data */
  int idleOn, frontOnly;                      /* elements of window_data */
  int mouse_x, mouse_y;                       /* elements of window_data */
  Window window, panel;
  VOID (*FreeMem) _((IVIEW_WINDOW));
  ColorCode backColor, drawColor;
  int canvasWidth, canvasHeight;
  int lineType, drawMode, lineWidth;
  long RefCon;
  int use_color;
  int hasHscroll, hasVscroll, view_h, view_v;
  int v_scroll_inc[2], h_scroll_inc[2];
  Window hscroll, vscroll;
  int initialized;
  int symbolMode;
  int cursor;
  int clip_left, clip_top, clip_width, clip_height;
  int clipped;
  int frame_width, frame_height;
  GC gc, erase_gc, xor_gc;
  int go_away, has_menu_button;
} StGWWinInfo;
#endif /* X11WINDOWS */

#ifdef _Windows
#define IVIEW_WINDOW HWND
#define IVIEW_MENU HMENU

typedef struct {
  LVAL Object;                                /* elements of window_data */
  int idleOn;                                 /* elements of window_data */
  int mouse_x, mouse_y;                       /* elements of window_data */
  IVIEW_WINDOW window;
  void (*FreeMem)(HWND);
  ColorCode backColor, drawColor;
  int canvasWidth, canvasHeight;
  int lineType, drawMode, lineWidth;
  long RefCon;
  int use_color;
  int hasHscroll, hasVscroll, view_h, view_v;
  int v_scroll_inc[2], h_scroll_inc[2];
  int initialized;
  int symbolMode;
  int cursor;
  RECT clip_rect;
  int clipped;
  LOGPEN drawPen;
  int rect_offset;
  int bPrinting;
  int cxPrintPos, cyPrintPos;
} StGWWinInfo;

/* Access Defines */
#define SETWINOBJECT(w, d) SetWindowLong((HWND) (w), 0, (LONG) (d))
#define GETWINOBJECT(w) ((LVAL) GetWindowLong((HWND) (w), 0))
#define SETGWINFO(w, d) SetWindowLong((HWND) (w), 4, (LONG) (d))
#define GETGWINFO(w) ((StGWWinInfo *) GetWindowLong((HWND) (w), 4))

/* GDI defines */
#define GET_DC(w) (currentDC ? currentDC : GetDC((HWND) (w)))
#define RELEASE_DC(w, d) if (!currentDC) ReleaseDC((HWND) (w), (HDC) (d))
#define GET_DRAW_PEN(g) ((g)->drawPen)
#define RELEASE_PEN(p) DeleteObject(p);
#define SET_PEN(h,g) SelectObject(h, CreatePenIndirect(&GET_DRAW_PEN(g)))
#define RESTORE_PEN(h,p) RELEASE_PEN(SelectObject(h,p))
#define GET_DRAW_BRUSH(g) CreateSolidBrush(get_color((g)->drawColor))
#define GET_ERASE_BRUSH(g) CreateSolidBrush(get_color((g)->backColor))
#define SET_DRAW_BRUSH(h,g) SelectObject(h, GET_DRAW_BRUSH(g))
#define SET_ERASE_BRUSH(h,g) SelectObject(h, GET_ERASE_BRUSH(g))
#define RELEASE_BRUSH(b) DeleteObject(b)
#define RESTORE_BRUSH(h,b) RELEASE_BRUSH(SelectObject(h,b))
#endif /* _Windows */

extern StGWWinInfo *StGWObWinInfo _((LVAL object));
extern long StGWGetRefCon _((StGWWinInfo *));
extern long StGWGetColRefCon _((unsigned int index));
extern long StGWGetCursRefCon _((unsigned int index));
extern long StGWGetSymRefCon _((unsigned int index));

extern IVIEW_WINDOW IViewWindowNew _((LVAL object, int is_GW));
extern ColorCode StGWDrawColor _((StGWWinInfo *));
extern ColorCode StGWBackColor _((StGWWinInfo *));
extern LVAL IViewWindowGetObject _((IVIEW_WINDOW w));
StGWWinInfo *IViewWindowWinInfo _((IVIEW_WINDOW w));

extern IVIEW_WINDOW IViewNew _((LVAL object));
extern IVIEW_WINDOW IViewSpinNew _((LVAL object));
extern IVIEW_WINDOW IViewScatmatNew _((LVAL object));
extern IVIEW_WINDOW IViewListNew _((LVAL object));
extern IVIEW_WINDOW IViewHistNew _((LVAL object));

#define ARROW_CURSOR      0
#define WATCH_CURSOR      1
#define CROSS_CURSOR      2
#define BRUSH_CURSOR      3
#define HAND_CURSOR       4
#define FINGER_CURSOR     5
#define HOUR_GLASS_CURSOR 6
#define TRASH_BAG_CURSOR  7
#define TRASH_CAN_CURSOR  8

typedef char *IViewReallocData;

#ifdef MACINTOSH
#define Fixed Fixed_
#define FixRound FixRound_
#endif /* MACINTOSH */

typedef long Fixed;

typedef struct basic_points {
  int num_points, num_variables;
  double *scale, *location, **transformation;
  IViewReallocData *data, *screen_data;
  int recalculateScreenPoints, fixed;
} *IViewBasicPoints;

#define NOCOLOR 255
typedef unsigned char color_index;

typedef struct point_symbol {
  int normal, highlighted;
} PointSymbol;

typedef struct point_info {
  PointState state, screen_state;
  char marked, masked;
  color_index color;
  PointSymbol symbol;
  char *label;
} PointInfo;

typedef struct line_info {
  int next, type;
  char masked, width;
  color_index color;
} LineInfo;

#ifdef USESTRINGS
typedef struct string_info {
  char *string;
  char masked, up, h, v;
  color_index color;
} StringInfo;
#endif /* USESTRINGS */

typedef struct iview_data {
#ifdef USESTRINGS
  IViewBasicPoints points, lines, strings;
  IViewReallocData pointInfo, lineInfo, stringInfo;
#else
  IViewBasicPoints points, lines;
  IViewReallocData pointInfo, lineInfo;
#endif /* USESTRINGS */
  double *mins, *maxes, *scale, *location;
  int *screen_mins, *screen_maxes;
  char **variableLabels;
  int recalculateScreenPoints, transformed;
  double **transformation;
} *IViewData;

#define IVIEW_WINDOW_NULL(w) (w == (IVIEW_WINDOW) 0)
