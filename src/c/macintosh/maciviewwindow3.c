#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

/* forward declarations */
LOCAL VOID BufferMarkPoint _((unsigned x, unsigned y, unsigned sym, unsigned color));
LOCAL VOID make_picture _((StGWWinInfo *gwinfo, char *fname));
LOCAL VOID SetDrawingPort _((WindowPtr w));
LOCAL VOID MakeSymbols _((void));

/**************************************************************************/
/**                                                                      **/
/**                            Global Variables                          **/
/**                                                                      **/
/**************************************************************************/

static CGrafPort CWP;
static CGrafPtr CWorkPort;
static GrafPort WP;
static GrafPtr WorkPort;
static char **WorkRowStarts;
static int WorkPortWidth, WorkPortHeight;

static int buffering = FALSE;
static int copying_to_clip = FALSE;
static int bufflevel = 0;

static WindowPtr get_port() { return(thePort); }

static VOID set_port(WindowPtr port)
{
  if (port != thePort) SetPort(port);
}

VOID reset_clip_rect(StGWWinInfo *gwinfo)
{
  WindowPtr w;
  GrafPtr savePort;
  Rect r;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;  
  GetPort(&savePort);
  SetPort(w);
  r = w->portRect;
  r.right -= 15;
  if (gwinfo->hasHscroll) r.bottom -= 15;
  if (gwinfo->clipped) {
    gwinfo->clip_rect.right = min(gwinfo->clip_rect.right, r.right);
    gwinfo->clip_rect.bottom = min(gwinfo->clip_rect.bottom, r.bottom);
    r = gwinfo->clip_rect;
  }
  ClipRect(&r);
  if (buffering) {
    SetDrawingPort(w);
    ClipRect(&r);
  }
  SetPort(savePort);
}

/**************************************************************************/
/**                                                                      **/
/**                       Initialization Functions                       **/
/**                                                                      **/
/**************************************************************************/

VOID initialize_static_globals(void)
{
  int portSize;
  long i;  /* long to force row start calculation to be done long */
  
  WorkPort = &WP;
  OpenPort(WorkPort);
  WorkPort->portBits = screenBits;
  portSize = WorkPort->portBits.bounds.bottom 
           - WorkPort->portBits.bounds.top;
  WorkPort->portBits.baseAddr = 
      StCalloc(portSize, WorkPort->portBits.rowBytes);
  if (WorkPort->portBits.baseAddr == nil) {
  	xlfail("Buffer allocation failed");
  	exit(1);
  }
  WorkPortWidth = WorkPort->portBits.bounds.right 
                - WorkPort->portBits.bounds.left;
  WorkPortHeight = WorkPort->portBits.bounds.bottom 
                 - WorkPort->portBits.bounds.top;
  WorkRowStarts = (char **) StCalloc(sizeof(char *), portSize);
  for (i = 0; i < portSize; i++)
    WorkRowStarts[i] = WorkPort->portBits.baseAddr
                     + i * WorkPort->portBits.rowBytes;
  MakeSymbols();

  CWorkPort = (CGrafPtr) WorkPort;
  if (StScreenHasColor()) {
    char *p;
    Rect r;
    struct row_bytes {
      unsigned int flags : 3;
      unsigned int data  : 13;
    } *rowBytes;
    int size;

    CWorkPort = &CWP;
    OpenCPort(CWorkPort);
    
    r = (*CWorkPort->portPixMap)->bounds;
    rowBytes = (struct row_bytes *) &(*CWorkPort->portPixMap)->rowBytes;
    portSize = r.bottom - r.top;
    size = portSize * rowBytes->data;
    
    if ((nnodes < 20000 && FreeMem() < size + 900000) || /* no workspace */
         (nnodes >= 20000 && FreeMem() < size + 500000)) {
      xoserror("not enough memory to allocate color port");
      CWorkPort = (CGrafPtr) WorkPort;
    }
    else {
      HLock((Handle) CWorkPort->portPixMap);
      /* I do't know why I need this two-step business */
      p = StCalloc(portSize, rowBytes->data);
      (*CWorkPort->portPixMap)->baseAddr = p;
      HUnlock((Handle) CWorkPort->portPixMap);
    }
  }
}

VOID adjust_graph_workport(StGWWinInfo *gwinfo)
{
  GrafPtr savePort, workPort;
  WindowPtr w;
  
  if (buffering && gwinfo != nil && (w = gwinfo->window) != nil) {
    workPort = (StGWUseColor(gwinfo)) ? (GrafPtr) CWorkPort : WorkPort;
    GetPort(&savePort);
    SetPort(workPort);
    if (gwinfo->lineType != 0) PenPat(&gray);
    else PenPat(&black);
    PenMode(w->pnMode);
    TextMode(w->txMode);
    PenSize(w->pnSize.h, w->pnSize.h);
    set_fore_color(gwinfo);
    set_back_color(gwinfo);
    SetPort(savePort);
  }
}

/**************************************************************************/
/**                                                                      **/
/**                  Line and Rectangle Drawing Functions                **/
/**                                                                      **/
/**************************************************************************/

LOCAL VOID SetDrawingPort(WindowPtr w)
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(w);

  set_port((buffering) ? ((gwinfo->use_color) ? (GrafPtr) CWorkPort: WorkPort) : w);
}

static VOID draw_object(StGWWinInfo *gwinfo, int what, int how, int a, int b, int c, int d)
{
  GrafPtr savePort;
  Rect r;
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  savePort = get_port();
  SetDrawingPort(w);
	
  switch (what) {
  case 'L': MoveTo(a, b); LineTo(c, d); break;
  case 'P': MoveTo(a, b); Line(0, 0); break;
  case 'R':
    SetRect(&r, a, b, a + c, b + d);
    switch (how) {
    case 'E': EraseRect(&r); break;
    case 'F': FrameRect(&r); break;
    case 'P': PaintRect(&r); break;
    }
    break;
  case 'O':
    SetRect(&r, a, b, a + c, b + d);
    switch (how) {
    case 'E': EraseOval(&r); break;
    case 'F': FrameOval(&r); break;
    case 'P': PaintOval(&r); break;
    }
    break;
  }  
  set_port(savePort);
}

static VOID draw_arc(StGWWinInfo *gwinfo, int how, int a, int b, int c, int d,
                     double angle1, double angle2)
{
  GrafPtr savePort;
  Rect r;
  int a1, a2;
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  savePort = get_port();
  SetDrawingPort(w);
	
  SetRect(&r, a, b, a + c, b + d);
  a1 = 90 - angle1;
  a2 = -angle2;
  switch (how) {
  case 'E': EraseArc(&r, a1, a2); break;
  case 'F': FrameArc(&r, a1, a2); break;
  case 'P': PaintArc(&r, a1, a2); break;
  }
  set_port(savePort);
}

static VOID draw_poly(StGWWinInfo *gwinfo, int how, int n, short *p, int from_origin)
{
  GrafPtr savePort;
  int i;
  WindowPtr w;
  PolyHandle poly;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil || n <= 0) return;
  savePort = get_port();
  SetDrawingPort(w);
  
  poly = OpenPoly();
  MoveTo((int) p[0], (int) p[1]);
  for (i = 1; i < n; i++) {
    if (from_origin) LineTo((int) p[2 * i], (int) p[2 * i + 1]);
    else Line((int) p[2 * i], (int) p[2 * i + 1]);
  }
  ClosePoly();
  switch (how) {
  case 'E': ErasePoly(poly); break;
  case 'F': FramePoly(poly); break;
  case 'P': PaintPoly(poly); break;
  }
  KillPoly(poly);
  set_port(savePort);
}	
  
VOID StGWDrawLine(StGWWinInfo *gwinfo, int x1, int y1, int x2, int y2)
{
  draw_object(gwinfo, 'L', '0', x1, y1, x2, y2);
}

VOID StGWDrawPoint(StGWWinInfo *gwinfo, int x, int y)
{
  draw_object(gwinfo, 'P', '0', x, y, 0, 0);
}

VOID StGWEraseRect(StGWWinInfo *gwinfo, int left, int top, int width, int height)
{
  draw_object(gwinfo, 'R', 'E', left, top, width, height);
}

VOID StGWFrameRect(StGWWinInfo *gwinfo, int left, int top, int width, int height)
{
  draw_object(gwinfo, 'R', 'F', left, top, width, height);
} 

VOID StGWPaintRect(StGWWinInfo *gwinfo, int left, int top, int width, int height)
{
  draw_object(gwinfo, 'R', 'P', left, top, width, height);
}

VOID StGWEraseOval(StGWWinInfo *gwinfo, int left, int top, int width, int height)
{
  draw_object(gwinfo, 'O', 'E', left, top, width, height);
}

VOID StGWFrameOval(StGWWinInfo *gwinfo, int left, int top, int width, int height)
{
  draw_object(gwinfo, 'O', 'F', left, top, width, height);
}

VOID StGWPaintOval(StGWWinInfo *gwinfo, int left, int top, int width, int height)
{
  draw_object(gwinfo, 'O', 'P', left, top, width, height);
}

VOID StGWEraseArc(StGWWinInfo *gwinfo, int left, int top, int width, int height,
                  double angle1, double angle2)
{
  draw_arc(gwinfo, 'E', left, top, width, height, angle1, angle2);
}

VOID StGWFrameArc(StGWWinInfo *gwinfo, int left, int top, int width, int height,
                  double angle1, double angle2)
{
  draw_arc(gwinfo, 'F', left, top, width, height, angle1, angle2);
}

VOID StGWPaintArc(StGWWinInfo *gwinfo, int left, int top, int width, int height,
                  double angle1, double angle2)
{
  draw_arc(gwinfo, 'P', left, top, width, height, angle1, angle2);
}

VOID StGWErasePoly(StGWWinInfo *gwinfo, int n, short *p, int from_origin)
{
  draw_poly(gwinfo, 'E', n, p, from_origin);
}

VOID StGWFramePoly(StGWWinInfo *gwinfo, int n, short *p, int from_origin)
{
  draw_poly(gwinfo, 'F', n, p, from_origin);
}

VOID StGWPaintPoly(StGWWinInfo *gwinfo, int n, short *p, int from_origin)
{
  draw_poly(gwinfo, 'P', n, p, from_origin);
}

/**************************************************************************/
/**                                                                      **/
/**                            Text Functions                            **/
/**                                                                      **/
/**************************************************************************/

static text_size(StGWWinInfo *gwinfo, int ascent)
{
  WindowPtr w;
  GrafPtr savePort;
  FontInfo info;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil)
  	w = (StGWUseColor(gwinfo)) ? (WindowPtr) CWorkPort : (WindowPtr) WorkPort;

  savePort = get_port();
  set_port(w);
	
  GetFontInfo(&info);
  
  set_port(savePort);
  return((ascent) ? info.ascent : info.descent);
}

int StGWTextAscent(StGWWinInfo *gwinfo)  { return(text_size(gwinfo, TRUE));  }
int StGWTextDescent(StGWWinInfo *gwinfo) { return(text_size(gwinfo, FALSE)); }

int StGWTextWidth(StGWWinInfo *gwinfo, char *text)
{
  WindowPtr w;
  GrafPtr savePort;
  int string_width;
  Str255 pbuf;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil)
  	w = (StGWUseColor(gwinfo)) ? (WindowPtr) CWorkPort : (WindowPtr) WorkPort;

  savePort = get_port();
  set_port(w);
	
  CintoPstring(text, pbuf, sizeof pbuf, FALSE);
  string_width = StringWidth(pbuf);
  
  set_port(savePort);
  return(string_width);
}

VOID StGWDrawString(StGWWinInfo *gwinfo, char *s, int x, int y)
{
  GrafPtr savePort;
  WindowPtr w;
  Str255 pbuf;
  
  if (s == nil || gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  savePort = get_port();
  SetDrawingPort(w);
	
  MoveTo(x, y);
  CintoPstring(s, pbuf, sizeof pbuf, FALSE);
  DrawString(pbuf);
  
  set_port(savePort);
}

VOID StGWDrawText(StGWWinInfo *gwinfo, char *text, int x, int y, int h, int v)
{
  int FontAscent, string_width;

  if (text == nil || gwinfo == nil) return;
  
  FontAscent = StGWTextAscent(gwinfo);
  string_width = StGWTextWidth(gwinfo, text);

  if (v == 1) y += FontAscent;
  if (h == 1) x -= string_width / 2;
  if (h == 2) x -= string_width;

  StGWDrawString(gwinfo, text, x, y);
}

static VOID DrawCharUp(WindowPtr w, char myChar, int x, int y)
{
  static GrafPort gp;
  static GrafPtr charPort;
  static is_open = FALSE;
  char myImage[32], myTarget[32];
  BitMap myMap, saveBits;
  GrafPtr savePort;
  Rect r;
  
  register int i, j;
  
  savePort = get_port();
  
  if (! is_open) {
    charPort = &gp;
    OpenPort(charPort);
    is_open = TRUE;
  }
  set_port(charPort);
  
  charPort->txFont = w->txFont;
  charPort->txFace = w->txFace;
  charPort->txMode = w->txMode;
  charPort->txSize = w->txSize;
  charPort->spExtra = w->spExtra;
  
  myMap.baseAddr = myImage;
  myMap.rowBytes = 2;
  SetRect(&myMap.bounds, 0, 0, 15, 15);
  
  for (i = 0; i < 32; i++) {
    myImage[i] = '\0';
    myTarget[i] = '\0';
  }
  
  saveBits = charPort->portBits;
  charPort->portBits = myMap;
  MoveTo(3, 12);
  DrawChar(myChar);
  charPort->portBits = saveBits;
  set_port(savePort);
  
  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++)
      if (BitTst(myImage, 16 * i + j)) BitSet(myTarget, i + (16 - j) * 16);
  
  r = myMap.bounds;
  OffsetRect(&r, x - 12, y + 3 - (r.bottom - r.top));
  myMap.baseAddr = myTarget;
  
  SetDrawingPort(w);
  CopyBits(&myMap, &thePort->portBits, &myMap.bounds,  &r, w->txMode, nil);
  set_port(savePort);
}

VOID StGWDrawStringUp(StGWWinInfo *gwinfo, char *s, int x, int y)
{
  WindowPtr w;
  char str[2];
  int n;
  
  if (s == nil || gwinfo == nil || (w = gwinfo->window) == nil) return;
  str[1] = '\0';
  
  for (n = strlen(s); n > 0; n--, s++) {
  	DrawCharUp(w, *s, x, y);
  	str[0] = *s;
  	y -= StGWTextWidth(gwinfo, str);
  }
}
 
VOID StGWDrawTextUp(StGWWinInfo *gwinfo, char *text, int x, int y, int h, int v)
{
  int FontAscent, string_width;

  if (text == nil || gwinfo == nil) return;
  
  FontAscent = StGWTextAscent(gwinfo);
  string_width = StGWTextWidth(gwinfo, text);

  if (v == 1) x += FontAscent;
  if (h == 1) y += string_width / 2;
  if (h == 2) y += string_width;

  StGWDrawStringUp(gwinfo, text, x, y);
}

/**************************************************************************/
/**                                                                      **/
/**                           Symbol Functions                           **/
/**                                                                      **/
/**************************************************************************/

#define NUMSYMBOLS 18
#define SYMROWS 5

typedef struct {
  unsigned bit0 :1;
  unsigned bit1 :1;
  unsigned bit2 :1;
  unsigned bit3 :1;
  unsigned bit4 :1;
} SymBits;

typedef struct {
  BitMap map;
  int left, top;
  short image[SYMROWS];
  long refcon;
} Symbol;

static Symbol Symbols[NUMSYMBOLS];

VOID StGWSetSymRefCon(unsigned int index, long rc)
{
  if (index < NUMSYMBOLS) Symbols[index].refcon = rc;
}

long StGWGetSymRefCon(unsigned int index)
{	
  if (index < NUMSYMBOLS) return(Symbols[index].refcon);
  else return((long) nil);
}

static VOID InitSymbol(int sym, int left, int top, int width, int height)
{
  SetRect(&Symbols[sym].map.bounds, 0, 0, width, height);
  Symbols[sym].map.rowBytes = 2;
  Symbols[sym].map.baseAddr = (Ptr) &Symbols[sym].image;
  Symbols[sym].left = left;
  Symbols[sym].top = top;
}

VOID StGWGetSymbolSize(int sym, int *width, int *height)
{
  *width = Symbols[sym].map.bounds.right;
  *height = Symbols[sym].map.bounds.bottom;
}

static VOID SetSymbolData(int sym, int row, int bit0, int bit1, int bit2, int bit3, int bit4)
{
  SymBits *d = (SymBits *) &(Symbols[sym].image[row]);
  d->bit0 = bit0;
  d->bit1 = bit1;
  d->bit2 = bit2;
  d->bit3 = bit3;
  d->bit4 = bit4;
}

static VOID MakeSymbols(void)
{
  InitSymbol(0, 0, 0, 2, 2);
  SetSymbolData(0, 0, 1, 0, 0, 0, 0);
  SetSymbolData(0, 1, 0, 0, 0, 0, 0);
  SetSymbolData(0, 2, 0, 0, 0, 0, 0);
  SetSymbolData(0, 3, 0, 0, 0, 0, 0);
  SetSymbolData(0, 4, 0, 0, 0, 0, 0);

  InitSymbol(1, 0, 0, 2, 2);
  SetSymbolData(1, 0, 1, 1, 0, 0, 0);
  SetSymbolData(1, 1, 0, 0, 0, 0, 0);
  SetSymbolData(1, 2, 0, 0, 0, 0, 0);
  SetSymbolData(1, 3, 0, 0, 0, 0, 0);
  SetSymbolData(1, 4, 0, 0, 0, 0, 0);

  InitSymbol(2, 0, 0, 2, 2);
  SetSymbolData(2, 0, 1, 1, 0, 0, 0);
  SetSymbolData(2, 1, 1, 0, 0, 0, 0);
  SetSymbolData(2, 2, 0, 0, 0, 0, 0);
  SetSymbolData(2, 3, 0, 0, 0, 0, 0);
  SetSymbolData(2, 4, 0, 0, 0, 0, 0);

  InitSymbol(3, 0, 0, 2, 2);
  SetSymbolData(3, 0, 1, 1, 0, 0, 0);
  SetSymbolData(3, 1, 1, 1, 0, 0, 0);
  SetSymbolData(3, 2, 0, 0, 0, 0, 0);
  SetSymbolData(3, 3, 0, 0, 0, 0, 0);
  SetSymbolData(3, 4, 0, 0, 0, 0, 0);

  InitSymbol(4, 2, 2, 4, 4);
  SetSymbolData(4, 0, 0, 1, 1, 0, 0);
  SetSymbolData(4, 1, 1, 0, 0, 1, 0);
  SetSymbolData(4, 2, 1, 0, 0, 1, 0);
  SetSymbolData(4, 3, 0, 1, 1, 0, 0);
  SetSymbolData(4, 4, 0, 0, 0, 0, 0);

  InitSymbol(5, 2, 2, 4, 4);
  SetSymbolData(5, 0, 0, 1, 1, 0, 0);
  SetSymbolData(5, 1, 1, 1, 1, 1, 0);
  SetSymbolData(5, 2, 1, 1, 1, 1, 0);
  SetSymbolData(5, 3, 0, 1, 1, 0, 0);
  SetSymbolData(5, 4, 0, 0, 0, 0, 0);

  InitSymbol(6, 3, 3, 5, 5);
  SetSymbolData(6, 0, 0, 0, 1, 0, 0);
  SetSymbolData(6, 1, 0, 1, 0, 1, 0);
  SetSymbolData(6, 2, 1, 0, 0, 0, 1);
  SetSymbolData(6, 3, 0, 1, 0, 1, 0);
  SetSymbolData(6, 4, 0, 0, 1, 0, 0);

  InitSymbol(7, 3, 3, 5, 5);
  SetSymbolData(7, 0, 0, 0, 1, 0, 0);
  SetSymbolData(7, 1, 0, 1, 1, 1, 0);
  SetSymbolData(7, 2, 1, 1, 1, 1, 1);
  SetSymbolData(7, 3, 0, 1, 1, 1, 0);
  SetSymbolData(7, 4, 0, 0, 1, 0, 0);

  InitSymbol(8, 3, 3, 5, 5);
  SetSymbolData(8, 0, 0, 0, 1, 0, 0);
  SetSymbolData(8, 1, 0, 0, 1, 0, 0);
  SetSymbolData(8, 2, 1, 1, 1, 1, 1);
  SetSymbolData(8, 3, 0, 0, 1, 0, 0);
  SetSymbolData(8, 4, 0, 0, 1, 0, 0);

  InitSymbol(9, 3, 3, 5, 5);
  SetSymbolData(9, 0, 0, 1, 1, 1, 0);
  SetSymbolData(9, 1, 1, 0, 1, 0, 1);
  SetSymbolData(9, 2, 1, 1, 1, 1, 1);
  SetSymbolData(9, 3, 1, 0, 1, 0, 1);
  SetSymbolData(9, 4, 0, 1, 1, 1, 0);

  InitSymbol(10, 2, 2, 4, 4);
  SetSymbolData(10, 0, 1, 1, 1, 1, 0);
  SetSymbolData(10, 1, 1, 0, 0, 1, 0);
  SetSymbolData(10, 2, 1, 0, 0, 1, 0);
  SetSymbolData(10, 3, 1, 1, 1, 1, 0);
  SetSymbolData(10, 4, 0, 0, 0, 0, 0);

  InitSymbol(11, 2, 2, 4, 4);
  SetSymbolData(11, 0, 1, 1, 1, 1, 0);
  SetSymbolData(11, 1, 1, 1, 1, 1, 0);
  SetSymbolData(11, 2, 1, 1, 1, 1, 0);
  SetSymbolData(11, 3, 1, 1, 1, 1, 0);
  SetSymbolData(11, 4, 0, 0, 0, 0, 0);

  InitSymbol(12, 3, 3, 5, 5);
  SetSymbolData(12, 0, 0, 1, 1, 1, 0);
  SetSymbolData(12, 1, 1, 0, 0, 0, 1);
  SetSymbolData(12, 2, 1, 0, 0, 0, 1);
  SetSymbolData(12, 3, 0, 1, 0, 1, 0);
  SetSymbolData(12, 4, 0, 0, 1, 0, 0);

  InitSymbol(13, 3, 3, 5, 5);
  SetSymbolData(13, 0, 0, 1, 1, 1, 0);
  SetSymbolData(13, 1, 1, 1, 1, 1, 1);
  SetSymbolData(13, 2, 1, 1, 1, 1, 1);
  SetSymbolData(13, 3, 0, 1, 1, 1, 0);
  SetSymbolData(13, 4, 0, 0, 1, 0, 0);

  InitSymbol(14, 3, 3, 5, 5);
  SetSymbolData(14, 0, 0, 0, 1, 0, 0);
  SetSymbolData(14, 1, 0, 1, 0, 1, 0);
  SetSymbolData(14, 2, 1, 0, 0, 0, 1);
  SetSymbolData(14, 3, 1, 0, 0, 0, 1);
  SetSymbolData(14, 4, 0, 1, 1, 1, 0);

  InitSymbol(15, 3, 3, 5, 5);
  SetSymbolData(15, 0, 0, 0, 1, 0, 0);
  SetSymbolData(15, 1, 0, 1, 1, 1, 0);
  SetSymbolData(15, 2, 1, 1, 1, 1, 1);
  SetSymbolData(15, 3, 1, 1, 1, 1, 1);
  SetSymbolData(15, 4, 0, 1, 1, 1, 0);

  InitSymbol(16, 3, 3, 5, 5);
  SetSymbolData(16, 0, 1, 0, 0, 0, 1);
  SetSymbolData(16, 1, 0, 1, 0, 1, 0);
  SetSymbolData(16, 2, 0, 0, 1, 0, 0);
  SetSymbolData(16, 3, 0, 1, 0, 1, 0);
  SetSymbolData(16, 4, 1, 0, 0, 0, 1);

  InitSymbol(17, 3, 3, 5, 5);
  SetSymbolData(17, 0, 1, 1, 0, 1, 1);
  SetSymbolData(17, 1, 1, 1, 0, 1, 1);
  SetSymbolData(17, 2, 0, 0, 1, 0, 0);
  SetSymbolData(17, 3, 1, 1, 0, 1, 1);
  SetSymbolData(17, 4, 1, 1, 0, 1, 1);
}

VOID StGWDrawSymbol(StGWWinInfo *gwinfo, int sym, int x, int y)
{
  WindowPtr w;
  GrafPtr savePort;
  Rect r;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  savePort = get_port();
  SetDrawingPort(w);
  
  if (! copying_to_clip && buffering && sym < 4 && ! gwinfo->use_color) {
    BufferMarkPoint(x - gwinfo->view_h, y - gwinfo->view_v, 
                    sym, gwinfo->drawColor);
    return;
  }

  if (sym < 0 || sym >= NUMSYMBOLS) return;
  x -= Symbols[sym].left;
  y -= Symbols[sym].top;
  r = Symbols[sym].map.bounds;
  OffsetRect(&r, x, y);
  
  CopyBits(&Symbols[sym].map, &thePort->portBits,
           &Symbols[sym].map.bounds, &r, gwinfo->symbolMode, nil);
           
  set_port(savePort);
}

static VOID BufferMarkPoint(unsigned x, unsigned y, unsigned sym, unsigned color)
{
  if (x >= WorkPortWidth || y >= WorkPortHeight) return;
  if (color != 0) {
    BitSet(WorkRowStarts[y], x);     if (sym == 0) return;
    BitSet(WorkRowStarts[y], x + 1); if (sym == 1) return;
    BitSet(WorkRowStarts[y + 1], x); if (sym == 2) return;
    BitSet(WorkRowStarts[y + 1], x + 1);
  }
  else {
    BitClr(WorkRowStarts[y], x);     if (sym == 0) return;
    BitClr(WorkRowStarts[y], x + 1); if (sym == 1) return;
    BitClr(WorkRowStarts[y + 1], x); if (sym == 2) return;
    BitClr(WorkRowStarts[y + 1], x + 1);
  }  
}

VOID StGWReplaceSymbol(StGWWinInfo *gwinfo, unsigned oldsym, unsigned newsym, int x, int y)
{
  int oldwidth, oldheight, newwidth, newheight;
  
  if (oldsym >= NUMSYMBOLS || newsym >= NUMSYMBOLS) return;
  
  StGWGetSymbolSize(oldsym, &oldwidth, &oldheight);
  StGWGetSymbolSize(newsym, &newwidth, &newheight);
  if (oldwidth > newwidth || oldheight > newheight)
    StGWEraseRect(gwinfo, x - Symbols[oldsym].left, y - Symbols[oldsym].top,
                  oldwidth, oldheight);
  StGWDrawSymbol(gwinfo, newsym, x, y);
}

/**************************************************************************/
/**                                                                      **/
/**                           Bitmap Functions                           **/
/**                                                                      **/
/**************************************************************************/

static make_bitmap(int width, int height, char *image, BitMap *pbm)
{
  int i, j;
  char *rowdata;
  
  pbm->rowBytes = 2 * ((width % 16 == 0) ? width / 16 : width / 16 + 1);
  if (pbm->rowBytes * height <= 0) return(FALSE);
  
  pbm->baseAddr = StCalloc(pbm->rowBytes * height, 1);
  if (pbm->baseAddr == nil) return(FALSE);
  
  for (i = 0; i < height; i++) {
    rowdata = pbm->baseAddr + i * pbm->rowBytes;
    for (j = 0; j < width; j++)
      if (image[i * width + j] != 0) BitSet(rowdata, j);
  }
  pbm->bounds.left = 0; pbm->bounds.top = 0;
  pbm->bounds.right = width; pbm->bounds.bottom = height;
  return(TRUE);
}

static VOID free_bitmap(BitMap *pbm)
{
  if (pbm->baseAddr != nil) StFree(pbm->baseAddr);
}
    
VOID StGWDrawBitmap(StGWWinInfo *gwinfo, int left, int top, int width, int height, char *image)
{
  WindowPtr w;
  BitMap bm;
  GrafPtr savePort;
  Rect r;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  savePort = get_port();
  SetDrawingPort(w);
  if (make_bitmap(width, height, image, &bm)) {
    r = bm.bounds;
    OffsetRect(&r, left, top);
    CopyBits(&bm, &thePort->portBits, &bm.bounds, &r, gwinfo->symbolMode, nil);
    free_bitmap(&bm);
  }
  set_port(savePort);
}

/**************************************************************************/
/**                                                                      **/
/**                         Buffering Functions                          **/
/**                                                                      **/
/**************************************************************************/

VOID StGWStartBuffering(StGWWinInfo *gwinfo)
{
  WindowPtr w;
  GrafPtr savePort;
  Rect r;

  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  bufflevel++;
  if (buffering) return;
  buffering = TRUE;
  
  GetPort(&savePort);
  SetPort((StGWUseColor(gwinfo)) ? (GrafPtr) CWorkPort : WorkPort);

  set_fore_color(gwinfo);
  set_back_color(gwinfo);
  if (gwinfo->lineType != 0) PenPat(&gray);
  else PenPat(&black);
  
  PenMode(w->pnMode);
  TextFont(w->txFont);
  TextFace(w->txFace);
  TextMode(w->txMode);
  TextSize(w->txSize);
  SpaceExtra(w->spExtra);
  SetOrigin(gwinfo->view_h, gwinfo->view_v);
  r = w->portRect;
  r.right -= 15;
  if (gwinfo->hasHscroll) r.bottom -= 15;
  ClipRect(&r);
  CopyRgn(w->visRgn, thePort->visRgn);
  CopyRgn(w->clipRgn, thePort->clipRgn);
  SetPort(savePort);
}

VOID StGWBufferToScreen(StGWWinInfo *gwinfo, int left, int top, int width, int height)
{
  WindowPtr w;
  GrafPtr savePort, workPort;
  Rect r;

  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  if (bufflevel > 0) bufflevel--;
  if (bufflevel > 0) return;
  if (! buffering) return;
  
  buffering = FALSE;  
  workPort = (StGWUseColor(gwinfo)) ? (GrafPtr) CWorkPort : WorkPort;
  
  GetPort(&savePort);
  SetPort(w);
  ForeColor(blackColor);
  BackColor(whiteColor);
  SetRect(&r, left, top, left + width, top + height);
  if (SectRect(&r, &w->portRect, &r))
    CopyBits(&workPort->portBits,&w->portBits, &r, &r, srcCopy, nil);
  set_fore_color(gwinfo);
  set_back_color(gwinfo);
  SetPort(savePort);
}

/**************************************************************************/
/**                                                                      **/
/**                         Miscelaneous Functions                       **/
/**                                                                      **/
/**************************************************************************/

VOID StGWCopyToClip(StGWWinInfo *gwinfo)
{
  make_picture(gwinfo, nil);
}

VOID StGWDumpImage(StGWWinInfo *gwinfo, char *fname, double scale)
{
  make_picture(gwinfo, fname);
}

static VOID make_picture(StGWWinInfo *gwinfo, char *fname)
{
  PicHandle pic;
  Rect r;
  long result, mytype;
  GrafPtr savePort, workPort;
  LVAL object;
  WindowPtr w;
/*  int vRefNum, i;
  FILE *fptr; */
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  object = StGWGetObject(gwinfo);
  workPort = (StGWUseColor(gwinfo)) ? (GrafPtr) CWorkPort : WorkPort;

  StGWStartBuffering(gwinfo);
  r = w->portRect;
  r.right -= 15;
  if (StGWHasHscroll(gwinfo)) r.bottom -= 15;
  
  copying_to_clip = TRUE;
  GetPort(&savePort);
  SetPort(workPort);
  pic = OpenPicture(&r);
  SetPort(savePort);
  
  StGWObRedraw(object);

  GetPort(&savePort);
  SetPort(workPort);
  ClosePicture();
  SetPort(savePort);
  
  StGWBufferToScreen(gwinfo, 0, 0, 0, 0);
  
  if (fname == nil) {
    ZeroScrap();
    mytype = 'PICT';
    HLock((Handle) pic);
    result = PutScrap(GetHandleSize((Handle) pic), mytype, (Ptr) *pic);
    HUnlock((Handle) pic);
  }
  else {
/*    if ((fptr = fopen(fname, "w")) == nil)
      xlfail("can't open file");
    GetPort(&savePort);
    SetPort(WorkPort);
    DrawPicture(pic, &r);
    SetPort(savePort);
    writemacp(fptr, WorkRowStarts, r.right - r.left, r.bottom - r.top);
	xlfail("operation not supported");
	spoolpict(fname, pic, WorkPort);*/
  }
  KillPicture(pic);
  copying_to_clip = FALSE;
}

VOID StGWResetBuffer(void)
{
  copying_to_clip = FALSE;
  bufflevel = 0;
  buffering = FALSE;  
}
