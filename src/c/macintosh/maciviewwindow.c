#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

/* forward declarations */
LOCAL VOID DoCursor _((StGWWinInfo *gwinfo));

StGWWinInfo *IViewWindowWinInfo(WindowPtr w)
{
  return((w != nil) ? (StGWWinInfo *) GetWRefCon(w) : nil);
}

/**************************************************************************/
/**                                                                      **/
/**                       Initialization Functions                       **/
/**                                                                      **/
/**************************************************************************/

VOID StInitGraphics()
{
  initialize_static_globals();
  init_mac_colors();
  init_mac_cursors();
}

/**************************************************************************/
/**                                                                      **/
/**                       Window Creation Functions                      **/
/**                                                                      **/
/**************************************************************************/

Rect scroll_bar_bounds(WindowPtr w, int which)
{
  Rect r;
  
  r = w->portRect;
  switch(which) {
  case 'H':
    r.top = r.bottom - 15; r.right -= 14; r.left--; r.bottom++; break;
  case 'V':
    r.left = r.right - 15; r.bottom -= 14; r.top--; r.right++; break;
  }
  return(r);
}

static int which_scroll, old_view_h, old_view_v;

static pascal void scroll_action(ControlHandle theCtl, short part)
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(thePort);
  int value = GetControlValue(theCtl);
  int left, top, width, height, inc, pageInc;
  WindowPtr w = thePort;
  LVAL object;
  
  object = StGWGetObject(gwinfo);

  if (gwinfo == nil) return;
  
  switch (which_scroll) {
  case 'H': 
    inc = gwinfo->h_scroll_inc[0];
    pageInc = gwinfo->h_scroll_inc[1];
    break;
  case 'V': 
    inc = gwinfo->v_scroll_inc[0];
    pageInc = gwinfo->v_scroll_inc[1];
    break;
  }
  
  switch(part) {
  case kControlUpButtonPart: value -= inc; break;
  case kControlDownButtonPart: value += inc; break;
  case kControlPageUpPart: value -= pageInc; break;
  case kControlPageDownPart: value += pageInc; break;
  }
  switch (which_scroll) {
  case 'H': StGWSetScroll(gwinfo, value, gwinfo->view_v, FALSE); break;
  case 'V': StGWSetScroll(gwinfo, gwinfo->view_h, value, FALSE); break;
  }
  reset_clip_rect(gwinfo);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);
  StGWStartBuffering(gwinfo);
  StGWObRedraw(object);
  StGWBufferToScreen(gwinfo, left, top, width, height);
  SetPort(w);
  SetOrigin(old_view_h, old_view_v);
  ClipRect(&w->portRect);
}

static ControlActionUPP scroll_actionUPP = NULL;

static pascal void mouse_action(Point pt, long t, short mods)
{
  WindowPtr w = FrontWindow();
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(w);
  ControlHandle theCtl;
  int part, h, v, option, extend;
  Rect r;
  LVAL object;
  
  object = StGWGetObject(gwinfo);

  if (gwinfo == nil) return;
  
  if ((part = FindControl(pt, w, &theCtl)) != 0) {
    if (theCtl == gwinfo->hscroll) which_scroll = 'H';
    else which_scroll = 'V';
    ClipRect(&w->portRect);
    if (part != 0 && part != kControlIndicatorPart) {
      old_view_h = GetControlValue(gwinfo->hscroll);
      old_view_v = GetControlValue(gwinfo->vscroll);
      if (! scroll_actionUPP)
        scroll_actionUPP = NewControlActionProc((ProcPtr) scroll_action);
      TrackControl(theCtl, pt, scroll_actionUPP);
      SetPort(w);
      SetOrigin(gwinfo->view_h, gwinfo->view_v);
      SetRect(&r, 0, 0, 0, 0);
      ClipRect(&r);
      r = scroll_bar_bounds(w, 'H');
      MoveControl(gwinfo->hscroll, r.left, r.top);
      r = scroll_bar_bounds(w, 'V');
      MoveControl(gwinfo->vscroll, r.left, r.top);
    }
    else if (part == kControlIndicatorPart) 
      if (TrackControl(theCtl, pt, nil) == kControlIndicatorPart) {
        h = GetControlValue(gwinfo->hscroll);
        v = GetControlValue(gwinfo->vscroll);
        StGWSetScroll(gwinfo, h, v, TRUE);
        reset_clip_rect(gwinfo);
        StGWObRedraw(object);
      }
    reset_clip_rect(gwinfo);
  }
  else {
    extend = BitAnd(mods, shiftKey);
    option = BitAnd(mods, optionKey);
    mods = (extend) ? ExtendModifier : NoModifiers;
    if (option) mods += 2;
    StGWObDoMouse(gwinfo->Object, pt.h, pt.v, MouseClick, (MouseClickModifier) mods);
  }
  SetPort(w);                 /* I don't know why this is here - maybe it is needed */
/*  ValidRect(&w->portRect);*//* This definitely causes problems */
}

static pascal void key_action(short key, short code, short mods)
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(FrontWindow());
  LVAL object;
  int shift, opt;
  
  if (gwinfo == nil) return;
  object = StGWGetObject(gwinfo);
  shift = BitAnd(mods, shiftKey);
  opt = BitAnd(mods, optionKey);
  if (key == '\r') key = '\n';
  StGWObDoKey(object, key, shift, opt);
}

VOID graph_update_action(StGWWinInfo *gwinfo, int resized)
{
  WindowPtr w;
  int width, height;
  GrafPtr savePort;
  Rect r;
  LVAL object;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  object = StGWGetObject(gwinfo);

  GetPort(&savePort);
  SetPort(w);
  if (! gwinfo->initialized) {
    resized = TRUE;
    gwinfo->initialized = TRUE;
  }
  
  if (resized) {
    reset_clip_rect(gwinfo);
    EraseRect(&w->portRect);
    StGWGetViewRect(gwinfo, nil, nil, &width, &height);
    
    if (gwinfo->hasHscroll) {
      HideControl(gwinfo->hscroll);
      width = (gwinfo->canvasWidth > width) ? gwinfo->canvasWidth - width : 0;
      SetControlMaximum(gwinfo->hscroll, width);
      SetControlValue(gwinfo->hscroll, gwinfo->view_h);
      gwinfo->view_h = GetControlValue(gwinfo->hscroll);
    }

    if (gwinfo->hasVscroll) {
      HideControl(gwinfo->vscroll);
      height = (gwinfo->canvasHeight > height) ? gwinfo->canvasHeight - height : 0;
      SetControlMaximum(gwinfo->vscroll, height);
      SetControlValue(gwinfo->vscroll, gwinfo->view_v);
      gwinfo->view_v = GetControlValue(gwinfo->vscroll);
    }

    SetOrigin(gwinfo->view_h, gwinfo->view_v);
    r = scroll_bar_bounds(w, 'H');
    MoveControl(gwinfo->hscroll, r.left, r.top);
    SizeControl(gwinfo->hscroll, r.right - r.left, r.bottom - r.top);
    r = scroll_bar_bounds(w, 'V');
    MoveControl(gwinfo->vscroll, r.left, r.top);
    SizeControl(gwinfo->vscroll, r.right - r.left, r.bottom - r.top);
    ClipRect(&w->portRect);
    if (gwinfo->hasHscroll) ShowControl(gwinfo->hscroll);
    if (gwinfo->hasVscroll) ShowControl(gwinfo->vscroll);

    reset_clip_rect(gwinfo);
  }
  
  if (resized && gwinfo != nil) {
    if (! gwinfo->hasHscroll)
      gwinfo->canvasWidth = w->portRect.right
                         - w->portRect.left - 15;
    if (! gwinfo->hasVscroll) {
      gwinfo->canvasHeight = w->portRect.bottom
                          - w->portRect.top;
      if (gwinfo->hasHscroll) gwinfo->canvasHeight -= 15;
    }
    StGWObResize(object);
  }
  
  StGWObRedraw(object);
  
  DrawGWGrowBox(gwinfo);
  SetPort(w);
  ClipRect(&w->portRect);
  DrawControls(w);
  reset_clip_rect(gwinfo);
  SetHardwareState(gwinfo);
  SetPort(w);
}

VOID DrawGWGrowBox(StGWWinInfo *gwinfo)
{
  WindowPtr w;
  Rect r;
  GrafPtr savePort;
  int reverse;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  reverse = (StGWBackColor(gwinfo) != 0);
  GetPort(&savePort);
  SetPort(w);
    
  r = w->portRect;
  r.left = r.right - 15;
  ClipRect(&r);
  ForeColor(blackColor);
  BackColor(whiteColor);
  if (! StGWHasVscroll(gwinfo)) EraseRect(&r);
  DrawGrowIcon(w);
  if (reverse) {
    r = w->portRect;
    r.left = r.right - 15;
    if (StGWHasVscroll(gwinfo)) r.top = r.bottom - 15;
    if (! StGWHasVscroll(gwinfo)) InvertRect(&r);
  }
  set_fore_color(gwinfo);
  set_back_color(gwinfo);
  
  reset_clip_rect(gwinfo);
  SetHardwareState(gwinfo);
  SetPort(savePort);
}

VOID graph_activate_action(StGWWinInfo *gwinfo, int active)
{
  WindowPtr w;
  Point pt;
  int value;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  if (active) DoCursor(gwinfo);
  else SetCursor(&arrow);
  
  GetMouse(&pt);
  gwinfo->mouse_x = pt.h; gwinfo->mouse_y = pt.v;
  DrawGWGrowBox(gwinfo);
  
  SetPort(w);
  ClipRect(&w->portRect);
  value = (active) ? 0 : 255;
  HiliteControl(gwinfo->hscroll, value);
  HiliteControl(gwinfo->vscroll, value);
  reset_clip_rect(gwinfo);
}

static pascal void clobber_action(void)
{
  WindowPtr wind = thePort;
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(thePort);

  if (IViewInternalIsLinked(wind)) IViewUnlinkWindow(wind);
  StGWObDoClobber(gwinfo->Object);
  if (gwinfo != nil && gwinfo->FreeMem != nil) (*gwinfo->FreeMem)(wind);

  gwinfo->window = nil;

  DisposeWindow(wind);
}

static pascal void idle_action(void) 
{
  WindowPtr wind = thePort;
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(wind);
  Point pt;
  int old_x, old_y;
  
  if (gwinfo != nil && wind == FrontWindow()) {
    DoCursor(gwinfo);
    old_x = gwinfo->mouse_x; old_y = gwinfo->mouse_y;
    GetMouse(&pt);
    gwinfo->mouse_x = pt.h; gwinfo->mouse_y = pt.v;
    if ((abs(gwinfo->mouse_x - old_x) > MOUSE_TOLERANCE
         || abs(gwinfo->mouse_y - old_y) > MOUSE_TOLERANCE)) {
      GetMouse(&pt);
      gwinfo->mouse_x = pt.h; gwinfo->mouse_y = pt.v;     
      StGWObDoMouse(gwinfo->Object, pt.h, pt.v, MouseMove, NoModifiers);
    }
  }
  if (gwinfo != nil && gwinfo->idleOn) StGWObDoIdle(gwinfo->Object);
}

int StGWWinInfoSize(void) { return(sizeof(StGWWinInfo)); }

VOID StGWInitWinInfo(LVAL object)
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) StGWObWinInfo(object);

  gwinfo->Object = object;
  gwinfo->initialized = FALSE;
  gwinfo->symbolMode = srcCopy;
  gwinfo->canvasWidth = 0;
  gwinfo->canvasHeight = 0;
  gwinfo->hasHscroll = FALSE;
  gwinfo->hasVscroll = FALSE;
  gwinfo->view_h = 0;
  gwinfo->view_v = 0;
  gwinfo->h_scroll_inc[0] = 1; gwinfo->h_scroll_inc[1] = 50;
  gwinfo->v_scroll_inc[0] = 1; gwinfo->v_scroll_inc[1] = 50;
  gwinfo->lineType = 0;
  gwinfo->drawMode = 0;
  gwinfo->backColor = 0;
  gwinfo->drawColor = 1;
  gwinfo->lineWidth = 1;
  gwinfo->window = nil;
  gwinfo->idleOn = FALSE;
  gwinfo->use_color = FALSE;
  gwinfo->cursor = 0;
  gwinfo->RefCon = (long) nil;
}

WindowPtr IViewWindowNew(LVAL object, int is_GW)
{
  char *title;
  int left, top, width, height, goAway;
  StGWWinInfo *gwinfo;
  WindowPtr wind;
  Rect r;
  Str255 pbuf;
  
  StGWGetAllocInfo(object, &title, &left, &top, &width, &height, &goAway);
  if (title == nil || strlen(title) <= 0) title = "Graph Window";
  
  top += GetMBarHeight();
  SetRect(&r, left, top, left + width + 15, top + height);
              
  CintoPstring(title, pbuf, sizeof pbuf, FALSE);
  if (StScreenHasColor())
    wind = NewCWindow(nil, &r, pbuf, FALSE, 8, (WindowPtr) -1L, goAway, 0L);
  else  
    wind = NewWindow(nil, &r, pbuf, FALSE, 8, (WindowPtr) -1L, goAway, 0L);
  if (wind == nil) xlfail("allocation Failed");
  
  if (! SkelWindow(wind, mouse_action, key_action, mac_update_action, 
	               mac_activate_action, mac_close_action, clobber_action,
	               idle_action, FALSE))
    xlfail("allocation Failed");
     
  SetPort(wind);
  if (graphFontNum)
    TextFont(graphFontNum);
  TextSize(graphFontSize);
  TextFace(graphFontStyle);
  TextMode(srcXor);
  
  gwinfo = (StGWWinInfo *) StGWObWinInfo(object);
  
  r = wind->portRect;
  r.top = r.bottom - 15; r.right -= 14; r.left--; r.bottom++;
  gwinfo->hscroll = NewControl(wind, &r, "\p", FALSE, 0, 0, 0, 
                              scrollBarProc, 0L);
  r = wind->portRect;
  r.left = r.right - 15; r.bottom -= 14; r.top--; r.right++;
  gwinfo->vscroll = NewControl(wind, &r, "\p", FALSE, 0, 0, 0, 
                              scrollBarProc, 0L);

  gwinfo->window = wind;
  if (! gwinfo->hasHscroll) gwinfo->canvasWidth = width;
  if (! gwinfo->hasVscroll) gwinfo->canvasHeight = height;
  gwinfo->initialized = FALSE;

  SetWRefCon(wind, (long) gwinfo);
  SetHardwareState(gwinfo);
  StGWEraseRect(gwinfo, 0, 0, width, height);
  StGWSetClipRect(gwinfo, FALSE, 0, 0, 0, 0);
  
  if (is_GW) set_iview_window_address((CPTR) wind, object);
  else set_iview_address((CPTR) wind, object);

  return(wind);
}

/**************************************************************************/
/**                                                                      **/
/**                          Clipping  Functions                         **/
/**                                                                      **/
/**************************************************************************/

VOID StGWSetClipRect(StGWWinInfo *gwinfo, int clipped, int left, int top, int width, int height)
{
  if (gwinfo == nil) return;
  gwinfo->clipped = clipped;
  if (clipped) {
    SetRect(&gwinfo->clip_rect, left, top, left + width, top + height);
  }
  reset_clip_rect(gwinfo);
}

int StGWGetClipRect(StGWWinInfo *gwinfo, int *left, int *top, int *width, int *height)
{
  if (gwinfo == nil) return(FALSE);
  if (gwinfo->clipped) {
    if (left != nil) *left = gwinfo->clip_rect.left;
    if (top != nil) *top = gwinfo->clip_rect.top;
    if (width != nil) *width = gwinfo->clip_rect.right - gwinfo->clip_rect.left;
    if (height != nil) *height = gwinfo->clip_rect.bottom - gwinfo->clip_rect.top;
  }
  return(gwinfo->clipped);
}

/**************************************************************************/
/**                                                                      **/
/**                         Miscelaneous Functions                       **/
/**                                                                      **/
/**************************************************************************/

#define NumBasicCursors 9
static int NumCursors;

typedef struct {
  CursHandle curs;
  long refcon;
} cursor_entry;

static cursor_entry *curstab;

VOID init_mac_cursors(void)
{
  NumCursors = NumBasicCursors;
  curstab = (cursor_entry *) StCalloc(NumCursors, sizeof(cursor_entry));
}

VOID StGWSetCursRefCon(unsigned int index, long rc)
{
  if (index < NumCursors) curstab[index].refcon = rc;
}

long StGWGetCursRefCon(unsigned int index)
{	
  if (index < NumCursors) return(curstab[index].refcon);
  else return((long) nil);
}

static VOID set_image_bits(int n, char *bits, char *image)
{
  int i;
  
  for (i = 0; i < n; i++) {
    if (image[i] == 0) BitClr(bits, i);
    else BitSet(bits, i);
  }
}

static VOID clear_image_bits(int n, char *bits)
{
  int i;
  
  for (i = 0; i < n; i++) BitClr(bits, i);
}
  
int StGWMakeCursor(int n, char *image, char *mask, int h, int v, long refcon)
{
  int index;
  char *temp;
  CursHandle curs;
  
  if (n != 16 || image == nil) return(-1);
  for (index = 0; index < NumCursors && StGWGetCursRefCon(index) != (long) nil; index++);
  if (index >= NumCursors) {
    temp = realloc(curstab, (NumCursors + 1) * sizeof(cursor_entry));
    if (temp == nil) return(-1);
    curstab = (cursor_entry *) temp;
    NumCursors++;
    curstab[index].curs = nil;
    curstab[index].refcon = (long) nil;
  }
  if (curstab[index].curs != nil) DisposeHandle((Handle) curstab[index].curs);
  curs = (CursHandle) NewHandle(sizeof(Cursor));
  if (curs == nil) return(-1);
  
  if (mask == nil) clear_image_bits(n * n, (char *) (*curs)->mask);
  else set_image_bits(n * n, (char *) (*curs)->mask, mask);
  set_image_bits(n * n, (char *) (*curs)->data, image);
  (*curs)->hotSpot.h = h;
  (*curs)->hotSpot.v = v;
  curstab[index].curs = curs;
  curstab[index].refcon = refcon;
  return(index);
}

int StGWMakeResCursor(char *name, int num, long refcon)
{
  int index;
  char *temp;
  CursHandle curs, curs_copy;
  
  for (index = 0; index < NumCursors && StGWGetCursRefCon(index) != (long) nil; index++);
  if (index >= NumCursors) {
    temp = realloc(curstab, (NumCursors + 1) * sizeof(cursor_entry));
    if (temp == nil) return(-1);
    curstab = (cursor_entry *) temp;
    NumCursors++;
    curstab[index].curs = nil;
    curstab[index].refcon = (long) nil;
  }
  if (curstab[index].curs != nil) DisposeHandle((Handle) curstab[index].curs);
  
  if (name != nil) {
    Str255 pbuf;
    CintoPstring(name, pbuf, sizeof pbuf, FALSE);
    curs = (CursHandle) GetNamedResource('CURS', pbuf);
  }
  else curs = GetCursor(num);
  
  if (curs == nil) return(-1);
  curs_copy = (CursHandle) NewHandle(sizeof(Cursor));
  if (curs_copy == nil) return(-1);
  **curs_copy = **curs;
  
  curstab[index].curs = curs_copy;
  curstab[index].refcon = refcon;
  return(index);
}

VOID StGWFreeCursor(unsigned int index)
{
  if (index < NumCursors && index >= NumBasicCursors) {
    if (curstab[index].curs != nil)
      DisposeHandle((Handle) curstab[index].curs);
    curstab[index].curs = nil;
    curstab[index].refcon = (long) nil;
  }
  else xlfail("can't free standard cursor");
}

static CursHandle get_cursor(unsigned int index)
{
  CursHandle curs = nil;
  
  if (index < NumBasicCursors) {
    switch (index) {
    case ARROW_CURSOR:      curs = nil;                    break;
    case WATCH_CURSOR:      curs = GetCursor(watchCursor); break;
    case CROSS_CURSOR:      curs = GetCursor(crossCursor); break;
    case BRUSH_CURSOR:      curs = GetCursor(BRUSH_RES);   break;
    case HAND_CURSOR:       curs = GetCursor(HAND_RES);    break;
    case FINGER_CURSOR:     curs = GetCursor(FINGER_RES);  break;
    case HOUR_GLASS_CURSOR: curs = GetCursor(GLASS_RES);   break;
    case TRASH_BAG_CURSOR:  curs = GetCursor(BAG_RES);     break;
    case TRASH_CAN_CURSOR:  curs = GetCursor(CAN_RES);     break;
    }
  }
  else if (index < NumCursors)
    curs = curstab[index].curs;
    
  return(curs);
}
  
VOID mac_do_cursor(StGWWinInfo *gwinfo) { DoCursor(gwinfo); }

LOCAL VOID DoCursor(StGWWinInfo *gwinfo)
{
  int left, top, width, height;
  Point pt;
  CursHandle curs = nil;
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  GetMouse(&pt);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);
    
  if (w == FrontWindow()
      && pt.h > left && pt.h < left + width
      && pt.v > top && pt.v < top + height)
    curs = get_cursor(StGWCursor(gwinfo));
  if (curs != nil) SetCursor(*curs);
  else SetCursor(&arrow);
}

VOID StGWSetCursor(StGWWinInfo *gwinfo, int cursor)
{
  if (gwinfo == nil) return;
  gwinfo->cursor = cursor;
}

int StGWCursor(StGWWinInfo *gwinfo)
{
  if (gwinfo == nil) return(FALSE);
  return(gwinfo->cursor);
}

