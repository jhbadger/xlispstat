#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

/**************************************************************************/
/**                                                                      **/
/**                    Action Installation Functions                     **/
/**                                                                      **/
/**************************************************************************/

VOID StGWSetFreeMem(StGWWinInfo *gwinfo, void (*FreeMem)(IVIEW_WINDOW))
{
  if (gwinfo == nil) return;
  else gwinfo->FreeMem =  FreeMem;
}

int StGWIdleOn(StGWWinInfo *gwinfo)
{
  
  if (gwinfo == nil) return(FALSE);
  else return(gwinfo->idleOn);
}

VOID StGWSetIdleOn(StGWWinInfo *gwinfo, int on)
{
  if (gwinfo != nil) gwinfo->idleOn = on;
}

/**************************************************************************/
/**                                                                      **/
/**                      Window Management Functions                     **/
/**                                                                      **/
/**************************************************************************/

VOID StGWShowWindow(StGWWinInfo *gwinfo)
{
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  else {
    MyShowWindow(w);
    if (! gwinfo->initialized)
      graph_update_action(gwinfo, TRUE);
  }
}

VOID StGWRemove(StGWWinInfo *gwinfo)
{
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  else SkelRmveWind(w);
}

VOID StGWWhileButtonDown(StGWWinInfo *gwinfo, void (*action) (WindowPtr,int, int),int motionOnly)
{
  Point pt;
  GrafPtr savePort;
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  
  GetPort(&savePort);
  SetPort(w);
  while (StillDown()) {
    SetPort(w);
    mac_do_cursor(gwinfo);
    GetMouse(&pt);
    if (gwinfo->mouse_x != pt.h || gwinfo->mouse_y != pt.v || ! motionOnly) {
      gwinfo->mouse_x = pt.h; gwinfo->mouse_y = pt.v;
      if (action != nil) (*action)(w, pt.h, pt.v);
    }
  }
  SetPort(savePort);
}

static TitleBarHeight(WindowPtr w)
{
  Rect r;
  Point pt;
  WindowPeek wind = (WindowPeek) w;
  GrafPtr savePort;
  
  GetPort(&savePort);
  SetPort(w);
  SetPt(&pt, 0, 0);
  LocalToGlobal(&pt);
  r = (*(wind->strucRgn))->rgnBBox;
  SetPort(savePort);
  return(pt.v - r.top);
}

VOID StWSetLocation(WindowPtr w, int left, int top, int frame)
{
  int adjust;
  
  if (w == nil) return;
  adjust = (frame) ? GetMBarHeight() + TitleBarHeight(w) : GetMBarHeight();
  MoveWindow(w, left, top + adjust, FALSE);
}

VOID StWGetLocation(WindowPtr w, int *left, int *top, int frame)
{
  GrafPtr savePort;
  Point pt;
  int adjust;
  
  if (w == nil) return;
  else {
    adjust = (frame) ? GetMBarHeight() + TitleBarHeight(w) : GetMBarHeight();
    GetPort(&savePort);
    SetPort(w);
    pt.h = w->portRect.left;
    pt.v = w->portRect.top;
    LocalToGlobal(&pt);
    if (left != nil) *left = pt.h;
    if (top != nil) *top = pt.v - adjust;
    SetPort(savePort);
  }
}

VOID StGWSetSize(StGWWinInfo *gwinfo, int width, int height, int frame)
{
  WindowPtr w;
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  else StWSetSize(w, width, height, frame);
}

VOID StWSetSize(WindowPtr w, int width, int height, int frame)
{
  if (w == nil) return;
  SizeWindow(w, width, height - ((frame) ? TitleBarHeight(w) : 0), TRUE);
  SkelTriggerUpdate(w);
  SkelDoUpdates();
}

VOID StWGetSize(WindowPtr w, int *width, int *height, int frame)
{
  
  if (w == nil) return;
  else {
    if (width != nil) *width = w->portRect.right - w->portRect.left;
    if (height != nil) *height = w->portRect.bottom - w->portRect.top 
                               + ((frame) ? TitleBarHeight(w) : 0);
  }
}

/**************************************************************************/
/**                                                                      **/
/**             Window State Access and Mutation Functions               **/
/**                                                                      **/
/**************************************************************************/

VOID SetHardwareState(StGWWinInfo *gwinfo)
{
  GrafPtr savePort;
  WindowPtr w;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;

  GetPort(&savePort);
  SetPort(w);
  
  set_fore_color(gwinfo);
  set_back_color(gwinfo);
  if (gwinfo->drawMode == 0) w->pnMode = patCopy;
  else w->pnMode = patXor;
  if (gwinfo->drawMode == 0) {
    w->txMode = srcOr;
    gwinfo->symbolMode = srcCopy;
  }
  else {
    w->txMode = srcXor;
    gwinfo->symbolMode = srcXor;
  }
  
  /* set line type */
  if (gwinfo->lineType != 0 && gwinfo->drawColor != gwinfo->backColor)
    PenPat(&gray);
  else PenPat(&black);
  
  /* set pen size */
  PenSize(gwinfo->lineWidth, gwinfo->lineWidth);
  
  adjust_graph_workport(gwinfo);
  SetPort(savePort);
}

static int get_state(StGWWinInfo *gwinfo, int which)
{
  if (gwinfo == nil) return(0);
  switch (which) {
  case 'W': return(gwinfo->canvasWidth);
  case 'H': return(gwinfo->canvasHeight);
  case 'L': return(gwinfo->lineType);
  case 'M': return(gwinfo->drawMode);
  case 'D': return(gwinfo->drawColor);
  case 'B': return(gwinfo->backColor);
  case 'C': return(gwinfo->use_color);
  }
  return 0;
}

int StGWCanvasWidth(StGWWinInfo *gwinfo)  { return (get_state(gwinfo, 'W')); }
int StGWCanvasHeight(StGWWinInfo *gwinfo) { return (get_state(gwinfo, 'H')); }
int StGWLineType(StGWWinInfo *gwinfo)     { return (get_state(gwinfo, 'L')); }
int StGWDrawMode(StGWWinInfo *gwinfo)     { return (get_state(gwinfo, 'M')); }

ColorCode StGWDrawColor(StGWWinInfo *gwinfo)
{
  return ((ColorCode) get_state(gwinfo, 'D'));
}

ColorCode StGWBackColor(StGWWinInfo *gwinfo)
{
  return ((ColorCode) get_state(gwinfo, 'B'));
}

int StGWUseColor(StGWWinInfo *gwinfo) { return (get_state(gwinfo, 'C')); }

VOID StGWGetLineWidth(StGWWinInfo *gwinfo, int *width)
{
  if (gwinfo == nil) return;
  if (width != nil) *width = gwinfo->lineWidth;
}

static VOID set_state(StGWWinInfo *gwinfo, int which, int value)
{
  int changed;
  
  if (gwinfo == nil) return;
  switch (which) {
  case 'L': if ((changed = (value != gwinfo->lineType))) gwinfo->lineType = value;  break;
  case 'M': if ((changed = (value != gwinfo->drawMode))) gwinfo->drawMode = value;  break;
  case 'D': if ((changed = (value != gwinfo->drawColor))) gwinfo->drawColor = value; break;
  case 'B': if ((changed = (value != gwinfo->backColor))) gwinfo->backColor = value; break;
  case 'C': if ((changed = (value != gwinfo->use_color))) 
              gwinfo->use_color = (StScreenHasColor()) ? value : FALSE;
            break;
  }
  if (changed) SetHardwareState(gwinfo);
  if (changed && which == 'B') DrawGWGrowBox(gwinfo);
}

VOID StGWSetLineType(StGWWinInfo *gwinfo, int type)
{
  set_state(gwinfo, 'L', type);
}

VOID StGWSetDrawMode(StGWWinInfo *gwinfo, int mode)
{
  set_state(gwinfo, 'M', mode);
}

VOID StGWSetDrawColor(StGWWinInfo *gwinfo, ColorCode color)
{
  set_state(gwinfo, 'D', (ColorCode) color);
}

VOID StGWSetBackColor(StGWWinInfo *gwinfo, ColorCode color)
{
  set_state(gwinfo, 'B', (ColorCode) color);
}

VOID StGWSetUseColor(StGWWinInfo *gwinfo, int use)
{
  set_state(gwinfo, 'C', use);
}

VOID StGWSetLineWidth(StGWWinInfo *gwinfo, int width)
{
  int changed;
  
  if (gwinfo == nil) return;
  changed = (width != gwinfo->lineWidth);
  if (changed) {
    gwinfo->lineWidth = width;
    SetHardwareState(gwinfo);
  }
}

VOID StGWReverseColors(StGWWinInfo *gwinfo)
{
  ColorCode backColor, drawColor;
  LVAL object;
  
  object = StGWGetObject(gwinfo);

  backColor = StGWBackColor(gwinfo);
  drawColor = StGWDrawColor(gwinfo);
  if (backColor != drawColor) {
    StGWSetBackColor(gwinfo, drawColor);
    StGWSetDrawColor(gwinfo, backColor);
    StGWObRedraw(object);
  }
}

VOID StGWGetViewRect(StGWWinInfo *gwinfo, int *left, int *top, int *width, int *height)
{
  WindowPtr w;
  
  if (gwinfo != nil && (w = gwinfo->window) != nil) {
    if (left != nil) *left = w->portRect.left;
    if (top != nil) *top = w->portRect.top;
    if (width != nil) *width = w->portRect.right - w->portRect.left - 15;
    if (height != nil) *height = w->portRect.bottom - w->portRect.top;
    if (height != nil && StGWHasHscroll(gwinfo)) *height -= 15;
  }
  else {
    if (left != nil) *left = 0;
    if (top != nil) *top = 0;
    if (width != nil) *width = 1;
    if (height != nil) *height = 1;
  }
}

/**************************************************************************/
/**                                                                      **/
/**                       Window Scrolling Functions                     **/
/**                                                                      **/
/**************************************************************************/

static VOID set_has_scroll(StGWWinInfo *gwinfo, int which, int has, int size)
{
  WindowPtr w;
  GrafPtr savePort;
  int view_size, value, old_has;
  ControlHandle ctl;
  LVAL object;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;

  GetPort(&savePort);
  SetPort(w);
  
  if (has && size <= 0) xlfail("size must be positive");
  object = StGWGetObject(gwinfo);

  ClipRect(&w->portRect);

  ctl = (which == 'H') ? gwinfo->hscroll : gwinfo->vscroll;
   
  old_has = (which == 'H') ? gwinfo->hasHscroll : gwinfo->hasVscroll;
  if (which == 'H') gwinfo->hasHscroll = has;
  else gwinfo->hasVscroll = has;
    
  if (has) {
    if (w == FrontWindow()) HiliteControl(ctl, 0);
    else HiliteControl(ctl, 255);
    if (which == 'H') {
      if (! old_has) gwinfo->canvasHeight -= 15;
      if (gwinfo->hasVscroll) {
        StGWGetViewRect(gwinfo, nil, nil, nil, &view_size);
        value = (gwinfo->canvasHeight - view_size > 0)
              ? gwinfo->canvasHeight - view_size : 0;
        SetControlMaximum(gwinfo->vscroll, value);
      }
      gwinfo->canvasWidth = size;
      StGWGetViewRect(gwinfo, nil, nil, &view_size, nil);
    }
    else {
      gwinfo->canvasHeight = size;
      StGWGetViewRect(gwinfo, nil, nil, nil, &view_size);
    }
    value = (size - view_size > 0) ? size - view_size : 0;
    SetControlMaximum(ctl, value);
    ShowControl(ctl);
  }
  else {
    if (which == 'H') {
      StGWGetViewRect(gwinfo, nil, nil, &gwinfo->canvasWidth, nil);
      if (old_has) gwinfo->canvasHeight += 15;
      StGWSetScroll(gwinfo, 0, gwinfo->view_v, TRUE);
    }
    else {
      StGWGetViewRect(gwinfo, nil, nil, nil, &gwinfo->canvasHeight);
      StGWSetScroll(gwinfo, gwinfo->view_h, 0, TRUE);
    }
    HideControl(ctl);
    SetControlMaximum(ctl, 0);
  }
  InvalRect(&w->portRect);
  reset_clip_rect(gwinfo);
  SetPort(savePort);

  StGWObResize(object);
}

VOID StGWSetHasHscroll(StGWWinInfo *gwinfo, int has, int size)
{
  set_has_scroll(gwinfo, 'H', has, size);
}

VOID StGWSetHasVscroll(StGWWinInfo *gwinfo, int has, int size)
{
  set_has_scroll(gwinfo, 'V', has, size);
}

int StGWHasHscroll(StGWWinInfo *gwinfo)
{
  if (gwinfo == nil) return(FALSE);
  else return(gwinfo->hasHscroll);
}

int StGWHasVscroll(StGWWinInfo *gwinfo)
{
  if (gwinfo == nil) return(FALSE);
  else return(gwinfo->hasVscroll);
}

VOID StGWSetScroll(StGWWinInfo *gwinfo, int h, int v, int move)
{
  WindowPtr w;
  GrafPtr savePort;
  Rect r;
  
  if (gwinfo == nil || (w = gwinfo->window) == nil) return;
  gwinfo->view_h = (gwinfo->hasHscroll) ? h : 0;
  gwinfo->view_v = (gwinfo->hasVscroll) ? v : 0;
    
  GetPort(&savePort);
  SetPort(w);
  ClipRect(&w->portRect);
  SetControlValue(gwinfo->hscroll, h);
  SetControlValue(gwinfo->vscroll, v);
  gwinfo->view_h = GetControlValue(gwinfo->hscroll);
  gwinfo->view_v = GetControlValue(gwinfo->vscroll);
  SetOrigin(gwinfo->view_h, gwinfo->view_v);
  reset_clip_rect(gwinfo);
  if (move) {
    r = scroll_bar_bounds(w, 'H');
    MoveControl(gwinfo->hscroll, r.left, r.top);
    r = scroll_bar_bounds(w, 'V');
    MoveControl(gwinfo->vscroll, r.left, r.top);
  }
  SetPort(savePort);
}

VOID StGWGetScroll(StGWWinInfo *gwinfo, int *h, int *v)
{
  
  if (gwinfo != nil) {
    if (h != nil) *h = gwinfo->view_h;
    if (v != nil) *v = gwinfo->view_v;
  }
}

VOID StGWSetHscrollIncs(StGWWinInfo *gwinfo, int inc, int pageInc)
{
  if (gwinfo == nil) return;
  gwinfo->h_scroll_inc[0] = inc;
  gwinfo->h_scroll_inc[1] = pageInc;
}

VOID StGWGetHscrollIncs(StGWWinInfo *gwinfo, int *inc, int *pageInc)
{
  if (gwinfo == nil) return;
  if (inc != 0) *inc = gwinfo->h_scroll_inc[0];
  if (pageInc != 0) *pageInc = gwinfo->h_scroll_inc[1];
}

VOID StGWSetVscrollIncs(StGWWinInfo *gwinfo, int inc, int pageInc)
{
  if (gwinfo == nil) return;
  gwinfo->v_scroll_inc[0] = inc;
  gwinfo->v_scroll_inc[1] = pageInc;
}

VOID StGWGetVscrollIncs(StGWWinInfo *gwinfo, int *inc, int *pageInc)
{
  if (gwinfo == nil) return;
  if (inc != 0) *inc = gwinfo->v_scroll_inc[0];
  if (pageInc != 0) *pageInc = gwinfo->v_scroll_inc[1];
}

/**************************************************************************/
/**                                                                      **/
/**                    Graph Window RefCon Functions                     **/
/**                                                                      **/
/**************************************************************************/

VOID StGWSetRefCon(StGWWinInfo *gwinfo, long x)
{
  if (gwinfo == nil) return;
  else gwinfo->RefCon = x;
}

long StGWGetRefCon(StGWWinInfo *gwinfo)
{
  if (gwinfo == nil) return((long) nil);
  else return(gwinfo->RefCon);
}
 
VOID StGWSetObject(StGWWinInfo *gwinfo, LVAL x)
{
  if (gwinfo == nil) return;
  else gwinfo->Object = x;
}

LVAL IViewWindowGetObject(WindowPtr w)
{
  StGWWinInfo *gwinfo = (StGWWinInfo *) GetWRefCon(w);
  if (gwinfo == nil) return(NIL);
  else return(gwinfo->Object);
}

LVAL StGWGetObject(StGWWinInfo *gwinfo)
{
  return((gwinfo != nil) ? gwinfo->Object : NIL);
}


/**************************************************************************/
/**                                                                      **/
/**                    Graph Window Color Functions                      **/
/**                                                                      **/
/**************************************************************************/

#define NumBasicColors 8
#define NumRGBColors 256
# define MULTIPLIER 62535

static int NumColors;

typedef struct {
  union {
    long old;
    RGBColor rgb;
  } value;
  long refcon;
} ctab_entry;

static ctab_entry *ctable;

VOID init_mac_colors(void)
{
  NumColors = NumBasicColors;
  if (StScreenHasColor()) NumColors += NumRGBColors;
  ctable = (ctab_entry *) StCalloc(NumColors, sizeof(ctab_entry));
  
  ctable[0].value.old = whiteColor;
  ctable[1].value.old = blackColor;
  ctable[2].value.old = redColor;
  ctable[3].value.old = greenColor;
  ctable[4].value.old = blueColor;
  ctable[5].value.old = cyanColor;
  ctable[6].value.old = magentaColor;
  ctable[7].value.old = yellowColor;
}

int StGWMakeColor(double red, double green, double blue, long refcon)
{
  int index;
  
  for (index = NumBasicColors; 
       index < NumColors && StGWGetColRefCon(index) != (long) nil; 
       index++);
  if (index >= NumColors) return(-1);
  else {
    StGWSetColRefCon(index, refcon);
    ctable[index].value.rgb.red = MULTIPLIER * red;
    ctable[index].value.rgb.green = MULTIPLIER * green;
    ctable[index].value.rgb.blue = MULTIPLIER * blue;
    return(index);
  }
}

VOID StGWFreeColor(unsigned int index)
{
  if (index < NumColors && index >= NumBasicColors)
    StGWSetColRefCon(index, (long) nil);
  else xlfail("can't free standard color");
}

VOID StGWSetColRefCon(unsigned int index, long rc)
{
  if (index < NumColors) ctable[index].refcon = rc;
}

long StGWGetColRefCon(unsigned int index)
{	
  if (index < NumColors) return(ctable[index].refcon);
  else return((long) nil);
}

static long get_color(unsigned int index)
{
  if (index < NumBasicColors) return(ctable[index].value.old);
  else return(ctable[1].value.old);
}

static RGBColor *get_rgb_color(unsigned int index)
{
  if (index >= NumColors) xlfail("bad rgb color");
  return(&ctable[index].value.rgb);
}

VOID set_fore_color(StGWWinInfo *gwinfo)
{
  if (gwinfo->drawColor < NumBasicColors)
    ForeColor(get_color(gwinfo->drawColor));
  else RGBForeColor(get_rgb_color(gwinfo->drawColor));
}

VOID set_back_color(StGWWinInfo *gwinfo)
{
  if (gwinfo->backColor < NumBasicColors)
    BackColor(get_color(gwinfo->backColor));
  else RGBBackColor(get_rgb_color(gwinfo->backColor));
}
