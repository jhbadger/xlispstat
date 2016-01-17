/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

#define BRUSH_WIDTH 20
#define BRUSH_HEIGHT 40
#define AXIS_LABEL_GAP 4
#define AXIS_TICK_LENGTH 3
#define AXIS_LABEL_TEMPLATE "12345"
#define CLICK_WIDTH 4
#define CLICK_HEIGHT 4

typedef struct brush {
  int left, top, width, height, showing;
} Brush;

typedef struct clickrange {
  int width, height;
} ClickRange;

typedef struct content {
  int left, top, width, height, origin_x, origin_y, x_variable, y_variable;
} Content;

typedef struct {
  int left, top, right, bottom;
} Margin;

typedef struct {
  int showing, labeled, ticks, height, edge;
} Axis;

typedef struct iview {
  IViewData data;
  Content content;
  Margin margin;
  Axis x_axis, y_axis;
  Brush brush;
  ClickRange clickrange;
  MouseMode mouseMode;
  int showingLabels, fixed_aspect, dirty;
  LVAL links;
  double *scale, *shift;
} *IView;

typedef IView StGrInfo;

#define IViewGetIView(w) ((IView) StGWGetRefCon(IViewWindowWinInfo(w)))

/**************************************************************************/
/**                                                                      **/
/**                       IView Creation Functions                       **/
/**                                                                      **/
/**************************************************************************/

VOID IViewFreeMem P1C(IVIEW_WINDOW, w)
{
  IView iview = IViewGetIView(w);

  if (iview != NULL) {
    if (IViewDataPtr(w) != NULL) {
      IViewDataFree(IViewDataPtr(w));
      IViewSetData(w, NULL);
    }
    StFree(iview->scale);
    StFree(iview->shift);
    StFree(iview);
    StGWSetRefCon(IViewWindowWinInfo(w), (long) NULL);
  }
}

IVIEW_WINDOW IViewNew P1C(LVAL, object)
{
  IVIEW_WINDOW w = (IVIEW_WINDOW) IViewWindowNew(object, FALSE);
  IView iview;
  StGWWinInfo *gwinfo;
  int vars, i;

  gwinfo = StGWObWinInfo(object);
  get_iview_ivars(object, &vars);

  iview = (IView) StCalloc(sizeof(struct iview), 1);
  StGWSetRefCon(gwinfo, (long) iview);
  IViewSetData(w, IViewDataNew(vars));
  iview->scale = (double *) (vars > 0 ? StCalloc(vars, sizeof(double)) : NULL);
  iview->shift = (double *) (vars > 0 ? StCalloc(vars, sizeof(double)) : NULL);
  StGWSetFreeMem(gwinfo, IViewFreeMem);
  StGrSetContentVariables(gwinfo, 0, 1);
  IViewSetBrush(w, 0, 0, BRUSH_WIDTH, BRUSH_HEIGHT);
  StGrSetClickRange(gwinfo, CLICK_WIDTH, CLICK_HEIGHT);
  IViewSetShowingLabels(w, FALSE);
  IViewSetMouseMode(w, selecting);
  IViewSetLinks(w, NIL);
  StGrSetMargin(gwinfo, 0, 0, 0, 0);
  IViewSetFixedAspect(w, TRUE);
  iview->brush.showing = FALSE;
  
  for (i = 0; i < vars; i++) {
    IViewSetScaledRange(w, i, 0.0, 1.0);
    IViewSetScale(w, i, 1.0);
    IViewSetShift(w, i, 0.0);
  }
  return(w);
}

/**************************************************************************/
/**                                                                      **/
/**                 IView State Accessors and Mutators                   **/
/**                                                                      **/
/**************************************************************************/

int StGrDirty P1C(StGWWinInfo *, gwinfo)
{
  StGrInfo gr = (StGrInfo) StGWGetRefCon(gwinfo);
  if (gr == NULL) xlfail("no graph installed in this window");
  return(gr->dirty);
}

VOID StGrSetDirty P2C(StGWWinInfo *, gwinfo, int, dirty) 
{
  StGrInfo gr = (StGrInfo) StGWGetRefCon(gwinfo);
  if (gr == NULL) xlfail("no graph installed in this window");
  gr->dirty = dirty;
}

static  StGrInfo StGrGetGrInfo P1C(StGWWinInfo *, gwinfo) 
{
  StGrInfo gr = (StGrInfo) StGWGetRefCon(gwinfo);
  if (gr == NULL) xlfail("no graph installed in this window");
  return(gr);
}

VOID StGrSetContentRect P5C(StGWWinInfo *, gwinfo,
                            int, left, int, top, int, width, int, height)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  gr->content.left = left; gr->content.top = top;
  gr->content.width = width; gr->content.height = height;
}

VOID StGrGetContentRect P5C(StGWWinInfo *, gwinfo,
                            int *, left, int *, top, int *, width, int *, height)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  if (left != NULL) *left = gr->content.left;
  if (top != NULL) *top = gr->content.top;
  if (width != NULL) *width = gr->content.width;
  if (height != NULL) *height = gr->content.height;
}

VOID StGrSetContentOrigin P3C(StGWWinInfo *, gwinfo, int, x, int, y)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  gr->content.origin_x = x;
  gr->content.origin_y = y;
}

VOID StGrGetContentOrigin P3C(StGWWinInfo *, gwinfo, int *, x, int *, y)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  if (x != NULL) *x = gr->content.origin_x;
  if (y != NULL) *y = gr->content.origin_y;
}

VOID StGrSetContentVariables P3C(StGWWinInfo *, gwinfo, int, x, int, y)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);
  int vars = StGrNumVariables(gwinfo);
  
  gr->content.x_variable = (vars > x && x >= 0) ? x : 0;
  gr->content.y_variable = (vars > y && y >= 0) ? y : 1;
}

VOID StGrGetContentVariables P3C(StGWWinInfo *, gwinfo, int *, x, int *, y)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  if (x != NULL) *x = gr->content.x_variable;
  if (y != NULL) *y = gr->content.y_variable;
}

VOID StGrSetClickRange P3C(StGWWinInfo *, gwinfo, int, width, int, height)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  gr->clickrange.width = width;
  gr->clickrange.height = height;
}

VOID StGrGetClickRange P3C(StGWWinInfo *, gwinfo, int *, width, int *, height)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);

  if (width != NULL) *width = gr->clickrange.width;
  if (height != NULL) *height = gr->clickrange.height;
}

VOID IViewSetMouseMode P2C(IVIEW_WINDOW, w, MouseMode, mode)
{
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  IView iview = IViewGetIView(w);
  if (iview == NULL) return;

  if (iview->mouseMode == brushing) IViewEraseBrush(w);
  iview->mouseMode = mode;
  if (iview->mouseMode == brushing) IViewDrawBrush(w);
  switch (mode) {
  case brushing:  StGWSetCursor(gwinfo, BRUSH_CURSOR); break;
  case usermode:  StGWSetCursor(gwinfo, HAND_CURSOR);  break;
  case selecting:
  default:        StGWSetCursor(gwinfo, ARROW_CURSOR);
  }
}

MouseMode IViewMouseMode P1C(IVIEW_WINDOW, w)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) return((MouseMode) 0);

  return(iview->mouseMode);
}

VOID IViewSetShowingLabels P2C(IVIEW_WINDOW, w, int, show)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) return;
  
  IViewUnselectAllPoints(w);
  iview->showingLabels = show;
}

int IViewShowingLabels P1C(IVIEW_WINDOW, w)
{
  IView iview = IViewGetIView(w);

  return(iview != NULL && iview->showingLabels);
}

VOID IViewSetData P2C(IVIEW_WINDOW, w, IViewData, data)
{
  IView iview = IViewGetIView(w);

  if (iview != NULL) iview->data = data;
}

char *StGrData P1C(StGWWinInfo *, gwinfo)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);
  if (gr->data == NULL) xlfail("No data in this IView");
  return((char *) gr->data);
}

IViewData IViewDataPtr P1C(IVIEW_WINDOW, w)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  if (iview->data == NULL) xlfail("No data in this IView");
  return(iview->data);
}

VOID StGrSetMargin P5C(StGWWinInfo *, gwinfo, int, left, int, top, int, right, int, bottom)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);
  
  gr->margin.left = left;
  gr->margin.top = top;
  gr->margin.right = right;
  gr->margin.bottom = bottom;
}

VOID StGrGetMargin P5C(StGWWinInfo *, gwinfo,
                       int *, left, int *, top,int *,  right, int *, bottom)
{
  StGrInfo gr = StGrGetGrInfo(gwinfo);
  
  if (left != NULL) *left = gr->margin.left;
  if (top != NULL) *top = gr->margin.top;
  if (right != NULL) *right = gr->margin.right;
  if (bottom != NULL) *bottom = gr->margin.bottom;
}

VOID IViewSetFixedAspect P2C(IVIEW_WINDOW, w, int, fixed)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  iview->fixed_aspect = fixed;
}

int IViewFixedAspect P1C(IVIEW_WINDOW, w)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  return(iview->fixed_aspect);
}

VOID IViewSetScale P3C(IVIEW_WINDOW, w, unsigned, var, double, scale)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  if (var >= IViewNumVariables(w) || scale <= 0.0) return;
  else iview->scale[var] = scale;
}

double IViewScale P2C(IVIEW_WINDOW, w, unsigned, var)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  if (var >= IViewNumVariables(w)) return(0.0);
  else return(iview->scale[var]);
}

VOID IViewSetShift P3C(IVIEW_WINDOW, w, unsigned, var, double, shift)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  if (var >= IViewNumVariables(w)) return;
  else iview->shift[var] = shift;
}

double IViewShift P2C(IVIEW_WINDOW, w, unsigned, var)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  if (var >= IViewNumVariables(w)) return(0.0);
  else return(iview->shift[var]);
}

/**************************************************************************/
/**                                                                      **/
/**                            Axis Functions                            **/
/**                                                                      **/
/**************************************************************************/

static VOID set_axis P5C(IVIEW_WINDOW, w,
                         int, which, int, showing, int, labeled, int, ticks)
{
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  IView iview = IViewGetIView(w);
  Axis *axis = NULL;
  
  if (iview == NULL) xlfail("No IView installed in this window");
   
  switch (which) {
  case 'X': axis = &iview->x_axis; break;
  case 'Y': axis = &iview->y_axis; break;
  }

  axis->showing = showing;
  axis->labeled = labeled;
  axis->ticks = ticks;

  if (axis->showing) {
    axis->height = StGWTextAscent(gwinfo)
                 + StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE) / 2
                 + AXIS_LABEL_GAP + AXIS_TICK_LENGTH;
    if (axis->labeled)
      axis->height += StGWTextAscent(gwinfo) + AXIS_LABEL_GAP;
    axis->edge = StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE);
  }
  else {
    axis->height = 0;
    axis->edge = 0;
  }
}  
  
VOID IViewGetAxisMargin P5C(IVIEW_WINDOW, w,
                            int *, left, int *, top, int *, right, int *, bottom)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");

  if (left != NULL) 
    *left = (iview->x_axis.edge > iview->y_axis.height)
          ? iview->x_axis.edge : iview->y_axis.height;
  if (bottom != NULL)
    *bottom = (iview->y_axis.edge > iview->x_axis.height)
            ? iview->y_axis.edge : iview->x_axis.height;
  if (top != NULL) *top = iview->y_axis.edge;
  if (right != NULL) *right = iview->x_axis.edge;
}

VOID IViewSetXaxis P4C(IVIEW_WINDOW, w, int, showing, int, labeled, int, ticks)
{
  set_axis(w, 'X', showing, labeled, ticks);
}

VOID IViewGetXaxis P4C(IVIEW_WINDOW, w, int *, showing, int *, labeled, int *, ticks)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  if (showing != NULL) *showing = iview->x_axis.showing;
  if (labeled != NULL) *labeled = iview->x_axis.labeled;
  if (ticks != NULL) *ticks = iview->x_axis.ticks;
}

VOID IViewSetYaxis P4C(IVIEW_WINDOW, w, int, showing, int, labeled, int, ticks)
{
  set_axis(w, 'Y', showing, labeled, ticks);
}

VOID IViewGetYaxis P4C(IVIEW_WINDOW, w, int *, showing, int *, labeled, int *, ticks)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  if (showing != NULL) *showing = iview->y_axis.showing;
  if (labeled != NULL) *labeled = iview->y_axis.labeled;
  if (ticks != NULL) *ticks = iview->y_axis.ticks;
}

static VOID draw_tick P5C(IVIEW_WINDOW, w, int, x, int, y, double, value, int, axis)
{
  char s[100];
  int offset;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  offset = StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE) / 3;
  switch (axis) {
  case 'X':
    offset += AXIS_TICK_LENGTH + StGWTextAscent(gwinfo);
    StGWDrawLine(gwinfo, x, y, x, y + AXIS_TICK_LENGTH);
    sprintf(s, "%.3g", value);
    StGWDrawText(gwinfo, s, x, y + offset, 1, 0);
    break;
  case 'Y':
    offset += AXIS_TICK_LENGTH + AXIS_LABEL_GAP;
    StGWDrawLine(gwinfo, x, y, x - AXIS_TICK_LENGTH, y);
    sprintf(s, "%.3g", value);
    StGWDrawTextUp(gwinfo, s, x - offset, y, 1, 0);
    break;
  }
}
  
VOID IViewDrawAxes P1C(IVIEW_WINDOW, w)
{
  IView iview = IViewGetIView(w);
  int left, top, width, height, right, bottom, x, y;
  double low, high, value;
  int offset, tick, i;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  if (iview == NULL) xlfail("No IView installed in this window");
  StGrGetContentVariables(gwinfo, &x, &y);
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  right = left + width;
  bottom = top + height;
  
  offset = StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE) / 3;
  if (iview->x_axis.showing) {
    StGWDrawLine(gwinfo, left, bottom + 1, right, bottom + 1);
    IViewGetRange(w, x, &low, &high);
    if (iview->x_axis.ticks >= 2) {
      draw_tick(w, left, bottom + 1, low, 'X'); 
      draw_tick(w, right, bottom + 1, high, 'X');
      for (i = 1; i < iview->x_axis.ticks - 1; i++) {
        tick = left + (((double) i) * width) / (iview->x_axis.ticks - 1);
        value = low + i * (high - low) / (iview->x_axis.ticks - 1);
        draw_tick(w, tick, bottom + 1, value, 'X');
      }
    }
    if (iview->x_axis.labeled) {
      offset += AXIS_TICK_LENGTH + AXIS_LABEL_GAP + 2 * StGWTextAscent(gwinfo);
      StGWDrawText(gwinfo, IViewVariableLabel(w, x),
                           (left + right) / 2, bottom + offset, 1, 0);
    }
  }
  offset = StGWTextWidth(gwinfo, AXIS_LABEL_TEMPLATE) / 3;
  if (iview->y_axis.showing) {
    StGWDrawLine(gwinfo, left - 1, bottom, left - 1, top);
    IViewGetRange(w, y, &low, &high);
    if (iview->y_axis.ticks >= 2) {
      draw_tick(w, left - 1, bottom, low, 'Y'); 
      draw_tick(w, left - 1, top, high, 'Y');
      for (i = 1; i < iview->y_axis.ticks - 1; i++) {
        tick = bottom - (((double) i) * height) / (iview->y_axis.ticks - 1);
        value = low + i * (high - low) / (iview->y_axis.ticks - 1);
        draw_tick(w, left - 1, tick, value, 'Y');
      }
    }
    if (iview->y_axis.labeled) {
      offset += AXIS_TICK_LENGTH + 2 * AXIS_LABEL_GAP + StGWTextAscent(gwinfo);
      StGWDrawTextUp(gwinfo, IViewVariableLabel(w, y),
                            left - offset, (top + bottom) / 2, 1, 0);
    }
  }
}

/**************************************************************************/
/**                                                                      **/
/**                           Brush Functions                            **/
/**                                                                      **/
/**************************************************************************/

VOID IViewSetBrush P5C(IVIEW_WINDOW, w,
                       int, x, int, y, int, width, int, height)
{
  IView iview = IViewGetIView(w);
  int showing = iview->brush.showing;
  if (iview == NULL) return;

  if (showing) IViewEraseBrush(w);
  iview->brush.left = x - width;
  iview->brush.top = y - height;
  iview->brush.width = width;
  iview->brush.height = height;
  if (showing) IViewDrawBrush(w);
}

VOID IViewGetBrush P5C(IVIEW_WINDOW, w,
                       int *, x, int *, y, int *, width, int *, height)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) return;

  if (x != NULL) *x = iview->brush.left + iview->brush.width;
  if (y != NULL) *y = iview->brush.top + iview->brush.height;
  if (width != NULL) *width = iview->brush.width;
  if (height != NULL) *height = iview->brush.height;
}

VOID IViewEraseBrush P1C(IVIEW_WINDOW, w)
{
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  IView iview = IViewGetIView(w);
  int mode, type;
  
  if (iview != NULL && iview->brush.showing) {
    mode = StGWDrawMode(gwinfo); StGWSetDrawMode(gwinfo, 1);
    type = StGWLineType(gwinfo); StGWSetLineType(gwinfo, 1);
    StGWFrameRect(gwinfo, iview->brush.left, iview->brush.top,
		                  iview->brush.width, iview->brush.height);
    iview->brush.showing = FALSE;
    StGWSetDrawMode(gwinfo, mode);
    StGWSetLineType(gwinfo, type);
  }
}

VOID IViewDrawBrush P1C(IVIEW_WINDOW, w)
{
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  IView iview = IViewGetIView(w);
  int mode, type;

  if (iview != NULL && ! iview->brush.showing) {
    mode = StGWDrawMode(gwinfo); StGWSetDrawMode(gwinfo, 1);
    type = StGWLineType(gwinfo); StGWSetLineType(gwinfo, 1);
    StGWFrameRect(gwinfo, iview->brush.left, iview->brush.top,
		                  iview->brush.width, iview->brush.height);
    iview->brush.showing = TRUE;
    StGWSetDrawMode(gwinfo, mode);
    StGWSetLineType(gwinfo, type);
  }
}

VOID IViewMoveBrush P3C(IVIEW_WINDOW, w, int, x, int, y)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) return;

  IViewEraseBrush(w);
  iview->brush.left = x - iview->brush.width;
  iview->brush.top = y - iview->brush.height;
  IViewDrawBrush(w);
}

/**************************************************************************/
/**                                                                      **/
/**                      Mouse Action Functions                          **/
/**                                                                      **/
/**************************************************************************/

static struct {
  int x, y, left, top, width, height;
} dragRect;

static VOID drag P3C(IVIEW_WINDOW, w, int, x, int, y)
{
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  if (dragRect.width != 0 && dragRect.height != 0) 
    StGWFrameRect(gwinfo, dragRect.left, dragRect.top, 
                          dragRect.width, dragRect.height);
  dragRect.width = abs(dragRect.x - x); 
  dragRect.height = abs(dragRect.y - y);
  dragRect.left = (x < dragRect.x) ? x : dragRect.x; 
  dragRect.top = (y < dragRect.y) ? y : dragRect.y; 
  if (dragRect.width != 0 && dragRect.height != 0) 
    StGWFrameRect(gwinfo, dragRect.left, dragRect.top, 
                          dragRect.width, dragRect.height);
}

VOID IViewStdSelectingMouseAction P5C(IVIEW_WINDOW, w, int, x, int, y,
                                      MouseEventType, type, MouseClickModifier, mods)
{
  int mode, line_type;
  int clickwidth, clickheight;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);

  if (type == MouseClick) {
    if (mods != ExtendModifier) IViewUnselectAllPoints(w);
    StGrGetClickRange(gwinfo, &clickwidth, &clickheight);
    IViewAdjustPointsInRect(w, x - clickwidth / 2, y - clickheight / 2,
                               clickwidth, clickheight, pointSelected);
    
    mode = StGWDrawMode(gwinfo); StGWSetDrawMode(gwinfo, 1);
    line_type = StGWLineType(gwinfo); StGWSetLineType(gwinfo, 1);
    dragRect.x = x; dragRect.y = y;
    dragRect.left = x, dragRect.top = y;
    dragRect.width = 0; dragRect.height = 0;
    StGWWhileButtonDown(gwinfo, drag, TRUE);
    if (dragRect.width != 0 && dragRect.height != 0)
    StGWFrameRect(gwinfo, dragRect.left, dragRect.top, 
                          dragRect.width, dragRect.height);
    StGWSetDrawMode(gwinfo, mode);
    StGWSetLineType(gwinfo, line_type);

    IViewAdjustPointsInRect(w, dragRect.left, dragRect.top,
				               dragRect.width, dragRect.height, pointSelected);
  }
}

static VOID dragbrush P3C(IVIEW_WINDOW, w, int, x, int, y)
{
  IView iview = IViewGetIView(w);
  
  IViewMoveBrush(w, x, y);
  IViewAdjustPointsInRect(w, iview->brush.left, iview->brush.top,
                             iview->brush.width, iview->brush.height,
                             pointSelected);
}

VOID IViewStdBrushingMouseAction P5C(IVIEW_WINDOW, w, int, x, int, y,
                                     MouseEventType, type, MouseClickModifier, mods)
{
  IView iview = IViewGetIView(w);
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);

  IViewMoveBrush(w, x, y);
  if (type == MouseClick) {
    if (mods != ExtendModifier) IViewUnselectAllPoints(w);
    StGWWhileButtonDown(gwinfo, dragbrush, TRUE);
  }
  else if (type == MouseMove) {
    IViewMoveBrush(w, x, y);
    IViewAdjustPointsInRect(w, iview->brush.left, iview->brush.top,
                               iview->brush.width, iview->brush.height, pointHilited);
  }
}

VOID IViewStdMouseAction P5C(IVIEW_WINDOW, w, int, x, int, y,
                             MouseEventType, type,MouseClickModifier, mods)
{
  switch (IViewMouseMode(w)) {
  case selecting: IViewStdSelectingMouseAction(w, x, y, type, mods); break;
  case brushing:  IViewStdBrushingMouseAction(w, x, y, type, mods); break;
  case usermode:  break; /* to keep compiler happy */
  }
}

VOID IViewStdUnselectAllPoints P1C(IVIEW_WINDOW, w)
{
  int i, n = IViewNumPoints(w);
  IViewCheckLinks(w);
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++)
    if ((int) IViewPointState(w, i) > (int) pointNormal 
        && ! IViewPointMasked(w, i)) 
      IViewSetPointState(w, i, pointNormal);
  IViewAdjustScreens(w);
}

VOID IViewEraseSelection P1C(IVIEW_WINDOW, w)
{
  int n = IViewNumPoints(w), i;
  IViewCheckLinks(w);
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++) 
    if (IViewPointState(w, i) == pointSelected)
      IViewSetPointState(w, i, pointInvisible);
  IViewAdjustScreens(w);
}

VOID IViewMaskSelection P1C(IVIEW_WINDOW, w)
{
  int n = IViewNumPoints(w), i;
  
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++) 
    if (IViewPointState(w, i) == pointSelected)
      IViewSetPointMask(w, i, TRUE);
  IViewRedrawContent(w);
}

VOID IViewUnmaskAllPoints P1C(IVIEW_WINDOW, w)
{
  int n = IViewNumPoints(w), i;
  
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++) IViewSetPointMask(w, i, FALSE);
  IViewRedrawContent(w);
}

VOID IViewShowAllPoints P1C(IVIEW_WINDOW, w)
{
  int n = IViewNumPoints(w), i;
  IViewCheckLinks(w);
  IViewClearPointMarks(w);
  for (i = 0; i < n; i++) IViewSetPointState(w, i, pointNormal);
  IViewAdjustScreens(w);
}

int IViewAllPointsShowing P1C(IVIEW_WINDOW, w)
{
  int result = TRUE, n = IViewNumPoints(w), i;
  
  for (i = 0; i < n && result; i++)
    if (IViewPointState(w, i) == pointInvisible) result = FALSE;
  return(result);
}

int IViewAllPointsUnmasked P1C(IVIEW_WINDOW, w)

{
  int result = TRUE, n = IViewNumPoints(w), i;
  
  for (i = 0; i < n && result; i++)
    if (IViewPointMasked(w, i)) result = FALSE;
  return(result);
}

int IViewAnyPointsSelected P1C(IVIEW_WINDOW, w)
{
  int result = FALSE, n = IViewNumPoints(w), i;
  
  for (i = 0; i < n && ! result; i++)
    if (IViewPointState(w, i) == pointSelected) result = TRUE;
  return(result);
}

/*************************************************************************/
/**                                                                     **/
/**                      IView Linking Functions                        **/
/**                                                                     **/
/*************************************************************************/

/**** storing links in internal structure is risky because of possible GC */

VOID IViewSetLinks P2C(IVIEW_WINDOW, w, LVAL, links)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  iview->links = links;
}

LVAL IViewGetLinks P1C(IVIEW_WINDOW, w)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) xlfail("No IView installed in this window");
  return(iview->links);
}

int IViewIsLinked P1C(IVIEW_WINDOW, w)
{
  IView iview = IViewGetIView(w);
  if (iview == NULL) return(FALSE);
  else return(iview->links != NIL);
}
