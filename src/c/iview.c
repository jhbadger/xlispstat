/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

#define LABEL_OFFSET 5

#ifdef USESTRINGS
extern char *IViewDataStringString();
extern double IViewDataStringValue(), IViewDataStringTransformedValue()
#endif /* USESTRINGS */

/**************************************************************************/
/**                                                                      **/
/**                    General IView Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

int StGrNumVariables P1C(StGWWinInfo *, gwinfo)
{
  return(IViewDataNumVariables((IViewData) StGrData(gwinfo)));
}

int IViewNumVariables P1C(IVIEW_WINDOW, w)
{
  return(IViewDataNumVariables(IViewDataPtr(w)));
}

VOID IViewSetVariableLabel P3C(IVIEW_WINDOW, w, int, var, char *, s)
{
  IViewDataSetVariableLabel(IViewDataPtr(w), var, s);
}

char *IViewVariableLabel P2C(IVIEW_WINDOW, w, int, var)
{
  return(IViewDataVariableLabel(IViewDataPtr(w), var));
}

VOID IViewSetRange P4C(IVIEW_WINDOW, w, int, var, double, low, double, high)
{
  double scale, shift;

  scale = IViewScale(w, var);
  shift = IViewShift(w, var);
  low = scale * low + shift;
  high = scale * high + shift;
  IViewSetScaledRange(w, var, low, high);
}

VOID IViewGetRange P4C(IVIEW_WINDOW, w, int, var, double *, low, double *, high)
{
  double scale, shift;

  scale = IViewScale(w, var);
  shift = IViewShift(w, var);
  IViewGetScaledRange(w, var, low, high);
  if (scale > 0.0) {
    if (low != NULL) *low = (*low - shift) / scale;
    if (high != NULL) *high = (*high - shift) / scale;
  }
}

VOID IViewSetScaledRange P4C(IVIEW_WINDOW, w, int, var, double, low, double, high)
{
  IViewDataSetRange(IViewDataPtr(w), var, low, high);
}

VOID IViewGetScaledRange P4C(IVIEW_WINDOW, w, int, var, double *, low, double *, high)
{
  IViewDataGetRange(IViewDataPtr(w), var, low, high);
}

VOID IViewSetScreenRange P4C(IVIEW_WINDOW, w, int, var, int, low, int, high)
{
  IViewDataSetScreenRange(IViewDataPtr(w), var, low, high);
}

VOID IViewGetScreenRange P4C(IVIEW_WINDOW, w, int, var, int *, low, int *, high)
{
  IViewDataGetScreenRange(IViewDataPtr(w), var, low, high);
}

VOID IViewSetIdentityTransformation P1C(IVIEW_WINDOW, w)
{
  IViewDataSetIdentityTransformation(IViewDataPtr(w));
}

VOID IViewSetTransformation P2C(IVIEW_WINDOW, w, double **, a)
{
  IViewDataSetTransformation(IViewDataPtr(w), a);
}

double **IViewTransformation P1C(IVIEW_WINDOW, w)
{
  return(IViewDataTransformation(IViewDataPtr(w)));
}

int IViewIsTransformed P1C(IVIEW_WINDOW, w)
{
  return(IViewDataIsTransformed(IViewDataPtr(w)));
}

VOID IViewApplyTransformation P3C(IVIEW_WINDOW, w, double **, a, int *, inbasis)
{
  IViewDataApplyTransformation(IViewDataPtr(w), a, inbasis);
}

double IViewEncodeValue P3C(IVIEW_WINDOW, w, double, value, int, var)
{
  double scale = IViewScale(w, var), shift = IViewShift(w, var);
  if (scale == 1.0 && shift == 0.0) return(value);
  else return(scale * value + shift);
}

double IViewDecodeValue P3C(IVIEW_WINDOW, w, double, value, int, var)
{
  double scale = IViewScale(w, var), shift = IViewShift(w, var);
  return((scale > 0.0) ? (value - shift) / scale : 0.0);
}

/**************************************************************************/
/**                                                                      **/
/**                      IView Point Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

VOID IViewAddPoints P2C(IVIEW_WINDOW, w, int, n)
{
  IViewDataAddPoints(IViewDataPtr(w), n);
}

VOID IViewClearPoints P1C(IVIEW_WINDOW, w)
{
  IViewDataClearPoints(IViewDataPtr(w));
}

int IViewNumPoints P1C(IVIEW_WINDOW, w)
{
  return(IViewDataNumPoints(IViewDataPtr(w)));
}

VOID IViewSetPointValue P4C(IVIEW_WINDOW, w, int, var, int, point, double, value)
{
  IViewDataSetPointValue(IViewDataPtr(w), var, point, IViewEncodeValue(w, value, var));
}

double IViewPointValue P3C(IVIEW_WINDOW, w, int, var, int, point)
{
  return(IViewDecodeValue(w, IViewDataPointValue(IViewDataPtr(w), var, point), var));
}

VOID IViewSetPointScaledValue P4C(IVIEW_WINDOW, w, int, var, int, point, double, value)
{
  IViewDataSetPointValue(IViewDataPtr(w), var, point, value);
}

double IViewPointScaledValue P3C(IVIEW_WINDOW, w, int, var, int, point)
{
  return(IViewDataPointValue(IViewDataPtr(w), var, point));
}

double IViewPointTransformedValue P3C(IVIEW_WINDOW, w, int, var, int, point)
{
  return(IViewDataPointTransformedValue(IViewDataPtr(w), var, point));
}

int IViewPointScreenValue P3C(IVIEW_WINDOW, w, int, var, int, point)
{
  return(IViewDataPointScreenValue(IViewDataPtr(w), var, point));
}

VOID IViewGetScreenPointValues P3C(IVIEW_WINDOW, w, int, point, int *, x)
{
  IViewDataGetScreenPointValues(IViewDataPtr(w), point, x);
}

VOID IViewSetPointMask P3C(IVIEW_WINDOW, w, int, point, int, masked)
{
  IViewDataSetPointMask(IViewDataPtr(w), point, masked);
}

int IViewPointMasked P2C(IVIEW_WINDOW, w, int, point)
{
  return(IViewDataPointMasked(IViewDataPtr(w),  point));
}

VOID IViewSetPointColor P3C(IVIEW_WINDOW, w, int, point, int, color)
{
  IViewDataSetPointColor(IViewDataPtr(w), point, color);
}

int IViewPointColor P2C(IVIEW_WINDOW, w, int, point)
{
  return(IViewDataPointColor(IViewDataPtr(w),  point));
}

VOID IViewSetPointState P3C(IVIEW_WINDOW, w, int, point, PointState, state)
{
  if (IViewPointState(w, point) != state) {
    IViewSetPointScreenState(w, point, IViewPointState(w, point));
    IViewDataSetPointState(IViewDataPtr(w), point, state);
    IViewAdjustOwnScreenPoint(w, point);
    IViewSetPointScreenState(w, point, IViewPointState(w, point));
    if (IViewIsLinked(w)) IViewMatchPointState(w, point);
  }
}

PointState IViewPointState P2C(IVIEW_WINDOW, w, int, point)
{
  return(IViewDataPointState(IViewDataPtr(w),  point));
}

VOID IViewSetPointScreenState P3C(IVIEW_WINDOW, w, int, point, PointState, state)
{
  IViewDataSetPointScreenState(IViewDataPtr(w), point, state);
}

PointState IViewPointScreenState P2C(IVIEW_WINDOW, w, int, point)
{
  return(IViewDataPointScreenState(IViewDataPtr(w),  point));
}

VOID IViewResetScreenStates P1C(IVIEW_WINDOW, w)
{
  IViewDataResetScreenStates(IViewDataPtr(w));
}

VOID IViewSetPointMark P3C(IVIEW_WINDOW, w, int, point, int, marked)
{
  IViewDataSetPointMark(IViewDataPtr(w), point, marked);
}

int IViewPointMarked P2C(IVIEW_WINDOW, w, int, point)
{
  return(IViewDataPointMarked(IViewDataPtr(w),  point));
}

VOID IViewClearPointMarks P1C(IVIEW_WINDOW, w)
{
  IViewDataClearPointMarks(IViewDataPtr(w));
}

VOID IViewSetPointLabel P3C(IVIEW_WINDOW, w, int, point, char *, s)
{
  IViewDataSetPointLabel(IViewDataPtr(w), point, s);
}

char *IViewPointLabel P2C(IVIEW_WINDOW, w, int, point)
{
  return(IViewDataPointLabel(IViewDataPtr(w), point));
}

VOID IViewSetPointSymbol P4C(IVIEW_WINDOW, w, int, point, int, sym, int, hsym)
{
  IViewDataSetPointSymbol(IViewDataPtr(w),  point, sym, hsym);
}

VOID IViewGetPointSymbol P4C(IVIEW_WINDOW, w, int, point, int *, sym, int *, hsym)
{
  IViewDataGetPointSymbol(IViewDataPtr(w),  point, sym, hsym);
}

/**************************************************************************/
/**                                                                      **/
/**                      IView Line Data Functions                       **/
/**                                                                      **/
/**************************************************************************/

int IViewNumLines P1C(IVIEW_WINDOW, w)
{
  return(IViewDataNumLines(IViewDataPtr(w)));
}

VOID IViewAddLines P2C(IVIEW_WINDOW, w, int, n)
{
  IViewDataAddLines(IViewDataPtr(w), n);
}

VOID IViewClearLines P1C(IVIEW_WINDOW, w)
{
  IViewDataClearLines(IViewDataPtr(w));
}

VOID IViewSetLineValue P4C(IVIEW_WINDOW, w, int, var, int, line, double, value)
{
  IViewDataSetLineValue(IViewDataPtr(w), var, line, IViewEncodeValue(w, value, var));
}

double IViewLineValue P3C(IVIEW_WINDOW, w, int, var, int, line)
{
  return(IViewDecodeValue(w, IViewDataLineValue(IViewDataPtr(w), var, line), var));
}

VOID IViewSetLineScaledValue P4C(IVIEW_WINDOW, w, int, var, int, line, double, value)
{
  IViewDataSetLineValue(IViewDataPtr(w), var, line, value);
}

double IViewLineScaledValue P3C(IVIEW_WINDOW, w, int, var, int, line)
{
  return(IViewDataLineValue(IViewDataPtr(w), var, line));
}

double IViewLineTransformedValue P3C(IVIEW_WINDOW, w, int, var, int, line)
{
  return(IViewDataLineTransformedValue(IViewDataPtr(w), var, line));
}

int IViewLineScreenValue P3C(IVIEW_WINDOW, w, int, var, int, line)
{
  return(IViewDataLineScreenValue(IViewDataPtr(w), var, line));
}

VOID IViewSetLineMask P3C(IVIEW_WINDOW, w, int, line, int, masked)
{
  IViewDataSetLineMask(IViewDataPtr(w), line, masked);
}

int IViewLineMasked P2C(IVIEW_WINDOW, w, int, line)
{
  return(IViewDataLineMasked(IViewDataPtr(w),  line));
}

VOID IViewSetLineColor P3C(IVIEW_WINDOW, w, int, line, int, color)
{
  IViewDataSetLineColor(IViewDataPtr(w), line, color);
}

int IViewLineColor P2C(IVIEW_WINDOW, w, int, line)
{
  return(IViewDataLineColor(IViewDataPtr(w),  line));
}

VOID IViewSetNextLine P3C(IVIEW_WINDOW, w, int, line, int, next)
{
  IViewDataSetNextLine(IViewDataPtr(w), line, next);
}

int IViewNextLine P2C(IVIEW_WINDOW, w, int, line)
{
  return(IViewDataNextLine(IViewDataPtr(w), line));
}

VOID IViewSetLineType P3C(IVIEW_WINDOW, w, int, line, int, type)
{
  IViewDataSetLineType(IViewDataPtr(w), line, type);
}

int IViewLineType P2C(IVIEW_WINDOW, w, int, line)
{
  return(IViewDataLineType(IViewDataPtr(w), line));
}

VOID IViewSetLineWidth P3C(IVIEW_WINDOW, w, int, line, int, width)
{
  IViewDataSetLineWidth(IViewDataPtr(w), line, width);
}

VOID IViewGetLineWidth P3C(IVIEW_WINDOW, w, int, line, int *, width)
{
  IViewDataGetLineWidth(IViewDataPtr(w), line, (unsigned *) width);
}

#ifdef USESTRINGS
/**************************************************************************/
/**                                                                      **/
/**                     IView String Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

IViewNumStrings P1C(IVIEW_WINDOW, w)
{
  return(IViewDataNumStrings(IViewDataPtr(w)));
}

IViewAddStrings P2C(IVIEW_WINDOW, w, int, n)
{
  IViewDataAddStrings(IViewDataPtr(w), n);
}

IViewClearStrings P1C(IVIEW_WINDOW, w)
{
  IViewDataClearStrings(IViewDataPtr(w));
}

IViewSetStringValue P4C(IVIEW_WINDOW, w, int, var, int, string, double, value)
{
  IViewDataSetStringValue(IViewDataPtr(w), var, string, IViewEncodeValue(w, value, var));
}

double IViewStringValue P3C(IVIEW_WINDOW, w, int, var, int, string)
{
  return(IViewDecodeValue(w, IViewDataStringValue(IViewDataPtr(w), var, string), var));
}

IViewSetStringScaledValue P4C(IVIEW_WINDOW, w, int, var, int, string, double, value)
{
  IViewDataSetStringValue(IViewDataPtr(w), var, string, value);
}

double IViewStringScaledValue P3C(IVIEW_WINDOW, w, int, var, int, string)
{
  return(IViewDataStringValue(IViewDataPtr(w), var, string));
}

double IViewStringTransformedValue P3C(IVIEW_WINDOW, w, int, var, int, string)
{
  return(IViewDataStringTransformedValue(IViewDataPtr(w), var, string));
}

IViewStringScreenValue P3C(IVIEW_WINDOW, w, int, var, int, string)
{
  return(IViewDataStringScreenValue(IViewDataPtr(w), var, string));
}

IViewSetStringMask P3C(IVIEW_WINDOW, w, int, string, int, masked)
{
  IViewDataSetStringMask(IViewDataPtr(w), string, masked);
}

IViewStringMasked P2C(IVIEW_WINDOW, w, int, string)
{
  return(IViewDataStringMasked(IViewDataPtr(w),  string));
}

IViewSetStringColor P3C(IVIEW_WINDOW, w, int, string, int, color)
{
  IViewDataSetStringColor(IViewDataPtr(w), string, color);
}

IViewStringColor P2C(IVIEW_WINDOW, w, int, string)
{
  return(IViewDataStringColor(IViewDataPtr(w),  string));
}

IViewSetStringString P3C(IVIEW_WINDOW, w, int, string, char *, str)
{
  IViewDataSetStringString(IViewDataPtr(w), string, str);
}

char *IViewStringString P2C(IVIEW_WINDOW, w, int, string)
{
  return(IViewDataStringString(IViewDataPtr(w), string));
}

IViewSetStringModifiers P4C(IVIEW_WINDOW, w, int, string, int, up, int, h, int, v)
{
  IViewDataSetStringModifiers(IViewDataPtr(w), string, up, h, v);
}

IViewGetStringModifiers P4C(IVIEW_WINDOW, w, int, string, int *, up, int *, h, int *, v)
{
  IViewDataGetStringModifiers(IViewDataPtr(w), string, up, h, v);
}
#endif /* USESTRINGS */
	
/**************************************************************************/
/**                                                                      **/
/**                     IView Data Drawing Functions                     **/
/**                                                                      **/
/**************************************************************************/

VOID IViewDrawDataPoints P5C(IVIEW_WINDOW, w,
                             unsigned, var1, unsigned, var2,
                             unsigned, m, unsigned, n)
{
  IViewDataDrawPoints(IViewDataPtr(w), w, var1, var2, m, n, LABEL_OFFSET);
}

VOID IViewDrawDataLines P5C(IVIEW_WINDOW, w,
                            unsigned, var1, unsigned, var2,
                            unsigned, m, unsigned, n)
{
  IViewDataDrawLines(IViewDataPtr(w), w, var1, var2, m, n);
}


#ifdef DODO
static IViewDrawDataLine P12C(IVIEW_WINDOW, w,
                              unsigned, var1, unsigned, var2, unsigned, line,
                              int, left, int, top, int, width, int, height,
                              int, orig_x, int, orig_y,
                              int, use_color, int, draw_color)
{
  int n = IViewNumLines(w);
  int x, y, nx, ny;
  int next;
/*  int right = left + width, bottom = top + height;*/
  int type, color, linewidth;
  char *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  if (line >= n || IViewLineMasked(w, line)) return;
  x = orig_x + IViewLineScreenValue(w, var1, line);
  y = orig_y - IViewLineScreenValue(w, var2, line);
/*  if (x < left || x > right) return;
  if (y < top || y > bottom) return;*/

  next = IViewNextLine(w, line);
  if (next >= n || next < 0 || IViewLineMasked(w, next)) return;
  nx = orig_x + IViewLineScreenValue(w, var1, next);
  ny = orig_y - IViewLineScreenValue(w, var2, next);
/*  if (nx < left || nx > right) return;
  if (ny < top || ny > bottom) return;*/
  
  IViewGetLineWidth(w, line, &linewidth);
  StGWSetLineWidth(gwinfo, linewidth);
  type = IViewLineType(w, line);
  if (use_color) {
    color = IViewLineColor(w, line);
    if (color != NOCOLOR) StGWSetDrawColor(gwinfo, color);
    else StGWSetDrawColor(gwinfo, draw_color);
  }
  StGWSetLineType(gwinfo, type);
  StGWDrawLine(gwinfo, x, y, nx, ny);
}

IViewDrawDataLines P5C(IVIEW_WINDOW, w,
                       unsigned, var1, unsigned, var2, unsigned, m, unsigned, n)
{
  int vars = IViewNumVariables(w);
  int i, left, top, width, height, x, y, use_color;
  int line_type, line_width, draw_color;
  StGWWinInfo *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  if (var1 >= vars || var2 >= vars) return;
  if (n > IViewNumLines(w)) return;
  
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &x, &y);
  use_color = StGWUseColor(gwinfo);
  line_type = StGWLineType(gwinfo);
  StGWGetLineWidth(gwinfo, &line_width);
  if (use_color) draw_color = StGWDrawColor(gwinfo);

  for (i = m; i < n; i++)
    IViewDrawDataLine(w, var1, var2, i, left, top, width, height, x, y,
                      use_color, draw_color);

  StGWSetLineType(gwinfo, line_type);
  StGWSetLineWidth(gwinfo, line_width);
  if (use_color) StGWSetDrawColor(gwinfo, draw_color);
}
#endif /* DODO */

#ifdef USESTRINGS
static IViewDrawDataString P12C(IVIEW_WINDOW, w,
                                unsigned, var1, unsigned, var2, unsigned, string,
                                int, left, int, top,int,  width, int, height,
                                int, orig_x, int, orig_y,
                                int, use_color, int, draw_color)
{
  int n = IViewNumStrings(w);
  int x, y;
/*  int right = left + width, bottom = top + height; not needed JKL */
  int up, h, v;
  int color;
  char *s;
  char *gwinfo = IViewWindowWinInfo(w);

  if (string >= n || IViewStringMasked(w, string)) return;
  x = orig_x + IViewStringScreenValue(w, var1, string);
  y = orig_y - IViewStringScreenValue(w, var2, string);
/*  if (x < left || x > right) return;
  if (y < top || y > bottom) return;*/
  
  if (use_color) {
    color = IViewStringColor(w, string);
    if (color != NOCOLOR) StGWSetDrawColor(gwinfo, color);
    else StGWSetDrawColor(gwinfo, draw_color);
  }
  IViewGetStringModifiers(w, string, &up, &h, &v);
  s = IViewStringString(w, string);
  if (s != NULL) {
    if (up) StGWDrawTextUp(gwinfo, s, x, y, h, v);
    else StGWDrawText(gwinfo, s, x, y, h, v);
  }
}

IViewDrawDataStrings P5C(IVIEW_WINDOW, w,
                         unsigned, var1, unsigned, var2, unsigned, m, unsigned, n)
{
  int vars = IViewNumVariables(w);
  int i, left, top, width, height, x, y, use_color, draw_color;
  char *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  if (var1 >= vars || var2 >= vars) return;
  if (n > IViewNumStrings(w)) return;
  
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &x, &y);
  use_color = StGWUseColor(gwinfo);
  if (use_color) draw_color = StGWDrawColor(gwinfo);
  for (i = m; i < n; i++)
    IViewDrawDataString(w, var1, var2, i, left, top,
                        width, height, x, y, use_color, draw_color);
  if (use_color) StGWSetDrawColor(gwinfo, draw_color);
}
#endif /* USESTRINGS */

VOID IViewDepthCuePoints P7C(IVIEW_WINDOW, w,
                             unsigned, var,
                             unsigned, cut1, unsigned, cut2, unsigned, cut3,
                             unsigned, m, unsigned, n)
{
  IViewDataCuePoints(IViewDataPtr(w), var, cut1, cut2, cut3, m, n);
}

/**************************************************************************/
/**                                                                      **/
/**                     Standard Callback Functions                      **/
/**                                                                      **/
/**************************************************************************/

VOID IViewStdResize P1C(IVIEW_WINDOW, w)
{
  int left, top, width, height, x, y, size;
  int m_left, m_top, m_right, m_bottom;
  int i , vars = IViewNumVariables(w);
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  width = StGWCanvasWidth(gwinfo);
  height = StGWCanvasHeight(gwinfo);
  StGrGetMargin(gwinfo, &m_left, &m_top, &m_right, &m_bottom);
  left = m_left;
  top = m_top;
  width -= m_left + m_right;
  height -= m_top + m_bottom;
  IViewGetAxisMargin(w, &m_left, &m_top, &m_right, &m_bottom);
  left += m_left;
  top += m_top;
  width -= m_left + m_right;
  height -= m_top + m_bottom;
  if (IViewFixedAspect(w)) {
    size = (width > height) ? height : width;
    left += (width - size) / 2;
    top += (height - size) / 2;
    StGrSetContentRect(gwinfo, left, top, size, size);
    StGrSetContentOrigin(gwinfo, left, top + size);
    for (i = 0; i < vars; i++) IViewSetScreenRange(w, i, 0, size);
  }
  else {
    StGrSetContentRect(gwinfo, left, top, width, height);
    StGrSetContentOrigin(gwinfo, left, top + height);
    StGrGetContentVariables(gwinfo, &x, &y);
    IViewSetScreenRange(w, x, 0, width);
    IViewSetScreenRange(w, y, 0, height);
    for (i = 0; i < vars; i++) 
      if (i != x && i != y)
	IViewSetScreenRange(w, i, 0, width);
  }
  IViewResizeOverlays(w);
}

VOID IViewStdRedraw P1C(IVIEW_WINDOW, w)
{
  int left, top, height, width;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  StGWStartBuffering(gwinfo);
  
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);  
  StGWSetClipRect(gwinfo, TRUE, left, top, width, height);
  IViewRedrawBackground(w);
  IViewRedrawOverlays(w);
  IViewRedrawContent(w);
  StGWBufferToScreen(gwinfo, left, top, width, height);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
  StGrSetDirty(gwinfo, FALSE);
}

VOID IViewStdRedrawBackground P1C(IVIEW_WINDOW, w)
{
  int left, top, height, width;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  StGWStartBuffering(gwinfo);
  
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);  
  StGWSetClipRect(gwinfo, TRUE, left, top, width, height);
  StGWEraseRect(gwinfo, left, top, width, height);
  IViewDrawAxes(w);
  StGWBufferToScreen(gwinfo, left, top, width, height);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
}

VOID IViewGetContentMarginRect P5C(IVIEW_WINDOW, w,
                                   int *, left, int *, top, int *,
                                   width, int *, height)
{
  int cleft, ctop, cwidth, cheight, mleft, mtop, mright, mbottom;
  int x_showing, y_showing;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  StGrGetContentRect(gwinfo, &cleft, &ctop, &cwidth, &cheight);
#ifdef DODO
  StGrGetMargin(gwinfo, &mleft, &mtop, &mright, &mbottom);
#endif /* DODO */
  IViewGetAxisMargin(w, &mleft, &mtop, &mright, &mbottom);
  IViewGetXaxis(w, &x_showing, NULL, NULL);
  IViewGetYaxis(w, &y_showing, NULL, NULL);
  if (y_showing || x_showing) {
    cwidth += mright;
    ctop -= mtop; cheight += mtop;
    if (! y_showing) { cleft -= mleft; cwidth += mleft; }
    if (! x_showing) cheight += mbottom;
  }
  if (left != NULL) *left = cleft;
  if (top != NULL) *top = ctop;
  if (width != NULL) *width = cwidth;
  if (height != NULL) *height = cheight;
}

VOID IViewStdClearContent P1C(IVIEW_WINDOW, w)
{
  int left, top, width, height;
  StGWWinInfo *gwinfo;
  
  gwinfo = IViewWindowWinInfo(w);
  IViewGetContentMarginRect(w, &left, &top, &width, &height);
  StGWEraseRect(gwinfo, left, top, width + 1, height + 1);
}

VOID IViewStdRedrawContent P1C(IVIEW_WINDOW, w)
{
  int left, top, width, height, vleft, vtop, vwidth, vheight;
  int x, y;
  StGWWinInfo *gwinfo;
  
  gwinfo = IViewWindowWinInfo(w);

  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  IViewGetContentMarginRect(w, &left, &top, &width, &height);
  StGrGetContentVariables(gwinfo, &x, &y);
  
  StGWStartBuffering(gwinfo);
  StGWSetClipRect(gwinfo, TRUE, left, top, width + 1, height + 1);
  /*StGWEraseRect(gwinfo, left, top, width + 1, height + 1);*/
  IViewClearContent(w);
  IViewDrawDataPoints(w, x, y, 0, IViewNumPoints(w));
  IViewDrawDataLines(w, x, y, 0, IViewNumLines(w));
#ifdef USESTRINGS
  IViewDrawDataStrings(w, x, y, 0, IViewNumStrings(w));
#endif /* USESTRINGS */
  StGWBufferToScreen(gwinfo, left, top, width + 1, height + 1);
  StGWGetViewRect(gwinfo, &vleft, &vtop, &vwidth, &vheight);
  StGWSetClipRect(gwinfo, TRUE, vleft, vtop, vwidth, vheight);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
  IViewResetScreenStates(w);
}

VOID IViewStdMarkPointsInRect P5C(IVIEW_WINDOW, w,
                                  int, left, int, top, int, width, int, height)
{
  int c_left, c_top, c_width, c_height, x, y, center_x, center_y;
  int i, n = IViewNumPoints(w), vars = IViewNumVariables(w);
  unsigned var1, var2;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  
  StGrGetContentRect(gwinfo, &c_left, &c_top, &c_width, &c_height);
  StGrGetContentOrigin(gwinfo, &center_x, &center_y);
  StGrGetContentVariables(gwinfo, (int *) &var1, (int *) &var2);
  
  if (var1 >= vars || var2 >= vars) return;
  
  for (i = 0; i < n; i++) {
    x = center_x + IViewPointScreenValue(w, var1, i);
    y = center_y - IViewPointScreenValue(w, var2, i);
    if ((x >= left && x <= left + width && y >= top && y <= top + height)
        && (! IViewPointMasked(w, i) && IViewPointState(w, i) != pointInvisible))
      IViewSetPointMark(w, i, TRUE);
    else IViewSetPointMark(w, i, FALSE);
  }
}

static VOID DrawPoint P12C(IVIEW_WINDOW, w, int, i, PointState, state,
                           int, var1, int, var2,
                           int, left, int, top, int, width, int, height,
                           int, center_x, int, center_y, int, use_color)
{
  int x, y, sym, hsym;
  int color=0, oldcolor=0;
  StGWWinInfo *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  if (use_color) {
    oldcolor = StGWDrawColor(gwinfo);
    color = IViewPointColor(w, i);
    if (color != NOCOLOR) StGWSetDrawColor(gwinfo, color);
  }
  x = center_x + IViewPointScreenValue(w, var1, i);
  y = center_y - IViewPointScreenValue(w, var2, i);
  IViewGetPointSymbol(w, i, &sym, &hsym);
  if (state == pointNormal) StGWReplaceSymbol(gwinfo, hsym, sym, x, y);
  else StGWReplaceSymbol(gwinfo, sym, hsym, x, y);
  if (use_color && color != NOCOLOR) StGWSetDrawColor(gwinfo, oldcolor);
}

static VOID DrawLabel P10C(IVIEW_WINDOW, w, int, i, int, var1, int, var2,
                           int, left, int, top, int, width, int, height,
                           int, center_x, int, center_y)
{
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  int x, y, mode = StGWDrawMode(gwinfo);

  StGWSetDrawMode(gwinfo, 1);
  x = center_x + IViewPointScreenValue(w, var1, i);
  y = center_y - IViewPointScreenValue(w, var2, i);
  StGWDrawString(gwinfo, IViewPointLabel(w, i), 
                           x + LABEL_OFFSET, y - LABEL_OFFSET);
  StGWSetDrawMode(gwinfo, mode);
}

VOID IViewStdAdjustScreen P1C(IVIEW_WINDOW, w)
{
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  if (StGrDirty(gwinfo)) IViewRedrawContent(w);
  StGrSetDirty(gwinfo, FALSE);
  IViewResetScreenStates(w);
}

VOID IViewStdAdjustPointsInRect P6C(IVIEW_WINDOW, w,
                                    int, left, int, top, int, width, int, height,
                                    PointState, state)
{
  int i, n = IViewNumPoints(w);
  PointState point_state;
  int masked, in_rect;
  
  IViewCheckLinks(w);
  
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  
  IViewMarkPointsInRect(w, left, top, width, height);
  for (i = 0; i < n; i++) {
    masked = IViewPointMasked(w, i);
    point_state = IViewPointState(w, i);
    if (! masked && point_state != pointInvisible) {
      in_rect = IViewPointMarked(w, i);
      if (in_rect && (int) point_state < (int) state) {
        IViewSetPointState(w, i, state);
      }
      else if (! in_rect 
	       && state == pointHilited && point_state == pointHilited) {
        IViewSetPointState(w, i, pointNormal);
      }
    }
  }
  IViewAdjustScreens(w);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
}


VOID IViewStdAdjustScreenPoint P2C(IVIEW_WINDOW, w, int, i)
{
  unsigned var1, var2;
  int left, top, width, height, x, y;
  PointState point_state, screen_state;
  int masked, showingLabels = IViewShowingLabels(w);
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  int use_color = StGWUseColor(gwinfo);
  
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &x, &y);
  StGrGetContentVariables(gwinfo, (int *) &var1, (int *) &var2);
  
  masked = IViewPointMasked(w, i);
  point_state = IViewPointState(w, i);
  screen_state = IViewPointScreenState(w, i);
  if (! masked && point_state != screen_state) {
    IViewSetPointScreenState(w, i, point_state);
	if (point_state == pointInvisible || screen_state == pointInvisible) {
	  StGrSetDirty(gwinfo, TRUE);
	}
    else if ((int) point_state > (int) screen_state) { 
      if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
      DrawPoint(w, i, point_state, var1, var2, left, top, width, height, x, y, use_color);
      if (showingLabels)
        DrawLabel(w, i, var1, var2, left, top, width, height, x, y); /* to draw */
      if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
    }
    else {
      if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
      if (showingLabels)
        DrawLabel(w, i, var1, var2, left, top, width, height, x, y); /* to erase */
      DrawPoint(w, i, point_state, var1, var2, left, top, width, height, x, y, use_color);
      if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
    }
  }
}

/**************************************************************************/
/**                                                                      **/
/**                       IView Rotation Functions                       **/
/**                                                                      **/
/**************************************************************************/

VOID IViewRotate2 P4C(IVIEW_WINDOW, w, unsigned, var1, unsigned, var2,
                      double, newalpha)
{
  static int *inbasis;
  static double **a;
  static int maxvars = 0;
  static double alpha = 0.0, s = 0.0, c = 1.0;
  int i, j, vars = IViewNumVariables(w);
  
  if (var1 >= vars || var2 >= vars) return;
  
  if (vars > maxvars) {
    maxvars = vars;
    if (a != NULL) StFree(a[0]);
    StFree(a);
    StFree(inbasis);
    a = (double **) StCalloc(sizeof(double *), maxvars);
    a[0] = (double *) StCalloc(sizeof(double), maxvars * maxvars);
    for (i = 1; i < vars; i++) a[i] = a[0] + i * maxvars;
    inbasis = (int *) StCalloc(sizeof(int), maxvars);
  }
  
  if (alpha != newalpha) {
    alpha = newalpha;
    s = sin(alpha);
    c = cos(alpha);
  }
  
  for (i = 0; i < vars; i++) {
    inbasis[i] = FALSE;
    for (j = 0; j < vars; j++) a[i][j] = (i == j) ? 1.0 : 0.0;
  }
  a[var1][var1] =  c; a[var1][var2] = -s;
  a[var2][var1] =  s; a[var2][var2] =  c;
  inbasis[var1] = TRUE; inbasis[var2] = TRUE;

  IViewApplyTransformation(w, a, inbasis);
}
  
/**************************************************************************/
/**                                                                      **/
/**                        Miscellaneous Functions                       **/
/**                                                                      **/
/**************************************************************************/

static double NiceValue P1C(double, x)
{
  long ilx;
  double lx, v1, v2, v3, v4;
  
  if (x <= 0.0) return (0.0);
  else {
    lx = log(x) / log(10.0);
    ilx = floor(lx);
    v1 = pow(10.0, (float) ilx);
    v2 = pow(10.0, (float) ilx) * 2.0;
    v3 = pow(10.0, (float) ilx) * 5.0;
    v4 = pow(10.0, (float) ilx + 1);
    
    if ((fabs(x - v1) < fabs(x - v2))
	&& (fabs(x - v1) < fabs(x - v3))
	&& (fabs(x - v1) < fabs(x - v4)))
      return(v1);
    else if ((fabs(x - v2) < fabs(x - v3))
	     && (fabs(x - v2) < fabs(x - v4)))
      return(v2);
    else if (fabs(x - v3) < fabs(x - v4))
      return(v3);
    else
      return(v4);
  }
}

VOID GetNiceRange P3C(double *, low, double *, high, int *, ticks)
{
  double delta;
  
#ifdef IEEEFP
  if (! is_finite(*high) || ! is_finite(*low)) return;
#else
  if (fabs(*high) >= HUGE || fabs(*low) >= HUGE) return;
#endif /* IEEEFP */
  if ((*high <= *low) || (*ticks < 2)) return;
  
  delta = NiceValue((*high - *low) / (*ticks - 1));
  if (delta <= 0.0) return;
  
  *low = floor(*low / delta + .01) * delta;   /* adjust by .01 for rounding */
  *high = ceil(*high / delta - .01) * delta;
  
  *ticks = 1 + (.01 + (*high - *low) / delta); /* add .01 for rounding */
}

