/* xshistogram - XLISP interface to IVIEW dynamic graphics package.    */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

#define HIST_MAX_BINS 50
#define HIST_INITIAL_BINS 6
#define NOT_IN_HIST -2
#define NO_MORE_POINTS -1
#define BIN_POINTER -1
#define HIST_CLICK_WIDTH 1
#define HIST_CLICK_HEIGHT 1

typedef struct hist_point {
  int left, top, width, height, next;
} HistPoint;

typedef struct hist_bin {
  int low, high, count, height, smallest;
} Bin;

typedef struct hist {
  Bin bins[HIST_MAX_BINS];
  char *point_data;
  int num_bins, num_showing;
} *IViewHist;

extern LVAL s_histogram_internals, sk_draw, sk_redraw, sk_resize, s_true,
  s_scale_type, sk_show, sk_show_window;

extern LVAL xsapplysubr(), iview_isnew(), makearglist();

/* forward declarations */
LOCAL VOID bad_hist_data(V);
LOCAL VOID IViewHistSetNumBins P3H(LVAL, LVAL, int);
LOCAL VOID clear_bins P1H(LVAL);
LOCAL VOID draw_hist P2H(IVIEW_WINDOW, LVAL);
LOCAL VOID initialize_points P4H(IVIEW_WINDOW, LVAL, int, int);
LOCAL int insert_point P5H(IVIEW_WINDOW, LVAL, int, int, int);
LOCAL int remove_non_showing_points P2H(IVIEW_WINDOW, LVAL);
LOCAL VOID find_point_rects P2H(IVIEW_WINDOW, LVAL);
LOCAL VOID hilite_points P2H(IVIEW_WINDOW, LVAL);
LOCAL int sect_point_rect P6H(LVAL, int, int, int, int, int);
LOCAL int sort_bins P2H(IVIEW_WINDOW, LVAL);
LOCAL VOID size_bins P2H(IVIEW_WINDOW, LVAL);
LOCAL VOID draw_hist_point_state P4H(IVIEW_WINDOW, LVAL, int, int);
LOCAL VOID set_density_range P3H(IVIEW_WINDOW, int, double);

static LVAL gethistdata P1C(LVAL, object)
{
  LVAL val;
  
  val = slot_value(object, s_histogram_internals);
  if (! consp(val) || !adatap(car(val))) bad_hist_data();
  return(val);
}

static HistPoint *getpointdata P1C(LVAL, hdata)
{
  if (adatap(cdr(hdata)))
    return((HistPoint *) StRPtr(getadaddr(cdr(hdata))));
  else return(NULL);
}
  
static IViewHist getinternals P1C(LVAL, hdata)
{
  if (getadaddr(car(hdata)) == NULL) bad_hist_data();
  return((IViewHist) getadaddr(car(hdata)));
}

static VOID gethistargs P3C(IVIEW_WINDOW *, w, LVAL *, object, LVAL *, hdata)
{
  *object = xlgaobject();
  if (w != NULL) *w = (IVIEW_WINDOW) GETIVIEWADDRESS(*object);
  if (hdata != NULL) *hdata = gethistdata(*object);
}

VOID newhistinternals P1C(LVAL, object)
{
  LVAL val;
  
  xlsave1(val);
  val = newadata(sizeof(struct hist), 1, FALSE);
  val = consa(val);
  set_slot_value(object, s_histogram_internals, val);
  xlpop();
}

static VOID allocate_internal_points P2C(LVAL, object, int, n)
{
  LVAL val;
  val = gethistdata(object);
  if (adatap(cdr(val)))
    reallocaddata(cdr(val), sizeof(struct hist_point), n);
  else
    rplacd(val, newadata(sizeof(struct hist_point), n, TRUE));
}  

static VOID clear_internal_points P1C(LVAL, object)
{
  LVAL val;
  
  val = slot_value(object, s_histogram_internals);
  if (! consp(val) || ! adatap(cdr(val))) bad_hist_data();
  freeadata(cdr(val));
}

LOCAL VOID bad_hist_data(V) { xlfail("bad internal histogram data"); }

/**************************************************************************/
/**                                                                      **/
/**                     Histogram Creation Functions                     **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_hist_isnew(V)
{
  LVAL object, args, hdata;
  int vars;
  
  object = xlgaobject();
  vars = getfixnum(xlgafixnum());
  if (vars < 1) xlfail("bad number of variables for histogram");
  
  newhistinternals(object);
  hdata = gethistdata(object);
  IViewHistSetNumBins(object, hdata, HIST_INITIAL_BINS);
  clear_bins(hdata);

  xlsave1(args);
  args = makearglist(xlargc, xlargv);
  args = cons(cvfixnum((FIXTYPE) (vars + 1)), args);
  args = cons(object, args);
  xsapplysubr(iview_isnew, args);
  xlpop();

  return(object);
}

LVAL iview_hist_allocate(V)
{
  LVAL object;
  IVIEW_WINDOW w;
  int vars, show;
  StGWWinInfo *gwinfo;

  object = xlgaobject();
  show = xsboolkey(sk_show, TRUE);

  gwinfo = StGWObWinInfo(object);
  w = IViewNew(object);
  initialize_iview(w, object);

  vars = IViewNumVariables(w);
  StGrSetClickRange(gwinfo, HIST_CLICK_WIDTH, HIST_CLICK_HEIGHT);
  StGrSetContentVariables(gwinfo, 0, vars - 1);

  /* use StShowWindow to show (map) window but NOT send :resize or :redraw */
  if (show) StShowWindow(w);
  
  return(object);
}

/**************************************************************************/
/**                                                                      **/
/**                     State Accessors and Mutators                     **/
/**                                                                      **/
/**************************************************************************/

LOCAL VOID IViewHistSetNumBins P3C(LVAL, object, LVAL, hdata, int, n)
{
  IViewHist h = getinternals(hdata);
  
  if (n < 1 || n > HIST_MAX_BINS) return;
  h->num_bins = n;
  send_message(object, sk_resize);
}

static int IViewHistNumBins P1C(LVAL, hdata)
{
  IViewHist h = getinternals(hdata);
  
  return(h->num_bins);
}
  
LVAL iview_hist_num_bins(V)
{
  LVAL object, hdata;
  IVIEW_WINDOW w;
  int bins;
  
  gethistargs(&w, &object, &hdata);
  if (moreargs()) {
    bins = getfixnum(xlgafixnum());
    IViewHistSetNumBins(object, hdata, bins);
    check_redraw(object, TRUE, TRUE);
  }
  return(cvfixnum((FIXTYPE) IViewHistNumBins(hdata)));
}

LVAL iview_hist_bin_counts(V)
{
  LVAL object, hdata, result, next;
  IVIEW_WINDOW w;
  int i, bins;
  IViewHist h;
  
  gethistargs(&w, &object, &hdata);
  xllastarg();
  
  if (hdata == NULL || (h = getinternals(hdata)) == NULL) result = NIL;
  else {
    bins = h->num_bins;
    xlsave1(result);
    result = mklist(bins, NIL);
    for (i = 0, next = result; i < bins; i++, next = cdr(next))
      rplaca(next, cvfixnum((FIXTYPE) h->bins[i].count));
    xlpop();
  }
  return(result);
}

/**************************************************************************/
/**                                                                      **/
/**                            Data Functions                            **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_hist_add_points(V)
{
  IVIEW_WINDOW w;
  int old_n, n;
  LVAL object, data, hdata;
  
  gethistargs(&w, &object, &hdata);
  if (IVIEW_WINDOW_NULL(w)) return(NIL);
  
  old_n = IViewNumPoints(w);
  
  xlsave1(data);
  data = xlgetarg();
  data = (fixp(data) || (consp(data) && seqp(car(data)))) 
       ? data : consa(data);
  internal_iview_add_points(w, object, data);
  xlpop();
  
  n = IViewNumPoints(w);
  allocate_internal_points(object, n);
  initialize_points(w, hdata, old_n, n);
  
  check_add_to_screen(object, 'P', old_n, n, TRUE);
  
  return(NIL);
}

LVAL iview_hist_clear_points(V)
{
  IVIEW_WINDOW w;
  LVAL object, hdata;
  
  gethistargs(&w, &object, &hdata);
 
  if (! IVIEW_WINDOW_NULL(w)) {
    IViewClearPoints(w);
    clear_bins(hdata);
    clear_internal_points(object);
    check_redraw(object, TRUE, TRUE);
  }
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                    Drawing and Resizing Functions                    **/
/**                                                                      **/
/**************************************************************************/

static VOID IViewHistResize P2C(IVIEW_WINDOW, w, LVAL, hdata)
{
  IViewStdResize(w);
  clear_bins(hdata);
  initialize_points(w, hdata, 0, IViewNumPoints(w));
}

static VOID IViewHistRedrawContent P2C(IVIEW_WINDOW, w, LVAL, hdata)
{
  int left, top, width, height, x, y, vleft, vtop, vwidth, vheight;
  StGWWinInfo *gwinfo;
  
  gwinfo = IViewWindowWinInfo(w);

  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  IViewGetContentMarginRect(w, &left, &top, &width, &height);
  StGrGetContentVariables(gwinfo, &x, &y);
  
  StGWStartBuffering(gwinfo);
  StGWSetClipRect(gwinfo, TRUE, left, top, width + 1, height + 1);
  /*StGWEraseRect(gwinfo, left, top, width + 1, height + 1);*/
  IViewClearContent(w);

  sort_bins(w, hdata);
  size_bins(w, hdata);
  draw_hist(w, hdata);  
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

LVAL iview_hist_resize(V)
{
  LVAL object, hdata;
  IVIEW_WINDOW w;

  gethistargs(&w, &object, &hdata);
  if (! IVIEW_WINDOW_NULL(w)) IViewHistResize(w, hdata);
  return(NIL);
}

LVAL iview_hist_redraw_content(V)
{
  LVAL object, hdata;
  IVIEW_WINDOW w;

  gethistargs(&w, &object, &hdata);
  if (! IVIEW_WINDOW_NULL(w)) IViewHistRedrawContent(w, hdata);
  return(NIL);
}

LVAL iview_hist_adjust_to_data(V)
{
  LVAL object;
  IVIEW_WINDOW w;
  StGWWinInfo *gwinfo;
  double low, high;
  int ticks, labeled, x, y, scaled, bins;
  char *label;
  LVAL arg, hdata;
  
  gethistargs(&w, &object, &hdata);

  if (! IVIEW_WINDOW_NULL(w)) {
    gwinfo = StGWObWinInfo(object);
    StGrObAdjustToData(object, FALSE);
    scaled = (slot_value(object, s_scale_type) != NIL) ? TRUE : FALSE;
    
    StGrGetContentVariables(gwinfo, &x, &y);
    IViewGetRange(w, x, &low, &high);
    label = IViewVariableLabel(w, x);
    labeled = (label != NULL && strlen(label) != 0) ? TRUE : FALSE;
    ticks = 4;
    GetNiceRange(&low, &high, &ticks);
    IViewSetRange(w, x, low, high);
    IViewSetXaxis(w, ! scaled, labeled, ticks);
    bins = 5 + log(1.0 + IViewNumPoints(w));
    GetNiceRange(&low, &high, &bins);
    if (bins > 1) bins --;
    if (bins > 30) bins = 30;
    IViewHistSetNumBins(object, hdata, bins);

    if (! xlgetkeyarg(sk_draw, &arg)) arg = s_true;
    if (arg != NIL) send_message(object, sk_resize);
    if (arg != NIL) send_message(object, sk_redraw);  
  }
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                           Mouse Functions                            **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_hist_adjust_screen(V)
{
  IVIEW_WINDOW w;
  LVAL object, hdata;
  int x, y, lines_only = FALSE;
  StGWWinInfo *gwinfo;

  gethistargs(&w, &object, &hdata);

  if (IVIEW_WINDOW_NULL(w)) return(NIL);
  gwinfo = StGWObWinInfo(object);
  if (StGrDirty(gwinfo)) {
	StGrSetDirty(gwinfo, FALSE);
    if (lines_only) {
      if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
      StGrGetContentVariables(gwinfo, &x, &y);
      IViewDrawDataLines(w, x, y, 0, IViewNumLines(w));
#ifdef USESTRINGS
      IViewDrawDataStrings(w, x, y, 0, IViewNumStrings(w));
#endif /* USESTRINGS */
      if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
	}
	else {
      IViewHistResize(w, hdata);
      IViewHistRedrawContent(w, hdata);
    }
  }
  return(NIL);
}

LVAL iview_hist_adjust_points_in_rect(V)
{
  int  i, n, in_rect;
  PointState point_state;
  IVIEW_WINDOW w;
  int left, top, width, height;
  PointState state;
  LVAL object, hdata;
  
  gethistargs(&w, &object, &hdata);

  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());
  state = decode_point_state(xlgetarg());

  if (IVIEW_WINDOW_NULL(w)) return(NIL);
  
  IViewCheckLinks(w);
  n = IViewNumPoints(w);
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);

  for (i = 0; i < n; i++) {
    point_state = IViewPointState(w, i);
    if (! IViewPointMasked(w, i) && point_state != pointInvisible) {
      in_rect = sect_point_rect(hdata, i, left, top, width, height);
      if (in_rect && (int) point_state < (int) state) {
        IViewSetPointState(w, i, state);
      }
      else if (! in_rect
	       && state == pointHilited
	       && point_state == pointHilited) {
        IViewSetPointState(w, i, pointNormal);
      }
    }
  }
  IViewAdjustScreens(w);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
  return(NIL);
}

LVAL iview_hist_mark_points_in_rect(V)
{
  int  i, n, in_rect;
  PointState point_state;
  IVIEW_WINDOW w;
  int left, top, width, height;
  LVAL object, hdata;
  
  gethistargs(&w, &object, &hdata);

  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());

  if (IVIEW_WINDOW_NULL(w)) return(NIL);
  
  n = IViewNumPoints(w);

  for (i = 0; i < n; i++) {
    point_state = IViewPointState(w, i);
    if (! IViewPointMasked(w, i) && point_state != pointInvisible) {
      in_rect = sect_point_rect(hdata, i, left, top, width, height);
      IViewSetPointMark(w, i, in_rect);
    }
  }
  return(NIL);
}

LVAL iview_hist_adjust_screen_point(V)
{
  IVIEW_WINDOW w;
  LVAL object, hdata;
  int point;
  PointState state, screen_state;
  
  gethistargs(&w, &object, &hdata);
  point = getfixnum(xlgafixnum());
  
  if (! IVIEW_WINDOW_NULL(w) && ! IViewPointMasked(w, point)) {
    state = IViewPointState(w, point);
    screen_state = IViewPointScreenState(w, point);
	if (state == pointInvisible || screen_state == pointInvisible) {
	  StGrSetDirty(StGWObWinInfo(object), TRUE);
	}
    else {
      draw_hist_point_state(w, hdata, point, state);
    }  
  }
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                         Internal Functions                           **/
/**                                                                      **/
/**************************************************************************/

LOCAL VOID clear_bins P1C(LVAL, hdata)
{
  IViewHist h = getinternals(hdata);
  int i;
   
  for (i = 0; i < h->num_bins; i++) {
    h->bins[i].low = 0;
    h->bins[i].high = 0;
    h->bins[i].count = 0;
    h->bins[i].height = 0;
    h->bins[i].smallest = NO_MORE_POINTS;
  }
}
  
LOCAL int sort_bins P2C(IVIEW_WINDOW, w, LVAL, hdata)
{
  IViewHist h = getinternals(hdata);
  int i, k, low, high, val, x, y, n = IViewNumPoints(w);
  int changed = FALSE;
  StGWWinInfo *gwinfo;
  
  gwinfo = IViewWindowWinInfo(w);

  StGrGetContentVariables(gwinfo, &x, &y);
  IViewGetScreenRange(w, x, &low, &high);
  if (high <= low) return(FALSE);
  if (remove_non_showing_points(w, hdata)) changed = TRUE;
  for (i = 0; i < h->num_bins; i++) h->bins[i].count = 0;
  h->num_showing = 0;
  for (i = 0; i < n; i++) {
    if (! IViewPointMasked(w, i) && IViewPointState(w, i) != pointInvisible) {
      val = IViewPointScreenValue(w, x, i);
      k = (h->num_bins * (val - low)) / (high - low);
      if (k == h->num_bins && val == high) k--; /* on boundary of last bin */
      if (k >= 0 && k < h->num_bins) {
        h->bins[k].count++;
        if (insert_point(w, hdata, x, i, k)) changed = TRUE;    
        h->num_showing++;
      }
    }
  }
  return(changed);
}

LOCAL VOID size_bins P2C(IVIEW_WINDOW, w, LVAL, hdata)
{
  IViewHist h = getinternals(hdata);
  int i, low, high, width, x, y, maxcount, m;
  double factor, density, x_low, x_high, range;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);

  StGrGetContentVariables(gwinfo, &x, &y);
  StGrGetContentRect(gwinfo, &low, NULL, &width, NULL);
  high = low + width;
  
  maxcount = 1;
  for (i = 0; i < h->num_bins; i++) {
    h->bins[i].low = low + (i * (high - low)) / h->num_bins;
    h->bins[i].high = low + ((i + 1) * (high - low)) / h->num_bins;
    maxcount = (h->bins[i].count > maxcount) ? h->bins[i].count : maxcount;
  }
  
  IViewGetScreenRange(w, y, &low, &high);
  IViewGetScaledRange(w, x, &x_low, &x_high);
  range = x_high - x_low;
  m = h->num_showing;
  density = (range > 0.0 && m > 0) ? 1.2 * (h->num_bins * maxcount / range) / m : 1.0;
  set_density_range(w, y, density);
  factor = ((double) (high - low)) / (maxcount * 1.2);
  for (i = 0; i < h->num_bins; i++) {
    h->bins[i].height = h->bins[i].count * factor;
  }
  find_point_rects(w, hdata);
}

LOCAL VOID draw_hist P2C(IVIEW_WINDOW, w, LVAL, hdata)
{
  IViewHist h = getinternals(hdata);
  int i, left, top, width, height, content_top, content_height;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
   
  StGrGetContentRect(gwinfo, NULL, &content_top, NULL, &content_height);
  
  for (i = 0; i < h->num_bins; i++) {
    left = h->bins[i].low;
    top  = content_top  + content_height - h->bins[i].height;
    width = h->bins[i].high - left + 1;
    height = h->bins[i].height + 1;
    StGWFrameRect(gwinfo, left, top, width, height);
  }
  hilite_points(w, hdata);
}

LOCAL VOID initialize_points P4C(IVIEW_WINDOW, w, LVAL, hdata, int, m, int, n)
{
  HistPoint *points;
  int i;
   
  if (m < 0 || n > IViewNumPoints(w)) return;
  points = getpointdata(hdata);
  for (i = m; i < n; i++) {
    points[i].left = 0;
    points[i].top = 0;
    points[i].width = 0;
    points[i].height = 0;
    points[i].next = NOT_IN_HIST;
  }
}

LOCAL int insert_point P5C(IVIEW_WINDOW, w, LVAL, hdata, int, var, int, p, int, bin)
{
  IViewHist h = getinternals(hdata);
  int last, current, bin_value, point_value;
  HistPoint* points;
   
  points = getpointdata(hdata);
 
  if (points[p].next != NOT_IN_HIST) return(FALSE); /* already in hist */
  
  point_value = IViewPointScreenValue(w, var, p);
  last = BIN_POINTER;
  current = h->bins[bin].smallest;
  bin_value = (current >= 0) ? IViewPointScreenValue(w, var, current) : 0;
  while (bin_value < point_value && current != NO_MORE_POINTS) {
    last = current;
    current = points[current].next;
    bin_value = (current >= 0) ? IViewPointScreenValue(w, var, current) : 0;
  }
  
  points[p].next = current;
  if (last == BIN_POINTER) h->bins[bin].smallest = p;
  else points[last].next = p;
  
  return(TRUE);
}

LOCAL int remove_non_showing_points P2C(IVIEW_WINDOW, w, LVAL, hdata)
{
  IViewHist h = getinternals(hdata);
  int last, next, showing, p, bin;
  HistPoint* points;
  int changed = FALSE;
  
  points = getpointdata(hdata);
 
  for (bin = 0; bin < h->num_bins; bin++) 
    for (p = h->bins[bin].smallest, last = BIN_POINTER; p >= 0;) {  
      showing = (! IViewPointMasked(w, p) && IViewPointState(w, p) != pointInvisible);
      if (showing) {
        last = p;
        p = points[p].next;
      }
      else {
        changed = TRUE;
        if (last == BIN_POINTER) h->bins[bin].smallest = points[p].next;
        else points[last].next = points[p].next;
        next = points[p].next;
        points[p].next = NOT_IN_HIST;
        p = next;
      }
    }
  return(changed);
}

LOCAL VOID find_point_rects P2C(IVIEW_WINDOW, w, LVAL, hdata)
{
  IViewHist h = getinternals(hdata);
  int showing, p, k, bin, bin_width, bin_left, height, bottom, top, count, base;
  HistPoint* points;
  StGWWinInfo *gwinfo;
  
  gwinfo = IViewWindowWinInfo(w);
  points = getpointdata(hdata);

  StGrGetContentOrigin(gwinfo, NULL, &base);
  
  for (bin = 0; bin < h->num_bins; bin++) {
    bin_left = h->bins[bin].low + 1;
    bin_width = h->bins[bin].high - h->bins[bin].low - 1;
    count = h->bins[bin].count;
    height = h->bins[bin].height - 1;
    for (p = h->bins[bin].smallest, k = 0; p >= 0; p = points[p].next) {  
      showing = (! IViewPointMasked(w, p) && IViewPointState(w, p) != pointInvisible);
      if (showing) {
        points[p].left = bin_left;
        points[p].width = bin_width;
        bottom = base - ((count > 0) ? (height * k) / count : 0);
        top = base - ((count > 0) ? (height * (k + 1)) / count : 0);
        points[p].top = top;
        points[p].height = bottom - top;
        k++;
      }
    }
  }
}

LOCAL VOID hilite_points P2C(IVIEW_WINDOW, w, LVAL, hdata)
{
  int p, n = IViewNumPoints(w);
  HistPoint* points;
  int oldcolor = 0, color = 0, use_color;
  StGWWinInfo *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  points = getpointdata(hdata);
  if (points == NULL) return;
  use_color = StGWUseColor(gwinfo);
  
  for (p = 0; p < n; p++)
    if (points[p].next != NOT_IN_HIST && IViewPointState(w, p) != pointNormal) {
      if (use_color) {
        oldcolor = StGWDrawColor(gwinfo);
        color = IViewPointColor(w, p);
        if (color != NOCOLOR) StGWSetDrawColor(gwinfo, color);
      }
      StGWPaintRect(gwinfo, points[p].left, points[p].top,
                            points[p].width, points[p].height);
      if (use_color && color != NOCOLOR) StGWSetDrawColor(gwinfo, oldcolor);
    }
}

LOCAL int sect_point_rect P6C(LVAL, hdata,
                              int, p,
                              int, left, int, top, int, width, int, height)
{
  int right, bottom, p_right, p_bottom;
  HistPoint* points;
  
  points = getpointdata(hdata);

  right = left + width;
  bottom = top + height;
  p_right = points[p].left + points[p].width;
  p_bottom = points[p].top + points[p].height;
  
  left = (left > points[p].left) ? left : points[p].left;
  top = (top > points[p].top) ? top : points[p].top;
  right = (right < p_right) ? right : p_right;
  bottom = (bottom < p_bottom) ? bottom : p_bottom;
  
  return((left <= right) && (top <= bottom));
}

LOCAL VOID draw_hist_point_state P4C(IVIEW_WINDOW, w, LVAL, hdata, int, p, int, state)
{
  HistPoint* points;
  int oldcolor = 0 , color = 0, use_color;
  StGWWinInfo *gwinfo;

  gwinfo = IViewWindowWinInfo(w);
  
  points = getpointdata(hdata);

  if (state == pointNormal)
    StGWEraseRect(gwinfo, points[p].left, points[p].top,
                          points[p].width, points[p].height);
  else {
    use_color = StGWUseColor(gwinfo);
    if (use_color) {
      oldcolor = StGWDrawColor(gwinfo);
      color = IViewPointColor(w, p);
      if (color != NOCOLOR) StGWSetDrawColor(gwinfo, color);
    }
    StGWPaintRect(gwinfo, points[p].left, points[p].top,
                          points[p].width, points[p].height);
    if (use_color && color != NOCOLOR) StGWSetDrawColor(gwinfo, oldcolor);
  }
  IViewSetPointScreenState(w, p, (PointState) state);
}


LOCAL VOID set_density_range P3C(IVIEW_WINDOW, w, int, y, double, density)
{
  int showing, labeled, ticks;
  double low = 0.0;
  
  IViewGetYaxis(w, &showing, &labeled, &ticks);
  if (showing) {
    GetNiceRange(&low, &density, &ticks);
    IViewSetYaxis(w, showing, labeled, ticks);
  }
  IViewSetRange(w, y, 0.0, density);
}

