/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/* external functions */
#ifdef USESTRINGS
extern double IViewStringValue(), IViewStringScaledValue();
#endif /* USESTRINGS */

static double dmin P2C(double, x, double, y) { return((x > y) ? y : x); }
static double dmax P2C(double, x, double, y) { return((x > y) ? x : y); }

static VOID adjust_range_step P4C(double, value, double *, low, double *, high, int *, inited)
{
  if (*inited) { 
    *low = dmin(*low, value); 
    *high = dmax(*high, value);
  }
  else {
    *low = value;
    *high = value;
    *inited = TRUE;
  }
}

VOID IViewGetVisibleRange P4C(IVIEW_WINDOW, w, unsigned, var, double *, plow, double *, phigh)
{
  int inited, i, n;
  double value, low, high;
  
  inited = FALSE;
  
  n = IViewNumPoints(w);
  for (i = 0; i < n; i++) {
    if (! IViewPointMasked(w, i) && IViewPointState(w, i) != pointInvisible) {
      value = IViewPointScaledValue(w, var, i);
      adjust_range_step(value, &low, &high, &inited);
    }
  }
  n = IViewNumLines(w);
  for (i = 0; i < n; i++) {
    if (! IViewLineMasked(w, i)) {
      value = IViewLineScaledValue(w, var, i);
      adjust_range_step(value, &low, &high, &inited);
    }      
  }
#ifdef USESTRINGS
  n = IViewNumStrings(w);
  for (i = 0; i < n; i++) {
    if (! IViewStringMasked(w, i)) {
    value = IViewStringScaledValue(w, var, i);
      adjust_range_step(value, &low, &high, &inited);
    }
  }
#endif /* USESTRINGS */
  if (plow != NULL) *plow = IViewDecodeValue(w, low, var);
  if (phigh != NULL) *phigh = IViewDecodeValue(w, high, var);
}

static VOID apply_scale_shift_data P4C(IVIEW_WINDOW, w, unsigned, var, double, scale, double, shift)
{
  double value;
  int i, n;

  if (var >= IViewNumVariables(w)) return;
  IViewSetScale(w, var, scale * IViewScale(w, var));
  IViewSetShift(w, var, scale * IViewShift(w, var) + shift);
  
  n = IViewNumPoints(w);
  for (i = 0; i < n; i++) {
    value = IViewPointScaledValue(w, var, i);
    value = scale * value + shift;
    IViewSetPointScaledValue(w, var, i, value);
  }
  n = IViewNumLines(w);
  for (i = 0; i < n; i++) {
    value = IViewLineScaledValue(w, var, i);
    value = scale * value + shift;
    IViewSetLineScaledValue(w, var, i, value);
  }
#ifdef USESTRINGS
  n = IViewNumStrings(w);
  for (i = 0; i < n; i++) {
    value = IViewStringScaledValue(w, var, i);
    value = scale * value + shift;
    IViewSetStringScaledValue(w, var, i, value);
  }
#endif /* USESTRINGS */
}

static VOID scale_to_range P4C(IVIEW_WINDOW, w, unsigned,  var, double, low, double, high)
{
  double old_low, old_high, scale, shift, old_scale, old_shift;

  if (var >= IViewNumVariables(w)) return;
  IViewGetVisibleRange(w, var, &old_low, &old_high);
  if (old_high <= old_low) return;
  scale = (high - low) / (old_high - old_low);
  shift = - scale * old_low + low;

  old_scale = IViewScale(w, var);
  old_shift = IViewShift(w, var);
  if (old_scale > 0.0) {
    scale = scale / old_scale;
    shift = shift - scale * old_shift;
  }
  
  apply_scale_shift_data(w, var, scale, shift);
}

VOID IViewScaleToRange P4C(IVIEW_WINDOW, w, unsigned, var, double, low, double, high)
{
  scale_to_range(w, var, low, high);
}

VOID IViewApplyScaleShift P4C(IVIEW_WINDOW, w, unsigned, var, double, scale, double, shift)
{
  apply_scale_shift_data(w, var, scale, shift);
}
