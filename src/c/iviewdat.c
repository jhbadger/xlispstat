/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                      Fixed Point Arithmetic Package                  **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

#define SCALE 65536

#ifdef MACINTOSH
typedef struct { short h; unsigned short v; } fixed_struct;
static FixRound(Fixed x) { return(((fixed_struct *) &x)->h); }

#define INTPART(x)  (((fixed_struct *) (x))->h)
#define FRACPART(x) (((fixed_struct *) (x))->v)

static Fixed FixInnerProduct(int vars, Fixed *x, Fixed *y, int *inbasis)
{
  register Fixed result = 0, hx, lx, hy, ly;
  
  for (; vars-- > 0; x++, y++) 
    if (*inbasis++) {
      hx = INTPART(x); lx = FRACPART(x);
      hy = INTPART(y); ly = FRACPART(y);
      if (hx == 0) result += *x * hy + ((*x * (ly >> 2)) >> 14);
      else result += *x * hy + hx * ly + ((lx * (ly >> 2)) >> 14);
    }
    
  return(result);
}

#else
#define FixRound(x) ((int) ((x) / SCALE))

#ifdef MSDOS
static Fixed FixInnerProduct(int vars, Fixed *x, Fixed *y, int *inbasis)
{
  register unsigned int pos;
  register Fixed result = 0, val, xx, yy, hx, lx, hy, ly;

  for (; vars-- > 0; x++, y++)
    if (*inbasis++) {
      pos = TRUE;
      xx = *x;
      if (xx < 0) { pos = FALSE; xx = -xx; }
      yy = *y;
      if (yy < 0) { pos = ! pos; yy = -yy; }

      lx = LOWORD(xx); hx = HIWORD(xx);
      ly = LOWORD(yy); hy = HIWORD(yy);
      val = xx * hy + ((lx * (ly >> 2)) >> 14);
      if (hx) val += hx * ly;
      if (pos) result += val;
      else result -= val;
    }

  return(result);
}
#else
static Fixed FixInnerProduct P4C(int, vars, Fixed *, x, Fixed *, y, int *, inbasis)
{
  double result = 0.0;
  
  for (; vars-- > 0; x++, y++) 
    if (*inbasis++) result += ((double) *x) * ((double) *y);
    
  return((Fixed) (result / SCALE));
}
#endif /* MSDOS */
#endif /* MACINTOSH */

#define Int2Fixed(x) ((Fixed)((x) * SCALE))
#define Double2Fixed(x) ((Fixed) ((x) * SCALE))
#define Fixed2Double(x) (((double) (x)) / SCALE)

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                         IViewBasicPoints Package                     **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

/* forward declarations */
LOCAL double IViewBasicPointValue P3H(IViewBasicPoints, unsigned, unsigned);


/**************************************************************************/
/**                                                                      **/
/**            Basic Points Creation and Destruction Functions           **/
/**                                                                      **/
/**************************************************************************/

static IViewBasicPoints IViewNewBasicPoints P3C(int, m, double *, scale, double *, location)
{
  IViewBasicPoints p;
  
  p = (IViewBasicPoints) StCalloc(sizeof(struct basic_points), 1);
  
  p->num_points = 0;
  p->num_variables = m;
  p->scale = scale;
  p->location = location;
  p->recalculateScreenPoints = FALSE;
  p->transformation = (double **) NULL;

  if (m > 0) {
    p->data = (IViewReallocData *) StCalloc(sizeof(IViewReallocData), m);
    p->screen_data = (IViewReallocData *) StCalloc(sizeof(IViewReallocData), m);    
  }

  return(p);
}

static VOID IViewFreeBasicPoints P1C(IViewBasicPoints, p)
{
  int i;
  
  for (i = 0; i < p->num_variables; i++) {
    if (p->data != NULL) StRFree(p->data[i]);
    if (p->screen_data != NULL) StRFree(p->screen_data[i]);
  }
  
  StFree(p->data);
  StFree(p->screen_data);
  StFree(p);
}

static VOID IViewAddBasicPoints P2C(IViewBasicPoints, p, int, n)
{
  int size, i;

  size = p->num_points + n;
  if (p->num_variables > 0) {
    if (p->data == NULL || p->screen_data == NULL) return;

    for (i = 0; i < p->num_variables; i++) {
      p->data[i] = StRRealloc(p->data[i], sizeof(double), size);
      p->screen_data[i] = StRRealloc(p->screen_data[i], sizeof(Fixed), size);
    }
  }
  p->num_points += n;
}

static VOID IViewClearBasicPoints P1C(IViewBasicPoints, p)
{
  int i;
  p->num_points = 0;
  if (p->data == NULL || p->screen_data == NULL) return;

  for (i = 0; i < p->num_variables; i++) {
    StRFree(p->data[i]);
    p->data[i] = NULL;
    StRFree(p->screen_data[i]);
    p->screen_data[i] = NULL;
  }
}

/**************************************************************************/
/**                                                                      **/
/**       Basic Points Data Set Access and Modificaiton Functions        **/
/**                                                                      **/
/**************************************************************************/

static int IViewBasicPointsNumPoints P1C(IViewBasicPoints, p)
{
  return(p->num_points);
}

static int IViewBasicPointsNumVariables P1C(IViewBasicPoints, p)
{
  return(p->num_variables);
}

static VOID IViewBasicPointsSetRecalculate P2C(IViewBasicPoints, p, int, fixed)
{
  p->recalculateScreenPoints = TRUE;
  p->fixed = fixed;
}

static double TransformedValue P3C(IViewBasicPoints, p, unsigned, var, unsigned, point)
{
  double x, a;
  int i;
  
  if (p->transformation == NULL) 
    return(IViewBasicPointValue(p, var, point));
  else {
    for (i = 0, x = 0.0; i < p->num_variables; i++)
      if ((a = p->transformation[var][i]) != 0.0)
        x += IViewBasicPointValue(p, i, point) * a;
    return(x);
  }
}

static VOID SetScreenValue P3C(IViewBasicPoints, p, unsigned, var, unsigned, point)
{
  double x = TransformedValue(p, var, point);
  Fixed screen_x, *screen_data;
  
  if (p->screen_data == NULL) return;
  screen_data = (Fixed *) StRPtr(p->screen_data[var]);
  if (screen_data == NULL) return;

  if (p->location == NULL || p->scale == NULL) x = 0.0;
  else x = x * p->scale[var] + p->location[var];
  
  screen_x = Double2Fixed(x);
  screen_data[point] = screen_x;
}

static VOID RecalculateScreenPoints P1C(IViewBasicPoints, p)
{
  int var, point;
  
  for (var = 0; var < p->num_variables; var++)
    for (point = 0; point < p->num_points; point++) 
      SetScreenValue(p, var, point);
  p->recalculateScreenPoints = FALSE;
}

static VOID IViewBasicPointsSetTransformation P2C(IViewBasicPoints, p, double **, a)
{
  p->transformation = a;
  p->recalculateScreenPoints = TRUE;
}  

static VOID IViewBasicPointsApplyTransformation P3C(IViewBasicPoints, p,
                                                    double **, a, int *, inbasis)
{
  static maxvars = 0;
  static Fixed **b, *x, **screen_data, *screen_location;
  int vars, n, i, j;
  double *scale = p->scale, *location = p->location;
  if (p->screen_data == NULL) return;

  vars = p->num_variables;
  n = p->num_points;

  /* allocate local working storage */
  if (maxvars < vars) {
    maxvars = vars;
    if (b != NULL) StFree(b[0]);
    StFree(b);
    StFree(x);
    StFree(screen_data);
    StFree(screen_location);
    b = (Fixed **) StCalloc(sizeof(Fixed *), maxvars);
    b[0] = (Fixed *) StCalloc(sizeof(Fixed), maxvars * maxvars);
    for (i = 1; i < maxvars; i++) b[i] = b[0] + i * maxvars;
    x = (Fixed *) StCalloc(sizeof(Fixed), maxvars);
    screen_data = (Fixed **) StCalloc(sizeof(Fixed *), maxvars);
    screen_location = (Fixed *) StCalloc(sizeof(Fixed), maxvars);
  }
  
  /* convert transformation to fixed point format*/ 
  if (p->fixed) {
    for (i = 0; i < vars; i++)
      if (inbasis[i])
        for (j = 0; j < vars; j++)
          if (inbasis[j]) b[i][j] = Double2Fixed(a[i][j]);
  }
  else {
    for (i = 0; i < vars; i++)
      if (inbasis[i]) {
        for (j = 0; j < vars; j++)
          if (inbasis[j]) 
            b[i][j] = Double2Fixed(scale[i] * a[i][j] / scale[j]);
        screen_location[i] = Double2Fixed(location[i]);
      }
  }
  
  /* set up array with screen coordinates */
  for (i = 0; i < vars; i++)
    screen_data[i] = (Fixed *) StRPtr(p->screen_data[i]);
  if (! p->fixed)
    for (i = 0; i < vars; i++) 
      if (inbasis[i]) 
        for (j = 0; j < n; j++) screen_data[i][j] -= screen_location[i];
    
  /* apply the transformation */
  for (i = 0; i < n; i++) {
    for (j = 0; j < vars; j++) 
      if (inbasis[j]) x[j] = screen_data[j][i];    
    for (j = 0; j < vars; j++) 
      if (inbasis[j]) {
        screen_data[j][i] = FixInnerProduct(vars, b[j], x, inbasis);
      }
  }

  /* adjust origins if scaling is not fixed */
  if (! p->fixed)
    for (i = 0; i < vars; i++) 
      if (inbasis[i]) 
        for (j = 0; j < n; j++) screen_data[i][j] += screen_location[i];
}

/**************************************************************************/
/**                                                                      **/
/**         Basic Points Point Access and Modification Functions         **/
/**                                                                      **/
/**************************************************************************/

static VOID IViewBasicPointSetValue P4C(IViewBasicPoints, p,
                                        unsigned, var, unsigned, point,
                                        double, value)
{
  double *x;
  
  if (var < p->num_variables && point < p->num_points && p->data != NULL) {
    x = (double *) StRPtr(p->data[var]);
    if (x != NULL) x[point] = value;
  }
  if (p->transformation != NULL) p->recalculateScreenPoints = TRUE;
  if (! p->recalculateScreenPoints) SetScreenValue(p, var, point);
}

static double IViewBasicPointValue P3C(IViewBasicPoints, p,
                                       unsigned, var, unsigned, point)
{
  double *x, value;
  
  if (var < p->num_variables  && point < p->num_points && p->data != NULL) {
    x = (double *) StRPtr(p->data[var]);
    value = (x != NULL) ? x[point] : 0.0;
    return(value);
  }
  else xlfail("index out of range");
  return 0.0; /* not reached */
}

static int IViewBasicPointScreenValue P3C(IViewBasicPoints, p,
                                          unsigned, var, unsigned, point)
{
  Fixed *screen_data;
  
  if (p->recalculateScreenPoints) RecalculateScreenPoints(p);
  
  if (var < p->num_variables && point < p->num_points 
      && p->screen_data != NULL) {
    screen_data = (Fixed *) StRPtr(p->screen_data[var]);
#ifdef PERSPECTIVE
    return(PerspScreenValue(p, var, point));
#else
    return(FixRound(screen_data[point]));
#endif /* PERSPECTIVE */
  }
  else xlfail("index out of range");
  return 0; /* not reached */
}

static VOID IViewBasicPointsGetScreenValues P3C(IViewBasicPoints, p, unsigned, point, int *, x)
{
  int i, n, vars;
  Fixed *screen_data;
  
  n = p->num_points;
  vars = p->num_variables;

  if (p->recalculateScreenPoints) RecalculateScreenPoints(p);
  if (p->screen_data == NULL || point >= n) return;
  
  for (i = 0; i < vars; i++){
    screen_data = (Fixed *) StRPtr(p->screen_data[i]);
    if (screen_data != NULL) x[i] = FixRound(screen_data[point]);
  }
}

/**************************************************************************/
/**************************************************************************/
/**                                                                      **/
/**                            IViewData Package                         **/
/**                                                                      **/
/**************************************************************************/
/**************************************************************************/

VOID IViewDataSetPointColor P3H(IViewData, unsigned, int);
VOID IViewDataSetPointState P3H(IViewData, unsigned, int);
VOID IViewDataSetPointSymbol P4H(IViewData, unsigned, int, int);
VOID IViewDataSetLineValue P4H(IViewData, int, int, double);
VOID IViewDataSetLineColor P3H(IViewData, unsigned, int);
VOID IViewDataSetLineWidth P3H(IViewData, unsigned, unsigned);
VOID IViewDataSetNextLine P3H(IViewData, unsigned, int);
VOID IViewDataSetLineType P3H(IViewData, unsigned, int);

/**************************************************************************/
/**                                                                      **/
/**          IView Data Construction and Destruction Functions           **/
/**                                                                      **/
/**************************************************************************/

IViewData IViewDataNew P1C(int, vars)
{
  IViewData p;
  
  p = (IViewData) StCalloc(sizeof(struct iview_data), 1);
  if (vars > 0) {
    p->mins = (double *) StCalloc(sizeof(double), vars);
    p->maxes = (double *) StCalloc(sizeof(double), vars);
    p->scale = (double *) StCalloc(sizeof(double), vars);
    p->location = (double *) StCalloc(sizeof(double), vars);
    p->screen_mins = (int *) StCalloc(sizeof(int), vars);
    p->screen_maxes = (int *) StCalloc(sizeof(int), vars);
  }
  
  p->points = IViewNewBasicPoints(vars, p->scale, p->location);
  p->lines = IViewNewBasicPoints(vars, p->scale, p->location);
#ifdef USESTRINGS
  p->strings = IViewNewBasicPoints(vars, p->scale, p->location);
#endif /* USESTRINGS */
  if (vars > 0)
    p->variableLabels = (char **) StCalloc(sizeof(char *), vars);
  p->recalculateScreenPoints = FALSE;
  p->transformed = FALSE;
  
  return(p);
}

VOID IViewDataFree P1C(IViewData, p)
{
  int i;
  PointInfo *pointInfo;
#ifdef USESTRINGS
  StringInfo *stringInfo;
#endif /* USESTRINGS */
  
  StFree(p->mins);
  StFree(p->maxes);
  StFree(p->scale);
  StFree(p->location);
  StFree(p->screen_mins);
  StFree(p->screen_maxes);

  for (i = 0; i < p->points->num_points; i++) {
    pointInfo = (PointInfo *) StRPtr(p->pointInfo);
    if (pointInfo[i].label != NULL) StFree(pointInfo[i].label);
  }
  StRFree(p->pointInfo);
  
  StRFree(p->lineInfo);
  
#ifdef USESTRINGS
  for (i = 0; i < p->strings->num_points; i++) {
    stringInfo = (StringInfo *) StRPtr(p->stringInfo);
    if (stringInfo[i].string != NULL) StFree(stringInfo[i].string);
  }
  StRFree(p->stringInfo);
#endif /* USESTRINGS */

  for (i = 0; i < p->points->num_variables; i++) {
    if (p->variableLabels[i] != NULL) StFree(p->variableLabels[i]);
  }
  StFree(p->variableLabels);
    
  IViewFreeBasicPoints(p->points);
  IViewFreeBasicPoints(p->lines);
#ifdef USESTRINGS
  IViewFreeBasicPoints(p->strings);
#endif /* USESTRINGS */
  
  if (p->transformation != NULL) {
    StFree(p->transformation[0]);
    StFree(p->transformation);
  }
  StFree(p);
}

LOCAL IViewBasicPoints IViewDataPoints P1C(IViewData, data)
{
  if (data == NULL) xlfail("nil data pointer");
  if (data->points == NULL) xlfail("nil point data pointer");
  return(data->points);
}

LOCAL IViewBasicPoints IViewDataLines P1C(IViewData, data)
{
  if (data == NULL) xlfail("nil data pointer");
  if (data->lines == NULL) xlfail("nil line data pointer");
  return(data->lines);
}

#ifdef USESTRINGS
LOCAL IViewBasicPoints IViewDataStrings P1C(IViewData, data)
{
  if (data == NULL) xlfail("nil data pointer");
  if (data->strings == NULL) xlfail("nil string data pointer");
  return(data->strings);
}
#endif /* USESTRINGS */

/**************************************************************************/
/**                                                                      **/
/**                        General Data Functions                        **/
/**                                                                      **/
/**************************************************************************/

int IViewDataNumVariables P1C(IViewData, p)
{
  return(IViewBasicPointsNumVariables(IViewDataPoints(p)));
}

VOID IViewDataSetVariableLabel P3C(IViewData, p, unsigned, var, char *, s)
{
  if (p == NULL || var >= IViewDataNumVariables(p)) return;
  
  StFree(p->variableLabels[var]);
  p->variableLabels[var] = NULL;
  if (s != 0 && strlen(s) != 0) {
    p->variableLabels[var] = StCalloc(sizeof(char), 1 + strlen(s));
    strcpy(p->variableLabels[var], s);
  }
}

char *IViewDataVariableLabel P2C(IViewData, p, unsigned, var)
{
  if (p == NULL || var >= IViewDataNumVariables(p)) return(NULL);
  return(p->variableLabels[var]);
}

static int CalculateLocationScale P1C(IViewData, p)
{
  int i, fixed = 0, vars = IViewDataNumVariables(p);
  double range, screen_range, scale;
  
  for (i = 0; i < vars; i++) {
    range = p->maxes[i] - p->mins[i];
    screen_range = p->screen_maxes[i] - p->screen_mins[i];
    p->scale[i] = (range > 0) ? screen_range / range : 0.0;
    p->location[i] = p->screen_mins[i] - p->mins[i] * p->scale[i];
  }
  if (vars > 0) {
    fixed = (p->location[0] == 0.0) ? TRUE : FALSE;
    scale = p->scale[0];
    for (i = 1; i < vars; i++)
      fixed = (fixed && scale == p->scale[i] && p->location[i] == 0.0);
  }
  return(fixed);
}

VOID IViewDataSetRange P4C(IViewData, p, unsigned, var, double, low, double, high)
{
  int fixed;
  
  if (p != NULL && var < IViewDataNumVariables(p) && low < high) {
    p->mins[var] = low;
    p->maxes[var] = high;
    fixed = CalculateLocationScale(p);
    IViewBasicPointsSetRecalculate(p->points, fixed);
    IViewBasicPointsSetRecalculate(p->lines, fixed);
#ifdef USESTRINGS
    IViewBasicPointsSetRecalculate(p->strings, fixed);
#endif /* USESTRINGS */
  }
}

VOID IViewDataGetRange P4C(IViewData, p, unsigned, var, double *, low, double *, high)
{
  if (p != NULL && var < IViewDataNumVariables(p)) {
    if (low != NULL) *low = p->mins[var];
    if (high != NULL) *high = p->maxes[var];
  }
}

VOID IViewDataSetScreenRange P4C(IViewData, p, unsigned, var, int, low, int, high)
{
  int fixed;
  
  if (p != NULL && var < IViewDataNumVariables(p) && low < high) {
    p->screen_mins[var] = low;
    p->screen_maxes[var] = high;
    fixed = CalculateLocationScale(p);
    IViewBasicPointsSetRecalculate(IViewDataPoints(p), fixed);
    IViewBasicPointsSetRecalculate(IViewDataLines(p), fixed);
#ifdef USESTRINGS
    IViewBasicPointsSetRecalculate(IViewDataStrings(p), fixed);
#endif /* USESTRINGS */
  }
}

VOID IViewDataGetScreenRange P4C(IViewData, p, unsigned, var, int *, low, int *, high)
{
  if (p != NULL && var < IViewDataNumVariables(p)) {
    if (low != NULL) *low = p->screen_mins[var];
    if (high != NULL) *high = p->screen_maxes[var];
  }
}

VOID IViewDataSetIdentityTransformation P1C(IViewData, data)
{
  int i, j, vars = IViewDataNumVariables(data);
  double *p;
  
  if (data == NULL) return;
  
  data->transformed = FALSE;
  if (data->transformation == NULL) {
    data->transformation = (double **) StCalloc(sizeof(double *), vars);
    p = (double *) StCalloc(sizeof(double), vars * vars);
    for (i = 0; i < vars; i++)
      data->transformation[i] = p + i * vars;
  }
  
  for (i = 0; i < vars; i++)
    for (j = 0; j < vars; j++)
      data->transformation[i][j] = (i == j) ? 1.0 : 0.0;

  IViewBasicPointsSetTransformation(IViewDataPoints(data), data->transformation);
  IViewBasicPointsSetTransformation(IViewDataLines(data), data->transformation);
#ifdef USESTRINGS
  IViewBasicPointsSetTransformation(IViewDataStrings(data), data->transformation);
#endif /* USESTRINGS */
}

VOID IViewDataSetTransformation P2C(IViewData, data, double **, a)
{
  int i, j, vars = IViewDataNumVariables(data);
  double *p;
  
  if (data == NULL) return;
  data->transformed = TRUE;
  if (a != NULL) {
    if (data->transformation == NULL) {
      data->transformation = (double **) StCalloc(sizeof(double *), vars);
      p = (double *) StCalloc(sizeof(double), vars * vars);
      for (i = 0; i < vars; i++)
        data->transformation[i] = p + i * vars;
    }
  
    for (i = 0; i < vars; i++)
      for (j = 0; j < vars; j++)
        data->transformation[i][j] = a[i][j];
  }
  else if (data->transformation != NULL) {
    StFree(data->transformation[0]);
    StFree(data->transformation);
  }

  IViewBasicPointsSetTransformation(IViewDataPoints(data), data->transformation);
  IViewBasicPointsSetTransformation(IViewDataLines(data), data->transformation);
#ifdef USESTRINGS
  IViewBasicPointsSetTransformation(IViewDataStrings(data), data->transformation);
#endif /* USESTRINGS */
}

double **IViewDataTransformation P1C(IViewData, data)
{
  if (data == NULL) return(NULL);
  else return(data->transformation);
}

VOID IViewDataApplyTransformation P3C(IViewData, data, double **, a, int *, inbasis)
{
  static int temp_size;
  static double *temp;
  double **b;
  int i, j, k, vars = IViewDataNumVariables(data);

  if (data == NULL) return;
  
  if (data->transformation == NULL) IViewDataSetIdentityTransformation(data);
  data->transformed = TRUE;
  b = data->transformation;
  
  if (temp_size < vars) {
    StFree(temp);
    temp = NULL;
  }
  if (temp == NULL) {
    temp_size = vars;
    temp = (double *) StCalloc(sizeof(double), temp_size);
  }

  for (j = 0; j < vars; j++) {
    for (i = 0; i < vars; i++) 
      if (inbasis[i]){
        temp[i] = 0.0;
        for (k = 0; k < vars; k++)
          if (inbasis[k] && a[i][k] != 0.0 && b[k][j] != 0) 
            temp[i] += a[i][k] * b[k][j];
      }
    for (i = 0; i < vars; i++)
      if (inbasis[i]) b[i][j] = temp[i];
  }

  IViewBasicPointsApplyTransformation(IViewDataPoints(data), a, inbasis);
  IViewBasicPointsApplyTransformation(IViewDataLines(data), a, inbasis);
#ifdef USESTRINGS
  IViewBasicPointsApplyTransformation(IViewDataStrings(data), a, inbasis);
#endif /* USESTRINGS */
}

int IViewDataIsTransformed P1C(IViewData, data)
{
  return (data != NULL && data->transformed);
}

/**************************************************************************/
/**                                                                      **/
/**                          Point Data Functions                        **/
/**                                                                      **/
/**************************************************************************/

VOID IViewDataAddPoints P2C(IViewData, p, int, n)
{
  int i, fixed, old_n = IViewDataNumPoints(p);
    
  IViewAddBasicPoints(IViewDataPoints(p), n);
  n += old_n;
  p->pointInfo = StRRealloc(p->pointInfo, sizeof(struct point_info), n);
  
  for (i = old_n; i < n; i++) {
    IViewDataSetPointSymbol(p, i, 4, 5);
    IViewDataSetPointState(p, i, pointNormal);
    IViewDataSetPointColor(p, i, NOCOLOR);
  }
  fixed = CalculateLocationScale(p);
  IViewBasicPointsSetRecalculate(IViewDataPoints(p), fixed);
}

VOID IViewDataClearPoints P1C(IViewData, p)
{
  PointInfo *pointInfo;
  int i, n = IViewDataNumPoints(p);
   
  for (i = 0; i < n; i++) {
    pointInfo = (PointInfo *) StRPtr(p->pointInfo);
    if (pointInfo[i].label != NULL) StFree(pointInfo[i].label);
  }
  StRFree(p->pointInfo);
  p->pointInfo = NULL;
  IViewClearBasicPoints(IViewDataPoints(p));
}

int IViewDataNumPoints P1C(IViewData, p)
{
  return(IViewBasicPointsNumPoints(IViewDataPoints(p)));
}

VOID IViewDataSetPointValue P4C(IViewData, p, int, var, int, point, double, value)
{
  IViewBasicPointSetValue(IViewDataPoints(p), var, point, value);
}

double IViewDataPointValue P3C(IViewData, p, int, var, int, point)
{
  return(IViewBasicPointValue(IViewDataPoints(p), var, point));
}

double IViewDataPointTransformedValue P3C(IViewData, p, int, var, int, point)
{
  return(TransformedValue(IViewDataPoints(p), var, point));
}

int IViewDataPointScreenValue P3C(IViewData, p, int, var, int, point)
{
  return(IViewBasicPointScreenValue(IViewDataPoints(p), var, point));
}

VOID IViewDataGetScreenPointValues P3C(IViewData, d, int, point, int *, x)
{
  IViewBasicPointsGetScreenValues(IViewDataPoints(d), point, x);
}

VOID IViewDataSetPointMask P3C(IViewData, p, unsigned, point, int, masked)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].masked = masked;
}

int IViewDataPointMasked P2C(IViewData, p, unsigned, point)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return(0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return((int) info[point].masked);
}

VOID IViewDataSetPointColor P3C(IViewData, p, unsigned, point, int, color)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].color = color;
}

int IViewDataPointColor P2C(IViewData, p, unsigned, point)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return(0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return((int) info[point].color);
}

VOID IViewDataSetPointState P3C(IViewData, p, unsigned, point, int, state)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].state = (PointState) state;
}

PointState IViewDataPointState P2C(IViewData, p, unsigned, point)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return((PointState) 0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return(info[point].state);
}

VOID IViewDataSetPointScreenState P3C(IViewData, p, unsigned, point, PointState, state)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].screen_state = state;
}

PointState IViewDataPointScreenState P2C(IViewData, p, unsigned, point)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return((PointState) 0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return(info[point].screen_state);
}

VOID IViewDataResetScreenStates P1C(IViewData, p)
{
  int i, n = IViewDataNumPoints(p);
  PointInfo *info;
  
  if (p == NULL || p->pointInfo == NULL) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  for (i = 0; i < n; i++) info[i].screen_state = info[i].state;
}

VOID IViewDataSetPointMark P3C(IViewData, p, unsigned, point, int, marked)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].marked = marked;
}

int IViewDataPointMarked P2C(IViewData, p, unsigned, point)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return(0);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  return((int) info[point].marked);
}

VOID IViewDataClearPointMarks P1C(IViewData, p)
{
  int i, n = IViewDataNumPoints(p);
  PointInfo *info;
  
  if (p == NULL || p->pointInfo == NULL) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  for (i = 0; i < n; i++) info[i].marked = FALSE;
}

VOID IViewDataSetPointLabel P3C(IViewData, p, unsigned, point, char *, s)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  StRLock(p->pointInfo);
  info = (PointInfo *) StRPtr(p->pointInfo);
  
  StFree(info[point].label);
  info[point].label = NULL;
  if (s != 0 && strlen(s) != 0) {
    info[point].label = StCalloc(sizeof(char), 1 + strlen(s));
    strcpy(info[point].label, s);
  }
  StRUnlock(p->pointInfo);
}

char *IViewDataPointLabel P2C(IViewData, p, unsigned, point)
{
  PointInfo *info;
  char label[100];
  
  if (point >= IViewDataNumPoints(p)) return(NULL);
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  
  /* only allocate label on demand to save time and space */
  if (info[point].label == NULL) {
    sprintf(label, "%d", point);
    IViewDataSetPointLabel(p, point, label);
  }
  return(info[point].label);
}

VOID IViewDataSetPointSymbol P4C(IViewData, p, unsigned, point, int, sym, int, hsym)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  info[point].symbol.normal = sym;
  info[point].symbol.highlighted = hsym;
}

VOID IViewDataGetPointSymbol P4C(IViewData, p, unsigned, point, int *, sym, int *, hsym)
{
  PointInfo *info;
  
  if (point >= IViewDataNumPoints(p)) return;
  
  info = (PointInfo *) StRPtr(p->pointInfo);
  if (sym != NULL) *sym = info[point].symbol.normal;
  if (hsym != NULL) *hsym = info[point].symbol.highlighted;
}
  
/**************************************************************************/
/**                                                                      **/
/**                            Line Data Functions                       **/
/**                                                                      **/
/**************************************************************************/

int IViewDataNumLines P1C(IViewData, p)
{
  return(IViewBasicPointsNumPoints(IViewDataLines(p)));
}

VOID IViewDataAddLines P2C(IViewData, p, int, n)
{
  int i, old_n = IViewDataNumLines(p);
  if (p == NULL) return;
  
  IViewAddBasicPoints(IViewDataLines(p), n);
  n += old_n;
  p->lineInfo = StRRealloc(p->lineInfo, sizeof(struct line_info), n);
  
  for (i = old_n; i < n - 1; i++) {
    IViewDataSetNextLine(p, i, i + 1);
    IViewDataSetLineType(p, i, 0);
    IViewDataSetLineColor(p, i, NOCOLOR);
    IViewDataSetLineWidth(p, i, 1);
  }
  IViewDataSetNextLine(p, n - 1, -1);
  IViewDataSetLineType(p, n - 1, 0);  
  IViewDataSetLineColor(p, n - 1, NOCOLOR);
  IViewDataSetLineWidth(p, n - 1, 1);
}

VOID IViewDataClearLines P1C(IViewData, p)
{
  StRFree(p->lineInfo);
  p->lineInfo = NULL;
  IViewClearBasicPoints(IViewDataLines(p));
}

VOID IViewDataSetLineValue P4C(IViewData, p, int, var, int, line, double, value)
{
  IViewBasicPointSetValue(IViewDataLines(p), var, line, value);
}

double IViewDataLineValue P3C(IViewData, p, int, var, int, line)
{
  return(IViewBasicPointValue(IViewDataLines(p), var, line));
}

double IViewDataLineTransformedValue P3C(IViewData, p, int, var, int, line)
{
  return(TransformedValue(IViewDataLines(p), var, line));
}

int IViewDataLineScreenValue P3C(IViewData, p, int, var, int, line)
{
  return(IViewBasicPointScreenValue(IViewDataLines(p), var, line));
}

VOID IViewDataSetLineMask P3C(IViewData, p, unsigned, line, int, masked)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].masked = masked;
}

int IViewDataLineMasked P2C(IViewData, p, unsigned, line)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return(0);
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  return((int) info[line].masked);
}

VOID IViewDataSetLineColor P3C(IViewData, p, unsigned, line, int, color)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].color = color;
}

int IViewDataLineColor P2C(IViewData, p, unsigned, line)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return(0);
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  return((int) info[line].color);
}

VOID IViewDataSetLineWidth P3C(IViewData, p, unsigned, line, unsigned, width)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].width = width;
}

VOID IViewDataGetLineWidth P3C(IViewData, p, unsigned, line, unsigned *, width)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  if (width != 0) *width = (int) info[line].width;
}

VOID IViewDataSetNextLine P3C(IViewData, p, unsigned, line, int, next)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p) || next >= IViewDataNumLines(p)) return;

  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].next = next;
}

int IViewDataNextLine P2C(IViewData, p, unsigned, line)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return(0);
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  return(info[line].next);
}

VOID IViewDataSetLineType P3C(IViewData, p, unsigned, line, int, type)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return;
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  info[line].type = type;
}

int IViewDataLineType P2C(IViewData, p, unsigned, line)
{
  LineInfo *info;
  
  if (line >= IViewDataNumLines(p)) return(0);
  
  info = (LineInfo *) StRPtr(p->lineInfo);
  return(info[line].type);
}

#ifdef USESTRINGS
/**************************************************************************/
/**                                                                      **/
/**                           String Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

IViewDataNumStrings(p)
	IViewData p;
{
  return(IViewBasicPointsNumPoints(IViewDataStrings(p)));
}

IViewDataAddStrings(p, n)
     IViewData p;
     int n;
{
  int i, old_n = IViewDataNumStrings(p);
  if (p == NULL) return;
  
  IViewAddBasicPoints(IViewDataStrings(p), n);
  n += old_n;
  p->stringInfo = StRRealloc(p->stringInfo, sizeof(struct string_info), n);
  
  for (i = old_n; i < n; i++) IViewDataSetStringColor(p, i, NOCOLOR);
}

IViewDataClearStrings(p)
	IViewData p;
{
  StringInfo *stringInfo;
  int i, n = IViewDataNumStrings(p);
   
  for (i = 0; i < n; i++) {
    stringInfo = (StringInfo *) StRPtr(p->stringInfo);
    if (stringInfo[i].string != NULL) StFree(stringInfo[i].string);
  }
  StRFree(p->stringInfo);
  p->stringInfo = NULL;
  IViewClearBasicPoints(IViewDataStrings(p));
}

IViewDataSetStringValue(p, var, string, value)
	IViewData p;
	int var, string;
	double value;
{
  IViewBasicPointSetValue(IViewDataStrings(p), var, string, value);
}

double IViewDataStringValue(p, var, string)
	IViewData p;
	int var, string;
{
  return(IViewBasicPointValue(IViewDataStrings(p), var, string));
}

double IViewDataStringTransformedValue(p, var, string)
	IViewData p;
	int var, string;
{
  return(TransformedValue(IViewDataStrings(p), var, string));
}

IViewDataStringScreenValue(p, var, string)
	IViewData p;
	int var, string;
{
  return(IViewBasicPointScreenValue(IViewDataStrings(p), var, string));
}

IViewDataSetStringMask(p, string, masked)
     IViewData p;
     unsigned string;
     int masked;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  info[string].masked = masked;
}

IViewDataStringMasked(p, string)
     IViewData p;
     unsigned string;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return(0);
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  return((int) info[string].masked);
}

IViewDataSetStringColor(p, string, color)
     IViewData p;
     unsigned string;
     int color;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  info[string].color = color;
}

IViewDataStringColor(p, string)
     IViewData p;
     unsigned string;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return(0);
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  return((int) info[string].color);
}

IViewDataSetStringString(p, string, s)
     IViewData p;
     unsigned string;
     char *s;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  StRLock(p->stringInfo);
  StFree(info[string].string);
  info[string].string = NULL;
  if (s != 0 && strlen(s) > 0) {
    info[string].string = StCalloc(sizeof(char), 1 + strlen(s));
    strcpy(info[string].string, s);
  }
  StRUnlock(p->stringInfo);
}

char *IViewDataStringString(p, string)
     IViewData p;
     unsigned string;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return(NULL);
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  return(info[string].string);
}

IViewDataSetStringModifiers(p, string, up, h, v)
     IViewData p;
     unsigned string;
     int up, h, v;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  info[string].up = up;
  info[string].h = h;
  info[string].v = v;
}

IViewDataGetStringModifiers(p, string, up, h, v)
     IViewData p;
     unsigned string;
     int *up, *h, *v;
{
  StringInfo *info;
  
  if (string >= IViewDataNumStrings(p)) return;
  
  info = (StringInfo *) StRPtr(p->stringInfo);
  if (up != NULL) *up = info[string].up;
  if (h != NULL) *h = info[string].h;
  if (v != NULL) *v = info[string].v;
}
#endif /* USESTRINGS */

/**************************************************************************/
/**                                                                      **/
/**                          Data Drawing Functions                      **/
/**                                                                      **/
/**************************************************************************/

VOID IViewDataDrawPoints P7C(IViewData, data, IVIEW_WINDOW, w,
                             unsigned, var1, unsigned, var2,
                             unsigned, m, unsigned, n,
                             int, offset)
{
  int vars = IViewNumVariables(w);
  int right, bottom;
  int point, left, top, width, height, orig_x, orig_y;
  int showingLabels = IViewShowingLabels(w);
  int x, y, sym, hsym;
  PointState state;
  PointInfo *info;
  Fixed *screen1, *screen2;
  StGWWinInfo *gwinfo = IViewWindowWinInfo(w);
  int mode = StGWDrawMode(gwinfo);
  int draw_color = 0, use_color = StGWUseColor(gwinfo);
  
  if (n > IViewNumPoints(w)) return;
  if (var1 >= vars || var2 >= vars) return;
  if (data == NULL || data->pointInfo == NULL || data->points == NULL) return;
  if (data->points->screen_data[var1] == NULL 
      || data->points->screen_data[var2] == NULL) return;
  
  if (data->points->recalculateScreenPoints)
    RecalculateScreenPoints(data->points);
  info = (PointInfo *) StRPtr(data->pointInfo);
  screen1 = (Fixed *) StRPtr(data->points->screen_data[var1]);
  screen2 = (Fixed *) StRPtr(data->points->screen_data[var2]);

  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &orig_x, &orig_y);
  right = left + width;
  bottom = top + height;

  if (use_color) draw_color = StGWDrawColor(gwinfo);
  for (point = m; point < n; point++, info++) {/*** info not right if m > 0 */
    state = info->state;
    sym = info->symbol.normal;
    hsym = info->symbol.highlighted;
    if (! info->masked && state != pointInvisible) {
#ifdef PERSPECTIVE
      x = orig_x + PerspScreenValue(data->points, var1, point);
      y = orig_y - PerspScreenValue(data->points, var2, point);
#else
      x = orig_x + FixRound(screen1[point]);
      y = orig_y - FixRound(screen2[point]);
#endif /* PERSPECTIVE */
      if (TRUE /* x >= left && x <= right && y >= top && y <= bottom */) {
        if (use_color) {
          if (info->color != NOCOLOR) StGWSetDrawColor(gwinfo, info->color);
          else StGWSetDrawColor(gwinfo, draw_color);
        }
        if (state == pointNormal) StGWDrawSymbol(gwinfo, sym, x, y);
        else {
          StGWDrawSymbol(gwinfo, hsym, x, y);
          if (showingLabels) {
            StGWSetDrawMode(gwinfo, 1);
            StGWDrawString(gwinfo, info->label, x + offset, y - offset);
            StGWSetDrawMode(gwinfo, mode);
          }
        }
      }
    }
  }
  if (use_color) StGWSetDrawColor(gwinfo, draw_color);
}

VOID IViewDataCuePoints P7C(IViewData, data, unsigned, var,
                            int, cut1, int, cut2, int, cut3,
                            unsigned, m, unsigned, n)
{
  int vars;
  int point;
  PointInfo *info;
  Fixed fcut1, fcut2, fcut3;
  Fixed *screen_z, z;
  
  if (data == NULL || data->pointInfo == NULL || data->points == NULL) return;
  if (data->points->screen_data[var] == NULL) return;
  if (n > IViewDataNumPoints(data)) return;
  vars = IViewDataNumVariables(data);
  if (var >= vars) return;
  
  if (data->points->recalculateScreenPoints)
    RecalculateScreenPoints(data->points);
  info = (PointInfo *) StRPtr(data->pointInfo);
  screen_z = (Fixed *) StRPtr(data->points->screen_data[var]);

  fcut1 = Int2Fixed(cut1);
  fcut2 = Int2Fixed(cut2);
  fcut3 = Int2Fixed(cut3);
  for (point = m; point < n; point++, info++) {
    z = screen_z[point];
    if (z < fcut1) info->symbol.normal = 0;
    else if (z < fcut2) info->symbol.normal = 1;
    else if (z < fcut3) info->symbol.normal = 2;
    else info->symbol.normal = 3;
    info->symbol.highlighted = 5;
  }
}

#ifdef PERSPECTIVE
static PerspScreenValue P3C(IViewBasicPoints, p, unsigned, var, unsigned, point)
{
  Fixed screen_x, screen_z, d = 800;

  if (p->num_variables > 2 && p->num_variables > var) {
    screen_z = FixRound(((Fixed *) StRPtr(p->screen_data[2]))[point]);
    /* could create problems if StRPtr return value is zero JKL */
    screen_x = FixRound(((Fixed *) StRPtr(p->screen_data[var]))[point]);
    if (screen_z != d) {
      screen_x = (screen_x * d) / (d - screen_z);
      return((int) screen_x);
    }
    else return(FixRound(screen_x));
  }
  else return(0);
}
#endif /* PERSPECTIVE */

VOID IViewDataDrawLines P6C(IViewData, data, IVIEW_WINDOW, w,
                            unsigned, var1, unsigned, var2,
                            unsigned, m, unsigned, n)
{
  int vars = IViewNumVariables(w);
  int i, left, top, width, height, orig_x, orig_y, use_color;
  int line_type, line_width, draw_color=0;
  int old_line_type, old_line_width, old_draw_color;
  StGWWinInfo *gwinfo;
  LineInfo *info;
  Fixed *screen1, *screen2;
  int x, y, nx, ny;
  int next;
  /*  int right, bottom;*/
  int type, color, linewidth;
  
  gwinfo = IViewWindowWinInfo(w);
  
  if (var1 >= vars || var2 >= vars) return;
  if (n > IViewNumLines(w)) return;
  
  if (data == NULL || data->lineInfo == NULL || data->lines == NULL) return;
  if (data->lines->screen_data[var1] == NULL 
      || data->lines->screen_data[var2] == NULL) return;
  
  if (data->lines->recalculateScreenPoints)
    RecalculateScreenPoints(data->lines);
  info = (LineInfo *) StRPtr(data->lineInfo);
  screen1 = (Fixed *) StRPtr(data->lines->screen_data[var1]);
  screen2 = (Fixed *) StRPtr(data->lines->screen_data[var2]);

  StGrGetContentRect(gwinfo, &left, &top, &width, &height);
  StGrGetContentOrigin(gwinfo, &orig_x, &orig_y);
  use_color = StGWUseColor(gwinfo);
  line_type = StGWLineType(gwinfo);
  StGWGetLineWidth(gwinfo, &line_width);
  if (use_color) draw_color = StGWDrawColor(gwinfo);
  linewidth = old_line_width = line_width;
  type = old_line_type = line_type;
  color = old_draw_color = draw_color;
  /* right = left + width; bottom = top + height; */
  
  for (i = m; i < n; i++) {  
    if (i >= n || info[i].masked) continue;
    x = orig_x + FixRound(screen1[i]);
    y = orig_y - FixRound(screen2[i]);
    /* if (x < left || x > right || y < top || y > bottom) continue;*/

    next = info[i].next;
    if (next >= n || next < 0 || info[next].masked) continue;
    nx = orig_x + FixRound(screen1[next]);
    ny = orig_y - FixRound(screen2[next]);
    /*  if (nx < left || nx > right || ny < top || ny > bottom) continue;*/
  
    /**** avoid drawing state changes if not needed; maybe pack values */
    if (info[i].width != linewidth) {
	  linewidth = info[i].width;
      StGWSetLineWidth(gwinfo, linewidth);
	}
    if (use_color) {
	  if (info[i].color != color) {
        color = info[i].color;
        if (color != NOCOLOR) StGWSetDrawColor(gwinfo, color);
        else StGWSetDrawColor(gwinfo, draw_color);
	  }
    }
    if (info[i].type != type) {
	  type = info[i].type;
      StGWSetLineType(gwinfo, type);
	}
	
    StGWDrawLine(gwinfo, x, y, nx, ny);
  }

  StGWSetLineType(gwinfo, old_line_type);
  StGWSetLineWidth(gwinfo, old_line_width);
  if (use_color) StGWSetDrawColor(gwinfo, old_draw_color);
}
/**** do cuing also */
