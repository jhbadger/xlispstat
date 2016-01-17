/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"
#include "gnuplot.h"

int do_plot(struct curve_points *plots,
	    int pcount,
	    double xmin, double xmax, double ymin, double ymax);

extern LVAL coerce_to_list();
#ifndef ANSI
extern char *calloc();
#endif

static double NiceValue(double x)
{
  long ilx;
  double lx, v1, v2, v3, v4;
	
  if (x <= 0) return (0.0);
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

static void SetNiceRange(double *xmin, double *xmax, int *ticks)
{
  double delta;
	
  if ((*xmax <= *xmin) || (*ticks < 2)) return;
	
  delta = NiceValue((*xmax - *xmin) / (*ticks - 1));
	
  *xmin = floor(*xmin / delta) * delta;
  *xmax = ceil(*xmax / delta) * delta;
	
  *ticks = 1 + (.01 + (*xmax - *xmin) / delta);/* add .01 for rounding */
}

static void set_range(double *low, double *high)
{
  int ticks = 4;

  if (*low == *high) {
    (*low)--;
    (*high)++;
  }
  else SetNiceRange(low, high, &ticks);
}

static LVAL gnupointlineplot(enum PLOT_STYLE plot_style)
{
  LVAL next, nextx, nexty;
  int n, i;
  struct curve_points *this_plot, real_plot;
  struct coordinate *pts;
  double xmin, xmax, ymin, ymax, dx, dy;
  LVAL x, y;
  
  xlstkcheck(2);
  xlsave(x);
  xlsave(y);

  x = xlgetarg();
  if (consp(x) && seqp(car(x))) {
    y = car(cdr(x));
    x = car(x);
  }
  else y = xlgetarg();
  x = coerce_to_list(x);
  y = coerce_to_list(y);

  n = llength(x);
  if (llength(y) != n) xlfail("lengths do not match");
  for (next = x; consp(next); next = cdr(next))
    if (! realp(car(next))) xlerror("not a real number", car(next));
  for (next = y; consp(next); next = cdr(next))
    if (! realp(car(next))) xlerror("not a real number", car(next));

  pts = (struct coordinate *) calloc(sizeof(struct coordinate), n);
  if (! pts) xlfail("allocation failed");

  this_plot = &real_plot;
  this_plot->points = (struct coordinate *) pts;
  this_plot->next_cp = 0;
  this_plot->plot_type = DATA;
  this_plot->plot_style = plot_style;
  this_plot->title = "";
  this_plot->p_count = n;

  xmin = HUGE;
  xmax = -HUGE;
  ymin = HUGE;
  ymax = -HUGE;
  for(i = 0, nextx = x, nexty = y; i < n;
      i++, nextx = cdr(nextx), nexty = cdr(nexty)) {
    dx = makefloat(car(nextx));
    dy = makefloat(car(nexty));
    this_plot->points[i].undefined = FALSE;
    this_plot->points[i].x = dx;
    this_plot->points[i].y = dy;
    if (dx > xmax) xmax = dx;
    if (dx < xmin) xmin = dx;
    if (dy > ymax) ymax = dy;
    if (dy < ymin) ymin = dy;
  }

  set_range(&xmin, &xmax);
  set_range(&ymin, &ymax);

  do_plot(this_plot, 1, xmin, xmax, ymin, ymax);

  free(pts);
  xlpopn(2);

  return(NIL);
}

LVAL gnupointplot() { return(gnupointlineplot(POINTS)); }
LVAL gnulineplot() { return(gnupointlineplot(LINES)); }
