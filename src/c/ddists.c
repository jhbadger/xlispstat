/* ddistributions - Basic discrete probability distributions           */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

/* forward declarations */
#define rand Rand /* to avoid a name conflict with system include files */
LOCAL LVAL cdf P1H(int);
LOCAL LVAL quant P1H(int);
LOCAL LVAL pmf P1H(int);
LOCAL LVAL rand P1H(int);
LOCAL double binomial_cdf P3H(int, int, double);
LOCAL double poisson_cdf P2H(int, double);
LOCAL LVAL binomialcdf(V);
LOCAL LVAL poissoncdf(V);

LOCAL LVAL binomialpmf(V);
LOCAL LVAL poissonpmf(V);
LOCAL LVAL binomialquant(V);
LOCAL LVAL poissonquant(V);
LOCAL LVAL binomialrand(V);
LOCAL LVAL poissonrand(V);
LOCAL VOID getbinargs P2H(int *, double *);
LOCAL VOID getpoisarg P1H(double *);
LOCAL double poisson_cdf P2H(int, double);
LOCAL int binomial_quant P3H(double, int, double);
LOCAL int poisson_quant P2H(double, double);
LOCAL poisson_rand P1H(double);
LOCAL int binomial_rand P2H(int, double);

/* numerical distribution function */
LOCAL LVAL binomialcdf(V)  { return(cdf('B')); }
LOCAL LVAL poissoncdf(V)   { return(cdf('P')); }

/* recursive distribution functions */
LVAL xsrbinomialcdf(V)
    { return(recursive_subr_map_elements(binomialcdf, xsrbinomialcdf)); }
LVAL xsrpoissoncdf(V)
    { return(recursive_subr_map_elements(poissoncdf, xsrpoissoncdf)); }

/* numerical probability mass function */
LOCAL LVAL binomialpmf(V)  { return(pmf('B')); }
LOCAL LVAL poissonpmf(V)   { return(pmf('P')); }

/* recursive probability mass functions */
LVAL xsrbinomialpmf(V)
    { return(recursive_subr_map_elements(binomialpmf, xsrbinomialpmf)); }
LVAL xsrpoissonpmf(V)
    { return(recursive_subr_map_elements(poissonpmf, xsrpoissonpmf)); }

/* numerical quantile function */
LOCAL LVAL binomialquant(V)  { return(quant('B')); }
LOCAL LVAL poissonquant(V)   { return(quant('P')); }

/* recursive probability mass functions */
LVAL xsrbinomialquant(V)
    { return(recursive_subr_map_elements(binomialquant, xsrbinomialquant)); }
LVAL xsrpoissonquant(V)
    { return(recursive_subr_map_elements(poissonquant, xsrpoissonquant)); }

/* random number generating functions */
LOCAL LVAL binomialrand(V)  { return(rand('B')); }
LOCAL LVAL poissonrand(V)   { return(rand('P')); }

/* recursive probability mass functions */
LVAL xsrbinomialrand(V)
    { return(recursive_subr_map_elements(binomialrand, xsrbinomialrand)); }
LVAL xsrpoissonrand(V)
    { return(recursive_subr_map_elements(poissonrand, xsrpoissonrand)); }

/* argument readers */
LOCAL VOID getbinargs P2C(int *, pn, double *, pp)
{
  LVAL Ln, Lp;
  int n;
  double p;
  
  Ln = xlgafixnum(); n = getfixnum(Ln);
  Lp = xlgetarg(); p = makefloat(Lp);
  xllastarg();
  if (n <= 0) xlerror("n is too small", Ln);
  if (p < 0.0 || p > 1.0) xlerror("p not between 0 and 1", Lp);

  if (pn != NULL) *pn = n;
  if (pp != NULL) *pp = p;
}

LOCAL VOID getpoisarg P1C(double *, plam)
{
  LVAL Llam;
  double lam;
  
  Llam = xlgetarg(); lam = makefloat(Llam);
  xllastarg();
  if (lam < 0.0) xlerror("lambda is too small", Llam);
  
  if (plam != NULL) *plam = lam;
}

/* Numerical Cdf's */
LOCAL LVAL cdf P1C(int, dist)
{
  LVAL x;
  double p, dx, lam, dp = 0.0;
  int ix, n;

  x = xlgetarg(); dx = makefloat(x); ix = floor(dx);

  switch (dist) {
  case 'B': getbinargs(&n, &p);
            dp = binomial_cdf(ix, n, p);
            break;
  case 'P': getpoisarg(&lam);
            dp = poisson_cdf(ix, lam);
            break;
  default:  xlfail(" unknown distribution");
  }
  
  return(cvflonum((FLOTYPE) dp));
}

/* Numerical Pmf's */
LOCAL LVAL pmf P1C(int, dist)
{
  LVAL x;
  double p, dx, lam, dp = 0.0;
  int ix, n;

  x = xlgafixnum(); ix = getfixnum(x); dx = ix;

  switch (dist) {
  case 'B': getbinargs(&n, &p);
            if (p == 0.0) dp = (ix == 0) ? 1.0 : 0.0;
            else if (p == 1.0) dp = (ix == n) ? 1.0 : 0.0;
            else if (dx < 0.0 || dx > n) dp = 0.0;
            else {
              dp = exp(gamma(n + 1.0) - gamma(dx + 1.0) - gamma(n - dx + 1.0)
                       + dx * log(p) + (n - dx) * log(1.0 - p));
            }
            break;
  case 'P': getpoisarg(&lam);
            if (lam == 0.0) dp = (ix == 0) ? 1.0 : 0.0;
            else if (dx < 0.0) dp = 0.0;
            else {
              dp = exp(dx * log(lam) - lam - gamma(dx + 1.0));
            }
            break;
  default:  xlfail(" unknown distribution");
  }
  
  return(cvflonum((FLOTYPE) dp));
}

/* Numerical Quantiles */
LOCAL LVAL quant P1C(int, dist)
{
  LVAL x;
  double p, dx, lam;
  int n, k = 0;

  x = xlgetarg(); dx = makefloat(x);
  if (dx <= 0.0 || dx >= 1.0) xlerror("probability not between 0 and 1", x);
  
  switch (dist) {
  case 'B': getbinargs(&n, &p);
            if (p == 0.0) k = 0;
            else if (p == 1.0) k = n;
            else k = binomial_quant(dx, n, p);
            break;
  case 'P': getpoisarg(&lam);
            if (lam == 0.0) k = 0;
            else k = poisson_quant(dx, lam);
            break;
  default:  xlfail(" unknown distribution");
  }
  
  return(cvfixnum((FIXTYPE) k));
}

/* Random Number Generators */
LOCAL LVAL rand P1C(int, dist)
{
  LVAL M, sample, variate;
  double p, lam;
  int m, n, i;

  M = xlgafixnum(); m = getfixnum(M);
  if (m <= 0) xlerror("non positive number of variates", M);
  
  xlstkcheck(2);
  xlsave(sample);
  xlsave(variate);
  
  sample = NIL;
  
  switch (dist) {
  case 'B': getbinargs(&n, &p);
            for (i = 0; i < m; i++) {
              variate = cvfixnum((FIXTYPE) binomial_rand(n, p));
              sample = cons(variate, sample);
            }
            break;
  case 'P': getpoisarg(&lam);
            for (i = 0; i < m; i++) {
              variate = cvfixnum((FIXTYPE) poisson_rand(lam));
              sample = cons(variate, sample);
            }
            break;
  default:  xlfail(" unknown distribution");
  }

  xlpopn(2);
  
  return(sample);
}

LOCAL double binomial_cdf P3C(int, k, int, n, double, p)
{
  double da, db, dp;
  int ia, ib;
  
  if (k < 0) dp = 0.0;
  else if (k >= n) dp = 1.0;
  else if (p == 0.0) dp = (k < 0) ? 0.0 : 1.0;
  else if (p == 1.0) dp = (k < n) ? 0.0 : 1.0;
  else {
  da = k + 1;
  db = n - k;
  ia = floor(da); ib = floor(db);
  betabase(&p, &da, &db, &ia, &ib, &dp);
   dp = 1.0 - dp;
  }
  return(dp);
}

LOCAL double poisson_cdf P2C(int, k, double, L)
{
  double dp, dx;
  
  if (k < 0) dp = 0.0;
  else if (L == 0.0) dp = (k < 0) ? 0.0 : 1.0;
  else {
    dx = k + 1.0;
    gammabase(&L, &dx, &dp);
    dp = 1.0 - dp;
  }
  return(dp);
}

LOCAL int binomial_quant P3C(double, x, int, n, double, p)
{
  int k, k1, k2, del, ia;
  double m, s, p1, p2, pk;
  
  m = n * p;
  s = sqrt(n * p * (1 - p));
  del = max(1, (int) (0.2 * s));
  
  k = m + s * ppnd(x, &ia);
  k1 = k; k2 = k;
  
  do {
    k1 = k1 - del; k1 = max(0, k1);
    p1 = binomial_cdf(k1, n, p);
  } while (k1 > 0 && p1 > x);
  if (k1 == 0 && p1 >= x) return(k1);
  
  do {
    k2 = k2 + del; k2 = min(n, k2);
    p2 = binomial_cdf(k2, n, p);
  } while (k2 < n && p2 < x);
  if (k2 == n && p2 <= x) return(k2);
  
  while (k2 - k1 > 1) {
    k = (k1 + k2) / 2;
    pk = binomial_cdf(k, n, p);
    if (pk < x) { k1 = k; p1 = pk; }
    else { k2 = k; p2 = pk; }
  }
  return(k2);
}

LOCAL int poisson_quant P2C(double, x, double, L)
{
  int k, k1, k2, del, ia;
  double m, s, p1, p2, pk;
  
  m = L;
  s = sqrt(L);
  del = max(1, (int) (0.2 * s));
  
  k = m + s * ppnd(x, &ia);
  k1 = k; k2 = k;
  
  do {
    k1 = k1 - del; k1 = max(0, k1);
    p1 = poisson_cdf(k1, L);
  } while (k1 > 0 && p1 > x);
  if (k1 == 0 && p1 >= x) return(k1);
  
  do {
    k2 = k2 + del;
    p2 = poisson_cdf(k2, L);
  } while (p2 < x);
  
  while (k2 - k1 > 1) {
    k = (k1 + k2) / 2;
    pk = poisson_cdf(k, L);
    if (pk < x) { k1 = k; p1 = pk; }
    else { k2 = k; p2 = pk; }
  }
  return(k2);
}

/* poisson random generator from Numerical Recipes */
LOCAL int poisson_rand P1C(double, xm)
{
  static double sqrt2xm, logxm, expxm, g, oldxm = -1.0;
  double t, y;
  int k;
  
  if (xm < 12.0) {
    if (xm != oldxm) { expxm = exp(-xm); oldxm = xm; }
    k = -1;
    t = 1.0;
    do {
      k++;
      t *= xlunirand();
    } while (t > expxm);
  }
  else {
    if (xm != oldxm) {
      oldxm = xm;
      sqrt2xm = sqrt(2.0 * xm);
      logxm = log(xm);
      g = xm * logxm - gamma(xm + 1.0);
    }
    do {
      do {
        y = tan(PI * xlunirand());
        k = floor(sqrt2xm * y + xm);
      } while (k < 0);
      t = 0.9 * (1.0 + y * y) * exp(k * logxm - gamma(k + 1.0) - g);
    } while (xlunirand() > t);
  }
  return (k);
}

/* binomial random generator from Numerical Recipes */
LOCAL int binomial_rand P2C(int, n, double, pp)
{
  int j, k;
  static int nold = -1;
  double am, em, g, p, sq, t, y;
  static double pold = -1.0, pc, plog, pclog, en, oldg;
  
  p = (pp <= 0.5) ? pp : 1.0 - pp;
  
  am = n * p;
  if (p == 0.0) k = 0;
  else if (p == 1.0) k = n;
  else if (n < 50) {
    k = 0;
    for (j = 0; j < n; j++) if (xlunirand() < p) k++;
  }
  else if (am < 1.0) {
    g = exp(-am);
    t = 1.0;
    k = -1;
    do {
      k++;
      t *= xlunirand();
    } while (t > g);
    if (k > n) k = n;
  }
  else {
    if (n != nold) {
      en = n;
      oldg = gamma(en + 1.0);
      nold = n;
    }
    if (p != pold) {
      pc = 1.0 - p;
      plog = log(p);
      pclog = log(pc);
      pold = p;
    }
    sq = sqrt(2.0 * am * pc);
    do {
      do {
        y = tan(PI * xlunirand());
        em = sq * y + am;
      } while (em < 0.0 || em >= en + 1.0);
      em = floor(em);
      t = 1.2 * sq * (1.0 + y * y)
        * exp(oldg - gamma(em + 1.0) - gamma(en - em + 1.0)
              + em * plog + (en - em) * pclog);
    } while (xlunirand() > t);
    k = em;
  }
  if (p != pp) k = n - k;
  return(k);
}
