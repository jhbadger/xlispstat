/* distributions - Basic continuous probability distributions          */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/* forward declarations */
LOCAL double logbeta P2H(double, double);
LOCAL double betadens P3H(double, double, double);
LOCAL double gammadens P2H(double, double);
LOCAL double tdens P2H(double, double);
LOCAL VOID checkstrict P1H(double);

/***************************************************************************/
/**                                                                       **/
/**                         Argument Readers                              **/
/**                                                                       **/
/***************************************************************************/

static VOID getbetaargs P4C(double *, pa, double *, pb, int *, pia, int *, pib)
{
  LVAL La, Lb;
  double da, db;
  
  La = xlgetarg(); da = makefloat(La);
  Lb = xlgetarg(); db = makefloat(Lb);
  xllastarg();
  if (da <= 0.0) xlerror("alpha is too small", La);
  if (db <= 0.0) xlerror("beta is too small", Lb);
  
  if (pa != NULL) *pa = da; 
  if (pb != NULL) *pb = db;
  if (pia != NULL) *pia = floor(da);
  if (pib != NULL) *pib = floor(db);
}

static VOID getgxtarg P1C(double *, pa)
{
  LVAL La;
  double da;
  
  La = xlgetarg(); da = makefloat(La);
  xllastarg();
  if (da <= 0.0) xlerror("alpha is too small", La);
  if (pa != NULL) *pa = da; 
}

static VOID getfargs P5C(double *, px, double *, pa, double *, pb,
                         int *, pia, int *, pib)
{
  LVAL La, Lb;
  double da, db;
  
  La = xlgetarg(); da = makefloat(La);
  Lb = xlgetarg(); db = makefloat(Lb);
  xllastarg();
  if (da <= 0.0) xlerror("alpha is too small", La);
  if (db <= 0.0) xlerror("beta is too small", Lb);
  da = 0.5 * da; db = 0.5 * db; 
  
  if (px != NULL) *px = db / (db + da * *px);
  if (pa != NULL) *pa = da; 
  if (pb != NULL) *pb = db;
  if (pia != NULL) *pia = floor(da);
  if (pib != NULL) *pib = floor(db);
}

static double getXarg(V) { return(makefloat(xlgetarg())); }
     
static VOID check_one P2C(LVAL, p, double, dp)
{
  if (dp < 0.0 || dp >= 1.0)
    xlerror("probability not between 0 and 1", p);
}

/***************************************************************************/
/**                                                                       **/
/**                          Numerical Cdf's                              **/
/**                                                                       **/
/***************************************************************************/

LOCAL LVAL normalcdf(V)
{
  double dx, dp;
  
  dx = getXarg();
  normbase(&dx, &dp);
  return(cvflonum((FLOTYPE) dp));
}

LOCAL LVAL betacdf(V)
{
  double dx, da, db, dp;
  int ia, ib;
  
  dx = getXarg();
  getbetaargs(&da, &db, &ia, &ib);
  betabase(&dx, &da, &db, &ia, &ib, &dp);
  return(cvflonum((FLOTYPE) dp));
}

LOCAL LVAL gammacdf(V)
{
  double dx, da, dp;
  
  dx = getXarg();
  getgxtarg(&da);
  gammabase(&dx, &da, &dp);
  return(cvflonum((FLOTYPE) dp));
}

LOCAL LVAL chisqcdf(V)
{
  double dx, da, dp;
  
  dx = getXarg();
  getgxtarg(&da);
  da = 0.5 * da; dx = 0.5 * dx;
  gammabase(&dx, &da, &dp);
  return(cvflonum((FLOTYPE) dp));
}

LOCAL LVAL tcdf(V)
{
  double dx, da, dp;
  
  dx = getXarg();
  getgxtarg(&da);
  studentbase(&dx, &da, &dp);
  return(cvflonum((FLOTYPE) dp));
}

LOCAL LVAL fcdf(V)
{
  double dx, da, db, dp;
  int ia, ib;
  
  dx = getXarg();
  getfargs(&dx, &da, &db, &ia, &ib);
  betabase(&dx, &db, &da, &ib, &ia, &dp);
  dp = 1.0 - dp;
  return(cvflonum((FLOTYPE) dp));
}

LOCAL LVAL cauchycdf(V)
{
  double dx, dp;
  
  dx = getXarg();
  dp = (atan(dx) + PI / 2) / PI;
  return(cvflonum((FLOTYPE) dp));
}

/* log-gamma function does not really belong, but... */
LOCAL LVAL loggamma(V)
{
  LVAL x;
  double dx, dp;
  
  x = xlgetarg();
  dx = makefloat(x);
  if (dx <= 0) xlerror("non positive argument", x);
  dp = gamma(dx);
  return(cvflonum((FLOTYPE) dp));
}

/* bivariate normal cdf */
LOCAL LVAL bnormcdf(V)
{
  LVAL R;
  double x, y, r;
  x = makefloat(xlgetarg());
  y = makefloat(xlgetarg());
  R = xlgetarg(); r = makefloat(R);
  xllastarg();
  
  if (r < -1 || r > 1) xlerror("correlation out of range", R);
  return(cvflonum((FLOTYPE) bivnor(-x, -y, r)));
}

/* recursive distribution functions */
LVAL xsrnormalcdf(V)
    { return(recursive_subr_map_elements(normalcdf, xsrnormalcdf)); }
LVAL xsrbetacdf(V) 
    { return(recursive_subr_map_elements(betacdf, xsrbetacdf));   }
LVAL xsrgammacdf(V)
    { return(recursive_subr_map_elements(gammacdf, xsrgammacdf));  }
LVAL xsrchisqcdf(V)
    { return(recursive_subr_map_elements(chisqcdf, xsrchisqcdf));  }
LVAL xsrtcdf(V)
    { return(recursive_subr_map_elements(tcdf, xsrtcdf));      }
LVAL xsrfcdf(V)
    { return(recursive_subr_map_elements(fcdf, xsrfcdf));      }
LVAL xsrcauchycdf(V)
    { return(recursive_subr_map_elements(cauchycdf, xsrcauchycdf)); }
LVAL xsrloggamma(V) 
    { return(recursive_subr_map_elements(loggamma, xsrloggamma));  }
LVAL xsrbnormcdf(V) 
    { return(recursive_subr_map_elements(bnormcdf, xsrbnormcdf));  }

/***************************************************************************/
/**                                                                       **/
/**                    Numerical Quantile Functions                       **/
/**                                                                       **/
/***************************************************************************/

LOCAL LVAL quant P1C(int, dist)
{
  LVAL p;
  double dp, da, db, dx=0.0;
  int ia, ib;

  p = xlgetarg(); dp = makefloat(p);
  if (dp < 0.0 || dp > 1.0) xlerror("probability out of range", p);

  switch (dist) {
  case 'N': xllastarg(); checkstrict(dp); dx = ppnd(dp, &ia); break;
  case 'C': xllastarg(); checkstrict(dp); dx = tan(PI * (dp - 0.5)); break;
  case 'B': getbetaargs(&da, &db, &ia, &ib);
            check_one(p, dp);
            dx = ppbeta(dp, da, db, &ia);
            break;
  case 'G': getgxtarg(&da);
            db = 0.0;
            check_one(p, dp);
            dx = ppgamma(dp, da, &ia);
            break;
  case 'X': getgxtarg(&da);
            da = 0.5 * da; db = 0.0;
            check_one(p, dp);
            dx = 2.0 * ppgamma(dp, da, &ia);
            break;
  case 'T': getgxtarg(&da);
            db = 0.0;
            checkstrict(dp); 
            dx = ppstudent(dp, da, &ia);
            break;
  case 'F': getfargs(NULL, &da, &db, &ia, &ib);
            check_one(p, dp);
            if (dp == 0.0) dx = 0.0;
            else {           
              dp = 1.0 - dp;
              dx = ppbeta(dp, db, da, &ia);
              dx = db * (1.0 / dx - 1.0) / da;
            }
            break;
  default:  xlfail("unknown distribution");
  }
  return(cvflonum((FLOTYPE) dx));
}

LOCAL LVAL normalquant(V) { return(quant('N')); }
LOCAL LVAL cauchyquant(V) { return(quant('C')); }
LOCAL LVAL betaquant(V)   { return(quant('B')); }
LOCAL LVAL gammaquant(V)  { return(quant('G')); }
LOCAL LVAL chisqquant(V)  { return(quant('X')); }
LOCAL LVAL tquant(V)      { return(quant('T')); }
LOCAL LVAL fquant(V)      { return(quant('F')); }

/* recursive quantile functions */
LVAL xsrnormalquant(V)  
    { return(recursive_subr_map_elements(normalquant, xsrnormalquant)); }
LVAL xsrcauchyquant(V)
    { return(recursive_subr_map_elements(cauchyquant, xsrcauchyquant)); }
LVAL xsrbetaquant(V)
    { return(recursive_subr_map_elements(betaquant, xsrbetaquant)); }
LVAL xsrgammaquant(V)
    { return(recursive_subr_map_elements(gammaquant, xsrgammaquant)); }
LVAL xsrchisqquant(V)
    { return(recursive_subr_map_elements(chisqquant, xsrchisqquant)); }
LVAL xsrtquant(V)
    { return(recursive_subr_map_elements(tquant, xsrtquant)); }
LVAL xsrfquant(V)
    { return(recursive_subr_map_elements(fquant, xsrfquant)); }

/***************************************************************************/
/**                                                                       **/
/**                    Numerical Density Functions                       **/
/**                                                                       **/
/***************************************************************************/

LOCAL LVAL dens P1C(int, dist)
{
  LVAL x;
  double dx, da, db, dens=0.0;

  x = xlgetarg(); dx = makefloat(x);

  switch (dist) {
  case 'N': xllastarg(); dens = exp(- 0.5 * dx * dx) / sqrt(2.0 * PI); break;
  case 'B': getbetaargs(&da, &db, NULL, NULL);
            dens = betadens(dx, da, db);
            break;
  case 'G': getgxtarg(&da);
            dens = gammadens(dx, da);
            break;
  case 'X': getgxtarg(&da);
            da = 0.5 * da; dx = 0.5 * dx;
            dens = 0.5 * gammadens(dx, da);
            break;
  case 'T': getgxtarg(&da);
            dens = tdens(dx, da);
            break;
  case 'F': getbetaargs(&da, &db, NULL, NULL);
            if (dx <= 0.0) dens = 0.0;
            else {
              dens = exp(0.5 * da * log(da) + 0.5 * db *log(db)
                         + (0.5 * da - 1.0) * log(dx)
                         - logbeta(0.5 * da, 0.5 * db)
                         - 0.5 * (da + db) * log(db + da * dx));
            }
            break;
  case 'C': xllastarg(); dens = tdens(dx, 1.0); break;
  default:  xlfail(" unknown distribution");
  }
  
  return(cvflonum((FLOTYPE) dens));
}

/* density functions */
LOCAL LVAL normal_dens(V) { return(dens('N')); }
LOCAL LVAL cauchy_dens(V) { return(dens('C')); }
LOCAL LVAL beta_dens(V)   { return(dens('B')); }
LOCAL LVAL gamma_dens(V)  { return(dens('G')); }
LOCAL LVAL chisq_dens(V)  { return(dens('X')); }
LOCAL LVAL t_dens(V)      { return(dens('T')); }
LOCAL LVAL f_dens(V)      { return(dens('F')); }

/* recursive density functions */
LVAL xsrnormaldens(V)  
    { return(recursive_subr_map_elements(normal_dens, xsrnormaldens)); }
LVAL xsrcauchydens(V)
    { return(recursive_subr_map_elements(cauchy_dens, xsrcauchydens)); }
LVAL xsrbetadens(V)
    { return(recursive_subr_map_elements(beta_dens, xsrbetadens)); }
LVAL xsrgammadens(V)
    { return(recursive_subr_map_elements(gamma_dens, xsrgammadens)); }
LVAL xsrchisqdens(V)
    { return(recursive_subr_map_elements(chisq_dens, xsrchisqdens)); }
LVAL xsrtdens(V)
    { return(recursive_subr_map_elements(t_dens, xsrtdens)); }
LVAL xsrfdens(V)
    { return(recursive_subr_map_elements(f_dens, xsrfdens)); }

LOCAL double logbeta P2C(double, a, double, b)
{
  static double da = 0.0, db = 0.0, lbeta = 0.0;
  
  if (a != da || b != db) { /* cache most recent call */
    da = a; db = b;
    lbeta = gamma(da) + gamma(db) - gamma(da + db);
  }
  return(lbeta);
}

LOCAL double betadens P3C(double, x, double, a, double, b)
{
  double dens;
  
  if (x <= 0.0 || x >= 1.0) dens = 0.0;
  else {
    dens = exp(log(x) * (a - 1) + log(1 - x) * (b - 1) - logbeta(a, b));
  }
  return(dens);
}

LOCAL double gammadens P2C(double, x, double, a)
{
  double dens;
  if (x <= 0.0) dens = 0.0;
  else {
    dens = exp(log(x) * (a - 1) - x - gamma(a));
  }
  return(dens);
}

LOCAL double tdens P2C(double, x, double, a)
{
  double dens;
  
  dens = (1.0 / sqrt(a * PI)) 
       * exp(gamma(0.5 * (a + 1)) - gamma(0.5 * a) 
             - 0.5 * (a + 1) * log(1.0 + x * x / a));
  return(dens);
}

LOCAL VOID checkstrict P1C(double, dp)
{
  if (dp <= 0.0 || dp >= 1.0)
    xlfail("probability not strictly between 0 and 1");
}

LOCAL double getposdouble(V)
{
  LVAL x;
  double dx;
  
  x = xlgetarg();
  dx = makefloat(x);
  if (dx <= 0.0) xlerror("not a positive number", x);
  return(dx);
}

LOCAL double normrand(V)
{
  double x, y, u, u1, v;
  static double c = -1.0;
   
  if (c < 0.0) c = sqrt(2.0 / exp(1.0));
   
  /* ratio of uniforms with linear pretest */
  do {
    u = xlunirand();
    u1 = xlunirand();
    v = c * (2 * u1 - 1);
    x = v / u;
    y = x * x / 4.0;
  } while(y > (1 - u) && y > - log(u));
  return(x);
}

LOCAL double cauchyrand(V)
{
  double u1, u2, v1, v2;
   
  /* ratio of uniforms on half disk */
  do {
    u1 = xlunirand();
    u2 = xlunirand();
    v1 = 2.0 * u1 - 1.0;
    v2 = u2;
  } while(v1 * v1 + v2 * v2 > 1.0);
  return(v1 / v2);
}

LOCAL double gammarand P1C(double, a)
{
  double x, u0, u1, u2, v, w, c, c1, c2, c3, c4, c5;
  static double e = -1.0;
  int done;
  
  if (e < 0.0) e = exp(1.0);
  
  if (a < 1.0) {
    /* Ahrens and Dieter algorithm */
    done = FALSE;
    c = (a + e) / e;
    do {
      u0 = xlunirand();
      u1 = xlunirand();
      v = c * u0;
      if (v <= 1.0) {
        x = exp(log(v) / a);
        if (u1 <= exp(-x)) done = TRUE;
      }
      else {
        x = -log((c - v) / a);
        if (x > 0.0 && u1 < exp((a - 1.0) * log(x))) done = TRUE;
      }
    } while(! done);
  }
  else if (a == 1.0) x = -log(xlunirand());
  else {
    /* Cheng and Feast algorithm */
    c1 = a - 1.0;
    c2 = (a - 1.0 / (6.0 * a)) / c1;
    c3 = 2.0 / c1;
    c4 = 2.0 / (a - 1.0) + 2.0;
    c5 = 1.0 / sqrt(a);
    do {
      do {
        u1 = xlunirand();
        u2 = xlunirand();
        if (a > 2.5) u1 = u2 + c5 * (1.0 - 1.86 * u1);
      } while (u1 <= 0.0 || u1 >= 1.0);
      w = c2 * u2 / u1;
    } while ((c3 * u1 + w + 1.0/w) > c4 && (c3 * log(u1) - log(w) + w) > 1.0);
    x = c1 * w;
  }
  return(x);
}

LOCAL double chisqrand P1C(double, df)
{
  return(2.0 * gammarand(df / 2.0));
}

LOCAL double trand P1C(double, df)
{
  return(normrand() / sqrt(chisqrand(df) / df));
}

LOCAL double betarand P2C(double, a, double, b)
{
  double x, y;
  
  x = gammarand(a);
  y = gammarand(b);
  return(x / (x + y));
}

LOCAL double frand P2C(double, ndf, double, ddf)
{
  return((ddf * chisqrand(ndf)) / (ndf * chisqrand(ddf)));
}

LOCAL LVAL contrand P1C(int, which)
{
  LVAL next, result;
  int n;
  double dx=0.0, da=0.0, db=0.0;
  
  n = getfixnum(xlgafixnum());
  switch (which) {
  case 'G':
  case 'X': 
  case 'T': da = getposdouble(); break;
  case 'B': 
  case 'F': da = getposdouble(); db = getposdouble(); break;
  }
  xllastarg();
  
  if (n <= 0) return(NIL);
  
  /* protect result pointer */
  xlsave1(result);
  
  result = mklist(n, NIL);
  for (next = result; consp(next); next = cdr(next)) {
    switch (which) {
    case 'U': dx = xlunirand();         break;
    case 'N': dx = normrand();        break;
    case 'C': dx = cauchyrand();      break;
    case 'G': dx = gammarand(da);     break;
    case 'X': dx = chisqrand(da);     break;
    case 'T': dx = trand(da);         break;
    case 'B': dx = betarand(da, db);  break;
    case 'F': dx = frand(da, db);     break;
    }
    rplaca(next, cvflonum((FLOTYPE) dx));
  }
  
  /* restore the stack frame */
  xlpop();
  
  return(result);
}

LOCAL LVAL xsuniformrand(V) { return(contrand('U')); }
LOCAL LVAL xsnormalrand(V)  { return(contrand('N')); }
LOCAL LVAL xscauchyrand(V)  { return(contrand('C')); }
LOCAL LVAL xsgammarand(V)   { return(contrand('G')); }
LOCAL LVAL xschisqrand(V)   { return(contrand('X')); }
LOCAL LVAL xstrand(V)       { return(contrand('T')); }
LOCAL LVAL xsbetarand(V)    { return(contrand('B')); }
LOCAL LVAL xsfrand(V)       { return(contrand('F')); }

LVAL xsruniformrand(V) 
    { return(recursive_subr_map_elements(xsuniformrand, xsruniformrand)); }
LVAL xsrnormalrand(V) 
    { return(recursive_subr_map_elements(xsnormalrand, xsrnormalrand)); }
LVAL xsrcauchyrand(V) 
    { return(recursive_subr_map_elements(xscauchyrand, xsrcauchyrand)); }
LVAL xsrgammarand(V) 
    { return(recursive_subr_map_elements(xsgammarand, xsrgammarand)); }
LVAL xsrchisqrand(V) 
    { return(recursive_subr_map_elements(xschisqrand, xsrchisqrand)); }
LVAL xsrtrand(V) 
    { return(recursive_subr_map_elements(xstrand, xsrtrand)); }
LVAL xsrbetarand(V) 
    { return(recursive_subr_map_elements(xsbetarand, xsrbetarand)); }
LVAL xsrfrand(V) 
    { return(recursive_subr_map_elements(xsfrand, xsrfrand)); }
