#include "xlisp.h"
#include "xlstat.h"
#include "linalg.h"

#define PLIMIT 1e4
#define XBIG 1e8
#define TOL 1e-14
#define OFLO 1e37

/*
 * ALGORITHM AS239  APPL. STATIST. (1988) VOL. 37, NO. 3
 * Computation of the Incomplete Gamma Integral
 * Translated by f2c and modified.
 */

VOID gammabase P3C(double *, x, double *, p, double *, val)
{
  /* Local variables */
  double a, b, c, an, rn;
  double pn1, pn2, pn3, pn4, pn5, pn6, arg;

  *val = 0.0;
  if (*x <= 0 || *p <= 0.0) {
    return;
  }
  else if (*p > PLIMIT) {
    /* Use a normal approximation if P > PLIMIT */
    pn1 = sqrt(*p) * 3.0 * (pow(*x / *p, 1.0 / 3.0) + 1.0 / (*p * 9.) - 1.0);
    normbase(&pn1, val);
    return;
  }
  else if (*x > XBIG) {
    /* If X is extremely large compared to P then set value = 1 */
    *val = 1.0;
  }
  else if (*x <= 1.0 || *x < *p) {
    /* Use Pearson's series expansion. */
    /* (Note that P is not large enough to force overflow in gamma). */
    arg = *p * log(*x) - *x - gamma(*p + 1.0);
    c = 1.0;
    *val = 1.0;
    a = *p;
    do {
      a += 1.0;
      c = c * *x / a;
      *val += c;
    } while (c > TOL);
    arg += log(*val);
    *val = exp(arg);
  }
  else {
    /* Use a continued fraction expansion */
    arg = *p * log(*x) - *x - gamma(*p);
    a = 1.0 - *p;
    b = a + *x + 1.0;
    c = 0.0;
    pn1 = 1.0;
    pn2 = *x;
    pn3 = *x + 1.0;
    pn4 = *x * b;
    *val = pn3 / pn4;
    while (TRUE) {
      a += 1.0;
      b += 2.0;
      c += 1.0;
      an = a * c;
      pn5 = b * pn3 - an * pn1;
      pn6 = b * pn4 - an * pn2;
      if (abs(pn6) > 0.0) {
	rn = pn5 / pn6;
	if (abs(*val - rn) <= TOL * min(1.0,rn))
	  break;
	*val = rn;
      }

      pn1 = pn3;
      pn2 = pn4;
      pn3 = pn5;
      pn4 = pn6;
      if (abs(pn5) >= OFLO) {
	/* Re-scale terms in continued fraction if terms are large */
	pn1 /= OFLO;
	pn2 /= OFLO;
	pn3 /= OFLO;
	pn4 /= OFLO;
      }
    }
    arg += log(*val);
    *val = 1.0 - exp(arg);
  }
}

static double gammad_ P3C(double *, x, double *, a, int *,iflag)
{
  double cdf;

  gammabase(x, a, &cdf);
  return(cdf);
}

/*
  ppchi2.f -- translated by f2c and modified

  Algorithm AS 91   Appl. Statist. (1975) Vol.24, P.35
  To evaluate the percentage points of the chi-squared
  probability distribution function.
  
  p must lie in the range 0.000002 to 0.999998,
    (but I am using it for 0 < p < 1 - seems to work)
  v must be positive,
  g must be supplied and should be equal to ln(gamma(v/2.0)) 
  
  Auxiliary routines required: ppnd = AS 111 (or AS 241) and gammad_.
*/

static double ppchi2 P4C(double *, p, double *, v, double *, g, int *, ifault)
{
  /* Initialized data */

  static double aa = .6931471806;
  static double six = 6.;
  static double c1 = .01;
  static double c2 = .222222;
  static double c3 = .32;
  static double c4 = .4;
  static double c5 = 1.24;
  static double c6 = 2.2;
  static double c7 = 4.67;
  static double c8 = 6.66;
  static double c9 = 6.73;
  static double e = 5e-7;
  static double c10 = 13.32;
  static double c11 = 60.;
  static double c12 = 70.;
  static double c13 = 84.;
  static double c14 = 105.;
  static double c15 = 120.;
  static double c16 = 127.;
  static double c17 = 140.;
  static double c18 = 1175.;
  static double c19 = 210.;
  static double c20 = 252.;
  static double c21 = 2264.;
  static double c22 = 294.;
  static double c23 = 346.;
  static double c24 = 420.;
  static double c25 = 462.;
  static double c26 = 606.;
  static double c27 = 672.;
  static double c28 = 707.;
  static double c29 = 735.;
  static double c30 = 889.;
  static double c31 = 932.;
  static double c32 = 966.;
  static double c33 = 1141.;
  static double c34 = 1182.;
  static double c35 = 1278.;
  static double c36 = 1740.;
  static double c37 = 2520.;
  static double c38 = 5040.;
  static double zero = 0.;
  static double half = .5;
  static double one = 1.;
  static double two = 2.;
  static double three = 3.;

/*
  static double pmin = 2e-6;
  static double pmax = .999998;
*/
  static double pmin = 0.0;
  static double pmax = 1.0;

  /* System generated locals */
  double ret_val, d_1, d_2;
  
  /* Local variables */
  static double a, b, c, q, t, x, p1, p2, s1, s2, s3, s4, s5, s6, ch;
  static double xx;
  static int if1;


  /* test arguments and initialise */
  ret_val = -one;
  *ifault = 1;
  if (*p <= pmin || *p >= pmax) return ret_val;
  *ifault = 2;
  if (*v <= zero) return ret_val;
  *ifault = 0;
  xx = half * *v;
  c = xx - one;

  if (*v < -c5 * log(*p)) {
    /* starting approximation for small chi-squared */
    ch = pow(*p * xx * exp(*g + xx * aa), one / xx);
    if (ch < e) {
      ret_val = ch;
      return ret_val;
    }
  }
  else if (*v > c3) {
    /* call to algorithm AS 111 - note that p has been tested above. */
    /* AS 241 could be used as an alternative. */
    x = ppnd(*p, &if1);

    /* starting approximation using Wilson and Hilferty estimate */
    p1 = c2 / *v;
    /* Computing 3rd power */
    d_1 = x * sqrt(p1) + one - p1;
    ch = *v * (d_1 * d_1 * d_1);

    /* starting approximation for p tending to 1 */
    if (ch > c6 * *v + six)
      ch = -two * (log(one - *p) - c * log(half * ch) + *g);
  }
  else{
    /* starting approximation for v less than or equal to 0.32 */
    ch = c4;
    a = log(one - *p);
    do {
      q = ch;
      p1 = one + ch * (c7 + ch);
      p2 = ch * (c9 + ch * (c8 + ch));
      d_1 = -half + (c7 + two * ch) / p1;
      d_2 = (c9 + ch * (c10 + three * ch)) / p2;
      t = d_1 - d_2;
      ch -= (one - exp(a + *g + half * ch + c * aa) * p2 / p1) / t;
    } while (fabs(q / ch - one) > c1);
  }

  do {
    /* call to gammad_ and calculation of seven term Taylor series */
    q = ch;
    p1 = half * ch;
    p2 = *p - gammad_(&p1, &xx, &if1);
    if (if1 != 0) {
      *ifault = 3;
      return ret_val;
    }
    t = p2 * exp(xx * aa + *g + p1 - c * log(ch));
    b = t / ch;
    a = half * t - b * c;
    s1 = (c19 + a * (c17 + a * (c14 + a * (c13 + a * (c12 + c11 * a))))) / c24;
    s2 = (c24 + a * (c29 + a * (c32 + a * (c33 + c35 * a)))) / c37;
    s3 = (c19 + a * (c25 + a * (c28 + c31 * a))) / c37;
    s4 = (c20 + a * (c27 + c34 * a) + c * (c22 + a * (c30 + c36 * a))) / c38;
    s5 = (c13 + c21 * a + c * (c18 + c26 * a)) / c37;
    s6 = (c15 + c * (c23 + c16 * c)) / c38;
    d_1 = (s3 - b * (s4 - b * (s5 - b * s6)));
    d_1 = (s1 - b * (s2 - b * d_1));
    ch += t * (one + half * t * s1 - b * c * d_1);
  } while (fabs(q / ch - one) > e);

  ret_val = ch;
  return ret_val;
}

double ppgamma P3C(double, p, double, a, int *, ifault)
{
  double x, v, g;

  if (p == 0.0)
    return 0.0;
  else {
    v = 2.0 * a;
    g = gamma(a);
    x = ppchi2(&p, &v, &g, ifault);
    return x / 2.0;
  }
}
