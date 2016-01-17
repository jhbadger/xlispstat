/* Double precision version of the routine in ALGAMA from the netlib */
/* SPECFUN library. Translated by f2c and modified. */

#include "xlisp.h"
#include "xlstat.h"

double gamma P1C(double, x)
{
  /* Local variables */
  double xden, corr, xnum;
  int i;
  double y, xm1, xm2, xm4, res, ysq;

  /* ---------------------------------------------------------------------- */

  /* This routine calculates the LOG(GAMMA) function for a positive real */
  /*   argument X.  Computation is based on an algorithm outlined in */
  /*   references 1 and 2.  The program uses rational functions that */
  /*   theoretically approximate LOG(GAMMA) to at least 18 significant */
  /*   decimal digits.  The approximation for X > 12 is from reference */
  /*   3, while approximations for X < 12.0 are similar to those in */
  /*   reference 1, but are unpublished.  The accuracy achieved depends */
  /*   on the arithmetic system, the compiler, the intrinsic functions, */
  /*   and proper selection of the machine-dependent constants. */


  /* ********************************************************************* */
  /* ********************************************************************* */

  /* Explanation of machine-dependent constants */

  /* beta   - radix for the floating-point representation */
  /* maxexp - the smallest positive power of beta that overflows */
  /* XBIG   - largest argument for which LN(GAMMA(X)) is representable */
  /*          in the machine, i.e., the solution to the equation */
  /*                  LN(GAMMA(XBIG)) = beta**maxexp */
  /* XINF   - largest machine representable floating-point number; */
  /*          approximately beta**maxexp. */
  /* EPS    - The smallest positive floating-point number such that */
  /*          1.0+EPS .GT. 1.0 */
  /* FRTBIG - Rough estimate of the fourth root of XBIG */


  /*     Approximate values for some important machines are: */

  /*                           beta      maxexp         XBIG */

  /* CRAY-1        (S.P.)        2        8191       9.62E+2461 */
  /* Cyber 180/855 */
  /*   under NOS   (S.P.)        2        1070       1.72E+319 */
  /* IEEE (IBM/XT, */
  /*   SUN, etc.)  (S.P.)        2         128       4.08E+36 */
  /* IEEE (IBM/XT, */
  /*   SUN, etc.)  (D.P.)        2        1024       2.55D+305 */
  /* IBM 3033      (D.P.)       16          63       4.29D+73 */
  /* VAX D-Format  (D.P.)        2         127       2.05D+36 */
  /* VAX G-Format  (D.P.)        2        1023       1.28D+305 */


  /*                           XINF        EPS        FRTBIG */

  /* CRAY-1        (S.P.)   5.45E+2465   7.11E-15    3.13E+615 */
  /* Cyber 180/855 */
  /*   under NOS   (S.P.)   1.26E+322    3.55E-15    6.44E+79 */
  /* IEEE (IBM/XT, */
  /*   SUN, etc.)  (S.P.)   3.40E+38     1.19E-7     1.42E+9 */
  /* IEEE (IBM/XT, */
  /*   SUN, etc.)  (D.P.)   1.79D+308    2.22D-16    2.25D+76 */
  /* IBM 3033      (D.P.)   7.23D+75     2.22D-16    2.56D+18 */
  /* VAX D-Format  (D.P.)   1.70D+38     1.39D-17    1.20D+9 */
  /* VAX G-Format  (D.P.)   8.98D+307    1.11D-16    1.89D+76 */

  /* ************************************************************** */
  /* ************************************************************** */

  /* Error returns */

  /*  The program returns the value XINF for X .LE. 0.0 or when */
  /*     overflow would occur.  The computation is believed to */
  /*     be free of underflow and overflow. */


  /* Intrinsic functions required are: */

  /*      LOG */


  /* References: */

  /*  1) W. J. Cody and K. E. Hillstrom, 'Chebyshev Approximations for */
  /*     the Natural Logarithm of the Gamma Function,' Math. Comp. 21, */
  /*     1967, pp. 198-203. */

  /*  2) K. E. Hillstrom, ANL/AMD Program ANLC366S, DGAMMA/DLGAMA, May, */
  /*     1969. */

  /*  3) Hart, Et. Al., Computer Approximations, Wiley and sons, New */
  /*     York, 1968. */


  /*  Authors: W. J. Cody and L. Stoltz */
  /*           Argonne National Laboratory */

  /*  Latest modification: June 16, 1988 */

  /* ---------------------------------------------------------------------- */
  /* ---------------------------------------------------------------------- */
  /*  Mathematical constants */
  /* ---------------------------------------------------------------------- */
  static double one = 1.;
  static double half = .5;
  static double twelve = 12.;
  static double zero = 0.;
  static double four = 4.;
  static double thrhal = 1.5;
  static double two = 2.;
  static double pnt68 = .6796875;
  static double sqrtpi = .9189385332046727417803297;

  /* ---------------------------------------------------------------------- */
  /*  Machine dependent parameters */
  /* ---------------------------------------------------------------------- */
#ifdef IEEEFP
  static double xbig = 2.55e305;
  static double xinf = 1.79e308;
  static double eps = 2.22e-16;
  static double frtbig = 2.25e76;
#else
#ifdef CRAYCC
  static double xbig = 9.62e+2461;
  static double xinf = 5.45e+2465;
  static double eps = 7.11e-15;
  static double frtbig = 3.13e+615;
#else /* use IBM 3033 values */
  static double xbig = 4.29e+73;
  static double xinf = 7.23e+75;
  static double eps = 2.22e-16;
  static double frtbig = 2.56e+18;
#endif /* CRAYCC */
#endif /* IEEEFP */

  /* ---------------------------------------------------------------------- */
  /*  Numerator and denominator coefficients for rational minimax */
  /*     approximation over (0.5,1.5). */
  /* ---------------------------------------------------------------------- */
  static double d1 = -.5772156649015328605195174;
  static double p1[8] = { 4.945235359296727046734888,
			    201.8112620856775083915565,
			    2290.838373831346393026739,
			    11319.67205903380828685045,
			    28557.24635671635335736389,
			    38484.96228443793359990269,
			    26377.48787624195437963534,
			    7225.813979700288197698961 };
  static double q1[8] = { 67.48212550303777196073036,
			    1113.332393857199323513008,
			    7738.757056935398733233834,
			    27639.87074403340708898585,
			    54993.10206226157329794414,
			    61611.22180066002127833352,
			    36351.27591501940507276287,
			    8785.536302431013170870835 };

  /* ---------------------------------------------------------------------- */
  /*  Numerator and denominator coefficients for rational minimax */
  /*     Approximation over (1.5,4.0). */
  /* ---------------------------------------------------------------------- */
  static double d2 = .4227843350984671393993777;
  static double p2[8] = { 4.974607845568932035012064,
			    542.4138599891070494101986,
			    15506.93864978364947665077,
			    184793.2904445632425417223,
			    1088204.76946882876749847,
			    3338152.967987029735917223,
			    5106661.678927352456275255,
			    3074109.054850539556250927 };
  static double q2[8] = { 183.0328399370592604055942,
			    7765.049321445005871323047,
			    133190.3827966074194402448,
			    1136705.821321969608938755,
			    5267964.117437946917577538,
			    13467014.54311101692290052,
			    17827365.30353274213975932,
			    9533095.591844353613395747 };

  /* ---------------------------------------------------------------------- */
  /*  Numerator and denominator coefficients for rational minimax */
  /*     Approximation over (4.0,12.0). */
  /* ---------------------------------------------------------------------- */
  static double d4 = 1.791759469228055000094023;
  static double p4[8] = { 14745.02166059939948905062,
			    2426813.369486704502836312,
			    121475557.4045093227939592,
			    2663432449.630976949898078,
			    29403789566.34553899906876,
			    170266573776.5398868392998,
			    492612579337.743088758812,
			    560625185622.3951465078242 };
  static double q4[8] = { 2690.530175870899333379843,
			    639388.5654300092398984238,
			    41355999.30241388052042842,
			    1120872109.61614794137657,
			    14886137286.78813811542398,
			    101680358627.2438228077304,
			    341747634550.7377132798597,
			    446315818741.9713286462081 };

  /* ---------------------------------------------------------------------- */
  /*  Coefficients for minimax approximation over (12, INF). */
  /* ---------------------------------------------------------------------- */
  static double c[7] = { -.001910444077728,
			   8.4171387781295e-4,
			   -5.952379913043012e-4,
			   7.93650793500350248e-4,
			   -.002777777777777681622553,
			   .08333333333333333331554247,
			   .0057083835261 };

  /* ---------------------------------------------------------------------- */
  y = x;
  if (y > zero && y <= xbig) {
    if (y <= eps) {
      res = -log(y);
    } else if (y <= thrhal) {
      /* ------------------------------------------------------------------- */
      /*  EPS .LT. X .LE. 1.5 */
      /* ------------------------------------------------------------------- */
      if (y < pnt68) {
	corr = -log(y);
	xm1 = y;
      } else {
	corr = zero;
	xm1 = y - half - half;
      }
      if (y <= half || y >= pnt68) {
	xden = one;
	xnum = zero;
	for (i = 1; i <= 8; ++i) {
	  xnum = xnum * xm1 + p1[i - 1];
	  xden = xden * xm1 + q1[i - 1];
	}
	res = corr + xm1 * (d1 + xm1 * (xnum / xden));
      } else {
	xm2 = y - half - half;
	xden = one;
	xnum = zero;
	for (i = 1; i <= 8; ++i) {
	  xnum = xnum * xm2 + p2[i - 1];
	  xden = xden * xm2 + q2[i - 1];
	}
	res = corr + xm2 * (d2 + xm2 * (xnum / xden));
      }
    } else if (y <= four) {
      /* ------------------------------------------------------------------- */
      /*  1.5 .LT. X .LE. 4.0 */
      /* ------------------------------------------------------------------- */
      xm2 = y - two;
      xden = one;
      xnum = zero;
      for (i = 1; i <= 8; ++i) {
	xnum = xnum * xm2 + p2[i - 1];
	xden = xden * xm2 + q2[i - 1];
      }
      res = xm2 * (d2 + xm2 * (xnum / xden));
    } else if (y <= twelve) {
      /* ------------------------------------------------------------------- */
      /*  4.0 .LT. X .LE. 12.0 */
      /* ------------------------------------------------------------------- */
      xm4 = y - four;
      xden = -one;
      xnum = zero;
      for (i = 1; i <= 8; ++i) {
	xnum = xnum * xm4 + p4[i - 1];
	xden = xden * xm4 + q4[i - 1];
      }
      res = d4 + xm4 * (xnum / xden);
    } else {
      /* ------------------------------------------------------------------- */
      /*  Evaluate for argument .GE. 12.0, */
      /* ------------------------------------------------------------------- */
      res = zero;
      if (y <= frtbig) {
	res = c[6];
	ysq = y * y;
	for (i = 1; i <= 6; ++i) {
	  res = res / ysq + c[i - 1];
	}
      }
      res /= y;
      corr = log(y);
      res = res + sqrtpi - half * corr;
      res += y * (corr - one);
    }
  } else {
    /* -------------------------------------------------------------------- */
    /*  Return for bad arguments */
    /* -------------------------------------------------------------------- */
    res = xinf;
  }
  /* ---------------------------------------------------------------------- */
  /*  Final adjustments and return */
  /* ---------------------------------------------------------------------- */
  return res;
  /* ---------- Last line of DLGAMA ---------- */
}
