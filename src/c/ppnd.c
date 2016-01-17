/* ppnd.f -- translated by f2c and modified */

#include "xlisp.h"
#include "xlstat.h"

#ifdef abs
#undef abs
#endif
#define abs(x) ((x) >= 0 ? (x) : -(x))

double ppnd P2C(double, p, int *, ifault)
{
  /* System generated locals */
  double ret_val;

  /* Local variables */
  double q, r;

  /* ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3 */

  /* Produces the normal deviate Z corresponding to a given lower */
  /* tail area of P; Z is accurate to about 1 part in 10**16. */

  /* The hash sums below are the sums of the mantissas of the */
  /* coefficients.   They are included for use in checking */
  /* transcription. */

  /* Coefficients for P close to 0.5 */

  /* HASH SUM AB    55.88319 28806 14901 4439 */

  /* Coefficients for P not close to 0, 0.5 or 1. */

  /* HASH SUM CD    49.33206 50330 16102 89036 */

  /* Coefficients for P near 0 or 1. */

  /* HASH SUM EF    47.52583 31754 92896 71629 */

  *ifault = 0;
  q = p - .5;
  if (abs(q) <= .425) {
    r = .180625 - q * q;
    ret_val = q * (((((((r * 2509.0809287301226727 +
			 33430.575583588128105) * r +
			67265.770927008700853) * r + 
		       45921.953931549871457) * r +
		      13731.693765509461125) * r + 
		     1971.5909503065514427) * r +
		    133.14166789178437745) * r + 
		   3.387132872796366608) /
		     (((((((r * 5226.495278852854561 + 
			    28729.085735721942674) * r +
			   39307.89580009271061) * r + 
			  21213.794301586595867) * r +
			 5394.1960214247511077) * r + 
			687.1870074920579083) * r +
		       42.313330701600911252) * r + 1.);
    return ret_val;
  } else {
    if (q < 0.) {
      r = p;
    } else {
      r = 1. - p;
    }
    if (r <= 0.) {
      *ifault = 1;
      ret_val = 0.;
      return ret_val;
    }
    r = sqrt(-log(r));
    if (r <= 5.) {
      r += -1.6;
      ret_val = (((((((r * 7.7454501427834140764e-4 + 
		       .0227238449892691845833) * r +
		      .24178072517745061177) * r 
		     + 1.27045825245236838258) * r +
		    3.64784832476320460504) * 
		   r + 5.7694972214606914055) * r +
		  4.6303378461565452959) * 
		 r + 1.42343711074968357734) /
		   (((((((r * 1.05075007164441684324e-9 +
			  5.475938084995344946e-4) * r 
			 + .0151986665636164571966) * r +
			.14810397642748007459) * 
		       r + .68976733498510000455) * r +
		      1.6763848301838038494) * r +
		     2.05319162663775882187) * r + 1.);
    } else {
      r += -5.;
      ret_val = (((((((r * 2.01033439929228813265e-7 + 
		       2.71155556874348757815e-5) * r +
		      .0012426609473880784386) 
		     * r + .026532189526576123093) * r +
		    .29656057182850489123) * r +
		   1.7848265399172913358) * r + 5.4637849111641143699)
		 * r + 6.6579046435011037772) /
		   (((((((r * 2.04426310338993978564e-15 +
			  1.4215117583164458887e-7) * 
			 r + 1.8463183175100546818e-5) * r + 
			7.868691311456132591e-4) * r +
		       .0148753612908506148525) * r +
		      .13692988092273580531) * r + .59983220655588793769) * 
		    r + 1.);
    }
    if (q < 0.) {
      ret_val = -ret_val;
    }
    return ret_val;
  }
}
