#include "xlisp.h"
#include "xlstat.h"

#ifdef abs
#undef abs
#endif
#define abs(x) ((x) >= 0 ? (x) : -(x))
#ifdef min
#undef min
#endif
#define min(a,b) ((a) <= (b) ? (a) : (b))
#ifdef max
#undef max
#endif
#define max(a,b) ((a) >= (b) ? (a) : (b))

double POW2 P1H(double);
double ABS P1H(double);

VOID d_cnjg P2H(dcomplex *, dcomplex *);
double dcabs1 P1H(dcomplex *);
double d_int P1H(double *);

/* BLAS routines */
extern double blas_dasum P3H(int, double *, int);
extern VOID blas_daxpy P6H(int, double, double *, int, double *, int);
extern VOID blas_dcopy P5H(int, double *, int, double *, int);
extern double blas_ddot P5H(int, double *, int, double *, int);
extern double blas_dnrm2 P3H(int, double *, int);
extern VOID blas_drot P7H(int, double *, int, double *, int, double, double);
extern VOID blas_drotg P4H(double *, double *, double *, double *);
extern VOID blas_dscal P4H(int, double, double *, int);
extern VOID blas_dswap P5H(int, double *, int, double *, int);
extern int blas_idamax P3H(int, double *, int);
extern double blas_dzasum P3H(int, dcomplex *, int);
extern double blas_dznrm2 P3H(int, dcomplex *, int);
extern int blas_izamax P3H(int, dcomplex *, int);
extern VOID blas_zaxpy P6H(int, dcomplex *, dcomplex *, int, dcomplex *, int);
extern VOID blas_zcopy P5H(int, dcomplex *, int, dcomplex *, int);
extern VOID blas_zdotc P6H(dcomplex *, int, dcomplex *, int, dcomplex *, int);
extern VOID blas_zdotu P6H(dcomplex *, int, dcomplex *, int, dcomplex *, int);
extern VOID blas_zdrot P7H(int, dcomplex *, int, dcomplex *, int,
			   double, double);
extern VOID blas_zdscal P4H(int, double, dcomplex *, int);
extern VOID blas_zrotg P4H(dcomplex *, dcomplex *, double *, dcomplex *);
extern VOID blas_zscal P4H(int, dcomplex *, dcomplex *, int);
extern VOID blas_zswap P5H(int, dcomplex *, int, dcomplex *, int);
VOID blas_dgemv P11H(char *, int, int, double, double *, int,
		     double *, int, double, double *, int);
VOID blas_dtrmv P8H(char *, char *, char *, int, double *, int, double *, int);
VOID blas_dger P9H(int, int, double, double *, int, double *, int,
		   double *, int);
VOID blas_dtrsv P8H(char *, char *, char *, int, double *, int, double *, int);
VOID blas_zgemv P11H(char *, int, int, dcomplex *, dcomplex *, int,
		     dcomplex *, int, dcomplex *, dcomplex *, int);
VOID blas_ztrmv P8H(char *, char *, char *, int, dcomplex *, int,
		    dcomplex *, int);
VOID blas_zgerc P9H(int, int, dcomplex *, dcomplex *, int, dcomplex *, int,
		    dcomplex *, int);
VOID blas_zgeru P9H(int, int, dcomplex *, dcomplex *, int, dcomplex *, int,
		    dcomplex *, int);
VOID blas_ztrsv P8H(char *, char *, char *, int, dcomplex *, int,
		    dcomplex *, int);
VOID blas_dgemm P13H(char *, char *, int, int, int, double,
		     double *, int, double *, int, double, double *, int);
VOID blas_dtrsm P11H(char *, char *, char *, char *, int, int, double,
		     double *, int, double *, int);
VOID blas_zgemm P13H(char *, char *, int, int, int, dcomplex *,
		     dcomplex *, int, dcomplex *, int, dcomplex *,
		     dcomplex *, int);
VOID blas_ztrsm P11H(char *, char *, char *, char *, int, int, 
		     dcomplex *, dcomplex *, int, dcomplex *, int);


/* LINPACK routines */
VOID linpack_dgeco P6H(double *, int, int, int *, double *, double *);
VOID linpack_dgedi P7H(double *, int, int, int *, double *, double *, int);
VOID linpack_dgefa P5H(double *, int, int, int *, int *);
VOID linpack_dgesl P6H(double *, int, int, int *, double *, int);
VOID linpack_dqrdc P8H(double *, int, int, int, double *,
		       int *, double *, int);
VOID linpack_dqrsl P13H(double *, int, int, int, double *, double *,
			double *, double *, double *, double *,	double *,
			int, int *);
VOID linpack_dsvdc P13H(double *, int, int, int, double *, double *,
			double *, int, double *, int, double *,
			int, int *);
VOID linpack_zgeco P6H(dcomplex *, int, int, int *, double *, dcomplex *);
VOID linpack_zgedi P7H(dcomplex *, int, int, int *, dcomplex *, dcomplex *,
		       int);
VOID linpack_zgefa P5H(dcomplex *, int, int, int *, int *);
VOID linpack_zgesl P6H(dcomplex *, int, int, int *, dcomplex *, int);
VOID linpack_zqrdc P8H(dcomplex *, int, int, int, dcomplex *,
		       int *, dcomplex *, int);
VOID linpack_zqrsl P13H(dcomplex *, int, int, int, dcomplex *, dcomplex *,
			dcomplex *, dcomplex *, dcomplex *, dcomplex *,
			dcomplex *, int, int *);
VOID linpack_zsvdc P13H(dcomplex *, int, int, int, dcomplex *, dcomplex *,
			dcomplex *, int, dcomplex *, int, dcomplex *,
			int, int *);

/* EISPACK routines */
VOID eispack_ch P12H(int, int, double *, double *, double *, int, double *,
		     double *, double *, double *, double *, int *);

VOID eispack_rs P9H(int, int, double *, double *, int,
		    double *, double *, double *, int *);

/* in place transpose */
int gtrans P8H(char *, int, int, int, int *, int, int, char *);
