/* linalg - Lisp interface for basic linear algebra routines           */
/* XLISP-STAT 2.1 Copyright (c) 1990-1995, by Luke Tierney             */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

#define checktvecsize(x,n) if ((n) > gettvecsize(x)) xlbadtype(x)
#define checknonneg(n) if ((n) < 0) xlbadtype(cvfixnum((FIXTYPE) (n)))
#define checksquarematrix(x) \
  if (! matrixp(x) || numrows(x) != numcols(x)) xlbadtype(x);

#define CXDAT(x) ((dcomplex *) gettvecdata(x))
#define REDAT(x) ((double *) gettvecdata(x))
#define INDAT(x) ((int *) gettvecdata(x))

#define IN 0
#define RE 1
#define CX 2


/************************************************************************/
/**                                                                    **/
/**            Argument Checking and Conversion Functions              **/
/**                                                                    **/
/************************************************************************/

int anycomplex P1C(LVAL, x)
{
  LVAL data;

  data = compounddataseq(x);

  switch (ntype(data)) {
  case CONS:
    for (; consp(data); data = cdr(data))
      if (complexp(car(data)))
	return TRUE;
    return FALSE;
  case VECTOR:
    {
      int i, n;
      n = getsize(data);
      for (i = 0; i < n; i++)
	if (complexp(getelement(data, i)))
	  return TRUE;
      return FALSE;
    }
  case TVEC:
    switch (gettvectype(data)) {
    case CD_CXFIXTYPE:
    case CD_CXFLOTYPE:
    case CD_COMPLEX:
    case CD_DCOMPLEX:
      return TRUE;
    default:
      return FALSE;
    }
  default:
    return FALSE;
  }
}

LVAL xsanycomplex(V)
{
  while (moreargs())
    if (anycomplex(xlgetarg()))
      return s_true;
  return NIL;
}

LOCAL LVAL newmatrix P2C(int, m, int, n)
{
  LVAL dim, result;

  xlsave1(dim);
  checknonneg(m);
  checknonneg(n);
  dim = integer_list_2(m, n);
  result = mkarray(dim, NIL, NIL, s_true);
  xlpop();

  return result;
}

LOCAL LVAL getlinalgdata P4C(int, off, int, n, LVAL, arg, int, type)
{
  LVAL x;

  x = darrayp(arg) ? getdarraydata(arg) : arg;
  if (! tvecp(x))
    xlbadtype(arg);
  if (off < 0 || n < 0 || gettvecsize(x) < off + n)
    xlerror("incompatible with access indices", x);
  switch (type) {
  case IN:
    if (gettvectype(x) != CD_INT)
      xlbadtype(x);
    break;
  case RE:
    switch(gettvectype(x)) {
    case CD_FLOTYPE:
    case CD_DOUBLE:
      break;
    default:
      xlbadtype(x);
    }
    break;
  case CX:
    switch(gettvectype(x)) {
    case CD_CXFLOTYPE:
    case CD_DCOMPLEX:
      break;
    default:
      xlbadtype(x);
    }
    break;
  }
  return x;
}

#define getlinalgivec(off,n,arg) (INDAT(getlinalgdata(off,n,arg,IN)) + (off))
#define getlinalgdvec(off,n,arg) (REDAT(getlinalgdata(off,n,arg,RE)) + (off))
#define getlinalgzvec(off,n,arg) (CXDAT(getlinalgdata(off,n,arg,CX)) + (off))

LOCAL VOID transposeinto P4C(LVAL, x, int, m, int, n, LVAL, y)
{
  int i, j, in, jm;
  
  x = compounddataseq(x);
  y = compounddataseq(y);
  if (! vectorp(x) && ! tvecp(x) && ! stringp(x)) xlbadtype(x);
  if (! vectorp(y) && ! tvecp(y) && ! stringp(y)) xlbadtype(y);
  checknonneg(n);
  checknonneg(m);
  checktvecsize(x, n * m);
  checktvecsize(y, n * m);

  for (i = 0, in = 0; i < m; i++, in += n)
    for (j = 0, jm = 0; j < n; j++, jm += m)
      settvecelement(y, jm + i, gettvecelement(x, in + j));
}

LVAL xstransposeinto(V)
{
  LVAL x, y;
  int m, n;

  x = xlgetarg();
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  y = xlgetarg();
  xllastarg();

  transposeinto(x, m, n, y);

  return y;
}

LOCAL LVAL gen2linalg P5C(LVAL, arg, int, m, int, n, LVAL, type, int , trans)
{
  LVAL x, y;
  int mn;

  x = compounddataseq(arg);
  mn = n * m;

  xlsave1(y);
  y = mktvec(mn, type);
  if (trans)
    transposeinto(x, m, n, y);
  else
    xlreplace(y, x, 0, mn, 0, mn);
  xlpop();
  return y;
}

LOCAL LVAL linalg2genvec P2C(LVAL, x, int, n)
{
  LVAL y;
  
  if (! tvecp(x)) xlbadtype(x);
  if (n <= 0 || gettvecsize(x) < n) xlfail("bad dimensions");

  xlsave1(y);
  y = newvector(n);
  xlreplace(y, x, 0, n, 0, n);
  xlpop();
  return y;
}

LOCAL LVAL linalg2genmat P4C(LVAL, arg, int, m, int, n, int, trans)
{
  LVAL x, y;
  int mn;

  x = compounddataseq(arg);
  mn = m * n;
  if (! tvecp(x)) xlbadtype(arg);
  if (n <= 0 || m <= 0 || gettvecsize(x) < mn) xlfail("bad dimensions");

  xlsave1(y);
  y = newmatrix(m, n);
  if (trans)
    transposeinto(x, n, m, y);
  else
    xlreplace(getdarraydata(y), x, 0, mn, 0, mn);
  xlpop();
  return y;
}

LVAL xsgen2linalg(V)
{
  LVAL x, type;
  int m, n, trans;

  x = xlgetarg();
  m = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  type = xlgetarg();
  trans = moreargs() ? ! null(xlgetarg()) : FALSE;
  xllastarg();

  return gen2linalg(x, m, n, type, trans);
}

LVAL xslinalg2gen(V)
{
  LVAL x, d;
  int trans;

  x = xlgetarg();
  d = xlgetarg();
  trans = moreargs() ? ! null(xlgetarg()) : FALSE;
  xllastarg();

  if (fixp(d))
    return linalg2genvec(x, getfixnum(d));
  else if (consp(d) && consp(cdr(d)) && fixp(car(d)) && fixp(car(cdr(d))))
    return linalg2genmat(x, getfixnum(car(d)), getfixnum(car(cdr(d))), trans);
  else
    xlbadtype(d);
  return NIL;
}    

LOCAL VOID checkldim P2C(int, lda, int, n)
{
  if (lda < 0 || n < 0 || n < lda)
    xlfail("bad dimensions");
}


/****************************************************************************/
/*                                                                          */
/*                         LU Decomposition Routines                        */
/*                                                                          */
/****************************************************************************/

LVAL xslpdgeco(V)
{
  LVAL a, ipvt, z;
  double *da, rcond, *dz;
  int lda, offa, n, *dipvt;

  a = xlgetarg();
  offa = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ipvt = xlgetarg();
  z = xlgetarg();
  xllastarg();

  checkldim(lda, n);
  da = getlinalgdvec(offa, lda * n, a);
  dipvt = getlinalgivec(0, n, ipvt);
  dz = getlinalgdvec(0, n, z);

  linpack_dgeco(da, lda, n, dipvt, &rcond, dz);

  return cvflonum((FLOTYPE) rcond);
}

LVAL xslpdgedi(V)
{
  LVAL a, ipvt, det, work;
  double *da, *ddet, *dwork;
  int lda, offa, n, *dipvt, job, i, ilda;

  a = xlgetarg();
  offa = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ipvt = xlgetarg();
  det = xlgetarg();
  work = xlgetarg();
  job = getfixnum(xlgafixnum());
  xllastarg();

  checkldim(lda, n);
  da = getlinalgdvec(offa, lda * n, a);
  dipvt = getlinalgivec(0, n, ipvt);
  ddet = (job / 10 != 0) ? getlinalgdvec(0, 2, det) : NULL;
  dwork = getlinalgdvec(0, n, work);

  if (job % 10 != 0)
    for (i = 0, ilda = 0; i < n; i++, ilda += lda)
      if (da[ilda + i] == 0.0)
	xlfail("matrix is (numerically) singular");

  linpack_dgedi(da, lda, n, dipvt, ddet, dwork, job);

  return NIL;
}

LVAL xslpdgefa(V)
{
  LVAL a, ipvt;
  double *da;
  int lda, offa, n, *dipvt, info;

  a = xlgetarg();
  offa = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ipvt = xlgetarg();
  xllastarg();

  checkldim(lda, n);
  da = getlinalgdvec(offa, lda * n, a);
  dipvt = getlinalgivec(0, n, ipvt);

  linpack_dgefa(da, lda, n, dipvt, &info);

  return cvfixnum((FIXTYPE) info);
}

LVAL xslpdgesl(V)
{
  LVAL a, ipvt, b;
  double *da, *db;
  int lda, offa, n, *dipvt, job, i, ilda;

  a = xlgetarg();
  offa = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ipvt = xlgetarg();
  b = xlgetarg();
  job = getfixnum(xlgafixnum());
  xllastarg();

  checkldim(lda, n);
  da = getlinalgdvec(offa, lda * n, a);
  dipvt = getlinalgivec(0, n, ipvt);
  db = getlinalgdvec(0, n, b);

  for (i = 0, ilda = 0; i < n; i++, ilda += lda)
    if (da[ilda + i] == 0.0)
      xlfail("matrix is (numerically) singular");

  linpack_dgesl(da, lda, n, dipvt, db, job);

  return NIL;
}

LVAL xslpzgeco(V)
{
  LVAL a, ipvt, z;
  dcomplex *da, *dz;
  double rcond;
  int lda, offa, n, *dipvt;

  a = xlgetarg();
  offa = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ipvt = xlgetarg();
  z = xlgetarg();
  xllastarg();

  checkldim(lda, n);
  da = getlinalgzvec(offa, lda * n, a);
  dipvt = getlinalgivec(0, n, ipvt);
  dz = getlinalgzvec(0, n, z);

  linpack_zgeco(da, lda, n, dipvt, &rcond, dz);

  return cvflonum((FLOTYPE) rcond);
}

LVAL xslpzgedi(V)
{
  LVAL a, ipvt, det, work;
  dcomplex *da, *ddet, *dwork;
  int lda, offa, n, *dipvt, job, i, ilda;

  a = xlgetarg();
  offa = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ipvt = xlgetarg();
  det = xlgetarg();
  work = xlgetarg();
  job = getfixnum(xlgafixnum());
  xllastarg();

  checkldim(lda, n);
  da = getlinalgzvec(offa, lda * n, a);
  dipvt = getlinalgivec(0, n, ipvt);
  ddet = (job / 10 != 0) ? getlinalgzvec(0, 2, det) : NULL;
  dwork = getlinalgzvec(0, n, work);

  if (job % 10 != 0)
    for (i = 0, ilda = 0; i < n; i++, ilda += lda)
      if (da[ilda + i].r == 0.0 && da[ilda + i].i == 0.0)
	xlfail("matrix is (numerically) singular");

  linpack_zgedi(da, lda, n, dipvt, ddet, dwork, job);

  return NIL;
}

LVAL xslpzgefa(V)
{
  LVAL a, ipvt;
  dcomplex *da;
  int lda, offa, n, *dipvt, info;

  a = xlgetarg();
  offa = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ipvt = xlgetarg();
  xllastarg();

  checkldim(lda, n);
  da = getlinalgzvec(offa, lda * n, a);
  dipvt = getlinalgivec(0, n, ipvt);

  linpack_zgefa(da, lda, n, dipvt, &info);

  return cvfixnum((FIXTYPE) info);
}

LVAL xslpzgesl(V)
{
  LVAL a, ipvt, b;
  dcomplex *da, *db;
  int lda, offa, n, *dipvt, job, i, ilda;

  a = xlgetarg();
  offa = getfixnum(xlgafixnum());
  lda = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ipvt = xlgetarg();
  b = xlgetarg();
  job = getfixnum(xlgafixnum());
  xllastarg();

  checkldim(lda, n);
  da = getlinalgzvec(offa, lda * n, a);
  dipvt = getlinalgivec(0, n, ipvt);
  db = getlinalgzvec(0, n, b);

  for (i = 0, ilda = 0; i < n; i++, ilda += lda)
    if (da[ilda + i].r == 0.0 && da[ilda + i].i == 0.0)
      xlfail("matrix is (numerically) singular");

  linpack_zgesl(da, lda, n, dipvt, db, job);

  return NIL;
}

/****************************************************************************/
/*                                                                          */
/*                         SV Decomposition Routines                        */
/*                                                                          */
/****************************************************************************/

LVAL xslpdsvdc(V)
{
  LVAL x, s, e, u, v, work;
  int n, p, job, info, jobu, jobv, ncu, offx, ldx, offu, ldu, offv, ldv;
  double *dx, *ds, *de, *du, *dv, *dwork;

  x = xlgetarg();
  offx = getfixnum(xlgafixnum());
  ldx = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  p = getfixnum(xlgafixnum());
  s = xlgetarg();
  e = xlgetarg();
  u = xlgetarg();
  offu = getfixnum(xlgafixnum());
  ldu = getfixnum(xlgafixnum());
  v = xlgetarg();
  offv = getfixnum(xlgafixnum());
  ldv = getfixnum(xlgafixnum());
  work = xlgetarg();
  job = getfixnum(xlgafixnum());
  xllastarg();

  jobu = job % 100 / 10;
  ncu = jobu > 1 ? (n < p ? n : p) : n;
  jobv = job % 10;

  checkldim(ldx, n);
  if (jobu) checkldim(ldu, n);
  if (jobv) checkldim(ldv, p);

  dx = getlinalgdvec(offx, ldx * p, x);
  ds = getlinalgdvec(0, n + 1 < p ? n + 1 : p, s);
  de = getlinalgdvec(0, p, e);
  du = jobu > 0 ? getlinalgdvec(offu, ldu * ncu, u) : NULL;
  dv = jobv > 0 ? getlinalgdvec(offv, ldv * p, v) : NULL;
  dwork = getlinalgdvec(0, n, work);
  
  linpack_dsvdc(dx, ldx, n, p, ds, de, du, ldu, dv, ldv, dwork, job, &info);
  return info ? cvfixnum((FIXTYPE) (info - 1)) : NIL;
}

LVAL xslpzsvdc(V)
{
  LVAL x, s, e, u, v, work;
  int n, p, job, info, jobu, jobv, ncu, offx, ldx, offu, ldu, offv, ldv;
  dcomplex *dx, *ds, *de, *du, *dv, *dwork;

  x = xlgetarg();
  offx = getfixnum(xlgafixnum());
  ldx = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  p = getfixnum(xlgafixnum());
  s = xlgetarg();
  e = xlgetarg();
  u = xlgetarg();
  offu = getfixnum(xlgafixnum());
  ldu = getfixnum(xlgafixnum());
  v = xlgetarg();
  offv = getfixnum(xlgafixnum());
  ldv = getfixnum(xlgafixnum());
  work = xlgetarg();
  job = getfixnum(xlgafixnum());
  xllastarg();

  jobu = job % 100 / 10;
  ncu = jobu > 1 ? (n < p ? n : p) : n;
  jobv = job % 10;

  checkldim(ldx, n);
  if (jobu) checkldim(ldu, n);
  if (jobv) checkldim(ldv, p);

  dx = getlinalgzvec(offx, ldx * p, x);
  ds = getlinalgzvec(0, n + 1 < p ? n + 1 : p, s);
  de = getlinalgzvec(0, p, e);
  du = jobu > 0 ? getlinalgzvec(offu, ldu * ncu, u) : NULL;
  dv = jobv > 0 ? getlinalgzvec(offv, ldv * p, v) : NULL;
  dwork = getlinalgzvec(0, n, work);
  
  linpack_zsvdc(dx, ldx, n, p, ds, de, du, ldu, dv, ldv, dwork, job, &info);
  return info ? cvfixnum((FIXTYPE) (info - 1)) : NIL;
}


/****************************************************************************/
/*                                                                          */
/*                         QR Decomposition Routines                        */
/*                                                                          */
/****************************************************************************/

LVAL xslpdqrdc(V)
{
  LVAL x, a, j, w, r, q;
  double *dx, *da, *dw, *dr, *dq;
  int offx, ldx, n, p, *dj, job;

  x = xlgetarg();
  offx = getfixnum(xlgafixnum());
  ldx = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  p = getfixnum(xlgafixnum());
  a = xlgetarg();
  j = xlgetarg();
  w = xlgetarg();
  job = getfixnum(xlgafixnum());
  r = moreargs() ? xlgetarg() : NIL;
  q = moreargs() ? xlgetarg() : NIL;
  xllastarg();

  if (p > n && (! null(r) || ! null(q)))
    xlfail("more columns than rows");
  checkldim(ldx, n);

  dx = getlinalgdvec(offx, ldx * p, x);
  da = getlinalgdvec(0, p, a);
  dj = job != 0 ? getlinalgivec(0, p, j) : NULL;
  dw = job != 0 ? getlinalgdvec(0, p, w) : NULL;
  dr = null(r) ? NULL : getlinalgdvec(0, p * p, r);
  dq = null(q) ? NULL : getlinalgdvec(0, n * p, q);

  linpack_dqrdc(dx, ldx, n, p, da, dj, dw, job);

  if (! null(r)) {
    int i, j, ip, jn;

    /* copy the upper triangle of X to R in row major order */
    for (i = 0, ip = 0; i < p; i++, ip += p) {
      for (j = 0; j < i; j++)
	dr[ip + j] = 0.0;
      for (j = i, jn = j * n; j < p; j++, jn += n) 
	dr[ip + j] = dx[jn + i];
    }
  }

  if (! null(q)) {
    int i, j, ip, jn, jp;
    double t;

    /* copy X into Q in row major order */
    for (i = 0, ip = 0; i < n; i++, ip += p)
      for (j = 0, jn = 0; j < p; j++, jn += n)
	dq[ip + j] = dx[jn + i];

    /* accumulate the Q transformation */
    for (i = 0, ip = 0; i < p; i++, ip += p) {
      dq[ip + i] = da[i];
      for (j = i + 1; j < p; j++)
	dq[ip + j] = 0.0;
    }

    for (i = p - 1, ip = i * p; i >= 0; i--, ip -= p) {
      if (i == n - 1)
	dq[ip + i] = 1.0;
      else {
	for (j = i, jp = ip; j < n; j++, jp += p)
	  dq[jp + i] = -dq[jp + i];
	dq[ip + i] += 1.0;
      }
      for (j = i - 1, jp = ip - p; j >= 0; j--, jp -= p) {
	if (dq[jp + j] != 0.0) {
	  t = -blas_ddot(n - j, dq + jp + j, p, dq + jp + i, p) / dq[jp + j];
	  blas_daxpy(n - j, t, dq + jp + j, p, dq + jp + i, p);
	}
      }
    }
  }

  return NIL;
}

LVAL xslpdqrsl(V)
{
  LVAL x, qraux, y, qy, qty, b, rsd, xb;
  int n, k, job, info, cqy, cqty, cb, cr, cxb, offx, ldx;
  double *dx, *dqraux, *dy, *dqy, *dqty, *db, *drsd, *dxb;

  x = xlgetarg();
  offx = getfixnum(xlgafixnum());
  ldx = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  k = getfixnum(xlgafixnum());
  qraux = xlgetarg();
  y = xlgetarg();
  qy = xlgetarg();
  qty = xlgetarg();
  b = xlgetarg();
  rsd = xlgetarg();
  xb = xlgetarg();
  job = getfixnum(xlgafixnum());
  xllastarg();

  cqy = job / 10000 != 0;
  cqty = job % 10000 != 0;
  cb = job % 1000 / 100 != 0;
  cr = job % 100 / 10 != 0;
  cxb = job % 10 != 0;

  if (k > n)
    xlfail("more columns than rows");
  checkldim(ldx, n);

  dx = getlinalgdvec(offx, ldx * k, x);
  dqraux = getlinalgdvec(0, k, qraux);
  dy = getlinalgdvec(0, n, y);
  dqy = cqy ? getlinalgdvec(0, n, qy) : NULL;
  dqty = cqty || cb || cr || cxb ? getlinalgdvec(0, n, qty) : NULL;
  db = cb ? getlinalgdvec(0, k, b) : NULL;
  drsd = cr ? getlinalgdvec(0, n, rsd) : NULL;
  dxb = cxb ? getlinalgdvec(0, n, xb) : NULL;

  linpack_dqrsl(dx, ldx, n, k, dqraux,
		dy, dqy, dqty, db, drsd, dxb, job, &info);
  return info ? cvfixnum((FIXTYPE) (info - 1)) : NIL;
}

LVAL xslpzqrdc(V)
{
  LVAL x, a, j, w, r, q;
  dcomplex *dx, *da, *dw, *dr, *dq;
  int offx, ldx, n, p, *dj, job;

  x = xlgetarg();
  offx = getfixnum(xlgafixnum());
  ldx = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  p = getfixnum(xlgafixnum());
  a = xlgetarg();
  j = xlgetarg();
  w = xlgetarg();
  job = getfixnum(xlgafixnum());
  r = moreargs() ? xlgetarg() : NIL;
  q = moreargs() ? xlgetarg() : NIL;
  xllastarg();

  if (p > n && (! null(r) || ! null(q)))
    xlfail("more columns than rows");
  checkldim(ldx, n);

  dx = getlinalgzvec(offx, ldx * p, x);
  da = getlinalgzvec(0, p, a);
  dj = job != 0 ? getlinalgivec(0, p, j) : NULL;
  dw = job != 0 ? getlinalgzvec(0, p, w) : NULL;
  dr = null(r) ? NULL : getlinalgzvec(0, p * p, r);
  dq = null(q) ? NULL : getlinalgzvec(0, n * p, q);

  linpack_zqrdc(dx, ldx, n, p, da, dj, dw, job);

  if (! null(r)) {
    int i, j, ip, jn;

    /* copy the upper triangle of X to R in row major order */
    for (i = 0, ip = 0; i < p; i++, ip += p) {
      for (j = 0; j < i; j++)
	dr[ip + j].r = 0.0,
	dr[ip + j].i = 0.0;
      for (j = i, jn = j * n; j < p; j++, jn += n) 
	dr[ip + j] = dx[jn + i];
    }
  }

  if (! null(q)) {
    int i, j, ip, jn, jp;
    dcomplex t;

    /* copy X into Q in row major order */
    for (i = 0, ip = 0; i < n; i++, ip += p)
      for (j = 0, jn = 0; j < p; j++, jn += n)
	dq[ip + j] = dx[jn + i];

    /* accumulate the Q transformation */
    for (i = 0, ip = 0; i < p; i++, ip += p) {
      dq[ip + i] = da[i];
      for (j = i + 1; j < p; j++)
	dq[ip + j].r = 0.0,
	dq[ip + j].i = 0.0;
    }

    for (i = p - 1, ip = i * p; i >= 0; i--, ip -= p) {
      if (i == n - 1)
	dq[ip + i].r = 1.0,
	dq[ip + i].i = 0.0;
      else {
	for (j = i, jp = ip; j < n; j++, jp += p)
	  dq[jp + i].r = -dq[jp + i].r,
	  dq[jp + i].i = -dq[jp + i].i;
	dq[ip + i].r += 1.0;
      }
      for (j = i - 1, jp = ip - p; j >= 0; j--, jp -= p) {
	if (dq[jp + j].r != 0.0 || dq[jp + j].i != 0.0) {
	  blas_zdotc(&t, n - j, dq + jp + j, p, dq + jp + i, p);
	  t.r = -t.r,
	  t.i = -t.i;
	  z_div(&t, &t, &dq[jp + j]);
	  blas_zaxpy(n - j, &t, dq + jp + j, p, dq + jp + i, p);
	}
      }
    }
  }

  return NIL;
}

LVAL xslpzqrsl(V)
{
  LVAL x, qraux, y, qy, qty, b, rsd, xb;
  int n, k, job, info, cqy, cqty, cb, cr, cxb, offx, ldx;
  dcomplex *dx, *dqraux, *dy, *dqy, *dqty, *db, *drsd, *dxb;

  x = xlgetarg();
  offx = getfixnum(xlgafixnum());
  ldx = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  k = getfixnum(xlgafixnum());
  qraux = xlgetarg();
  y = xlgetarg();
  qy = xlgetarg();
  qty = xlgetarg();
  b = xlgetarg();
  rsd = xlgetarg();
  xb = xlgetarg();
  job = getfixnum(xlgafixnum());
  xllastarg();

  cqy = job / 10000 != 0;
  cqty = job % 10000 != 0;
  cb = job % 1000 / 100 != 0;
  cr = job % 100 / 10 != 0;
  cxb = job % 10 != 0;

  if (k > n)
    xlfail("more columns than rows");
  checkldim(ldx, n);

  dx = getlinalgzvec(offx, ldx * k, x);
  dqraux = getlinalgzvec(0, k, qraux);
  dy = getlinalgzvec(0, n, y);
  dqy = cqy ? getlinalgzvec(0, n, qy) : NULL;
  dqty = cqty || cb || cr || cxb ? getlinalgzvec(0, n, qty) : NULL;
  db = cb ? getlinalgzvec(0, k, b) : NULL;
  drsd = cr ? getlinalgzvec(0, n, rsd) : NULL;
  dxb = cxb ? getlinalgzvec(0, n, xb) : NULL;

  linpack_zqrsl(dx, ldx, n, k, dqraux,
		dy, dqy, dqty, db, drsd, dxb, job, &info);
  return info ? cvfixnum((FIXTYPE) (info - 1)) : NIL;
}


/************************************************************************/
/**                                                                    **/
/**                    Cholesky Decomposition                          **/
/**                                                                    **/
/************************************************************************/

LVAL xschol_decomp(V)
{
  LVAL a, da, val;
  int n;
  double maxoffl, maxadd;

  a = xlgadarray();
  maxoffl = moreargs() ? makefloat(xlgetarg()) : 0.0;
  xllastarg();

  checksquarematrix(a);
  n = numrows(a);

  xlstkcheck(2);
  xlsave(da);
  xlsave(val);

  da = gen2linalg(a, n, n, s_c_double, FALSE);
  choldecomp(REDAT(da), n, maxoffl, &maxadd);

  val = consa(cvflonum((FLOTYPE) maxadd));
  val = cons(linalg2genmat(da, n, n, FALSE), val);

  xlpopn(2);

  return val;
}


/************************************************************************/
/**                                                                    **/
/**                 Rotation Matrix Construction                       **/
/**                                                                    **/
/************************************************************************/

LVAL xsmake_rotation(V)
{
  LVAL x, y, dx, dy, val;
  double alpha=0.0;
  int n, use_alpha = FALSE;
  
  x = xlgetarg();
  y = xlgetarg();
  if (moreargs()) {
    use_alpha = TRUE;
    alpha = makefloat(xlgetarg());
  }
  xllastarg();
  
  xlstkcheck(3);
  xlsave(dx);
  xlsave(dy);
  xlsave(val);

  dx = coerce_to_tvec(x, s_c_double);
  dy = coerce_to_tvec(y, s_c_double);
  n = gettvecsize(dx);

  if (gettvecsize(dy) != n)
    xlfail("sequences not the same length");

  val = mktvec(n * n, s_c_double);
  make_rotation(n, REDAT(val), REDAT(dx), REDAT(dy), use_alpha, alpha);
  val = linalg2genmat(val, n, n, FALSE);
  
  xlpopn(3);

  return val;
}


/************************************************************************/
/**                                                                    **/
/**                        EISPACK Routines                            **/
/**                                                                    **/
/************************************************************************/

LVAL xseispackch(V)
{
  int nm, n, matz, ierr;
  LVAL ar, ai, w, zr, zi, fv1, fv2, fm1;
  double *dar, *dai, *dw, *dzr, *dzi, *dfv1, *dfv2, *dfm1;

  nm = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  ar = xlgetarg();
  ai = xlgetarg();
  w = xlgetarg();
  matz = getfixnum(xlgafixnum());
  zr = xlgetarg();
  zi = xlgetarg();
  fv1 = xlgetarg();
  fv2 = xlgetarg();
  fm1 = xlgetarg();
  xllastarg();
  
  checkldim(nm, n);
  dar = getlinalgdvec(0, nm * n, ar);
  dai = getlinalgdvec(0, nm * n, ai);
  dw = getlinalgdvec(0, n, w);
  dzr = (matz != 0) ? getlinalgdvec(0, nm * n, zr) : NULL;
  dzi = (matz != 0) ? getlinalgdvec(0, nm * n, zi) : NULL;
  dfv1 = getlinalgdvec(0, n, fv1);
  dfv2 = getlinalgdvec(0, n, fv2);
  dfm1 = getlinalgdvec(0, 2 * n, fm1);

  eispack_ch(nm, n, dar, dai, dw, matz, dzr, dzi, dfv1, dfv2, dfm1, &ierr);
  return (ierr == 0) ? NIL : cvfixnum((FIXTYPE) ierr);
}

LVAL xseispackrs(V)
{
  int nm, n, matz, ierr;
  LVAL a, w, z, fv1, fv2;
  double *da, *dw, *dz, *dfv1, *dfv2;

  nm = getfixnum(xlgafixnum());
  n = getfixnum(xlgafixnum());
  a = xlgetarg();
  w = xlgetarg();
  matz = getfixnum(xlgafixnum());
  z = xlgetarg();
  fv1 = xlgetarg();
  fv2 = xlgetarg();
  xllastarg();
  
  checkldim(nm, n);
  da = getlinalgdvec(0, nm * n, a);
  dw = getlinalgdvec(0, n, w);
  dz = (matz != 0) ? getlinalgdvec(0, nm * n, z) : NULL;
  dfv1 = getlinalgdvec(0, n, fv1);
  dfv2 = getlinalgdvec(0, n, fv2);

  
  eispack_rs(nm, n, da, dw, matz, dz, dfv1, dfv2, &ierr);
  return (ierr == 0) ? NIL : cvfixnum((FIXTYPE) ierr);
}


/************************************************************************/
/**                                                                    **/
/**                             A x + y                                **/
/**                                                                    **/
/************************************************************************/

LVAL xsaxpy(V)
{
  LVAL result, next, tx, a, x, y;
  int i, j, m, n, start, end, lower;
  double val;
  
  a = getdarraydata(xlgamatrix());
  x = xlgaseq();
  y = xlgaseq();
  lower = (moreargs() && xlgetarg() != NIL) ? TRUE : FALSE;
  
  n = seqlen(x);
  m = seqlen(y);
  if (lower && m != n)
    xlfail("dimensions do not match");
  
  xlsave1(result);
  result = mklist(m, NIL);
  for (i = 0, start = 0, next = result;
       i < m;
       i++, start += n, next = cdr(next)) {
    val = makefloat(getnextelement(&y, i));
    end = (lower) ? i +1 : n;
    for (j = 0, tx = x; j < end; j++) {
      val += makefloat(getnextelement(&tx, j)) 
	* makefloat(gettvecelement(a, start + j));
    }
    rplaca(next, cvflonum((FLOTYPE) val));
  }
  xlpop();
  return(result);
}


/************************************************************************/
/**                                                                    **/
/**                      Fast Fourier Transform                        **/
/**                                                                    **/
/************************************************************************/

LVAL xsfft(V)
{
  LVAL data, result, x, work;
  int n, isign;
  
  data = xlgaseq();
  isign = (moreargs() && xlgetarg() != NIL) ? -1.0 : 1.0; 
  xllastarg();
  
  /* check and convert the data */
  n = seqlen(data);
  if (n <= 0)
    xlfail("not enough data");

  xlstkcheck(2);
  xlsave(x);
  xlsave(work);
  x = gen2linalg(data, n, 1, s_c_dcomplex, FALSE);
  work = mktvec(4 * n + 15, s_c_double);

  cfft(n, REDAT(x), REDAT(work), isign);

  result = listp(x) ? coerce_to_list(x) : coerce_to_tvec(x, s_true);
  xlpopn(2);

  return result;
}


/************************************************************************/
/**                                                                    **/
/**               Smoothing and Interpolation Routines                 **/
/**                                                                    **/
/************************************************************************/

#define NS_DEFAULT 30

LVAL xsgetsmdata(V)
{
  LVAL s1, s2, arg;
  LVAL x, y, xs, ys;
  int n, ns, i, supplied, is_reg;
  double xmin, xmax, *dx, *dxs;

  s1 = xlgaseq();
  s2 = xlgetarg();
  arg = xlgetarg();
  is_reg = ! null(xlgetarg());
  xllastarg();

  if (is_reg && ! seqp(s2))
    xlbadtype(s2);
  if (! seqp(arg) && ! fixp(arg))
    xlbadtype(arg);

  ns = (fixp(arg)) ? getfixnum(arg) : seqlen(arg);
  supplied = (seqp(arg) && ns >= 1) ? TRUE : FALSE;
  if (ns < 1) ns = NS_DEFAULT;

  n = seqlen(s1);
  if (n <= 0)
    xlfail("sequence too short");
  if (is_reg && seqlen(s2) != n)
    xlfail("sequences not the same length");
  
  xlstkcheck(4);
  xlsave(x);
  xlsave(y);
  xlsave(xs);
  xlsave(ys);

  x = gen2linalg(s1, n, 1, s_c_double, FALSE);
  y = is_reg ? gen2linalg(s2, n, 1, s_c_double, FALSE) : NIL;
  xs = supplied ?
    gen2linalg(arg, ns, 1, s_c_double, FALSE) : mktvec(ns, s_c_double);
  ys = mktvec(ns, s_c_double);

  if (! supplied) {
    dx = REDAT(x);
    dxs = REDAT(xs);
    for (xmax = xmin = dx[0], i = 1; i < n; i++) {
      if (dx[i] > xmax) xmax = dx[i];
      if (dx[i] < xmin) xmin = dx[i];
    }
    for (i = 0; i < ns; i++)
      dxs[i] = xmin + (xmax - xmin) * ((double) i) / ((double) (ns - 1));
  }

  xlnumresults = 0;
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) n);
  xlresults[xlnumresults++] = x;
  xlresults[xlnumresults++] = y;
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) ns);
  xlresults[xlnumresults++] = xs;
  xlresults[xlnumresults++] = ys;
  xlpopn(4);
  return xlresults[0];
}

LVAL xsbasespline(V)
{
  LVAL x, y, xs, ys, work;
  double *dx, *dy, *dxs, *dys, *dwork;
  int n, ns, error;

  n = getfixnum(xlgafixnum());
  x = xlgetarg();
  y = xlgetarg();
  ns = getfixnum(xlgafixnum());
  xs = xlgetarg();
  ys = xlgetarg();
  work = xlgetarg();
  xllastarg();

  dx = getlinalgdvec(0, n, x);
  dy = getlinalgdvec(0, n, y);
  dxs = getlinalgdvec(0, ns, xs);
  dys = getlinalgdvec(0, ns, ys);
  dwork = getlinalgdvec(0, 2 * n, work);

  error = fit_spline(n, dx, dy, ns, dxs, dys, dwork);

  return error ? s_true : NIL;
}

LVAL xsbasekernelsmooth(V)
{
  LVAL x, y, xs, ys, targ;
  int n, ns, error, ktype;
  double *dx, *dy, *dxs, *dys, width;

  n = getfixnum(xlgafixnum());
  x = xlgetarg();
  y = xlgetarg();
  ns = getfixnum(xlgafixnum());
  xs = xlgetarg();
  ys = xlgetarg();
  width = makefloat(xlgetarg());
  targ = xlgasymbol();
  xllastarg();

  dx = getlinalgdvec(0, n, x);
  dy = null(y) ? NULL : getlinalgdvec(0, n, y);
  dxs = getlinalgdvec(0, ns, xs);
  dys = getlinalgdvec(0, ns, ys);

  switch (getstring(getpname(targ))[0]) {
  case 'U': ktype = 'U'; break;
  case 'T': ktype = 'T'; break;
  case 'G': ktype = 'G'; break;
  default:  ktype = 'B'; break;
  }

  error = kernel_smooth(dx, dy, n, width, NULL, NULL, dxs, dys, ns, ktype);

  return error ? s_true : NIL;
}

LVAL xsbaselowess(V) 
{
  LVAL x, y, ys, rw, res;
  double *dx, *dy, *dys, *drw, *dres;
  int n, nsteps, error;
  double f, delta;

  x = xlgetarg();
  y = xlgetarg();
  n = getfixnum(xlgafixnum());
  f = makefloat(xlgetarg());
  nsteps = getfixnum(xlgafixnum());
  delta = makefloat(xlgetarg());
  ys = xlgetarg();
  rw = xlgetarg();
  res = xlgetarg();
  xllastarg();

  dx = getlinalgdvec(0, n, x);
  dy = getlinalgdvec(0, n, y); 
  dys = getlinalgdvec(0, n, ys);
  drw = getlinalgdvec(0, n, rw);
  dres = getlinalgdvec(0, n, res);

  error = lowess(dx, dy, n, f, nsteps, delta, dys, drw, dres);

  return error ? s_true : NIL;
}

static LVAL add_contour_point P10C(int, m,
				   int, i,
				   int, j,
				   int,  k,
				   int, l,
				   double *, x,
				   double *, y,
				   double *, z,
				   double, v,
				   LVAL, result)
{
  LVAL pt;
  double p, q;
  double zij = z[i * m + j];
  double zkl = z[k * m + l];
  
  if ((zij <= v && v < zkl) || (zkl <= v && v < zij)) {
    xlsave(pt);
    pt = mklist(2, NIL);
    p = (v - zij) / (zkl - zij);
    q = 1.0 - p;
    rplaca(pt, cvflonum((FLOTYPE) (q * x[i] + p * x[k])));
    rplaca(cdr(pt), cvflonum((FLOTYPE) (q * y[j] + p * y[l])));
    result = cons(pt, result);
    xlpop();
  }
  return(result);
}

LVAL xssurface_contour(V)
{
  LVAL s1, s2, mat, result;
  LVAL x, y, z;
  double *dx, *dy, *dz;
  double v;
  int i, j, n, m;
  
  s1 = xlgaseq();
  s2 = xlgaseq();
  mat = xlgamatrix();
  v = makefloat(xlgetarg());
  xllastarg();
    
  n = seqlen(s1);
  m = seqlen(s2);
  if (n != numrows(mat) || m != numcols(mat))
    xlfail("dimensions do not match");

  xlstkcheck(4);
  xlsave(x);
  xlsave(y);
  xlsave(z);
  xlsave(result);

  x = gen2linalg(s1,  n, 1, s_c_double, FALSE); dx = REDAT(x);
  y = gen2linalg(s2,  m, 1, s_c_double, FALSE); dy = REDAT(y);
  z = gen2linalg(mat, n, m, s_c_double, FALSE); dz = REDAT(z);
  result = NIL;

  for (i = 0; i < n - 1; i++) {
    for (j = 0; j < m - 1; j++) {
      result = add_contour_point(m, i,   j,   i,   j+1, dx, dy, dz, v, result);
      result = add_contour_point(m, i,   j+1, i+1, j+1, dx, dy, dz, v, result);
      result = add_contour_point(m, i+1, j+1, i+1, j,   dx, dy, dz, v, result);
      result = add_contour_point(m, i+1, j,   i,   j,   dx, dy, dz, v, result);
    }
  }
  xlpopn(4);
  
  return(result);
}

/************************************************************************/
/**                                                                    **/
/**                  Machine Epsilon Determination                     **/
/**                                                                    **/
/************************************************************************/

double macheps(V)
{
  static int calculated = FALSE;
  static double epsilon = 1.0;
  
  if (! calculated)
    while (1.0 + epsilon / 2.0 != 1.0) epsilon = epsilon / 2.0;
  calculated = TRUE;
  return(epsilon);
}
