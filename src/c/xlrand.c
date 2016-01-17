/* xlrand.c - random number generators */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

/*
Generators are represented by a state consisting of a fixnum index and
a fixnum vector of seed data. By convention, the 0 entry in the seed
is the maxial integer the generator produces, and entry 1 is the
current random integer. MAKE-RANDOM-STATE accepts additional optional
arguments specifying the generator and the seed data. If seed data is
not given, the generator is seeded using the system clock. If the
generator is not specified, a new seed for the current generator is
returned. So

	(make-random-state t)

creates a new seed for the current generator, and

	(make-random-state t n)

creates a new seed for generator n.

Currently four generators are supported:

0 -- The original XLISP-STAT generator, Marsaglia's portable generator
from CMLIB. This is a lagged Fibonacci generator.

1 -- L'Ecuyer's version of the Wichmann Hill generator, also used in
Bratley, Fox and Schrage, 2nd ed., in program UNIFL.  L'Ecuyer,
(1986), "Efficient and portable combined random number generators,"
Communications of the ACM 31, 742-749.

2 -- Marsaglia's Super-Duper, as used in S.

3 -- Combined Tausworthe generator. Tezuka and L'Ecuyer, (1991),
"Efficient and portable combined Tauseworthe random number
generators," ACM Transactions on Modeling and Computer Simulation 1,
99-112.

All four implementations should work on machines where long's and
FIXNUM's are at least 32 bits.
*/

#include "xlisp.h"

#define DFLTGEN 1
#define MPFIX32 2147483647

#define HPART(x) (((unsigned long) (x)) >> 16)
#define LPART(x) (((unsigned long) (x)) & 0xFFFF)
#define SETULONG(x,h,l) ((x) = ((h) << 16) + (l))

LOCAL LVAL mkrndstate P2H(LVAL, LVAL);
LOCAL LVAL cvtoldstate P1H(LVAL);
LOCAL LVAL getrndstate(V);
LOCAL LVAL copyrndstate P1H(LVAL);
LOCAL VOID xlbadrndstate P1H(LVAL);
LOCAL VOID xlbadrnddata P1H(LVAL);

LOCAL LVAL mkrndstate P2C(LVAL, gen, LVAL, data)
{
  xlstkcheck(2);
  xlprotect(gen);
  xlprotect(data);

  if (null(gen)) gen = getrndgen(getrndstate());
  if (! fixp(gen)) xlbadtype(gen);

  if (! null(data) && vectorp(data)) {
    int i, n;
    LVAL tmp;
    n = getsize(data);
    tmp = mktvec(n, a_fixnum);
    for (i = 0; i < n; i++)
      settvecelement(tmp, i, getelement(data, i));
    data = tmp;
  }
  if (!null(data) && (! tvecp(data) || gettvecetype(data) != a_fixnum))
    xlbadtype(data);

  switch ((int) getfixnum(gen)) {
  case 0: /* original Marsaglia portable generator from CMLIB */
    {
      int MDIG = 32;
      int M1   = 20;
      int M2   = 21;
      int I    = 22;
      int J    = 23;
      FIXTYPE *d, m1, m2, k0, k1, j0, j1;
      int i;
      unsigned long seed;

      if (null(data)) {
	data = mktvec(24, a_fixnum);
	d = (FIXTYPE *) gettvecdata(data);

	seed = system_tick_count();

	m1 = (1L << (MDIG - 2)) + ((1L << (MDIG - 2)) - 1);
	m2 = (1L << (MDIG / 2));

	d[0]  = MPFIX32;
	d[1]  = 0;
	d[2]  = (FIXTYPE) seed;
	d[M1] = m1;
	d[M2] = m2;
	d[I]  = 7;
	d[J]  = 19;
	
	if (seed % 2 == 0) seed--;
	k0 = 9069 % m2;
	k1 = 9069 / m2;
	j0 = seed % m2;
	j1 = seed / m2;
	for (i = 3; i <= 19; i++) {
	  seed = j0 * k0;
	  j1 = ((seed / m2) + j0 * k1 + j1 * k0) % (m2 / 2);
	  j0 = seed % m2;
	  d[i] = j0 + m2 * j1;
	}
      }
      else {
	if (gettvecsize(data) != 24) xlbadrnddata(data);
	d = (FIXTYPE *) gettvecdata(data);
	if (d[M1] <= 0 || d[I] <= 2 || d[I] > 19 || d[J] <= 2 || d[J] > 19)
	  xlbadrnddata(data);
      }
    }
    break;
  case 1: /* L'Ecuyer's variant of Wichman-Hill */
    {
      unsigned long Mask1=2147483562, Mask2=2147483563, Mask3 = 2147483399;
      unsigned long seed;
      long J2, J3;
      FIXTYPE *d;

      if (null(data)) {
	data = mktvec(4, a_fixnum);
	d = (FIXTYPE *) gettvecdata(data);

	seed = system_tick_count();
	J2 = seed / (1L << 16);
	J3 = seed % (1L << 16);
	if (J2 <= 0 || J2 >= Mask2) J2 = 1;
	if (J3 <= 0 || J3 >= Mask3) J3 = 1;

	d[0] = Mask1;
	d[1] = 0;
	d[2] = J2;
	d[3] = J3;
      }
      else {
	if (gettvecsize(data) != 4) xlbadrnddata(data);
	d = (FIXTYPE *) gettvecdata(data);
	if (d[2] <= 0 || d[2] >= Mask2 || d[3] <= 0 || d[3] >= Mask3)
	  xlbadrnddata(data);
      }
    }
    break;
  case 2: /* Super-Duper */
    {
      unsigned long Mask2 = 0x10000;
      unsigned long JC, JT, seed;
      FIXTYPE *d;


      if (null(data)) {
	data = mktvec(6, a_fixnum);
	d = (FIXTYPE *) gettvecdata(data);

	seed = system_tick_count();
	JC = seed / (1L << 16);
	JT = seed % (1L << 16);
	if (JC <= 0 || JC >= Mask2) JC = 1;
	if (JT <= 0 || JT >= Mask2) JT = 1;
	if (JC % 2 == 0) JC--;

	d[0] = MPFIX32;
	d[1] = 0;
	d[2] = HPART(JC);
	d[3] = LPART(JC);
	d[4] = HPART(JT);
	d[5] = LPART(JT);
      }
      else {
	if (gettvecsize(data) != 6) xlbadrnddata(data);
	d = (FIXTYPE *) gettvecdata(data);
	if (d[2] < 0 || d[2] >= Mask2 || d[3] < 0 || d[3] >= Mask2 ||
	    (d[2] == 0 && d[3] == 0) ||
	    d[4] < 0 || d[4] >= Mask2 || d[5] < 0 || d[5] >= Mask2 ||
	    (d[4] == 0 && d[5] == 0))
	  xlbadrnddata(data);
      }
    }
    break;
  case 3: /* L'Ecuyer and Tezuka combined Tausworthe generator */
    {
      unsigned long I1, I2, Mask1=2147483647, Mask2=536870911;
      unsigned long seed;
      FIXTYPE *d;

      if (null(data)) {
	data = mktvec(4, a_fixnum);
	d = (FIXTYPE *) gettvecdata(data);

	seed = system_tick_count();
	I1 = seed / (1L << 16);
	I2 = seed % (1L << 16);
	if (I1 <= 0 || I1 > Mask1) I1 = 1;
	if (I2 <= 0 || I2 > Mask2) I1 = 1;

	d[0] = Mask1;
	d[1] = 0;
	d[2] = I1;
	d[3] = I2;
      }
      else {
	if (gettvecsize(data) != 4) xlbadrnddata(data);
	d = (FIXTYPE *) gettvecdata(data);
	if (d[2] <= 0 || d[2] > Mask1 || d[3] <= 0 || d[3] > Mask2)
	  xlbadrnddata(data);
      }
    }
    break;
  default: xlerror("unknown generator", gen);
  }
  
  xlpopn(2);

  return(newrndstate(gen, data));
}

/* convert old-style state to new one */
LOCAL LVAL cvtoldstate P1C(LVAL, oldstate)
{
  LVAL tmp;
  int i, n;
  int I = 22, J = 23;

  xlsave1(tmp);
  n = getsize(oldstate);
  tmp = newvector(n + 2);
  setelement(tmp, 0, cvfixnum((FIXTYPE) MPFIX32));
  setelement(tmp, 1, cvfixnum((FIXTYPE) 0));
  for (i = 0; i < n; i++)
    setelement(tmp, i + 2, getelement(oldstate, i));
  setelement(tmp, I, cvfixnum((FIXTYPE) (getfixnum(getelement(tmp, I)) + 2)));
  setelement(tmp, J, cvfixnum((FIXTYPE) (getfixnum(getelement(tmp, J)) + 2)));
  tmp = mkrndstate(cvfixnum((FIXTYPE) 0), tmp);
  xlpop();
  return(tmp);
}

LOCAL LVAL getrndstate(V)
{
  LVAL val;
  val = getvalue(s_rndstate);
  switch(ntype(val)) {
  case RNDSTATE:
    break;
  case VECTOR:
    val = cvtoldstate(val);
    setvalue(s_rndstate, val);
  default:
    xlbadrndstate(val);
  }
  return(val);
}

LOCAL LVAL copyrndstate P1C(LVAL, val)
{
  return(newrndstate(getrndgen(val), copyvector(getrnddata(val))));
}

LVAL xmkrndstate(V)
{
  LVAL arg, gen, data;
  
  arg = moreargs() ? nextarg() : NIL;
  gen = moreargs() ? xlgafixnum() : NIL;
  data = moreargs() ? xlgetarg() : NIL;
  xllastarg();
  
  if (arg == NIL)
    return(copyrndstate(getrndstate()));
  else if (arg == s_true)
    return(mkrndstate(gen, data));
  else if (rndstatep(arg))
    return(copyrndstate(arg));
  else if (vectorp(arg))
    return(cvtoldstate(arg));
  else xlbadtype(arg);
  /* not reached */
  return(NIL);
}

double xlunirand(V)
{
  double x = 0.0;
  LVAL state;
  int g;
  FIXTYPE *d;

  state = getrndstate();
  g = (int) getfixnum(getrndgen(state));
  d = (FIXTYPE *) gettvecdata(getrnddata(state));

  do {
    switch (g) {
    case 0: /* original Marsaglia portable generator from CMLIB */
      {
	static int M1   = 20;
	static int I    = 22;
	static int J    = 23;
	long k, i, j, m1;

	m1 = d[M1];
	i =  d[I];
	j =  d[J];

	k =  d[i] - d[j];
	if (k < 0) k = k + m1;
	d[j] = k;
	i = i - 1;
	if (i <= 2) i = 19;
	j = j - 1;
	if (j <= 2) j = 19;
	d[I] = i;
	d[J] = j;
	
	d[1] = k;
	x = ((double) k) / m1;
      }
      break;
    case 1: /* L'Ecuyer's variant of Wichman-Hill */
      {
	static unsigned long Mask1=2147483562;
	static unsigned long Mask2=2147483563;
	static unsigned long Mask3 = 2147483399;
	static double Norm = 4.6566130573E-10;
	long k, J2, J3;

	J2 = d[2];
	J3 = d[3];

	/* Get next term in 40014 * J2 MOD Mask2 */
	k = J2 / 53668;
	J2 = 40014 * (J2 - k * 53668) - k * 12211;
	if (J2 < 0) J2 = J2 + Mask2;

	/* Get next term in 40692 * J3 MOD Mask3 */
	k = J3 / 52774;
	J3 = 40692 * (J3 - k * 52774) - k * 3791;
	if (J3 < 0) J3 = J3 + Mask3;

	/* Set J1 = ((J3 + Mask1 - J2) MOD Mask1) + 1 */
	k = J2 - J3;
	if (k < 1) k = k + Mask1;

	/* Store results */
	d[1] = k;
	d[2] = J2;
	d[3] = J3;

	/* Put it on the interval (0,1) */
	x = k * Norm;
      }
      break;
    case 2: /* Super-Duper */
      {
	static double Norm=4.656612873E-10;
	unsigned long JC, JT;
	long k;

	SETULONG(JC, d[2], d[3]);
	SETULONG(JT, d[4], d[5]);

	JC = (JC * 69069) & 0xFFFFFFFF;	/* congruential part */
	JT ^= JT >> 15;				/* tausworthe part */
	JT ^= (JT << 17) & 0xFFFFFFFF;
	k = ((JT ^ JC) >> 1);

	d[1] = k;
	d[2] = HPART(JC);
	d[3] = LPART(JC);
	d[4] = HPART(JT);
	d[5] = LPART(JT);

	x = k * Norm;
      }
      break;
    case 3: /* L'Ecuyer and Tezuka combined Tausworthe generator */
      {
	static long Q1=13, Q2=2, S1=17, S2=17, P1mS1=19, P2mS2=12, P1mP2=2;
	static unsigned long Mask1=2147483647, Mask2=536870911;
	static double Norm=4.656612873E-10;
	unsigned long I1, I2;
	unsigned long b;

	I1 = d[2];
	I2 = d[3];

	b=((I1 << Q1) ^ I1) & Mask1;
	I1 = ((I1 << S1) ^ (b >> P1mS1)) & Mask1;
	b = ((I2 << Q2) ^ I2) & Mask2;
	I2 = ((I2 << S2) ^ (b >> P2mS2)) & Mask2;
	b = I1 ^ (I2 << P1mP2);

	d[1] = b;
	d[2] = I1;
	d[3] = I2;

	x = b * Norm;
      }
      break;
    default: xlbadrndstate(state);
    }
  } while (x <= 0.0 || x >= 1.0);
  return (x);
}

long osrand P1C(long, n)
{
  long k;

  if (n < 1) xlbadtype(cvfixnum((FIXTYPE) n));

  do {
    k = n * xlunirand();
  } while (k < 0 && k >= n);
  return(k);
}

LVAL xrndstatep(V)
{
  LVAL x;
  x = xlgetarg();
  xllastarg();
  return(rndstatep(x) ? s_true : NIL);
}

LVAL xrand(V)
{
  LVAL arg;
  FIXTYPE n, k;
  double x, y;

  arg = xlgetarg();
  xllastarg();

  switch (ntype(arg)) {
  case FIXNUM:
    n = getfixnum(arg);
    if (n <= 0) xlbadtype(arg);
    do {
      k = n * xlunirand();
    } while (k < 0 || k >= n);
    return(cvfixnum(k));
  case FLONUM:
    x = getflonum(arg);
    if (x <= 0.0) xlbadtype(arg);
    do {
      y = x * xlunirand();
    } while (y <= 0.0 && y >= x);
    return(cvflonum((FLOTYPE) y));
  default:
    xlbadtype(arg);
  }
  /* not reached */
  return(NIL);
}

VOID initrndstate(V)
{
  setsvalue(s_rndstate, mkrndstate(cvfixnum((FIXTYPE) DFLTGEN), NIL));
}

LOCAL VOID xlbadrndstate P1C(LVAL, val)
{
  xlerror("bad random state", val);
}

LOCAL VOID xlbadrnddata P1C(LVAL, val)
{
  xlerror("bad random state data", val);
}

