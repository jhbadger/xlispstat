#include "xlisp.h"
#include "xlbcode.h"

typedef VOID (*modulefun) P1H(int);

LOCAL VOID dummy1 P1H(int);
LOCAL VOID dummy2 P1H(int);
LOCAL VOID dummy3 P1H(int);
LOCAL VOID dummy4 P1H(int);
LOCAL VOID dummy5 P1H(int);
LOCAL VOID dummy6 P1H(int);
VOID init___dummy P2H(int *, modulefun **);

/* (cmp '(lambda (x) (+ x 1))) */
LOCAL VOID dummy1 P1C(int, entry)
{
  LVAL function = vsbase[-1];
  LVAL literals = bcode_literals(function);
  CONTINUATIONP FVcont = xlcstop;
  LVAL tmp, *base;

 Entry:

 L0:
  /* (%INITIALIZE 0 2 1 0 0) */
  cmp_check_required_only_argcount(2);
  pusharg(getlitval(0));

  /* (%ARITH2 43 1 2 0) */
  set_one_result(xladd2(getregval(1), getregval(2)));

  /* (%RETURN 0) */
  RETURN(0);
}

/* (cmp '(lambda (x) (let ((s 0)) (dolist (y x s) (incf s y))))) */
LOCAL VOID dummy2 P1C(int, entry)
{
  LVAL function = vsbase[-1];
  LVAL literals = bcode_literals(function);
  CONTINUATIONP FVcont = xlcstop;
  LVAL tmp;

 Entry:

  /* (%INITIALIZE 0 2 1 0 3) */
  cmp_check_required_only_argcount(2);
  pusharg(getlitval(0));
  cmp_push_space(3);

  /* (%COPY 2 3) */
  setregval(3, getregval(2));

  /* (%COPY 1 4) */
  setregval(4, getregval(1));

  /* (%CAR 4 5) */
  setregval(5, cmpCAR(getregval(4)));

  /* (%GOTO L2) */
  goto L2;

 L1:
  /* (%ARITH2 43 3 5 3) */
  setregval(3, xladd2(getregval(3), getregval(5)));

  /* (%CDR 4 4) */
  setregval(4, cmpCDR(getregval(4)));

  /* (%CAR 4 5) */
  setregval(5, cmpCAR(getregval(4)));

 L2:
  /* (%TEST-1 1 L1 L3 4) */
  if (consp(getregval(4))) goto L1;
  else goto L3;

 L3:
  /* (%SET-ONE-VALUE-RETURN 0 3) */
  set_one_result(getregval(3));
  RETURN(0);
}

/* (cmp '(lambda (x)
           (let ((s 0)) (dotimes (i (length x) s) (incf s (aref x i))))))*/
LOCAL VOID dummy3 P1C(int, entry)
{
  LVAL function = vsbase[-1];
  LVAL literals = bcode_literals(function);
  CONTINUATIONP FVcont = xlcstop;
  LVAL tmp, *base;

 Entry:

  switch (entry) {
  case 0: goto L0;
  case 1: goto L1;
  }

 L0:
  /* (%INITIALIZE 0 2 1 0 4) */
  cmp_check_required_only_argcount(2);
  pusharg(getlitval(0));
  cmp_push_space(4);

  /* (%COPY 2 3) */
  setregval(3, getregval(2));

  /* (%SAVE-CALL 1 4 1 1) */
  getlitfun(1, tmp);
  base = vsbase;
  cmp_call_setup(tmp, 4, 1, 1, NIL, FALSE);
  *xlsp++ = base[1];
  cmp_do_call_set(tmp, 1, 4);
 L1:

  /* (%COPY 2 5) */
  setregval(5, getregval(2));

  /* (%GOTO L3) */
  goto L3;

 L2:
  /* (%AREF1 1 5 6) */
  setregval(6, cmpAREF1(getregval(1), getregval(5)));

  /* (%ARITH2 43 3 6 3) */
  setregval(3, xladd2(getregval(3), getregval(6)));

  /* (%ARITH1 112 5 5) */
  setregval(5, xladd1(getregval(5)));

 L3:
  /* (%TEST-ARITH-2 60 L2 L4 5 4) */
  if (num_cmp2(60, getregval(5), getregval(4))) goto L2;
  else goto L4;

 L4:
  /* (%SET-ONE-VALUE-RETURN 0 3) */
  set_one_result(getregval(3));
  RETURN(0);
}

/*(cmp '(lambda (x) (f x)))*/
LOCAL VOID dummy4 P1C(int, entry)
{
  LVAL function = vsbase[-1];
  LVAL literals = bcode_literals(function);
  CONTINUATIONP FVcont = xlcstop;
  LVAL tmp, *base;

 Entry:

 L0:
  /* (%INITIALIZE 0 2 0 0) */
  cmp_check_required_only_argcount(2);

  /* (%CALL 0 0 1 1) */
  getlitfun(0, tmp);
  base = vsbase;
  cmp_call_setup(tmp, -1, 0, 1, getregval(0), TRUE);
  *xlsp++ = base[1];
  cmp_do_tail_call(tmp, base, 1, 0);
}

/* tak */
LOCAL VOID dummy5 P1C(int, entry)
{
  LVAL function = vsbase[-1];
  LVAL literals = bcode_literals(function);
  CONTINUATIONP FVcont = xlcstop;
  LVAL tmp, *base;

 Entry:

  switch (entry) {
  case 0: goto L0;
  case 2: goto L2;
  case 3: goto L3;
  case 4: goto L4;
  }

 L0:
  /* (%INITIALIZE 0 4 0 3) */
  cmp_check_required_only_argcount(4);
  cmp_push_space(3);

  /* (%TEST-ARITH-2 60 L1 L5 2 1) */
  if (num_cmp2(60, getregval(2), getregval(1))) goto L1;
  else goto L5;

 L1:
  /* (%ARITH1 109 1 4) */
  setregval(4, xlsub1(getregval(1)));

  /* (%SAVE-CALL 0 4 3 4 2 3) */
  getlitfun(0, tmp);
  base = vsbase;
  cmp_call_setup(tmp, 4, 2, 3, NIL, FALSE);
  *xlsp++ = base[4];
  *xlsp++ = base[2];
  *xlsp++ = base[3];
  cmp_do_call_set(tmp, 3, 4);
 L2:

  /* (%ARITH1 109 2 5) */
  setregval(5, xlsub1(getregval(2)));

  /* (%SAVE-CALL 0 5 3 5 3 1) */
  getlitfun(0, tmp);
  base = vsbase;
  cmp_call_setup(tmp, 5, 3, 3, NIL, FALSE);
  *xlsp++ = base[5];
  *xlsp++ = base[3];
  *xlsp++ = base[1];
  cmp_do_call_set(tmp, 3, 5);
 L3:

  /* (%ARITH1 109 3 6) */
  setregval(6, xlsub1(getregval(3)));

  /* (%SAVE-CALL 0 6 3 6 1 2) */
  getlitfun(0, tmp);
  base = vsbase;
  cmp_call_setup(tmp, 6, 4, 3, NIL, FALSE);
  *xlsp++ = base[6];
  *xlsp++ = base[1];
  *xlsp++ = base[2];
  cmp_do_call_set(tmp, 3, 6);
 L4:

  /* (%CALL 0 0 3 4 5 6) */
  getlitfun(0, tmp);
  base = vsbase;
  cmp_call_setup(tmp, -1, 0, 3, getregval(0), TRUE);
  *xlsp++ = base[4];
  *xlsp++ = base[5];
  *xlsp++ = base[6];
  cmp_do_tail_call(tmp, base, 3, 0);

 L5:
  /* (%SET-ONE-VALUE-RETURN 0 3) */
  set_one_result(getregval(3));
  RETURN(0);
}

/* ltak */
LOCAL VOID dummy6 P1C(int, entry)
{
  LVAL function = vsbase[-1];
  LVAL literals = bcode_literals(function);
  CONTINUATIONP FVcont = xlcstop;
  LVAL tmp, *base;

 Entry:

  switch (entry) {
  case 0: goto L0;
  case 3: goto L3;
  case 4: goto L4;
  case 5: goto L5;
  }

 L0:
  /* (%INITIALIZE 0 4 0 0) */
  cmp_check_required_only_argcount(4);

  /* (%LCALL L1 0 3 1 2 3) */
  base = vsbase;
  cmp_tail_lcall_setup(3, getregval(0));
  *xlsp++ = base[1];
  *xlsp++ = base[2];
  *xlsp++ = base[3];
  cmp_do_tail_lcall(L1, base);

 L1:
  /* (%INITIALIZE 0 4 0 3) */
  cmp_check_required_only_argcount(4);
  cmp_push_space(3);

  /* (%TEST-ARITH-2 60 L2 L6 2 1) */
  if (num_cmp2(60, getregval(2), getregval(1))) goto L2;
  else goto L6;

 L2:
  /* (%ARITH1 109 1 4) */
  setregval(4, xlsub1(getregval(1)));

  /* (%SAVE-LCALL L1 4 3 4 2 3) */
  base = vsbase;
  cmp_lcall_setup(4, 3, 3);
  *xlsp++ = base[4];
  *xlsp++ = base[2];
  *xlsp++ = base[3];
  cmp_do_lcall(L1);
 L3:

  /* (%ARITH1 109 2 5) */
  setregval(5, xlsub1(getregval(2)));

  /* (%SAVE-LCALL L1 5 3 5 3 1) */
  base = vsbase;
  cmp_lcall_setup(5, 4, 3);
  *xlsp++ = base[5];
  *xlsp++ = base[3];
  *xlsp++ = base[1];
  cmp_do_lcall(L1);
 L4:

  /* (%ARITH1 109 3 6) */
  setregval(6, xlsub1(getregval(3)));

  /* (%SAVE-LCALL L1 6 3 6 1 2) */
  base = vsbase;
  cmp_lcall_setup(6, 5, 3);
  *xlsp++ = base[6];
  *xlsp++ = base[1];
  *xlsp++ = base[2];
  cmp_do_lcall(L1);
 L5:

  /* (%LCALL L1 0 3 4 5 6) */
  base = vsbase;
  cmp_tail_lcall_setup(3, getregval(0));
  *xlsp++ = base[4];
  *xlsp++ = base[5];
  *xlsp++ = base[6];
  cmp_do_tail_lcall(L1, base);

 L6:
  /* (%SET-ONE-VALUE-RETURN 0 3) */
  set_one_result(getregval(3));
  RETURN(0);
}

static VOID (*dummy___functions[]) _((int)) = {
  dummy1,
  dummy2,
  dummy3,
  dummy4,
  dummy5,
  dummy6
  };

#define dummy___count 6

VOID init___dummy P2C(int *, pn, modulefun **, pf)
{
  *pn = dummy___count;
  *pf = dummy___functions;
}
