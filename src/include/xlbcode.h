#define DONE NIL
#define NO_VALUE -1

#define bcode_literals(fun) getbclits(getbcccode(fun))
#define get_one_result() xlresults[0]
#define get_nth_result(n) (((n) < xlnumresults) ? xlresults[n] : NIL)
#define set_nth_result(n,v) (xlresults[n] = (v))
#define set_no_results() (xlnumresults = 0, xlresults[0] = NIL)
#define set_one_result(v) (xlnumresults = 1, xlresults[0] = (v))
#define getlitval(n) (getelement(literals,n))

#define getlitfun(n, fun) { \
  (fun) = getlitval(n); \
  while (! fboundp(fun)) xlfunbound(fun); \
  (fun) = getfunction(fun); \
}

#define getregval(i) (vsbase[i])
#define setregval(i,v) (vsbase[i] = (v))

#define RETURN(c) { \
  LVAL __c__ = (getregval(c)); \
  if (__c__ == DONE) { vsbase[-1] = NIL; return; } \
  xlcstop = xlcontinuation_stack + getfixnum(__c__); \
  vsbase = xlcstop->base; \
  vstop = xlcstop->top; \
  if (xlcstop->vreg != NO_VALUE) setregval(xlcstop->vreg,get_one_result()); \
  if (xlcstop < FVcont) return; \
  else { entry = xlcstop->pe.entry; goto Entry; } \
}

#define cmp_check_required_only_argcount(n) { \
  int argc = vstop - vsbase; \
  if (argc != (n)) { \
    if (argc < (n)) xltoofew(); \
    else xltoomany(); \
  } \
}

#define cmp_push_space(n) { \
  int __n__ = (n); \
  if (xlsp + __n__ > xlargstktop) xlargstkoverflow(); \
  while (__n__ -- > 0) *xlsp++ = NIL; \
}

extern LVAL cmpAREF1 _((LVAL xl, LVAL il));

#define cmpCAR(x) \
  (tmp = (x), (null(tmp)) ? NIL : (consp(tmp)) ? car(tmp) : xlbadtype(tmp))
#define cmpCDR(x) \
  (tmp = (x), (null(tmp)) ? NIL : (consp(tmp)) ? cdr(tmp) : xlbadtype(tmp))

#define cmp_save_current_continuation(Entry, vr) { \
  if (xlcstop >= xlcsend) xlabort("continuation stack overflow"); \
  xlcstop->base = vsbase; \
  xlcstop->top = vstop; \
  xlcstop->pe.entry = (Entry); \
  xlcstop->vreg = (vr); \
  xlcstop++; \
}

extern VOID cmp_call_setup _((LVAL fun, int vi, int entry, int argc,
			      LVAL cont, int tailp));

#define cmp_shift_tail_frame(base) { \
  if (xlcstop[-1].base != base) { \
    int n = vstop - vsbase; \
    MEMMOVE(base - 1, vsbase - 1, sizeof(LVAL) * (n + 1)); \
    vstop = base + n; \
    vsbase = base; \
  } \
}

#define cmp_do_call(fun, argc) { \
  if (bcclosurep(fun)) return; \
  else xlapply(argc); \
}

#define cmp_do_call_set(fun, argc, vreg) { \
  if (bcclosurep(fun)) return; \
  else setregval(vreg, xlapply(argc)); \
}

#define cmp_do_tail_call(fun,base,argc,creg) { \
  if (bcclosurep(fun)) { \
    cmp_shift_tail_frame(base); \
    return; \
  } \
  else { \
    xlapply(argc); \
    RETURN(creg); \
  } \
}

#define cmp_do_lcall(f) goto f;

#define cmp_do_tail_lcall(f,base) {\
  cmp_shift_tail_frame(base); \
  goto f; \
}

#define cmp_tail_lcall_setup(argc, cont) { \
  LVAL Cont = (cont); \
  pusharg(vsbase[-1]); \
  vsbase = vstop; \
  pusharg(Cont); \
  if (xlsp + argc > xlargstktop) xlargstkoverflow(); \
}

#define cmp_lcall_setup(vi, entry, argc) { \
  LVAL Cont; \
  Cont = cvfixnum((FIXTYPE) (xlcstop - xlcontinuation_stack)); \
  cmp_save_current_continuation(entry, vi); \
  pusharg(vsbase[-1]); \
  vsbase = vstop; \
  pusharg(Cont); \
  if (xlsp + argc > xlargstktop) xlargstkoverflow(); \
}
