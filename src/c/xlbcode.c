/**** use getlitfun where needed */
/**** allow literals to be NIL instead of zero-length vector */
/**** reduce stack checks */
/**** think through adding C continuations */
/**** avoid making closure in evform? */
/**** work through all opcodes provided and needed to get all special forms */
/**** separate out inline code */
/**** make more efficient function represaentation */
/**** move argument fixup, expansion, constant loading into code */

/**** build complete machine for unoptimized code */
/**** check that fast dispatch in non_bcode_call is OK */
/**** check MULVALS, protection of fun in non_bcode_call; check efficiency */
/**** play with efficiency of VM before adding lots of opcodes!!!*/
/**** check efficiency of number stuff */
/**** check on dynamic env and unwind-protect */

#include "xlisp.h"
#ifdef BYTECODE
#include "xlbcode.h"
#include "xlmodule.h"

/* forward declarations */
LOCAL LVAL copy_bcode P1H(LVAL);
LOCAL bytecode *non_bcode_call P4H(bytecode *, LVAL, int, int);
LOCAL bytecode *bccall_setup P5H(bytecode *, LVAL, LVAL, int, int);
LOCAL LVAL getf P3H(LVAL, LVAL, LVAL);
LOCAL int is_member P2H(LVAL, LVAL);
LOCAL int member_eql P2H(LVAL, LVAL);
LOCAL VOID bcloop P1H(int);
LOCAL int any_references_p P2H(LVAL, LVAL);
LOCAL VOID find_references P3H(LVAL, LVAL, LVAL *);
LOCAL LVAL cps_node_internals P1H(int);
LOCAL LVAL set_cps_node_internals P1H(int);

/*#define PROFILE*/
#ifdef PROFILE
LVAL s_profile_output;
#endif /* PROFILE */

/* external variables and functions */
extern LVAL xlenv, xlfenv, xldenv;
extern LVAL k_allow_other_keys, s_strict_keywords;

static LVAL s_leaf, s_call, s_not_supplied;

/* System Constants */ /**** move to xlisp.h */
#define CDEPTH 1000               /**** see if this is big enough */


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**         Byte Code Representation, Construction and Modification         **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

LVAL xlbcclose(V)
{
  LVAL form = xlgabcode();
  return(newbcclosure(s_lambda, form));
}

/****** maybe this should copy its closure? */
LVAL xlcoercemacro(V)
{
  LVAL fun;
  fun = xlgetarg();
  xllastarg();
  switch (ntype(fun)) {
  case CLOSURE:   settype(fun, s_macro); break;
  case BCCLOSURE: setbcctype(fun, s_macro); break;
  default:        xlbadtype(fun);
  }
  return(fun);
}

#define bcode_codevec(fun) ((bytecode *) getstring(getbccode(getbcccode(fun))))
#define bcode_jumptable(fun) getbcjtab(getbcccode(fun))
/*****
#define bcode_literals(fun) getbclits(getbcccode(fun))
*/
#define bcode_index(fun) getfixnum(getbcidx(getbcccode(fun)))
#define bcode_environment(fun) getbcenv(getbcccode(fun))

#define set_bcode_index(fun,fi) setbcidx(getbcccode(fun),cvfixnum((FIXTYPE) fi))
#define set_bcode_environment(fun,env) setbcenv(getbcccode(fun),env)


LVAL xlmakebcode(V)
{
  LVAL code, jtab, lits, idx, env, codestr, val, cl;
  int i, n, c;
  unsigned char *s;

  code = xlgetarg();
  jtab = (vectorp(code)) ? xlgavector() : xlgetarg();
  lits = xlgavector();
  idx = xlgafixnum();
  env = xlgetarg();
  xllastarg();

  if (! (null(env) || vectorp(env))) xlbadtype(env);
  
  xlsave1(codestr);
  switch (ntype(code)) {
  case VECTOR:
    n = getsize(code);
    codestr = newstring(n);
    s = (unsigned char *) getstring(codestr);
    for (i = 0; i < n; i++) {
      cl = getelement(code,i);
      if (! fixp(cl)) xlerror("not a fixnum", cl);
      c = getfixnum(cl);
      if (c < 0 || c >= 256) xlerror("out of character range", cl);
      s[i] = c;
    }
    s[n] = 0;
    break;
  case FIXNUM:
    /**** modify for handling dynamically loaded modules */
    if (xlcurrentmodule < 0 || xlcurrentmodule >= xlnummodules)
      xlfail("bad module index");
    if (getfixnum(code) < 0
	|| getfixnum(code) >= xlmodules[xlcurrentmodule].numfunctions)
      xlfail("bad module function index");
    codestr = cons(cvfixnum((FIXTYPE) xlcurrentmodule), code);
    break;
  default:
    xlbadtype(code);
  }

  val = newbcode(codestr, jtab, lits, idx, env);
  xlpop();

  return(val);
}

LOCAL LVAL copy_bcode P1C(LVAL, fun)
{
  LVAL body, old_body, closure;

  xlsave1(body);
  old_body = getbcccode(fun);
  body = newbcode(getbccode(old_body),
		  getbcjtab(old_body),
		  getbclits(old_body),
		  getbcidx(old_body),
		  getbcenv(old_body));
  closure = newbcclosure(s_lambda, body);
  xlpop();
  return(closure);
}


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**         Register, Literals and Value Accessors and Modifiers            **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

/**** should be (LVAL *) */
/****
#define getlitval(n) (getelement(literals,n))
*/

/**** put this in bcloop, do same for function passed in register */
/****
#define getlitfun(n, fun) { \
  (fun) = getlitval(n); \
  while (! fboundp(fun)) xlfunbound(fun); \
  (fun) = getfunction(fun); \
}
*/

LVAL *vsbase = NULL;

#define vstop xlsp

/****
#define getregval(i) (vsbase[i])
#define setregval(i,v) (vsbase[i] = (v))
*/

#define set_result_or_regval(i,v) { \
  if (i) setregval(i,v); \
  else set_one_result(v); \
}

#define push_environment(e) { \
  LVAL env = (e); \
  if (env != NIL) { \
    int n = getsize(env); \
    if (xlsp + n > xlargstktop) xlargstkoverflow(); \
    MEMCPY(xlsp, &(getelement(env, 0)), sizeof(LVAL) * n); \
    xlsp += n; \
  } \
}


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                             Continuation Stack                          **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

/* The current byte code is stored in vsbase[-1]. This GC-protects the    */
/* code. This should work fine, since the code only changes when vsbase   */
/* does.                                                                  */

#define current_function vsbase[-1]

CONTINUATIONP xlcontinuation_stack, xlcsend;
CONTINUATIONP xlcstop;

#define save_current_continuation(vr) { \
  if (xlcstop >= xlcsend) xlabort("continuation stack overflow"); \
  xlcstop->base = vsbase; \
  xlcstop->top = vstop; \
  xlcstop->pe.pc = current_pc; \
  xlcstop->vreg = (vr); \
  xlcstop++; \
}

#define check_catch_continuation() \
  if (xlcstop >= xlcsend) xlabort("continuation stack overflow")

#define save_catch_continuation() xlcstop->pe.pc = current_pc
#define get_catch_continuation() xlcstop->pe.pc

#define set_saved_continuation_pc(npc) (xlcstop[-1].pe.pc = (npc))

/*****
#define DONE NIL
#define NO_VALUE -1
*/

#define restore_continuation(cont) { \
  xlcstop = (cont); \
  vsbase = xlcstop->base; \
  vstop = xlcstop->top; \
  current_pc = xlcstop->pe.pc; \
  if (xlcstop->vreg != NO_VALUE) setregval(xlcstop->vreg,get_one_result()); \
}

#define do_return(c) { \
  LVAL __c__ = (c); \
  if (__c__ == DONE) goto done; \
  xlcstop = xlcontinuation_stack + getfixnum(__c__); \
  vsbase = xlcstop->base; \
  vstop = xlcstop->top; \
  if (xlcstop->vreg != NO_VALUE) setregval(xlcstop->vreg,get_one_result()); \
  if (stringp(getbccode(getbcccode(current_function)))) { \
    current_pc = xlcstop->pe.pc; \
  } \
  else { \
    entry = xlcstop->pe.entry; \
    goto compiled_continuation; \
  } \
}

#define goto_target_pc(n) \
  (bcode_codevec(current_function)\
   + getfixnum(getelement(bcode_jumptable(current_function),n)))

#define BCSAMPLE 10 * SAMPLE

static int BCsample = BCSAMPLE;

#define do_goto(n) { \
  if (--BCsample < 0) { \
    BCsample = BCSAMPLE; \
    oscheck(); \
  } \
  current_pc = goto_target_pc(n); \
}

#define setup_call(fi) do_goto(fi)

#define test_do_goto(t,x,y) if (t) { do_goto(x); } else { do_goto(y); }

#define set_current_function() { \
  if (stringp(getbccode(getbcccode(current_function)))) { \
    setup_call(bcode_index(current_function)); \
  } \
  else { \
    entry = bcode_index(current_function); \
    goto compiled_continuation; \
  } \
}


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                     PC Definitions and Mutators                         **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

#define NULL_PC ((bytecode *) 0)
#define next_opcode() (*current_pc++)

#define bit(n) (1L<<(n))

/* adapted from CLISP */
#define get_operand(v) { \
  v = next_opcode(); /* read first byte */ \
  if (v & bit(7)) { /* bit 7 set? */ \
    v &= ~bit(7); /* unset bit 7 */ \
    v = v << 8;   /* shift by 8 */ \
    v |= next_opcode(); /* read next byte */ \
  } \
}


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                           Available Opcodes                             **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

enum OPCODES {
  COPY,
  GOTO,
  ARITH2_OP,
  ARITH_PRED2_OP,
  SET_SVREF_OP,
  SVREF_OP,
  SET_AREF1,
  AREF1,
  SET_ELT,
  ELT,
  SET_ONE_VALUE_OP,
  SET_ONE_VALUE_RETURN_OP,
  SET_VALUES_OP,
  SET_VALUES_RETURN_OP,
  SET_VALUES_LIST_OP,
  SET_VALUES_LIST_RETURN_OP,
  CAR,
  CDR,
  RPLACA_OP,
  RPLACD_OP,
  CONS_OP,
  TEST1_OP,
  SAVE_MVCALL,
  SAVE_CALL,
  MVCALL,
  CALL,
  SAVE_MVLCALL,
  SAVE_LCALL,
  MVLCALL,
  LCALL,
  SAVE_MVVCALL,
  SAVE_VCALL,
  MVVCALL,
  VCALL,
  MAKE_CELL,
  CELL_VALUE,
  SET_CELL_VALUE,
  TEST_ARITH2_OP,
  SYMVAL,
  SYMFUN,
  EQ_OP,
  EQL_OP,
  EQUAL_OP,
  CONSP_OP,
  ENDP_OP,
  SET_GET_OP,
  GET_OP,
  SET_NTH_OP,
  NTH_OP,
  SET_SYMVAL_OP,
  TEST2_OP,
  MAKE_CLOSURE_OP,
  CATCH_BLOCK_OP,
  THROW_RETURN_FROM_OP,
  CATCH_TAGBODY_OP,
  THROW_GO_OP,
  UNWIND_PROTECT_OP,
  RETURN_OP,
  GET_ONE_VALUE_OP,
  GET_VALUES_OP,
  CASE_OP,
  ARITH1_OP,
  SLOT_VALUE_OP,
  SET_SLOT_VALUE_OP,
  SUPPLIED_P_OP,
  CATCH_OP,
  THROW_OP,
  SET_AREF2_OP,
  AREF2_OP,
  DYNAMIC_BIND_OP,
  DYNAMIC_UNBIND_OP,
  CXR_OP,
  ERRSET_OP,
  NTH_VALUE_OP,
  MAKE_Y_CLOSURES_OP,
  PUSH_VALUES_OP,
  POP_VALUES_OP,
  INIT_OP,
  SET_CAR_OP,
  SET_CDR_OP,
  INIT_0_OP,
  STOP_OP,
  SWAP_OP,
  LDCONST_OP,
  NCASE_OP,
  MKCLOS_OP,
  SETCLOSDATA_OP,
  SETCLOSCODE_OP,
  LDNOTSUPP_OP,
  LDMVARGS_OP,
  NOT_OP,
  NEW_BLOCK_OP,
  NEW_TAGBODY_OP,
  NEW_GO_OP,
  NEW_CATCH_OP,
  NEW_ERRSET_OP,
  NEW_UNWIND_PROTECT_OP,
  GET_OPTARG_OP,
  MAKE_KEYARGS_OP,
  CHECK_LAST_KEYARG_OP,
  GET_KEYARG_OP,
  NEW_DYNAMIC_BIND_OP,
  NEW_DYNAMIC_UNBIND_OP,
  STRUCT_OP
};


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                        Opcode Support Functions                         **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

LOCAL bytecode *non_bcode_call P4C(bytecode *, pc, LVAL, fun, int, vi, int, mvals)
{
  bytecode *current_pc;
  LVAL *newfp, val, *oldbase;
  int argc, xi, i;
  
  current_pc = pc;
  if (mvals) argc = xlnumresults;
  else get_operand(argc);

#ifdef PROFILE
  if (subrp(fun)) {
    LVAL s = getvalue(s_profile_output);
    char *name = funtab[getoffset(fun)].fd_name;
    if (s != s_unbound && name != NULL) {
      xlputstr(s, name);
      xlterpri(s);
    }
  }
#endif /* PROFILE */

  /**** can't use fast subr dispatch with FASTMESS in objects.c */
  /**** check MULVALS */
  oldbase = vsbase;
  if (subrp(fun)) {
    LVAL *oldargv, *oldtop;
    int oldargc;

    oldargc = xlargc;
    oldargv = xlargv;
    oldtop = vstop;
    xlargc = argc;
    xlargv = vstop;
    
    if (xlsp + argc > xlargstktop) xlargstkoverflow();
    if (mvals)
      for (i = 0; i < argc; i++)
	*xlsp++ = xlresults[i];
    else
      for (i = 0; i < argc; i++) {
      get_operand(xi);
      *xlsp++ = getregval(xi);
    }
	
    val = (*getsubr(fun))();
    if (! mulvalp(fun)) {
      xlnumresults = 1;
      xlresults[0] = val;
    }

    xlargc = oldargc;
    xlargv = oldargv;
    vstop = oldtop;
  }
  else {
    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE) argc));
    if (xlsp + argc > xlargstktop) xlargstkoverflow();
    if (mvals)
      for (i = 0; i < argc; i++)
	*xlsp++ = xlresults[i];
    else
      for (i = 0; i < argc; i++) {
      get_operand(xi);
      *xlsp++ = getregval(xi);
    }
    xlfp = newfp;

    /* call the function */
    val = xlapply(argc);
  }
  vsbase = oldbase;

  if (vi != NO_VALUE) setregval(vi, val);
	      
  return(current_pc);
}

LOCAL bytecode *bccall_setup P5C(bytecode *, pc, LVAL, fun, LVAL, cont, int, tailp, int, mvals)
{
  bytecode *current_pc;
  int argc, i, xi;
  LVAL *base;

  current_pc = pc;

  if (mvals) argc = xlnumresults;
  else get_operand(argc);
  base = vsbase;
  vsbase = vstop;
  pusharg(cont);
  if (!null(fun)) push_environment(bcode_environment(fun));
  if (xlsp + argc > xlargstktop) xlargstkoverflow();
  if (mvals)
    for (i = 0; i < argc; i++)
      *xlsp++ = xlresults[i];
  else
    for (i = 0; i < argc; i++) {
      get_operand(xi);
      *xlsp++ = base[xi];
    }
  if (tailp && xlcstop[-1].base != base) {
    int n = vstop - vsbase;
    MEMMOVE(base - 1, vsbase - 1, sizeof(LVAL) * (n + 1));
    vstop = base + n;
    vsbase = base;
  }

  return(current_pc);
}


#define case_match_p(x, y) (consp(y) ? member_eql(x, y) : eql(x, y))

/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                         Local Utility Functions                         **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

LOCAL LVAL getf P3C(LVAL, list, LVAL, sym, LVAL, dflt)
{
  for (; consp(list) && consp(cdr(list)); list = cdr(cdr(list))) {
    if (sym == car(list)) return(car(cdr(list)));
  }
  return(dflt);
}
      
/**** duplicated from common.c */
LOCAL int is_member P2C(LVAL, x, LVAL, list)
{
  int result = FALSE;
  
  for (; ! result && consp(list); list = cdr(list))
    if (equal(x, car(list))) result = TRUE;
  return(result);
}

/**** used in CASE_OP */
LOCAL int member_eql P2C(LVAL, x, LVAL, y)
{
  for (; consp(y); y = cdr(y))
    if (eql(x, car(y))) return(TRUE);
  return(FALSE);
}

/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                           The Virtual Machine                           **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

/*
 * The following functions have been moved out of bcloop in order to
 * make bcloop easier to compile with optimization. The resulting code is
 * maybe a couple of percent slower than it would be with these things
 * inlined, but t should compile with a reasonable amount of optimization
 * on most systems.
 */

LOCAL bytecode *do_ARITH2_OP P1H(bytecode *);
LOCAL bytecode *do_ARITH_PRED2_OP P1H(bytecode *);
LOCAL bytecode *do_SVREF_OP P2H(bytecode *, int);
LOCAL bytecode *do_AREF1_OP P2H(bytecode *, int);
LOCAL bytecode *do_ELT_OP P2H(bytecode *, int);
LOCAL bytecode *do_GET_OP P2H(bytecode *, int);
LOCAL bytecode *do_NTH_OP P2H(bytecode *, int);
LOCAL bytecode *do_MAKE_CLOSURE_OP P1H(bytecode *);
LOCAL bytecode *do_ARITH1_OP P1H(bytecode *);
LOCAL bytecode *do_AREF2_OP P2H(bytecode *, int);
LOCAL bytecode *do_CXR_OP P1H(bytecode *);
LOCAL bytecode *do_MAKE_Y_CLOSURES_OP P1H(bytecode *);
LOCAL bytecode *do_MKCLOS_OP P1H(bytecode *);
LOCAL bytecode *do_INIT_OP P1H(bytecode *);

LOCAL bytecode *do_ARITH2_OP P1C(bytecode *, current_pc)
{
  int which, xi, yi, vi;
  LVAL xl, yl, val;
  
  which = next_opcode();
  get_operand(xi);
  get_operand(yi);
  get_operand(vi);
  xl = getregval(xi);
  yl = getregval(yi);
  
  switch (which) {
  case '+': val = xladd2(xl, yl); break;
  case '-': val = xlsub2(xl, yl); break;
  case '*': val = xlmul2(xl, yl); break;
  case '/': val = xldiv2(xl, yl); break;
  case 'm': val = xlmin2(xl, yl); break;
  case 'M': val = xlmax2(xl, yl); break;
  default:  val = NIL; /* to keep compiler happy */
  }
  
  set_result_or_regval(vi, val);
  
  return(current_pc);
}

LOCAL bytecode *do_ARITH_PRED2_OP P1C(bytecode *, current_pc)
{
  int which, xi, yi, vi;
  LVAL xl, yl, val;
  
  which = next_opcode();
  get_operand(xi);
  get_operand(yi);
  get_operand(vi);
  xl = getregval(xi);
  yl = getregval(yi);
  
  switch (which) {
  case '<': val = xllss2(xl, yl); break;
  case 'L': val = xlleq2(xl, yl); break;
  case '=': val = xlequ2(xl, yl); break;
  case '#': val = xlneq2(xl, yl); break;
  case 'G': val = xlgeq2(xl, yl); break;
  case '>': val = xlgtr2(xl, yl); break;
  default:  val = NIL; /* to keep compiler happy */
  }
  
  set_result_or_regval(vi, val);
  return(current_pc);
}

LOCAL bytecode *do_SVREF_OP P2C(bytecode *, current_pc, int, set)
{
  int ai, ii, vi, ri, i;
  LVAL al, il, vl;
  
  get_operand(ai);
  al = getregval(ai);
  get_operand(ii);
  il = getregval(ii);
  if (set) {
    get_operand(vi);
    vl = getregval(vi);
  }
  get_operand(ri);
  
  if (! vectorp(al)) xlbadtype(al);
  if (! fixp(il)) xlbadtype(il);
  
  i = getfixnum(il);
  if (i < 0 || i >= getsize(al))
    xlerror("index out of range", il);
  if (set) {
    setelement(al, i, vl);
    set_result_or_regval(ri, vl);
  }
  else
    set_result_or_regval(ri, getelement(al, i));
  
  return(current_pc);
}

LOCAL bytecode *do_AREF1_OP P2C(bytecode *, current_pc, int, set)
{
  int ai, ii, vi, ri, i;
  LVAL al, il, vl;
  
  get_operand(ai);
  al = getregval(ai);
  get_operand(ii);
  il = getregval(ii);
  if (set) {
    get_operand(vi);
    vl = getregval(vi);
  }
  get_operand(ri);
  
  if (darrayp(al)) al = getdarraydata(al);
  
  switch (ntype(al)) {
  case VECTOR:
  case STRING:
  case TVEC:
    if (! fixp(il)) xlbadtype(il);
    i = getfixnum(il);
    if (i < 0 || i >= gettvecsize(al))
      xlerror("index out of range", il);
    if (set) {
      settvecelement(al, i, vl);
      set_result_or_regval(ri, vl);
    }
    else
      set_result_or_regval(ri, gettvecelement(al, i));
    break;
  default:
    xlbadtype(al);
  }
  
  return(current_pc);
}

LOCAL bytecode *do_ELT_OP P2C(bytecode *, current_pc, int, set)
{
  int ai, ii, vi, ri, i;
  LVAL al, il, vl;
  
  get_operand(ai);
  al = getregval(ai);
  get_operand(ii);
  il = getregval(ii);
  if (set) {
    get_operand(vi);
    vl = getregval(vi);
  }
  get_operand(ri);
  
  if (! fixp(il)) xlbadtype(il);
  i = getfixnum(il);
  
  switch (ntype(al)) {
  case CONS:
    {
      for (; i > 0 && consp(al); --i)
	al = cdr(al);
      if((!consp(al)) || i < 0)
	xlerror("index out of range", il);
      if (set) {
	rplaca(al,vl);
	set_result_or_regval(ri, vl);
      }
      else
	set_result_or_regval(ri, car(al));
    }
    break;
  case VECTOR:
  case STRING:
  case TVEC:
    if (i < 0 || i >= gettvecsize(al))
      xlerror("index out of range", il);
    if (set) {
      settvecelement(al, i, vl);
      set_result_or_regval(ri, vl);
    }
    else
      set_result_or_regval(ri, gettvecelement(al, i));
    break;
  default:
    xlbadtype(al);
  }
  
  return(current_pc);
}

LOCAL bytecode *do_GET_OP P2C(bytecode *, current_pc, int, set)
{
  int xi, yi, vi, ri;
  LVAL xl, yl, vl;
  
  get_operand(xi);
  xl = getregval(xi);
  get_operand(yi);
  yl = getregval(yi);
  if (set) {
    get_operand(vi);
    vl = getregval(vi);
  }
  get_operand(ri);
  
  if (! symbolp(xl)) xlbadtype(xl);
  
  if (set) {
    xlputprop(xl, vl, yl);
    set_result_or_regval(ri, vl);
  }
  else
    set_result_or_regval(ri, xlgetprop(xl, yl));
  
  return(current_pc);
}

LOCAL bytecode *do_NTH_OP P2C(bytecode *, current_pc, int, set)
{
  int ni, xi, vi, ri, i;
  LVAL nl, xl, vl;
  
  get_operand(ni);
  nl = getregval(ni);
  get_operand(xi);
  xl = getregval(xi);
  if (set) {
    get_operand(vi);
    vl = getregval(vi);
  }
  get_operand(ri);
  
  
  if (! fixp(nl)) xlbadtype(nl);
  for (i = (int) getfixnum(nl);
       i > 0 && consp(xl);
       i--, xl = cdr(xl));
  
  if (set) {
    if (consp(xl)) rplaca(xl, vl);
  }
  else
    vl = (consp(xl)) ? car(xl) : NIL;
  set_result_or_regval(ri, vl);
  
  return(current_pc);
}

LOCAL bytecode *do_MAKE_CLOSURE_OP P1C(bytecode *, current_pc)
{
  int fi, ri, n, xi, i;
  LVAL env, closure;
  
  xlstkcheck(2);
  xlsave(env);
  xlsave(closure);
  get_operand(fi);
  get_operand(ri);
  get_operand(n);
  
  env = newvector(n);
  for (i = 0; i < n; i++) {
    get_operand(xi);
    setelement(env, i, getregval(xi));
  }
  
  closure = copy_bcode(current_function);
  set_bcode_environment(closure, env);
  set_bcode_index(closure, fi);
  set_result_or_regval(ri, closure);
  xlpopn(2);

  return(current_pc);
}

LOCAL bytecode *do_ARITH1_OP P1C(bytecode *, current_pc)
{
  int which, xi, ri;
  LVAL xl, val;
  
  which = next_opcode();
  get_operand(xi);
  get_operand(ri);
  xl = getregval(xi);
  
  switch(which) {
  case 'p': val = xladd1(xl); break;
  case 'm': val = xlsub1(xl); break;
  case '-': val = xlsub2(cvfixnum((FIXTYPE) 0), xl); break;
  case '/': val = xldiv2(cvfixnum((FIXTYPE) 1), xl); break;
  default:  val = NIL; /* to keep compiler happy */
  }
  
  set_result_or_regval(ri, val);
  
  return(current_pc);
}

LOCAL bytecode *do_AREF2_OP P2C(bytecode *, current_pc, int, set)
{
  int ai, ii, ji, vi, ri, i, j, k;
  LVAL al, il, jl, vl, data, dims;
  
  get_operand(ai);
  al = getregval(ai);
  get_operand(ii);
  il = getregval(ii);
  get_operand(ji);
  jl = getregval(ji);
  if (set) {
    get_operand(vi);
    vl = getregval(vi);
  }
  get_operand(ri);
  
  if (! darrayp(al)) xlbadtype(al);
  if (! fixp(il)) xlbadtype(il);
  if (! fixp(jl)) xlbadtype(jl);
  
  i = getfixnum(il);
  j = getfixnum(jl);
  data = getdarraydata(al);
  dims = getdarraydim(al);
  if (getsize(dims) != 2) xlbadtype(al);
  
  k = (i * getfixnum(getelement(dims, 1))) + j;
  if (k < 0 || k >= gettvecsize(data)) xlfail("index out of range");
  
  if (set) {
    settvecelement(data, k, vl);
    set_result_or_regval(ri, vl);
  }
  else
    set_result_or_regval(ri, gettvecelement(data, k));
  
  return(current_pc);
}

LOCAL bytecode *do_CXR_OP P1C(bytecode *, current_pc)
{
  int xi, vi;
  int n, x;
  LVAL a;
  
  n = next_opcode();
  x = next_opcode();
  get_operand(xi);
  get_operand(vi);
  
  a = getregval(xi);
  
  for (; n > 0; n--, x >>= 1) {
    if (null(a)) break;
    else if consp(a)
      a = (x & 1) ? car(a) : cdr(a);
    else xlbadtype(a);
  }
  
  set_result_or_regval(vi,a);
  
  return(current_pc);
}

LOCAL bytecode *do_MAKE_Y_CLOSURES_OP P1C(bytecode *, current_pc)
{
  int fi, ri, n, nv, xi, i;
  LVAL env, closure;
  
  xlstkcheck(2);
  xlsave(env);
  xlsave(closure);
  
  get_operand(n);
  get_operand(nv);
  env = newvector(nv);
  for (i = 0; i < n; i++) {
    get_operand(fi);
    get_operand(ri);
    closure = copy_bcode(current_function);
    set_bcode_environment(closure, env);
    set_bcode_index(closure, fi);
    setregval(ri, closure);
  }
  for (i = 0; i < nv; i++) {
    get_operand(xi);
    setelement(env, i, getregval(xi));
  }
  xlpopn(2);

  return(current_pc);
}

LOCAL bytecode *do_MKCLOS_OP P1C(bytecode *, current_pc)
{
  int ri, n, nv, i;
  LVAL env, closure;
  
  xlstkcheck(2);
  xlsave(env);
  xlsave(closure);
  
  get_operand(nv);
  get_operand(n);
  env = newvector(nv);
  for (i = 0; i < n; i++) {
    get_operand(ri);
    closure = copy_bcode(current_function);
    set_bcode_environment(closure, env);
    setregval(ri, closure);
  }
  xlpopn(2);

  return(current_pc);
}

/**** see if this can be cleaned up */
LOCAL bytecode *do_INIT_OP P1C(bytecode *, current_pc)
{
  int nc, nr, xi, argc;
  LVAL literals = bcode_literals(current_function);

  argc = vstop-vsbase;
  
  switch (next_opcode()) {
  case 0: /* This case should be handled handled separately by INIT_OP_0 */
    {
      int nreq;
      
      get_operand(nreq);
      if (nreq != argc) {
	if (nreq > argc)
	  xltoofew();
	else xltoomany();
      }
    }
    break;
  case 1:
    {
      int nreq, nopt, oi, i;
      LVAL odef;
      
      get_operand(nreq);
      get_operand(nopt);
      get_operand(oi);
      odef = getlitval(oi);
      
      if (argc < nreq) xltoofew();
      if (nreq + nopt < argc) xltoomany();
      
      for (i = argc - nreq; i < nopt; i++)
	pusharg(getelement(odef,i));
    }
    break;
  case 2:
    {
      int nreq, nopt, nro, oi, i;
      LVAL odef, rest_arg, last;
      
      get_operand(nreq);
      get_operand(nopt);
      nro = nreq + nopt;
      
      if (argc < nreq) xltoofew();
      
      if (nopt != 0)  {
	get_operand(oi);
	odef = getlitval(oi);
	for (i = argc - nreq; i < nopt; i++)
	  pusharg(getelement(odef,i));
      }
      
      if (argc > nro) {
	xlsave1(rest_arg);
	rest_arg = consa(vsbase[nro]);
	for (i = nro + 1, last = rest_arg; i < argc; i++) {
	  rplacd(last, consa(vsbase[i]));
	  last = cdr(last);
	}
	xlpop();
      }
      else rest_arg = NIL;
      vstop = vsbase + nro;
      pusharg(rest_arg);
    }
    break;
  default:
    {
      int nreq, nopt, nro, oi, rest, aok, ksi, kdi, i;
      LVAL odef, kdef, ksym, rest_arg, last, ks, kd, args, key;
      
      get_operand(nreq);
      get_operand(nopt);
      nro = nreq + nopt;
      
      if (argc < nreq) xltoofew();
      
      if (nopt != 0)  {
	get_operand(oi);
	odef = getlitval(oi);
	for (i = argc - nreq; i < nopt; i++)
	  pusharg(getelement(odef,i));
      }
      
      rest = next_opcode();
      aok = next_opcode();
      get_operand(ksi);
      get_operand(kdi);
      ksym = getlitval(ksi);
      kdef = getlitval(kdi);
      
      xlsave1(rest_arg);
      
      if (argc > nro) {
	rest_arg = consa(vsbase[nro]);
	for (i = nro + 1, last = rest_arg; i < argc; i++) {
	  rplacd(last, consa(vsbase[i]));
	  last = cdr(last);
	}
      }
      else rest_arg = NIL;
      vstop = vsbase + nro;
      if (rest) pusharg(rest_arg);
      
      if (!null(getvalue(s_strict_keywords))
	  && null(getf(rest_arg,
		       k_allow_other_keys,
		       (aok) ? s_true : NIL))) {
	for (args = rest_arg; consp(args); args = cdr(cdr(args))) {
	  if (!consp(cdr(args)))
	    xlfail("keyword/value args must be even");
	  key = car(args);
	  if (! symbolp(key))
	    xlerror("not a valid keyword", key);
	  if (! is_member(key, ksym))
	    xlerror("keyword is not supported", key);
	}
      }
      
      for (ks = ksym, kd = kdef; consp(ks) && consp(kd);
	   ks = cdr(ks), kd = cdr(kd))
	pusharg(getf(rest_arg, car(ks), car(kd)));
      
      xlpop();
    }
    break;
  }
  
  /* load any constanst used from the literals */
  get_operand(nc);
  while (nc-- > 0) {
    get_operand(xi);
    pusharg(getlitval(xi));
  }
  
  /* push additional space on top and initialize to nil */  
  get_operand(nr);
  if (xlsp + nr > xlargstktop) xlargstkoverflow();
  while (nr-- > 0) *xlsp++ = NIL;
  
  return(current_pc);
}

static LVAL bc_get_keyarg P4C(LVAL, args, LVAL, key, LVAL, dflt, int *, pfound)
{
  LVAL last = args;
  for (args = cdr(args); consp(args); last = cdr(args), args = cdr(last)) {
    if (! consp(cdr(args))) xlfail("keyword value missing");
    if (car(args) == key) {
      rplacd(last, cdr(cdr(args)));
      if (pfound != NULL) *pfound = TRUE;
      return car(cdr(args));
    }
  }
  if (pfound != NULL) *pfound = FALSE;
  return dflt;
}

/* bcloop - the interpreter main loop */
LOCAL VOID bcloop P1C(int, entry)
{
  bytecode *current_pc;
  LVAL fun;
  int mvals;

#ifdef STSZ         /* This function is a good candidate for stack ov */
  stchck();
#endif

  /**** this must only happen for byte code functions */
  if (stringp(getbccode(getbcccode(current_function)))) {
    do_goto(entry);
    goto byte_code_continuation;
  }
  else goto compiled_continuation;

  /* main loop */
 byte_code_continuation:
  while (TRUE) {
    switch (next_opcode()) {
    case COPY:
      {
	int xi, yi;
	    
	get_operand(xi);
	get_operand(yi);
	set_result_or_regval(yi, getregval(xi));
      }
      break;
    case GOTO:
      {
	int n;
	get_operand(n);
	if (--xlsample <= 0) {
	  xlsample = SAMPLE;
	  oscheck();
        }
	do_goto(n);
      }
      break;
    case ARITH2_OP:
      current_pc = do_ARITH2_OP(current_pc);
      break;
    case ARITH_PRED2_OP:
      current_pc = do_ARITH_PRED2_OP(current_pc);
      break;
    case SET_SVREF_OP:
      current_pc = do_SVREF_OP(current_pc, TRUE);
      break;
    case SVREF_OP:
      current_pc = do_SVREF_OP(current_pc, FALSE);
      break;
    case SET_AREF1:
      current_pc = do_AREF1_OP(current_pc, TRUE);
      break;
    case AREF1:
      current_pc = do_AREF1_OP(current_pc, FALSE);
      break;
    case SET_ELT:
      current_pc = do_ELT_OP(current_pc, TRUE);
      break;
    case ELT:
      current_pc = do_ELT_OP(current_pc, FALSE);
      break;
    case SET_ONE_VALUE_OP:
      {
	int vi;
	
	get_operand(vi);
	set_one_result(getregval(vi));
      }
      break;
    case SET_ONE_VALUE_RETURN_OP:
      {
	int ci, vi;
	
	get_operand(ci);
	get_operand(vi);
	set_one_result(getregval(vi));
	do_return(getregval(ci));
      }
      break;
    case SET_VALUES_OP:
      {
	int n, i, vi;
	get_operand(n);
	/***** check ought to be in code generation */
	if (n > MULVALLIMIT) xlfail("too many results");
	for (i = 0; i < n; i++) {
	  get_operand(vi);
	  xlresults[i] = getregval(vi);
	}
	xlnumresults = n;
      }
      break;
    case SET_VALUES_RETURN_OP:
      {
	int ci, n, i, vi;
	get_operand(ci);
	get_operand(n);
	/***** check ought to be in code generation */
	if (n > MULVALLIMIT) xlfail("too many results");
	for (i = 0; i < n; i++) {
	  get_operand(vi);
	  xlresults[i] = getregval(vi);
	}
	xlnumresults = n;
	do_return(getregval(ci));
      }
      break;
    case SET_VALUES_LIST_OP:
      {
	int vi, i;
	LVAL v;
	
	get_operand(vi);
	
	for (i = 0, v = getregval(vi); consp(v); i++, v = cdr(v)) {
	  if (i >= MULVALLIMIT) xlfail("too many results");
	  xlresults[i] = car(v);
	}
	xlnumresults = i;
      }
      break;
    case SET_VALUES_LIST_RETURN_OP:
      {
	int ci, vi, i;
	LVAL v;
	
	get_operand(ci);
	get_operand(vi);
	
	for (i = 0, v = getregval(vi); consp(v); i++, v = cdr(v)) {
	  if (i >= MULVALLIMIT) xlfail("too many results");
	  xlresults[i] = car(v);
	}
	xlnumresults = i;
	do_return(getregval(ci));
      }
      break;
    case CAR:
      {
	int xi, vi;
	LVAL xl;
	
	get_operand(xi);
	get_operand(vi);
	xl = getregval(xi);
	if (! null(xl)) {
	  if (consp(xl)) xl = car(xl);
	  else xlbadtype(xl);
	}
	set_result_or_regval(vi, xl);
      }
      break;
    case CDR:
      {
	int xi, vi;
	LVAL xl;
	
	get_operand(xi);
	get_operand(vi);
	xl = getregval(xi);
	if (! null(xl)) {
	  if (consp(xl)) xl = cdr(xl);
	  else xlbadtype(xl);
	}
	set_result_or_regval(vi, xl);
      }
      break;
    case RPLACA_OP:
      {
	int xi, vi, ri;
	LVAL xl, vl;
	
	get_operand(xi);
	get_operand(vi);
	get_operand(ri);
	xl = getregval(xi);
	vl = getregval(vi);
	if (! null(xl)) {
	  if (consp(xl)) rplaca(xl, vl);
	  else xlbadtype(xl);
	}
	set_result_or_regval(ri, xl);
      }
      break;
    case RPLACD_OP:
      {
	int xi, vi, ri;
	LVAL xl, vl;
	
	get_operand(xi);
	get_operand(vi);
	get_operand(ri);
	xl = getregval(xi);
	vl = getregval(vi);
	if (! null(xl)) {
	  if (consp(xl)) rplacd(xl, vl);
	  else xlbadtype(xl);
	}
	set_result_or_regval(ri, xl);
      }
      break;
    case CONS_OP:
      {
	int xi, yi, vi;
	
	get_operand(xi);
	get_operand(yi);
	get_operand(vi);
	set_result_or_regval(vi,cons(getregval(xi),getregval(yi)));
      }
      break;
    case TEST1_OP:
      {
	int which, xi, yi, vi;
	int tval = FALSE; /* initialized to keep compiler happy */
	LVAL v;
	
	which = next_opcode();
	get_operand(xi);
	get_operand(yi);
	get_operand(vi);
	v = getregval(vi);
	
	switch (which) {
	case 0: tval = (v != NIL) ? TRUE : FALSE; break;
	case 1: tval = consp(v); break;
	case 2: tval = (v != s_not_supplied) ? TRUE : FALSE; break;
	case 3:
	  if (consp(v)) tval = FALSE;
	  else if (null(v)) tval = TRUE;
	  else xlbadtype(v);
	  break;
	}
	
	test_do_goto(tval, xi, yi);
      }
      break;
      /*
       * Function call opcodes use a fair number of goto's in order to
       * maximize code reuse. It isn't pretty, but it keeps things small
       * and prevents small variations from kreeping in.
       */
    case SAVE_MVCALL:
      mvals = TRUE;
      goto save_call;
    case SAVE_CALL:
      mvals = FALSE;
    save_call:
      {
	int fi;
	LVAL literals = bcode_literals(current_function);

	get_operand(fi);
	fun = getlitval(fi);
	if (symbolp(fun)) {
	  while (! fboundp(fun)) xlfunbound(fun);
	  fun = getfunction(fun);
	}
      }
    save_call_body:
      {
	int vi;
	LVAL cont;

	get_operand(vi);
	if (vi == 0) vi = NO_VALUE;
	
	if (bcclosurep(fun)) {
	  cont = cvfixnum((FIXTYPE) (xlcstop - xlcontinuation_stack));
	  save_current_continuation(vi);
	  pusharg(fun);
	  current_pc = bccall_setup(current_pc, fun, cont, FALSE, mvals);
	  set_saved_continuation_pc(current_pc);
	  set_current_function();
	}
	else current_pc = non_bcode_call(current_pc, fun, vi, mvals);
      }
      break;
    case MVCALL:
      mvals = TRUE;
      goto call;
    case CALL:
      mvals = FALSE;
    call:
      {
	int fi;
	LVAL literals = bcode_literals(current_function);
	
	get_operand(fi);
	fun = getlitval(fi);
	if (symbolp(fun)) {
	  while (! fboundp(fun)) xlfunbound(fun);
	  fun = getfunction(fun);
	}
      }
    call_body:
      {
	int ci;
	LVAL cont;
	
	get_operand(ci);
	cont = getregval(ci);
	
	if (bcclosurep(fun)) {
	  pusharg(fun);
	  current_pc = bccall_setup(current_pc, fun, cont, TRUE, mvals);
	  set_current_function();
	}
	else {
	  current_pc = non_bcode_call(current_pc, fun, NO_VALUE, mvals);
	  do_return(cont);
	}
      }
      break;
    case SAVE_MVLCALL:
      mvals = TRUE;
      goto save_lcall;
    case SAVE_LCALL:
      mvals = FALSE;
    save_lcall:
      {
	int fi, vi;
	LVAL cont;
	
	get_operand(fi);
	cont = cvfixnum((FIXTYPE) (xlcstop - xlcontinuation_stack));
	
	get_operand(vi);
	if (vi == 0) vi = NO_VALUE;
	
	save_current_continuation(vi);
	pusharg(current_function);
	current_pc = bccall_setup(current_pc, NIL, cont, FALSE, mvals);
	set_saved_continuation_pc(current_pc);
	setup_call(fi);
      }
      break;
    case MVLCALL:
      mvals = TRUE;
      goto lcall;
    case LCALL:
      mvals = FALSE;
    lcall:
      {
	int fi, ci;
	LVAL cont;

	get_operand(fi);
	get_operand(ci);
	cont = getregval(ci);
	
	pusharg(current_function);
	current_pc = bccall_setup(current_pc, NIL, cont, TRUE, mvals);
	setup_call(fi);
      }
      break;
    case SAVE_MVVCALL:
      mvals = TRUE;
      goto save_vcall;
    case SAVE_VCALL:
      mvals = FALSE;
    save_vcall:
      {
	int fi;
	
	get_operand(fi);
	fun = getregval(fi);
	if (symbolp(fun)) {
	  while (! fboundp(fun)) xlfunbound(fun);
	  fun = getfunction(fun);
	}
      }
      goto save_call_body;
    case MVVCALL:
      mvals = TRUE;
      goto vcall;
    case VCALL:
      mvals = FALSE;
    vcall:
      {
	int fi;
	
	get_operand(fi);
	fun = getregval(fi);
	if (symbolp(fun)) {
	  while (! fboundp(fun)) xlfunbound(fun);
	  fun = getfunction(fun);
	}
      }
      goto call_body;
    case MAKE_CELL:
      {
	int vi, ci;
	
	get_operand(vi);
	get_operand(ci);
	set_result_or_regval(ci, consa(getregval(vi)));
      }
      break;
    case CELL_VALUE:
      {
	int ci, vi;
	
	get_operand(ci);
	get_operand(vi);
	set_result_or_regval(vi, car(getregval(ci))); /**** NO error checking***/
      }
      break;
    case SET_CELL_VALUE:
      {
	int ci, vi, ri;
	LVAL cl, vl;
	
	get_operand(ci);
	get_operand(vi);
	get_operand(ri);
	cl = getregval(ci);
	vl = getregval(vi);
	rplaca(cl, vl); /**** NO error checking***/
	set_result_or_regval(ri, vl);
      }
      break;
    case TEST_ARITH2_OP:
      {
	int which, xi, yi, ui, vi;
	LVAL ul, vl;
	
	which = next_opcode();
	get_operand(xi);
	get_operand(yi);
	get_operand(ui);
	get_operand(vi);
	ul = getregval(ui);
	vl = getregval(vi);
	
	test_do_goto(num_cmp2(which, ul, vl), xi, yi);
      }
      break;
    case SYMVAL:
      {
	int si, vi;
	LVAL sl;
	LVAL literals = bcode_literals(current_function);
	
	get_operand(si);
	get_operand(vi);
	sl = getlitval(si);
	
	if (! symbolp(sl)) xlbadtype(sl);
#ifdef XLISP_STAT
	if (! boundp(sl)) xlunbound(sl);
	set_result_or_regval(vi, getvalue(sl));
#else
	{
	  LVAL pair, val;
	  pair = getvalue(s_self);
	  if (! (consp(pair) &&
		 objectp(car(pair)) &&
		 xlobgetvalue(pair,sl,&val))) {
	    if (! boundp(sl)) xlunbound(sl);
	    val = getvalue(sl);
	  }
	  set_result_or_regval(vi, val);
	}
#endif /* XLISP_STAT */
      }
      break;
    case SYMFUN:
      {
	int si, vi;
	LVAL sl;
	LVAL literals = bcode_literals(current_function);
	
	get_operand(si);
	get_operand(vi);
	sl = getlitval(si);
	
	if (! symbolp(sl)) xlbadtype(sl);
	while (! fboundp(sl)) xlfunbound(sl);
	set_result_or_regval(vi, getfunction(sl));
      }
      break;
    case EQ_OP:
      {
	int xi, yi, vi;
	LVAL val;
	
	get_operand(xi);
	get_operand(yi);
	get_operand(vi);
	val = (getregval(xi) == getregval(yi)) ? s_true : NIL;
	set_result_or_regval(vi, val);
      }
      break;
    case EQL_OP:
      {
	int xi, yi, vi;
	LVAL val;
	
	get_operand(xi);
	get_operand(yi);
	get_operand(vi);
	val = eql(getregval(xi), getregval(yi)) ? s_true : NIL;
	set_result_or_regval(vi, val);
      }
      break;
    case EQUAL_OP:
      {
	int xi, yi, vi;
	LVAL val;
	
	get_operand(xi);
	get_operand(yi);
	get_operand(vi);
	val = equal(getregval(xi), getregval(yi)) ? s_true : NIL;
	set_result_or_regval(vi, val);
      }
      break;
    case CONSP_OP:
      {
	int xi, vi;
	get_operand(xi);
	get_operand(vi);
	set_result_or_regval(vi, (consp(getregval(xi))) ? s_true : NIL);
      }
      break;
    case ENDP_OP:
      {
	int xi, vi;
	LVAL xl, val = NIL;
	
	get_operand(xi);
	get_operand(vi);
	xl = getregval(xi);
	if (null(xl)) val = s_true;
	else if (! consp(xl)) xlbadtype(xl);
	set_result_or_regval(vi, val);
      }
      break;
    case SET_GET_OP:
      current_pc = do_GET_OP(current_pc, TRUE);
      break;
    case GET_OP:
      current_pc = do_GET_OP(current_pc, FALSE);
      break;	      
    case SET_NTH_OP:
      current_pc = do_NTH_OP(current_pc, TRUE);
      break;
    case NTH_OP:
      current_pc = do_NTH_OP(current_pc, FALSE);
      break;
    case SET_SYMVAL_OP:
      {
	int si, vi, ri;
	LVAL sl, vl;
	LVAL literals = bcode_literals(current_function);
	
	get_operand(si);
	get_operand(vi);
	get_operand(ri);
	sl = getlitval(si);
	vl = getregval(vi);
	
	if (! symbolp(sl)) xlbadtype(sl);
#ifdef XLISP_STAT
	setvalue(sl, vl);
#else
	{
	  LVAL pair;
	  pair = getvalue(s_self);
	  if (! (consp(pair) &&
		 objectp(car(pair)) &&
		 xlobsetvalue(pair,sl,vl)))
	    setvalue(sl, vl);
	}
#endif /* XLISP_STAT */
	set_result_or_regval(ri, vl);
      }
      break;
    case TEST2_OP:
      {
	int which, xi, yi, ui, vi;
	int tval = FALSE; /* initialized to keep compiler happy */
	LVAL u, v;
	
	which = next_opcode();
	get_operand(xi);
	get_operand(yi);
	get_operand(ui);
	get_operand(vi);
	u = getregval(ui);
	v = getregval(vi);
	
	switch (which) {
	case 0: tval = (u == v) ? TRUE : FALSE; break;
	case 1: tval = eql(u, v); break;
	case 2: tval = equal(u, v); break;
	}
	
	test_do_goto(tval, xi, yi);
      }
      break;
    case MAKE_CLOSURE_OP:
      current_pc = do_MAKE_CLOSURE_OP(current_pc);
      break;
    case CATCH_BLOCK_OP:
      {
	int ni, li, ci, ti, sts;
	LVAL tag;
	CONTEXT cntxt;
	CONTINUATIONP old_cstop = xlcstop;
	struct { int li; bytecode *cpc; } state;

	get_operand(ni);
	get_operand(li);
	get_operand(ci);
	get_operand(ti);
	
	xlsave1(tag);
	tag = consa(getregval(ni));
	setregval(ci, DONE);
	setregval(ti, tag);
	save_current_continuation(NO_VALUE);
	
	xlbegin(&cntxt, CF_THROW, tag);
	state.li = li; state.cpc = current_pc;
	sts = XL_SETJMP(cntxt.c_jmpbuf);
	li = state.li; current_pc = state.cpc;
	if (! sts)
	  bcloop(li);
	restore_continuation(old_cstop);
	xlend(&cntxt);
	
	xlpop();
      }
      break;
    case THROW_RETURN_FROM_OP:
      {
	int ri;
	get_operand(ri);
	xlthrow(getregval(ri), NIL);
      }
      break;
    case CATCH_TAGBODY_OP:
      {
	int li, ci, ti, ri, sts;
	LVAL tag;
	CONTEXT cntxt;
	CONTINUATIONP old_cstop = xlcstop;
	struct { int li, ci, ti; bytecode *cpc; } state;

	get_operand(li);
	get_operand(ci);
	get_operand(ti);
	get_operand(ri);
	
	xlsave1(tag);
	tag = consa(NIL);
	
	xlbegin(&cntxt, CF_THROW, tag);
	
	/* check for a go */
	state.li = li; state.ci = ci; state.ti = ti; state.cpc = current_pc;
	sts = XL_SETJMP(cntxt.c_jmpbuf);
	li = state.li; ci = state.ci; ti = state.ti; current_pc = state.cpc;
	if (sts) {
	  restore_continuation(old_cstop);
	  if (fixp(xlvalue)) li = getfixnum(xlvalue);
	  else xlerror("bad go target", xlvalue);
	}
	
	setregval(ci, DONE);
	setregval(ti, tag);
	save_current_continuation(NO_VALUE);
	bcloop(li);
	restore_continuation(old_cstop);
	xlend(&cntxt);
	
	xlpop();
      }
      break;
    case THROW_GO_OP:
      {
	int ri, li;
	LVAL ll;
	get_operand(ri);
	get_operand(li);
	ll = cvfixnum((FIXTYPE) li);
	pusharg(ll); /* protect from GC */
	xlthrow(getregval(ri), ll);
      }
      break;
    case UNWIND_PROTECT_OP:
      /***** ought to save values on stack, not cons them */
      /**** state.val need to be protected? */
      /**** is the value really needed? */
      {
	int l1i, l2i, pi, ui, sts;
	CONTINUATIONP old_cstop = xlcstop;
	CONTEXT cntxt;
	struct {
	  CONTEXT *target;
	  int mask, l1i, l2i, ui;
	  LVAL val;
	  bytecode *cpc;
	} state;
	
	get_operand(l1i);
	get_operand(l2i);
	get_operand(pi);
	get_operand(ui);
	
	setregval(pi, DONE);
	save_current_continuation(NO_VALUE);
	
	xlbegin(&cntxt,CF_UNWIND,NIL);
	state.l1i = l1i; state.l2i = l2i; state.ui = ui;
	state.cpc = current_pc;
	sts = XL_SETJMP(cntxt.c_jmpbuf);
	l1i = state.l1i; l2i = state.l2i; ui = state.ui;
	current_pc = state.cpc;
	if (sts) {
	  state.target = xltarget;
	  state.mask = xlmask;
	  state.val = xlvalue;
	}
	else
	  bcloop(l1i);

	xlend(&cntxt);
	{
	  LVAL val;
	  int nvals, i;
	  
	  restore_continuation(old_cstop);
	  
	  setregval(ui, DONE);
	  save_current_continuation(NO_VALUE);
	  
	  nvals = xlnumresults;
	  for (i = 0; i < nvals; i++) {
	    val = get_nth_result(i);
	    pusharg(val);
	  }
	  
	  bcloop(l2i);
	  
	  restore_continuation(old_cstop);
	  
	  for (i = 0; i < nvals; i++)
	    set_nth_result(i, vstop[i]);
	  xlnumresults = nvals;
	}
	
	if (sts)
	  xljump(state.target,state.mask,state.val);
	
      }
      break;
    case RETURN_OP:
      {
	int ci;
	get_operand(ci);
	do_return(getregval(ci));
      }
      break;
    case GET_ONE_VALUE_OP:
      {
	int ri;
	get_operand(ri);
	setregval(ri, get_one_result());
      }
      break;
    case GET_VALUES_OP:
      {
	int n, ri, i;
	get_operand(n);
	for (i = 0; i < n; i++) {
	  get_operand(ri);
	  setregval(ri, get_nth_result(i));
	}
      }
      break;
    case CASE_OP:
      {
	int ci, chi, li, i, n;
	LVAL cl, chl, cs;
	
	get_operand(ci);
	get_operand(chi);
	cl = getregval(ci);
	chl = getregval(chi);
	n = getsize(chl);
	
	li = 0; /* to keep compiler happy */
	for (i = 0; i < n; i++) {
	  get_operand(li);
	  cs = getelement(chl, i);
	  if (case_match_p(cl, cs)) break;
	}
	if (i == n) get_operand(li);
	
	do_goto(li);
      }
      break;
    case ARITH1_OP:
      current_pc = do_ARITH1_OP(current_pc);
      break;
    case SLOT_VALUE_OP:
      {
	int xi, ri;
	LVAL xl, self;
	
	get_operand(xi);
	get_operand(ri);
	xl = getregval(xi);
	self = getvalue(s_self);
	set_result_or_regval(ri, slot_value(self, xl));
      }
      break;
    case SET_SLOT_VALUE_OP:
      {
	int xi, vi, ri;
	LVAL xl, vl, self;
	
	get_operand(xi);
	get_operand(vi);
	get_operand(ri);
	xl = getregval(xi);
	vl = getregval(vi);
	self = getvalue(s_self);
	set_slot_value(self, xl, vl);
	set_result_or_regval(ri, vl);
      }
      break;
    case SUPPLIED_P_OP:
      {
	int xi, vi;
	
	get_operand(xi);
	get_operand(vi);
	set_result_or_regval(vi, (getregval(xi) != s_not_supplied) ? s_true : NIL);
      }
      break;
    case CATCH_OP:
      {
	int ti, li, ci, sts;
	LVAL tag;
	CONTEXT cntxt;
	CONTINUATIONP old_cstop = xlcstop;
	struct { int li; bytecode *cpc; } state;

	get_operand(ti);
	get_operand(li);
	get_operand(ci);
	
	tag = getregval(ti);
	setregval(ci, DONE);
	save_current_continuation(NO_VALUE);
	
	xlbegin(&cntxt, CF_THROW, tag);
	state.li = li; state.cpc = current_pc;
	sts = XL_SETJMP(cntxt.c_jmpbuf);
	li = state.li; current_pc = state.cpc;
	if (! sts)
	  bcloop(li);
	restore_continuation(old_cstop);
	xlend(&cntxt);
      }
      break;
    case THROW_OP:
      {
	int ti;
	get_operand(ti);
	xlthrow(getregval(ti), get_one_result());
      }
      break;
    case SET_AREF2_OP:
      current_pc = do_AREF2_OP(current_pc, TRUE);
      break;
    case AREF2_OP:
      current_pc = do_AREF2_OP(current_pc, FALSE);
      break;
    case DYNAMIC_BIND_OP:
      {
	int si, vi;
	LVAL nexts, nextv;
	
	get_operand(si);
	get_operand(vi);
	
	pusharg(xldenv);
	
	if (getregval(vi) == s_true) {
	  for (nexts = getregval(si); consp(nexts); nexts = cdr(nexts))
	    xldbind(car(nexts), getvalue(car(nexts)));
	}
	else {
	  for (nexts = getregval(si), nextv = getregval(vi);
	       consp(nexts) && consp(nextv);
	       nexts = cdr(nexts), nextv = cdr(nextv))
	    xldbind(car(nexts), car(nextv));
	  for (; consp(nexts); nexts = cdr(nexts))
	    xldbind(car(nexts), s_unbound);
	}
      }
      break;
    case DYNAMIC_UNBIND_OP:
      {
	LVAL olddenv = *--vstop;
	xlunbind(olddenv);
      }
      break;
    case CXR_OP:
      current_pc = do_CXR_OP(current_pc);
      break;
    case ERRSET_OP:
      {
	int li, ci, fi, sts;
	LVAL flag, val;
	CONTEXT cntxt;
	CONTINUATIONP old_cstop = xlcstop;
	struct { int li; bytecode *cpc; } state;

	get_operand(li);
	get_operand(ci);
	get_operand(fi);
	
	flag = getregval(fi);
	
	setregval(ci, DONE);
	save_current_continuation(NO_VALUE);
	
	xlbegin(&cntxt, CF_ERROR, flag);
	state.li = li; state.cpc = current_pc;
	sts = XL_SETJMP(cntxt.c_jmpbuf);
	li = state.li; current_pc = state.cpc;
	if (sts) {
	  set_one_result(NIL);
	}
	else {
	  bcloop(li);
	  val = get_one_result(); /* has to be done before set */
	  set_one_result(consa(val));
	}
	restore_continuation(old_cstop);
	xlend(&cntxt);
      }
      break;
    case NTH_VALUE_OP:
      {
	int ni, vi, n;
	LVAL nl, vl;
	
	get_operand(ni);
	get_operand(vi);
	nl = getregval(ni);
	
	if (! fixp(nl)) xlbadtype(nl);
	n = getfixnum(nl);
	vl = (n < 0) ? NIL : get_nth_result(n);
	set_result_or_regval(vi,vl);
      }
      break;
    case MAKE_Y_CLOSURES_OP:
      current_pc = do_MAKE_Y_CLOSURES_OP(current_pc);
      break;
    case PUSH_VALUES_OP:
      {
	int i, vr;
	LVAL nl;
	
	get_operand(vr);
	nl = cvfixnum((FIXTYPE) xlnumresults);
	setregval(vr, nl);
	for (i = 0; i < xlnumresults; i++)
	  pusharg(xlresults[i]);
      }
      break;
    case POP_VALUES_OP:
      {
	int i, ni, n;
	LVAL nl;
	
	get_operand(ni);
	nl = getregval(ni);
	n = getfixnum(nl);
	vstop -= n;
	for (i = 0; i < n; i++)
	  xlresults[i] = vstop[i];
	xlnumresults = n;
      }
      break;
    case INIT_OP:
      current_pc = do_INIT_OP(current_pc);
      break;
    case SET_CAR_OP:
      {
	int xi, vi, ri;
	LVAL xl, vl;
	
	get_operand(xi);
	get_operand(vi);
	get_operand(ri);
	xl = getregval(xi);
	vl = getregval(vi);
	if (! null(xl)) {
	  if (consp(xl)) rplaca(xl, vl);
	  else xlbadtype(xl);
	}
	set_result_or_regval(ri, vl);
      }
      break;
    case SET_CDR_OP:
      {
	int xi, vi, ri;
	LVAL xl, vl;
	
	get_operand(xi);
	get_operand(vi);
	get_operand(ri);
	xl = getregval(xi);
	vl = getregval(vi);
	if (! null(xl)) {
	  if (consp(xl)) rplacd(xl, vl);
	  else xlbadtype(xl);
	}
	set_result_or_regval(ri, vl);
      }
      break;
    case INIT_0_OP:
      {
	int nc, nr, xi, argc, nreq;
	LVAL literals = bcode_literals(current_function);

	argc = vstop - vsbase;

	get_operand(nreq);
	if (nreq != argc) {
	  if (nreq > argc)
	    xltoofew();
	  else
	    xltoomany();
	}

	/* load any constanst used from the literals */
	get_operand(nc);
	while (nc-- > 0) {
	  get_operand(xi);
	  pusharg(getlitval(xi));
	}

	/* push additional space on top and initialize to nil */  
	get_operand(nr);
	if (xlsp + nr > xlargstktop) xlargstkoverflow();
	while (nr-- > 0) *xlsp++ = NIL;
      }
      break;
    case STOP_OP:
      goto done;
      break;
    case SWAP_OP:
      {
	int xi, yi;
	LVAL tmp;
	get_operand(xi);
	get_operand(yi);
	tmp = getregval(yi);
	setregval(yi, getregval(xi));
	setregval(xi, tmp);
      }
      break;
    case LDCONST_OP:
      {
	int xi, yi;
	LVAL literals = bcode_literals(current_function);
	get_operand(xi);
	get_operand(yi);
	setregval(yi, getlitval(xi));
      }
      break;
    case NCASE_OP:
      {
	int ci, chi, li, i, n;
	LVAL cl, chl, cs;
	LVAL literals = bcode_literals(current_function);
	
	get_operand(ci);
	get_operand(chi);
	cl = getregval(ci);
	chl = getlitval(chi);
	n = getsize(chl);
	
	li = 0; /* to keep compiler happy */
	for (i = 0; i < n; i++) {
	  get_operand(li);
	  cs = getelement(chl, i);
	  if (case_match_p(cl, cs)) break;
	}
	if (i == n) get_operand(li);
	
	do_goto(li);
      }
      break;
    case MKCLOS_OP:
      current_pc = do_MKCLOS_OP(current_pc);
      break;
    case SETCLOSDATA_OP:
      {
	int ci, nv, xi, i;
	LVAL env;
	get_operand(ci);
	env = bcode_environment(getregval(ci));
	get_operand(nv); /**** could check against size of env */
	for (i = 0; i < nv; i++) {
	  get_operand(xi);
	  setelement(env, i, getregval(xi));
	}
	break;
      }
    case SETCLOSCODE_OP:
      {
	int ci, fi, li;
	LVAL closure;
	LVAL literals = bcode_literals(current_function);
	get_operand(ci);
	get_operand(fi);
	get_operand(li);
	closure = getregval(ci);
	set_bcode_index(closure, fi);
	setbcname(getbcccode(closure), getlitval(li));
	break;
      }
    case LDNOTSUPP_OP:
      {
	int vi;
	get_operand(vi);
	setregval(vi, s_not_supplied);
	break;
      }
    case LDMVARGS_OP:
      {
	int nreq, nopt, restp, fi, vi, nro, i;
	/*LVAL literals = bcode_literals(current_function);*/
	get_operand(nreq);
	get_operand(nopt);
	get_operand(restp);
	get_operand(fi);/***** currently ignored -- use for error messages */
	nro = nreq + nopt;
	if (nreq > xlnumresults) xltoofew();
	if (! restp && xlnumresults > nro) xltoomany();
	for (i = 0; i < nro && i < xlnumresults; i++) {
	  get_operand(vi);
	  setregval(vi, xlresults[i]);
	}
	for (i = xlnumresults; i < nro; i++) {
	  get_operand(vi);
	  setregval(vi, s_not_supplied);
	}
	if (restp) {
	  get_operand(vi);
	  setregval(vi, NIL);
	  for (i = nro; i < xlnumresults; i++)
	    setregval(vi, cons(xlresults[i], getregval(vi)));
	  setregval(vi, xlnreverse(getregval(vi)));
	}
	break;
      }
    case NOT_OP:
      {
	int ai, vi;
	get_operand(ai);
	get_operand(vi);
	setregval(vi, null(getregval(ai)) ? s_true: NIL);
	break;
      }
    case NEW_BLOCK_OP:
      {
	volatile int ci;
	LVAL * volatile oldbase = vsbase;
	LVAL tag;
	int bi;
	CONTEXT cntxt;

	get_operand(bi);
	get_operand(ci);
	xlsave1(tag);
	tag = consa(NIL);
	xlbegin(&cntxt, CF_THROW, tag);
	if (! XL_SETJMP(cntxt.c_jmpbuf))
	  xlapp1(getregval(bi), tag); /**** inefficient but easy */
	/* vsbase needs to be restored in jumps. xlcstop is handled in
           the standard jump code, as is vstop = xlsp */
	vsbase = oldbase;
	do_goto(ci);
	xlend(&cntxt);
	xlpop();
	break;
      }
    case NEW_TAGBODY_OP:
      {
	volatile int ci;
	LVAL * volatile oldbase = vsbase;
	LVAL tag;
	int bi;
	CONTEXT cntxt;

	get_operand(bi);
	get_operand(ci);
	xlsave1(tag);
	tag = consa(NIL);
	xlbegin(&cntxt, CF_THROW, tag);
	if (XL_SETJMP(cntxt.c_jmpbuf)) {
	  BC_evfun(xlvalue, 0, xlsp);
	  /* vsbase needs to be restored in jumps. xlcstop is handled in
	     the standard jump code, as is vstop = xlsp */
	  vsbase = oldbase;
	}
	else {
	  LVAL *oldsp = xlsp;
	  pusharg(tag);
	  BC_evfun(getregval(bi), 1, oldsp);
	  xlsp = oldsp;
	}
	do_goto(ci);
	xlend(&cntxt);
	xlpop();
	break;
      }
    case NEW_GO_OP:
      {
	int ri, li;
	get_operand(ri);
	get_operand(li);
	xlthrow(getregval(ri), getregval(li));
	break; /* not reached */
      }
    case NEW_CATCH_OP:
      {
	volatile int ci;
	LVAL * volatile oldbase = vsbase;
	int ti, bi;
	CONTEXT cntxt;

	get_operand(ti);
	get_operand(bi);
	get_operand(ci);
	xlbegin(&cntxt, CF_THROW, getregval(ti));
	if (! XL_SETJMP(cntxt.c_jmpbuf))
	  BC_evfun(getregval(bi), 0, xlsp);
	/* vsbase needs to be restored in jumps. xlcstop is handled in
           the standard jump code, as is vstop = xlsp */
	vsbase = oldbase;
	do_goto(ci);
	xlend(&cntxt);
	break;
      }
    case NEW_ERRSET_OP:
      {
	volatile int ci;
	LVAL * volatile oldbase = vsbase;
	int bi, fi;
	CONTEXT cntxt;

	get_operand(bi);
	get_operand(fi);
	get_operand(ci);
	xlbegin(&cntxt, CF_ERROR, getregval(fi));
	if (XL_SETJMP(cntxt.c_jmpbuf)) {
	  set_one_result(NIL);
	  /* vsbase needs to be restored in jumps. xlcstop is handled in
	     the standard jump code, as is vstop = xlsp */
	  vsbase = oldbase;
	}
	else {
	  LVAL val;
	  BC_evfun(getregval(bi), 0, xlsp);
	  val = get_one_result(); /* has to be done before set */
	  set_one_result(consa(val));
	}
	do_goto(ci);
	xlend(&cntxt);
	break;
      }
    case NEW_UNWIND_PROTECT_OP:
      {
	volatile int ci, pi;
	LVAL * volatile oldbase = vsbase;
	int bi, sts = FALSE, nvals, i, mask;
	CONTEXT cntxt, *target;
	LVAL val;

	get_operand(bi);
	get_operand(pi);
	get_operand(ci);
	xlbegin(&cntxt,CF_UNWIND,NIL);
	if (XL_SETJMP(cntxt.c_jmpbuf)) {
	  sts = TRUE;
	  target = xltarget;
	  mask = xlmask;
	  val = xlvalue;
	  /* vsbase needs to be restored in jumps. xlcstop is handled in
	     the standard jump code, as is vstop = xlsp */
	  vsbase = oldbase;
	}
	else {
	  BC_evfun(getregval(bi), 0, xlsp);
	  nvals = xlnumresults;
	  for (i = 0; i < nvals; i++) {
	    val = get_nth_result(i);
	    pusharg(val);
	  }
	}
	xlend(&cntxt);
	BC_evfun(getregval(pi), 0, xlsp);
	if (sts)
	  xljump(target, mask, val);
	else {
	  vstop -= nvals;
	  for (i = 0; i < nvals; i++)
	    set_nth_result(i, vstop[i]);
	  xlnumresults = nvals;
	  do_goto(ci);
	}
	break;
      }
    case GET_OPTARG_OP:
      {
	int ai, di, vi, si;
	LVAL val;
	get_operand(ai);
	get_operand(di);
	get_operand(vi);
	get_operand(si);
	val = getregval(ai);
	if (val == s_not_supplied) {
	  LVAL literals = bcode_literals(current_function);
	  setregval(vi, getlitval(di));
	  setregval(si, NIL);
	}
	else {
	  setregval(vi, val);
	  setregval(si, s_true);
	}
	break;
      }
    case MAKE_KEYARGS_OP:
      {
	int ai, vi;
	get_operand(ai);
	get_operand(vi);
	setregval(vi, consd(copylist(getregval(ai))));
	break;
      }
    case CHECK_LAST_KEYARG_OP:
      {
	int ai, others;
	LVAL keys;
	get_operand(ai);
	get_operand(others);
	keys = getregval(ai);
	if (llength(cdr(keys)) % 2)
	  xlfail("missing keyword value");
	if (! null(cdr(keys)) &&
	    ! others &&
	    null(bc_get_keyarg(keys, k_allow_other_keys, NIL, NULL)) &&
	    ! null(getvalue(s_strict_keywords)))
	  xlfail("too many keyword arguments");
	break;
      }
    case GET_KEYARG_OP:
      {
	int ai, ki, di, vi, si, found;
	LVAL literals = bcode_literals(current_function);
	LVAL val;
	get_operand(ai);
	get_operand(ki);
	get_operand(di);
	get_operand(vi);
	get_operand(si);
	val = bc_get_keyarg(getregval(ai), getlitval(ki), getlitval(di),
			    &found);
	setregval(vi, val);
	setregval(si, found ? s_true : NIL);
	break;
      }
    case NEW_DYNAMIC_BIND_OP:
      {
	int si, vi, ei;
	LVAL nexts, nextv, olddenv = xldenv;
	
	get_operand(si);
	get_operand(vi);
	get_operand(ei);
	
	/**** For the moment, the old xldenv value is passed both in a
              register and on the stack -- this is to help with
              debugging. Eventually it will just be passed in a
              register */
	pusharg(xldenv);
	
	if (getregval(vi) == s_true) {
	  for (nexts = getregval(si); consp(nexts); nexts = cdr(nexts))
	    xldbind(car(nexts), getvalue(car(nexts)));
	}
	else {
	  for (nexts = getregval(si), nextv = getregval(vi);
	       consp(nexts) && consp(nextv);
	       nexts = cdr(nexts), nextv = cdr(nextv))
	    xldbind(car(nexts), car(nextv));
	  for (; consp(nexts); nexts = cdr(nexts))
	    xldbind(car(nexts), s_unbound);
	}
	setregval(ei, olddenv);
	break;
      }
    case NEW_DYNAMIC_UNBIND_OP:
      {
	/**** For the moment, the old xldenv value is passed both in a
              register and on the stack -- this is to help with
              debugging. Eventually it will just be passed on the
              stack */
	LVAL olddenv = *--vstop;
	int ei;
	get_operand(ei);
	if (getregval(ei) != olddenv)
	  stdputstr("bad stack in unbind\n");
	xlunbind(olddenv);
	break;
      }
    case STRUCT_OP:
      {
	int si;
	LVAL str;
	get_operand(si);
	str = getregval(si);
	switch (next_opcode()) {
	case 0:
	  {
	    int i, vi;
	    get_operand(i);
	    get_operand(vi);
	    if (! structp(str)) xlbadtype(str);
	    if (i >= getsize(str)) xlerror("bad structure reference",str);
	    setregval(vi, getelement(str,i));
	    break;
	  }
	case 1:
	  {
	    int i, ni, vi;
	    LVAL val;
	    get_operand(i);
	    get_operand(ni);
	    get_operand(vi);
	    if (! structp(str)) xlbadtype(str);
	    if (i >= getsize(str)) xlerror("bad structure reference",str);
	    val = getregval(ni);
	    setelement(str, i, val);
	    setregval(vi, val);
	    break;
	  }
	case 2:
	  {
	    int ti, vi, val = FALSE;
	    LVAL literals = bcode_literals(current_function);
	    LVAL type;
	    get_operand(ti);
	    get_operand(vi);
	    type = getlitval(ti);
	    if (structp(str)) {
	      for (str = getelement(str,0);
		   ! null(str);
		   str = xlgetprop(str,s_strinclude))
		if (str == type) {
		  val = TRUE;
		  break;
		}
	    }
	    setregval(vi, val ? s_true : NIL);
	    break;
	  }
	default: xlfail("unknown opcode");
	}
	break;
      }
    default: xlfail("unknown opcode");
    }
  }

 compiled_continuation:
  {
    LVAL code = getbccode(getbcccode(current_function));
    int i = (int) getfixnum(car(code));
    int j = (int) getfixnum(cdr(code));
    (*xlmodules[i].functions[j])(entry);
    if (null(current_function)) goto done;
    else if (stringp(getbccode(getbcccode(current_function)))) {
      current_pc = xlcstop->pe.pc;
      goto byte_code_continuation;
    }
    else {
      entry = xlcstop->pe.entry;
      goto compiled_continuation;
    }
  }

 done:
  return;
}

LVAL BC_evfun P3C(LVAL, fun, int, argc, LVAL *, argv)
{
  CONTINUATIONP old_cstop = xlcstop;
  LVAL *old_vsbase = vsbase, *old_vstop = vstop;
  LVAL val;
  bytecode *current_pc;

  vsbase = argv; /**** for testing new frame handling */
  current_pc = NULL_PC;
  save_current_continuation(NO_VALUE); /***** replace */

  pusharg(fun);
  vsbase = vstop;
  pusharg(DONE);
  push_environment(bcode_environment(fun));

  /* shift up the arguments */
  if (xlsp + argc > xlargstktop) xlargstkoverflow();
  MEMCPY(xlsp, argv, sizeof(LVAL) * argc);
  xlsp += argc;

  bcloop(bcode_index(fun));
  val = get_one_result();

  xlcstop = old_cstop;
  vsbase = old_vsbase;
  vstop = old_vstop;

  return(val);
}

LVAL BC_evform P1C(LVAL, form)
{
  LVAL fun, val, *argv;
  int argc;

  xlsave1(fun);
  fun = newbcclosure(s_lambda, form);
  argc = pushargs(fun,NIL);
  argv = xlfp + 3;
  val = BC_evfun(fun,argc,argv);
  xlsp = xlfp;
  xlfp = xlfp - (int)getfixnum(*xlfp);
  xlpop();
  return(val);
}

VOID bcsymbols(V)
{
  s_not_supplied = xlenter("%NOT-SUPPLIED");
  s_leaf = xlenter("LEAF");
  s_call = xlenter("CALL");
#ifdef PROFILE
  s_profile_output = xlenter("*PROFILE-OUTPUT*");
#endif /* PROFILE */
}

VOID init_bytecode(V)
{
  xlcontinuation_stack = (CONTINUATIONP) malloc(CDEPTH * sizeof(CONTINUATION));
  if (xlcontinuation_stack == NULL)
    xlfatal("insufficient memory");
  xlcsend = xlcontinuation_stack + CDEPTH;
  xlcstop = xlcontinuation_stack;
  init_modules();
}

/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                   Internal CPS Node Representation                      **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

#define leaf_node_p(n) (cpsnodep(n) && (getcpstype(n) == s_leaf))
#define lambda_node_p(n) (cpsnodep(n) && (getcpstype(n) == s_lambda))
#define call_node_p(n) (cpsnodep(n) && (getcpstype(n) == s_call))
#define cps_node_internal(n, i) getelement(n, ((i) + 1))
#define set_cps_node_internal(n, i, v) setelement(n, ((i) + 1), (v))

#define node_children(n) cps_node_internal((n), 0)
#define node_parent(n) cps_node_internal((n), 1)
#define node_simplified(n) cps_node_internal((n), 2)
#define node_note(n) cps_node_internal((n), 3)

#define CPS_NODE_CHILDREN   0
#define CPS_NODE_PARENT     1
#define CPS_NODE_SIMPLIFIED 2
#define CPS_NODE_NOTE       3
#define CPS_LEAF_NODE_VALUE   4
#define CPS_LEAF_NODE_COUNT   5
#define CPS_LAMBDA_NODE_ARGLIST     4
#define CPS_LAMBDA_NODE_LAMBDA_LIST 5
#define CPS_LAMBDA_NODE_NAME        6

#define NUM_CPS_INTERNALS (CPSNODESIZE - 1)

LVAL xlmakecpsnode(V)
{
  LVAL type;
  type = xlgetarg();
  xllastarg();
  return(newcpsnode(type));
}

LVAL xlcpsinternal(V)
{
  LVAL n, il;
  int i;

  n = xlgacpsnode();
  il = xlgafixnum();
  i = getfixnum(il);

  if (i < 0 || i >= NUM_CPS_INTERNALS) xlerror("index out of range", il);
  if (moreargs()) set_cps_node_internal(n, i, xlgetarg());
  xllastarg();

  return(cps_node_internal(n, i));
}

LVAL xlcpstransform(V)
{
  LVAL a, b;
  int i;
  
  a = xlgacpsnode();
  b = xlgacpsnode();
  xllastarg();

  setcpstype(a, getcpstype(b));
  for (i = 0; i < NUM_CPS_INTERNALS; i++)
    set_cps_node_internal(a, i, cps_node_internal(b, i));

  return(a);
}

LVAL xlcpsleafnodep(V)
{
  LVAL n;

  n = xlgetarg();
  xllastarg();
  return((leaf_node_p(n)) ? s_true : NIL);
}

LVAL xlcpslambdanodep(V)
{
  LVAL n;

  n = xlgetarg();
  xllastarg();
  return((lambda_node_p(n)) ? s_true : NIL);
}

LVAL xlcpscallnodep(V)
{
  LVAL n;

  n = xlgetarg();
  xllastarg();
  return((call_node_p(n)) ? s_true : NIL);
}

LOCAL int any_references_p P2C(LVAL, v, LVAL, n)
{
  LVAL ch;

  if (! cpsnodep(n)) xlbadtype(n);

  if (leaf_node_p(n)) {
    return((v == n) ? TRUE : FALSE);
  }
  else {
    for (ch = node_children(n); consp(ch); ch = cdr(ch)) {
      if (any_references_p(v, car(ch))) return(TRUE);
    }
    return(FALSE);
  }
}

LVAL xlcpsanyrefs(V)
{
  LVAL v, n;

  v = xlgacpsnode();
  n = xlgacpsnode();
  xllastarg();

  return((any_references_p(v, n)) ? s_true : NIL);
}

LOCAL VOID find_references P3C(LVAL, v, LVAL, n, LVAL *, refs)
{
  LVAL c, ch, tmp;
  int i;

  if (! cpsnodep(n)) xlbadtype(n);

  if (! leaf_node_p(n)) {
    for (ch = node_children(n), i = 0; consp(ch); ch = cdr(ch), i++) {
      c = car(ch);
      if (leaf_node_p(c)) {
	if (v == c) {
	  xlsave1(tmp);
	  tmp = cons(n, cvfixnum((FIXTYPE) i));
	  *refs = cons(tmp, *refs);
	  xlpop();
	}
      }
      else find_references(v, c, refs);
    }
  }
}

LVAL xlcpsfindrefs(V)
{
  LVAL v, n, refs;

  v = xlgacpsnode();
  n = xlgacpsnode();
  xllastarg();

  xlsave1(refs);
  refs = NIL;
  find_references(v, n, &refs);
  xlpop();
  
  return(refs);
}

LOCAL LVAL cps_node_internals P1C(int, which)
{
  LVAL n;
  
  n = xlgacpsnode();
  xllastarg();
  
  return(cps_node_internal(n, which));
}

LVAL xlcpsnodechildren(V) { return(cps_node_internals(CPS_NODE_CHILDREN)); }
LVAL xlcpsnodeparent(V) { return(cps_node_internals(CPS_NODE_PARENT)); }
LVAL xlcpsnodesimplified(V) { return(cps_node_internals(CPS_NODE_SIMPLIFIED)); }
LVAL xlcpsnodenote(V) { return(cps_node_internals(CPS_NODE_NOTE)); }
LVAL xlcpsleafnodevalue(V) { return(cps_node_internals(CPS_LEAF_NODE_VALUE)); }
LVAL xlcpsleafnodecount() { return(cps_node_internals(CPS_LEAF_NODE_COUNT)); }

LVAL xlcpslambdanodearglist(V)
{
  return(cps_node_internals(CPS_LAMBDA_NODE_ARGLIST));
}

LVAL xlcpslambdanodelambdalist(V)
{
  return(cps_node_internals(CPS_LAMBDA_NODE_LAMBDA_LIST));
}

LVAL xlcpslambdanodename(V)
{
  return(cps_node_internals(CPS_LAMBDA_NODE_NAME));
}

LOCAL LVAL set_cps_node_internals P1C(int, which)
{
  LVAL n, v;
  
  n = xlgacpsnode();
  v = xlgetarg();
  xllastarg();
  
  set_cps_node_internal(n, which, v);

  return(cps_node_internal(n, which));
}

LVAL xlcpssetnodechildren(V)
{
  return(set_cps_node_internals(CPS_NODE_CHILDREN));
}

LVAL xlcpssetnodeparent(V) { return(set_cps_node_internals(CPS_NODE_PARENT)); }

LVAL xlcpssetnodesimplified(V)
{
  return(set_cps_node_internals(CPS_NODE_SIMPLIFIED));
}

LVAL xlcpssetnodenote(V) { return(set_cps_node_internals(CPS_NODE_NOTE)); }

LVAL xlcpssetleafnodevalue(V)
{
  return(set_cps_node_internals(CPS_LEAF_NODE_VALUE));
}

LVAL xlcpssetleafnodecount(V)
{
  return(set_cps_node_internals(CPS_LEAF_NODE_COUNT));
}

LVAL xlcpssetlambdanodearglist(V)
{
  return(set_cps_node_internals(CPS_LAMBDA_NODE_ARGLIST));
}

LVAL xlcpssetlambdanodelambdalist(V)
{
  return(set_cps_node_internals(CPS_LAMBDA_NODE_LAMBDA_LIST));
}

LVAL xlcpssetlambdanodename(V)
{
  return(set_cps_node_internals(CPS_LAMBDA_NODE_NAME));
}

LVAL xlcpslambdanodebody(V)
{
  LVAL n, ch;

  n = xlgacpsnode();
  xllastarg();
  ch = node_children(n);
  return(consp(ch) ? car(ch) : NIL);
}
  
LVAL xlcpscallnodefunction(V)
{
  LVAL n, ch;

  n = xlgacpsnode();
  xllastarg();
  ch = node_children(n);
  return(consp(ch) ? car(ch) : NIL);
}
  
LVAL xlcpscallnodeargs(V)
{
  LVAL n, ch;

  n = xlgacpsnode();
  xllastarg();
  ch = node_children(n);
  return(consp(ch) ? cdr(ch) : NIL);
}


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                    Some Compiler Support Functions                      **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

/**** what is wrong wih using symbol-value??? */
LVAL xldval(V)
{
  LVAL s;
  
  s = xlgasymbol();
  xllastarg();
  return(getvalue(s));
}

/**** is this needed? */
/* xlgetlambdaname - get the name associated with a closure */
LVAL xlgetlambdaname(V)
{
  LVAL closure;
  closure = xlgaclosure();
  return(getname(closure));
}


/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**                         Some SETF Functions                             **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

LVAL xsetget(V)
{
  LVAL x, y, v;

  x = xlgasymbol();
  y = xlgetarg();
  v = xlgetarg();
  if (moreargs()) v = nextarg(); /* allows for extra argument */
  xllastarg();

  xlputprop(x, v, y);
  return(v);
}

LVAL xsetsymval(V)
{
  LVAL x, v;
  x = xlgasymbol();
  v = xlgetarg();
  xllastarg();
  setvalue(x, v);
  return(v);
}

LVAL xsetsymfun(V)
{
  LVAL x, v;
  x = xlgasymbol();
  v = xlgetarg();
  xllastarg();
  setfunction(x, v);
  return(v);
}

LVAL xsetsymplist(V)
{
  LVAL x, v;
  x = xlgasymbol();
  v = xlgetarg();
  xllastarg();
  setplist(x, v);
  return(v);
}

LVAL xsetaref(V)
{
  LVAL x, v;
  int i;

  x = xlgetarg();
  i = rowmajorindex(x, NIL, TRUE); /* does error checking */
  v = xlgetarg();
  xllastarg();

  settvecelement(darrayp(x) ? getdarraydata(x) : x, i, v);
  return(v);
}

LVAL xsetgethash(V)
{
  LVAL key, table, value;
  key = xlgetarg();
  table = xlgetarg();
  value = xlgetarg();
  xllastarg();
  xlsetgethash(key, table, value);
  return(value);
}

LVAL xsetcar(V)
{
  LVAL x, v;
  x = xlgacons();
  v = xlgetarg();
  xllastarg();
  rplaca(x, v);
  return v;
}

LVAL xsetcdr(V)
{
  LVAL x, v;
  x = xlgacons();
  v = xlgetarg();
  xllastarg();
  rplacd(x, v);
  return v;
}

LVAL xsetnth(V)
{
  LVAL nl, xl, vl;
  int i;

  nl = xlgafixnum();
  xl = xlgalist();
  vl = xlgetarg();
  xllastarg();
  
  for (i = (int) getfixnum(nl); i > 0 && consp(xl); i--, xl = cdr(xl));
  if (consp(xl)) rplaca(xl, vl);
  return vl;
}

LVAL xsetelt(V)
{
  LVAL al, il, vl;
  int i;
  
  al = xlgetarg();
  il = xlgafixnum();
  vl = xlgetarg();
  xllastarg();
  
  i = getfixnum(il);
  
  switch (ntype(al)) {
  case CONS:
    for (; i > 0 && consp(al); --i)
      al = cdr(al);
    if((!consp(al)) || i < 0)
      xlerror("index out of range", il);
    rplaca(al,vl);
    break;
  case VECTOR:
  case STRING:
  case TVEC:
    if (i < 0 || i >= gettvecsize(al))
      xlerror("index out of range", il);
    settvecelement(al, i, vl);
    break;
  default:
    xlbadtype(al);
  }
  
  return vl;
}

LVAL xsetsvref(V)
{
  LVAL al, il, vl;
  int i;
  
  al = xlgavector();
  il = xlgafixnum();
  vl = xlgetarg();
  xllastarg();
  
  i = getfixnum(il);

  if (i < 0 || i >= getsize(al))
    xlerror("index out of range", il);

  setelement(al, i, vl);
  
  return vl;
}

/*****************************************************************************/
/*****************************************************************************/
/**                                                                         **/
/**             Some experimental code for C compiled code                  **/
/**                                                                         **/
/*****************************************************************************/
/*****************************************************************************/

/****
#define RETURN(c) { \
  LVAL __c__ = (getregval(c)); \
  if (__c__ == DONE) { vsbase[-1] = NIL; return; } \
  xlcstop = xlcontinuation_stack + getfixnum(__c__); \
  vsbase = xlcstop->base; \
  vstop = xlcstop->top; \
  if (xlcstop->vreg != NO_VALUE) setregval(xlcstop->vreg,get_one_result()); \
  if (xlcstop < FVcont) return; \
  else { entry = xlcstop->pe.entry; goto entry; } \
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
*/

LVAL cmpAREF1 P2C(LVAL, xl, LVAL, il)
{
  int i;

  if (darrayp(xl)) xl = getdarraydata(xl);

  if (! fixp(il)) xlbadtype(il);
  i = getfixnum(il);
  if (i < 0 || i >= gettvecsize(xl)) /* does error check */
    xlerror("index out of range", il);
  xl = gettvecelement(xl, i);
  return(xl);
}

/****
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
*/

VOID cmp_call_setup P6C(LVAL, fun, int, vi, int, entry, int, argc, LVAL, cont, int, tailp)
{
  if (bcclosurep(fun)) {

    if (!tailp) {
      cont = cvfixnum((FIXTYPE) (xlcstop - xlcontinuation_stack));
      cmp_save_current_continuation(entry, vi);
    }
    pusharg(fun);
    vsbase = vstop;
    pusharg(cont);
    push_environment(bcode_environment(fun));
    if (xlsp + argc > xlargstktop) xlargstkoverflow();
    if (xlcstop >= xlcsend) xlabort("continuation stack overflow");
    if (stringp(getbccode(getbcccode(fun)))) {
      xlcstop->pe.pc =
	bcode_codevec(fun)
	  + getfixnum(getelement(bcode_jumptable(fun),bcode_index(fun)));
    }
    else xlcstop->pe.entry = bcode_index(fun);
  }
  else {
    LVAL *newfp;
  
    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE) argc));
    if (xlsp + argc > xlargstktop) xlargstkoverflow();
    xlfp = newfp;
  }
}

/*
VOID cmp_shift_tail_frame(base)
     LVAL *base;
     int argc;
{
  if (xlcstop[-1].base != base) {
    n = vstop - vsbase;
    MEMMOVE(base - 1, vsbase - 1, sizeof(LVAL) * (n + 1));
    vstop = base + n;
    vsbase = base;
  }
}
*/
/****
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
*/
/*
VOID cmp_tail_lcall_setup(argc, cont)
     int argc;
     LVAL cont;
{
  pusharg(vsbase[-1]);
  vsbase = vstop;
  pusharg(cont);
  if (xlsp + argc > xlargstktop) xlargstkoverflow();
}
*/

/****
#define cmp_lcall_setup(vi, entry, argc) { \
  LVAL Cont; \
  Cont = cvfixnum((FIXTYPE) (xlcstop - xlcontinuation_stack)); \
  cmp_save_current_continuation(entry, vi); \
  pusharg(vsbase[-1]); \
  vsbase = vstop; \
  pusharg(Cont); \
  if (xlsp + argc > xlargstktop) xlargstkoverflow(); \
}
*/
/*
VOID cmp_lcall_setup(vi, entry, argc)
     int vi, entry, argc;
{
  LVAL cont;
  cont = cvfixnum((FIXTYPE) (xlcstop - xlcontinuation_stack));
  cmp_save_current_continuation(entry, vi);
  pusharg(vsbase[-1]);
  vsbase = vstop;
  pusharg(cont);
  if (xlsp + argc > xlargstktop) xlargstkoverflow();
}
*/
#endif /* BYTECODE */
