/* xlbfun.c - xlisp basic built-in functions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* forward declarations */
LOCAL LVAL makesymbol P1H(int);
#ifdef CONDITIONS
LOCAL LVAL conditionhook P1H(LVAL);
#endif /* CONDITIONS */

/* xeval - the built-in function 'eval' */
LVAL xeval(V)
{
    LVAL expr,oldenv,oldfenv;

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(oldenv);
    xlprotect(oldfenv);

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    /* establish global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;

    /* evaluate the expression */
    expr = xleval(expr);

    /* restore environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(2);

    /* return evaluated expression */
    return (expr);
}

/* xapply - the built-in function 'apply' */
/* Algorithm based on Luke Tierney's XLISP-STAT */
LVAL xapply(V)
{
    LVAL fun,arglist;
    int n;

    if (xlargc < 2) xltoofew();
    if (! listp(xlargv[xlargc - 1])) xlfail("last argument must be a list");

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(arglist);
    xlprotect(fun);

    fun = xlgetarg();
    n = xlargc - 1;
    arglist = xlargv[n];
    while (n-- > 0) arglist = cons(xlargv[n], arglist);

    /* restore the stack */
    xlpopn(2);

    return xlapply(pushargs(fun, arglist));
}

/* xfuncall - the built-in function 'funcall' */
LVAL xfuncall(V)
{
    FRAMEP newfp;
    int argc;

    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(xlgetarg());
    pusharg(NIL); /* will be argc */

    /* push each argument */
    for (argc = 0; moreargs(); ++argc)
	pusharg(nextarg());

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* apply the function to the arguments */
    return (xlapply(argc));
}

/* xl1macroexpand - expand a macro call; internal version. */
/* Uses macro definitions of FSUBR's stored in the MACRO property. */
LOCAL int xl1macroexpand P3C(LVAL, form, LVAL, newenv, LVAL *, newform)
{
    LVAL sym,fun,args,oldenv,oldfenv;
    int expanded = FALSE;

    /* protect some pointers */
    xlstkcheck(4);
    xlsave(fun);
    xlsave(args);
    xlsave(oldenv);
    xlsave(oldfenv);

    /* expand the form if it isn't atomic */
    if (consp(form)) {
      oldenv = xlenv;
      oldfenv = xlfenv;
      if (!null(newenv)) {
	xlenv = car(newenv);
	xlfenv = cdr(newenv);
      }
      else {
	xlenv = xlfenv = NIL;
      }

      sym = car(form);		/* get the macro name */
      args = cdr(form);		/* get the arguments */

      /* FSUBR's are assumed to be global. Macro definitions for them can */
      /* be stored as the MACRO property of the symbol */
      if (symbolp(sym)) {
	fun = xlxgetfunction(sym);
	if (fsubrp(fun))
	  fun = xlgetprop(sym, s_macro);
	if (null(fun))
	  fun = s_unbound;
      }
      else
	fun = s_unbound;

      if (fun != s_unbound)
	expanded = macroexpand(fun,args,&form);

      xlenv = oldenv;
      xlfenv = oldfenv;
    }

    /* restore the stack and return the expansion */
    xlpopn(4);

    *newform = form;
    return expanded;
}

/* x1macroexpand - expand a macro call */
LVAL x1macroexpand(V)
{
  LVAL form, newenv;
  int expanded;

  /* get the form */
  form = xlgetarg();
  newenv = (moreargs() ? xlgalist() : NIL);
  xllastarg();

  expanded = xl1macroexpand(form, newenv, &form);

#ifdef MULVALS
  xlnumresults = 2;
  xlresults[0] = form;
  xlresults[1] = (expanded) ? s_true : NIL;
#endif /* MULVALS */
  return (form);
}

/* xmacroexpand - expand a macro call repeatedly */
LVAL xmacroexpand(V)
{
  LVAL form, newenv;
  int expanded, again;

  /* get the form */
  form = xlgetarg();
  newenv = (moreargs() ? xlgalist() : NIL);
  xllastarg();

  expanded = again = xl1macroexpand(form, newenv, &form);
  while (again)
    again = xl1macroexpand(form, newenv, &form);

#ifdef MULVALS
  xlnumresults = 2;
  xlresults[0] = form;
  xlresults[1] = (expanded) ? s_true : NIL;
#endif /* MULVALS */
  return (form);
}

/* xmacrofun - return the function of a macro */
LVAL xmacrofun(V)
{
  LVAL sym, env, fun;
  LVAL oldenv, oldfenv;
  LVAL closure;

  sym = xlgasymbol();
  env = moreargs() ? xlgalist() : NIL;
  xllastarg();

  oldenv = xlenv;
  oldfenv = xlfenv;
  if (!null(env)) {
    xlenv = car(env);
    xlfenv = cdr(env);
  }
  else {
    xlenv = xlfenv = NIL;
  }

  closure = NIL;
  fun = xlxgetfunction(sym);

  /* FSUBR's are assumed to be global. Macro definitions for them can */
  /* be stored as the MACRO property of the symbol */
  if (fsubrp(fun))
    fun = xlgetprop(sym, s_macro);

  switch (ntype(fun)) {
  case CLOSURE:
    if (gettype(fun) == s_macro)
      closure = xlclose(getname(fun), s_lambda, getlambda(fun), getbody(fun),
			getenvi(fun), getfenv(fun));
    break;
#ifdef BYTECODE
  case BCCLOSURE:
    if (getbcctype(fun) == s_macro)
      closure = newbcclosure(s_lambda, getbcccode(fun));
    break;
#endif /* BYTECODE */
  }

  xlenv = oldenv;
  xlfenv = oldfenv;

  return closure;
}

/* xatom - is this an atom? */
LVAL xatom(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (atom(arg) ? s_true : NIL);
}

/* xsymbolp - is this an symbol? */
LVAL xsymbolp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (symbolp(arg) ? s_true : NIL);
}

/* xnumberp - is this a number? */
LVAL xnumberp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (numberp(arg) ? s_true : NIL);
}

/* xcomplexp - is this a complex number? */
LVAL xcomplexp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (complexp(arg) ? s_true : NIL);
}

/* xintegerp - is this an integer? */
LVAL xintegerp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (integerp(arg) ? s_true : NIL);
}

/* xfloatp - is this a float? */
LVAL xfloatp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (floatp(arg) ? s_true : NIL);
}

#ifdef BIGNUMS
LVAL xrationalp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (rationalp(arg) ? s_true : NIL);
}

LVAL xnumerator(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (integerp(arg)) return arg;
    if (ratiop(arg)) return getnumer(arg);
    xlbadtype(arg);
    return NIL; /* never executes */
}

LVAL xdenominator(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (integerp (arg)) return cvfixnum((FIXTYPE)1);
    if (ratiop(arg)) return getdenom(arg);
    xlbadtype(arg);
    return NIL; /* never executes */
}
#endif

/* xcharp - is this a character? */
LVAL xcharp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (charp(arg) ? s_true : NIL);
}

/* xstringp - is this a string? */
LVAL xstringp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (stringp(arg) ? s_true : NIL);
}

/* xstreamp - is this a stream? */
LVAL xstreamp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (streamp(arg) || ustreamp(arg) ? s_true : NIL);
}

/* xopenstreamp - is this an open stream? */
LVAL xopenstreamp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return s_true;
    if (streamp(arg)) return (getfile(arg) != CLOSED ? s_true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}

/* xinputstreamp - is this an input stream? */
LVAL xinputstreamp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return s_true;
    if (streamp(arg))
        return (getfile(arg)!=CLOSED && (arg->n_sflags&S_FORREADING)?
		s_true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}

/* xoutputstreamp - is this an output stream? */
LVAL xoutputstreamp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return s_true;
    if (streamp(arg))
        return (getfile(arg)!=CLOSED && (arg->n_sflags&S_FORWRITING)?
		s_true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}

/* xobjectp - is this an object? */
LVAL xobjectp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (objectp(arg) ? s_true : NIL);
}

/* xboundp - is this a value bound to this symbol? */
LVAL xboundp(V)
{
    LVAL sym;
    sym = xlgasymbol();
    xllastarg();
    return (boundp(sym) ? s_true : NIL);
}

/* xfboundp - is this a functional value bound to this symbol? */
LVAL xfboundp(V)
{
    LVAL sym;
    sym = xlgasymbol();
    xllastarg();
    return (fboundp(sym) ? s_true : NIL);
}

/* xconstantp - is this constant? TAA addition*/
LVAL xconstantp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();

    if ((!null(arg)) &&
        (((ntype(arg)==CONS) && (car(arg) != s_quote)) ||
         ((ntype(arg)==SYMBOL) && (!constantp(arg)))))
        return (NIL);
    return (s_true);
}

/* xspecialp - is the symbol marked special?  Luke Tierney 9/93 */
LVAL xspecialp(V)
{
  LVAL arg;
  arg = xlgetarg();
  xllastarg();
  return((symbolp(arg) && specialp(arg)) ? s_true : NIL);
}

/* xmarkspecial - mark the symbol as special Luke Tierney 9/93 */
LVAL xmarkspecial(V)
{
  LVAL arg, val;
  int constant, newvalue;
  arg = xlgasymbol();
  constant = moreargs() ? ! null(nextarg()) : FALSE;
  if (moreargs()) {
    newvalue = TRUE;
    val = nextarg();
  }
  else {
    newvalue = FALSE;
    val = NIL;
  }
  xllastarg();

  if (constant)
    setsconstant(arg);
  else if (! specialp(arg))
    setsspecial(arg);
  if (newvalue)
    setvalue(arg, val);
  return(NIL);
}    

/* xnull - is this null? */
LVAL xnull(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (null(arg) ? s_true : NIL);
}

/* xlistp - is this a list? */
LVAL xlistp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (listp(arg) ? s_true : NIL);
}

/* xendp - is this the end of a list? */
LVAL xendp(V)
{
    LVAL arg;
    arg = xlgalist();
    xllastarg();
    return (null(arg) ? s_true : NIL);
}

/* xconsp - is this a cons? */
LVAL xconsp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (consp(arg) ? s_true : NIL);
}

/* xvectorp - is this a vector? */
LVAL xvectorp(V)
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return ((vectorp(arg) || stringp(arg) || tvecp(arg)) ? s_true : NIL);
}

/* xeq - are these equal? */
LVAL xeq(V)
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (arg1 == arg2 ? s_true : NIL);
}

/* xeql - are these equal? */
LVAL xeql(V)
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (eql(arg1,arg2) ? s_true : NIL);
}

/* xequal - are these equal? (recursive) */
LVAL xequal(V)
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (equal(arg1,arg2) ? s_true : NIL);
}

/* xset - built-in function set */
LVAL xset(V)
{
    LVAL sym,val;

    /* get the symbol and new value */
    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    if (constantp(sym)) {
        xlnoassign(sym);
    }
    
    /* assign the symbol the value of argument 2 and the return value */
    setvalue(sym,val);

    /* return the result value */
    return (val);
}

/* xgensym - generate a symbol */
LVAL xgensym(V)
{
    char sym[STRMAX+11]; /* enough space for prefix and number */
    LVAL x;

    /* get the prefix or number */
    if (moreargs()) {
	x = xlgetarg();
	switch (null(x)? CONS : ntype(x)) { /* was ntype(x)   TAA Mod */
	case SYMBOL:
		x = getpname(x);
	case STRING:
		STRNCPY(gsprefix,getstring(x),STRMAX);
		gsprefix[STRMAX] = '\0';
		break;
	case FIXNUM:
		gsnumber = getfixnum(x);
		break;
	default:
		xlbadtype(x);
	}
    }
    xllastarg();

    /* create the pname of the new symbol */
    sprintf(sym,"%s%ld",gsprefix,(long) gsnumber++); /* TAA Fix 2/94 --
                                                originally considered gsnumber
                                                to be an int */

    /* make a symbol with this print name */
    return (xlmakesym(sym));
}

/* xmakesymbol - make a new uninterned symbol */
LVAL xmakesymbol(V)
{
    return (makesymbol(FALSE));
}

/* xintern - make a new interned symbol */
LVAL xintern(V)
{
    return (makesymbol(TRUE));
}

/* makesymbol - make a new symbol */
LOCAL LVAL makesymbol P1C(int, iflag)
{
    LVAL pname;
#ifdef PACKAGES
    LVAL pack = NIL;
#ifdef MULVALS
    LVAL sym;
    int found;
#endif /* MULVALS */
#endif /* PACKAGES */
    int i;

    /* get the print name of the symbol to intern */
    pname = xlgastring();
#ifdef PACKAGES
    if (iflag)
      pack = xlgetpackage((moreargs()) ? xlgetarg() : getvalue(s_package));
#endif /* PACKAGES */
    xllastarg();

    /* check for containing only printable characters */
    i = getslength(pname);
    if (i >= STRMAX)
        xlerror("too long", pname);
    while (i-- > 0) if (pname->n_string[i] < 32 )
        xlerror("non-printing characters",pname);

    /* make the symbol */
#ifdef PACKAGES
#ifdef MULVALS
    if (iflag) {
      /* TAA fix, 7/97, change order of next two statements and
       * added if condition */
      found = xlfindsymbol(getstring(pname), pack, &sym);
      if (found == SYM_NOT_FOUND)
	sym = xlintern(getstring(pname), pack);
      xlnumresults = 2;
      xlresults[0] = sym;
      switch (found) {
      case SYM_INTERNAL: xlresults[1] = k_internal; break;
      case SYM_EXTERNAL: xlresults[1] = k_external; break;
      case SYM_INHERITED: xlresults[1] = k_inherited; break;
      default: xlresults[1] = NIL;
      }
      return(sym);
    }
    else return(xlmakesym(getstring(pname)));
#else
    return (iflag ? xlintern(getstring(pname),pack)
    		  : xlmakesym(getstring(pname)));
#endif /* MULVALS */
#else
    return (iflag ? xlenter(getstring(pname))
    		  : xlmakesym(getstring(pname)));
#endif /* PACKAGES */
}

/* xsymname - get the print name of a symbol */
LVAL xsymname(V)
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* return the print name */
    return (getpname(sym));
}

/* xsymvalue - get the value of a symbol */
LVAL xsymvalue(V)
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* get the global value */
    while ((val = getvalue(sym)) == s_unbound)
	xlunbound(sym);

    /* return its value */
    return (val);
}

/* xsymfunction - get the functional value of a symbol */
LVAL xsymfunction(V)
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* get the global value */
    while ((val = getfunction(sym)) == s_unbound)
	xlfunbound(sym);

    /* return its value */
    return (val);
}

/* xsymplist - get the property list of a symbol */
LVAL xsymplist(V)
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* return the property list */
    return (getplist(sym));
}

/* xget - get the value of a property */
/* TAA MOD 7/93 -- added default argument */
LVAL xget(V)
{
    LVAL sym,prp,dflt=NIL;

    /* get the symbol and property */
    sym = xlgasymbol();
    prp = xlgetarg();
    if (moreargs()) dflt = xlgetarg();
    xllastarg();

    /* retrieve the property value */
    return (null(prp = findprop(getplist(sym),prp)) ? dflt : car(prp));
}

/* xgetf - get the value of a property  NEW 7/93 */
LVAL xgetf(V)
{
    LVAL plist,prp,dflt=NIL;

    /* get the plist and property */
    plist = xlgalist();
    prp = xlgetarg();
    if (moreargs()) dflt = xlgetarg();
    xllastarg();

    /* retrieve the property value */
    return (null(prp = findprop(plist,prp)) ? dflt : car(prp));
}

/* xputprop - set the value of a property */
LVAL xputprop(V)
{
    LVAL sym,val,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    val = xlgetarg();
    prp = xlgetarg();
    xllastarg();

    /* set the property value */
    xlputprop(sym,val,prp);

    /* return the value */
    return (val);
}

/* xremprop - remove a property value from a property list */
LVAL xremprop(V)
{
    LVAL sym,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    prp = xlgetarg();
    xllastarg();

    /* remove the property */
    xlremprop(sym,prp);

    /* return nil */
    return (NIL);
}

/* xhash - compute the hash value of a string or symbol */
/* TAA Modified to hash anything */
LVAL xhash(V)
{
    LVAL len,val;
    int n;

    /* get the object and the table length */
    val = xlgetarg();
    len = xlgafixnum(); n = (int)getfixnum(len);
    xllastarg();

    /* check for hash arg out of range */
    if (n <= 0) xlbadtype(len);

    /* return the hash index */
    return (cvfixnum((FIXTYPE)xlhash(val,n)));
}

/* xvector - make a vector */
LVAL xvector(V)
{
    LVAL val;
    int i;

    /* make the vector */
    val = newvector(xlargc);

    /* store each argument */
    for (i = 0; moreargs(); ++i)
	setelement(val,i,nextarg());
    xllastarg();

    /* return the vector */
    return (val);
}

#ifdef OLDERRORS    /* Normally we don't want to use this code! */
/* xerror - special form 'error' */
LVAL xerror(V)
{
    LVAL emsg,arg;

#ifdef CONDITIONS
    if (s_condition_hook != NULL && getvalue(s_condition_hook) != NIL)
      return(conditionhook(s_error));
#endif /* CONDITIONS */

    /* get the error message and the argument */
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    return (xlerror(getstring(emsg),arg));
}

/* xcerror - special form 'cerror' */
LVAL xcerror(V)
{
    LVAL cmsg,emsg,arg;

#ifdef CONDITIONS
    if (s_condition_hook != NULL && getvalue(s_condition_hook) != NIL)
      return(conditionhook(s_cerror));
#endif /* CONDITIONS */

    /* get the correction message, the error message, and the argument */
    cmsg = xlgastring();
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    xlcerror(getstring(cmsg),getstring(emsg),arg);

    /* return nil */
    return (NIL);
}

/* xbreak - special form 'break' */
LVAL xbreak(V)
{
    LVAL emsg,arg;

#ifdef CONDITIONS
    if (s_condition_hook != NULL && getvalue(s_condition_hook) != NIL)
      return(conditionhook(s_break));
#endif /* CONDITIONS */

    /* get the error message */
    emsg = (moreargs() ? xlgastring() : NIL);
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* enter the break loop */
    xlbreak((!null(emsg) ? getstring(emsg) : (char *)"**BREAK**"),arg);

    /* return nil */
    return (NIL);
}

#else

/* xerror - special form 'error' */
LVAL xerror(V)
{
    LVAL emsg;
    LVAL val;

#ifdef CONDITIONS
    if (s_condition_hook != NULL && getvalue(s_condition_hook) != NIL)
      return(conditionhook(s_error));
#endif /* CONDITIONS */

    xlsave1(val);
    val = newustream();

    /* get the error message */
    emsg = xlgastring();

    xlformat(emsg, val);

    val = getstroutput(val);
    
    /* we don't need xlpop here because we don't return! */

    /* signal the error */
    return (xlerror(getstring(val),s_unbound));
}

/* xcerror - special form 'cerror' */
LVAL xcerror(V)
{
    LVAL cmsg, emsg, val1, val2;
    LVAL *origargv;
    int origargc;

#ifdef CONDITIONS
    if (s_condition_hook != NULL && getvalue(s_condition_hook) != NIL)
      return(conditionhook(s_cerror));
#endif /* CONDITIONS */

    /* create the string streams */
    xlstkcheck(2);
    xlprotect(val1);
    val1 = newustream();
    xlprotect(val2);
    val2 = newustream();

    /* get the correction message and the error message */
    cmsg = xlgastring();
    emsg = xlgastring();

    /* process the message strings */
    origargv = xlargv;
    origargc = xlargc;
    xlformat(cmsg, val1);
    val1 = getstroutput(val1);
    xlargv = origargv;
    xlargc = origargc;
    xlformat(emsg, val2);
    val2 = getstroutput(val2);

    /* signal the error */
    xlcerror(getstring(val1),getstring(val2),s_unbound);

    xlpopn(2);

    /* return nil */
    return (NIL);
}

/* xbreak - special form 'break' */
LVAL xbreak(V)
{
    LVAL emsg;
    LVAL val;

#ifdef CONDITIONS
    if (s_condition_hook != NULL && getvalue(s_condition_hook) != NIL)
      return(conditionhook(s_break));
#endif /* CONDITIONS */

    xlsave1(val);
    val = newustream();

    /* get the error message */
    emsg = xlgastring();

    xlformat(emsg, val);

    val = getstroutput(val);
    
    /* enter the break loop */
    xlbreak(getstring(val), s_unbound);

    /* restore stack */
    xlpop();

    /* return nil */
    return (NIL);
}
#endif

#ifdef CONDITIONS
LVAL xsignal(V) { return(conditionhook(s_signal)); }
LVAL xwarn(V) { return(conditionhook(s_warn)); }
LVAL xdebug(V) { return(conditionhook(s_debug)); }
#endif /* CONDITION */

/* xcleanup - special form 'clean-up' */
LVAL xcleanup(V)
{
    xllastarg();
    xlcleanup();
    return (NIL);
}

/* xtoplevel - special form 'top-level' */
LVAL xtoplevel(V)
{
    int print;
    print = moreargs() ? ! null(nextarg()) : TRUE;
    xllastarg();
    xltoplevel(print);
    return (NIL);
}

/* xcontinue - special form 'continue' */
LVAL xcontinue(V)
{
    xllastarg();
    xlcontinue();
    return (NIL);
}

/* xevalhook - eval hook function */
LVAL xevalhook(V)
{
    LVAL expr,newehook,newahook,newenv,oldenv,oldfenv,olddenv,val;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(newenv);

    /* get the expression, the new hook functions and the environment */
    expr = xlgetarg();
    newehook = xlgetarg();
    newahook = xlgetarg();
    newenv = (moreargs() ? xlgalist() : NIL);
    xllastarg();

    /* bind *evalhook* and *applyhook* to the hook functions */
    olddenv = xldenv;
    xldbind(s_evalhook,newehook);
    xldbind(s_applyhook,newahook);

    /* establish the environment for the hook function */
#if 0   /* old way, if env is NIL then uses current environment */
    if (!null(newenv)) {
	oldenv = xlenv;
	oldfenv = xlfenv;
	xlenv = car(newenv);
	xlfenv = cdr(newenv);
    }
#else   /* TAA MOD -- if env is NIL then uses global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    if (!null(newenv)) {
        xlenv = car(newenv);
        xlfenv = cdr(newenv);
    }
    else {
        xlenv = xlfenv = NIL;
    }
#endif
    /* evaluate the expression (bypassing *evalhook*) */
    val = xlxeval(expr);

    /* restore the old environment */
    xlunbind(olddenv);
#if 0
    if (!null(newenv)) {
	xlenv = oldenv;
	xlfenv = oldfenv;
    }
#else
    xlenv = oldenv;
    xlfenv = oldfenv;
#endif

    /* restore the stack */
    xlpopn(3);

    /* return the result */
    return (val);
}

#ifdef APPLYHOOK
/* xapplyhook - apply hook function */
LVAL xapplyhook(V)
{
    LVAL fcn,args,newehook,newahook,olddenv,val;

    /* get the function, arguments, and the new hook functions */
    fcn = xlgetarg();
    args = xlgetarg();
    newehook = xlgetarg();
    newahook = xlgetarg();
    xllastarg();

    /* bind *evalhook* and *applyhook* to the hook functions */
    olddenv = xldenv;
    xldbind(s_evalhook,newehook);
    xldbind(s_applyhook,newahook);

    /* apply function (apply always bypasses hooks) */
    val = xlapply(pushargs(fcn,args));

    /* restore the old environment */
    xlunbind(olddenv);

    /* return the result */
    return (val);
}

#endif

#ifdef MULVALS
LVAL xvalues(V)
{
  int i;
  if (xlargc > MULVALLIMIT)
    xlfail("too many values ");
  for (i = 0; i < xlargc; i++)
    xlresults[i] = xlargv[i];
  xlnumresults = xlargc;
  return(xlargc > 0 ? xlargv[0] : NIL);
}
#endif /* MULVALS */

/* CLtL2 - compliant version of FUNCTIONP */
LVAL xfunctionp(V)
{
  LVAL x;
  x = xlgetarg();
  xllastarg();
  switch(ntype(x)) {
  case SUBR: return(s_true);
  case CLOSURE:
    return(gettype(x) == s_lambda ? s_true : NIL);
#ifdef BYTECODE
  case BCCLOSURE:
    return(getbcctype(x) == s_lambda ? s_true : NIL);
#endif /*BYTECODE */
  default: return(NIL);
  }
}

#ifdef CONDITIONS
LOCAL LVAL conditionhook P1C(LVAL, type)
{
  FRAMEP newfp,fp;
  LVAL olddenv,val,hook,args;
  int argc;

  /* rebind the hook functions to nil */
  hook = getvalue(s_condition_hook);
  olddenv = xldenv;
  xldbind(s_condition_hook,NIL);

  /* save a pointer */
  xlsave1(args);

  /* get the arglist */
  argc = xlargc + 3;
  args = makearglist(xlargc, xlargv);

  /* build a new argument stack frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(hook);
  pusharg(cvfixnum((FIXTYPE) argc));

  /* push the type symbol, the function, and the environments */
  pusharg(type);
  fp = null(xlfp[0]) ? xlfp : xlfp - getfixnum(*xlfp);
  pusharg(null(fp[0]) ? NIL : cvfixnum((FIXTYPE) (fp - xlargstkbase)));
  pusharg(cons(xlenv,xlfenv));

  /* push each argument */
  for (; consp(args); args = cdr(args))
    pusharg(car(args));

  /* establish the new stack frame */
  xlfp = newfp;

  /* apply the function */
  val = xlapply(argc);

  /* restore the stack */
  xlpop();

  /* unbind the symbols */
  xlunbind(olddenv);

  return(val);
}

LVAL xstackval(V)
{
  LVAL *sp;
  sp = xlargstkbase + getfixnum(xlgafixnum());
  xllastarg();
  if (xlargstkbase <= sp && sp < xlargstktop)
    return(*sp);
  else return(NIL);
}
#endif /* CONDITIONS */

#if (defined(TIMES) && defined(MULVALS) && defined(BIGNUMS))
LVAL xgetdectime(V)
{
  time_t t;
  struct tm t_local, t_gm;
  long secswest;

  xllastarg();
  t = time(NULL);
  t_local = *localtime(&t);
  t_gm = *gmtime(&t);
  secswest = difftime(mktime(&t_gm), mktime(&t_local));
  xlnumresults = 0;
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) t_local.tm_sec);
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) t_local.tm_min);
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) t_local.tm_hour);
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) t_local.tm_mday);
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) t_local.tm_mon + 1);
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) (t_local.tm_year + 1900));
  xlresults[xlnumresults++] = cvfixnum((FIXTYPE) (t_local.tm_wday + 6) % 7);

  /* The following treats unknown DST the same as no DST. This means
     both may be wrong if DST is in effect and mktime can't figure
     this out. */
  xlresults[xlnumresults++] = t_local.tm_isdst > 0 ? s_true : NIL;
  xlresults[xlnumresults++] = cvratio((FIXTYPE) secswest, (FIXTYPE) 3600);
  return xlresults[0];
}
#endif
