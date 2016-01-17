/* xleval - xlisp evaluator */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* macro to check for lambda list keywords */
#define iskey(s) ((s) == lk_optional \
               || (s) == lk_rest \
               || (s) == lk_key \
               || (s) == lk_aux \
               || (s) == lk_allow_other_keys \
               || (s) == lk_whole \
               || (s) == lk_body \
               || (s) == lk_environment)

/* macros to handle tracing */
#define trenter(sym,argc,argv) {if (!null(sym)) doenter(sym,argc,argv);}
#define trexit(sym,val) {if (!null(sym)) doexit(sym,val);}

/* local forward declarations */
LOCAL LVAL xlbadfunction P1H(LVAL);
LOCAL VOID badarglist(V);
/*LOCAL*/ VOID doenter P3H(LVAL, int, FRAMEP);
/*LOCAL*/ VOID doexit P2H(LVAL, LVAL);
LOCAL LVAL evalhook P1H(LVAL);
LOCAL LVAL evform P1H(LVAL);
LOCAL LVAL evfun P3H(LVAL, int, FRAMEP);
LOCAL int  evpushargs P2H(LVAL, LVAL);
LOCAL int  member P2H(LVAL, LVAL);
#ifndef XLISP_STAT
LOCAL LVAL member2 P3H(LVAL, LVAL, LVAL);
#endif /* XLISP_STAT */
#ifdef APPLYHOOK
LOCAL LVAL applyhook P2H(LVAL, LVAL);
#endif
#ifdef CONDITIONS
LOCAL VOID xlcondunbound P2H(LVAL, LVAL);
#endif /* CONDITIONS */

LOCAL LVAL xlbadfunction P1C(LVAL, arg)
{
  return xlerror("bad function",arg);
}

/* xleval - evaluate an xlisp expression (checking for *evalhook*) */
LVAL xleval P1C(LVAL, expr)
{
    /* check for control codes */
    if (--xlsample <= 0) {
	xlsample = SAMPLE;
	oscheck();
    }

    /* check for *evalhook* */
    if (!null(getvalue(s_evalhook)))
	return (evalhook(expr));

    /* dispatch on the node type */
    switch (ntype(expr)) {
    case CONS:
	return (evform(expr));
#ifdef BYTECODE
    case BCODE:
	return (BC_evform(expr));
#endif /* BYTECODE */
    case SYMBOL:
#ifdef MULVALS
	xlnumresults = 1;
	return (xlresults[0] = xlgetvalue(expr));
#else
	return (xlgetvalue(expr));
#endif /* MULVALS */
    default:
#ifdef MULVALS
	xlnumresults = 1;
	return (xlresults[0] = expr);
#else
	return (expr);
#endif /* MULVALS */
    }
}

/* xlxeval - evaluate an xlisp expression (bypassing *evalhook*) */
LVAL xlxeval P1C(LVAL, expr)
{
    /* dispatch on node type */
    switch (ntype(expr)) {
    case CONS:
	return (evform(expr));
#ifdef BYTECODE
    case BCODE:
	return (BC_evform(expr));
#endif /* BYTECODE */
    case SYMBOL:
#ifdef MULVALS
	xlnumresults = 1;
	return (xlresults[0] = xlgetvalue(expr));
#else
	return (xlgetvalue(expr));
#endif /* MULVALS */
    default:
#ifdef MULVALS
	xlnumresults = 1;
	return (xlresults[0] = expr);
#else
	return (expr);
#endif /* MULVALS */
    }
}

/* xlapply - apply a function to arguments (already on the stack) */
LVAL xlapply P1C(int, argc)
{
    LVAL fun,val;

    /* get the function */
    fun = xlfp[1];

    /* get the functional value of symbols */
    if (symbolp(fun)) {
	while ((val = getfunction(fun)) == s_unbound)
	    xlfunbound(fun);
	fun = xlfp[1] = val;
    }

    /* check for nil */
    if (null(fun))
	xlbadfunction(fun);

    /* dispatch on node type */
    switch (ntype(fun)) {
    case SUBR: {
        FRAMEP oldargv;
        int oldargc;
	oldargc = xlargc;
	oldargv = xlargv;
	xlargc = argc;
	xlargv = xlfp + 3;
	val = (*getsubr(fun))();
#ifdef MULVALS
	if (! mulvalp(fun)) {
	  xlnumresults = 1;
	  xlresults[0] = val;
	}
#endif /* MULVALS */
	xlargc = oldargc;
	xlargv = oldargv;
	break;
        }
    case CONS:
	if (!consp(cdr(fun)))
	    xlbadfunction(fun);
	if (car(fun) == s_lambda)
	    fun = xlfp[1]         /* TAA fix (vanNiekerk) */
		= xlclose(NIL,
	                  s_lambda,
	                  car(cdr(fun)),
	                  cdr(cdr(fun)),
	                  xlenv,xlfenv);
	else
	    xlbadfunction(fun);
	/**** fall through into the next case ****/
    case CLOSURE:
	if (gettype(fun) != s_lambda)
	    xlbadfunction(fun);
	val = evfun(fun,argc,xlfp+3);
	break;
#ifdef BYTECODE
    case BCCLOSURE:
	if (getbcctype(fun) != s_lambda)
	    xlbadfunction(fun);
        val = BC_evfun(fun,argc,xlfp+3);
	break;
#endif /* BYTECODE */
    default:
	xlbadfunction(fun);
	val = NIL; /* to keep compiler happy */
    }

    /* remove the call frame */
    xlsp = xlfp;
    xlfp = xlfp - (int)getfixnum(*xlfp);

    /* return the function value */
    return (val);
}

/* evform - evaluate a form */
LOCAL LVAL evform P1C(LVAL, form)
{
    LVAL fun,args,val;
    LVAL tracing=NIL;
    FRAMEP argv;
    int argc;
LVAL **oldst = xlstack, oform = form; /**** remove after debugging */

#ifdef STSZ
/* Debugging -- print system and eval stack remaining at each invocation */
/*  fprintf(stderr, "%d/%d  ",STACKREPORT(argc), xlstack-xlstkbase); */
    /* check the stack */
    stchck();
#endif

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(args);

    /* get the function and the argument list */
    fun = car(form);
    args = cdr(form);

    /* get the functional value of symbols */
    if (symbolp(fun)) {
	if (!null(getvalue(s_tracelist)) && member(fun,getvalue(s_tracelist)))
	    tracing = fun;
	fun = xlgetfunction(fun);
    }

    /* check for nil */
    if (null(fun))
	xlbadfunction(NIL);

    /* dispatch on node type */
    switch (ntype(fun)) {
    case SUBR:
#ifdef APPLYHOOK
        /* check for *applyhook* */
        if (!null(getvalue(s_applyhook))) {
            val = (applyhook(fun,args));
            break;
        }
#endif
	argv = xlargv;
	argc = xlargc;
	xlargc = evpushargs(fun,args);
	xlargv = xlfp + 3;
	trenter(tracing,xlargc,xlargv);
	val = (*getsubr(fun))();
#ifdef MULVALS
	if (! mulvalp(fun)) {
	  xlnumresults = 1;
	  xlresults[0] = val;
	}
#endif /* MULVALS */
	trexit(tracing,val);
	xlsp = xlfp;
	xlfp = xlfp - (int)getfixnum(*xlfp);
	xlargv = argv;
	xlargc = argc;
	break;
    case FSUBR:
	argv = xlargv;
	argc = xlargc;
	xlargc = pushargs(fun,args);
	xlargv = xlfp + 3;
	val = (*getsubr(fun))();
#ifdef MULVALS
	if (! mulvalp(fun)) {
	  xlnumresults = 1;
	  xlresults[0] = val;
	}
#endif /* MULVALS */
	xlsp = xlfp;
	xlfp = xlfp - (int)getfixnum(*xlfp);
	xlargv = argv;
	xlargc = argc;
	break;
    case CONS:
	if (!consp(cdr(fun)))
	    xlbadfunction(fun);
	if ((/* type = */ car(fun)) == s_lambda)
	    fun = xlclose(NIL,
	                  s_lambda,
	                  car(cdr(fun)),
	                  cdr(cdr(fun)),
	                  xlenv,xlfenv);
	else
	    xlbadfunction(fun);
	/**** fall through into the next case ****/
    case CLOSURE:
	if (gettype(fun) == s_lambda) {
#ifdef APPLYHOOK
            /* check for *applyhook* */
            if (!null(getvalue(s_applyhook))) {
                val = (applyhook(fun,args));
                break;
            }
#endif
	    argc = evpushargs(fun,args);
	    argv = xlfp + 3;
	    trenter(tracing,argc,argv);
	    val = evfun(fun,argc,argv);
	    trexit(tracing,val);
	    xlsp = xlfp;
	    xlfp = xlfp - (int)getfixnum(*xlfp);
	}
	else {
	    LVAL tmp = form;
	    macroexpand(fun,args,&tmp);
	    fun = tmp;
	    if (!null(getvalue(s_dispmacros)) && consp(fun)) {
		/* substitute back into original fcn */
		rplaca(form, car(fun));
		rplacd(form, cdr(fun));
	    }
	    val = xleval(fun);
	}
	break;
#ifdef BYTECODE
    case BCCLOSURE:
	if (getbcctype(fun) == s_lambda) {
#ifdef APPLYHOOK
            /* check for *applyhook* */
            if (!null(getvalue(s_applyhook))) {
                val = (applyhook(fun,args));
                break;
            }
#endif
	    argc = evpushargs(fun,args);
	    argv = xlfp + 3;
	    trenter(tracing,argc,argv);
	    val = BC_evfun(fun,argc,argv);
	    trexit(tracing,val);
	    xlsp = xlfp;
	    xlfp = xlfp - (int)getfixnum(*xlfp);
	}
	else {
	    LVAL tmp = form;
	    macroexpand(fun,args,&tmp);
	    fun = tmp;
	    if (!null(getvalue(s_dispmacros)) && consp(fun)) {
		/* substitute back into original fcn */
		rplaca(form, car(fun));
		rplacd(form, cdr(fun));
	    }
	    val = xleval(fun);
	}
	break;
#endif /* BYTECODE */
    default:
	xlbadfunction(fun);
	val = NIL; /* to keep compiler happy */
    }

    /* restore the stack */
    xlpopn(2);

if (oldst != xlstack) { /**** remove after debugging */
  stdputstr("stack messup - ");
  stdprint(oform);
}
    
    /* return the result value */
    return (val);
}

/* xlexpandmacros - expand macros in a form */
LVAL xlexpandmacros P1C(LVAL, form)
{
    LVAL fun,args;

    /* protect some pointers */
    xlstkcheck(3);
    xlprotect(form);
    xlsave(fun);
    xlsave(args);

    /* expand until the form isn't a macro call */
    while (consp(form)) {
	fun = car(form);		/* get the macro name */
	args = cdr(form);		/* get the arguments */
	if (! symbolp(fun) || (fun = xlxgetfunction(fun)) == s_unbound)
	    break;
	if (!macroexpand(fun,args,&form))
	    break;
    }

    /* restore the stack and return the expansion */
    xlpopn(3);
    return (form);
}

/* macroexpand - expand a macro call */
/* assumes form is passed as *pval */
int macroexpand P3C(LVAL, fun, LVAL, args, LVAL *, pval)
{
    FRAMEP argv;
    int argc;
    LVAL tmp1, tmp2;

    /* make sure it's really a macro call */
#ifdef BYTECODE
    if (! (bcclosurep(fun) && getbcctype(fun) == s_macro)
	&& ! (closurep(fun) && gettype(fun) == s_macro))
      return (FALSE);
#else
    if (! (closurep(fun) && gettype(fun) == s_macro))
      return (FALSE);
#endif

    /* call the expansion function */
    /* modified for CL-compliant form of expansion function */
    xlstkcheck(2);
    xlsave(tmp1);
    xlsave(tmp2);
#ifdef OLDMACROS
    tmp1 = cons(xlenv,xlfenv);
    tmp2 = copylist(*pval);
    tmp2 = cons(tmp2,args);
#else
    tmp1 = copylist(*pval);
    tmp2 = cons(xlenv,xlfenv);
    tmp2 = consa(tmp2);
#endif /* OLDMACROS */
    argc = pushargs(fun,cons(tmp1, tmp2));
    xlpopn(2);
    argv = xlfp + 3;
#ifdef BYTECODE
    if (bcclosurep(fun))
      *pval = BC_evfun(fun,argc,argv);
    else
      *pval = evfun(fun,argc,argv);
#else
    *pval = evfun(fun,argc,argv);
#endif /* BYTECODE */
    xlsp = xlfp;
    xlfp = xlfp - (int)getfixnum(*xlfp);
    return (TRUE);
}

/* evalhook - call the evalhook function */
LOCAL LVAL evalhook P1C(LVAL, expr)
{
    FRAMEP newfp;
    LVAL olddenv,val;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(getvalue(s_evalhook));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(expr);
    pusharg(cons(xlenv,xlfenv));
    xlfp = newfp;

    /* rebind the hook functions to nil */
    olddenv = xldenv;
    xldbind(s_evalhook,NIL);
    xldbind(s_applyhook,NIL);

    /* call the hook function */
    val = xlapply(2);

    /* unbind the symbols */
    xlunbind(olddenv);

    /* return the value */
    return (val);
}

#ifdef APPLYHOOK
/* applyhook - call the applyhook function */
LOCAL LVAL applyhook P2C(LVAL, fun, LVAL, args)
{
    FRAMEP newfp;
    LVAL olddenv,val,last,next;

    xlsave1(val);   /* protect against GC */

    if (consp(args)) { /* build argument list -- if there are any */
        /* we will pass evaluated arguments, with hooks enabled */
        /* so argument evaluation will be hooked too */
        val = last = consa(xleval(car(args)));
        args = cdr(args);
        while (consp(args)) { /* handle any more in loop */
            next = consa(xleval(car(args)));
            rplacd(last,next);
            last = next;
            args = cdr(args);
        }
    }

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(getvalue(s_applyhook));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(fun);
    pusharg(val);
    xlfp = newfp;

    /* rebind hook functions to NIL */

    olddenv = xldenv;
    xldbind(s_evalhook,NIL);
    xldbind(s_applyhook,NIL);


    /* call the hook function */
    val = xlapply(2);

    /* unbind the symbols */
    xlunbind(olddenv);

    /* return the value */
    return (val);
}
#endif

/* evpushargs - evaluate and push a list of arguments */
LOCAL int evpushargs P2C(LVAL, fun, LVAL, args)
{
    FRAMEP newfp;
    int argc;

    /* protect the argument list */
    xlprot1(args);

    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(NIL); /* will be argc */

    /* evaluate and push each argument */
    for (argc = 0; consp(args); args = cdr(args), ++argc)
	pusharg(xleval(car(args)));

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* restore the stack */
    xlpop();

    /* return the number of arguments */
    return (argc);
}

/* pushargs - push a list of arguments */
int pushargs P2C(LVAL, fun, LVAL, args)
{
    FRAMEP newfp;
    int argc;

    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(NIL); /* will be argc */

    /* push each argument */
    for (argc = 0; consp(args); args = cdr(args), ++argc)
	pusharg(car(args));

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* return the number of arguments */
    return (argc);
}

/* makearglist - make a list of the remaining arguments */
LVAL makearglist P2C(int, argc, LVAL *, argv)
{
    LVAL list,this,last;
    xlsave1(list);
    for (last = NIL; --argc >= 0; last = this) {
	this = cons(*argv++,NIL);
	if (!null(last)) rplacd(last,this);
	else list = this;
	last = this;
    }
    xlpop();
    return (list);
}

/* evfun - evaluate a function */
LOCAL LVAL evfun P3C(LVAL, fun, int, argc, FRAMEP, argv)
{
    LVAL oldenv,oldfenv,cptr,val;
    LVAL olddenv=xldenv;
    CONTEXT cntxt;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(cptr);

    /* create a new environment frame */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlframe(getenvi(fun));
    xlfenv = getfenv(fun);

    /* bind the formal parameters */
    xlabind(fun,argc,argv);

    /* setup the implicit block */
    if (!null(getname(fun)))
	xlbegin(&cntxt,CF_RETURN,getname(fun));

    /* execute the block */
#ifdef CRAYCC
    if (null(getname(fun))) goto noname;
    if (XL_SETJMP(cntxt.c_jmpbuf))
        val = xlvalue;
    else {
    noname:
#else
    if (!null(getname(fun)) && XL_SETJMP(cntxt.c_jmpbuf))
	val = xlvalue;
    else {
#endif /* CRAYCC */
#ifdef LEXBIND
        if (!null(getname(fun)))
	  xlbindtag(&cntxt,getname(fun),xlenv);
#endif
#ifdef MULVALS
        xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	for (val = NIL, cptr = getbody(fun); consp(cptr); cptr = cdr(cptr)) {

	  /* check for control codes */
	  if (--xlsample <= 0) {
	    xlsample = SAMPLE;
	    oscheck();
	  }

	  val = car(cptr);

	  /* check for *evalhook* */
	  if (!null(getvalue(s_evalhook))) {
	    val = evalhook(val);
	    continue;
	  }

	  /* dispatch on the node type */
	  switch (ntype(val)) {
	  case CONS:
	    val = evform(val);
	    break;
#ifdef MULVALS
	  case SYMBOL:
	    val = xlgetvalue(val);
	    /* fall through */
	  default:
	    xlnumresults = 1;
	    xlresults[0] = val;
	    break;
#else
	  case SYMBOL:
	    val = xlgetvalue(val);
	    break;
	  default: /* nothing */
	    break;
#endif /* MULVALS */
	  }
        }
    }

    /* finish the block context */
    if (!null(getname(fun)))
	xlend(&cntxt);

    /* restore the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;
    xlunbind(olddenv);

    /* restore the stack */
    xlpopn(3);

    /* return the result value */
    return (val);
}

/* xlclose - create a function closure */
LVAL xlclose P6C(LVAL, name, LVAL, type, LVAL, fargs, LVAL, body, LVAL, env, LVAL, fenv)
{
    LVAL closure,key=NULL,arg,def,svar,new,last;
#ifndef PACKAGES
    char keyname[STRMAX+2];
#endif /* PACKAGES */
    LVAL wholesym=NULL,envsym=NULL;
    int destruct = FALSE;

    /* protect some pointers */
    xlsave1(closure);
    xlprot1(fargs);

    /* create the closure object */
    closure = newclosure(name,type,env,fenv);
    setlambda(closure,fargs);
    setbody(closure,body);

    /* check for &whole and &environment in macros */
    if (type == s_macro) {
      LVAL next, last;

      fargs = copylist(fargs);
      envsym = s_unbound;

      /* check for &whole argument */
      if (consp(fargs) && car(fargs) == lk_whole) {
	fargs = cdr(fargs);
	if (consp(fargs) && (!null(arg = car(fargs))) &&
	    symbolp(arg) && !iskey(arg))
	  wholesym = arg;
	else
	  badarglist();
	fargs = cdr(fargs);
      }
      else {
	wholesym = s_unbound;
	envsym = s_unbound;
      }

      /* check for &environment argument */
      for (next = fargs, last = NIL;
	   consp(next);
	   last = next, next = cdr(next)) {
	if (car(next) == lk_environment) {
	  if (consp(cdr(next)) && (!null(arg = car(cdr(next)))) &&
	      symbolp(arg) && !iskey(arg))
	    envsym = arg;
	  else
	    badarglist();
	  if (null(last))
	    fargs = cdr(cdr(fargs));
	  else
	    rplacd(last, cdr(cdr(next)));
	  break;
	}
      }

      /* replace &body by &rest */
      for (next = fargs; consp(next); next = cdr(next))
	if (car(next) == lk_body)
	  rplaca(next, lk_rest);

      /* replace dotted list by &rest */
      for (next = fargs, last = NIL;
	   consp(next);
	   last = next, next = cdr(next));
      if (consp(last) && ! null(arg = cdr(last))) {
	if (symbolp(arg) && !iskey(arg))
	  rplacd(last, cons(lk_rest, cons(arg, NIL)));
	else
	  badarglist();
      }

      /* check for destructuring */
      for (next = fargs; consp(next) && !iskey(car(next)); next = cdr(next))
	 if (consp(car(next))) {
	   if (fboundp(s_destructbind)) {
	     destruct = TRUE;
	     break;
	   }
	   else
	     xlfail("destructuring macro arglists not supported yet");
	 }

      if (destruct) {
	LVAL sym;
	xlsave1(sym);
	sym = xlmakesym("R");
	setbody(closure,
		consa(cons(s_destructbind, cons(fargs, cons(sym, body)))));
	fargs = cons(lk_rest, consa(sym));
	xlpop();
      }
      setlambda(closure, fargs);
    }

    /* handle each required argument */
    last = NIL;
    while (consp(fargs) && (!null(arg = car(fargs))) && !iskey(arg)) {

	/* make sure the argument is a symbol */
	if (!symbolp(arg))
	    badarglist();

	/* create a new argument list entry */
	new = cons(arg,NIL);

	/* link it into the required argument list */
	if (!null(last))
	    rplacd(last,new);
	else
	    setargs(closure,new);
	last = new;

	/* move the formal argument list pointer ahead */
	fargs = cdr(fargs);
    }
    if (type == s_macro)
      setargs(closure,cons(envsym,cons(wholesym,getargs(closure))));

    /* check for the '&optional' keyword */
    if (consp(fargs) && car(fargs) == lk_optional) {
	fargs = cdr(fargs);

	/* handle each optional argument */
	last = NIL;
	while (consp(fargs) && (!null(arg = car(fargs))) && !iskey(arg)) {

	    /* get the default expression and specified-p variable */
	    def = svar = NIL;
	    if (consp(arg)) {
		if (!null(def = cdr(arg)))
		    if (consp(def)) {
			if (!null(svar = cdr(def)))
			    if (consp(svar)) {
				svar = car(svar);
				if (!symbolp(svar))
				    badarglist();
			    }
			    else
				badarglist();
			def = car(def);
		    }
		    else
			badarglist();
		arg = car(arg);
	    }

	    /* make sure the argument is a symbol */
	    if (!symbolp(arg))
		badarglist();

	    /* create a fully expanded optional expression */
	    new = cons(cons(arg,cons(def,cons(svar,NIL))),NIL);

	    /* link it into the optional argument list */
	    if (!null(last))
		rplacd(last,new);
	    else
		setoargs(closure,new);
	    last = new;

	    /* move the formal argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* check for the '&rest' keyword */
    if (consp(fargs)
	&& (car(fargs) == lk_rest
	    || (type == s_macro && car(fargs) == lk_body))) {
        fargs = cdr(fargs);

	/* get the &rest argument */
	if (consp(fargs) && (!null((arg = car(fargs)))) && !iskey(arg) && symbolp(arg))
	    setrest(closure,arg);
	else
	    badarglist();

	/* move the formal argument list pointer ahead */
	fargs = cdr(fargs);
    }

    /* check for the '&key' keyword */
    if (consp(fargs) && car(fargs) == lk_key) {
	fargs = cdr(fargs);

	/* handle each key argument */
	last = NIL;
	while (consp(fargs) && (!null(arg = car(fargs))) && !iskey(arg)) {

	    /* get the default expression and specified-p variable */
	    def = svar = NIL;
	    if (consp(arg)) {
		if (!null(def = cdr(arg)))
		    if (consp(def)) {
			if (!null(svar = cdr(def)))
			    if (consp(svar)) {
				svar = car(svar);
				if (!symbolp(svar))
				    badarglist();
			    }
			    else
				badarglist();
			def = car(def);
		    }
		    else
			badarglist();
		arg = car(arg);
	    }

	    /* get the keyword and the variable */
	    if (consp(arg)) {
		key = car(arg);
		if (!symbolp(key))
		    badarglist();
		if (!null(arg = cdr(arg)))
		    if (consp(arg))
			arg = car(arg);
		    else
			badarglist();
	    }
	    else if (symbolp(arg)) {
#ifdef PACKAGES
		key = xlintern(getstring(getpname(arg)), xlkeypack);
#else
		strcpy(keyname,":");
		STRCAT(keyname,getstring(getpname(arg)));
		key = xlenter(keyname);
#endif /* PACKAGES */
	    }

	    /* make sure the argument is a symbol */
	    if (!symbolp(arg))
		badarglist();

	    /* create a fully expanded key expression */
	    new = cons(cons(key,cons(arg,cons(def,cons(svar,NIL)))),NIL);

	    /* link it into the optional argument list */
	    if (!null(last))
		rplacd(last,new);
	    else
		setkargs(closure,new);
	    last = new;

	    /* move the formal argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* check for the '&allow-other-keys' keyword */
    if (consp(fargs) && car(fargs) == lk_allow_other_keys)  {
        /* save marker that other keys are allowed */
        setkargs(closure,cons(lk_allow_other_keys,getkargs(closure)));
	fargs = cdr(fargs);
    }

    /* check for the '&aux' keyword */
    if (consp(fargs) && car(fargs) == lk_aux) {
	fargs = cdr(fargs);

	/* handle each aux argument */
	last = NIL;
	while (consp(fargs) && (!null(arg = car(fargs))) && !iskey(arg)) {

	    /* get the initial value */
	    def = NIL;
	    if (consp(arg)) {
		if (!null(def = cdr(arg)))
		    if (consp(def))
			def = car(def);
		    else
			badarglist();
		arg = car(arg);
	    }

	    /* make sure the argument is a symbol */
	    if (!symbolp(arg))
		badarglist();

	    /* create a fully expanded aux expression */
	    new = cons(cons(arg,cons(def,NIL)),NIL);

	    /* link it into the aux argument list */
	    if (!null(last))
		rplacd(last,new);
	    else
		setaargs(closure,new);
	    last = new;

	    /* move the formal argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* make sure this is the end of the formal argument list */
    if (!null(fargs))
      badarglist();

    /* restore the stack */
    xlpopn(2);

#ifndef OLDMACROS
    /* now if it is a macro fix it up to be CL-compliant */
    if (type == s_macro) {
      LVAL wsym, esym, fun, tmp;
      xlstkcheck(5);
      xlprotect(closure);
      xlsave(wsym);
      xlsave(esym);
      xlsave(fun);
      xlsave(tmp);
      esym = car(getargs(closure));
      wsym = car(cdr(getargs(closure)));
      if (esym == s_unbound) esym = xlmakesym("E");
      if (wsym == s_unbound) wsym = xlmakesym("W");
      if (destruct) {
	tmp = cdr(cdr(car(getbody(closure))));
	rplaca(tmp, cons(s_cdr, consa(wsym)));
      }
      else {
	fun = cons(s_lambda, cons(getlambda(closure), getbody(closure)));
	fun = cons(s_function, consa(fun));
	tmp = cons(s_apply, cons(fun, consa(cons(s_cdr, consa(wsym)))));
	setbody(closure, consa(tmp));
      }
      setargs(closure, cons(wsym, consa(esym)));
      setlambda(closure, cons(wsym, consa(esym)));
      setoargs(closure, NIL);
      setrest(closure, NIL);
      setkargs(closure, NIL);
      setaargs(closure, NIL);
      xlpopn(5);
    }
#endif /* OLDMACROS */
      
    /* return the new closure */
    return (closure);
}

/* xlabind - bind the arguments for a function */
VOID xlabind P3C(LVAL, fun, int, argc, LVAL *, argv)
{
    LVAL *kargv,fargs,key,arg,def,svar,p;
    int keycount=0;
    int rargc,kargc;

    /* protect some pointers */
    xlsave1(def);

    /* bind each required argument */
    for (fargs = getargs(fun); consp(fargs); fargs = cdr(fargs)) {

	/* make sure there is an actual argument */
	if (--argc < 0)
	    xltoofew();

        if (constantp(car(fargs))) xlnoassign(car(fargs));

	/* bind the formal variable to the argument value */
	xlbind(car(fargs),*argv++);
    }

    /* bind each optional argument */
    for (fargs = getoargs(fun); consp(fargs); fargs = cdr(fargs)) {

	/* get argument, default and specified-p variable */
	p = car(fargs);
	arg = car(p); p = cdr(p);
	def = car(p); p = cdr(p);
	svar = car(p);

        if (constantp(arg)) xlnoassign(arg);
        if ((!null(svar)) && constantp(svar)) xlnoassign(svar);

	/* bind the formal variable to the argument value */
	if (argc > 0) {
	    argc--;
	    xlbind(arg,*argv++);
	    if (!null(svar)) xlbind(svar, s_true);
	}

	/* bind the formal variable to the default value */
	else {
	    if (!null(def)) def = xleval(def);
	    xlbind(arg,def);
	    if (!null(svar)) xlbind(svar,NIL);
	}
    }

    /* save the count of the &rest of the argument list */
    rargc = argc;

    /* handle '&rest' argument */
    if (!null(arg = getrest(fun))) {
	if (constantp(arg)) xlnoassign(arg);
	def = makearglist(argc,argv);
	xlbind(arg,def);
	argc = 0;
    }

    /* handle '&key' arguments */
    if (!null(fargs = getkargs(fun))) {
	if (rargc & 1)  /* TAA Mod added check -- 9/93 */
	    xlfail( "keyword value missing");
	if (car(fargs) == lk_allow_other_keys)
	    fargs = cdr(fargs);     /* toss marker */
        else
            keycount = (rargc+1)/2; /* number of keyword arguments */

	for (; consp(fargs); fargs = cdr(fargs)) {

	    /* get keyword, argument, default and specified-p variable */
	    p = car(fargs);
	    key = car(p); p = cdr(p);
	    arg = car(p); p = cdr(p);
	    def = car(p); p = cdr(p);
	    svar = car(p);

            if (constantp(arg)) xlnoassign(arg);
            if (!null(svar) && constantp(svar)) xlnoassign(svar);

	    /* look for the keyword in the actual argument list */
	    for (kargv = argv, kargc = rargc; (kargc -= 2) >= 0; kargv += 2)
		if (*kargv == key)
		    break;

	    /* bind the formal variable to the argument value */
	    if (kargc >= 0) {
	        keycount--;
		xlbind(arg,*++kargv);
		if (!null(svar)) xlbind(svar, s_true);
	        /* "delete" any duplicate arguments  TAA Added 9/93 */
                for (;(kargc -= 2) >= 0 ; kargv++)
                    if (*++kargv == key) keycount--;

            }

	    /* bind the formal variable to the default value */
	    else {
		if (!null(def)) def = xleval(def);
		xlbind(arg,def);
		if (!null(svar)) xlbind(svar,NIL);
	    }
	}

        if (keycount > 0 && !null(getvalue(s_strict_keywords))) {
	  /* some keyword args were left over, and ! &allow-other-keys */
	  for (kargv = argv, kargc = rargc; kargc > 0; kargc -= 2, kargv += 2)
	    if (kargv[0] == k_allow_other_keys && !null(kargv[1]))
	      break;

	  if (kargc == 0)
	    xlfail("too many or invalid keyword arguments");
	}
	argc = 0;
    }

    /* check for the '&aux' keyword */
    for (fargs = getaargs(fun); consp(fargs); fargs = cdr(fargs)) {

	/* get argument and default */
	p = car(fargs);
	arg = car(p); p = cdr(p);
	def = car(p);

        if (constantp(arg)) xlnoassign(arg);

	/* bind the auxiliary variable to the initial value */
	if (!null(def)) def = xleval(def);
	xlbind(arg,def);
    }

    /* make sure there aren't too many arguments */
    if (argc > 0)
	xltoomany();

    /* restore the stack */
    xlpop();
}

#ifndef XLISP_STAT
/* evmethod - evaluate a method */
LVAL evmethod(obj,msgcls,message)
  LVAL obj,msgcls,message;
{
    LVAL oldenv,oldfenv,cptr,name,val;
    LVAL olddenv=xldenv;
    LVAL tracing = NIL;
    CONTEXT cntxt;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(cptr);

    /* create an 'object' stack entry and a new environment frame */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = cons(cons(obj,msgcls),getenvi(cdr(message)));
    xlenv = xlframe(xlenv);
    xlfenv = getfenv(cdr(message));

    /* bind the formal parameters */
    xlabind(cdr(message),xlargc,xlargv);

    /* check for and start tracing  TAA 9/1/96 */
    if (!null(getvalue(s_tracelist)) &&
        (tracing = member2(msgcls, car(message), getvalue(s_tracelist))) != NIL) {
        trenter(tracing,xlargc,xlargv);
    }

    /* setup the implicit block */
    if (!null(name = getname(cdr(message))))
        xlbegin(&cntxt,CF_RETURN,name);

    /* execute the block */
    if (!null(name) && XL_SETJMP(cntxt.c_jmpbuf))
        val = xlvalue;
    else {
#ifdef LEXBIND
        if (!null(name))
          xlbindtag(&cntxt,name,xlenv);
#endif
#ifdef MULVALS
        xlnumresults = 1;
        xlresults[0] = NIL;
#endif /* MULVALS */
        for (val = NIL, cptr = getbody(cdr(message)); consp(cptr); cptr = cdr(cptr))
            val = xleval(car(cptr));
    }

    /* finish the block context */
    if (!null(name))
        xlend(&cntxt);

    /* end tracing */
    trexit(tracing, val);

    /* restore the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;
    xlunbind(olddenv);

    /* restore the stack */
    xlpopn(3);

    /* return the result value */
    return (val);
}
#endif /* XLISP_STAT */

/* doenter - print trace information on function entry */
/* LOCAL VOID doenter(sym,argc,argv) *//* made global for method tracing */
VOID doenter P3C(LVAL, sym, int, argc, FRAMEP, argv)
{
    int i;
    LVAL olddenv;

    /* indent to the current trace level */
    for (i = 0; i < xltrcindent; ++i)
	trcputstr(" ");
    ++xltrcindent;

    /* rebind tracelist during printing - L. Tierney */
    olddenv = xldenv;
    xldbind(s_tracelist,NIL);
	
    /* display the function call */
    if (consp(sym)) {
        sprintf(buf,"Entering: %s,%s, Argument list: (",
                getstring(getivar(car(sym), PNAME)),
                getstring(getpname(car(cdr(sym)))));
    }
    else 
        sprintf(buf,"Entering: %s, Argument list: (",
                getstring(getpname(sym)));
    trcputstr(buf);
    while (--argc >= 0) {
	trcprin1(*argv++);
	if (argc) trcputstr(" ");
    }
    trcputstr(")\n");

    /* unbind the symbols - L. Tierney */
    xlunbind(olddenv);
}

/* doexit - print trace information for function/macro exit */
/* LOCAL VOID doexit(sym,val) *//* made global for method tracing */
VOID doexit P2C(LVAL, sym, LVAL, val)
{
#ifdef MULVALS
    extern int xltrcindent;
    int i, n;
    LVAL olddenv, *oldsp;
    
    /* indent to the current trace level */
    --xltrcindent;
    for (i = 0; i < xltrcindent; ++i)
	trcputstr(" ");
    
    /* rebind tracelist during printing - L. Tierney */
    olddenv = xldenv;
    xldbind(s_tracelist,NIL);
	
    /* save the results on the stack */
    oldsp = xlsp;
    n = xlnumresults;
    for (i = 0; i < n; i++)
      pusharg(xlresults[i]);

    /* display the function values */
    switch (n) {
    case 0:
      sprintf(buf,"Exiting: %s, No values. ",
	      getstring(getpname(consp(sym)?car(cdr(sym)):sym)));
      break;
    case 1:
      sprintf(buf,"Exiting: %s, Value: ",
	      getstring(getpname(consp(sym)?car(cdr(sym)):sym)));
      break;
    default:
      sprintf(buf,"Exiting: %s, Values: ",
	      getstring(getpname(consp(sym)?car(cdr(sym)):sym)));
    }
    trcputstr(buf);
    for (i = 0; i < n; i++) {
      trcprin1(oldsp[i]);
      if (i < n - 1) trcputstr(" ");
    }
    trcputstr("\n");

    /* restore the results and the stack */
    for (i = 0; i < n; i++)
      xlresults[i] = oldsp[i];
    xlnumresults = n;
    xlsp = oldsp;

    /* unbind the symbols - L. Tierney */
    xlunbind(olddenv);
#else
    int i;
    LVAL olddenv;

    /* indent to the current trace level */
    --xltrcindent;
    for (i = 0; i < xltrcindent; ++i)
	trcputstr(" ");

    /* rebind tracelist during printing - L. Tierney */
    olddenv = xldenv;
    xldbind(s_tracelist,NIL);
	
    /* display the function value */
    sprintf(buf,"Exiting: %s, Value: ",
            getstring(getpname(consp(sym)?car(cdr(sym)):sym)));
    trcputstr(buf);
    trcprin1(val);
    trcputstr("\n");

    /* unbind the symbols - L. Tierney */
    xlunbind(olddenv);
#endif /* MULVALS */
}

/* member - is 'x' a member of 'list'? */
LOCAL int member P2C(LVAL, x, LVAL, list)
{
    for (; consp(list); list = cdr(list))
	if (x == car(list))
	    return (TRUE);
    return (FALSE);
}

#ifndef XLISP_STAT
/* member2 - is '(x y)' a member of 'list'? */
LOCAL LVAL member2(x, y,list)
  LVAL x, y, list;
{
    LVAL cl;
    for (; consp(list); list = cdr(list)) 
        if (consp((cl=car(list))) && x == car(cl) && y == car(cdr(cl)))
            return (cl);

    return (NIL);
}
#endif /* XLISP_STAT */

/* xlunbound - signal an unbound variable error */
VOID xlunbound P1C(LVAL, sym)
{
#ifdef CONDITIONS
  if (! null(getvalue(s_condition_hook)))
    xlcondunbound(s_unboundvar, sym);
  else
#endif /* CONDITIONS */
    xlcerror("try evaluating symbol again","unbound variable",sym);
}

/* xlfunbound - signal an unbound function error */
VOID xlfunbound P1C(LVAL, sym)
{
#ifdef CONDITIONS
  if (! null(getvalue(s_condition_hook)))
    xlcondunbound(s_unboundfun, sym);
  else
#endif /* CONDITIONS */
    xlcerror("try evaluating symbol again","unbound function",sym);
}

/* xlstkoverflow - signal a stack overflow error */
VOID xlstkoverflow(V)
{
    xlabort("evaluation stack overflow");
}

/* xlargstkoverflow - signal an argument stack overflow error */
VOID xlargstkoverflow(V)
{
    xlabort("argument stack overflow");
}

/* badarglist - report a bad argument list error */
LOCAL VOID badarglist(V)
{
    xlfail("bad formal argument list");
}

#ifdef CONDITIONS
LOCAL VOID xlcondunbound P2C(LVAL, type, LVAL, sym)
{
  FRAMEP newfp, fp;

  /* build a new argument stack frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(getvalue(s_condition_hook));
  pusharg(cvfixnum((FIXTYPE) 7));
  pusharg(s_cerror);
  fp = null(xlfp[0]) ? xlfp : xlfp - getfixnum(*xlfp);
  pusharg(null(fp[0]) ? NIL : cvfixnum((FIXTYPE) (fp - xlargstkbase)));
  pusharg(cons(xlenv,xlfenv));
  pusharg(cvstring("Try evaluating the symbol ~*~s again."));
  pusharg(type);
  pusharg(k_name);
  pusharg(sym);

  /* establish the new stack frame */
  xlfp = newfp;

  /* apply the function */
  xlapply(7);
}
#endif /* CONDITIONS */
