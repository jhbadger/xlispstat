/* xlcont - xlisp special forms */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* forward declarations */
LOCAL LVAL evarg P1H(LVAL *);
LOCAL LVAL match P2H(int, LVAL *);
LOCAL LVAL evmatch P2H(int, LVAL *);
LOCAL VOID placeform P2H(LVAL, LVAL *);
LOCAL LVAL setffunction P3H(LVAL, LVAL, LVAL);
LOCAL LVAL setffunctionl P3H(LVAL, LVAL, LVAL);
LOCAL VOID doupdates P2H(LVAL, int);
LOCAL VOID tagbody(V);
LOCAL int  keypresent P2H(LVAL, LVAL);
LOCAL VOID dobindings P4H(LVAL, LVAL, LVAL *, int);
LOCAL VOID toofew P1H(LVAL);
LOCAL VOID toomany P1H(LVAL);
LOCAL LVAL bquote1 P1H(LVAL);
LOCAL LVAL let P1H(int);
LOCAL LVAL flet P2H(LVAL, int);
LOCAL LVAL prog P1H(int);
LOCAL LVAL progx P1H(int);
LOCAL LVAL doloop P1H(int);

/* dummy node type for a list */
#define LIST	-1

/* toofew - too few arguments */
LOCAL VOID toofew P1C(LVAL, args)
{
    xlerror("too few arguments",args);
}

/* toomany - too many arguments */
LOCAL VOID toomany P1C(LVAL, args)
{
    xlerror("too many arguments",args);
}

/* xquote - special form 'quote' */
LVAL xquote(V)
{
    LVAL val;
    val = xlgetarg();
    xllastarg();
    return (val);
}

/* xfunction - special form 'function' */
LVAL xfunction(V)
{
    LVAL val;

    /* get the argument */
    val = xlgetarg();
    xllastarg();

    /* create a closure for lambda expressions */
    if (consp(val) && car(val) == s_lambda && consp(cdr(val)))
	val = xlclose(NIL,s_lambda,car(cdr(val)),cdr(cdr(val)),xlenv,xlfenv);

    /* otherwise, get the value of a symbol */
    else if (symbolp(val))
	val = xlgetfunction(val);

    /* otherwise, its an error */
    else
	xlerror("not a function",val);

    /* return the function */
    return (val);
}

/* xcomplement - create a complementary function */
LVAL xcomplement(V)
{
    LVAL val;
    LVAL args, body;
    LVAL newxlenv;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(newxlenv);
    xlsave(args);
    xlsave(body);


    /* get the argument */
    val = xlgetarg();
    xllastarg();

    /* build the argument list (&rest x) */
    args = cons(lk_rest, consa(s_x));

    /* build body (not (apply s x)) */
    body = consa(cons(s_not, consa(cons(s_apply, cons(s_s, consa(s_x))))));

    /* create a closure for lambda expressions */
    newxlenv = xlframe(newxlenv);
    xlpbind(s_s, val, newxlenv);
    val = xlclose(NIL,s_lambda,args,body,newxlenv,NIL);

    /* unprotect pointers */
    xlpopn(3);

    /* return the function */
    return (val);
}

/* bquote1 - back quote helper function */
LOCAL LVAL bquote1 P1C(LVAL, expr)
{
    LVAL val,list,last,new;

    /* handle atoms */
    if (atom(expr))
	val = expr;

    /* handle (comma <expr>) */
    else if (car(expr) == s_comma) {
	if (atom(cdr(expr)))
	    xlfail("bad comma expression");
	val = xleval(car(cdr(expr)));
    }

    /* handle ((comma-at <expr>) ... ) */
    else if (consp(car(expr)) && car(car(expr)) == s_comat) {
	xlstkcheck(3);
	xlsave(list);
	xlsave(val);
	xlprotect(expr);        /* JSP fix 12/96 */
	if (atom(cdr(car(expr))))
	    xlfail("bad comma-at expression");
	list = xleval(car(cdr(car(expr))));
        if (!listp(list)) xlerror("not a list", list); /* ADDED 5/94, from Gottfried Ira */
	for (last = NIL; consp(list); list = cdr(list)) {
	    new = consa(car(list));
	    if (!null(last))
		rplacd(last,new);
	    else
		val = new;
	    last = new;
	}
	if (!null(last))
	    rplacd(last,bquote1(cdr(expr)));
	else
	    val = bquote1(cdr(expr));
	xlpopn(3);
    }

    /* handle any other list */
    else {
	xlsave1(val);
	val = consa(NIL);
	rplaca(val,bquote1(car(expr)));
	rplacd(val,bquote1(cdr(expr)));
	xlpop();
    }

    /* return the result */
    return (val);
}

/* xbquote - back quote special form */
LVAL xbquote(V)
{
    LVAL expr;

    /* get the expression */
    expr = xlgetarg();
    xllastarg();

    /* fill in the template */
    return (bquote1(expr));
}

/* xlambda - special form 'lambda' */
LVAL xlambda(V)
{
    LVAL fargs,arglist,val;

    /* get the formal argument list and function body */
    xlsave1(arglist);
    fargs = xlgalist();
    arglist = makearglist(xlargc,xlargv);

    /* create a new function definition */
    val = xlclose(NIL,s_lambda,fargs,arglist,xlenv,xlfenv);

    /* restore the stack and return the closure */
    xlpop();
    return (val);
}

/* xgetlambda - get the lambda expression associated with a closure */
LVAL xgetlambda(V)
{
    LVAL closure, fun;
    closure = xlgetarg();   /* fixed to allow operation when not a closure */
    if (closurep(closure)) 
      fun = cons(gettype(closure), cons(getlambda(closure), getbody(closure)));
    else if (bcclosurep(closure)) {
      LVAL def = getbcdef(getbcccode(closure));
      fun = consp(def) ? car(def) : NIL;
    }
    else
      fun = NIL;
#ifdef MULVALS
    xlnumresults = 3;
    xlresults[0] = fun;
    if (closurep(closure)) {
      xlresults[1] = (! null(getenvi(closure))) ? s_true : NIL;
      xlresults[2] = getname(closure);
    }
    else if (bcclosurep(closure)) {
      LVAL def = getbcdef(getbcccode(closure));
      xlresults[1] = consp(def) ? cdr(def) : s_true;
      xlresults[2] = getbcname(getbcccode(closure));
    }
    else {
      xlresults[1] = s_true;
      xlresults[2] = NIL;
    }
#endif /* MULVALS */
    return(fun);
}

/* xsetq - special form 'setq' */
LVAL xsetq(V)
{
    LVAL sym,val;

    /* handle each pair of arguments */
    for (val = NIL; moreargs(); ) {
	sym = xlgasymbol();
	val = xleval(xlgetarg());
	xlsetvalue(sym,val);
    }

    /* return the result value */
    return (val);
}

/* xpsetq - special form 'psetq' */
LVAL xpsetq(V)
{
    LVAL plist,sym,val;

    /* protect some pointers */
    xlsave1(plist);

    /* handle each pair of arguments */
    while (moreargs()) {
	sym = xlgasymbol();
	val = xleval(xlgetarg());
	plist = cons(cons(sym,val),plist);
    }

    /* do parallel sets */
    for (; consp(plist); plist = cdr(plist))
      xlsetvalue(car(car(plist)),cdr(car(plist)));
    
    /* restore the stack */
    xlpop();

    /* return NIL */
    return (NIL);
}

/* xsetf - special form 'setf' */
/* TAA note -- this code cheats by returning value directly, rather
   than returning the return value of a evaled defsetf lambda. But
   since that lambda expr is supposed to return value anyway, it
   should all work!!?? 7/92 */
/* TAA addendum -- defsetf problem actually fixed now! 4/25/95 */
LVAL xsetf(V)
{
    LVAL place,value;

    /* protect some pointers */
    xlsave1(value);

    /* handle each pair of arguments */
    while (moreargs()) {

	/* get place and value */
	place = xlgetarg();
	value = xleval(xlgetarg());

	/* expand macros in the place form */
	if (consp(place))
	    place = xlexpandmacros(place);
	
	/* check the place form */
	if (symbolp(place))
	    xlsetvalue(place,value);
	else if (consp(place))
	    placeform(place,&value); /* Was place,value  4/25/95 */
	else
	    xlfail("bad place form");
    }

    /* restore the stack */
    xlpop();

    /* return the value */
    return (value);
}

/* xpsetf - special form "psetf" */
LVAL xpsetf(V)
{
    LVAL plist, place, val;
    
    /* protect some pointers */
    xlstkcheck(2);
    xlsave(plist);
    xlsave(place);
    
    /* handle each pair of arguments */
    while (moreargs()) {
        place = xlgetarg();
        /* expand macros in place form */
        if (consp(place)) place = xlexpandmacros(place);
        if (!symbolp(place) && !consp(place))
            xlfail("bad place form");
        val = xleval(xlgetarg());
        plist = cons(cons(place,val),plist);
    }

    /* do parallel sets */
    for (; !null(plist); plist = cdr(plist)) {
        place = car(car(plist));
        val = cdr(car(plist));
        /* check the placeform */
        if (symbolp(place))
            xlsetvalue(place,val);
        else
            placeform(place,&val); /* was place,val  4/25/95 */
    }

    /* restore the stack */
    xlpopn(2);

    /* return NIL */
    return (NIL);
}


/* placeform - handle a place form other than a symbol */
LOCAL VOID placeform P2C(LVAL, place, LVAL *, val)
{
    LVAL fun,arg1,arg2;
    LVAL value = *val;  /* dereference TAA fixe 4/25/95 */
    FIXTYPE i;  /* TAA fix */

    /* check the function name */
    if ((fun = match(SYMBOL,&place)) == s_get) {
	xlstkcheck(2);
	xlsave(arg1);
	xlsave(arg2);
	arg1 = evmatch(SYMBOL,&place);
	arg2 = evarg(&place);
        if (!null(place)) { /* TAA MOD 7/93--allow and ignore 3rd argument */
            place = cdr(place);
            if (!null(place))
                toomany(place);
        }
	xlputprop(arg1,value,arg2);
	xlpopn(2);
    }
    else if (fun == s_svalue) {
	arg1 = evmatch(SYMBOL,&place);
	if (!null(place)) toomany(place);
	if (constantp(arg1)) xlnoassign(arg1);
	setvalue(arg1,value);
    }
    else if (fun == s_sfunction) {
	arg1 = evmatch(SYMBOL,&place);
	if (!null(place)) toomany(place);
	setfunction(arg1,value);
    }
    else if (fun == s_splist) {
	arg1 = evmatch(SYMBOL,&place);
	if (!null(place)) toomany(place);
	setplist(arg1,value);
    }
    else if (fun == s_car) {
	arg1 = evmatch(CONS,&place);
	if (!null(place)) toomany(place);
	rplaca(arg1,value);
    }
    else if (fun == s_cdr) {
	arg1 = evmatch(CONS,&place);
	if (!null(place)) toomany(place);
	rplacd(arg1,value);
    }
    else if (fun == s_nth) {
	xlsave1(arg1);
	arg1 = evmatch(FIXNUM,&place);
	arg2 = evmatch(LIST,&place);
	if (!null(place)) toomany(place);
	for (i = /*(int) */getfixnum(arg1); i > 0 && consp(arg2); --i)
	    arg2 = cdr(arg2);
	if (consp(arg2))
	    rplaca(arg2,value);
	xlpop();
    }
    else if (fun == s_aref || fun == s_row_major_aref) {
      xlsave1(arg1);
      arg1 = evarg(&place);
      switch (ntype(arg1)) {
      case VECTOR:
      case STRING:
      case TVEC:
	arg2 = evmatch(FIXNUM,&place); i = getfixnum(arg2);
	if (!null(place)) toomany(place);
	if (i < 0 || i >= gettvecsize(arg1))
	  xlerror("index out of range",arg2);
	settvecelement(arg1,(int)i,value);	/*taa fix -- added cast */
	break;
      case DARRAY:
	if (fun == s_row_major_aref) {
	  LVAL x = getdarraydata(arg1);
	  arg2 = evmatch(FIXNUM,&place); i = getfixnum(arg2);
	  if (!null(place)) toomany(place);
	  if (i < 0 || i >= gettvecsize(x))
	    xlerror("index out of range",arg2);
	  settvecelement(x,(int)i,value);
	}
	else {
	  LVAL next;

	  /* protect args pointer */
	  xlsave1(arg2);

	  arg2 = mklist(llength(place), NIL);
	  for (next = arg2; consp(next); next = cdr(next))
	    rplaca(next, evmatch(FIXNUM,&place));

	  settvecelement(getdarraydata(arg1),
			 rowmajorindex(arg1, arg2, FALSE),
			 value);

	  xlpop();
	}
	break;
      default: xlbadtype(arg1);
      }
      xlpop();
    }
    else if (fun == s_elt) {
      xlsave1(arg1);
      arg1 = evarg(&place);
      arg2 = evmatch(FIXNUM,&place); i = getfixnum(arg2);
      if (!null(place)) toomany(place);
      if (listp(arg1)) {
	for (; i > 0 && consp(arg1); --i)
	  arg1 = cdr(arg1);
	if((!consp(arg1)) || i < 0)
	  xlerror("index out of range",arg2);
	rplaca(arg1,value);
      }
      else {
	switch (ntype(arg1)) {
	case VECTOR:
	case STRING:
	case TVEC:
	  if (i < 0 || i >= gettvecsize(arg1))
	    xlerror("index out of range",arg2);
	  settvecelement(arg1,(int)i,value);
	  break;
	default: xlbadtype(arg1);
	}
      }
      xlpop();
    }
#ifdef HASHFCNS
    else if (fun == s_gethash) {
        xlstkcheck(2);
        xlsave(arg1);
        xlsave(arg2);
        arg1 = evarg(&place);
        arg2 = evarg(&place);
        if (consp(place)) place = cdr(place);
        if (!null(place)) toomany(place);
        xlsetgethash(arg1,arg2,value);
        xlpopn(2);
        }
#endif
    else if (fun == s_getf) { /* TAA MOD 7/93 -- added form */
        LVAL tmp=NULL,pair;
        xlstkcheck(2);
        xlsave(arg1);
        xlsave(arg2);
        if (!null(place)) tmp = car(place); /* save form for later storing */
        if (!symbolp(tmp) && !consp(tmp)) xlerror("bad place form",tmp);
        arg1 = evarg(&place);   /* get property list */
        arg2 = evarg(&place);   /* get property to search for */
        if (!null(place)) {     /* toss additional argument, if any */
            place = cdr(place);
            if (!null(place))
                toomany(place);
        }
        if (!null(pair=findprop(arg1,arg2))) 
            rplaca(pair, value);    /* replace old value */
        else {
            arg1 = cons(arg2, cons(value, arg1));   /* cons new property */
            if (symbolp(tmp))
                xlsetvalue(tmp, arg1);  /* simple assignment */
            else
                placeform(tmp, &arg1);  /* recurse to store value */
        }
        xlpopn(2);
    }
    /* in the following two cases, now return value  4/25/95 */
    else if (!null(arg1 = xlgetprop(fun,s_setf))) /* TAA 7/92 changed fun to 
                            arg1 to preserve initial value for next test */
	*val = setffunction(arg1,place,value);
    else if (!null(fun = xlgetprop(fun,s_setfl)))  /* TAA fix 7/92 */
        *val = setffunctionl(fun,place,value);  /* added code for proper defsetf support */
    else
	xlfail("bad place form");
}

/* setffunction - call a user defined setf function */
LOCAL LVAL setffunction P3C(LVAL, fun, LVAL, place, LVAL, value)
{
    FRAMEP newfp;
    int argc;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(NIL);

    /* push the values of all of the place expressions and the new value */
    for (argc = 1; consp(place); place = cdr(place), ++argc)
	pusharg(xleval(car(place)));
    pusharg(value);

    /* insert the argument count and establish the call frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* apply the function */
    return xlapply(argc);
}
		       
/* setffunctionl -- call a user defined setf function (lambda expression) */
LOCAL LVAL setffunctionl P3C(LVAL, fun, LVAL, place, LVAL, value)
{
    FRAMEP newfp;
    int argc;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(NIL);

    /* push the values of all of the place expressions and the new value */
    for (argc = 1; consp(place); place = cdr(place), ++argc)
        pusharg(car(place));
    pusharg(cons(s_quote, consa(value)));

    /* insert the argument count and establish the call frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* apply the function, then evaluate it */
    return xleval(xlapply(argc));
}

/* xdefun - special form 'defun' */
LVAL xdefun(V)
{
    LVAL sym,fargs,arglist;

    /* get the function symbol and formal argument list */
    xlsave1(arglist);
    sym = xlgasymbol();
    fargs = xlgalist();
    arglist = makearglist(xlargc,xlargv);

    /* install documentation string - L. Tierney */
    if (consp(arglist) && stringp(car(arglist)) && consp(cdr(arglist))) {
        if (getvalue(s_keepdocs) != NIL)
	  xlputprop(sym, car(arglist), s_fundoc);
	arglist = cdr(arglist);
    }

    /* make the symbol point to a new function definition */
    /* TAA Bug fix 1/94, was xlsetfunction */
    setfunction(sym,xlclose(sym,s_lambda,fargs,arglist,xlenv,xlfenv));

    /* restore the stack and return the function symbol */
    xlpop();
    return (sym);
}

/* xdefmacro - special form 'defmacro' */
LVAL xdefmacro(V)
{
    LVAL sym,fargs,arglist;

    /* get the function symbol and formal argument list */
    xlsave1(arglist);
    sym = xlgasymbol();
    fargs = xlgalist();
    arglist = makearglist(xlargc,xlargv);

    /* install documentation string - L. Tierney */
    if (consp(arglist) && stringp(car(arglist)) && consp(cdr(arglist))) {
        if (getvalue(s_keepdocs) != NIL)
	  xlputprop(sym, car(arglist), s_fundoc);
	arglist = cdr(arglist);
    }

    /* make the symbol point to a new function definition */
    /* TAA Bug fix 1/94, was xlsetfunction */
    setfunction(sym,xlclose(sym,s_macro,fargs,arglist,NIL,NIL));

    /* restore the stack and return the function symbol */
    xlpop();
    return (sym);
}

/* xcond - special form 'cond' */
LVAL xcond(V)
{
    LVAL list,val;

    /* find a predicate that is true */
    for (; moreargs(); ) {
	/* get the next conditional */
	list = nextarg();

	/* evaluate the predicate part */
        if (atom(list)) /* TAA MOD 5/94, added */
            xlerror("bad cond clause", list);

        if (!null(val = xleval(car(list)))) {
#ifdef MULVALS      /* TAA MOD 2/94, added */
            xlnumresults = 1;
            xlresults[0] = val;
#endif /* MULVALS */
	    /* evaluate each expression */
	    for (list = cdr(list); consp(list); list = cdr(list))
		val = xleval(car(list));

	    /* exit the loop */
	    return val;
	}
    }
#ifdef MULVALS
    xlnumresults = 1;   /* TAA MOD 5/97 --rearranged to return correct value */
    xlresults[0] = NIL;
#endif /* MULVALS */
    return NIL;
}

/* xwhen - special form 'when' */
LVAL xwhen(V)
{
    LVAL val;
    int flag;   /* TAA Mod 4/97, changed logic */

    /* check the test expression */
    flag = !null(xleval(xlgetarg()));

    /* Set default NIL result */
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = NIL;
#endif /* MULVALS */
    val = NIL;
    
    if (flag)
	while (moreargs())
	    val = xleval(nextarg());

    /* return the value */
    return (val);
}

/* xunless - special form 'unless' */
LVAL xunless(V)
{
    LVAL val=NIL;
    int flag;   /* TAA Mod 4/97, changed logic */

    /* check the test expression */
    flag = null(xleval(xlgetarg()));

    /* Set default NIL result */
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = NIL;
#endif /* MULVALS */
    val = NIL;
    
    if (flag)
	while (moreargs())
	    val = xleval(nextarg());

    /* return the value */
    return (val);
}

/* xcase - special form 'case' */
LVAL xcase(V)
{
    LVAL key,list,cases,val;

    /* protect some pointers */
    xlsave1(key);

    /* get the key expression */
    key = xleval(xlgetarg());   /* TAA Bug fix 1/94, was nextarg() */

    /* find a case that matches */
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = NIL;
#endif /* MULVALS */
    for (val = NIL; moreargs(); ) {
#ifdef MULVALS
        xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	/* get the next case clause */
	list = nextarg();

	/* make sure this is a valid clause */
	if (consp(list)) {

	    /* compare the key list against the key */
	    if (((cases = car(list)) == s_true && ! moreargs())||
		(cases == s_otherwise && ! moreargs()) ||
                (listp(cases) && keypresent(key,cases)) ||
                eql(key,cases)) {

		/* evaluate each expression */
		for (list = cdr(list); consp(list); list = cdr(list))
		    val = xleval(car(list));

		/* exit the loop */
		break;
	    }
	}
	else
	    xlerror("bad case clause",list);
    }

    /* restore the stack */
    xlpop();

    /* return the value */
    return (val);
}

/* keypresent - check for the presence of a key in a list */
LOCAL int keypresent P2C(LVAL, key, LVAL, list)
{
    for (; consp(list); list = cdr(list))
	if (eql(car(list),key))
	    return (TRUE);
    return (FALSE);
}

/* xand - special form 'and' */
/* Rewritten 4/97 by TAA */
LVAL xand(V)
{
  LVAL val;

  /* No arguments? Return T */
  if (!moreargs()) {
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = s_true;
#endif /* MULVALS */
    return s_true;
  }

  /* evaluate each argument */
  for (;;) {
    val = nextarg();
    if (!moreargs())    /* return evaluated last argument */
      return xleval(val);
    /* Otherwise return NIL if evaled expression is NIL */
    if (null(xleval(val))) {
#ifdef MULVALS
      xlnumresults = 1;
      xlresults[0] = NIL;
#endif /* MULVALS */
      return NIL;
    }
  }
  /* loop always exits via explicit return */
}

/* xor - special form 'or' */
/* Rewritten 4/97 by TAA */
LVAL xor(V)
{
  LVAL val;

  /* No arguments? Return NIL */
  if (!moreargs()) {
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = NIL;
#endif /* MULVALS */
    return NIL;
  }

  /* evaluate each argument */
  for (;;) {
    val = nextarg();
    if (!moreargs())    /* return evaluated last argument */
      return xleval(val);
    /* Otherwise return single value if evaled expression is not NIL */
    if (!null(val = xleval(val))) {
#ifdef MULVALS
      xlnumresults = 1;
      xlresults[0] = val;
#endif /* MULVALS */
      return val;
    }
  }
  /* loop always exits via explicit return */
}

/* xif - special form 'if' */
LVAL xif(V)
{
    LVAL testexpr,thenexpr,elseexpr;

    /* get the test expression, then clause and else clause */
    testexpr = xlgetarg();
    thenexpr = xlgetarg();
    elseexpr = (moreargs() ? xlgetarg() : NIL);
    xllastarg();

    /* evaluate the appropriate clause */
    return (xleval(null(xleval(testexpr)) ? elseexpr : thenexpr));
}

/* let - common let routine */
LOCAL LVAL let P1C(int, pflag)
{
    LVAL newenv,val;
    LVAL olddenv=xldenv;

    /* protect some pointers */
    xlsave1(newenv);

    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* get the list of bindings and bind the symbols */
    if (pflag) {    /* bind "simultaneously" */
 	LVAL newdenv = xldenv;
	xlprot1(newdenv);   /* Added 1/94 */
	dobindings(xlgalist(), newenv, &newdenv, FALSE);
	xlenv = newenv;
	xldenv = newdenv;
	xlpop();
    }
    else {          /* bind "sequentially") */
	xlenv = newenv;
	dobindings(xlgalist(), newenv, &xldenv, TRUE);
    }

    /* execute the code */
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = NIL;
#endif /* MULVALS */
    for (val = NIL; moreargs(); )
	val = xleval(nextarg());

    /* unbind the arguments */
    xlenv = cdr(xlenv);

    /* restore the stack */
    xlunbind(olddenv);
    xlpop();

    /* return the result */
    return (val);
}

/* xlet - special form 'let' */
LVAL xlet(V)
{
    return (let(TRUE));
}

/* xletstar - special form 'let*' */
LVAL xletstar(V)
{
    return (let(FALSE));
}

/* flet - common flet/labels/macrolet routine */
LOCAL LVAL flet P2C(LVAL, type, int, letflag)
{
    LVAL list,bnd,sym,fargs,val;

    /* create a new environment frame */
    xlfenv = xlframe(xlfenv);

    /* bind each symbol in the list of bindings */
    for (list = xlgalist(); consp(list); list = cdr(list)) {

	/* get the next binding */
	bnd = car(list);

	/* get the symbol and the function definition */
	sym = match(SYMBOL,&bnd);
	fargs = match(LIST,&bnd);
	val = xlclose(sym,type,fargs,bnd,xlenv,(letflag?cdr(xlfenv):xlfenv));

	/* bind the value to the symbol */
	xlfbind(sym,val);
    }

    /* execute the code */
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
    for (val = NIL; moreargs(); )
	val = xleval(nextarg());

    /* unbind the arguments */
    xlfenv = cdr(xlfenv);

    /* return the result */
    return (val);
}

/* xflet - built-in function 'flet' */
LVAL xflet(V)
{
    return (flet(s_lambda,TRUE));
}

/* xlabels - built-in function 'labels' */
LVAL xlabels(V)
{
    return (flet(s_lambda,FALSE));
}

/* xmacrolet - built-in function 'macrolet' */
LVAL xmacrolet(V)
{
    return (flet(s_macro,TRUE));
}

/* prog - common prog routine */
LOCAL LVAL prog P1C(int, pflag)
{
    LVAL newenv,val;
    CONTEXT cntxt;
    LVAL olddenv=xldenv;

    /* protect some pointers */
    xlsave1(newenv);

#ifdef LEXBIND
    /* create a new environment frame for the block tag */
    xlenv = xlframe(xlenv);
#endif

    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* establish a new execution context */
    xlbegin(&cntxt,CF_RETURN,NIL);
    if (XL_SETJMP(cntxt.c_jmpbuf))
	val = xlvalue;
    else {
#ifdef LEXBIND
        /* bind the block tag */
	xlbindtag(&cntxt,NIL,xlenv);
#endif
	/* get the list of bindings and bind the symbols */
        if (pflag) {    /* bind "simultaneously" */
	    LVAL newdenv = xldenv;
	    xlprot1(newdenv);   /* Added 1/94 */
	    dobindings(xlgalist(), newenv, &newdenv, FALSE);
	    xlenv = newenv;
	    xldenv = newdenv;
	    xlpop();
        }
        else {          /* bind "sequentially") */
	    xlenv = newenv;
	    dobindings(xlgalist(), newenv, &xldenv, TRUE);
        }

	/* execute the code */
	tagbody();
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	val = NIL;

	/* unbind the arguments */
	xlenv = cdr(xlenv);
    }
    xlend(&cntxt);

#ifdef LEXBIND
    /* unbind the block tag */
    xlenv = cdr(xlenv);
#endif
    /* restore the stack */
    xlunbind(olddenv);
    xlpop();

    /* return the result */
    return (val);
}

/* xprog - special form 'prog' */
LVAL xprog(V)
{
    return (prog(TRUE));
}

/* xprogstar - special form 'prog*' */
LVAL xprogstar(V)
{
    return (prog(FALSE));
}

/* xgo - special form 'go' */
LVAL xgo(V)
{
    LVAL label;

    /* get the target label */
    label = xlgetarg();
    xllastarg();

    /* transfer to the label */
    xlgo(label);
    return (NIL);
}

/* xreturn - special form 'return' */
LVAL xreturn(V)
{
    LVAL val;

    /* get the return value */
#ifdef MULVALS
    if (moreargs())
      val = xleval(nextarg());
    else {
      val = xlresults[0] = NIL;
      xlnumresults = 1;
    }
#else
    val = (moreargs() ? xleval(nextarg()) : NIL);
#endif /* MULVALS */
    xllastarg();

    /* return from the inner most block */
    xlreturn(NIL,val);
    return (NIL);
}

/* xrtnfrom - special form 'return-from' */
LVAL xrtnfrom(V)
{
    LVAL name,val;

    /* get the return value */
    name = xlgasymbol();
#ifdef MULVALS
    if (moreargs())
      val = xleval(nextarg());
    else {
      val = xlresults[0] = NIL;
      xlnumresults = 1;
    }
#else
    val = (moreargs() ? xleval(nextarg()) : NIL);
#endif /* MULVALS */
    xllastarg();

    /* return from the inner most block */
    xlreturn(name,val);
    return (NIL);
}

/* progx - common progx code */
LOCAL LVAL progx P1C(int, n)
{
    LVAL val;

    /* protect some pointers */
    xlsave1(val);

    /* evaluate the first n expressions */
    while (moreargs() && --n >= 0)
	val = xleval(nextarg());

    /* evaluate each remaining argument */
    while (moreargs())
	xleval(nextarg());

    /* restore the stack */
    xlpop();

    /* return the last test expression value */
    return (val);
}

/* xprog1 - special form 'prog1' */
LVAL xprog1(V)
{
    return (progx(1));
}

/* xprog2 - special form 'prog2' */
LVAL xprog2(V)
{
    return (progx(2));
}

/* xprogn - special form 'progn' */
LVAL xprogn(V)
{
    LVAL val;

    /* evaluate each expression */
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = NIL;
#endif /* MULVALS */
    for (val = NIL; moreargs(); )
	val = xleval(nextarg());

    /* return the last test expression value */
    return (val);
}

/* xprogv - special form 'progv' */
LVAL xprogv(V)
{
    LVAL olddenv,vars,vals,val;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(vars);
    xlsave(vals);

    /* get the list of variables and the list of values */
    vars = xlgetarg(); vars = xleval(vars);
    if (!listp(vars))
      xlbadtype(vars);    /* Bug fix from Luke Tierney, 11/93 */
    vals = xlgetarg(); vals = xleval(vals);
    if (!listp(vals))
        xlbadtype(vals);  /* finish bug fix 2/94, from Gottfried Ira */

    /* bind the values to the variables */
    for (olddenv = xldenv; consp(vars); vars = cdr(vars)) {
	val = car(vars);    /* TAA mod, reducing car(vars) operation */
	if (!symbolp(val))
	    xlerror("expecting a symbol",val);
	if (constantp(val))
	    xlnoassign(val);
	if (consp(vals)) {
	    xldbind(val,car(vals));
	    vals = cdr(vals);
	}
	else
	    xldbind(val,s_unbound);
    }

    /* evaluate each expression */
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = NIL;
#endif /* MULVALS */
    for (val = NIL; moreargs(); )
	val = xleval(nextarg());

    /* restore the previous environment and the stack */
    xlunbind(olddenv);
    xlpopn(2);

    /* return the last test expression value */
    return (val);
}

/* xloop - special form 'loop' */
LVAL xloop(V)
{
    FRAMEP argv;
    LVAL arg,val;
    CONTEXT cntxt;
    int argc;

    /* protect some pointers */
    xlsave1(arg);

#ifdef LEXBIND
    /* add a new environment frame for the block tag */
    xlenv = xlframe(xlenv);
#endif
    /* establish a new execution context */
    xlbegin(&cntxt,CF_RETURN,NIL);
    if (XL_SETJMP(cntxt.c_jmpbuf))
      val = xlvalue;
    else {
#ifdef LEXBIND
      xlbindtag(&cntxt,NIL,xlenv);
#endif
      for (argv = xlargv, argc = xlargc; ; xlargv = argv, xlargc = argc) {
	while (moreargs()) {
	  arg = nextarg();
	  if (consp(arg))
	    xleval(arg);
	}
	/* check for control codes */ /* TAA addition 6/17/93 */
	if (--xlsample <= 0) {
	  xlsample = SAMPLE;
	  oscheck();
	}
      }
    }
    xlend(&cntxt);

#ifdef LEXBIND
    /* unbind the block tag */
    xlenv = cdr(xlenv);
#endif
    /* restore the stack */
    xlpop();

    /* return the result */
    return (val);
}

/* doloop - common do routine */
LOCAL LVAL doloop P1C(int, pflag)
{
    FRAMEP argv;
    LVAL newenv,blist,clist,test,val;
    LVAL olddenv=xldenv;
    CONTEXT cntxt;
    int argc, sts;
    struct { LVAL blist, clist, test; } state;

    /* protect some pointers */
    xlsave1(newenv);

    /* get the list of bindings, the exit test and the result forms */
    blist = xlgalist();
    clist = xlgalist();
    test = (consp(clist) ? car(clist) : NIL);
    argv = xlargv;
    argc = xlargc;

#ifdef LEXBIND
    /* create a new environment frame for the block tag */
    xlenv = xlframe(xlenv);
#endif
    /* create a new environment frame */
    newenv = xlframe(xlenv);

    /* establish a new execution context */
    xlbegin(&cntxt,CF_RETURN,NIL);
    state.blist = blist; state.clist = clist; state.test = test;
    sts = XL_SETJMP(cntxt.c_jmpbuf);
    blist = state.blist; clist = state. clist; test = state.test;
    if (sts)
	val = xlvalue;
    else {
#ifdef LEXBIND
	xlbindtag(&cntxt,NIL,xlenv);
#endif
	/* bind the symbols */
        if (pflag) {    /* bind "simultaneously" */
	    LVAL newdenv = xldenv;
	    xlprot1(newdenv);   /* Added 1/94 */
	    dobindings(blist, newenv, &newdenv, FALSE);
	    xlenv = newenv;
	    xldenv = newdenv;
	    xlpop();
        }
        else {          /* bind "sequentially") */
	    xlenv = newenv;
	    dobindings(blist, newenv, &xldenv, TRUE);
        }

	/* execute the loop as long as the test is false */
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	for (val = NIL; null(xleval(test)); doupdates(blist,pflag)) {
	    xlargv = argv;
	    xlargc = argc;
	    tagbody();
	}

	/* evaluate the result expression */
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	if (consp(clist))
	    for (clist = cdr(clist); consp(clist); clist = cdr(clist))
		val = xleval(car(clist));

	/* unbind the arguments */
	xlenv = cdr(xlenv);
    }
    xlend(&cntxt);

    /* unbind the block tag */
    xlenv = cdr(xlenv);

    /* restore the stack */
    xlunbind(olddenv);
    xlpop();

    /* return the result */
    return (val);
}

/* xdo - special form 'do' */
LVAL xdo(V)
{
    return (doloop(TRUE));
}

/* xdostar - special form 'do*' */
LVAL xdostar(V)
{
    return (doloop(FALSE));
}

/* xdolist - special form 'dolist' */
LVAL xdolist(V)
{
    FRAMEP argv;
    LVAL list,clist,sym,val;
    LVAL olddenv=xldenv;
    CONTEXT cntxt;
    int argc;

    /* protect some pointers */
    xlsave1(list);

    /* get the control list (sym list result-expr) */
    clist = xlgalist();
    sym = match(SYMBOL,&clist);
    argv = xlargv;
    argc = xlargc;

    /* initialize the local environment */
    xlenv = xlframe(xlenv);

    /* establish a new execution context */
    xlbegin(&cntxt,CF_RETURN,NIL);
    if (XL_SETJMP(cntxt.c_jmpbuf))
	val = xlvalue;
    else {
#ifdef LEXBIND
	xlbindtag(&cntxt,NIL,xlenv);
#endif
        /* TAA MOD -  Moved the following two statements from before
           the xlbegin()  2/94 */
        /* Get the argument, in the block context */
        list = evmatch(LIST,&clist);
        
        /* finish local environment initialization */
        xlbind(sym,NIL);

	/* loop through the list */
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	for (val = NIL; consp(list); list = cdr(list)) {

	    /* bind the symbol to the next list element */
	    xlsetvalue(sym,car(list));

	    /* execute the loop body */
	    xlargv = argv;
	    xlargc = argc;
	    tagbody();
	}

	/* evaluate the result expression */
	xlsetvalue(sym,NIL);
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	val = (consp(clist) ? xleval(car(clist)) : NIL);

    }
    /* unbind the arguments */  /* TAA mod -- moved out of above "else" */
    xlenv = cdr(xlenv);

    xlend(&cntxt);

    /* restore the stack */
    xlunbind(olddenv);
    xlpop();

    /* return the result */
    return (val);
}

/* xdotimes - special form 'dotimes' */
LVAL xdotimes(V)
{
    FRAMEP argv;
    LVAL clist,sym,cnt,val;
    LVAL olddenv=xldenv;
    CONTEXT cntxt;
    int argc;
    FIXTYPE n,i; /* TAA MOD (fix) */

    /* get the control list (sym list result-expr) */
    clist = xlgalist();
    sym = match(SYMBOL,&clist);
    argv = xlargv;
    argc = xlargc;

    /* initialize the local environment */
    xlenv = xlframe(xlenv);

    /* establish a new execution context */
    xlbegin(&cntxt,CF_RETURN,NIL);
    if (XL_SETJMP(cntxt.c_jmpbuf))
	val = xlvalue;
    else {
#ifdef LEXBIND
	xlbindtag(&cntxt,NIL,xlenv);
#endif
        /* TAA MOD -  Moved the following three statements from before
           the xlbegin()  2/94 */
        /* Get the argument, in the block context */
        cnt = evmatch(FIXNUM,&clist);
        n = getfixnum(cnt);

        /* finish local environment initialization */
        xlbind(sym, NIL);
        
	/* loop through for each value from zero to n-1 */
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	for (val = NIL, i = 0; i < n; ++i) {

	    /* bind the symbol to the next list element */
	    xlsetvalue(sym,cvfixnum((FIXTYPE)i));

	    /* execute the loop body */
	    xlargv = argv;
	    xlargc = argc;
	    tagbody();
	}

	/* evaluate the result expression */
        xlsetvalue(sym,cvfixnum((FIXTYPE)n)); /* TAA FIX 2/94 */
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	val = (consp(clist) ? xleval(car(clist)) : NIL);

    }

    /* unbind the arguments */  /* TAA mod -- moved out of above "else" */
    xlenv = cdr(xlenv);

    xlend(&cntxt);

    /* unbind dynamic arguments */
    xlunbind(olddenv);

    /* return the result */
    return (val);
}

/* xblock - special form 'block' */
LVAL xblock(V)
{
    LVAL name,val;
    CONTEXT cntxt;
    int sts;
    struct { LVAL name; } state;

    /* get the block name */
    name = xlgetarg();
    if (!null(name) && !symbolp(name))
	xlbadtype(name);

#ifdef LEXBIND
    /* add a frame for the block tag */
    xlenv = xlframe(xlenv);
#endif

    /* execute the block */
    xlbegin(&cntxt,CF_RETURN,name);
    state.name = name;
    sts = XL_SETJMP(cntxt.c_jmpbuf);
    name = state.name;
    if (sts)
	val = xlvalue;
    else {
#ifdef LEXBIND
	xlbindtag(&cntxt,name,xlenv);
#endif
#ifdef MULVALS
	xlnumresults = 1;
	xlresults[0] = NIL;
#endif /* MULVALS */
	for (val = NIL; moreargs(); )
	    val = xleval(nextarg());
    }
    xlend(&cntxt);

#ifdef LEXBIND
    /* unbind the block tag */
    xlenv = cdr(xlenv);
#endif
    /* return the value of the last expression */
    return (val);
}

/* xtagbody - special form 'tagbody' */
LVAL xtagbody(V)
{
    tagbody();
    return (NIL);
}

/* xcatch - special form 'catch' */
LVAL xcatch(V)
{
    CONTEXT cntxt;
    LVAL tag,val;

    /* protect some pointers */
    xlsave1(tag);

    /* get the tag */
    tag = xleval(xlgetarg());   /* TAA Bug fix 1/94, was nextarg() */

    /* establish an execution context */
    xlbegin(&cntxt,CF_THROW,tag);

    /* check for 'throw' */
    if (XL_SETJMP(cntxt.c_jmpbuf))
	val = xlvalue;

    /* otherwise, evaluate the remainder of the arguments */
    else {
#ifdef MULVALS
    xlnumresults = 1;
    xlresults[0] = NIL;
#endif /* MULVALS */
	for (val = NIL; moreargs(); )
	    val = xleval(nextarg());
    }
    xlend(&cntxt);

    /* restore the stack */
    xlpop();

    /* return the result */
    return (val);
}

/* xthrow - special form 'throw' */
LVAL xthrow(V)
{
    LVAL tag,val;

    /* get the tag and value */
    tag = xleval(xlgetarg());   /* TAA Bug fix 1/94, was nextarg() */
    val = xleval(xlgetarg());
    xllastarg();

    /* throw the tag */
    xlthrow(tag,val);
    return (NIL);
}

/* xunwindprotect - special form 'unwind-protect' */
LVAL xunwindprotect(V)
{
    CONTEXT cntxt,*target=NULL;
    int mask=0,sts;
    LVAL val;
#ifdef MULVALS
    LVAL *oldsp;
    int i, n;
#endif /* MULVALS */
    struct { int mask; CONTEXT *target; } state;

    /* protect some pointers */
    xlsave1(val);

    /* get the expression to protect */
    val = xlgetarg();

    /* evaluate the protected expression */
    xlbegin(&cntxt,CF_UNWIND,NIL);
    state.mask = mask; state.target = target;
    sts = XL_SETJMP(cntxt.c_jmpbuf);
    mask = state.mask; target = state.target;
    if (sts != 0) {
	target = xltarget;
	mask = xlmask;
	val = xlvalue;
    }
    else
	val = xleval(val);
    xlend(&cntxt);
	
#ifdef MULVALS
    /* save results on the stack */
    oldsp = xlsp;
    n = xlnumresults;
    for (i = 0; i < n; i++)
      pusharg(xlresults[i]);
#endif /* MULVALS */

    /* evaluate the cleanup expressions */
    while (moreargs())
	xleval(nextarg());

#ifdef MULVALS
    /* restore the results */
    for (i = 0; i < n; i++)
      xlresults[i] = oldsp[i];
    xlnumresults = n;
    xlsp = oldsp;
#endif /* MULVALS */

    /* if unwinding, continue unwinding */
    if (sts)
	xljump(target,mask,val);

    /* restore the stack */
    xlpop();

    /* return the value of the protected expression */
    return (val);
}

/* xerrset - special form 'errset' */
LVAL xerrset(V)
{
    LVAL expr=NIL,flag,val;
    CONTEXT cntxt;
    int sts;
    struct { LVAL expr; } state;

    /* get the expression and the print flag */
    expr = xlgetarg();
    flag = (moreargs() ? xlgetarg() : s_true);
    xllastarg();

    /* establish an execution context */
    xlbegin(&cntxt,CF_ERROR,flag);

    /* check for error */
    state.expr = expr;
    sts = XL_SETJMP(cntxt.c_jmpbuf);
    expr = state.expr;
    if (sts)
	val = NIL;

    /* otherwise, evaluate the expression */
    else {
	expr = xleval(expr);
	val = consa(expr);
    }
    xlend(&cntxt);

    /* return the result */
    return (val);
}

/* xtrace - special form 'trace' */
LVAL xtrace(V)
{
    /* TAA MOD -- changed to use s_tracelist rather than looking it up */
    LVAL fun,this;

    /* loop through all of the arguments */
    while (moreargs()) {
	fun = xlgasymbol();

	/* check for the function name already being in the list */
	for (this = getvalue(s_tracelist); consp(this); this = cdr(this))
	    if (car(this) == fun)
		break;

	/* add the function name to the list */
	if (null(this))
	    setvalue(s_tracelist,cons(fun,getvalue(s_tracelist)));
    }
    return (getvalue(s_tracelist));
}

/* xuntrace - special form 'untrace' */
LVAL xuntrace(V)
{
    /* TAA MOD -- changed to use s_tracelist rather than looking it up */
    LVAL fun,this,last;

    /* loop through all of the arguments */
    if (!moreargs()) {  /* list empty -- then untrace all functions */
	setvalue(s_tracelist,NIL);
	return (NIL);
    }
    while (moreargs()) {
	fun = xlgasymbol();

	/* remove the function name from the list */
	last = NIL;
	for (this = getvalue(s_tracelist); consp(this); this = cdr(this)) {
	    if (car(this) == fun) {
		if (!null(last))
		    rplacd(last,cdr(this));
		else
		    setvalue(s_tracelist,cdr(this));
		break;
	    }
	    last = this;
	}
    }
    return (getvalue(s_tracelist));
}

/* dobindings - handle bindings for let/let*, prog/prog*, do/do* */
LOCAL VOID dobindings P4C(LVAL, list, LVAL, env, LVAL *, denv, int, seq)
{
    LVAL bnd,sym=NIL,val;
    LVAL plist;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(val);
    xlsave(plist);

    /* bind each symbol in the list of bindings */
    for (; consp(list); list = cdr(list)) {

	/* get the next binding */
	bnd = car(list);

	/* handle a symbol */
	if (symbolp(bnd)) {
	    sym = bnd;
	    val = NIL;
	}

	/* handle a list of the form (symbol expr) or (symbol) */
	else if (consp(bnd)) {
	    sym = match(SYMBOL,&bnd);
	    val = null(bnd) ? NIL : evarg(&bnd);
	}
	else
	    xlfail("bad binding");

	/* bind the value to the symbol */
        if (constantp(sym)) xlnoassign(sym);
        if (specialp(sym)) { /* For parallel binding, defer binding of
                                specials until end, by creating temporary
                                binding list */
	  if (seq) {
	    xlpdbind(sym, val, *denv);
	  }
	  else {
	    plist = cons(cons(sym,val), plist);
	  }
        }
        else {xlpbind(sym,val,env);}
    }

    /* now do the binding of the specials, since all vals have been
       evaluated. */
    while (consp(plist)) {
      bnd = car(plist);
      xlpdbind(car(bnd),cdr(bnd),*denv);
      plist = cdr(plist);
    }

    /* restore the stack */
    xlpopn(2);
}

/* doupdates - handle updates for do/do* */
LOCAL VOID doupdates P2C(LVAL, list, int, pflag)
{
    LVAL plist,bnd,sym,val;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(plist);
    xlsave(val);

    /* bind each symbol in the list of bindings */
    for (; consp(list); list = cdr(list)) {

	/* get the next binding */
	bnd = car(list);

	/* handle a list of the form (symbol expr) or (symbol) */
	if (consp(bnd)) {
	    sym = match(SYMBOL,&bnd);
	    bnd = consp(bnd) ? cdr(bnd) : NIL;
	    if (!null(bnd)) {
		val = evarg(&bnd);
		if (pflag)
		    plist = cons(cons(sym,val),plist);
		else
		    xlsetvalue(sym,val);
	    }
	}
    }

    /* set the values for parallel updates */
    for (; !null(plist); plist = cdr(plist)) {  /* TAA MOD for efficiency */
	bnd = car(plist);
	xlsetvalue(car(bnd),cdr(bnd));
    }

    /* restore the stack */
    xlpopn(2);
}

/* tagbody - execute code within a block and tagbody */
/* TAA MOD 4/94 -- changed mechanism that GO target was
   propagated back to tagbody(). Formerly the context block
   was altered, but this caused problems with GO's within
   UNWIND-PROTECTS (Error reported by Gottfried Ira, 3/94) */
LOCAL VOID tagbody(V)
{
    LVAL arg;
    CONTEXT cntxt;
    int i;
#ifdef LEXBIND
    LVAL tags;

    /* save a pointer */
    xlsave1(tags);

    /* add a new frame for the go tags */
    xlenv = xlframe(xlenv);
#endif

    /* establish an execution context */
    xlbegin(&cntxt,CF_GO,NIL);

#ifdef LEXBIND
    /* bind the tags */
    for (i = xlargc - 1, tags = NIL; i >= 0; i--)
      if (! consp(xlargv[i]))
	tags = cons(xlargv[i], tags);
    tags = consa(tags);
    xlbindtag(&cntxt,tags,xlenv);
#endif
    /* check for a 'go' */
#ifdef CRAYCC
    i = XL_SETJMP(cntxt.c_jmpbuf);
    if (i != 0) {
#else
    if ((i = XL_SETJMP(cntxt.c_jmpbuf))!=0) {
#endif /* CRAYCC */
        xlargc -= i;    /* point to jump target */
        xlargv += i;
    }

    /* Check for key hit at least once per tagbody execution */
    /*TAA added 11/93*/
    if (--xlsample <= 0) {
	    xlsample = SAMPLE;
	    oscheck();
    }

    /* execute the body */
    while (moreargs()) {
	arg = nextarg();
	if (consp(arg))
	    xleval(arg);
    }
    xlend(&cntxt);

#ifdef LEXBIND
    /* unbind the go tags */
    xlenv = cdr(xlenv);

    /* restore the stack */
    xlpop();
#endif
}

/* match - get an argument and match its type */
LOCAL LVAL match P2C(int, type, LVAL *, pargs)
{
    LVAL arg;

    /* make sure the argument exists */
    if (!consp(*pargs))
	toofew(*pargs);

    /* get the argument value */
    arg = car(*pargs);

    /* move the argument pointer ahead */
    *pargs = cdr(*pargs);

    /* check its type */
    if (type == LIST) {
	if (!null(arg) && ntype(arg) != CONS)
	    xlbadtype(arg);
    }
    else {
	if (/*null(arg) ||*/ ntype(arg) != type)
	    xlbadtype(arg);
    }

    /* return the argument */
    return (arg);
}

/* evarg - get the next argument and evaluate it */
LOCAL LVAL evarg P1C(LVAL *, pargs)
{
    LVAL arg;

    /* protect some pointers */
    xlsave1(arg);

    /* make sure the argument exists */
    if (!consp(*pargs))
	toofew(*pargs);

    /* get the argument value */
    arg = car(*pargs);

    /* move the argument pointer ahead */
    *pargs = cdr(*pargs);

    /* evaluate the argument */
    arg = xleval(arg);

    /* restore the stack */
    xlpop();

    /* return the argument */
    return (arg);
}

/* evmatch - get an evaluated argument and match its type */
LOCAL LVAL evmatch P2C(int, type, LVAL *, pargs)
{
    LVAL arg;

    /* protect some pointers */
    xlsave1(arg);

    /* make sure the argument exists */
    if (!consp(*pargs))
	toofew(*pargs);

    /* get the argument value */
    arg = car(*pargs);

    /* move the argument pointer ahead */
    *pargs = cdr(*pargs);

    /* evaluate the argument */
    arg = xleval(arg);

    /* check its type */
    if (type == LIST) {
	if (!null(arg) && ntype(arg) != CONS)
	    xlbadtype(arg);
    }
    else {
	if (/*null(arg) ||*/ ntype(arg) != type)
	    xlbadtype(arg);
    }

    /* restore the stack */
    xlpop();

    /* return the argument */
    return (arg);
}

#ifdef MULVALS
LVAL xmulvalcall(V)
{
  FRAMEP newfp;
  LVAL fun;
  int i, argc;

  fun = xleval(xlgetarg());
  
  /* build a new argument stack frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(fun);
  pusharg(NIL); /* will be argc */

  /* evaluate the forms and push their results */
  for (argc = 0; moreargs();) {
    xleval(nextarg()); /* value is ignored -- results are in xlresults */

    for (i = 0; i < xlnumresults; i++, argc++)
      pusharg(xlresults[i]);
  }

  /* establish the new stack frame */
  newfp[2] = cvfixnum((FIXTYPE) argc);
  xlfp = newfp;

  /* apply the function to the arguments */
  return (xlapply(argc));
}

/* progx - common progx code */
LVAL xmulvalprog1(V)
{
  LVAL *oldsp, val;
  int i, n;

  /* evaluate the first n expressions */
  if (moreargs()) {

    /* evaluate the first form */
    xleval(nextarg());/* value is ignored -- results are in xlresults */

    /* store the results on the stack */
    n = xlnumresults;
    oldsp = xlsp;
    for (i = 0; i < n; i++)
      pusharg(xlresults[i]);

    /* evaluate each remaining forms */
    while (moreargs())
      xleval(nextarg());

    /* restore the values and the stack */
    for (i = 0; i < n; i++)
      xlresults[i] = oldsp[i];
    xlnumresults = n;
    xlsp = oldsp;
    val = (n > 0) ? xlresults[0] : NIL;
  }
  else {
    xlnumresults = 1;
    val = xlresults[0] = NIL;
  }
  return (val);
}

LVAL xnthvalue(V)
{
  LVAL form;
  FIXTYPE n;

  n = getfixnum(xlgafixnum());
  form = xlgetarg();
  xllastarg();

  xleval(form); /* value is ignored -- results are in xlresults */
  return((n >= 0 && n < (FIXTYPE)xlnumresults) ? xlresults[(int)n] : NIL);
}  
#endif /* MULVALS */

/* xthe - special form 'the' */
LVAL xthe(V)
{
  LVAL arg;
  xlgetarg(); /* skip type argument */
  arg = xlgetarg();
  xllastarg();
  return xleval(arg);
}

/* xsymaclet - special form symbol-macrolet */
LVAL xsymaclet(V)
{
  LVAL binds;
  LVAL newenv, val;

  /* protect some pointers */
  xlsave1(newenv);

  /* create a new environment frame */
  newenv = xlframe(xlenv);

  /* get the list of bindings and bind the symbols */
  for (binds = xlgalist(); consp(binds); binds = cdr(binds)) {
    val = car(binds);
    if (! consp(val) || ! symbolp(car(val)) ||
	specialp(car(val)) || ! consp(cdr(val)))
      xlfail("bad symbol macro binding");
    xlpbind(k_symbol_macro, car(cdr(val)), newenv);
    xlpbind(car(val), k_symbol_macro, newenv);
  }
  xlenv = newenv;

  /* execute the code */
#ifdef MULVALS
  xlnumresults = 1;
  xlresults[0] = NIL;
#endif /* MULVALS */
  for (val = NIL; moreargs(); )
    val = xleval(nextarg());

  /* unbind the arguments */
  xlenv = cdr(xlenv);

  /* restore the stack */
  xlpop();

  /* return the result */
  return (val);
}

