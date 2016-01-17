/* xllist.c - xlisp built-in list functions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

struct nsubargs { /* TAA added 7/93 */
    LVAL to;    /* and ALIST */
    LVAL from;
    LVAL fcn;
#ifdef KEYARG
    LVAL kfcn;
#endif
    int tresult;
    int expr;
    int subst;
};

struct substargs {  /* TAA MOD - 7/93 to reduce stack usage */
    LVAL to;
    LVAL from;
    LVAL fcn;
#ifdef KEYARG
    LVAL kfcn;
#endif
    int tresult;
};

struct sublargs {   /* TAA MOD - 7/93 to reduce stack usage */
    LVAL alist;
    LVAL fcn;
#ifdef KEYARG
    LVAL kfcn;
#endif
    int tresult;
};

/* forward declarations */
LOCAL LVAL cxr P1H(char *);
LOCAL LVAL nth P1H(int);
LOCAL LVAL subst P2H(LVAL, struct substargs *);
LOCAL LVAL sublis P2H(LVAL, struct sublargs *);
#ifdef KEYARG
LOCAL LVAL assoc P5H(LVAL, LVAL, LVAL, LVAL, int);
LOCAL LVAL membr P5H(LVAL, LVAL, LVAL, LVAL, int);
#else
LOCAL LVAL assoc P4H(LVAL, LVAL, LVAL, int);
LOCAL LVAL membr P4H(LVAL, LVAL, LVAL, int);
#endif
LOCAL LVAL nsub P3H(int, int, int);
LOCAL VOID nsub1 P2H(LVAL *, struct nsubargs *);
LOCAL LVAL map P2H(int, int);
LOCAL LVAL set_op P1H(int);

/* xlcircular -- circular list error */
VOID xlcircular(V)
{
    xlfail("circular list");
}

/* xcar - take the car of a cons cell */
LVAL xcar(V)
{
    LVAL list;
    list = xlgalist();
    xllastarg();
    return (null(list) ? NIL : car(list));
}

/* xcdr - take the cdr of a cons cell */
LVAL xcdr(V)
{
    LVAL list;
    list = xlgalist();
    xllastarg();
    return (null(list) ? NIL : cdr(list));
}

/* cxxr functions */
LVAL xcaar(V) { return (cxr("aa")); }
LVAL xcadr(V) { return (cxr("da")); }
LVAL xcdar(V) { return (cxr("ad")); }
LVAL xcddr(V) { return (cxr("dd")); }

/* cxxxr functions */
LVAL xcaaar(V) { return (cxr("aaa")); }
LVAL xcaadr(V) { return (cxr("daa")); }
LVAL xcadar(V) { return (cxr("ada")); }
LVAL xcaddr(V) { return (cxr("dda")); }
LVAL xcdaar(V) { return (cxr("aad")); }
LVAL xcdadr(V) { return (cxr("dad")); }
LVAL xcddar(V) { return (cxr("add")); }
LVAL xcdddr(V) { return (cxr("ddd")); }

/* cxxxxr functions */
LVAL xcaaaar(V) { return (cxr("aaaa")); }
LVAL xcaaadr(V) { return (cxr("daaa")); }
LVAL xcaadar(V) { return (cxr("adaa")); }
LVAL xcaaddr(V) { return (cxr("ddaa")); }
LVAL xcadaar(V) { return (cxr("aada")); }
LVAL xcadadr(V) { return (cxr("dada")); }
LVAL xcaddar(V) { return (cxr("adda")); }
LVAL xcadddr(V) { return (cxr("ddda")); }
LVAL xcdaaar(V) { return (cxr("aaad")); }
LVAL xcdaadr(V) { return (cxr("daad")); }
LVAL xcdadar(V) { return (cxr("adad")); }
LVAL xcdaddr(V) { return (cxr("ddad")); }
LVAL xcddaar(V) { return (cxr("aadd")); }
LVAL xcddadr(V) { return (cxr("dadd")); }
LVAL xcdddar(V) { return (cxr("addd")); }
LVAL xcddddr(V) { return (cxr("dddd")); }

/* cxr - common car/cdr routine */
LOCAL LVAL cxr P1C(char *, adstr)
{
    LVAL list;

    /* get the list */
    list = xlgalist();
    xllastarg();

    /* perform the car/cdr operations */
    while (*adstr && consp(list))
	list = (*adstr++ == 'a' ? car(list) : cdr(list));

    /* make sure the operation succeeded */
    if (*adstr && !null(list))
	xlfail("bad argument");

    /* return the result */
    return (list);
}

/* xcons - construct a new list cell */
LVAL xcons(V)
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* construct a new list element */
    return (cons(arg1,arg2));
}

/* xlist - built a list of the arguments */
/* Rewritten by TAA for compactness and speed */
LVAL xlist(V)
{
    LVAL val;
    int i=xlargc;

    /* protect a pointer */
    xlsave1(val);

    /* do the work */
    while (i-- > 0)
        val = cons(xlargv[i],val);

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}

/* xliststar - built a list of the arguments */
/* by TAA */
LVAL xliststar(V)
{
    LVAL val;
    int i=xlargc;

    if (i==0) xltoofew();   /* must have at least one argument */

    /* protect a pointer */
    xlprot1(val);

    /* last argument is list tail */

    val = xlargv[--i];

    /* do the work */
    while (i-- > 0)
        val = cons(xlargv[i],val);

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}

/* xbutlast -- copy list for all but last n */
/* Added function TAA */

LVAL xbutlast(V)
{
    LVAL val,list,last,next;
    FIXTYPE n=1,l=0;

    /* get argument(s) */
    list = xlgalist();
    if (moreargs()) {
        n = getfixnum(next=xlgafixnum());
        if (n<0) xlerror("bad index",next);
        xllastarg();
    }

    /* get length */
    for (next=list; consp(next);) {
        next=cdr(next);
        l++;
        if (l > nnodes) xlcircular();
    }

    /* calc final length */
    l-=n;
    if (l <= 0) return (NIL);   /* nothing left */

    /* do the first cons */

    val = consa(car(list));
    if (l-- == 1) return val;

    /* protect a pointer */
    xlprot1(val);

    /* do remaining conses */
    last = val;
    while (l-- > 0) {
        list = cdr(list);
        next = consa(car(list));
        rplacd(last,next);
	last = next;
    }


    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}


/* xappend - built-in function append */
LVAL xappend(V)
{
    LVAL list,last=NIL,next,val;
    long n;

    /* protect some pointers */
    xlsave1(val);

    /* append each argument */
    if (moreargs()) {
	while (xlargc > 1) {
            /* check for circular list (Added 5/6/94) */
            next = list = nextarg();
            for (n = 0; consp(next); next=cdr(next)) {
                if (n++ > nnodes) xlcircular(); /*DIRTY, but we loose anyway!*/
            }
	    /* append each element of this list to the result list */
	    for (; consp(list); list = cdr(list)) {
		next = consa(car(list));
		if (!null(val)) rplacd(last,next);
		else val = next;
		last = next;
	    }
	    if (!null(list)) xlbadtype(*--xlargv);  /*TAA added errormessage*/
	}

	/* handle the last argument */
	if (!null(val)) rplacd(last,nextarg());
	else val = nextarg();
    }

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}

/* xlast - return the last cons of a list */
LVAL xlast(V)
{
    LVAL list;
    long l=0;

    /* get the list */
    list = xlgalist();
    xllastarg();

    /* find the last cons */
    if (consp(list))            /* TAA fix */
      while (consp(cdr(list))) {
	list = cdr(list);
	if (l++ > nnodes) xlcircular();
      }

    /* return the last element */
    return (list);
}

/* xmember - built-in function 'member' */
LVAL xmember(V)
{
  LVAL x,list,slist,fcn,val;
  int tresult;
#ifdef KEYARG
  LVAL kfcn;

  /* protect some pointers */
  xlstkcheck(2);
  xlsave(fcn);
  xlsave(kfcn);
#else
  /* protect some pointers */
  xlsave1(fcn);
#endif

  /* get the expression to look for and the list */
  x = xlgetarg();
  slist = list = xlgalist();
  xltest(&fcn,&tresult);

#ifdef KEYARG
  kfcn = xlkey();
#endif

  xllastkey();

  /* look for the expression */
  for (val = NIL; consp(list); list = cdr(list), slist = cdr(slist)) {
    /* do a pair per iteration */

#ifdef KEYARG
    if (dotest2(x,car(list),fcn,kfcn) == tresult)
#else
    if (dotest2(x,car(list),fcn) == tresult)
#endif
      {
	val = list;
	break;
      }

    if (!consp(list = cdr(list))) break;
        
#ifdef KEYARG
    if (dotest2(x,car(list),fcn,kfcn) == tresult)
#else
    if (dotest2(x,car(list),fcn) == tresult)
#endif
      {
	val = list;
	break;
      }

    if (list == slist)  /* list must be circular, and no match */
      xlerror("not a proper list", car (list));
  }

  /* restore the stack */
#ifdef KEYARG
  xlpopn(2);
#else
  xlpop();
#endif

  /* return the result */
  return (val);
}

/* xassoc - built-in function 'assoc' */
LVAL xassoc(V)
{
  LVAL x,alist,slist,fcn,pair,val;
  int tresult;
#ifdef KEYARG
  LVAL kfcn;

  /* protect some pointers */
  xlstkcheck(2);
  xlsave(fcn);
  xlsave(kfcn);
#else
  /* protect some pointers */
  xlsave1(fcn);
#endif

  /* get the expression to look for and the association list */
  x = xlgetarg();
  slist = alist = xlgalist();
  xltest(&fcn,&tresult);

#ifdef KEYARG
  kfcn = xlkey();
#endif

  xllastkey();

  /* look for the expression */
  for (val = NIL; consp(alist); alist = cdr(alist), slist = cdr(slist)) {
    /* do two iterations per loop */
    if ((!null(pair = car(alist))) && consp(pair))
#ifdef KEYARG
      if (dotest2(x,car(pair),fcn,kfcn) == tresult)
#else
      if (dotest2(x,car(pair),fcn) == tresult)
#endif
	{
	  val = pair;
	  break;
	}

    if (!consp(alist = cdr(alist))) break;

    if ((!null(pair = car(alist))) && consp(pair))
#ifdef KEYARG
      if (dotest2(x,car(pair),fcn,kfcn) == tresult)
#else
      if (dotest2(x,car(pair),fcn) == tresult)
#endif
	{
	  val = pair;
	  break;
	}

    if (slist == alist) break;  /* circular alist */
  }

  /* restore the stack */
#ifdef KEYARG
  xlpopn(2);
#else
  xlpop();
#endif

  /* return result */
  return (val);
}

/* xnsubst,xnsublis - destructive versions of subst and sublis */
/* ADDED 7/93 */
LOCAL VOID nsub1 P2C(LVAL *, tree, struct nsubargs *, args)
{
  LVAL pair = NULL;
  FIXTYPE n=0;

tailrecursion:

#ifdef KEYARG
  if (args->subst? 
      (args->expr?
       (dotest2(args->from,*tree,args->fcn,args->kfcn)==args->tresult): 
       (dotest1(*tree, args->fcn, args->kfcn)==args->tresult)) :
      !null(pair=assoc(args->kfcn!=NIL?xlapp1(args->kfcn,*tree):*tree,args->to,args->fcn,NIL,args->tresult)))
#else
  if (args->subst? 
      (args->expr?
       (dotest2(args->from,*tree,args->fcn)==args->tresult): 
       (dotest1(*tree, args->fcn)==args->tresult)) :
      !null(pair=assoc(*tree,args->to,args->fcn,args->tresult)))
#endif
    {
      *tree = (args->subst ? args->to : cdr(pair));
    }
    else if (consp(*tree)) {
#ifdef STSZ	    /* This function is a good candidate for stack ov */
      stchck();
#endif
      nsub1(&car(*tree), args);
      tree = &cdr(*tree);
      if (++n > nnodes) 
	xlfail("circular list");    /* only the tip of the iceburg */
      goto tailrecursion;
    }
    else return;
}

LOCAL LVAL nsub P3C(int, subst, int, tresult, int, expr)
{
  struct nsubargs args;
  LVAL tree;
  /* protect some pointers */
#ifdef KEYARG
  xlstkcheck(2);
  xlsave(args.fcn);
  xlsave(args.kfcn);
#else
  xlsave1(args.fcn);
#endif

  args.subst = subst;
  args.tresult = tresult;
  args.expr = expr;

  if (expr) { /* get the expressions and the tree */
    args.to = xlgetarg();
    if (subst) args.from = xlgetarg();
    tree = xlgetarg();
    xltest(&args.fcn, &args.tresult);
  }
  else {
    /* get the result expression, the function and the tree */
    args.to = xlgetarg();
    args.fcn = xlgetarg();
    tree = xlgetarg();
  }
    
#ifdef KEYARG
  args.kfcn = xlkey();
#endif

  xllastkey();

  nsub1(&tree, &args);
    
#ifdef KEYARG
  xlpopn(2);
#else
  xlpop();
#endif

  return (tree);
}

LVAL xnsubst(V) { return nsub(TRUE, TRUE, TRUE);}
LVAL xnsubstif(V) { return nsub(TRUE, TRUE, FALSE); }
LVAL xnsubstifnot(V) { return nsub(TRUE, FALSE, FALSE); }
LVAL xnsublis(V) { return nsub(FALSE, TRUE, TRUE);}

/* xsubst - substitute one expression for another */
LVAL xsubst(V)
{
    struct substargs args;
    LVAL expr;

    /* protect some pointers */
#ifdef KEYARG
    xlstkcheck(2);
    xlsave(args.fcn);
    xlsave(args.kfcn);
#else
    xlsave1(args.fcn);
#endif

    /* get the to value, the from value and the expression */
    args.to = xlgetarg();
    args.from = xlgetarg();
    expr = xlgetarg();
    xltest(&args.fcn,&args.tresult);

#ifdef KEYARG
    args.kfcn = xlkey();
#endif

    xllastkey();

    /* do the substitution */
    expr = subst(expr,&args);

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the result */
    return (expr);
}

/* subst - substitute one expression for another */
LOCAL LVAL subst P2C(LVAL, expr, struct substargs *, args)
{
    LVAL carval,cdrval;

#ifdef KEYARG
    if (dotest2(args->from,expr,args->fcn,args->kfcn) == args->tresult)
#else
    if (dotest2(args->from,expr,args->fcn) == args->tresult)
#endif
	return (args->to);
    else if (consp(expr)) {
#ifdef STSZ	    /* This function is a good candidate for stack ov */
        stchck();
#endif
	xlsave1(carval);
        carval = subst(car(expr),args);
        cdrval = subst(cdr(expr),args);
	xlpop();

	/* the following TAA mod makes subst like COMMON LISP */
        if ((carval == car(expr)) && (cdrval == cdr(expr)))
            return expr; /* no change */
        else
	return (cons(carval,cdrval));
    }
    else
	return (expr);
}

/* xsublis - substitute using an association list */
LVAL xsublis(V)
{
    struct sublargs args;
    LVAL expr;

    /* protect some pointers */
#ifdef KEYARG
    xlstkcheck(2);
    xlsave(args.fcn);
    xlsave(args.kfcn);
#else
    xlsave1(args.fcn);
#endif

    /* get the assocation list and the expression */
    args.alist = xlgalist();
    expr = xlgetarg();
    xltest(&args.fcn,&args.tresult);

#ifdef KEYARG
    args.kfcn = xlkey();
#endif

    xllastkey();

    /* do the substitution */
    expr = sublis(expr,&args);

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the result */
    return (expr);
}

/* sublis - substitute using an association list */
LOCAL LVAL sublis P2C(LVAL, expr, struct sublargs *, args)
{
    LVAL carval,cdrval,pair;

#ifdef KEYARG
    if (!null(pair = assoc(args->kfcn!=NIL?
			   xlapp1(args->kfcn,expr):
			   expr,
			   args->alist,
			   args->fcn,
			   NIL,
			   args->tresult)))
#else
    if (!null(pair = assoc(expr,args->alist,args->fcn,args->tresult)))
#endif
	return (cdr(pair));
    else if (consp(expr)) {
#ifdef STSZ	    /* This function is a good candidate for stack ov */
	stchck();
#endif
	xlsave1(carval);
        carval = sublis(car(expr),args);
        cdrval = sublis(cdr(expr),args);
	xlpop();
	/* TAA MOD for making like common lisp */
        if ((car(expr) == carval) && (cdr(expr) == cdrval))
            return (expr);
        else
	return (cons(carval,cdrval));
    }
    else
	return (expr);
}

/* assoc - find a pair in an association list */
#ifdef KEYARG
LOCAL LVAL assoc P5C(LVAL, expr, LVAL, alist, LVAL, fcn, LVAL, kfcn, int, tresult)
#else
LOCAL LVAL assoc P4C(LVAL, expr, LVAL, alist, LVAL, fcn, int, tresult)
#endif
{
    LVAL pair;

    for (; consp(alist); alist = cdr(alist))
	if ((!null((pair = car(alist)))) && consp(pair))
#ifdef KEYARG
            if (dotest2(expr,car(pair),fcn,kfcn) == tresult)
#else
	    if (dotest2(expr,car(pair),fcn) == tresult)
#endif
		return (pair);
    return (NIL);
}

/* xnth - return the nth element of a list */
LVAL xnth(V)
{
    return (nth(TRUE));
}

/* xnthcdr - return the nth cdr of a list */
LVAL xnthcdr(V)
{
    return (nth(FALSE));
}

/* nth - internal nth function */
LOCAL LVAL nth P1C(int, carflag)
{
    LVAL list,num;
    FIXTYPE n;

    /* get n and the list */
    num = xlgafixnum();
/*  list = xlgacons(); */
    list = xlgalist();      /* TAA fix */
    xllastarg();

    /* make sure the number isn't negative */
    if ((n = getfixnum(num)) < 0)
	xlfail("bad argument");

    /* find the nth element */
    while (consp(list) && --n >= 0)
	list = cdr(list);

    /* return the list beginning at the nth element */
    return (carflag && consp(list) ? car(list) : list);
}

/* xlength - return the length of a list or string */
LVAL xlength(V)
{
    FIXTYPE n = 0;
    LVAL arg;

    /* get the list or string */
    arg = xlgetarg();
    xllastarg();

    /* find the length of a list */
    if (listp(arg))
	for (n = 0; consp(arg);) {
	    arg = cdr(arg);
	    n++;
	    if (n > nnodes) xlcircular();   /*DIRTY, but we loose anyway!*/
	}

    /* find the length of a string */
    else if (stringp(arg))
	n = (FIXTYPE)getslength(arg);

    /* find the length of a typed vector */
    else if (tvecp(arg))
        n = (FIXTYPE)gettvecsize(arg);

    /* find the length of a vector */
    else if (vectorp(arg))
	n = (FIXTYPE)getsize(arg);

    /* otherwise, bad argument type */
    else
	xlbadtype(arg);

    /* return the length */
    return (cvfixnum(n));
}

/* xlistlength -- return the length of a list */
LVAL xlistlength(V)
{
    FIXTYPE n = 0;
    LVAL arg, sarg;
    
    /* get the list */
    arg = sarg = xlgalist();
    xllastarg();
    
    while (consp(arg)) {
        arg = cdr(arg);
        if (!consp(arg)) { n++; break; }
        if (sarg == arg) return NIL;    /* circular list */
        arg = cdr(arg);
        sarg = cdr(sarg);
        n += 2;
    }
    
    /* return the length */
    return (cvfixnum(n));
}


/* map - internal mapping function */
#define CONCAT 2    /* third choice for valflag */

LOCAL LVAL map P2C(int, carflag, int, valflag)
{
    FRAMEP newfp;
    LVAL fun,lists,val,last,p,x,y;
    int argc;
    long n=0, nmax=nnodes;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(fun);
    xlsave(lists);
    xlsave(val);

    /* get the function to apply and the first list */
    fun = xlgetarg();
    lists = xlgalist();

    /* initialize the result list */
    val = (valflag ? NIL : lists);

    /* build a list of argument lists */
    argc = 1;
    for (lists = last = consa(lists); moreargs(); last = cdr(last)) {
	argc++;
	rplacd(last,cons(xlgalist(),NIL));
    }

    /* loop through each of the argument lists */
    for (;;) {

        if (n++ > nmax) xlcircular();

	/* build an argument list from the sublists */
	newfp = xlsp;
	pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	pusharg(fun);
	pusharg(cvfixnum((FIXTYPE)argc));
	for (x = lists; (consp(x)) && (consp(y = car(x))); x = cdr(x)) {
	    pusharg(carflag ? car(y) : y);
	    rplaca(x,cdr(y));
	}

	/* quit if any of the lists were empty */
	if (!null(x)) {
	    xlsp = newfp;
	    break;
	}

	/* apply the function to the arguments */
	xlfp = newfp;
        switch (valflag) {
        case CONCAT:
            p = xlapply(argc);
            if (!null(p)) {
                if (!consp(p)) xlerror("non-list to concatenate", p);
                if (null(val)) val = p;
                else rplacd(last, p);
                while (consp(cdr(p))) p = cdr(p); /* find end--no circular check */
                last = p;
            }
            break;

        case TRUE:
	    p = consa(xlapply(argc));
            if (!null(val)) rplacd(last,p);
	    else val = p;
	    last = p;
            break;

        case FALSE:
	    xlapply(argc);
            break;
    }
    }

    /* restore the stack */
    xlpopn(3);

    /* return the last test expression value */
    return (val);
}

/* xmapc - built-in function 'mapc' */
LVAL xmapc(V)
{
    return (map(TRUE,FALSE));
}

/* xmapcar - built-in function 'mapcar' */
LVAL xmapcar(V)
{
    return (map(TRUE,TRUE));
}

/* xmapl - built-in function 'mapl' */
LVAL xmapl(V)
{
    return (map(FALSE,FALSE));
}

/* xmaplist - built-in function 'maplist' */
LVAL xmaplist(V)
{
    return (map(FALSE,TRUE));
}

/* xmapcan - built-in function 'mapcan' */
LVAL xmapcan(V)
{
    return (map(TRUE,CONCAT));
}

/* xmapcon - built-in function 'mapcon' */
LVAL xmapcon(V)
{
    return (map(FALSE,CONCAT));
}

/* xrplca - replace the car of a list node */
LVAL xrplca(V)
{
    LVAL list,newcar;

    /* get the list and the new car */
    list = xlgacons();
    newcar = xlgetarg();
    xllastarg();

    /* replace the car */
    rplaca(list,newcar);

    /* return the list node that was modified */
    return (list);
}

/* xrplcd - replace the cdr of a list node */
LVAL xrplcd(V)
{
    LVAL list,newcdr;

    /* get the list and the new cdr */
    list = xlgacons();
    newcdr = xlgetarg();
    xllastarg();

    /* replace the cdr */
    rplacd(list,newcdr);

    /* return the list node that was modified */
    return (list);
}

/* xnconc - destructively append lists */
LVAL xnconc(V)
{
    LVAL next,last=NIL,val=NIL;
    long l; /* TAA MOD */

    /* concatenate each argument */
    if (moreargs()) {
	while (xlargc > 1) {

            /* TAA mod -- give error message if not a list */
	    if ((!null(next = nextarg())) && consp(next)) {

		/* concatenate this list to the result list */
		if (!null(val)) rplacd(last,next);
		else val = next;

		/* find the end of the list */
                l = 0;
		while (consp(cdr(next))) {
		    next = cdr(next);
                    if (l++ > nnodes) xlcircular();
                }
		last = next;
	    }
            else if (!null(next)) xlbadtype(*--xlargv); /* TAA -- oops! */
	}

	/* handle the last argument */
	if (!null(val)) rplacd(last,nextarg());
	else val = nextarg();
    }

    /* return the list */
    return (val);
}

/* xsort - built-in function 'sort' */
LOCAL LVAL xlmergesort P3C(LVAL, list, LVAL, sortfcn, LVAL, sortkey)
{
  /* Strategy: divide into two parts, (recurse) to sort each, then
     merge them together */
  LVAL left, right;

  /* less than 2 cells needn't be sorted */ 
  if (!(consp(list) && consp(cdr(list))))
    return list;

  xlstkcheck(5);		/* Only two are used at recursion */
  xlprotect(left);
  xlprotect(right);

  /* Find the center of the list */
  {
    unsigned i=0;
    LVAL temp = NULL;
    left = right = list;
    while (consp(list) && consp(list=cdr(list))) {
      list = cdr(list);
      right = cdr(temp=right);
      if ((i += 2) > /*MAXSLEN*/ nnodes) xltoolong();
    }
    rplacd(temp, NIL);		/* split left and right parts */
  }

  left = xlmergesort(left, sortfcn, sortkey);
  right = xlmergesort(right, sortfcn, sortkey);

  {
    LVAL result, resultt = NULL, leftarg, rightarg;
    xlsave(leftarg);
    xlsave(rightarg);
    xlsave(result);		/* set to NIL */
    leftarg = null(sortkey) ? car(left) : xlapp1(sortkey, car(left));
    rightarg = null(sortkey) ? car(right) : xlapp1(sortkey, car(right));

    while (TRUE) {
      if (!dotest2(leftarg, rightarg, sortfcn, NIL) &&
	  dotest2(rightarg, leftarg, sortfcn, NIL)) {
	/* right is smaller */
	if (null(result)) {
	  result = resultt = right;
	}
	else {
	  rplacd(resultt, right);
	  resultt = right;
	}
	right = cdr(right);
	if (null(right)) {	/* finished the merge */
	  rplacd(resultt, left);
	  break;
	}
	rightarg=null(sortkey) ? car(right) : xlapp1(sortkey,car(right));
      }
      else  {			/* left is smaller */
	if (null(result)) {
	  result = resultt = left;
	}
	else {
	  rplacd(resultt, left);
	  resultt = left;
	}
	left = cdr(left);
	if (null(left)) {	/* finished the merge */
	  rplacd(resultt, right);
	  break;
	}
	leftarg=null(sortkey) ? car(left) : xlapp1(sortkey,car(left));
      }
    }
    xlpopn(5);
    return result;
  }
}

LVAL xsort()
{
  LVAL list, sortfcn;

  /* protect some pointers */
  LVAL sortkey;
  xlstkcheck(3);
  xlsave(sortkey);
  xlsave(list);
  xlsave(sortfcn);

  /* get the list to sort and the comparison function */
  list = xlgetarg();
  sortfcn = xlgetarg();
  sortkey = xlkey();
  xllastkey();

  /* sort the list */
  if (!null(list)) switch (ntype(list)) {
  case DARRAY:
  case VECTOR:
  case TVEC:
  case STRING:
    {
      LVAL etype = gettvecetype(list);
      list = coerce_to_list(list);
      list = xlmergesort(list, sortfcn, sortkey);
      list = coerce_to_tvec(list, etype);
      break;
    }
  case CONS:
    list = xlmergesort(list, sortfcn, sortkey);
    break;
  default: xlbadtype(list);
  }

  /* restore the stack and return the sorted list */
  xlpopn(3);
  return (list);
}


/* These functions have the following copyright notice: */
/* XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney                  */
/*      All Rights Reserved                                            */
/*      Permission is granted for unrestricted non-commercial use      */

/* membr - internal MEMBER for set functions TAA */
#ifdef KEYARG
LOCAL LVAL membr P5C(LVAL, expr, LVAL, list, LVAL, fcn, LVAL, kfcn, int, tresult)
{
    xlprot1(expr);
    if (!null(kfcn)) expr = xlapp1(kfcn,expr);
    for (; consp(list); list = cdr(list))
        if (dotest2(expr,car(list),fcn,kfcn) == tresult) {
            xlpop();
            return (list);
        }
    xlpop();
    return (NIL);
}
#else
LOCAL LVAL membr P4C(LVAL, expr, LVAL, list, LVAL, fcn, int, tresult)
  LVAL expr,list,fcn; int tresult;
{
    for (; consp(list); list = cdr(list))
        if (dotest2(expr,car(list),fcn) == tresult)
                return (list);
    return (NIL);
}
#endif

/* Common Lisp ADJOIN function */
LVAL xadjoin(V)
{
    LVAL x, list, fcn;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    xlsave1(fcn);
#endif

    /* get the lists and key arguements, if any */
    x = xlgetarg();
    list = xlgalist();
    xltest(&fcn,&tresult);
#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

#ifdef KEYARG
    if (null(membr(x,list,fcn,kfcn,tresult))) list = cons(x,list) ;
    xlpopn(2);
#else
    if (null(membr(x,list,fcn,tresult))) list = cons(x,list) ;
    xlpop();
#endif

    return list;
}

LOCAL LVAL set_op P1C(int, which)
{
    LVAL x, list1, list2, result, fcn;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(kfcn);
#else
    
    /* protect some pointers */
    xlstkcheck(2);
#endif
    xlsave(fcn);
    xlsave(result);

    /* get the lists and key arguements, if any */
    list1 = xlgalist();
    list2 = xlgalist();
    xltest(&fcn,&tresult);
#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

    switch(which) {
        case 'U':
            for (result = list1; consp(list2); list2 = cdr(list2)) {
                x = car(list2);
#ifdef KEYARG
                if (null(membr(x,list1,fcn,kfcn,tresult)))
#else
                if (null(membr(x,list1,fcn,tresult)))
#endif
                    result = cons(x, result);
    }
            break;
        case 'I':
            for (result = NIL; consp(list2); list2 = cdr(list2)) {
                x = car(list2);
#ifdef KEYARG
                if (!null(membr(x,list1,fcn,kfcn,tresult)))
#else
                if (!null(membr(x,list1,fcn,tresult)))
#endif
                    result = cons(x, result);
            }
            break;
        case 'D':
            for (result = NIL; consp(list1); list1 = cdr(list1)) {
                x = car(list1);
#ifdef KEYARG
                if (null(membr(x,list2,fcn,kfcn,tresult)))
#else
                if (null(membr(x,list2,fcn,tresult)))
#endif
                    result = cons(x, result);
            }
            break;
        case 'S':
            for (result = s_true; consp(list1); list1 = cdr(list1)) {
                x = car(list1);
#ifdef KEYARG
                if (null(membr(x,list2,fcn,kfcn,tresult)))
#else
                if (null(membr(x,list2,fcn,tresult)))
#endif
                {
                    result = NIL;
                    break;
                }
            }
            break;
    }

#ifdef KEYARG
    xlpopn(3);
#else
    xlpopn(2);
#endif
    return(result);
}

LVAL xunion(V)          { return(set_op('U')); }
LVAL xintersection(V)   { return(set_op('I')); }
LVAL xset_difference(V) { return(set_op('D')); }
LVAL xsubsetp(V)        { return(set_op('S')); }


/* HASH TABLES ARE IMPLEMENTED AS STRUCTS, WITHOUT ACCESSING FCNS */

#ifdef HASHFCNS
/* The hash tables have been modified to allow fast EQ, EQL and EQUAL
hashing by using addresses in the hash function. Since addresses are
not preserved accross save/restores, xlimage and dlimage use uflags to
mark hash tables for rehashing on the next access. (xlimage and
dlimage set uflags to TRUE for any STRUCT. If a proper hashtable type
is introduced, this should be changed.) In addition, the
:REHASH-THRESHOLD and :REHASH-SIZE keywords are now supported and hash
tables are resized when new entries are added that push the count over
the threshold. To simplify resizing, the hash table is now a fixed
size structure that contains its data as a vector in one of its slots.
The count is also maintained in a slot.
*/

#define HTABSIZE 6
#define HTAB_REHASH_THRESHOLD 0.8
#define HTAB_REHASH_SIZE 1.3
#define MAXHTABSIZE MAXSLEN

#define hashtablep(x)      (structp(x) && (getelement(x,0) == a_hashtable))
#define xlgahashtable()    (testarg(typearg(hashtablep)))

#define hashtablerehash(x)        nuflags(x)
#define sethashtablerehash(x,y)   setnuflags(x,y)

#define hashtablesize(x)          getsize(hashtabledata(x))
#define hashtablefun(x)           getelement(x,1)
#define sethashtablefun(x,fun)    setelement(x,1,fun)
#define hashtablecount(x)         getfixnum(getelement(x,2))
#define sethashtablecount(x,n)    setelement(x,2,cvfixnum((FIXTYPE)n))
#define hashtabledata(x)          getelement(x,3)
#define sethashtabledata(x,d)     setelement(x,3,d)
#define hashtablerhthresh(x)      getelement(x,4)
#define sethashtablerhthresh(x,v) setelement(x,4,v)
#define hashtablerhsize(x)        getelement(x,5)
#define sethashtablerhsize(x,v)   setelement(x,5,v)

#define hashtablelist(x,i)        getelement(hashtabledata(x),(i))
#define sethashtablelist(x,i,v)   setelement(hashtabledata(x),(i),v)


LOCAL VOID rehash_hashdata P3H(LVAL, LVAL, LVAL);
LOCAL VOID rehash_hashtable P1H(LVAL);
LOCAL unsigned FIXTYPE eqlhash P1H(LVAL);
LOCAL unsigned FIXTYPE equalhash P1H(LVAL);
LOCAL int hthash P3H(LVAL, int, LVAL);


LOCAL VOID rehash_hashdata P3C(LVAL, old, LVAL, new, LVAL, fun)
{
  LVAL next;
  int i, j, oldsize, newsize;

  oldsize = getsize(old);
  newsize = getsize(new);

  for (i = 0; i < oldsize; i++) {
    for (next = getelement(old, i); consp(next); next = cdr(next)) {
      j = hthash(car(car(next)), newsize, fun);
      setelement(new, j, cons(car(next), getelement(new, j)));
    }
  }
}

LOCAL VOID rehash_hashtable P1C(LVAL, table)
{
  LVAL new;

  xlsave1(new);
  new = newvector((int) hashtablesize(table));
  rehash_hashdata(hashtabledata(table), new, hashtablefun(table));
  sethashtabledata(table, new);
  sethashtablerehash(table, FALSE);
  xlpop();
}  

LOCAL unsigned FIXTYPE eqlhash P1C(LVAL, x)
{
  union {FIXTYPE i; FLOTYPE j; unsigned FIXTYPE k;} swizzle;
  unsigned FIXTYPE temp;

  switch (ntype(x)) {
  case FIXNUM:
    swizzle.i = getfixnum(x);
    return swizzle.k;
#ifdef BIGNUMS
  case RATIO:
    return (eqlhash(getnumer(x)) << 2) ^ eqlhash(getdenom(x));
  case BIGNUM:
    {
      int i, n;
      n = getbignumsize(x) + 1;
      for (i = 0, temp = 0; i < n; i++)
	temp += (unsigned)getbignumarray(x)[i];
      return temp;
    }
#endif
  case FLONUM:
    swizzle.j = getflonum(x);
    return swizzle.k;
  case COMPLEX:
    return (eqlhash(getreal(x)) << 2) ^ eqlhash(getimag(x));
  default:
    return (unsigned FIXTYPE) CVPTR(x);
  }
}

LOCAL unsigned FIXTYPE equalhash P1C(LVAL, x)
{
  unsigned FIXTYPE temp;

  temp = 0;
 hashloop:
  switch (ntype(x)) {
  case STRING:
    {
      char *str = getstring(x);
      while (*str != 0)
	temp = (temp << 2) ^ *str++;
      return temp;
    }
  case CONS:
    temp = (temp << 2) ^ equalhash(car(x));
    x = cdr(x);
    goto hashloop;
  default:
    return (temp << 2) ^ eqlhash(x);
  }
}
    
LOCAL int hthash P3C(LVAL, x, int, len, LVAL, fun)
{
  if (fun == getfunction(s_eq))
    return (int) (CVPTR(x) % len);
  else if (fun == getfunction(s_eql))
    return (int) (eqlhash(x) % len);
  else if (fun == getfunction(s_equal))
    return (int) (equalhash(x) % len);
  else
    return xlhash(x, len);
}

/* Hash table functions from Ken Whedbee */
LVAL xmakehash(V)    /* rewritten by TAA */
{
  LVAL size, testfcn, result, temp;
  double rhthresh, rhsize;
  FIXTYPE len = 0;
    
  if (xlgetkeyarg(k_size,&size)) {
    if (!fixp(size) || (len=getfixnum(size)) < 1)
      xlbadtype(size);
  }
  else len = 31;
  if (len % 2 == 0) len++;
  if (len < 1) xlfail("size out of bounds"); /**** check MAXSLEN */

  if (!xlgetkeyarg(k_test,&testfcn)) testfcn = getfunction(s_eql);
  if (symbolp(testfcn) && fboundp(testfcn)) testfcn = getfunction(testfcn);

  if (!xlgetkeyarg(k_rhthresh, &temp)) temp = NIL;
  if (floatp(temp) && getflonum(temp) > 0.0 && getflonum(temp) < 1.0)
    rhthresh = getflonum(temp);
  else
    rhthresh = HTAB_REHASH_THRESHOLD;
  
  if (!xlgetkeyarg(k_rhsize, &temp)) temp = NIL;
  if (fixp(temp) && getfixnum(temp) > 0)
    rhsize = ((double) getfixnum(temp) + len) / len;
  else if (floatp(temp) && getflonum(temp) > 1.0)
    rhsize = getflonum(temp);
  else
    rhsize = HTAB_REHASH_SIZE;

  xllastkey();
    
  xlprot1(testfcn);
  xlsave1(result);

  result = newstruct(a_hashtable,HTABSIZE-1);

  sethashtablerehash(result, FALSE);
  sethashtablefun(result, testfcn);
  sethashtablecount(result,0);
  sethashtabledata(result,newvector((int)len));
  sethashtablerhthresh(result, cvflonum((FLOTYPE) rhthresh));
  sethashtablerhsize(result, cvflonum((FLOTYPE) rhsize));

  xlpopn(2);

  return result;
}

LVAL xgethash(V)
{
  LVAL alist,val,key,fun,table,def=NIL;

  key = xlgetarg();
  table = xlgahashtable();
  if (moreargs()) {
    def = xlgetarg();
    xllastarg();
  }

  if (hashtablerehash(table))
    rehash_hashtable(table);

  fun = hashtablefun(table);
  alist = hashtablelist(table, hthash(key,(int)hashtablesize(table),fun));

#ifdef KEYARG
  val = assoc(key,alist,fun,NIL,TRUE);
#else
  val = assoc(key,alist,fun,TRUE);
#endif

  /* return result */
#ifdef MULVALS
  xlnumresults = 2;
  if (null(val)) {
    xlresults[0] = def;
    xlresults[1] = NIL;
  }
  else {
    xlresults[0] = cdr(val);
    xlresults[1] = s_true;
  }
  return(xlresults[0]);
#else
  return (null(val) ? def : cdr(val));
#endif /* MULVALS */
}

LVAL xremhash(V)
/* By TAA -- can't use assoc here*/
{
  LVAL alist,key,fun,table,last;

  int idx;

  key = xlgetarg();
  table = xlgahashtable();
  xllastarg();

  if (hashtablerehash(table))
    rehash_hashtable(table);

  fun = hashtablefun(table);
  idx = hthash(key,(int)hashtablesize(table),fun);

  alist = hashtablelist(table,idx);

  if (null(alist))
    return NIL;

#ifdef KEYARG
  else if (dotest2(key,car(car(alist)),fun,NIL)==TRUE)
#else
  else if (dotest2(key,car(car(alist)),fun)==TRUE)
#endif
    {
      sethashtablelist(table,idx,cdr(alist));   /* matches first element */
      sethashtablecount(table,hashtablecount(table)-1);
      return s_true;
    }
  else {
    last = alist;
    alist = cdr(alist);
    while (consp(alist)) {
#ifdef KEYARG
      if (dotest2(key,car(car(alist)),fun,NIL)==TRUE)
#else
      if (dotest2(key,car(car(alist)),fun)==TRUE)
#endif
	{
	  rplacd(last,cdr(alist));
	  sethashtablecount(table,hashtablecount(table)-1);
	  return s_true;
	}
      last = alist;
      alist = cdr(alist);
    }
  }
    
  return NIL;
}
  
VOID xlsetgethash P3C(LVAL, key, LVAL, table, LVAL, value)
{
  LVAL alist,fun,oldval;
  int idx;

  if (! hashtablep(table))
    xlbadtype(table);

  if (hashtablerehash(table))
    rehash_hashtable(table);

  fun = hashtablefun(table);
  idx = hthash(key,(int)hashtablesize(table),fun);

  alist = hashtablelist(table,idx);

#ifdef KEYARG
  if (!null(oldval = assoc(key,alist,fun,NIL,TRUE)))
#else
  if (!null(oldval = assoc(key,alist,fun,TRUE)))
#endif
    rplacd(oldval,value);
  else {
    LVAL new, data, temp;
    double rhthresh, rhsize;
    int size, newsize;

    temp = hashtablerhthresh(table);
    if (floatp(temp) && getflonum(temp) > 0.0 && getflonum(temp) < 1.0)
      rhthresh = getflonum(temp);
    else
      rhthresh = HTAB_REHASH_THRESHOLD;
  
    temp = hashtablerhsize(table);
    if (floatp(temp) && getflonum(temp) > 1.0)
      rhsize = getflonum(temp);
    else
      rhsize = HTAB_REHASH_SIZE;

    alist = cons(cons(key,value),alist);
    sethashtablelist(table,idx,alist);
    sethashtablecount(table,hashtablecount(table)+1);
    if (hashtablecount(table) > rhthresh * hashtablesize(table)) {
      size = hashtablesize(table);
      newsize = (int) (rhsize * (size + 1));
      if (newsize % 2 == 0) newsize++;
      if (newsize < 0) xlfail("bad rehash size");
      if (size < newsize && newsize < MAXHTABSIZE) {
	xlsave1(new);
	new = newvector(newsize);
	data = hashtabledata(table);
	rehash_hashdata(data, new, fun);
	sethashtabledata(table, new);
	xlpop();
      }
    }
  }
}

/* function clrhash  TAA */

LVAL xclrhash(V)
{
  LVAL table;
  int i;

  table = xlgahashtable();
  xllastarg();

  for (i = hashtablesize(table)-1; i >= 0; i--)
    sethashtablelist(table,i,NIL);
  sethashtablecount(table,0);

  return (table);

}

/* function hash-table-count  TAA */

LVAL xhashcount(V)
{
  LVAL table;

  table = xlgahashtable();
  xllastarg();

  return (cvfixnum((FIXTYPE) hashtablecount(table)));
}

/* function maphash  TAA */

LVAL xmaphash(V)
{
  FRAMEP newfp;
  LVAL fun, table, arg, element;
  int i;

  fun = xlgetarg();
  table = xlgahashtable();
  xllastarg();

  xlstkcheck(3);
  xlprotect(fun);
  xlprotect(table);
  xlsave(element);

  for (i = hashtablesize(table)-1; i >= 0; i--)
    for (element=hashtablelist(table,i); consp(element);) {
      arg = car(element);
      element = cdr(element); /* in case element is deleted */
      newfp =xlsp;
      pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
      pusharg(fun);
      pusharg(cvfixnum((FIXTYPE) 2));
      pusharg(car(arg));
      pusharg(cdr(arg));
      xlfp = newfp;
      xlapply(2);
    }

  xlpopn(3);

  return (NIL);
}

LVAL xhashtablep(V)
{
  LVAL x;

  x = xlgetarg();
  xllastarg();
  return(hashtablep(x) ? s_true : NIL);
}

LVAL xhashtablefun(V)
{
  LVAL x, fun;

  x = xlgahashtable();
  xllastarg();
  
  fun = hashtablefun(x);
  if (fun == getfunction(s_eq))
    return(s_eq);
  else if (fun == getfunction(s_eql))
    return(s_eql);
  else if (fun == getfunction(s_equal))
    return(s_equal);
  else
    return(fun);
}

LVAL xhashtablesize(V)
{
  LVAL x;

  x = xlgahashtable();
  xllastarg();
  return(cvfixnum((FIXTYPE) hashtablesize(x)));
}

LVAL xhashtablerhthresh(V)
{
  LVAL x;

  x = xlgahashtable();
  xllastarg();
  return(hashtablerhthresh(x));
}

LVAL xhashtablerhsize(V)
{
  LVAL x;

  x = xlgahashtable();
  xllastarg();
  return(hashtablerhsize(x));
}

#endif

/* Internal version of MAKE-LIST */
LVAL mklist P2C(int, n, LVAL, elem)
{
  LVAL result;
  
  xlsave1(result);
  for (result = NIL; n > 0; n--)
    result = cons(elem, result);
  xlpop();
  return(result);
}

/* Common Lisp MAKE-LIST function */
LVAL xmklist(V)
{
  int n;
  LVAL elem = NIL;
  
  n = getfixnum(xlgafixnum());
  xlgetkeyarg(k_initelem, &elem);
  xllastkey();
  
  return(mklist(n, elem));
}

#ifdef DODO
int geteqhash P3C(LVAL, key, LVAL, table, LVAL *, pval)
{
  LVAL alist, pair;
  int hindex;

  if (hashtablerehash(table))
    rehash_hashtable(table);

  hindex = (int) (CVPTR(key) % hashtablesize(table));
  alist = hashtablelist(table, hindex);

  for (; consp(alist); alist = cdr(alist)) {
    if (consp(pair = car(alist)) && car(pair) == key) {
      *pval = cdr(pair);
      return(TRUE);
    }
  }
  return(FALSE);
}
#endif /* DODO */
