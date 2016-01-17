/* xlstruct.c - the defstruct facility */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* forward declarations */
LOCAL VOID addslot P5H(LVAL, LVAL, int, LVAL *, LVAL *);
LOCAL VOID updateslot P3H(LVAL, LVAL, LVAL);

/* local variables */
static char prefix[STRMAX+1];
static char makestr[] = "MAKE-%s";

/* xmkstruct - the '%make-struct' function */
LVAL xmkstruct(V)
{
    LVAL type,val;
    int i;

    /* get the structure type */
    type = xlgasymbol();

    /* make the structure */
    val = newstruct(type,xlargc);

    /* store each argument */
    for (i = 1; moreargs(); ++i)
	setelement(val,i,nextarg());
    xllastarg();

    /* return the structure */
    return (val);
}

/* xcpystruct - the '%copy-struct' function */
LVAL xcpystruct(V)
{
    LVAL str,val;
    int size,i;
    str = xlgastruct();
    xllastarg();
    size = getsize(str);
    val = newstruct(getelement(str,0),size-1);
    for (i = 1; i < size; ++i)
	setelement(val,i,getelement(str,i));
    return (val);
}

/* xstrref - the '%struct-ref' function */
LVAL xstrref(V)
{
    LVAL str,val;
    int i;
    str = xlgastruct();
    val = xlgafixnum(); i = (int)getfixnum(val);
    xllastarg();
    if (i >= getsize(str)) /* wrong structure TAA MOD fix*/
	xlerror("bad structure reference",str);
    return (getelement(str,i));
}

/* xstrset - the '%struct-set' function */
LVAL xstrset(V)
{
    LVAL str,val;
    int i;
    str = xlgastruct();
    val = xlgafixnum(); i = (int)getfixnum(val);
    val = xlgetarg();
    xllastarg();
    if (i >= getsize(str)) /* wrong structure TAA MOD fix*/
	xlerror("bad structure reference",str);
    setelement(str,i,val);
    return (val);
}

/* xstrtypep - the '%struct-type-p' function */
LVAL xstrtypep(V)
{
  LVAL type,val;
  type = xlgasymbol();
  val = xlgetarg();
  xllastarg();
  if (structp(val)) {
    for (val = getelement(val,0);
	 ! null(val);
	 val = xlgetprop(val,s_strinclude))
      if (val == type) return(s_true);
    return(NIL);
  }
  else return(NIL);
}

/* xdefstruct - the 'defstruct' special form */
LVAL xdefstruct(V)
{
  LVAL structname,slotname,defexpr,sym,tmp,args,body;
  LVAL options,oargs,slots,constrsym,predsym;
  char *pname = NULL;
  int slotn, has_include;

  /* protect some pointers */
  xlstkcheck(6);
  xlsave(structname);
  xlsave(slotname);
  xlsave(defexpr);
  xlsave(args);
  xlsave(body);
  xlsave(tmp);

  /* initialize */
  args = body = NIL;
  slotn = 0;
  has_include = FALSE;
  constrsym = NULL;
  predsym = NULL;

  /* get the structure name */
  tmp = xlgetarg();
  if (symbolp(tmp)) {
    structname = tmp;
    pname = getstring(getpname(structname));
    sprintf(prefix, "%s-", pname);
  }

  /* get the structure name and options */
  else if (consp(tmp) && symbolp(car(tmp))) {
    structname = car(tmp);
    pname = getstring(getpname(structname));
    sprintf(prefix, "%s-", pname);

    /* handle the list of options */
    for (options = cdr(tmp); consp(options); options = cdr(options)) {

      /* get the next argument */
      tmp = car(options);

      /* handle options that don't take arguments */
      if (symbolp(tmp)) {
	xlerror("unknown option",tmp);
      }

      /* handle options that take arguments */
      else if (consp(tmp) && symbolp(car(tmp))) {
	oargs = cdr(tmp);

	/* check for the :CONC-NAME keyword */
	if (car(tmp) == k_concname) {

	  /* get the name of the structure to include */
	  if (!consp(oargs) || !symbolp(car(oargs)))
	    xlerror("expecting a symbol",oargs);

	  /* save the prefix */
	  if (null(car(oargs)))
	    STRCPY(prefix, "");
	  else
	    STRCPY(prefix,getstring(getpname(car(oargs))));
	}

	/* check for the :INCLUDE keyword */
	else if (car(tmp) == k_include) {
	  LVAL parent;

	  if (has_include)
	    xlfail("only one :INCLUDE option allowed");
	  else
	    has_include = TRUE;

	  /* get the name of the structure to include */
	  if (!consp(oargs) || !symbolp(car(oargs)))
	    xlerror("expecting a structure name",oargs);
	  parent = tmp = car(oargs);
	  oargs = cdr(oargs);

	  /* add each slot from the included structure */
	  slots = xlgetprop(tmp,s_sslots);
	  for (; consp(slots); slots = cdr(slots)) {
	    if (consp(car(slots)) && consp(cdr(car(slots)))) {

	      /* get the next slot description */
	      tmp = car(slots);

	      /* create the slot access functions */
	      addslot(car(tmp),car(cdr(tmp)),++slotn,&args,&body);
	    }
	  }

	  /* handle slot initialization overrides */
	  for (; consp(oargs); oargs = cdr(oargs)) {
	    tmp = car(oargs);
	    if (symbolp(tmp)) {
	      slotname = tmp;
	      defexpr = NIL;
	    }
	    else if (consp(tmp) && symbolp(car(tmp))) {
	      slotname = car(tmp);
	      defexpr = (consp(cdr(tmp)) ? car(cdr(tmp)) : NIL);
	    }
	    else
	      xlerror("bad slot description",tmp);
	    updateslot(args,slotname,defexpr);
	  }
	  xlputprop(structname,parent,s_strinclude);
	}
	/* check for :PRINT-FUNCTION option (Ken Whedbee) */
	else if (car(tmp) == k_prntfunc) {
	  if (!consp(oargs) || !symbolp(car(oargs)))
	    xlerror("expecting a print function name",oargs);
	  xlputprop(structname,car(oargs),s_prntfunc);
	}
	else if (car(tmp) == k_construct) {
	  if (!consp(oargs) || !symbolp(car(oargs)))
	    xlerror("expecting a constructor function name",oargs);
	  if (consp(cdr(oargs)))
	    xlfail("BOA constructors not supported");
	  constrsym = car(oargs);
	  if (! symbolp(constrsym)) xlbadtype(constrsym);
	  xlputprop(structname,constrsym, s_strconstruct);
	}
	else if (car(tmp) == k_predicate) {
	  if (!consp(oargs) || !symbolp(car(oargs)))
	    xlerror("expecting a predicate function name",oargs);
	  predsym = car(oargs);
	  if (! symbolp(predsym)) xlbadtype(predsym);
	}
	else
	  xlerror("unknown option",tmp);
      }
      else
	xlerror("bad option syntax",tmp);
    }
  }

  /**** need to add documentation string */
  /* flush documentation string */
  if (moreargs() && stringp(*xlargv)) (void)(nextarg());

  /* get each of the structure members */
  while (moreargs()) {

    /* get the slot name and default value expression */
    tmp = xlgetarg();
    if (symbolp(tmp)) {
      slotname = tmp;
      defexpr = NIL;
    }
    else if (consp(tmp) && symbolp(car(tmp))) {
      slotname = car(tmp);
      defexpr = (consp(cdr(tmp)) ? car(cdr(tmp)) : NIL);
    }
    else
      xlerror("bad slot description",tmp);

    /* create a closure for non-trivial default expressions */
    if (defexpr != NIL) {
      tmp = newclosure(NIL,s_lambda,xlenv,xlfenv);
      setbody(tmp,cons(defexpr,NIL));
      tmp = cons(tmp,NIL);
      defexpr = tmp;
    }

    /* create the slot access functions */
    addslot(slotname,defexpr,++slotn,&args,&body);
  }

  /* store the slotnames and default expressions */
  xlputprop(structname,args,s_sslots);

  /* enter the MAKE-xxx symbol */
  if (constrsym == NULL) {
    sprintf(buf, makestr, pname);
#ifdef PACKAGES
    constrsym = xlintern(buf, getvalue(s_package));
#else
    constrsym = xlenter(buf);
#endif /* PACKAGES */
    xlputprop(structname,constrsym, s_strconstruct);      
  }

  /* make the MAKE-xxx function */
  if (! null(constrsym)) {
    args = cons(lk_key,args);
    tmp = cons(structname,NIL);
    tmp = cons(s_quote,tmp);
    body = cons(tmp,body);
    body = cons(s_mkstruct,body);
    body = cons(body,NIL);
    setfunction(constrsym,
		xlclose(constrsym,s_lambda,args,body,xlenv,xlfenv));
  }

  /* enter the xxx-P symbol */
  if (predsym == NULL) {
    sprintf(buf,"%s-P", pname);
#ifdef PACKAGES
    predsym = xlintern(buf, getvalue(s_package));
#else
    predsym = xlenter(buf);
#endif /* PACKAGES */
  }

  /* make the xxx-P function */
  if (! null(predsym)) {
    args = cons(s_x,NIL);
    body = cons(s_x,NIL);
    tmp = cons(structname,NIL);
    tmp = cons(s_quote,tmp);
    body = cons(tmp,body);
    body = cons(s_strtypep,body);
    body = cons(body,NIL);
    setfunction(predsym,
		xlclose(predsym,s_lambda,args,body,NIL,NIL));
  }

  /* enter the COPY-xxx symbol */
  sprintf(buf,"COPY-%s", pname);
#ifdef PACKAGES
  sym = xlintern(buf, getvalue(s_package));
#else
  sym = xlenter(buf);
#endif /* PACKAGES */

  /* make the COPY-xxx function */
  args = cons(s_x,NIL);
  body = cons(s_x,NIL);
  body = cons(s_cpystruct,body);
  body = cons(body,NIL);
  setfunction(sym,
	      xlclose(sym,s_lambda,args,body,NIL,NIL));

  /* restore the stack */
  xlpopn(6);

  /* return the structure name */
  return (structname);
}

/* xlrdstruct - convert a list to a structure (used by the reader) */
/* Modified by TAA to quote arguments and accept leading colons on keys */
LVAL xlrdstruct P1C(LVAL, list)
{
    LVAL structname,slotname,expr,last,val;

    /* protect the new structure */
    xlsave1(expr);

    /* get the structure name */
    if (!consp(list) || !symbolp(car(list)))
	xlerror("bad structure initialization list",list);
    structname = car(list);
    list = cdr(list);

    /* initialize the constructor function call expression */
    expr = consa(xlgetprop(structname, s_strconstruct));
    last = expr;

    /* turn the rest of the initialization list into keyword arguments */
    while (consp(list) && consp(cdr(list))) {

	/* get the slot keyword name */
	slotname = car(list);
	if (!symbolp(slotname))
	    xlerror("expecting a slot name",slotname);

	/* add the slot keyword */
#ifdef PACKAGES
	rplacd(last,consa(xlintern(getstring(getpname(slotname)), xlkeypack)));
#else
	if (*(getstring(getpname(slotname))) != ':') { /* add colon */
	    sprintf(buf,":%s",getstring(getpname(slotname)));
	    rplacd(last,cons(xlenter(buf),NIL));
	}
	else {
	    rplacd(last,cons(slotname,NIL));
	}
#endif /* PACKAGES */
	last = cdr(last);
	list = cdr(list);

	/* add the value expression  -- QUOTED (TAA MOD) */
	rplacd(last,cons(NIL,NIL));
	last = cdr(last);
	rplaca(last, (slotname = cons(s_quote,NIL)));
	rplacd(slotname, cons(car(list), NIL));
	list = cdr(list);
    }

    /* make sure all of the initializers were used */
    if (consp(list))
	xlerror("bad structure initialization list",list);

    /* invoke the creation function */
    val = xleval(expr);

    /* restore the stack */
    xlpop();

    /* return the new structure */
    return (val);
}

/* xlprstruct - print a structure (used by printer) */
VOID xlprstruct P4C(LVAL, fptr, LVAL, vptr, FIXTYPE, plevel, int, flag)
{
    LVAL next;
    int i,n;
    FRAMEP newfp;

    {
      LVAL type = getelement(vptr,0);
      next = xlgetprop(type, s_prntfunc);
      while (null(next) && ! null(type)) {
	type = xlgetprop(type,s_strinclude);
	next = xlgetprop(type, s_prntfunc);
      }
    }
    if (!null(next)) { /* Ken Whedbee addition */
        newfp = xlsp;
        pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
        pusharg(symbolp(next) ? xlgetfunction(next) : next);
        pusharg(cvfixnum((FIXTYPE) 3));
        pusharg(vptr);
        pusharg(fptr);
        pusharg(cvfixnum(plevel)); 
        xlfp = newfp;
        xlapply(3);
    }
    else {
        xlputstr(fptr,"#S(");   /* TAA MOD */
        xlprint(fptr,getelement(vptr,0),flag);
        next = xlgetprop(getelement(vptr,0), s_sslots);
        for (i = 1, n = getsize(vptr) - 1; i <= n && consp(next); ++i) {
            if (consp(car(next))) { /* should always succeed */
                xlputc(fptr,' ');   /* Alternate, could print " :" */
                xlprint(fptr,car(car(next)),flag);
                xlputc(fptr,' ');
                xlprint(fptr,getelement(vptr,i),flag);
            }
            next = cdr(next);
        }
        xlputc(fptr,')');
    }
}

/* addslot - make the slot access functions */
LOCAL VOID addslot P5C(LVAL, slotname, LVAL, defexpr, int, slotn, LVAL *, pargs, LVAL *, pbody)
{
    LVAL sym,args,body,tmp;

    /* protect some pointers */
    xlstkcheck(4);
    xlsave(sym);
    xlsave(args);
    xlsave(body);
    xlsave(tmp);

    /* construct the update function name */
    sprintf(buf,"%s%s",prefix,getstring(getpname(slotname)));
#ifdef PACKAGES
    sym = xlintern(buf, getvalue(s_package));
#else
    sym = xlenter(buf);
#endif /* PACKAGES */

    /* make the access function */
    args = cons(s_s,NIL);
    body = cons(cvfixnum((FIXTYPE)slotn),NIL);
    body = cons(s_s,body);
    body = cons(s_strref,body);
    body = cons(body,NIL);
    setfunction(sym,
		xlclose(sym,s_lambda,args,body,NIL,NIL));

    /* make the update function */
    args = cons(s_x,NIL);
    args = cons(s_s,args);
    body = cons(s_x,NIL);
    body = cons(cvfixnum((FIXTYPE)slotn),body);
    body = cons(s_s,body);
    body = cons(s_strset,body);
    body = cons(body,NIL);
    xlputprop(sym,
	      xlclose(NIL,s_lambda,args,body,NIL,NIL),
	      s_setf);

    /* add the slotname to the make-xxx keyword list */
    tmp = cons(defexpr,NIL);
    tmp = cons(slotname,tmp);
    tmp = cons(tmp,NIL);
    if ((args = *pargs) == NIL)
	*pargs = tmp;
    else {
	while (consp(cdr(args)))
	    args = cdr(args);
	rplacd(args,tmp);
    }

    /* add the slotname to the %make-xxx argument list */
    tmp = cons(slotname,NIL);
    if ((body = *pbody) == NIL)
	*pbody = tmp;
    else {
	while (consp(cdr(body)))
	    body = cdr(body);
	rplacd(body,tmp);
    }

    /* restore the stack */
    xlpopn(4);
}

/* updateslot - update a slot definition */
LOCAL VOID updateslot P3C(LVAL, args, LVAL, slotname, LVAL, defexpr)
{
    LVAL tmp;
    for (; consp(args); args = cdr(args))
	if (slotname == car(car(args))) {
	    if (defexpr != NIL) {
		xlsave1(tmp);
		tmp = newclosure(NIL,s_lambda,xlenv,xlfenv);
		setbody(tmp,cons(defexpr,NIL));
		tmp = cons(tmp,NIL);
		defexpr = tmp;
		xlpop();
	    }
	    rplaca(cdr(car(args)),defexpr);
	    break;
	}
    if (args == NIL)
	xlerror("unknown slot name",slotname);
}
