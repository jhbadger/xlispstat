/* xlsys.c - xlisp builtin system functions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* $putpatch.c$: "MODULE_XLSYS_C_GLOBALS" */

/* Function prototypes */
LOCAL int xltypep P2H(LVAL, LVAL);
LOCAL int is_member P2H(LVAL, LVAL);
LOCAL LVAL vectify P2H(LVAL, LVAL);
LOCAL LVAL listify P1H(LVAL);

/* xload - read and evaluate expressions from a file */
LVAL xload(V)
{
    char *name;
    int vflag,pflag,nflag;
    LVAL oldenv,oldfenv;    /* TAA MOD-- code sections using these variables
                               forces global environment on LOAD
                               Change based on Luke Tierney's XLISP-STAT */
    LVAL arg;

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(oldenv);
    xlprotect(oldfenv);

    /* establish global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;

    /* get the file name */
    name = getstring(xlgetfname());

    /* get the :verbose flag */ /* TAA MOD to simplify */
    vflag = xlgetkeyarg(k_verbose,&arg) ? (arg != NIL) : TRUE;

    /* get the :print flag */ /* TAA MOD to simplify */
    pflag = xlgetkeyarg(k_print,&arg) ? (arg != NIL) : FALSE;

    /* get the :if-does-not-exist flag */
    nflag = xlgetkeyarg(k_nexist,&arg) ? (arg != NIL) : TRUE;

    xllastkey();

    /* load the file, check for success */
    arg = xlload(name,vflag,pflag) ? s_true : NIL;
    if (nflag && null(arg))
      xlerror("can't load file",cvstring(name));

    /* restore environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(2);

    /* return success flag */
    return arg;
}

/* xtranscript - open or close a transcript file */
LVAL xtranscript(V)
{
    char *name;

    /* get the transcript file name */
    name = (moreargs() ? getstring(xlgetfname()) : NULL);
    xllastarg();

    /* close the current transcript */
    if (tfp != CLOSED) OSCLOSE(tfp);

    /* open the new transcript */
    tfp = (name != NULL ? OSAOPEN(name,CREATE_WR) : CLOSED);

    /* return T if a transcript is open, NIL otherwise */
    return (tfp != CLOSED ? s_true : NIL);
}

/* xenablintr - turn interrupts on or off */
LVAL xenablintr(V)
{
  if (moreargs() && null(xlgetarg())) {
    disable_interrupts();
    return NIL;
  }
  else {
    enable_interrupts();
    return s_true;
  }
}

/* xtype - return type of a thing */
LVAL xtype(V)
{
    LVAL arg;

    arg = xlgetarg();
    xllastarg();    /* TAA MOD -- this was missing */

    switch (ntype(arg)) {
    case SUBR:		return (a_subr);
    case FSUBR:		return (a_fsubr);
    case CONS:		return (a_cons);
    case SYMBOL:	return (a_symbol);
    case FIXNUM:	return (a_fixnum);
    case FLONUM:	return (a_flonum);
    case STRING:	return (a_string);
#ifdef BIGNUMS
    case RATIO:		return (a_ratio);
    case BIGNUM:	return (a_bignum);
#endif
    case OBJECT:	return (a_object);
    case STREAM:	return (a_stream);
    case VECTOR:	return (a_vector);
    case CLOSURE:	return (a_closure);
    case CHAR:		return (a_char);
    case USTREAM:	return (a_ustream);
    case STRUCT:	return (getelement(arg,0));
    case COMPLEX:	return (a_complex);
    case RNDSTATE:	return (a_rndstate);
    case DARRAY:	return (a_array);       /* L. Tierney */
#ifdef XLISP_STAT
    case ADATA:		return (a_adata);
#endif /* XLISP_STAT */
    case NATPTR:	return (a_ptr);
    case WEAKBOX:	return (a_weakbox);
    case TVEC:		return (a_tvec);
#ifdef BYTECODE
    case BCCLOSURE:	return (a_bcclosure);
    case CPSNODE:	return (a_cpsnode);
    case BCODE:		return (a_bcode);
#endif /* BYTECODE */
#ifdef PACKAGES
    case PACKAGE:	return (a_package);
#endif /* PACKAGES */
    /* $putpatch.c$: "MODULE_XLSYS_C_XTYPE" */
    default:		xlfail("bad node type");
                        return (NIL); /* eliminate warning message */
    }
}

int xlcvttype P1C(LVAL, arg)  /* find type of argument and return it */
{
  /*sorted into roughly most-likely-used-first order*/
  if (arg == a_cons)      return CONS;
  if (arg == a_list)      return CONS;    /* Synonym here */
  if (arg == a_vector)    return VECTOR;
  if (arg == a_string)    return STRING;
  if (arg == a_symbol)    return SYMBOL;
  if (arg == a_subr)      return SUBR;
  if (arg == a_fsubr)     return FSUBR;
  if (arg == a_fixnum)    return FIXNUM;
  if (arg == a_flonum)    return FLONUM;
#ifdef BIGNUMS
  if (arg == a_ratio)     return RATIO;
  if (arg == a_bignum)    return BIGNUM;
#endif
  if (arg == a_object)    return OBJECT;
  if (arg == a_stream)    return STREAM;
  if (arg == a_closure)   return CLOSURE;
  if (arg == a_char)      return CHAR;
  if (arg == a_ustream)   return USTREAM;
  if (arg == a_struct)    return STRUCT;
  if (arg == a_complex)   return COMPLEX;
  if (arg == a_rndstate)  return RNDSTATE;
  if (arg == a_array)     return DARRAY;
#ifdef XLISP_STAT
  if (arg == a_adata)     return ADATA;
#endif /* XLISP_STAT */
  if (arg == a_ptr)       return NATPTR;
  if (arg == a_weakbox)   return WEAKBOX;
  if (arg == a_tvec)      return TVEC;
#ifdef BYTECODE
  if (arg == a_bcclosure) return BCCLOSURE;
  if (arg == a_cpsnode)   return CPSNODE;
  if (arg == a_bcode)     return BCODE;
#endif /* BYTECODE */
#ifdef PACKAGES
  if (arg == a_package)   return PACKAGE;
#endif /* PACKAGES */
  if (arg == s_true)      return -1;  /* Fix for coerce */
  if (consp(arg))         return(xlcvttype(car(arg)));
  return 0;
}

/* xlparsetype - check for a type defined by deftype */
LVAL xlparsetype P1C(LVAL, typ)
{
  LVAL temp;

  while (symbolp(temp = consp(typ) ? car(typ) : typ)
	 && !null(temp = xlgetprop(temp, s_typespec)))
    typ = xlapply(pushargs(temp, consp(typ) ? cdr(typ) : NIL));
  return(typ);
}
  
/* typep -- check type of thing */
LOCAL int xltypep P2C(LVAL, arg, LVAL, typ)
{
  typ = xlparsetype(typ);

  if (symbolp(typ)) {

    /* everything is type T */
    if (typ == s_true) return TRUE;

    /* only NIL is NULL */
    if (typ == a_null) return null(arg);

    /* only atoms are ATOM */
    if (typ == a_atom) return atom(arg);

    /* two types of streams */
    if (typ == a_anystream)
      return (streamp(arg) || ustreamp(arg));

    /* many ways to be a function */
    if (typ == s_function)
      return (subrp(arg) || closurep(arg) || symbolp(arg) ||
	      (consp(arg) && car(arg) == s_lambda));

    /* NIL is type LIST or SYMBOL */
    if (null(arg)) return (typ==a_list || typ==a_symbol);

    /* Structures are type STRUCT or the structure type */
    if (ntype(arg) == STRUCT) {
      LVAL val = getelement(arg, 0);

      if (typ == a_struct)
#ifdef HASHFCNS
	return(val != a_hashtable);
#else
        return(TRUE);
#endif
      for (; ! null(val); val = xlgetprop(val,s_strinclude))
	if (val == typ) return(TRUE);
      return(FALSE);
    }

    /* If typename is NUMBER, then arg can be any numeric type */
    if (typ == a_number)
      return (numberp(arg));

#ifdef BIGNUMS
    /* if typename is RATIONAL then arg can be integer or ratio */
    if (typ == a_rational)
      return (rationalp(arg));
#endif

    /* if typename is INTEGER, then arg can be fixnum (possibly) bignum */
    if (typ == a_integer)
      return (integerp(arg));

    /* if typename is REAL, then arg can be any non-complex number */
    if (typ == a_real)
      return (realp(arg));

    /* vectors, typed vectors and strings are of type VECTOR */
    if (typ == a_vector)
      return (vectorp(arg) || stringp(arg) || tvecp(arg));

    /* arrays, vectors, typed vectors, and strings are of type ARRAY */
    if (typ == a_array)
      return (darrayp(arg) || vectorp(arg) || stringp(arg) || tvecp(arg));

    /* otherwise the typename must be the same as the type of the
       object (as would be returned by TYPE-OF) */

    return (ntype(arg) == xlcvttype(typ));
  }

  /* type specifier is a list */
  if (consp(typ)) {
    LVAL fn = car(typ);
    LVAL lst = cdr(typ);

    if (fn == s_not) {  /* (not spec) */
      if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
      return !xltypep(arg, car(lst));
    }
    if (fn == s_satisfies) { /* (satisfies predicatefn) */
      if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
#ifdef KEYARG
      return dotest1(arg, car(lst), NIL);
#else
      return dotest1(arg, car(lst));
#endif
    }
    if (fn == a_object) { /* (object class) */
      if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
      lst = car(lst);
      return (objectp(arg) &&
	      (symbolp(lst) ? getvalue(lst) : lst) == getclass(arg));
    }
    if (fn == s_and) {  /* (and {spec}) */
      for (; consp(lst); lst = cdr(lst))
	if (!xltypep(arg,car(lst))) return FALSE;
      return TRUE;
    }
    if (fn == s_or) {   /* (or {spec}) */
      for (; consp(lst); lst = cdr(lst))
	if (xltypep(arg,car(lst))) return TRUE;
      return FALSE;
    }
    if (fn == s_member) {   /* (member {args}) */
      for (; consp(lst); lst = cdr(lst))
	if (eql(car(lst),arg)) return TRUE;
      return FALSE;
    }
    if (fn == a_integer || fn == a_fixnum) {  /* (integer * *) */
      LVAL low, high;

      if (! consp(lst) || ! consp(cdr(lst))) goto bad_type;

      low = car(lst);
      high = car(cdr(lst));

      if (! integerp(arg)) return FALSE;
      if (low != s_1star) {
	if (integerp(low)) {
	  if (! null(xlcallsubr2(xgtr,low,arg))) return FALSE;
	}
	else if (consp(low) && integerp(car(low))) {
	  if (! null(xlcallsubr2(xgeq,car(low),arg))) return FALSE;
	}
	else goto bad_type;
      }
      if (high != s_1star) {
	if (integerp(high)) {
	  if (! null(xlcallsubr2(xlss,high,arg))) return FALSE;
	}
	else if (consp(high) && integerp(car(high))) {
	  if (! null(xlcallsubr2(xleq,car(high),arg))) return FALSE;
	}
	else goto bad_type;
      }
      return TRUE;
    }
    if (fn == a_flonum) {  /* (float * *) */
      LVAL low, high;

      if (! consp(lst) || ! consp(cdr(lst))) goto bad_type;

      low = car(lst);
      high = car(cdr(lst));

      if (! floatp(arg)) return FALSE;
      if (low != s_1star) {
	if (realp(low)) {
	  if (makefloat(low) > getflonum(arg)) return FALSE;
	}
	else if (consp(low) && realp(car(low))) {
	  if (makefloat(car(low)) >= getflonum(arg)) return FALSE;
	}
	else goto bad_type;
      }
      if (high != s_1star) {
	if (realp(high)) {
	  if (makefloat(high) < getflonum(arg)) return FALSE;
	}
	else if (consp(high) && realp(car(high))) {
	  if (makefloat(car(high)) <= getflonum(arg)) return FALSE;
	}
	else goto bad_type;
      }
      return TRUE;
    }
    if (fn == a_complex) {  /* (complex type) */
      if (! consp(lst)) goto bad_type;
      if (complexp(arg))
	return (car(lst) == s_1star || xltypep(getreal(arg),car(lst)));
      else
	return FALSE;
    }
    if (fn == a_vector) {   /* (vector type ...) */
      if (! consp(lst)) goto bad_type;
      switch (ntype(arg)) {
      case VECTOR:
      case STRING:
      case TVEC:
	if (consp(cdr(lst))) {
	  if (consp(cdr(cdr(lst)))) goto bad_type;
	  if (fixp(car(cdr(lst))) &&
	      getfixnum(car(cdr(lst))) != gettvecsize(arg))
	    return FALSE;
	}
	return(car(lst) == s_1star || equal(car(lst), gettvecetype(arg)));
      default:     return FALSE;
      }
    }
    if (fn == a_string) {   /* (string size) */
      if (! consp(lst)) goto bad_type;
      if (stringp(arg))
	return(car(lst) == s_1star ||
	       getfixnum(car(lst)) != getslength(arg));
      else return FALSE;
    }
    if (fn == a_array) {   /* (array type ...) */
      LVAL data, dim, next;
      int i, n;

      if (! consp(lst)) goto bad_type;
      data = darrayp(arg) ? getdarraydata(arg) : arg;
      switch (ntype(data)) {
      case VECTOR:
      case STRING:
      case TVEC:
	if (consp(cdr(lst))) {
	  if (consp(cdr(cdr(lst)))) goto bad_type;
	  next = listp(car(cdr(lst))) ? car(cdr(lst)) : cdr(lst);
	  dim = getdarraydim(arg);
	  n = darrayp(arg) ? getdarrayrank(arg) : 1;
	  if (llength(next) != n) return(FALSE);
	  for (i = 0; i < n; i++, next = cdr(next)) {
	    if (fixp(car(next)) &&
		getfixnum(car(next)) != getfixnum(getelement(dim, i)))
	      return FALSE;
	  }
	}
	return(car(lst) == s_1star || equal(car(lst), gettvecetype(data)));
      default:     return FALSE;
      }
    }
  }
 bad_type:
  xlerror("bad type specifier", typ);
  return FALSE; /* keep compilers happy */
}

LVAL xtypep(V)
{
    LVAL arg, typ;

    arg = xlgetarg();
    typ = xlgetarg();
    xllastarg();

    return (xltypep(arg, typ) ? s_true : NIL);
}

LOCAL LVAL listify P1C(LVAL, arg) /* arg must be vector or string */
{
  LVAL val;
  unsigned n;

  xlsave1(val);
  n = gettvecsize(arg);
  val = mklist(n, NIL);
  xlreplace(val, arg, 0, n, 0, n);
  xlpop();
  return (val);
}

LOCAL LVAL vectify P2C(LVAL, arg, LVAL, etype)
{
  LVAL val;
  unsigned n;

  n = listp(arg) ? llength(arg) : gettvecsize(arg);
  xlsave1(val);
  val = mktvec(n, etype);
  xlreplace(val, arg, 0, n, 0, n);
  xlpop();
  return val;
}

/* coerce function */
LVAL xcoerce(V)
{
  LVAL type, arg, temp;
  int newtype,oldtype;

  arg = xlgetarg();
  type = xlgetarg();
  xllastarg();

  type = xlparsetype(type);
  if (xltypep(arg, type))
    return arg;

  if ((newtype = xlcvttype(type)) == 0) goto badconvert;

  oldtype = (arg==NIL? CONS: ntype(arg)); /* TAA fix */

  if (newtype == -1 || (! consp(type) && oldtype == newtype))
    return (arg);  /* easy case! */

  switch (newtype) {
  case CONS:
    return (listify(arg));
    break;
  case STRING:
  case VECTOR:
  case DARRAY:
    {
      LVAL etype;
      if (consp(type)) {
	if (!consp(cdr(type))) goto badconvert;
	etype = car(cdr(type));
      }
      else if (newtype == STRING) etype = a_char;
      else etype = s_true;
      if (darrayp(arg)) {
	if (newtype != DARRAY) goto badconvert;
	return newdarray(getdarraydim(arg),
			 vectify(getdarraydata(arg), etype));
      }
      else
	return (vectify(arg, etype));
    }
    break;
  case CHAR:
    if (oldtype == FIXNUM) return cvchar((int)getfixnum(arg));
    else if ((oldtype == STRING) && (getslength(arg) == 1))
      return cvchar(getstringch(arg,0));
    else if (oldtype == SYMBOL) {
      temp = getpname(arg);
      if (getslength(temp) == 1) return cvchar(getstringch(temp,0));
    }
    break;
  case FLONUM:
    if (oldtype == FIXNUM)
      return (cvflonum((FLOTYPE) getfixnum(arg)));
#ifdef BIGNUMS
    else if (oldtype == RATIO)
      return cvflonum(cvtratioflonum(arg));
    else if (oldtype == BIGNUM) 
      return cvflonum(cvtbigflonum(arg));
#endif
    break;
  case COMPLEX:
    if (consp(type)) {
      LVAL ctype;
      if (!consp(cdr(type))) goto badconvert;
      ctype = car(cdr(type));
      if (ctype == a_fixnum) {
	if (fixp(arg) || (complexp(arg) && fixp(getreal(arg))))
	  return arg;
      }
      else if (ctype == a_integer) {
	if (integerp(arg) || (complexp(arg) && integerp(getreal(arg))))
          return arg;
      }
#ifdef BIGNUMS
      else if (ctype == a_rational) {
	if (rationalp(arg) || (complexp(arg) && rationalp(getreal(arg))))
          return arg;
      }
#endif /* BIGNUMS */
      else if (ctype == a_flonum) {
	if (complexp(arg)) {
	  if (floatp(getreal(arg)))
	    return arg;
	  else
	    return(newdcomplex(makefloat(getreal(arg)),
			       makefloat(getimag(arg))));
	}
	else
	  return(newdcomplex(makefloat(arg), (FLOTYPE) 0.0));
      }
      else if (ctype == s_1star) {
	if (rationalp(arg) || complexp(arg))
	  return (arg);   /* nothing happens */
	else if (floatp(arg))
	  return newdcomplex(getflonum(arg), (FLOTYPE) 0.0);
      }
    }
    else if (rationalp(arg) || complexp(arg))
      return (arg);   /* nothing happens */
    else if (floatp(arg))
      return newdcomplex(getflonum(arg), (FLOTYPE) 0.0);
    break;
  }

 badconvert:
  xlerror("illegal coersion",arg);
  return (NIL);   /* avoid compiler warnings */
}

/* xbaktrace - print the trace back stack */
LVAL xbaktrace(V)
{
    LVAL num;
    int n;

    if (moreargs()) {
	num = xlgafixnum();
	n = (int)getfixnum(num);
    }
    else
	n = -1;
    xllastarg();
    xlbaktrace(n);
    return (NIL);
}

/* xexit - get out of xlisp */
LVAL xexit(V)
{
    xllastarg();
    wrapup();
    return (NIL); /* never returns */
}

/* xpeek - peek at a location in memory */
LVAL xpeek(V)
{
    LVAL num;
    OFFTYPE *adr;   /* TAA MOD so that data fetched is sizeof(LVAL *) */

    /* get the address */
    num = xlgafixnum(); adr = (OFFTYPE *)getfixnum(num);
    xllastarg();

    /* return the value at that address */
    return (cvfixnum((FIXTYPE)*adr));
}

/* xpoke - poke a value into memory */
LVAL xpoke(V)
{
    LVAL val;
    OFFTYPE *adr;   /* TAA MOD so that data fetched is sizeof(LVAL *) */

    /* get the address and the new value */
    val = xlgafixnum(); adr = (OFFTYPE *)getfixnum(val);
    val = xlgafixnum();
    xllastarg();

    /* store the new value */
    *adr = (OFFTYPE)getfixnum(val);

    /* return the new value */
    return (val);
}

/* xaddrs - get the address of an XLISP node */
LVAL xaddrs(V)
{
    LVAL val;

    /* get the node */
    val = xlgetarg();
    xllastarg();

    /* changed to use native pointer -- L. Tierney */
    return newnatptr(val, val);
}

/* xnpaddr - get the address of a native pointer */
LVAL xnpaddr(V)
{
  LVAL p = xlganatptr();
  xllastarg();
#ifdef BIGNUMS
  if ((unsigned long) getnpaddr(p) > MAXFIX)
    return cvtulongbignum((unsigned long) getnpaddr(p), 0);
#endif /* BIGNUMS */
  return cvfixnum((FIXTYPE) getnpaddr(p));
}

/* xnpprot - get the protected value of a native pointer */
LVAL xnpprot(V)
{
  LVAL p = xlganatptr();
  xllastarg();
  return getnpprot(p);
}

/* xnpincr - increment native pointer */
LVAL xnpincr(V)
{
  LVAL p = xlganatptr();
  long count = getfixnum(xlgafixnum());
  long size = moreargs() ? getfixnum(xlgafixnum()) : 1;
  xllastarg();
  return newnatptr(((char *) p) + count * size, getnpprot(p));
}


/***********************************************************************/
/**                                                                   **/
/**                  Features Maintenance Functions                   **/
/**                                                                   **/
/***********************************************************************/

LOCAL int is_member P2C(LVAL, x, LVAL, list)
{
  for (; consp(list); list = cdr(list))
    if (x == car(list)) return TRUE;
  return FALSE;
}

int checkfeatures P2C(LVAL, arg, int, which)
{
  int has_feature = FALSE;
  LVAL features = getvalue(s_features);
  
  if (consp(arg)) {
    if (car(arg) == k_and)
      for (has_feature = TRUE, arg = cdr(arg);
	   consp(arg) && has_feature;
	   arg = cdr(arg)) {
	has_feature = has_feature && checkfeatures(car(arg), '+');
      }
    else if (car(arg) == k_or)
      for (has_feature = FALSE, arg = cdr(arg);
	   consp(arg) && ! has_feature;
	   arg = cdr(arg)) {
	has_feature = has_feature || checkfeatures(car(arg), '+');
      }
    else if (car(arg) == k_not && consp(cdr(arg)))
      has_feature = ! checkfeatures(car(cdr(arg)), '+');
    else xlerror("bad feature", arg);
  }
  else has_feature = is_member(arg, features);

  if (which == '-') has_feature = ! has_feature;
  return(has_feature);
}
