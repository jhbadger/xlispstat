/* xlobj - xlisp object functions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* forward declarations */
LOCAL LVAL entermsg _((LVAL cls, LVAL msg));
LOCAL LVAL sendmsg _((LVAL obj, LVAL cls, LVAL sym));
LOCAL int  getivcnt _((LVAL cls, int ivar));
LOCAL int  listlength _((LVAL list));
LOCAL VOID xputobj _((LVAL fptr, LVAL val));
LOCAL LVAL xlclass _((char *name, int vcnt));
LOCAL VOID xladdivar _((LVAL cls, char *var));
LOCAL VOID xladdmsg _((LVAL cls, char *msg, int offset));
LOCAL int getivcnt _((LVAL cls, int ivar));
LOCAL int listlength _((LVAL list));

/* $putpatch.c$: "MODULE_XLOBJ_C_GLOBALS" */

/* routine to print an object for PRINx */
LOCAL VOID xputobj(fptr,val)
     LVAL fptr; LVAL val;
{
    LVAL temp;
    if (!null(getvalue(s_printreadably)))
      xlfail("can't readably print an unreadable object");
    if ((temp = getclass(val)) == cls_class) { /* this is a class */
        if (null(temp = getivar(val,PNAME)) || (ntype(temp) != STRING) ) { 
            /* but nameless */
            xlputstr(fptr,"#<class ???: #");
        }
        else {
            sprintf(buf,"#<class %s: #",getstring(temp));
            xlputstr(fptr,buf);
        }
    }
    else { /* not a class */
        if (null(temp = getivar(temp,PNAME)) || (ntype(temp) != STRING) ) {
            /* but nameless */
            xlputstr(fptr,"#<a ??? object: #");
        }
        else {
            sprintf(buf,"#<a %s: #",getstring(temp));
            xlputstr(fptr,buf);
        }
    }
    sprintf(buf,AFMT, (OFFTYPE) val);
    xlputstr(fptr,buf);
    xlputc(fptr,'>');
}
                

/* xsend - send a message to an object */
LVAL xsend()
{
    LVAL obj;
    obj = xlgaobject();
    return (sendmsg(obj,getclass(obj),xlgasymbol()));
}

/* xsendsuper - send a message to the superclass of an object */
LVAL xsendsuper()
{
    LVAL env,p;
    for (env = xlenv; consp(env); env = cdr(env))
	if ((!null(p = car(env))) && objectp(car(p)))
	    return (sendmsg(car(p),
			    getivar(cdr(p),SUPERCLASS),
			    xlgasymbol()));
    xlfail("not in a method");
    return (NIL);   /* fake out compiler warning */
}

/* xlclass - define a class */
LOCAL LVAL xlclass(name,vcnt)
     char *name; int vcnt;
{
    LVAL sym,cls;

    /* create the class */
    sym = xlenter(name);
    cls = newobject(cls_class,CLASSSIZE);
    defconstant(sym,cls);   /* TAA MOD -- was setvalue */

    /* set the instance variable counts */
    setivar(cls,IVARCNT,cvfixnum((FIXTYPE)vcnt));
    setivar(cls,IVARTOTAL,cvfixnum((FIXTYPE)vcnt));

    /* set the class name   TAA Mod */
    setivar(cls,PNAME,cvstring(name));

    /* set the superclass to 'Object' */
    setivar(cls,SUPERCLASS,cls_object);

    /* return the new class */
    return (cls);
}

/* xladdivar - enter an instance variable */
LOCAL VOID xladdivar(cls,var)
     LVAL cls; char *var;
{
    setivar(cls,IVARS,cons(xlenter(var),getivar(cls,IVARS)));
}

/* xladdmsg - add a message to a class */
LOCAL VOID xladdmsg(cls,msg,offset)
     LVAL cls; char *msg; int offset;
{
    LVAL mptr;

    /* enter the message selector */
    mptr = entermsg(cls,xlenter(msg));

    /* store the method for this message */
    rplacd(mptr,cvsubr(funtab[offset].fd_subr,funtab[offset].fd_type,offset));
}

/* xlobgetvalue - get the value of an instance variable */
int xlobgetvalue(pair,sym,pval)
  LVAL pair,sym,*pval;
{
    LVAL cls,names;
    int ivtotal,n;

    /* find the instance or class variable */
    for (cls = cdr(pair); objectp(cls); cls = getivar(cls,SUPERCLASS)) {

	/* check the instance variables */
	names = getivar(cls,IVARS);
	ivtotal = getivcnt(cls,IVARTOTAL);
	for (n = ivtotal - getivcnt(cls,IVARCNT); n < ivtotal; ++n) {
	    if (car(names) == sym) {
		*pval = getivar(car(pair),n);
		return (TRUE);
	    }
	    names = cdr(names);
	}

	/* check the class variables */
	names = getivar(cls,CVARS);
	for (n = 0; consp(names); ++n) {
	    if (car(names) == sym) {
		*pval = getelement(getivar(cls,CVALS),n);
		return (TRUE);
	    }
	    names = cdr(names);
	}
    }

    /* variable not found */
    return (FALSE);
}

/* xlobsetvalue - set the value of an instance variable */
int xlobsetvalue(pair,sym,val)
  LVAL pair,sym,val;
{
    LVAL cls,names;
    int ivtotal,n;

    /* find the instance or class variable */
    for (cls = cdr(pair); objectp(cls); cls = getivar(cls,SUPERCLASS)) {

	/* check the instance variables */
	names = getivar(cls,IVARS);
	ivtotal = getivcnt(cls,IVARTOTAL);
	for (n = ivtotal - getivcnt(cls,IVARCNT); n < ivtotal; ++n) {
	    if (car(names) == sym) {
		setivar(car(pair),n,val);
		return (TRUE);
	    }
	    names = cdr(names);
	}

	/* check the class variables */
	names = getivar(cls,CVARS);
	for (n = 0; consp(names); ++n) {
	    if (car(names) == sym) {
		setelement(getivar(cls,CVALS),n,val);
		return (TRUE);
	    }
	    names = cdr(names);
	}
    }

    /* variable not found */
    return (FALSE);
}

/* obisnew - default 'isnew' method */
LVAL obisnew()
{
    LVAL self;
    self = xlgaobject();
    xllastarg();
    return (self);
}

/* obclass - get the class of an object */
LVAL obclass()
{
    LVAL self;
    self = xlgaobject();
    xllastarg();
    return (getclass(self));
}

/* obshow - show the instance variables of an object */
LVAL obshow()
{
    LVAL self,fptr,cls,names;
    int ivtotal,n;

    /* get self and the file pointer */
    self = xlgaobject();
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* get the object's class */
    cls = getclass(self);

    /* print the object and class */
    xlputstr(fptr,"Object is ");
    xlprint(fptr,self,TRUE);
    xlputstr(fptr,", Class is ");
    xlprint(fptr,cls,TRUE);
    xlterpri(fptr);

    /* print the object's instance variables */
    for (; !null(cls); cls = getivar(cls,SUPERCLASS)) {
	names = getivar(cls,IVARS);
	ivtotal = getivcnt(cls,IVARTOTAL);
	for (n = ivtotal - getivcnt(cls,IVARCNT); n < ivtotal; ++n) {
	    xlputstr(fptr,"  ");
	    xlprint(fptr,car(names),TRUE);
	    xlputstr(fptr," = ");
	    xlprint(fptr,getivar(self,n),TRUE);
	    xlterpri(fptr);
	    names = cdr(names);
	}
    }

    /* return the object */
    return (self);
}

/* clnew - create a new object instance */
LVAL clnew()
{
    LVAL self;
    self = xlgaobject();
    /* $putpatch.c$: "MODULE_XLOBJ_C_CLNEW" */
    return (newobject(self,getivcnt(self,IVARTOTAL)));
}

/* clisnew - initialize a new class */
LVAL clisnew()
{
    LVAL self,ivars,cvars,super;
    int n;

    /* get self, the ivars, cvars and superclass */
    self = xlgaobject();
    ivars = xlgalist();
    cvars = (moreargs() ? xlgalist() : NIL);
    super = (moreargs() ? xlgaobject() : cls_object);
    xllastarg();

    /* store the instance and class variable lists and the superclass */
    setivar(self,IVARS,ivars);
    setivar(self,CVARS,cvars);
    setivar(self,CVALS,(!null(cvars) ? newvector(listlength(cvars)) : NIL));
    setivar(self,SUPERCLASS,super);

    /* compute the instance variable count */
    n = listlength(ivars);
    setivar(self,IVARCNT,cvfixnum((FIXTYPE)n));
    n += getivcnt(super,IVARTOTAL);
    setivar(self,IVARTOTAL,cvfixnum((FIXTYPE)n));

    /* return the new class object */
    return (self);
}

/* clanswer - define a method for answering a message */
LVAL clanswer()
{
    LVAL self,msg,fargs,code,mptr;

    /* message symbol, formal argument list and code */
    self = xlgaobject();
    msg = xlgasymbol();
    fargs = xlgalist();
    code = xlgalist();
    xllastarg();

    /* make a new message list entry */
    mptr = entermsg(self,msg);

    /* setup the message node */
    xlprot1(fargs);
    fargs = cons(s_self,fargs); /* add 'self' as the first argument */
        /* The following TAA MOD is by Neils Mayer, at HP */
        /* it sets the lexical environment to be correct (non-global) */
/*    rplacd(mptr,xlclose(msg,s_lambda,fargs,code,NIL,NIL)); */
    rplacd(mptr,xlclose(msg,s_lambda,fargs,code,xlenv,xlfenv));
    xlpop();

    /* return the object */
    return (self);
}

/* clmethod - define a method for answering a message */
LVAL clmethod()
{
  LVAL self,msg,method,mptr,lptr;
  int set;

  /* message symbol, formal argument list and code */
  self = xlgaobject();
  msg = xlgasymbol();
  if (moreargs()) {
    set = TRUE;
    method = xlgetarg();
  }
  else {
    set = FALSE;
    method = NIL;
  }
  xllastarg();

  if (set) {
    mptr = entermsg(self,msg);
    rplacd(mptr, method);
    return(method);
  }
  else {
    for (lptr = getivar(self,MESSAGES); consp(lptr); lptr = cdr(lptr))
      if (car(mptr = car(lptr)) == msg)
	return cdr(mptr);
    return(NIL);
  }
}    

/* entermsg - add a message to a class */
LOCAL LVAL entermsg(cls,msg)
  LVAL cls,msg;
{
    LVAL lptr,mptr;

    /* lookup the message */
    for (lptr = getivar(cls,MESSAGES); consp(lptr); lptr = cdr(lptr))
	if (car(mptr = car(lptr)) == msg)
	    return (mptr);

    /* allocate a new message entry if one wasn't found */
    xlsave1(mptr);
    mptr = consa(msg);
    setivar(cls,MESSAGES,cons(mptr,getivar(cls,MESSAGES)));
    xlpop();

    /* return the symbol node */
    return (mptr);
}

/* sendmsg - send a message to an object */
LOCAL LVAL sendmsg(obj,cls,sym)
  LVAL obj,cls,sym;
{
    LVAL msg,msgcls,method,val,p;

    /* look for the message in the class or superclasses */
    for (msgcls = cls; !null(msgcls); ) {

	/* lookup the message in this class */
	for (p = getivar(msgcls,MESSAGES); consp(p); p = cdr(p))
	    if ((!null(msg = car(p))) && car(msg) == sym)
		goto send_message;

	/* look in class's superclass */
	msgcls = getivar(msgcls,SUPERCLASS);
    }

    /* message not found */
    xlerror("no method for this message",sym);

send_message:

    /* insert the value for 'self' (overwrites message selector) */
    *--xlargv = obj;
    ++xlargc;
    
    /* invoke the method */
    if (null(method = cdr(msg)))
	xlerror("bad method",method);
    switch (ntype(method)) {
    case SUBR:
	val = (*getsubr(method))();
#ifdef MULVALS
	if (! mulvalp(method)) {
	  xlnumresults = 1;
	  xlresults[0] = val;
	}
#endif /* MULVALS */
	break;
#ifdef BYTECODE
    case BCCLOSURE:
	if (getbcctype(method) != s_lambda)
	  xlerror("bad method",method);
	{
	  LVAL olddenv = xldenv;
	  LVAL oldenv = xlenv;
	  xldbind(s_self, cons(obj,msgcls));
	  xlenv = consa(getvalue(s_self));
	  val = BC_evfun(method,xlargc,xlargv);
	  xlunbind(olddenv);
	  xlenv = oldenv;
	}
	break;
#endif /* BYTECODE */
    case CLOSURE:
	if (gettype(method) != s_lambda)
	    xlerror("bad method",method);
	val = evmethod(obj,msgcls,msg);
	break;
    default:
	xlerror("bad method",method);
    }

    /* after creating an object, send it the ":isnew" message */
    if (car(msg) == k_new && !null(val)) {
	xlprot1(val);
	sendmsg(val,getclass(val),k_isnew);
	xlpop();
        /* TAA added 9/5/96 -- fix multiple value return */
#ifdef MULVALS
        xlnumresults = 1;
        xlresults[0] = val;
#endif /* MULVALS */
    }
    
    /* return the result value */
    return (val);
}

/* getivcnt - get the number of instance variables for a class */
LOCAL int getivcnt(cls,ivar)
  LVAL cls; int ivar;
{
    LVAL cnt;
    if (null(cnt = getivar(cls,ivar)) || !fixp(cnt))
	xlfail("bad value for instance variable count");
    return ((int)getfixnum(cnt));
}

/* listlength - find the length of a list */
LOCAL int listlength(list)
  LVAL list;
{
    int len;
    for (len = 0; consp(list); len++)
	list = cdr(list);
    return (len);
}

/* obsymbols - initialize symbols */
VOID obsymbols()
{
    /* enter the object related symbols */
    s_self  = xlenter("SELF");
    k_new   = xlenter(":NEW");
    k_isnew = xlenter(":ISNEW");
    k_prin1 = xlenter(":PRIN1");

    /* get the Object and Class symbol values */
    cls_object = getvalue(xlenter("OBJECT"));
    cls_class  = getvalue(xlenter("CLASS"));
    /* $putpatch.c$: "MODULE_XLOBJ_C_OBSYMBOLS" */
}

/* xloinit - object function initialization routine */
VOID xloinit()
{
    /* create the 'Class' object */
    cls_class = xlclass("CLASS",CLASSSIZE);
    setelement(cls_class,0,cls_class);

    /* create the 'Object' object */
    cls_object = xlclass("OBJECT",0);

    /* finish initializing 'class' */
    setivar(cls_class,SUPERCLASS,cls_object);

    xladdivar(cls_class,"PNAME");           /* ivar number 7  TAA Mod */
    xladdivar(cls_class,"IVARTOTAL");       /* ivar number 6 */
    xladdivar(cls_class,"IVARCNT");         /* ivar number 5 */
    xladdivar(cls_class,"SUPERCLASS");      /* ivar number 4 */
    xladdivar(cls_class,"CVALS");           /* ivar number 3 */
    xladdivar(cls_class,"CVARS");           /* ivar number 2 */
    xladdivar(cls_class,"IVARS");           /* ivar number 1 */
    xladdivar(cls_class,"MESSAGES");        /* ivar number 0 */
    xladdmsg(cls_class,":NEW",FT_CLNEW);
    xladdmsg(cls_class,":ISNEW",FT_CLISNEW);
    xladdmsg(cls_class,":ANSWER",FT_CLANSWER);
    xladdmsg(cls_class,":METHOD",FT_CLMETHOD);

    /* finish initializing 'object' */
    setivar( cls_object,SUPERCLASS,NIL);
    xladdmsg(cls_object,":ISNEW",FT_OBISNEW);
    xladdmsg(cls_object,":CLASS",FT_OBCLASS);
    xladdmsg(cls_object,":SHOW",FT_OBSHOW);
    xladdmsg(cls_object,":PRIN1",FT_OBPRIN1);
    /* $putpatch.c$: "MODULE_XLOBJ_C_XLOINIT" */

}


/* default :PRIN1 method for objects */
LVAL obprin1()
{
    LVAL self,fptr;

    /* get self and the file pointer */
    self = xlgaobject();
    if (moreargs()) {
	if (null(*xlargv)) {
	    fptr = NIL;
	    xlargv++;
	    --xlargc;
	}
	else fptr = xlgetfile(TRUE);
	xllastarg();
    }
    else fptr = getvalue(s_stdout);

    /* print it */
    xputobj(fptr,self);

    /* return the object */
    return (self);
}

/* called by xlprint to tell an object to print itself by faking
   a call like (send obj :prin1 fptr) */
VOID putobj(fptr,obj)
    LVAL fptr,obj;
{
    FRAMEP oldargv;
    int oldargc;

    /* check if there's room for the new call frame (5 slots needed) */
    if (xlsp >= (xlargstktop-5)) xlargstkoverflow();

    /* create a new (dummy) call frame. dummy because (1) stack backtraces
     * won't work anyway since if there's an error when PRINTing an object,
     * that error will probably occur again during the backtrace, and
     * (2) sendmsg() trashes the message selector slot.
     */
    *xlsp   = cvfixnum((FIXTYPE)(xlsp - xlfp));
    xlfp    = xlsp++;   /* new frame pointer */
    *xlsp++ = NIL;      /* dummy function */
    *xlsp++ = cvfixnum((FIXTYPE) 2);    /* we have two arguments */
    *xlsp++ = k_prin1; /* 1st arg: the message (trashed by sendmsg()) */
    *xlsp++ = fptr;     /* 2nd arg: the file/stream */

    /* save old xlargc and xlargv. set up new ones */
    oldargc = xlargc;
    oldargv = xlargv;
    xlargc  = 1;        /* one arg to be picked up */
    xlargv  = xlfp + 4; /* points at 2nd arg: the file/stream */

    /* do it */
    sendmsg(obj,getclass(obj),k_prin1);

    /* restore xlargc and xlargv */
    xlargc  = oldargc;
    xlargv  = oldargv;

    /* remove call frame */
    xlsp    = xlfp;
    xlfp   -= (int)getfixnum(*xlfp);
}
