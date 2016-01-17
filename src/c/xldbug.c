/* xldebug - xlisp debugging support */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* forward declarations */
LOCAL VOID breakloop P5H(char *, char *, char *, LVAL, int);
#ifdef CONDITIONS
LOCAL VOID conditionhook P3H(char *, char *, LVAL);
#endif /* CONDITIONS */

/* xlabort - xlisp serious error handler */
VOID xlabort P1C(char *, emsg)
{
    xlsignal(emsg,s_unbound);
    xlerrprint("error",NULL,emsg,s_unbound);
    xlbrklevel();
}

/* xlbreak - enter a break loop */
VOID xlbreak P2C(char *, emsg, LVAL, arg)
{
    breakloop("break","return from BREAK",emsg,arg,TRUE);
}

/* xlfail - xlisp error handler */
VOID xlfail P1C(char *, emsg)
{
    xlerror(emsg,s_unbound);
}

/* xlerror - handle a fatal error */
LVAL xlerror P2C(char *, emsg, LVAL, arg)
{
  /** This is an incredible hack.  If an error occurs in
      initialization we are in big trouble.  The most common reason is
      that on this hardware division by zero causes a SIGFPE.  This
      really needs to be dealt with in configure. **/
  if (s_breakenable == NULL) {
    if (strcmp(emsg, "floating point error")==0)
      xoserror("floating point error, probably in computing IEEE infinity"
	       "--check how to prevent floating point division by zero"
	       " from raising a SIGFPE on your system");
    else xoserror(emsg);
    exit(1);
  }
#ifdef CONDITIONS
    if (s_condition_hook != NULL && getvalue(s_condition_hook) != NIL)
      conditionhook(NULL, emsg, arg);
    else
#endif /* CONDITIONS */
    if (!null(getvalue(s_breakenable)))
	breakloop("error",NULL,emsg,arg,FALSE);
    else {
	xlsignal(emsg,arg);
	xlerrprint("error",NULL,emsg,arg);
	xlbrklevel();
    }
    return NIL;     /* actually doesn't return */
}

/* xlcerror - handle a recoverable error */
VOID xlcerror P3C(char *, cmsg, char *, emsg, LVAL, arg)
{
#ifdef CONDITIONS
    if (s_condition_hook != NULL && getvalue(s_condition_hook) != NIL)
      conditionhook(cmsg, emsg, arg);
    else
#endif /* CONDITIONS */
    if (!null(getvalue(s_breakenable)))
	breakloop("error",cmsg,emsg,arg,TRUE);
    else {
	xlsignal(emsg,arg);
	xlerrprint("error",NULL,emsg,arg);
	xlbrklevel();
    }
}

/* xlerrprint - print an error message */
VOID xlerrprint P4C(char *, hdr, char *, cmsg, char *, emsg, LVAL, arg)
{
    /* TAA MOD -- start error message on a fresh line */
    xlfreshline(getvalue(s_stderr));

    /* print the error message */
    sprintf(buf,"%s: %s",hdr,emsg);
    errputstr(buf);

    /* print the argument */
    if (arg != s_unbound) {
	errputstr(" - ");
	errprint(arg);
    }

    /* no argument, just end the line */
    else
	errputstr("\n");

    /* print the continuation message */
    if (cmsg != NULL) {
	sprintf(buf,"if continued: %s\n",cmsg);
	errputstr(buf);
    }
}

#ifdef NEED_TO_REPLACE_BREAKLOOP
/* $putpatch.c$: "MODULE_XLDBUG_C_BREAKLOOP_REPLACEMENT" */
#else

/* breakloop - the debug read-eval-print loop */
LOCAL VOID breakloop P5C(char *, hdr, char *, cmsg, char *, emsg, LVAL, arg, int, cflag)
{
    LVAL expr,val;
    CONTEXT cntxt;
    int type;

    /* print the error message */
    xlerrprint(hdr,cmsg,emsg,arg);

    /* handle running in batch mode */
    if (batchmode) xlfatal("uncaught error");

    /* flush the input buffer */
    xlflush();

    /* do the back trace */
    if (!null(getvalue(s_tracenable))) {
	val = getvalue(s_tlimit);
	xlbaktrace(fixp(val) ? (int)getfixnum(val) : -1);
    }

    /* protect some pointers */
    xlsave1(expr);

    /* increment the debug level */
    ++xldebug;

    /* debug command processing loop */
    xlbegin(&cntxt, CF_BRKLEVEL|CF_CLEANUP|CF_CONTINUE, s_true);
    for (type = 0; type == 0; ) {

	/* setup the continue trap */
#ifdef CRAYCC
        type = XL_SETJMP(cntxt.c_jmpbuf);
	if (type != 0)
#else
	if ((type = XL_SETJMP(cntxt.c_jmpbuf)) != 0)
#endif /* CRAYCC */
	    switch (type) {
	    case CF_CLEANUP:
		continue;
	    case CF_BRKLEVEL:
		type = 0;
		break;
	    case CF_CONTINUE:
		if (cflag) {
		    dbgputstr("[ continue from break loop ]\n");
		    continue;
		}
		else xlabort("this error can't be continued");
	    }

	/* print a prompt */
#ifdef PACKAGES
	{
	  LVAL pack = getvalue(s_package);
	  if (pack != xluserpack && goodpackagep(pack)) {
	    dbgputstr(getstring(xlpackagename(pack)));
	    dbgputstr(" ");
	  }
	}
#endif /* PACKAGES */
	sprintf(buf,"%d> ",xldebug);
	dbgputstr(buf);

	/* read an expression and check for eof */
	if (!xlread(getvalue(s_debugio),&expr, FALSE, FALSE)) {
	    type = CF_CLEANUP;
	    break;
	}

	/* save the input expression */
	xlrdsave(expr);

	/* evaluate the expression */
	expr = xleval(expr);

	/* save the result */
	xlevsave(expr);

	/* Show result on a new line -- TAA MOD to improve display */
	xlfreshline(getvalue(s_debugio));

	/* print it */
#ifdef MULVALS
	{
	  int i;
	  for (i = 0; i < xlnumresults; i++)
	    dbgprint(xlresults[i]);
	}
#else
	dbgprint(expr);
#endif /* MULVALS */
    }
    xlend(&cntxt);

    /* decrement the debug level */
    --xldebug;

    /* restore the stack */
    xlpop();

    /* check for aborting to the previous level */
    if (type == CF_CLEANUP)
	xlbrklevel();
}
#endif

/* baktrace - do a back trace */
VOID xlbaktrace P1C(int, n)
{
  FRAMEP fp, p;
  int argc;
  for (fp = xlfp; (n < 0 || n--) && !null(*fp); fp = fp - (int)getfixnum(*fp)) {
    p = fp + 1;
    errputstr("Function: ");
    errprint(*p++);
    if (getvalue(s_baktraceprargs) != NIL) {
      if ((argc = (int)getfixnum(*p++)) != 0)
	errputstr("Arguments:\n");
      while (--argc >= 0) {
	errputstr("  ");
	errprint(*p++);
      }
    }
  }
}

/* xldinit - debug initialization routine */
VOID xldinit(V)
{
    xlsample = 0;
    xldebug = 0;
}

VOID xlsigint(V)
{
  FRAMEP newfp;

  if (boundp(s_intaction) && !null(getvalue(s_intaction))) {
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(getvalue(s_intaction));
    pusharg(cvfixnum((FIXTYPE)0));
    xlfp = newfp;
    xlapply(0);
  }
}

#ifdef CONDITIONS
LOCAL VOID conditionhook P3C(char *, cmsg, char *, emsg, LVAL, arg)
{
  FRAMEP newfp;
  LVAL olddenv,val,hook;
  int argc;

  /* rebind the hook functions to nil */
  hook = getvalue(s_condition_hook);
  olddenv = xldenv;
  xldbind(s_condition_hook,NIL);

  /* compute argument count */
  argc = cmsg == NULL ? 4 : 5;
  if (arg != s_unbound) argc++;

  /* create the new call frame */
  newfp = xlsp;
  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
  pusharg(hook);
  pusharg(cvfixnum((FIXTYPE) argc));
  pusharg(cmsg == NULL ? s_error : s_cerror);
  pusharg(null(xlfp[0]) ? NIL : cvfixnum((FIXTYPE) (xlfp - xlargstkbase)));
  pusharg(cons(xlenv,xlfenv));
  if (cmsg != NULL) pusharg(cvstring(cmsg));
  if (arg != s_unbound) {
    strcpy(buf, emsg);
    strcat(buf, " - ~S");
    pusharg(cvstring(buf));
  }
  else
    pusharg(cvstring(emsg));
  if (arg != s_unbound) pusharg(arg);
  xlfp = newfp;

  /* call the hook function */
  val = xlapply(argc);

  /* unbind the symbols */
  xlunbind(olddenv);

  if (cmsg == NULL)
    xlabort("condition hook function should not have returned");
}
#endif /* CONDITIONS */
