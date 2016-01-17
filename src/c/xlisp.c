/* xlisp.c - a small implementation of lisp with object-oriented programming */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

/* For full credits see file xlisp.h */

#include "xlisp.h"

/* define the banner line string */
#define BANNER  "XLISP-PLUS version 3.04\n\
Portions Copyright (c) 1988, by David Betz.\n\
Modified by Thomas Almy and others."

/* global variables */
#ifdef SAVERESTORE
XL_JMP_BUF top_level;
#endif
char *progname;  /* used for reading the symbol table - L. Tierney */
#ifdef SAVERESTORE
char *resfile = "xlisp.wks"; /* make extern to allow setting elsewhere */
#endif

/* local variables */
XL_JMP_BUF exit_xlisp;

/* forward declarations */
#ifdef MACINTOSH
int main(void);
#else
int main _((int argc, char *argv[]));
#endif /* MACINTOSH */
LOCAL VOID toplevelloop(V);

/* main - the main routine */
#ifdef MACINTOSH
int main(void)
#else
int main(argc,argv)
     int argc; char *argv[];
#endif /* MACINTOSH */
{
    char *transcript;
    CONTEXT cntxt;
    int verbose,i, sts;
    struct { char *transcript; int verbose, i; } state;
#ifdef AMIGA
    char project[30],defdir[50];
#endif /* AMIGA */

    /* The way out on errors */
    i = XL_SETJMP(exit_xlisp);
    if (i != 0)
	return i-1;

    /* setup default argument values */
    transcript = NULL;
    verbose = FALSE;

#ifdef FILETABLE
    /* Initialize the file table values */
    filetab[0].fp = stdin;
    filetab[0].tname = "(stdin)";
    filetab[1].fp = stdout;
    filetab[1].tname = "(stdout)";
    filetab[2].fp = stderr;
    filetab[2].tname = "(console)";
    filetab[3].fp = NULL;
    filetab[3].tname = "";
#endif

    /* parse the argument list switches */
#ifndef MACINTOSH
#ifdef AMIGA
    FindStart(&argc,argv,deftool,project,defdir);
#endif /* AMIGA */
    progname = argv[0];  /* L. Tierney */
    for (i = 1; i < argc; ++i)
	if (argv[i][0] == '-')
	    switch(isupper(argv[i][1])?tolower(argv[i][1]):argv[i][1]) {
	    case 't':
		transcript = &argv[i][2];
		break;
	    case 'b':
		batchmode = TRUE;
		break;
	    case 'v':
		verbose = TRUE;
		break;
#ifdef SAVERESTORE
            case 'w':
                resfile = &argv[i][2];
                break;
#endif
#ifdef XLISP_STAT
	    case 'p':
		defaultpath = &argv[i][2];
		break;
#endif /* XLISP_STAT */
#ifndef _Windows
            default: /* Added to print bad switch message */
                fprintf(stderr,"Bad switch: %s\n",argv[i]);
#endif
	    }
#endif /* MACINTOSH */

    /* initialize and print the banner line */
    osinit(BANNER);

    /* setup initialization error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
    state.transcript = transcript; state.verbose = verbose; state.i = i;
    sts = XL_SETJMP(cntxt.c_jmpbuf);
    transcript = state.transcript; verbose = state.verbose; i = state.i;
    if (sts)
	xlfatal("fatal initialization error");
#ifdef SAVERESTORE
    state.transcript = transcript; state.verbose = verbose; state.i = i;
    sts = XL_SETJMP(top_level);
    transcript = state.transcript; verbose = state.verbose; i = state.i;
    if (sts)
	xlfatal("RESTORE not allowed during initialization");
#endif

    /* initialize xlisp */
#ifdef SAVERESTORE
#ifdef MACINTOSH
    i = macxlinit(resfile);
#else  
    i = xlinit(resfile);
#endif /* MACINTOSH */
#else
    i = xlinit(NULL);
#endif

    /* reset the error handler, since we know what "true" is */
    xlend(&cntxt);
    xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, s_true);

    /* open the transcript file */
    if (transcript!=NULL && (tfp = OSAOPEN(transcript,CREATE_WR)) == CLOSED) {
	/* TAA Mod -- quote name so "-t foo" will indicate no file name */
	sprintf(buf,"error: can't open transcript file: \"%s\"",transcript);
	stdputstr(buf);
    }

#ifndef MACINTOSH
    /* enter the command line (L. Tierney 9/93) */
    state.transcript = transcript; state.verbose = verbose; state.i = i;
    sts = XL_SETJMP(cntxt.c_jmpbuf);
    transcript = state.transcript; verbose = state.verbose; i = state.i;
    if (sts == 0) {
      LVAL line;
      int j;
      xlsave1(line);
      line = NIL;
      for (j = argc - 1; j >= 0; j--)
	line = cons(cvstring(argv[j]), line);
      xlpop();
      setsvalue(s_command_line, line);
    }
#endif /* MACINTOSH */

    enable_interrupts();

    /* load "init.lsp" */
    if (i) {
      state.transcript = transcript; state.verbose = verbose; state.i = i;
      sts = XL_SETJMP(cntxt.c_jmpbuf);
      transcript = state.transcript; verbose = state.verbose; i = state.i;
      if (sts == 0)
	xsload("init.lsp",TRUE,FALSE);
    }

    /* run any startup functions (L. Tierney 9/93) */
    state.transcript = transcript; state.verbose = verbose; state.i = i;
    sts = XL_SETJMP(cntxt.c_jmpbuf);
    transcript = state.transcript; verbose = state.verbose; i = state.i;
    if (sts == 0) {
      LVAL funs = getvalue(s_startup_functions);
      FRAMEP newfp;

      for (; consp(funs); funs = cdr(funs)) {
	newfp = xlsp;
	pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	pusharg(car(funs));
	pusharg(cvfixnum((FIXTYPE) 0));
	xlfp = newfp;
	xlapply(0);
      }
    }

    /* load any files mentioned on the command line */
    if (! null(getvalue(s_loadfileargs))) {
      state.transcript = transcript; state.verbose = verbose; state.i = i;
      sts = XL_SETJMP(cntxt.c_jmpbuf);
      transcript = state.transcript; verbose = state.verbose; i = state.i;
      if (sts == 0) {
#ifdef MACINTOSH
        macloadinits();
#else
        for (i = 1; i < argc; i++)
	    if (argv[i][0] != '-' && !xsload(argv[i],TRUE,verbose))
		xlerror("can't load file",cvstring(argv[i]));
#endif /* MACINTOSH */
      }
    }

    /* target for restore */
#ifdef SAVERESTORE
    state.transcript = transcript; state.verbose = verbose; state.i = i;
    sts = XL_SETJMP(top_level);
    transcript = state.transcript; verbose = state.verbose; i = state.i;
    if (sts)
	xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, s_true);
#endif

    /* main command processing loop */
    for (;;) {

	/* setup the error return */
	if (XL_SETJMP(cntxt.c_jmpbuf)) {
	    setvalue(s_evalhook,NIL);
	    setvalue(s_applyhook,NIL);
	    xltrcindent = 0;
	    xldebug = 0;
	    osreset();   /* L. Tierney */
	    xlflush();
	}

#ifdef STSZ
	stackwarn = FALSE;
#endif

	if (boundp(s_toplevelloop)) {
	  FRAMEP newfp;

	  newfp = xlsp;
	  pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	  pusharg(getvalue(s_toplevelloop));
	  pusharg(cvfixnum((FIXTYPE) 0));
	  xlfp = newfp;
	  xlapply(0);
	}
	else 
	  toplevelloop();
    }   /* never exit from here */
}

/* xtoplevelloop - lisp-callable top level loop */
/* Luke Tierney 9/93 */
LVAL xtoplevelloop(V)
{
  xllastarg();
  
  toplevelloop();
  return(NIL); /* doesn't return */
}

/* toplevelloop - the default command loop */
LOCAL VOID toplevelloop(V)
{
  LVAL expr;
#ifdef MULVALS
  int i;
#endif /* MULVALS */

  /* protect some pointers */
  xlsave1(expr);

  for(;;) {
    /* print a prompt */
#ifdef PACKAGES
    if (!redirectin) {
      LVAL pack = getvalue(s_package);
      if (pack != xluserpack && goodpackagep(pack)) {
	dbgputstr(getstring(xlpackagename(pack)));
      }
      dbgputstr("> ");
    }
#else
    if (!redirectin) dbgputstr("> ");
#endif /* PACKAGES */

    /* read an expression */
    if (!xlread(getvalue(s_stdin),&expr,FALSE,FALSE)) {
      /* clean up */
      wrapup();
      break;
    }

    /* save the input expression */
    xlrdsave(expr);

    /* evaluate the expression */
    expr = xleval(expr);

    /* save the result */
    xlevsave(expr);

    /* Show result on a new line -- TAA MOD to improve display */
    xlfreshline(getvalue(s_stdout));

    /* print it */
#ifdef MULVALS
    switch (xlnumresults) {
    case 0: break;
    case 1: stdprint(expr); break;
    default:
      {
	LVAL vals;
	xlsave1(vals);
	for (i = xlnumresults; i-- > 0; ) vals = cons(xlresults[i], vals);
	for (; consp(vals); vals = cdr(vals)) stdprint(car(vals));
	xlpop();
      }
    }
#else
    stdprint(expr);
#endif /* MULVALS */
  }
}

/* xlrdsave - save the last expression returned by the reader */
VOID xlrdsave P1C(LVAL, expr)
{
    setvalue(s_3plus,getvalue(s_2plus));
    setvalue(s_2plus,getvalue(s_1plus));
    setvalue(s_1plus,getvalue(s_minus));
    setvalue(s_minus,expr);
}

/* xlevsave - save the last expression returned by the evaluator */
VOID xlevsave P1C(LVAL, expr)
{
    setvalue(s_3star,getvalue(s_2star));
    setvalue(s_2star,getvalue(s_1star));
    setvalue(s_1star,expr);
}

/* xlfatal - print a fatal error message and exit */
VOID xlfatal P1C(char *, msg)
{
    xoserror(msg);
    wrapup();
}

/* do-exits - run user exit functions */
VOID do_exits(V)
{
  CONTEXT cntxt;

  xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,s_true);
  XL_SETJMP(cntxt.c_jmpbuf);

  while (s_exit_functions != NULL && consp(getvalue(s_exit_functions))) {
    FRAMEP newfp;
    LVAL func = car(getvalue(s_exit_functions));
    setvalue(s_exit_functions, cdr(getvalue(s_exit_functions)));
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(func);
    pusharg(cvfixnum((FIXTYPE) 0));
    xlfp = newfp;
    xlapply(0);
  }
  xlend(&cntxt);
}

/* wrapup - clean up and exit to the operating system */
VOID wrapup(V)
{
  /* $putpatch.c$: "MODULE_XLISP_C_WRAPUP" */
  CONTEXT cntxt;

  do_exits();
  if (XL_SETJMP(cntxt.c_jmpbuf) == 0) {
    if (tfp != CLOSED)
      OSCLOSE(tfp);
    osfinish();
  }
  XL_LONGJMP(exit_xlisp, 1);
}

/* xresetsystem - reset system for user top-levels */
LVAL xresetsystem(V)
{
  osreset();   /* L. Tierney */
  xlflush();
  return(NIL);
}

/* new internal load function -- allows load to be redefined in workspace */
int xsload P3C(char *, name, int, vflag, int, pflag)
{
  if (fboundp(s_load)) {
    FRAMEP newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(getfunction(s_load));
    pusharg(cvfixnum((FIXTYPE) 7));
    pusharg(cvstring(name));
    pusharg(k_print);
    pusharg(pflag ? s_true : NIL);
    pusharg(k_verbose);
    pusharg(vflag ? s_true : NIL);
    pusharg(k_nexist);
    pusharg(NIL);
    xlfp = newfp;

    /* return the result of applying the function */
    return null(xlapply(7)) ? FALSE : TRUE;
  }
  else
    return xlload(name, pflag, vflag);
}
