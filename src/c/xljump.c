/* xljump - execution context routines */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* forward declarations */
LOCAL VOID findandjump P2H(int, char *);

/* xlbegin - beginning of an execution context */
VOID xlbegin P3C(CONTEXT *, cptr, int, flags, LVAL, expr)
{
    cptr->c_flags = flags;
    cptr->c_expr = expr;
    cptr->c_xlstack = xlstack;
    cptr->c_xlenv = xlenv;
    cptr->c_xlfenv = xlfenv;
    cptr->c_xldenv = xldenv;
    cptr->c_xlcontext = xlcontext;
    cptr->c_xlargv = xlargv;
    cptr->c_xlargc = xlargc;
    cptr->c_xlfp = xlfp;
    cptr->c_xlsp = xlsp;
#ifdef BYTECODE
    cptr->c_xlcstop = xlcstop;
#endif /* BYTECODE */
    xlcontext = cptr;
}

/* xlend - end of an execution context */
VOID xlend P1C(CONTEXT *, cptr)
{
    xlcontext = cptr->c_xlcontext;
}

/* xlgo - go to a label */
/* TAA MOD 4/94 -- changed mechanism that GO target was
   propagated back to tagbody(). Formerly the context block
   was altered, but this caused problems with GO's within
   UNWIND-PROTECTS (Error reported by Gottfried Ira, 3/94) */
VOID xlgo P1C(LVAL, label)
{
#ifdef LEXBIND
  CONTEXT *cptr, *ctarg;
  FRAMEP argv;
  LVAL env,frame,tags;
  int argc;

  /* find a lexically visible tag context */
  for (ctarg = NULL, env = xlenv; consp(env); env = cdr(env))
    for (frame = car(env); consp(frame); frame = cdr(frame))
      if (tagentry_p(car(frame)) && consp(tagentry_value(car(frame))))
	for (tags = car(tagentry_value(car(frame)));
	     consp(tags);
	     tags = cdr(tags))
	  if (label == car(tags)) {
	    ctarg = tagentry_context(car(frame));
	    goto find_and_jump;
	  }

  /* find a tagbody context */
 find_and_jump:
  for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
    if (cptr->c_flags & CF_GO && cptr == ctarg) {
      argc = cptr->c_xlargc;
      argv = cptr->c_xlargv;
      while (--argc >= 0)
	if (*argv++ == label) {
	  xljump(cptr,(cptr->c_xlargc - argc),NIL);
	}
    }
  xlerror("no target for GO", label);
#else
  CONTEXT *cptr;
  FRAMEP argv;
  int argc;

  /* find a tagbody context */
  for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
    if (cptr->c_flags & CF_GO) {
      argc = cptr->c_xlargc;
      argv = cptr->c_xlargv;
      while (--argc >= 0)
	if (*argv++ == label) {
	  xljump(cptr,(cptr->c_xlargc - argc),NIL);
	}
    }
  xlfail("no target for GO");
#endif
}

/* xlreturn - return from a block */
VOID xlreturn P2C(LVAL, name, LVAL, val)
{
#ifdef LEXBIND
  CONTEXT *cptr, *ctarg;
  LVAL env, frame;

  /* find the lexical context */
  for (ctarg = NULL, env = xlenv; consp(env); env = cdr(env))
    for (frame = car(env); consp(frame); frame = cdr(frame))
      if (consp(car(frame)) &&    /* Added 6/16/95, from Niels Mayer */
	  tagentry_p(car(frame)) && tagentry_value(car(frame)) == name) {
	ctarg = tagentry_context(car(frame));
	goto find_and_jump;
      }

  /* find the context and jump */
 find_and_jump:
  for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
    if (cptr->c_flags & CF_RETURN && cptr->c_expr == name && cptr == ctarg)
      xljump(cptr,CF_RETURN,val);
  xlerror("no target for RETURN", name);
#else
  CONTEXT *cptr;

  /* find a block context */
  for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
    if (cptr->c_flags & CF_RETURN && cptr->c_expr == name)
      xljump(cptr,CF_RETURN,val);
  xlfail("no target for RETURN");
#endif
}

/* xlthrow - throw to a catch */
VOID xlthrow P2C(LVAL, tag, LVAL, val)
{
    CONTEXT *cptr;

    /* find a catch context */
    for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
	if ((cptr->c_flags & CF_THROW) && cptr->c_expr == tag)
	    xljump(cptr,CF_THROW,val);
    xlfail("no target for THROW");
}

/* xlsignal - signal an error */
VOID xlsignal P2C(char *, emsg, LVAL, arg)
{
    CONTEXT *cptr;

    /* find an error catcher */
    for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_ERROR) {
	    if (!null(cptr->c_expr) && emsg != NULL)
		xlerrprint("error",NULL,emsg,arg);
	    xljump(cptr,CF_ERROR,NIL);
	}
}

/* xltoplevel - go back to the top level */
VOID xltoplevel P1C(int, print)
{
  if (print)
    dbgputstr("[ back to top level ]\n");   /* TAA MOD -- was std */
  findandjump(CF_TOPLEVEL,"no top level");
}

/* xlbrklevel - go back to the previous break level */
VOID xlbrklevel(V)
{
    if (batchmode) xlfatal("uncaught error");
    findandjump(CF_BRKLEVEL,"no previous break level");
}

/* xlcleanup - clean-up after an error */
VOID xlcleanup(V)
{
    dbgputstr("[ back to previous break level ]\n");   /* TAA MOD -- was std */
    findandjump(CF_CLEANUP,"not in a break loop");
}

/* xlcontinue - continue from an error */
VOID xlcontinue(V)
{
    findandjump(CF_CONTINUE,"not in a break loop");
}

/* xljump - jump to a saved execution context */
VOID xljump P3C(CONTEXT *, target, int, mask, LVAL, val)
{
    /* unwind the execution stack */
    for (; xlcontext != target; xlcontext = xlcontext->c_xlcontext)

	/* check for an UNWIND-PROTECT */
	if ((xlcontext->c_flags & CF_UNWIND)) {
	    xltarget = target;
	    xlmask = mask;
	    break;
	}
	   
    /* restore the state */
    xlstack = xlcontext->c_xlstack;
    xlenv = xlcontext->c_xlenv;
    xlfenv = xlcontext->c_xlfenv;
    xlunbind(xlcontext->c_xldenv);
    xlargv = xlcontext->c_xlargv;
    xlargc = xlcontext->c_xlargc;
    xlfp = xlcontext->c_xlfp;
    xlsp = xlcontext->c_xlsp;
#ifdef BYTECODE
    xlcstop = xlcontext->c_xlcstop;
#endif /* BYTECODE */
    xlvalue = val;

    /* call the handler */
    XL_LONGJMP(xlcontext->c_jmpbuf,mask);
}

/* findandjump - find a target context frame and jump to it */
LOCAL VOID findandjump P2C(int, mask, char *, error)
{
    CONTEXT *cptr;

    /* find a block context */
    for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & mask)
	    xljump(cptr,mask,NIL);
    xlabort(error);
}
