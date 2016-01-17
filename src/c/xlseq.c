/* xlseq.c - xlisp sequence functions */
/*  Written by Thomas Almy, based on code:
    Copyright (c) 1985, by David Michael Betz
    All Rights Reserved
    Permission is granted for unrestricted non-commercial use   */

#include "xlisp.h"

/* this is part of the COMMON LISP extension: */
/* (elt seq index)  -- generic sequence reference function */
/* (map type fcn seq1 [seq2 ...]) -- generic sequence mapping function */
/*   type is one of cons, array, string, or nil */
/* (some fcn seq1 [seq2 ...]) -- apply fcn until non-nil */
/*    also every notany and notevery */
/* (concatenate type seq1 [seq2 ...]) -- sequence concatenation function */
/*    type is one of cons, array, or string. */
/* (search seq1 seq1 &key :test :test-not :start1 :end1 :start2 :end2) --
    generic sequence searching function. */
/* subseq reverse remove remove-if remove-if-not delete delete-if 
   delete-if-not -- rewritten to allow all sequence types */
/* Position, position-if, position-if-not, count, count-if, count-if-not,
   find, find-if, find-if-not */
/* the keyword arguments :start and :end are now valid for the remove, delete,
   find position and count functions */
/* the keyword argument :key is also valid where appropriate */

/* The author, Tom Almy, appologizes for using "goto" several places in
   this code. */

/**** should add :FROM-END to countif, xsearch, xreduce */

/* Function prototypes */
LOCAL VOID getseqbounds P5H(unsigned *, unsigned *, unsigned, LVAL, LVAL);
LOCAL LVAL map P1H(int);
LOCAL LVAL xlmapwhile P1H(int);
LOCAL LVAL remif P3H(int, int, int);
LOCAL LVAL substituteif P3H(int, int, int);
LOCAL LVAL xlkitchensink P3H(int, int, int);
LOCAL unsigned calclength(V);
LOCAL LVAL cattovector P1H(LVAL);
LOCAL LVAL cattocons(V);

#define getlength(x) (listp(x) ? llength(x) : gettvecsize(x))

LOCAL VOID getseqbounds P5C(unsigned *, start,
			    unsigned *, end,
			    unsigned, length,
                            LVAL, startkey,
			    LVAL, endkey)
{
  LVAL arg;
  FIXTYPE temp;

  if (xlgkfixnum(startkey,&arg)) {
    temp = getfixnum(arg);
    if (temp < 0 || temp > (FIXTYPE)length ) goto rangeError;
    *start = (unsigned)temp;
  }
  else *start = 0;
    
  if (xlgetkeyarg(endkey, &arg) && !null(arg)) {
    if (!fixp(arg)) xlbadtype(arg);
    temp = getfixnum(arg);
    if (temp < (FIXTYPE)*start  || temp > (FIXTYPE)length) goto rangeError;
    *end = (unsigned)temp;  
  }
  else *end = length;
    
  return;
  /* else there is a range error */
    
 rangeError:
  xlerror("range error",arg);
}
        

/* xelt - sequence reference function */
LVAL xelt(V)
{
  LVAL seq,index;
  FIXTYPE i;
    
  /* get the sequence and the index */
  seq = xlgetarg();

  index = xlgafixnum(); i = getfixnum(index); 
  if (i < 0) goto badindex;
    
  xllastarg();

  if (listp(seq)) { /* do like nth, but check for in range */
    /* find the ith element */
    while (consp(seq)) {
      if (i-- == 0) return (car(seq));
      seq = cdr(seq);
    }
    goto badindex;  /* end of list reached first */
  }
        
  switch (ntype(seq)) {
  case VECTOR:
  case STRING:
  case TVEC:
    if (i >= (FIXTYPE)gettvecsize(seq)) goto badindex;
    return (gettvecelement(seq,(int)i));
  default:
    xlbadtype(seq);
  }
    
 badindex:
  xlerror("index out of bounds",index);
  return (NIL);   /* eliminate warnings */
}

LOCAL LVAL map P1C(int, into)
{
  FRAMEP newfp;
  LVAL fun, lists, val, last = NULL, x, y, etype = NULL;
  unsigned len, rlen, temp, i;
  int argc, typ = 0;
    
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(fun);
  xlsave(lists);
  xlsave(val);

  /* get the type of resultant, and resultant for map-into */
  val = xlgetarg();
  if (null(val))
    typ = 0;    /* return nothing */
  else {
    if (into) {
      switch (ntype(val)) {
      case SYMBOL:
	if (! null(val))
	  xlbadtype(val);
	typ = CONS;
	break;
      case CONS:   typ = CONS;   break;
      case VECTOR: typ = VECTOR; break;
      case STRING: typ = STRING; break;
      case TVEC:   typ = TVEC;   break;
      default: xlbadtype(val);
      }
    }
    else {
      if ((typ = xlcvttype(val)) != CONS &&
	  typ != STRING && typ != VECTOR &&
	  typ != DARRAY) {
	xlerror("invalid result type", val);
      }
      if (typ == STRING) etype = a_char;
      else if (consp(val) && consp(cdr(val))) /* very superficial check! */
	etype = car(cdr(val));
      else etype = s_true;
    }
  }

  /* get the function to apply and argument sequences */
  fun = xlgetarg();
  /* Initialization code bug fixed, Luke Tierney 3/94 */
  if (into) { /* MAP-INTO */
    len = getlength(val);
    if (moreargs()) {  /* handle first argument */
      x = xlgaseq();
      if ((temp = getlength(x)) < len) len = temp;
      argc = 1;
      lists = last = consa(x);
    }
    else {
      lists = NIL;
      argc = 0;
    }
  }
  else { /* MAP */
    val = NIL;
    lists = xlgaseq();
    len = getlength(lists);
    lists = last = consa(lists);
    argc = 1;
  }

  /* build a list of argument lists */
  for (; moreargs(); last = cdr(last)) {
    x = xlgaseq();
    if ((temp = getlength(x)) < len) len = temp;
    argc++;
    rplacd(last,(consa(x)));
  }
    
  if (into) {
    rlen = getlength(val);
    if (rlen < len) len = rlen;
  }
  else if (typ != 0) {
    /* initialize the result list */
    switch (typ) {
    case DARRAY:
    case VECTOR:
    case STRING:
    case TVEC:
      val = mktvec(len, etype); 
      break;
    default:    
      val = mklist(len, NIL);
      break;
    }
  }
  else val = NIL;

  /* loop through each of the argument lists */
  for (i=0,last=val;i<len;i++) {

    /* build an argument list from the sublists */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)argc));
    for (x = lists; consp(x) ; x = cdr(x)) {
      y = car(x);
      switch (ntype(y)) {
      case CONS: 
	pusharg(car(y));
	rplaca(x,cdr(y));
	break;
      case VECTOR:
      case STRING:
      case TVEC:
	pusharg(gettvecelement(y,i));
	break;
      }
    }

    /* apply the function to the arguments */
    xlfp = newfp;
    x = xlapply(argc);

    switch (typ) {
    case CONS:
      rplaca(last, x);
      last = cdr(last);
      break;
    case DARRAY:
    case VECTOR:
    case STRING:
    case TVEC:
      settvecelement(val,i,x);
      break;
    }
  }

  /* restore the stack */
  xlpopn(3);

  /* return the last test expression value */
  return (val);
}

/* xmap -- map function */
LVAL xmap(V)     {return map(FALSE);}
LVAL xmapinto(V) {return map(TRUE);}

/* every, some, notany, notevery */

#define EVERY 0
#define SOME 1
#define NOTEVERY 2
#define NOTANY 3

LOCAL LVAL xlmapwhile P1C(int, cond)
{
  int exitcond;
  FRAMEP newfp;
  LVAL fun, lists, val, last, x, y;
  unsigned len,temp,i;
  int argc;
    
  /* protect some pointers */
  xlstkcheck(2);
  xlsave(fun);
  xlsave(lists);

  /* get the function to apply and argument sequences */
  fun = xlgetarg();
  lists = xlgaseq();
  len = getlength(lists);
  argc = 1;

  /* build a list of argument lists */
  for (lists = last = consa(lists); moreargs(); last = cdr(last)) {
    val = xlgaseq();
    if ((temp = getlength(val)) < len) len = temp;
    argc++;
    rplacd(last,(cons(val,NIL)));
  }
  
  switch (cond) {
  case SOME:
  case NOTANY:
    exitcond = TRUE;
    val = NIL;
    break;
  case EVERY:
  case NOTEVERY:
    exitcond = FALSE;
    val = s_true;
    break;
  default: /* to keep compiler happy */
    exitcond = FALSE;
    val = NIL;
  }

  /* loop through each of the argument lists */
  for (i=0;i<len;i++) {

    /* build an argument list from the sublists */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)argc));
    for (x = lists; consp(x); x = cdr(x)) {
      y = car(x);
      switch (ntype(y)) {
      case CONS: 
	pusharg(car(y));
	rplaca(x,cdr(y));
	break;
      case VECTOR:
      case STRING:
      case TVEC:
	pusharg(gettvecelement(y,i));
	break;
      }
    }

    /* apply the function to the arguments */
    xlfp = newfp;
    val = xlapply(argc);
    if (null(val) ^ exitcond) break;
  }

  if ((cond == NOTANY) | (cond == NOTEVERY))
    val = (null(val) ? s_true : NIL);
    

  /* restore the stack */
  xlpopn(2);

  /* return the last test expression value */
  return (val);
}


LVAL xevery(V)
{
  return xlmapwhile(EVERY);
}

LVAL xsome(V)
{
  return xlmapwhile(SOME);
}

LVAL xnotany(V)
{
  return xlmapwhile(NOTANY);
}

LVAL xnotevery(V)
{
  return xlmapwhile(NOTEVERY);
}

/* xconcatenate - concatenate a bunch of sequences */
/* replaces (and extends) strcat, now a macro */
LOCAL unsigned calclength(V)
{
  LVAL tmp;
  FRAMEP saveargv;
  int saveargc;
  long len;

  /* save the argument list */
  saveargv = xlargv;
  saveargc = xlargc;

  /* find the length of the new string or vector */
  for (len = 0; moreargs(); ) {
    tmp = xlgaseq();
    len += getlength(tmp);
    /****if (len>MAXSLEN) xltoolong();*/  /*check for overflow*/
  }

  /* restore the argument list */
  xlargv = saveargv;
  xlargc = saveargc;

  return (unsigned)len;
}


LOCAL LVAL cattovector P1C(LVAL, etype)
{
  LVAL tmp,val;
  unsigned len,i, j;

  /* find resulting length -- also validates argument types */
  len = calclength();

  /* protect the result */
  xlsave1(val);

  /* create the result vector */
  val = mktvec(len, etype);

  /* combine the vectors */
  for (j = 0; moreargs();) {
    tmp = nextarg();
    if (!null(tmp)) switch (ntype(tmp)) {
    case VECTOR: 
    case STRING:
    case TVEC:
      len = gettvecsize(tmp);
      for (i = 0; i < len; i++, j++)
	settvecelement(val, j, gettvecelement(tmp,i));
      break;
    case CONS:
      for (; consp(tmp); tmp = cdr(tmp), j++)
	settvecelement(val, j, car(tmp));
      break;
    }
  }

  xlpop();

  /* return the new vector */
  return (val);
}


LOCAL LVAL cattocons(V)
{
  LVAL val,tmp,next,last=NIL;
  unsigned len,i;
  long n;
    
  xlsave1(val);       /* protect against GC */
    
  /* combine the lists */
  while (moreargs()) {
    tmp = nextarg();
    if (!null(tmp))
      switch (ntype(tmp)) {
      case CONS:
	/* check for circular list (Added 5/6/94) */
	next = tmp;
	for (n = 0; consp(next); next=cdr(next)) {
	  if (n++ > nnodes)
	    xlcircular();   /*DIRTY, but we loose anyway!*/
	}
	while (consp(tmp)) {
	  next = consa(car(tmp));
	  if (!null(val)) rplacd(last,next);
	  else val = next;
	  last = next;
	  tmp = cdr(tmp);
	}
	break;
      case VECTOR:
      case STRING:
      case TVEC:
	len = gettvecsize(tmp);
	for (i = 0; i < len; i++) {
	  next = consa(gettvecelement(tmp,i));
	  if (!null(val)) rplacd(last,next);
	  else val = next;
	  last = next;
	}
	break;
      default: 
	xlbadtype(tmp); break; /* need default because no precheck*/
      }
  }
    
  xlpop();
    
  return (val);
}
 

LVAL xconcatenate(V)
{
  LVAL type, etype;
    
  switch (xlcvttype(type = xlgetarg())) {  /* target type of data */
  case CONS:
    return cattocons();
  case STRING:
    return cattovector(a_char);
  case DARRAY:
  case VECTOR:
  case TVEC:
    if (consp(type) && consp(cdr(type))) /* very superficial check! */
      etype = car(cdr(type));
    else etype = s_true;
    return cattovector(etype);
  default:
    xlerror("invalid result type", type);
    return (NIL);   /* avoid warning */
  }
}

/* xsubseq - return a subsequence -- new version */
LVAL xsubseq(V)
{
  unsigned start,end=0,len,esize;
  FIXTYPE temp;
  int srctype;
  LVAL src,dst;
  LVAL next,last=NIL;

  /* get sequence */
  src = xlgaseq();
  if (listp(src)) srctype = CONS;
  else srctype=ntype(src);

    
  /* get length */
  len = getlength(src);

  /* get the starting position */
  dst = xlgafixnum(); temp = getfixnum(dst);
  if (temp < 0 || temp > len) 
    xlerror("sequence index out of bounds",dst);
  start = (unsigned) temp;

  /* get the ending position */
  if (moreargs()) {
    dst = nextarg();
    if (null(dst)) end = len;
    else if (fixp(dst)) {
      temp = getfixnum(dst);
      if (temp < start || temp > len)
	xlerror("sequence index out of bounds",dst);
      end = (unsigned) temp;
    }
    else xlbadtype(dst);
  }
  else
    end = len;
  xllastarg();

  len = end - start;
    
  switch (srctype) {  /* do the subsequencing */
  case STRING:
    dst = newstring(len);
    MEMCPY(getstring(dst), getstring(src)+start, len);
    dst->n_string[len] = 0;
    break;
  case VECTOR:
    dst = newvector(len);
    MEMCPY(dst->n_vdata, &src->n_vdata[start], sizeof(LVAL)*len);
    break;
  case TVEC:
    dst = mktvec(len, gettvecetype(src));
    esize = gettveceltsize(src);
    MEMCPY(gettvecdata(dst),
	   ((char *) gettvecdata(src)) + start * esize,
	   esize * len);
    break;
  case CONS:
    xlsave1(dst);
    while (start--) src = cdr(src);
    while (len--) {
      next = consa(car(src));
      if (!null(dst)) rplacd(last,next);
      else dst = next;
      last = next;
      src = cdr(src);
    }
    xlpop();
    break;
  }

  /* return the substring */
  return (dst);
}


/* xnreverse -- built-in function nreverse (destructive reverse) */
LVAL xlnreverse P1C(LVAL, seq)
{
  LVAL val;

  if (null(seq)) return (NIL);    /* empty argument */
    
  switch (ntype(seq)) {
  case CONS:
    val = NIL;
    while (consp(seq)) {
      LVAL next = cdr(seq);
      rplacd(seq,val);
      val = seq;
      seq = next;
    }
    break;
  case DARRAY:
    seq = getdarraydata(seq);
    /* drop through */
  case VECTOR:
  case STRING:
  case TVEC:
    {
      int esize, qstep;
      char *p, *q;

      esize = gettveceltsize(seq);
      p = (char *) gettvecdata(seq);
      q = p + (gettvecsize(seq) - 1) * esize;
      qstep = 2 * esize;
      while (p < q) {
	char *pp = p + esize;
	while (p < pp) {
	  int ival = *p;
	  *p++ = *q;
	  *q++ = ival;
	}
	q -= qstep;
      }
      val = seq;
    }
    break;
  default: 
    val = xlbadtype(seq);
    break;
  }

  /* return the sequence */
  return (val);
}

LVAL xnreverse(V)
{
  LVAL seq;

  /* get the sequence to reverse */
  seq = xlgaseq();
  xllastarg();

  return xlnreverse(seq);
}

/* xreverse - built-in function reverse -- new version */
LVAL xreverse(V)
{
  LVAL seq,val;
  unsigned i,len;

  /* get the sequence to reverse */
  seq = xlgetarg();
  xllastarg();

  if (null(seq)) return (NIL);    /* empty argument */
    
  switch (ntype(seq)) {
  case CONS:
    /* protect pointer */
    xlsave1(val);

    /* append each element to the head of the result list */
    for (val = NIL; consp(seq); seq = cdr(seq))
      val = cons(car(seq),val);

    /* restore the stack */
    xlpop();
    break;
  case VECTOR:
    len = getsize(seq);
    val = newvector(len);
    for (i = 0; i < len; i++)
      setelement(val,i,getelement(seq,len-i-1));
    break;
  case STRING:
    len = getslength(seq);
    val = newstring(len);
    for (i = 0; i < len; i++)
      val->n_string[i] = seq->n_string[len-i-1];
    val->n_string[len] = 0;
    break;
  case TVEC:
    {
      int esize;
      char *p, *q;

      len = gettvecsize(seq);
      val = mktvec(len, gettvecetype(seq));
      esize = gettveceltsize(seq);
      p = (char *) gettvecdata(val);
      q = ((char *) gettvecdata(seq)) + (len - 1) * esize;
      for (i = 0; i < len; i++, p += esize, q -= esize)
	MEMCPY(p, q, esize);
      break;
    }
  default: 
    xlbadtype(seq); break;
  }

  /* return the sequence */
  return (val);
}


/* remif - common code for remove and delete functions */
#define remtest(x,elt,fcn,kfcn) \
  ((expr?dotest2(x,elt,fcn,kfcn):dotest1(elt,fcn,kfcn)) == tresult)
LOCAL LVAL remif P3C(int, tresult, int, expr, int, destruct)
{
  LVAL x,seq,fcn,last,val,elt;
  long i,j,l;
  unsigned start,end;
  int esize;
  LVAL kfcn;
  int fromend=FALSE;  /* process from the end */
  FIXTYPE count=-1;

  if (expr) {
    /* get the expression to delete and the sequence */
    x = xlgetarg();
    seq = xlgaseq();
    xltest(&fcn,&tresult);
  }
  else {
    /* get the function and the sequence */
    fcn = xlgetarg();
    seq = xlgaseq();
    x = NIL;                    /* to keep compiler happy */
  }

  getseqbounds(&start,&end,getlength(seq),k_start,k_end);

  if (xlgetkeyarg(k_fromend, &val) && !null(val)) fromend=TRUE;
  if (xlgetkeyarg(k_count, &val)) {
    if (!null(val)) {
      if (!fixp(val)) xlbadtype(val);
      count = getfixnum(val);
      if (count < 0) count = 0;
    }
  }
  if (count <= 0) fromend=FALSE; /* save some time */

  kfcn = xlkey();

  xllastkey();

  if (null(seq)) return NIL;

  /* protect some pointers */
  xlstkcheck(4);
  xlsave1(val);
  xlprotect(seq);
  xlprotect(kfcn);
  xlprotect(fcn);

  /* delete matches */
  switch (ntype(seq)) {
  case CONS:
    if (! destruct) seq = copylist(seq);
    if (fromend) {
      unsigned temp;
      seq = xlnreverse(seq);
      l = getlength(seq);
      temp = l - end;
      end = l - start;
      start = temp;
    }
    end -= start;

    /* delete leading matches, only if start is 0 */
    if (start == 0) {
      while (consp(seq) && end > 0) {
	end--;
	if (count == 0 || ! remtest(x,car(seq),fcn,kfcn))
	  break;
	seq = cdr(seq);
	count--;
      }
    }
    val = last = seq;

    /* delete embedded matches */
    if (consp(seq) && end > 0) {

      /* skip the first non-matching element, start == 0 */
      if (start == 0) seq = cdr(seq);

      /* skip first elements if start > 0, correct "last" */
      for (; consp(seq) && start-- > 0; last = seq, seq = cdr(seq));

      /* look for embedded matches */
      for (; consp(seq) && end-- > 0; seq = cdr(seq)) {

	/* check to see if this element should be deleted */
	if (count != 0) {
	  if (remtest(x,car(seq),fcn,kfcn)) {
	    rplacd(last,cdr(seq));
	    count--;
	  }
	  else last = seq;
	}
      }
    }
    if (fromend) val = xlnreverse(val);
    break;
  case VECTOR:
  case STRING:
  case TVEC:
    l = gettvecsize(seq);
    esize = gettveceltsize(seq);
    if (end > l) end = l;
    if (! destruct) seq = copyvector(seq);
    xlsave1(elt);  /* just to be safe for typed arrays -- prob. not needed */
    if (fromend) {
      for (i = j = l - 1; i != -1; i--) {
	elt = gettvecelement(seq,i);
	if (i < start || i >= end || /* copy if out of range */
	    count == (FIXTYPE)(j-i) || ! remtest(x,elt,fcn,kfcn)) {
	  if (i != j) settvecelement(seq,j,elt);
	  j--;
	}
      }
      if (++j != 0) {		/* need new, shorter result -- too bad */
	fcn = seq;		/* save value in protected cell */
	l -= j;			/* new length */
	seq = mktvec(l, gettvecetype(seq));
	MEMCPY(gettvecdata(seq), ((char *) gettvecdata(fcn)) + j * esize,
	       l * esize);
      }
    }
    else {
      for (i = j = 0; i < l; i++) {
	elt = gettvecelement(seq, i);
	if (i < start || i >= end || /* copy if out of range */
	    count == (FIXTYPE)(i-j) || ! remtest(x,elt,fcn,kfcn)) {
	  if (i != j) settvecelement(seq,j,elt);
	  j++;
	}
      }
      if (l != j) {		/* need new, shorter result -- too bad */
	fcn = seq;		/* save value in protected cell */
	seq = mktvec(j, gettvecetype(seq));
	MEMCPY(gettvecdata(seq), gettvecdata(fcn), j * esize);
      }
    }
    xlpop();
    val = seq;
    break;
  default:
    xlbadtype(seq); break;
  }

  /* restore the stack */
  xlpopn(4);

  /* return the updated sequence */
  return (val);
}

/* xremif - built-in function 'remove-if' -- enhanced version */
LVAL xremif(V)
{
  return (remif(TRUE,FALSE,FALSE));
}

/* xremifnot - built-in function 'remove-if-not' -- enhanced version */
LVAL xremifnot(V)
{
  return (remif(FALSE,FALSE,FALSE));
}

/* xremove - built-in function 'remove' -- enhanced version */

LVAL xremove(V)
{
  return (remif(TRUE,TRUE,FALSE));
}

/* xdelif - built-in function 'delete-if' -- enhanced version */
LVAL xdelif(V)
{
  return (remif(TRUE,FALSE,TRUE));
}

/* xdelifnot - built-in function 'delete-if-not' -- enhanced version */
LVAL xdelifnot(V)
{
  return (remif(FALSE,FALSE,TRUE));
}

/* xdelete - built-in function 'delete' -- enhanced version */

LVAL xdelete(V)
{
  return (remif(TRUE,TRUE,TRUE));
}

#ifdef SUBSTITUTE
#define subtest(x,y,fcn,kfcn) \
  ((expr?dotest2(x,y,fcn,kfcn):dotest1(y,fcn,kfcn)) == tresult)
/* substituteif - common code for 'substitute*' functions */
LOCAL LVAL substituteif P3C(int, tresult, int, expr, int, destruct)
{
  LVAL x,seq,fcn,val,next,repl;
  long i,l;
  unsigned start,end;
  LVAL kfcn;
  int fromend=FALSE;		/* process from the end */
  FIXTYPE count=-1;

  repl = xlgetarg();		/* replacement expression */

  if (expr) {
    /* get the expression to substitute and the sequence */
    x = xlgetarg();
    seq = xlgaseq();
    xltest(&fcn,&tresult);
  }
  else {
    /* get the function and the sequence */
    fcn = xlgetarg();
    seq = xlgaseq();
    x = NIL;                    /* to keep compiler happy */
  }

  getseqbounds(&start,&end,getlength(seq),k_start,k_end);

  if (xlgetkeyarg(k_fromend, &val) && !null(val)) fromend=TRUE;
  if (xlgetkeyarg(k_count, &val)) {
    if (!null(val)) {
      if (!fixp(val)) xlbadtype(val);
      count = getfixnum(val);
      if (count < 0) count = 0;
    }
  }

  kfcn = xlkey();

  xllastkey();

  if (null(seq) || count == 0) return seq;

  /* protect some pointers */
  xlstkcheck(4);
  xlprotect(seq);
  xlprotect(kfcn);
  xlprotect(fcn);
  xlsave(val);

  /* substitute matches */
  switch (ntype(seq)) {
  case CONS:
    if (destruct)
      val = seq;
    else
      seq = val = copylist(seq);

    if (fromend) {
      unsigned temp;
      seq = val = xlnreverse(seq);
      l = getlength(seq);
      temp = l - end;
      end = l - start;
      start = temp;
    }
    end -= start;

    /* skip first elements if start > 0 */
    for (; consp(seq) && start-- > 0; seq = cdr(seq));

    /* look for embedded matches */
    for (; consp(seq) && end-- > 0 && count != 0; seq = cdr(seq)) {
      /* check to see if this element should be replaced */
      if (subtest(x,car(seq),fcn,kfcn)) {
	rplaca(seq, repl);
	count--;
      }
    }
    if (fromend) val = xlnreverse(val);
    break;
  case VECTOR:
  case STRING:
  case TVEC:
    l = gettvecsize(seq);
    if (end > l) end = l;
    val = destruct ? seq : copyvector(seq);
    xlsave1(next);
    i = fromend ? end : ((int) start) - 1;
    while ((count != 0) && (fromend ? i-- > start : ++i < end)) {
      next = gettvecelement(val, i);
      if (subtest(x,next,fcn,kfcn)) {
	settvecelement(val, i, repl);
	count--;
      }
    }
    xlpop();
    break;
  default:
    xlbadtype(seq); break;
  }

  /* restore the stack */
  xlpopn(4);

  /* return the updated sequence */
  return (val);
}

/* xsubstituteif - built-in function 'substitute-if' -- enhanced version */
LVAL xsubstituteif(V)
{
  return (substituteif(TRUE,FALSE,FALSE));
}

/* xsubstituteifnot - built-in function 'substitute-if-not' -- enhanced version */
LVAL xsubstituteifnot(V)
{
  return (substituteif(FALSE,FALSE,FALSE));
}

/* xsubstitute - built-in function 'substitute' -- enhanced version */

LVAL xsubstitute(V)
{
    return (substituteif(TRUE,TRUE,FALSE));
}

/* xnsubstituteif - built-in function 'nsubstitute-if' -- enhanced version */
LVAL xnsubstituteif(V)
{
  return (substituteif(TRUE,FALSE,TRUE));
}

/* xnsubstituteifnot - built-in function 'nsubstitute-if-not' -- enhanced version */
LVAL xnsubstituteifnot(V)
{
  return (substituteif(FALSE,FALSE,TRUE));
}

/* xnsubstitute - built-in function 'nsubstitute' -- enhanced version */
LVAL xnsubstitute(V)
{
  return (substituteif(TRUE,TRUE,TRUE));
}
#endif

#ifdef POSFCNS
/* TAA MOD -- This is a rewrite done 6/93 to incorporate missing variations */

#define CNTFCN 0    /* three different function types */
#define FNDFCN 1
#define POSFCN 2
    
/* This is the test expression for all cases */

#ifdef KEYARG
#define bigtest(i) ((expr?dotest2(x,i,fcn,kfcn):dotest1(i,fcn,kfcn)) == tresult)
#else
#define bigtest(i) ((expr?dotest2(x,i,fcn):dotest1(i,fcn)) == tresult)
#endif

/* count*, position*, and find* are all done by the following function */
LOCAL LVAL xlkitchensink P3C(int, ftype, int, tresult, int, expr)
{
  LVAL seq, fcn;          /* sequence and function */
  LVAL x;                 /* expression (when expr is TRUE) */
  unsigned start, end;    /* start and end positions */
  unsigned counter=0;     /* for CNTFCN */
  int count;              /* for POSFCN */
  LVAL val;               /* for FNDFCN */
  int fromend, i;

#ifdef KEYARG
  LVAL kfcn;
#endif

  if (expr) {
    x = xlgetarg();         /* get expression */
    seq = xlgaseq();        /* get sequence */
    xltest(&fcn, &tresult); /* get test function and invert from keyargs*/
  }
  else {
    fcn = xlgetarg();       /* get function */
    seq = xlgaseq();        /* get sequence */
    x = NIL;                /* to keep compiler happy */
  }

  getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef KEYARG
  kfcn = xlkey();             /* get :key keyword arg */
#endif

  if (xlgetkeyarg(k_fromend, &val) && !null(val))
    fromend = TRUE;
  else
    fromend = FALSE;

  xllastkey();

  if (null(seq))      /* nothing to do, return default result */
    return (ftype==CNTFCN ? cvfixnum((FIXTYPE)0) : NIL);

#ifdef KEYARG
  xlstkcheck(4);
  xlprotect(kfcn);
#else
  xlstkcheck(3);
#endif
  xlprotect(fcn);
  xlprotect(seq);
  xlsave(val);

  count = (fromend) ? end - 1 : start;

  /* examine arg and count */
  switch (ntype(seq)) {
  case CONS:
    end -= start;
    for (; consp(seq) && start-- > 0; seq = cdr(seq)) ;
    if (fromend) {
      val = seq;
      for (seq = NIL, i = 0; i < end; i++, val = cdr(val))
	seq = cons(car(val), seq);
    }
    for (; end-- > 0; seq = cdr(seq)) {
      val = car(seq);
      if (bigtest(val)) {
	if (ftype==CNTFCN) counter++;
	else goto fin;
      }
      if (fromend) count--;
      else count++;
    }
    break;
  case VECTOR:
  case STRING:
  case TVEC:
    if (fromend) {
      for (; count >= (int) start; count--) {
	val = gettvecelement(seq, count);
	if (bigtest(val)) {
	  if (ftype==CNTFCN) counter++;
	  else goto fin;
	}
      }
    }
    else {
      for (; count < end; count++) {
	val = gettvecelement(seq, count);
	if (bigtest(val)) {
	  if (ftype==CNTFCN) counter++;
	  else goto fin;
	}
      }
    }
    break;
  default:
    xlbadtype(seq); break;
  }

#ifdef KEYARG
  xlpopn(4);
#else
  xlpopn(3);
#endif

  return (ftype==CNTFCN ? cvfixnum((FIXTYPE)counter) : NIL);

 fin:
#ifdef KEYARG
  xlpopn(4);
#else
  xlpopn(3);
#endif

  return (ftype==POSFCN ? cvfixnum((FIXTYPE)count) : val);
}


/* nine different functions are done by xlkitchensink */
LVAL xcount(V) {             /* count */
  return(xlkitchensink(CNTFCN,TRUE,TRUE));
}
LVAL xcountif(V) {           /* count-if */
  return(xlkitchensink(CNTFCN,TRUE,FALSE));
}
LVAL xcountifnot(V) {        /* count-if-not */
  return(xlkitchensink(CNTFCN,FALSE,FALSE));
}
LVAL xposition(V) {          /* position */
  return(xlkitchensink(POSFCN,TRUE,TRUE));
}
LVAL xpositionif(V) {        /* position-if */
  return(xlkitchensink(POSFCN,TRUE,FALSE));
}
LVAL xpositionifnot(V) {     /* position-if-not */
  return(xlkitchensink(POSFCN,FALSE,FALSE));
}
LVAL xfind(V) {              /* find */
  return(xlkitchensink(FNDFCN,TRUE,TRUE));
}
LVAL xfindif(V) {            /* find-if */
  return(xlkitchensink(FNDFCN,TRUE,FALSE));
}
LVAL xfindifnot(V) {         /* find-if-not */
  return(xlkitchensink(FNDFCN,FALSE,FALSE));
}
#endif

#ifdef SRCHFCN
/* xsearch -- search function */
LVAL xsearch(V)
{
  LVAL seq1, seq2, fcn, temp1, temp2, elt1, elt2;
  unsigned start1, start2, end1, end2, len1, len2;
  unsigned i,j;
  int tresult,typ1, typ2;
#ifdef KEYARG
  LVAL kfcn;
#endif

  /* get the sequences */
  seq1 = xlgaseq();
  len1 = getlength(seq1);
  seq2 = xlgaseq();
  len2 = getlength(seq2);

  /* test/test-not args? */
  xltest(&fcn,&tresult);

  /* check for start/end keys */
  getseqbounds(&start1,&end1,len1,k_1start,k_1end);
  getseqbounds(&start2,&end2,len2,k_2start,k_2end);
    
#ifdef KEYARG
  kfcn = xlkey();
#endif

  xllastkey();

  /* calculate the true final search string location that needs to
     be checked (end2) */

  if (end2 - start2 < end1 - start1       /* nothing to compare */
      || end2 - start2 == 0) 
    return (NIL); 

  len1 = end1 - start1;   /* calc lengths of sequences to test */
  end2 -= len1;           /* we don't need to compare with start loc
			     beyond this value */

  typ1 = ntype(seq1);
  typ2 = ntype(seq2);
    
#ifdef KEYARG
  xlstkcheck(4);
  xlprotect(kfcn);
#else
  xlstkcheck(3);
#endif
  xlprotect(fcn);
  xlsave(elt1);
  xlsave(elt2);

  if (typ1 == CONS) { /* skip leading section of sequence 1 if a cons */
    j = start1;
    while (j--) seq1 = cdr(seq1);
  }

  if (typ2 == CONS) { /* second string is cons */
    i = start2;     /* skip leading section of string 2 */
    while (start2--) seq2 = cdr(seq2);

    for (;i<=end2;i++) {
      temp2 = seq2;
      if (typ1 == CONS) {
	temp1 = seq1;
	for (j = start1; j < end1; j++) {
#ifdef KEYARG
	  if (dotest2s(car(temp1),car(temp2),fcn,kfcn) != tresult)
	    goto next1;
#else
	  if (dotest2(car(temp1),car(temp2),fcn) != tresult)
	    goto next1;
#endif
	  temp1 = cdr(temp1);
	  temp2 = cdr(temp2);
	}
      }
      else {
	for (j = start1; j < end1; j++) {
	  elt1 = gettvecelement(seq1, j);
#ifdef KEYARG
	  if (dotest2s(elt1, car(temp2), fcn, kfcn) !=tresult)
#else
	  if (dotest2(elt1, car(temp2), fcn)!=tresult)
#endif
	      goto next1;
	  temp2 = cdr(temp2);
	}
      }
#ifdef KEYARG
      xlpopn(4);
#else
      xlpopn(3);
#endif
      return cvfixnum(i);
    next1: /* continue */
      seq2 = cdr(seq2);
    }
  }
  else
    for (i = start2; i <= end2 ; i++) { /* second string is array/string */
      if (typ1 == CONS) { 
	temp1 = seq1;
	for (j = 0; j < len1; j++) {
	  elt2 = gettvecelement(seq2, i+j);
#ifdef KEYARG
	  if (dotest2s(car(temp1), elt2, fcn,kfcn) != tresult)
#else
	  if (dotest2(car(temp1), elt2, fcn) != tresult)
#endif
	    goto next2;
	  temp1 = cdr(temp1);
	}
      }
      else
	for (j=start1; j < end1; j++) {
	  elt1 = gettvecelement(seq1,j);
	  elt2 = gettvecelement(seq2,i+j-start1);
#ifdef KEYARG
	  if (dotest2s(elt1, elt2, fcn, kfcn) != tresult)
#else
	  if (dotest2(elt1, elt2, fcn) != tresult)
#endif
	    goto next2;
	}
#ifdef KEYARG
      xlpopn(4);
#else
      xlpopn(3);
#endif
      return cvfixnum(i);
    next2:; /* continue */
    }

#ifdef KEYARG
  xlpopn(4);
#else
  xlpopn(3);
#endif

  return (NIL);   /*no match*/
}
#endif

/* The following is based on code with the following copyright message: */
/* XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney                  */
/*      All Rights Reserved                                            */
/*      Permission is granted for unrestricted non-commercial use      */

/* Extended by Tom Almy to put in a single C function, allow :start and 
   :end keywords, correctly  handle case of null(seq), and case where
   sequence is a string */

/* Common Lisp REDUCE function */
LVAL xreduce(V)
{
  LVAL fcn, seq, initial_value;
  LVAL next, args, result;
  int has_init;
  unsigned start, end;

  fcn = xlgetarg();
  seq = xlgaseq();
  has_init = xlgetkeyarg(k_ivalue, &initial_value);
  getseqbounds(&start, &end, getlength(seq), k_start, k_end);
  xllastkey();

  /* protect some pointers */
  xlstkcheck(4);
  xlsave(next);
  xlsave(args);
  xlsave(result);
  xlprotect(fcn);

  args = cons(NIL, cons(NIL,NIL));

  if (null(seq) || start==end) {
    result = has_init ? initial_value : xlapply(pushargs(fcn, NIL));
  }
  else switch (ntype(seq)) {
  case CONS:
    end -= start;
    while (start-- > 0) seq = cdr(seq); /* skip to start */
    next = seq;
    if (has_init) result = initial_value;
    else {
      result = car(next);
      next = cdr(next);
      end--;
    }
    for (; end-- > 0; next = cdr(next)) {
      rplaca(args, result);
      rplaca(cdr(args), car(next));
      result = xlapply(pushargs(fcn, args));
    }
    break;
  case VECTOR:
  case STRING:
  case TVEC:
    if (has_init) 
      result = initial_value;
    else {
      result = gettvecelement(seq, start);
      start++;
    }
    for (; start < end; start++) {
      rplaca(args, result);
      rplaca(cdr(args), gettvecelement(seq, start));
      result = xlapply(pushargs(fcn, args));
    }
    break;
  default:
    xlbadtype(seq);
  }

  /* restore the stack frame */
  xlpopn(4);
  
  return(result);
}

/* Common Lisp REMOVE-DUPLICATES function */
/* by Tom Almy */
/* unlike xllist.c version, this one works on all sequences and 
   allows the :start and :end keywords. */

LVAL xremove_duplicates(V)
{
  LVAL seq,fcn,val,next,tmp,item;
  LVAL last=NULL,vstart=NIL;
  unsigned i,j,l,k;
  unsigned start,end,esize;
  int tresult, fromend;

#ifdef KEYARG
  LVAL kfcn;
#endif

  /* get the sequence */
  seq = xlgaseq();

  /* get any optional args */
  xltest(&fcn,&tresult);

  getseqbounds(&start,&end,getlength(seq),k_start,k_end);
    
#ifdef KEYARG
  kfcn = xlkey();
#endif

  if (xlgetkeyarg(k_fromend, &tmp) && !null(tmp))
    fromend = TRUE;
  else
    fromend = FALSE;

  xllastkey();

  if (null(seq)) return NIL;

  /* protect some pointers */
#ifdef KEYARG
  xlstkcheck(5);
  xlprotect(kfcn);
#else
  xlstkcheck(4);
#endif
  xlsave(item);
  xlsave(tmp);
  xlprotect(fcn);
  xlsave(val);

  /* remove matches */
  switch (ntype(seq)) {
  case CONS:
    end -= start;   /* length of valid subsequence */
    while (start-- > 0) {   /* copy leading part intact */
      next = consa(car(seq));
      if (!null(val)) rplacd(last,next);
      else val=next;
      last= next;
    }

    if (fromend) {
      for (; end-- > 0; seq = cdr(seq)) {
	/* check to see if this element should be deleted */
	item = car(seq);
#ifdef KEYARG
	if (!null(kfcn)) item = xlapp1(kfcn,item);
	for (tmp=vstart; consp(tmp); tmp = cdr(tmp))
	  if (dotest2(item,car(tmp),fcn,kfcn)==tresult)
	    goto cons_noxfer_fromend;
#else
	for (tmp=vstart; consp(tmp); tmp = cdr(tmp))
	  if (dotest2(item,car(tmp),fcn)==tresult)
	    goto cons_noxfer_fromend;
#endif              
	next = consa(car(seq));
	if (!null(val)) rplacd(last,next);
	else val = next;
	last = next;
	if (null(vstart)) vstart = next;
      cons_noxfer_fromend:;
      }
    }
    else {
      for (; end-- > 1; seq = cdr(seq)) {
	/* check to see if this element should be deleted */
	item = car(seq);
#ifdef KEYARG
	if (!null(kfcn)) item = xlapp1(kfcn,item);
	for (l=end,tmp=cdr(seq); l-- >0; tmp = cdr(tmp))
	  if (dotest2(item,car(tmp),fcn,kfcn)==tresult)
	    goto cons_noxfer;
#else
	for (l=end,tmp=cdr(seq); l-- >0; tmp = cdr(tmp))
	  if (dotest2(item,car(tmp),fcn)==tresult)
	    goto cons_noxfer;
#endif              
	next = consa(car(seq));
	if (!null(val)) rplacd(last,next);
	else val = next;
	last = next;
      cons_noxfer:;
      }
    }
    /* now copy to end */
    while (consp(seq)) {
      next = consa(car(seq));
      if (!null(val)) rplacd(last,next);
      else val = next;
      last = next;
      seq = cdr(seq);
    }
    break;
  case VECTOR:
  case STRING:
  case TVEC:
    val = mktvec(l=gettvecsize(seq), gettvecetype(seq));
    esize = gettveceltsize(seq);

    if (start>0)
      MEMCPY(gettvecdata(val), gettvecdata(seq),start * esize);

    for (i=j=start; i < end; i++) {
      item = gettvecelement(seq,i);
#ifdef KEYARG
      if (!null(kfcn)) item = xlapp1(kfcn,item);
      if (fromend) {
	for (k=start; k<j; k++) {
	  tmp = gettvecelement(seq,k);
	  if (dotest2(item,tmp,fcn,kfcn)==tresult)
	    goto vector_noxfer;
	}
      }
      else {
	for (k=i+1; k<end; k++) {
	  tmp = gettvecelement(seq,k);
	  if (dotest2(item,tmp,fcn,kfcn)==tresult)
	    goto vector_noxfer;
	}
      }
#else
      if (fromend) {
	for (k=start; k<j; k++) {
	  tmp = gettvecelement(seq,k);
	  if (dotest2(item,tmp,fcn)==tresult)
	    goto vector_noxfer;
	}
      }
      else {
	for (k=i+1; k<end; k++) {
	  tmp = gettvecelement(seq,k);
	  if (dotest2(item,tmp,fcn)==tresult)
	    goto vector_noxfer;
	}
      }
#endif
      settvecelement(val,j++,item);
    vector_noxfer:;
    }

    if (l-end > 0) { /* elements at end to copy */
      MEMCPY(((char *) gettvecdata(val)) + j * esize,
	     ((char *) gettvecdata(seq)) + end * esize,
	     (l - end) * esize);
      j += l - end;
    }

    if (l != j) { /* need new, shorter result -- too bad */
      fcn = val; /* save value in protected cell */
      val = mktvec(j, gettvecetype(seq));
      MEMCPY(gettvecdata(val), gettvecdata(fcn), j * esize);
    }
    break;
  default:
    xlbadtype(seq); break;
  }

  /* restore the stack */
#ifdef KEYARG
  xlpopn(5);
#else
  xlpopn(4);
#endif

  /* return the updated sequence */
  return (val);
}

/* Common Lisp COPY-SEQ function */
LVAL xcopyseq(V)
{
  LVAL x;

  x = xlgaseq();
  xllastarg();

  if (listp(x)) return(copylist(x));
  else return(copyvector(x));
}

LVAL xreplace(V)
{
  LVAL seq1, seq2;
  unsigned start1, start2, end1, end2, len1, len2;

  /* get the sequences */
  seq1 = xlgaseq();  
  len1 = getlength(seq1);
  seq2 = xlgaseq();
  len2 = getlength(seq2);

  /* check for start/end keys */
  getseqbounds(&start1,&end1,len1,k_1start,k_1end);
  getseqbounds(&start2,&end2,len2,k_2start,k_2end);

  xlreplace(seq1, seq2, start1, end1, start2, end2);

  return seq1;
}
