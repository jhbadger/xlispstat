/* xldmem - xlisp dynamic memory management routines */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

typedef LVAL (*subrfun)(V);

static LVAL finalize_registered, finalize_pending;
static VOID check_finalize(V);
static VOID do_finalize(V);
static VOID check_weak_boxes(V);

#ifdef NEWGC

/* gc tuning parameters */
/**** these should probably be in special variables */
#define ALLOC_THRESHOLD_INCREMENT 0x20000L
#define GC_EXPAND_FRAC 0.2
#define FULL_GC_FRAC 0.85

static double gc_expand_frac = GC_EXPAND_FRAC;
static double full_gc_frac = FULL_GC_FRAC;

/* position offsets for doubly linked list heads in segment */
#define IMMEDIATE_OFFSET 0
#define ALLOCATED_OFFSET 1
#define IMMEDIATE_TENURED_OFFSET 2
#define ALLOCATED_TENURED_OFFSET 3
#define IMMEDIATE_TO_NEW_OFFSET 4
#define ALLOCATED_TO_NEW_OFFSET 5
#define NUM_OFFSETS 6
#define TENURED_OFFSET(n) (type_info[(int)ntype(n)].tenured_offset)
#define TO_NEW_OFFSET(n)  (type_info[(int)ntype(n)].to_new_offset)

/* limits for segment size - max is 2^15 - 5 for 16-bit shorts */
#define MIN_ANODES 1
#define MAX_ANODES ((1 << 15) - NUM_OFFSETS - 1)

/* macro to compute the size of a segment */
#define segsize(n) (sizeof(SEGMENT)+((n)-1)*sizeof(struct node))

#ifdef TIMES
static unsigned long gctime;    /* calcuate time in garbage collector */
#endif

/* macros to convert between size in bytes and vector size */
#define btow_size(n) (((unsigned)(n)+(sizeof(LVAL)-1))/(unsigned)sizeof(LVAL))
#define wtob_size(n) (((long) (n)) * sizeof(LVAL))

/* variables local to xldmem.c and xlimage.c */
SEGMENT *segs,*lastseg,*fixseg,*charseg,*lastpermseg;
int anodes,nsegs;

/* local variables */
static long minor_gccalls;
static long nperm,ntenured,alloc_threshold,threshold_crossing_gccount;
static SEGMENT *curseg;
static LVAL curbase, curtop;

/* structure for holding information about types */
struct {
  int has_children, allocated;
  int tenured_offset, to_new_offset;
} type_info[NUMTYPES];

/* macros using the structure */
#define node_has_children(node) (type_info[(int)ntype(node)].has_children)
#define type_is_allocated(type) (type_info[(int)type].allocated)
#define node_is_allocated(node) (type_info[(int)ntype(node)].allocated)

/* macros to check/set whether a node is in new or old space */
static int old_code = 0;
#define is_old_node(x) (ngcflag1(x)==old_code)
#define is_new_node(x) (ngcflag1(x)!=old_code)
#define set_old_node(x) (old_code ? setngcflag1(x) : unsetngcflag1(x))
#define set_new_node(x) (old_code ? unsetngcflag1(x) : setngcflag1(x))

/* macros to check/set whether a node is in the to new list */
#define is_to_new_node(x) (ngcflag2(x))
#define set_to_new_node(x) (setngcflag2(x))
#define unset_to_new_node(x) (unsetngcflag2(x))

/* macros to access and set the offsets of a node */
#define NEXT_NODE_OFFSET(x) ((x)->n_gc.n_gc_offsets.forward)
#define SET_NEXT_NODE_OFFSET(x,p) ((x)->n_gc.n_gc_offsets.forward = (p))
#define LAST_NODE_OFFSET(x) ((x)->n_gc.n_gc_offsets.backward)
#define SET_LAST_NODE_OFFSET(x,p) ((x)->n_gc.n_gc_offsets.backward = (p))
#define NODE_BASE_OFFSET(x) ((x)->n_base_offset)
#define SET_NODE_BASE_OFFSET(x,p) ((x)->n_base_offset = (p))

/* macros to access and set the forwarding pointer of a node */
#define NEXT_NODE_PTR(x) ((x)->n_gc.n_next)
#define SET_NEXT_NODE_PTR(x,p) ((x)->n_gc.n_next = (p))

/* macros to access and set the base (first node in seg.) pointer of a node */
#define NODE_BASE(x) ((x) - NODE_BASE_OFFSET(x))
#define SET_NODE_BASE(x, p) SET_NODE_BASE_OFFSET(x,(x) - (p))

/* macros to access and set linked list pointers */
#define NEXT_NODE(x) (NODE_BASE(x) + NEXT_NODE_OFFSET(x))
#define SET_NEXT_NODE(x,p) SET_NEXT_NODE_OFFSET(x,(p)-NODE_BASE(x))
#define LAST_NODE(x) (NODE_BASE(x) + LAST_NODE_OFFSET(x))
#define SET_LAST_NODE(x,p) SET_LAST_NODE_OFFSET(x,(p)-NODE_BASE(x))

/* macro to unlink a node from its current linked list */
#define UNLINK_NODE(x) {\
  register LVAL __x, base; \
  register  short lnox, nnox; \
  __x = (x); base = NODE_BASE(__x); \
  lnox = LAST_NODE_OFFSET(__x); nnox = NEXT_NODE_OFFSET(__x); \
  SET_NEXT_NODE_OFFSET(base + lnox,nnox); \
  SET_LAST_NODE_OFFSET(base + nnox,lnox); \
}

/* macro to link a node x after a node p in a linked list in the same seg. */
#define LINK_NODE(x,p) {\
  register LVAL __x, __p; \
  register short ox, op, nnop; \
  __x = (x); __p = (p); \
  nnop = NEXT_NODE_OFFSET(__p); \
  ox = NODE_BASE_OFFSET(__x); op = NODE_BASE_OFFSET(__p);\
  SET_NEXT_NODE_OFFSET(__p,ox); \
  SET_LAST_NODE_OFFSET(__x,op); \
  SET_NEXT_NODE_OFFSET(__x,nnop); \
  SET_LAST_NODE_OFFSET(NODE_BASE(__x) + nnop,ox); \
}

/* macro to link a node to the base with ffset b */
#define LINK_NODE_TO_BASE(x,b) LINK_NODE(x,NODE_BASE(x)+(b))

/* macro to move a node from one linked list to another in its segment */
#define MOVE_NODE(x,p) { UNLINK_NODE(x); LINK_NODE(x,p); }

/* macro to move a node to the list with base offset b */
#define MOVE_NODE_TO_BASE(x,b) MOVE_NODE(x,NODE_BASE(x)+(b))

/* macro to move all the nodes in a list except the base to another list */
#define MOVE_LIST(a,b) {\
  register LVAL nna, lna, nnb; \
  nna = NEXT_NODE(a); lna = LAST_NODE(a); nnb = NEXT_NODE(b); \
  if (a != nna) { \
    SET_NEXT_NODE(a,a); \
    SET_LAST_NODE(a,a); \
    SET_NEXT_NODE(b,nna); \
    SET_LAST_NODE(nna,b); \
    SET_NEXT_NODE(lna,nnb); \
    SET_LAST_NODE(nnb,lna); \
  } \
}

/* macro to find the next free node; also advances the pointer */
#define NEXT_FREE_NODE() \
  (curtop = curbase + NEXT_NODE_OFFSET(curtop), \
   (curtop == curbase) ? NIL : curtop)

/* macro to handle creating references from old to new space */
/* This assumes NIL is marked as 'old' and 'to_new' */
#define check_old_to_new(x,y) { \
  if (is_old_node(x) && is_new_node(y)) { \
    if (! is_to_new_node(x)) { \
      MOVE_NODE_TO_BASE(x, TO_NEW_OFFSET(x)); \
      set_to_new_node(x); \
    } \
  } \
}

/* forward declarations */
LOCAL LVAL newnode P1H(int);
LOCAL LVAL allocvector P2H(int, unsigned);
LOCAL int  addseg(V);
LOCAL VOID stats(V);
LOCAL VOID check_alloc_threshold P1H(long);
LOCAL VOID adjust_alloc_threshold(V);
LOCAL VOID expand_node_space(V);
LOCAL VOID morevmem P1H(int);
LOCAL VOID ggc P1H(int);
LOCAL VOID clean_free_allocated_node P1H(LVAL);
LOCAL char *IViewNewAData P4H(int, int, long *, int);

/* xlminit - initialize the dynamic memory module */
VOID xlminit(V)
{
    LVAL p;
    int i;

    /* initialize our internal variables */
    segs = lastseg = curseg = lastpermseg = NULL;
    nnodes = nfree = total = gccalls = minor_gccalls = nperm = ntenured = 0L;
    alloc_threshold = ALLOC_THRESHOLD_INCREMENT;
    threshold_crossing_gccount = 0L;
    nsegs = 0;
    anodes = NNODES;
    gctime = 0L;

    for (i = 0; i < NUMTYPES; i++) {
      type_info[i].has_children = FALSE;
      type_info[i].allocated = TRUE;
    }
    type_info[FREE].allocated = FALSE;
    type_info[SUBR].allocated = FALSE;
    type_info[FSUBR].allocated = FALSE;
    type_info[CONS].allocated = FALSE;
    type_info[CONS].has_children = TRUE;
    type_info[SYMBOL].has_children = TRUE;
    type_info[FIXNUM].allocated = FALSE;
    type_info[FLONUM].allocated = FALSE;
#ifdef BIGNUMS
    type_info[RATIO].allocated = FALSE;
    type_info[RATIO].has_children = TRUE;
#endif
    type_info[OBJECT].has_children = TRUE;
    type_info[VECTOR].has_children = TRUE;
    type_info[CLOSURE].has_children = TRUE;
    type_info[CHAR].allocated = FALSE;
    type_info[USTREAM].allocated = FALSE;
    type_info[USTREAM].has_children = TRUE;
    type_info[COMPLEX].allocated = FALSE;
    type_info[COMPLEX].has_children = TRUE;
    type_info[RNDSTATE].allocated = FALSE;
    type_info[RNDSTATE].has_children = TRUE;
    type_info[NATPTR].allocated = FALSE;
    type_info[NATPTR].has_children = TRUE;
    type_info[WEAKBOX].allocated = FALSE;
    type_info[WEAKBOX].has_children = FALSE;
    type_info[DARRAY].allocated = FALSE;
    type_info[DARRAY].has_children = TRUE;
    type_info[STRUCT].has_children = TRUE;
#ifdef BYTECODE
    type_info[BCCLOSURE].allocated = FALSE;
    type_info[BCCLOSURE].has_children = TRUE;
    type_info[CPSNODE].has_children = TRUE;
    type_info[BCODE].has_children = TRUE;
#endif /*BYTECODE */
#ifdef PACKAGES
    type_info[PACKAGE].has_children = TRUE;
#endif /* PACKAGES */
    for (i = 0; i < NUMTYPES; i++) {
      if (type_info[i].allocated) {
	type_info[i].tenured_offset = ALLOCATED_TENURED_OFFSET;
	type_info[i].to_new_offset = ALLOCATED_TO_NEW_OFFSET;
      }
      else {
	type_info[i].tenured_offset = IMMEDIATE_TENURED_OFFSET;
	type_info[i].to_new_offset = IMMEDIATE_TO_NEW_OFFSET;
      }
    }

    /* allocate the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
	xlfatal("insufficient memory");

    /* initialize the fixnum segment */
    p = &fixseg->sg_nodes[0] + NUM_OFFSETS;
    for (i = SFIXMIN; i <= SFIXMAX; ++i) {
	setntype(p, FIXNUM);
	p->n_fixnum = i;
	set_old_node(p);
	++p;
	nperm++;
    }

    /* allocate the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
	xlfatal("insufficient memory");

    /* initialize the character segment */
    p = &charseg->sg_nodes[0] + NUM_OFFSETS;
    for (i = CHARMIN; i <= CHARMAX; ++i) {
	setntype(p, CHAR);
	p->n_chcode = i;
	set_old_node(p);
	++p;
	nperm++;
    }

    /* set pointer to last permanent segment */
    lastpermseg = lastseg;

    /* allocate an initial segment */
    if (! addseg())
	xlfatal("insufficient memory");
    curseg = lastpermseg->sg_next;
    curtop = curbase = &curseg->sg_nodes[0];
    
    /* initialize structures that are marked by the collector */
    obarray = NULL;
    xlenv = xlfenv = xldenv = NIL;
    s_gcflag = s_gchook = NULL;

    /* allocate the evaluation stack */
    if ((xlstkbase = (LVAL **)malloc(EDEPTH * sizeof(LVAL *))) == NULL)
	xlfatal("insufficient memory");
    xlstack = xlstktop = xlstkbase + EDEPTH;

    /* allocate the argument stack */
    if ((xlargstkbase = (LVAL *)malloc(ADEPTH * sizeof(LVAL))) == NULL)
	xlfatal("insufficient memory");
    xlargstktop = xlargstkbase + ADEPTH;
    xlfp = xlsp = xlargstkbase;
    *xlsp++ = NIL;

#ifdef MULVALS
    /* allocate the result array */
    if ((xlresults = (LVAL *)malloc(MULVALLIMIT * sizeof(LVAL))) == NULL)
	xlfatal("insufficient memory");
    for (i = 0; i < MULVALLIMIT; i++)
      xlresults[i] = NIL;
#endif /* MULVALS */

    /* we have to make a NIL symbol before continuing */
    /**** this is a real hack */
    p = xlmakesym("NIL");
    MEMCPY(NIL, p, sizeof(struct node));    /* we point to this! */
    defconstant(NIL, NIL);
    set_old_node(NIL);
    set_to_new_node(NIL);
    setfunction(NIL, NIL);
    setntype(p, FREE);                      /* don't collect "garbage" */

    finalize_registered = NIL;
    finalize_pending = NIL;
}

/* The next three functions are function versions of rplaca, rplacd and */
/* setelement that check for creating references from old to new space. */
/**** all other files need to be checked carefully to see where the     */
/**** macro versions Rplaca, Rplacd and Setelement can be used safely   */

LVAL rplaca P2C(LVAL, x, LVAL, y) { check_old_to_new(x,y); return Rplaca(x,y); }
LVAL rplacd P2C(LVAL, x, LVAL, y) { check_old_to_new(x,y); return Rplacd(x,y); }

LVAL setelement P3C(LVAL, x, int, i, LVAL, v)
{
  check_old_to_new(x,v);
  return Setelement(x,i,v);
}

/* cons - construct a new cons node */
LVAL cons P2C(LVAL, x, LVAL, y)
{
    LVAL nnode;

    /* get a free node */
    if ((nnode = NEXT_FREE_NODE()) == NIL) {
      curtop = LAST_NODE(curtop); /* back up the curtop pointer */
      xlstkcheck(2);
      xlprotect(x);
      xlprotect(y);
      nnode = newnode(CONS);
      xlpopn(2);
    }
    else {
      setntype(nnode, CONS);
      unset_to_new_node(nnode);
      nfree--;
    }

    /* initialize the new node */
    Rplaca(nnode,x);
    Rplacd(nnode,y);

    /* return the new node */
    return (nnode);
}

/* cvstring - convert a string to a string node */
LVAL cvstring P1C(char *, str)
{
    LVAL val;
    val = newstring(STRLEN(str));
    STRCPY(getstring(val),str);
    return (val);
}

/* newstring - allocate and initialize a new string */
LVAL newstring P1C(unsigned, size)
{
    LVAL val;
    val = allocvector(STRING,btow_size(size+1));
    val->n_strlen = size;
    return (val);
}

#ifdef BIGNUMS
/* newbignum - allocate a new bignum */
LVAL newbignum P1C(unsigned, size)
{
  /* size of the sign field not included in n_vsize */
  BIGNUMDATA *x;
  LVAL val;
  xlsave1(val);
  val = allocvector(BIGNUM,btow_size((size+1)*sizeof(BIGNUMDATA)));
  val->n_bsize = size;
  x = getbignumarray(val);
  size++;
  while (size--) *x++ = 0;	/* set value to zero */
  xlpop();
  return val;
}
#endif	

/* cvsymbol - convert a string to a symbol */
LVAL cvsymbol P1C(char *, pname)
{
    LVAL val;
    xlsave1(val);
    val = allocvector(SYMBOL,SYMSIZE);
    setvalue(val,s_unbound);
    setfunction(val,s_unbound);
    setpname(val,cvstring(pname));
    setsnormal(val); /* L. Tierney */
    xlpop();
    return (val);
}

/* cvsubr - convert a function to a subr or fsubr */
LVAL cvsubr P3C(subrfun, fcn, int, type, int, offset)
{
    LVAL val;
    val = newnode(type);
    val->n_subr = fcn;
    val->n_offset = offset;
    return (val);
}

/* cvfile - convert a file pointer to a stream */
LVAL cvfile P2C(FILEP, fp, int, iomode)
{
    LVAL val;
    val = newnode(STREAM);
    setfile(val,fp);
    setsavech(val,'\0');
    val->n_sflags = iomode;
    val->n_cpos = 0;
    return (val);
}

/* cvfixnum - convert an integer to a fixnum node */
LVAL cvfixnum P1C(FIXTYPE, n)
{
    LVAL val;
    if (n >= SFIXMIN && n <= SFIXMAX)
	return (&fixseg->sg_nodes[(int)n-SFIXMIN+NUM_OFFSETS]);
    if ((val = NEXT_FREE_NODE()) == NIL) {
      curtop = LAST_NODE(curtop); /* back up the curtop pointer */
      val = newnode(FIXNUM);
    }
    else {
      setntype(val, FIXNUM);
      nfree--;
    }
    val->n_fixnum = n;
    return (val);
}

/* cvflonum - convert a floating point number to a flonum node */
LVAL cvflonum P1C(FLOTYPE, n)
{
    LVAL val;
    if ((val = NEXT_FREE_NODE()) == NIL) {
      curtop = LAST_NODE(curtop); /* back up the curtop pointer */
      val = newnode(FLONUM);
    }
    else {
      setntype(val, FLONUM);
      nfree--;
    }
    val->n_flonum = n;
    return (val);
}

/* cvchar - convert an integer to a character node */
LVAL cvchar P1C(int, n)
{
    if (n >= CHARMIN && n <= CHARMAX)
	return (&charseg->sg_nodes[n-CHARMIN+NUM_OFFSETS]);
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return (NIL);   /* never executed but gets rid of warning message */
}

#ifdef BIGNUMS
/* cvbratio - convert a pair of bignums into a ratio node */
LVAL cvbratio P2C(LVAL, num, LVAL, denom)
{
  FIXTYPE nu, d;
  int fixtyped;
  LVAL n,m,r;

  if (cvtbigfixnum(num, &nu) && cvtbigfixnum(denom, &d))
    return cvratio(nu,d);
  xlstkcheck(5);
  xlprotect(num);
  xlprotect(denom);
  xlsave(n);
  xlsave(m);
  xlsave(r);

  if (zeropbignum(num)) {	/* zero is fixnum zero */
    xlpopn(5);
    return cvfixnum((FIXTYPE) 0);
  }
  if (getbignumsign(denom)) {	/* denominator must be positive */
    denom = copybignum(denom, 0);
    num = copybignum(num,!getbignumsign(num)); /* final sign */
  }
  n = copybignum(num, 0);	/* abs of numerator */
  m = denom;
  for (;;) {			/* get gcd */
    divbignum(m, n, &r);	/* use remainder only */
    if (zeropbignum(r)) break;
    m = n;
    n = r;
  }
  if ((!cvtbigfixnum(n, &d)) || d != 1) { /* can reduce */
    denom = divbignum(denom, n, &r);
    num = divbignum(num, n, &r);
  }
  if ((fixtyped = cvtbigfixnum(denom, &d)) != 0 && d == 1) {
    /* reduced to an integer */
    xlpopn(5);
    if (cvtbigfixnum(num, &nu)) return cvfixnum(nu);
    return num;
  }
  /* got value to return */
  r = newnode(RATIO);
  r->n_cdr = r->n_car = NIL; /* in case of garbage collect */
  r->n_cdr = (fixtyped ? cvfixnum(d) : denom);
  r->n_car = (cvtbigfixnum(num, &nu) ? cvfixnum(nu) : num);
  xlpopn(5);
  return (r);
}

/* cvratio - convert an integer pair to a ratio node */
LVAL cvratio P2C(FIXTYPE, num, FIXTYPE, denom)
{
  LVAL val;
  unsigned long n, m, r, nu, de;
  int sign;

  if (num == 0) return cvfixnum((FIXTYPE) 0); /* zero is int zero */
  if (denom < 0) {		/* denominator must be positive */
    if (denom == -1 && num == MINFIX) {
      xlsave1(val);
      val = cvtulongbignum((unsigned long)MAXFIX+1, FALSE);
      xlpop();
      return val;
    }
    denom = -denom;
    sign = num >= 0;
  }
  else
    sign = num < 0;
	
  if (num < 0) num = -num;
  n = nu = (unsigned long)(long)num;
  m = de = (unsigned long)(long)denom; /* reduce the ratio: compute GCD */
  for (;;) {
    if ((r = m % n) == 0) break;
    m = n;
    n = r;
  }
  if (n != 1) {
    de /= n;
    nu /= n;
  }
  if (de == 1)
    return cvfixnum(sign ? -(long)nu : (long)nu); /* reduced to integer */
  xlsave1(val);
  val = newnode(RATIO);
  val->n_cdr = val->n_car = NIL; /* in case of garbage collect */
  if ((nu == (unsigned long)MAXFIX+1 && sign==0))
    val->n_car = cvtulongbignum(nu, sign);
  else
    val->n_car = cvfixnum(sign ? -(long)nu : (long)nu);
  if (de == (unsigned long)MAXFIX+1)
    val->n_cdr = cvtulongbignum(de, FALSE);
  else
    val->n_cdr = cvfixnum(de);
  xlpop();
  return (val);
}
#endif

/* newustream - create a new unnamed stream */
LVAL newustream(V)
{
    LVAL val;
    val = newnode(USTREAM);
    sethead(val,NIL);
    settail(val,NIL);
    return (val);
}

/* newobject - allocate and initialize a new object */
LVAL newobject P2C(LVAL, cls, int, size)
{
    LVAL val;
    val = allocvector(OBJECT,size+1);
    setelement(val,0,cls);
    return (val);
}

/* newclosure - allocate and initialize a new closure */
LVAL newclosure P4C(LVAL, name, LVAL, type, LVAL, env, LVAL, fenv)
{
    LVAL val;
    val = allocvector(CLOSURE,CLOSIZE);
    setname(val,name);
    settype(val,type);
    setenvi(val,env);
    setfenv(val,fenv);
    return (val);
}

/* newstruct - allocate and initialize a new structure node */
LVAL newstruct P2C(LVAL, type, int, size)
{
    LVAL val;
    val = allocvector(STRUCT,size+1);
    setelement(val,0,type);
    return (val);
}

#ifdef BYTECODE
/* newcpsnode - allocate and initialize a new CPS node for compiler */
LVAL newcpsnode P1C(LVAL, type)
{
    LVAL val;
    val = allocvector(CPSNODE,CPSNODESIZE);
    setcpstype(val,type);
    return (val);
}

/* newbcode - allocate and initialize a new byte code vector */
LVAL newbcode P5C(LVAL, code, LVAL, jtab, LVAL, lits, LVAL, idx, LVAL, env)
{
    LVAL val;
    val = allocvector(BCODE,BCODESIZE);
    setbccode(val, code);
    setbcjtab(val, jtab);
    setbclits(val, lits);
    setbcidx(val, idx);
    setbcenv(val, env);
    return (val);
}
#endif /* BYTECODE */

#ifdef PACKAGES
/* newpackage - allocate and initialize a new package */
LVAL newpackage(V)
{
  LVAL val;
  xlsave1(val);
  val = allocvector(PACKAGE,PACKSIZE);
  setintsyms(val, newvector(HSIZE));
  setextsyms(val, newvector(HSIZE));
  xlpop();
  return (val);
}
#endif /* PACKAGES */

/* newvector - allocate and initialize a new vector node */
LVAL newvector P1C(unsigned, size)
{
  return(allocvector(VECTOR,size));
}

/* allocvector - allocate and initialize a new vector node */
LOCAL LVAL allocvector P2C(int, type, unsigned, size)
{
  LVAL vect, *p;
  int i;
  unsigned long bsize; /* changed to unsigned long - L. Tierney */

  xlsave1(vect);
  vect = newnode(type);
  vect->n_vsize = 0;
  if (size != 0) {
    bsize = wtob_size(size);
    check_alloc_threshold(bsize);
    if ((vect->n_vdata = (LVAL *)VALLOC(size)) == NULL) {
      ggc(FALSE);
      if ((vect->n_vdata = (LVAL *)VALLOC(size)) == NULL) {
	ggc(TRUE);
	if ((vect->n_vdata = (LVAL *)VALLOC(size)) == NULL)
	  xlabort("insufficient vector space");
      }
    }
    vect->n_vsize = size;
    total += bsize;
  }

  /* set all the elements to NIL, except for STRINGs and TVECs */
  if (type != STRING && type != TVEC)
    for (i = 0, p = vect->n_vdata; i < size; i++)
      *p++ = NIL;

  xlpop();
  return (vect);
}

#ifdef XLISP_STAT
/* Added for internal allocated storage - L. Tierney */
#include "xlstat.h"

LOCAL char *IViewNewAData P4C(int, n, int, m, long *, size, int, reloc)
{
  char *addr;
  
  addr = (reloc) ? StRCalloc(n, m): StCalloc(n, m);
  *size = (reloc) ? StRSize(addr) : ((long) m) * ((long) n);
  return(addr);
}

/* newadata(n, m, reloc) - convert a string to a string node */
LVAL newadata P3C(int, n, int, m, int, reloc)
{
    LVAL val;
    long size;
    
    /*if (reloc) xlfail("relocatable allocated data not supported");*/

    size = ((long) m) * ((long) n);
    check_alloc_threshold(size);

    xlsave1(val);
    val = newnode(ADATA);
    setadreloc(val, reloc);

    /**** check what happens when asked for zero length */
    if ((val->n_adaddr = IViewNewAData(n, m, &size, reloc)) == NULL) {
      ggc(FALSE);  
      if ((val->n_adaddr = IViewNewAData(n, m, &size, reloc)) == NULL) {
	ggc(TRUE);  
	if ((val->n_adaddr = IViewNewAData(n, m, &size, reloc)) == NULL)
	  xlabort("insufficient memory");
      }
    }
    val->n_adsize = size;
    total += size;

    xlpop();
    return (val);
}

VOID reallocaddata P3C(LVAL, val, int, n, int, m)
{
  char *addr;
  
  check_alloc_threshold(((long) n) * ((long) m)); /**** overestimate */
  if (! adatap(val) || ! getadreloc(val)) xlfail("not relocatable");
  addr = StRRealloc(getadaddr(val), n, m);
  if (addr == NULL) xlabort("allocation failed");
  val->n_adaddr = addr;
  total -= getadsize(val);
  val->n_adsize = StRSize(addr);
  total += getadsize(val);
}

VOID freeadata P1C(LVAL, val)
{
  if (! adatap(val)) xlfail("not a data object");
  if (getadreloc(val)) StRFree(getadaddr(val));
  else StFree(getadaddr(val));
  val->n_adaddr = NULL;
  total -= getadsize(val);
  val->n_adsize = 0;
  adjust_alloc_threshold();
}

/* Added for internal allocated storage - L. Tierney */
#endif /* XLISP_STAT */

#ifdef DODO
/* find_free_node - search all segments for the first free node, NIL if none */
LOCAL LVAL find_free_node(V)
{
  LVAL val;

  val = NEXT_FREE_NODE();
  while (val == NIL && curseg != NULL) {
    curseg = curseg->sg_next;
    if (curseg != NULL) {
      curtop = curbase = &curseg->sg_nodes[0];
      val = NEXT_FREE_NODE();
    }
  }
  return(val);
}
#else
/* macro version */
#define FIND_FREE_NODE(val) { \
  val = NEXT_FREE_NODE(); \
  while (val == NIL && curseg != NULL) { \
    curseg = curseg->sg_next; \
    if (curseg != NULL) { \
      curtop = curbase = &curseg->sg_nodes[0]; \
      val = NEXT_FREE_NODE(); \
    } \
  } \
}
#endif /* DODO */

/**** should interrupts be disabled here? */
/* newnode - allocate a new node */
LOCAL LVAL newnode P1C(int, type)
{
  register LVAL nnode;

#ifdef DODO
  if ((nnode = find_free_node()) == NIL) {
    ggc(FALSE);
    if (nnodes - nfree > nnodes * full_gc_frac) {
      ggc(TRUE);
      expand_node_space();
    }
    if ((nnode = find_free_node()) == NIL)
      xlabort("insufficient node space");
  }
#else
  FIND_FREE_NODE(nnode);
  if (nnode == NIL) {
    ggc(FALSE);
    if (nnodes - nfree > nnodes * full_gc_frac) {
      ggc(TRUE);
      expand_node_space();
    }
    FIND_FREE_NODE(nnode);
    if (nnode == NIL)
      xlabort("insufficient node space");
  }
#endif /* DODO */
  nfree--;

  /* move allocated node from immediate list to allocated list */
  if (type_is_allocated(type)) {
    nnode->n_vdata = NULL;
    curtop = LAST_NODE(nnode); /* back up the curtop pointer */
    MOVE_NODE_TO_BASE(nnode, ALLOCATED_OFFSET);
  }

  /* initialize the new node */
  setntype(nnode, type);
  unset_to_new_node(nnode);

  /* return the new node */
  return (nnode);
}

/* macro to move a node to the tenured list -- node is already marked as old */
#define TENURE_NODE(n) { LINK_NODE_TO_BASE(n,TENURED_OFFSET(n)); ntenured++; }

/* macro to either tenure a node or place it on the forwarded list */
#ifdef NULLPTRDEBUG
static int hit_null_pointer;

#define forward_node(n) \
{\
  register LVAL tmp = (n); \
  if (tmp == NULL) { \
    hit_null_pointer = TRUE; \
  } \
  else { \
    if (is_new_node(tmp)) {\
      UNLINK_NODE(tmp); \
      if (node_has_children(tmp)) { \
        SET_NEXT_NODE_PTR(tmp, forwarded_nodes); \
        forwarded_nodes = tmp; \
        } \
      else { \
        TENURE_NODE(tmp); \
      } \
      set_old_node(tmp); \
    } \
  } \
}
#else
#define forward_node(n) \
{\
  register LVAL tmp = (n); \
  if (is_new_node(tmp)) {\
    UNLINK_NODE(tmp); \
    if (node_has_children(tmp)) { \
      SET_NEXT_NODE_PTR(tmp, forwarded_nodes); \
      forwarded_nodes = tmp; \
      } \
    else { \
      TENURE_NODE(tmp); \
    } \
    set_old_node(tmp); \
  }\
}
#endif /* NULLPTRDEBUG */

/* macro to forward all children of a node -- node type must have children */
#ifdef BYTECODE
#define CASE_BCCLOSURE case BCCLOSURE:
#else
#define CASE_BCCLOSURE
#endif
#ifdef BIGNUMS
#define CASE_RATIO case RATIO:
#else
#define CASE_RATIO
#endif

#define forward_children(node) \
{ \
  register LVAL temp; \
  register int i, n; \
  temp = (node); \
  switch (ntype(temp)) { \
  case CONS: \
  case USTREAM: \
  case COMPLEX: \
  case RNDSTATE: \
  CASE_RATIO \
  CASE_BCCLOSURE \
  case DARRAY: \
    forward_node(car(temp)); \
    forward_node(cdr(temp)); \
    break; \
  case NATPTR: \
    forward_node(cdr(temp)); \
    break; \
  default: \
    for (i = 0, n = getsize(temp); --n >= 0; ++i) \
      forward_node(getelement(temp,i)); \
    break; \
  } \
}

/* clean_free_allocated_node - do finalization for free allocated node */
LOCAL VOID clean_free_allocated_node P1C(LVAL, tmp)
{
  switch (ntype(tmp)) {
  case FREE: /**** I'm not sure I need this case */
    break;
  case STRING:
  case TVEC:
    if (getstring(tmp) != NULL) {
      long size = btow_size(getslength(tmp) + 1);
      total -= size * sizeof(LVAL);
      VRELEASE(getstring(tmp), size);
    }
    break;
#ifdef BIGNUMS
  case BIGNUM:
    if (getbignumarray(tmp) != NULL) {
      long size = btow_size((1+(long)getbignumsize(tmp))*sizeof(BIGNUMDATA));
      total -= size * sizeof(LVAL);
      VRELEASE(getstring(tmp), size);
    }
    break;
#endif
#ifdef XLISP_STAT
  case ADATA:
    if (getadaddr(tmp) != NULL) {
      total -= getadsize(tmp);
      if (getadreloc(tmp)) StRFree(getadaddr(tmp));
      else StFree(getadaddr(tmp));
    }
    break;
#endif /* XLISP_STAT */
  case STREAM:
    if (getfile(tmp) != CLOSED
	&& getfile(tmp) != STDIN
	&& getfile(tmp) != STDOUT
	&& getfile(tmp) != CONSOLE)/* taa fix - dont close stdio */
      OSCLOSE(getfile(tmp));
    break;
  default:
    if (tmp->n_vsize) {
      VRELEASE((ALLOCTYPE *) tmp->n_vdata, tmp->n_vsize);
      total -= wtob_size(tmp->n_vsize);
    }
    break;
  }
  setntype(tmp, FREE);
}

/* gc - garbage collect (only called here and in xlimage.c) */
VOID gc(V) { ggc(TRUE); }

/* ggc - generational garbage collector */
LOCAL VOID ggc P1C(int, full_gc)
{
  register LVAL **p, *ap, tmp, forwarded_nodes;
  char buf[STRMAX+1];
  LVAL *newfp, fun;
  register SEGMENT *sg;
#ifdef TIMES
    unsigned long gccount = run_tick_count();
#endif

  /**** make sure interrupts are turned off */
  
#ifdef STSZ
#if (GCSTMARGIN>0)
    if (STACKREPORT(fun)<GCSTMARGIN) { /* Do not try gc with less */
        dbgputstr("Insufficient stack left for GC  ");
        if (batchmode) xlfatal("uncaught error");
        xltoplevel(TRUE);
    }
#endif
#endif

  set_gc_cursor(TRUE); /* L. Tierney */
  
  /* print the start of the gc message */
  if (s_gcflag != NULL && getvalue(s_gcflag) != NIL) {
    /* print message on a fresh line */
    xlfreshline(getvalue(s_debugio));
    sprintf(buf,"[ gc: total %ld, ",nnodes);
    dbgputstr(buf); /* TAA MOD -- was std output */
  }

#ifdef NULLPTRDEBUG
  hit_null_pointer = FALSE;
#endif /* NULLPTRDEBUG */

  /* reset the current segment pointers */
  curseg = lastpermseg->sg_next;
  curtop = curbase = &curseg->sg_nodes[0];

  /* make tenured space into new space for a full gc */
  if (full_gc) {
    /* change the old node code */
    old_code = (old_code == 1) ? 0 : 1;

    /* mark NIL and the fixed segments as old */
    set_old_node(NIL);
    set_to_new_node(NIL);
    for (sg = segs; sg != NULL; sg = sg->sg_next) {
      int n;

      for (tmp = &sg->sg_nodes[0], n = sg->sg_size; --n >= 0; tmp++)
	set_old_node(tmp);

      if (sg == lastpermseg) break;
    }

    for (sg = lastpermseg->sg_next; sg != NULL; sg = sg->sg_next) {
      register LVAL base;
      int n;

      /**** mark the list heads as old -- probably not necessary */
      for (tmp = &sg->sg_nodes[0], n = NUM_OFFSETS; --n >= 0; tmp++)
	set_old_node(tmp);

      /* mark the nodes in the immediate and allocated lists as new; */
      base = &sg->sg_nodes[0] + IMMEDIATE_OFFSET;
      for (tmp = NEXT_NODE(base); tmp != base; tmp = NEXT_NODE(tmp))
	set_new_node(tmp);
      base = &sg->sg_nodes[0] + ALLOCATED_OFFSET;
      for (tmp = NEXT_NODE(base); tmp != base; tmp = NEXT_NODE(tmp))
	set_new_node(tmp);
    
      /* move the tenured and to new lists to the new lists */
      base = &sg->sg_nodes[0];
      MOVE_LIST(base + IMMEDIATE_TENURED_OFFSET, base + IMMEDIATE_OFFSET);
      MOVE_LIST(base + IMMEDIATE_TO_NEW_OFFSET, base + IMMEDIATE_OFFSET);
      MOVE_LIST(base + ALLOCATED_TENURED_OFFSET, base + ALLOCATED_OFFSET);
      MOVE_LIST(base + ALLOCATED_TO_NEW_OFFSET, base + ALLOCATED_OFFSET);
    }
    ntenured = 0;
  }

  /* reset the forwarded node list */
  forwarded_nodes = NIL;

  /* forward the children of NIL */
  forward_children(NIL);

  /* forward the unbound marker */
  if (s_unbound != NIL)
    forward_node(s_unbound);

  /* forward the obarray and the current environment */
  if (obarray != NULL)
    forward_node(obarray);
  forward_node(xlenv);
  forward_node(xlfenv);
  forward_node(xldenv);

  /* forward the evaluation stack */
  for (p = xlstack; p < xlstktop; ++p)
    forward_node(**p);

  /* forward the argument stack */
  for (ap = xlargstkbase; ap < xlsp; ++ap)
    forward_node(*ap);

#ifdef MULVALS
  /* forward the result array */
    {
      int i;
      for (i = 0; i < xlnumresults; i++)
	forward_node(xlresults[i]);
    }
#endif /* MULVALS */

  /* forward the children of tenured nodes pointing to new ones */
  for (sg = lastpermseg->sg_next; sg != NULL; sg = sg->sg_next) {
    register LVAL base;
    base  = &sg->sg_nodes[0] + IMMEDIATE_TO_NEW_OFFSET;
    for (tmp = NEXT_NODE(base); tmp != base; tmp = NEXT_NODE(tmp))
      forward_children(tmp);
    base  = &sg->sg_nodes[0] + ALLOCATED_TO_NEW_OFFSET;
    for (tmp = NEXT_NODE(base); tmp != base; tmp = NEXT_NODE(tmp))
      forward_children(tmp);
  }

  /* forward the children of forwarded nodes */
  while ((tmp = forwarded_nodes) != NIL) {
    forwarded_nodes = NEXT_NODE_PTR(tmp);
    forward_children(tmp);
    TENURE_NODE(tmp);
    unset_to_new_node(tmp);
  }
  check_finalize();
  if (finalize_registered != NIL || finalize_pending != NIL) {
    forward_node(finalize_registered);
    forward_node(finalize_pending);
    while ((tmp = forwarded_nodes) != NIL) {
      forwarded_nodes = NEXT_NODE_PTR(tmp);
      forward_children(tmp);
      TENURE_NODE(tmp);
      unset_to_new_node(tmp);
    }
  }
  check_weak_boxes();

  /* clean out free allocated nodes */
  for (sg = lastpermseg->sg_next; sg != NULL; sg = sg->sg_next) {
    register LVAL ibase, abase;
    ibase = &sg->sg_nodes[0];
    abase = ibase + ALLOCATED_OFFSET;
    for (tmp = NEXT_NODE(abase); tmp != abase; tmp = NEXT_NODE(tmp)) {
      clean_free_allocated_node(tmp);
    }
    MOVE_LIST(abase, ibase);
  }

  /* adjust the threshold for allocations to trigger a gc */
  adjust_alloc_threshold();

  /* update the statistics */
  nfree = nnodes - nperm - ntenured;
  ++gccalls;
  if (! full_gc) minor_gccalls++;

#ifdef NULLPTRDEBUG
  if (hit_null_pointer)
    xlfail("GC hit a null pointer -- probably time to bail out");
#endif /* NULLPTRDEBUG */

/**** move gc cursor thing to before hook? */
  if (! null(finalize_pending))
    do_finalize();

  /* call the *gc-hook* if necessary */
  if (s_gchook != NULL && ((fun = getvalue(s_gchook)) != NIL) ) {

    /* rebind hook function to NIL  TAA MOD */
    tmp = xldenv;
    xldbind(s_gchook,NIL);

    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(cvfixnum((FIXTYPE)nnodes));
    pusharg(cvfixnum((FIXTYPE)nfree));
    xlfp = newfp;
    xlapply(2);

    /* unbind the symbol TAA MOD */
    xlunbind(tmp);
  }

  /* print the end of the gc message */
  if (s_gcflag != NULL && getvalue(s_gcflag) != NIL) {
    sprintf(buf,"%ld free ]\n",nfree);
    dbgputstr(buf); /* TAA MOD -- was std output */
  }
  set_gc_cursor(FALSE); /* L. Tierney */
#ifdef TIMES
    gctime += run_tick_count() - gccount;
#endif
}

/* addseg - add a segment to the available memory */
LOCAL int addseg(V)
{
    SEGMENT *newseg;

    /* allocate the new segment */
    if (anodes == 0 || (newseg = newsegment(anodes)) == NULL)
	return (FALSE);
    adjust_alloc_threshold();
    nfree += (long)anodes;

    /* return successfully */
    return (TRUE);
}

/* newsegment - create a new segment (only called here and in xlimage.c) */
SEGMENT *newsegment P1C(int, n)
{
    SEGMENT *newseg;
    LVAL p, base;
    int m;

#ifdef MACINTOSH /* L. Tierney */
    maximum_memory();
#endif /* MACINTOSH */

    n += NUM_OFFSETS;

    /* allocate the new segment */
    if ((newseg = (SEGMENT *)CALLOC(1,segsize(n))) == NULL)
	return (NULL);

    /* initialize the new segment */
    newseg->sg_size = n;
    newseg->sg_next = NULL;
    if (segs != NULL)
	lastseg->sg_next = newseg;
    else
	segs = newseg;
    lastseg = newseg;

    /* initialize the list header nodes */
    base = &newseg->sg_nodes[0];
    for (p = base, m = NUM_OFFSETS; --m >= 0; ++p) {
      set_old_node(p);
      SET_NODE_BASE(p, base);
      SET_NEXT_NODE(p, p);
      SET_LAST_NODE(p, p);
    }

    /* add each new node to the segment's immediate free list */
    for (p = base + NUM_OFFSETS, m = n - NUM_OFFSETS; --m >= 0; ++p) {
      setntype(p, FREE);
      set_new_node(p);
      unset_to_new_node(p);
      SET_NODE_BASE(p, base);
      LINK_NODE(p, base);
    }

    /* update the statistics */
    total += (long)segsize(n);
    nnodes += (long)n;
    ++nsegs;

    /* return the new segment */
    return (newseg);
}

/* stats - print memory statistics */
LOCAL VOID stats(V)
{
  long major_gccalls = gccalls - minor_gccalls;

  sprintf(buf,"Nodes:             %ld\n",nnodes); stdputstr(buf);
  sprintf(buf,"Free nodes:        %ld\n",nfree);  stdputstr(buf);
  sprintf(buf,"Segments:          %d\n",nsegs);   stdputstr(buf);
  sprintf(buf,"Allocate:          %d\n",anodes);  stdputstr(buf);
  sprintf(buf,"Total:             %ld\n",total);  stdputstr(buf);
  sprintf(buf,"Major Collections: %ld\n",major_gccalls); stdputstr(buf);
  sprintf(buf,"Minor Collections: %ld\n",minor_gccalls); stdputstr(buf);
/**** drop after debugging is done */
  sprintf(buf,"Threshold crosses: %ld\n", threshold_crossing_gccount);
  stdputstr(buf);
#ifdef TIMES
    sprintf(buf,"Time (sec):  %ld\n",gctime/ticks_per_second());
    stdputstr(buf);
#endif
}

/* xgc - xlisp function to force garbage collection */
LVAL xgc(V)
{
    /* make sure there aren't any arguments */
    xllastarg();

    /* garbage collect */
    gc();

    /* return nil */
    return (NIL);
}

/* xexpand - xlisp function to force memory expansion */
LVAL xexpand(V)
{
    LVAL num;
    FIXTYPE n,i;

    /* get the new number to allocate */
    if (moreargs()) {
	num = xlgafixnum();
	n = getfixnum(num);
        /* make sure there aren't any more arguments */
        xllastarg();
    }
    else
	n = 1;

    /* allocate more segments */
    for (i = 0; i < n; i++)
	if (!addseg())
	    break;

    /* return the number of segments added */
    return (cvfixnum((FIXTYPE)i));
}

/* xalloc - xlisp function to set the number of nodes to allocate */
LVAL xalloc(V)
{
    FIXTYPE n;  /* TAA MOD -- prevent overflow */
    int oldn;

#ifdef DEBUG
    /**** drop after debugging is done */
    if (symbolp(peekarg(0))) {
      LVAL arg;
  
      if (xlgetkeyarg(xlenter(":GC-EXPAND-FRACTION"), &arg)) {
	if (floatp(arg)) {
	  gc_expand_frac = getflonum(arg);
	  return(arg);
	}
      }
      if (xlgetkeyarg(xlenter(":FULL-GC-FRACTION"), &arg)) {
	if (floatp(arg)) {
	  full_gc_frac = getflonum(arg);
	  return(arg);
	}
      }
      return(NIL);
    }
#endif /* DEBUG */

    /* get the new number to allocate */
    n = getfixnum(xlgafixnum());

    /* make sure there aren't any more arguments */
    if (xlargc > 1) xltoomany();    /* but one more is OK, TAA MOD */

    /* set the new number of nodes to allocate */
    if (n > MAX_ANODES || n < MIN_ANODES)
      xlfail("alloc out of range");
    oldn = anodes;
    anodes = (int)n;

    /* return the old number */
    return (cvfixnum((FIXTYPE)oldn));
}

/* xmem - xlisp function to print memory statistics */
LVAL xmem(V)
{
    /* allow one argument for compatiblity with common lisp */
    if (xlargc > 1) xltoomany();    /* TAA Mod */

    /* print the statistics */
    stats();

    /* return nil */
    return (NIL);
}

#ifdef SAVERESTORE
/* xsave - save the memory image */
LVAL xsave(V)
{
    char *name;

    /* get the file name */
    name = getstring(xlgetfname());
    xllastarg();

    /* save the memory image */
    return (xlisave(name) ? s_true : NIL);
}

/* xrestore - restore a saved memory image */
LVAL xrestore(V)
{
#ifdef XLISP_STAT
    xlfail("restore not available");
    return(NIL); /* never returns */
#else
    extern XL_JMP_BUF top_level;
    char *name;

    /* get the file name */
    name = getstring(xlgetfname());
    xllastarg();

    /* restore the saved memory image */
    if (!xlirestore(name))
	return (NIL);

    /* return directly to the top level */
    dbgputstr("[ returning to the top level ]\n");  /* TAA MOD --was std out*/
    XL_LONGJMP(top_level,1);
    return (NIL);   /* never executed, but avoids warning message */
#endif /* XLISP_STAT */
}

VOID sweep_free_nodes(V)
{
  SEGMENT *sg;
  LVAL p;
  int n;
  /**** use free list = immediate list */
  for (sg = segs->sg_next; sg != NULL; sg = sg->sg_next) {
    p = &sg->sg_nodes[0] + NUM_OFFSETS;
    for (n = sg->sg_size - NUM_OFFSETS; --n >= 0; ++p)
      if (! is_old_node(p))
	setntype(p, FREE);
  }
}

VOID initialize_node P1C(LVAL, node)
{
  /* set gc flags */
  set_old_node(node); /*** new??? */
  unset_to_new_node(node);

  /* move allocated node from immediate list to allocated list */
  if (node_is_allocated(node))
    MOVE_NODE_TO_BASE(node, ALLOCATED_OFFSET);
}
  
#endif /* SAVERESTORE */

/* From XLISP-STAT, Copyright (c) 1988 Luke Tierney */

LVAL newicomplex P2C(FIXTYPE, real, FIXTYPE, imag)
{
  LVAL val, r, i;
  
  if (imag == 0) val = cvfixnum(real);
  else {
    xlstkcheck(2);
    xlsave(r);
    xlsave(i);
    r = cvfixnum(real);
    i = cvfixnum(imag);
    val = cons(r, i);
    setntype(val, COMPLEX);
    xlpopn(2);
  }
  return(val);
}

LVAL newdcomplex P2C(double, real, double, imag)
{
  LVAL val, r, i;
  
  xlstkcheck(2);
  xlsave(r);
  xlsave(i);
  r = cvflonum((FLOTYPE) real);
  i = cvflonum((FLOTYPE) imag);
  val = cons(r, i);
  setntype(val, COMPLEX);
  xlpopn(2);
  return(val);
}

#ifdef BIGNUMS
/* newcomplex - allocate and initialize a new object */
LVAL newcomplex P2C(LVAL, real, LVAL, imag)
{
  LVAL val;
  xlstkcheck(2);
  xlprotect(real);
  xlprotect(imag);
	
  if (! rationalp(real) || ! rationalp(imag)) {
    if (! floatp(real)) real = cvflonum(makefloat(real));
    if (! floatp(imag)) imag = cvflonum(makefloat(imag));
  }
  if (fixp(imag) && getfixnum(imag) == 0)
    val = real;
  else {
    val = newnode(COMPLEX);
    getreal(val) = real;
    getimag(val) = imag;
  }
  xlpopn(2);
  return(val);
}
#else
/* newcomplex - allocate and initialize a new object */
LVAL newcomplex P2C(LVAL, real, LVAL, imag)
{
  if (fixp(real) && fixp(imag))
    return(newicomplex(getfixnum(real), getfixnum(imag)));
  else
    return(newdcomplex(makefloat(real), makefloat(imag)));
}
#endif
#else

/* node flags */
#define MARK    0x40
#define LEFT    0x80
#define node_marked(n) ((ntype(n) & MARK))
#define unmark_node(n) (ntype(n) &= ~MARK)
#define mark_node(n) (ntype(n) |= MARK)
#define mark_and_type(n) (mark_node(n) & TYPEFIELD)
#define unmarked_array_node(n) ((ntype(n) & (ARRAY|MARK)) == ARRAY)
#define came_from_left(n) (ntype(n) & LEFT)
#define set_left(n) (ntype(n) |= LEFT)
#define unset_left(n) (ntype(n) &= ~LEFT)
#define is_array_type(x) (((x) & ARRAY) != 0)
#define gcntype(n) (ntype(n)&TYPEFIELD)

/* expansion parameters */
#define ALLOC_THRESHOLD_INCREMENT 0x20000L
#define GC_EXPAND_FRAC 0.2

/* macro to compute the size of a segment */
#define segsize(n) (sizeof(SEGMENT)+((n)-1)*sizeof(struct node))

#ifdef TIMES
static unsigned long gctime;    /* calcuate time in garbage collector */
#endif

/* macros to convert between size in bytes and vector size */
#define btow_size(n) (((unsigned)(n)+(sizeof(LVAL)-1))/(unsigned)sizeof(LVAL))
#define wtob_size(n) (((long) (n)) * sizeof(LVAL))

/* variables local to xldmem.c and xlimage.c */
SEGMENT *segs,*lastseg,*fixseg,*charseg;
int anodes,nsegs;
LVAL fnodes = NIL;

/* local variables */
static long alloc_threshold,threshold_crossing_gccount;
static double gc_expand_frac = GC_EXPAND_FRAC;

/* forward declarations */
#ifdef JMAC
LOCAL LVAL Newnode P1H(int);
#else
LOCAL LVAL newnode P1H(int);
#endif
LOCAL LVAL allocvector P2H(int, unsigned);
LOCAL VOID mark P1H(LVAL);
LOCAL VOID sweep(V);
LOCAL int  addseg(V);
LOCAL VOID stats(V);
LOCAL VOID check_alloc_threshold P1H(long);
LOCAL VOID adjust_alloc_threshold(V);
LOCAL VOID expand_node_space(V);
LOCAL VOID morevmem P1H(int);
LOCAL char *IViewNewAData P4H(int, int, long *, int);

#ifdef JMAC
LVAL _nnode = NIL;
FIXTYPE _tfixed = 0;
int _tint = 0;

#define newnode(type) (((_nnode = fnodes) != NIL) ? \
            ((fnodes = cdr(_nnode)), \
             nfree--, \
             (_nnode->n_type = type), \
             rplacd(_nnode,NIL), \
             _nnode) \
            : Newnode(type))

#endif

/* $putpatch.c$: "MODULE_XLDMEM_C_GLOBALS" */

/* xlminit - initialize the dynamic memory module */
VOID xlminit(V)
{
    LVAL p;
    int i;

    /* initialize our internal variables */
    segs = lastseg = NULL;
    nnodes = nfree = total = gccalls = 0L;
    alloc_threshold = ALLOC_THRESHOLD_INCREMENT;
    threshold_crossing_gccount = 0L;
    nsegs = 0;
    anodes = NNODES;
    gctime = 0L;
    fnodes = NIL;

    /* allocate the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
	xlfatal("insufficient memory");

    /* initialize the fixnum segment */
    p = &fixseg->sg_nodes[0];
    for (i = SFIXMIN; i <= SFIXMAX; ++i) {
	setntype(p, FIXNUM);
	p->n_fixnum = i;
	++p;
    }

    /* allocate the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
	xlfatal("insufficient memory");

    /* initialize the character segment */
    p = &charseg->sg_nodes[0];
    for (i = CHARMIN; i <= CHARMAX; ++i) {
	setntype(p, CHAR);
	p->n_chcode = i;
	++p;
    }

    /* initialize structures that are marked by the collector */
    obarray = NULL;
    xlenv = xlfenv = xldenv = NIL;
    s_gcflag = s_gchook = NULL;

    /* $putpatch.c$: "MODULE_XLDMEM_C_XLMINIT" */

    /* allocate the evaluation stack */
    if ((xlstkbase = (LVAL **)malloc(EDEPTH * sizeof(LVAL *))) == NULL)
	xlfatal("insufficient memory");
    xlstack = xlstktop = xlstkbase + EDEPTH;

    /* allocate the argument stack */
    if ((xlargstkbase = (LVAL *)malloc(ADEPTH * sizeof(LVAL))) == NULL)
	xlfatal("insufficient memory");
    xlargstktop = xlargstkbase + ADEPTH;
    xlfp = xlsp = xlargstkbase;
    *xlsp++ = NIL;

#ifdef MULVALS
    /* allocate the result array */
    if ((xlresults = (LVAL *)malloc(MULVALLIMIT * sizeof(LVAL))) == NULL)
	xlfatal("insufficient memory");
    for (i = 0; i < MULVALLIMIT; i++)
      xlresults[i] = NIL;
    xlnumresults = 0;
#endif /* MULVALS */

    /* we have to make a NIL symbol before continuing */
    p = xlmakesym("NIL");
    MEMCPY(NIL, p, sizeof(struct node));    /* we point to this! */
    defconstant(NIL, NIL);
    setfunction(NIL, NIL);
    setntype(p, FREE);                      /* don't collect "garbage" */

    finalize_registered = NIL;
    finalize_pending = NIL;
}

/* cons - construct a new cons node */
LVAL cons P2C(LVAL, x, LVAL, y)
{
    LVAL nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
      xlstkcheck(2);
      xlprotect(x);
      xlprotect(y);
      nnode = newnode(CONS);
      xlpopn(2);
    }
    else {
      fnodes = cdr(fnodes);
      setntype(nnode, CONS);
      nfree--;
    }

    /* initialize the new node */
    rplaca(nnode,x);
    rplacd(nnode,y);

    /* return the new node */
    return (nnode);
}

/* cvstring - convert a string to a string node */
LVAL cvstring P1C(char *, str)
{
    LVAL val;
    val = newstring(STRLEN(str));
    STRCPY(getstring(val),str);
    return (val);
}

/* newstring - allocate and initialize a new string */
LVAL newstring P1C(unsigned, size)
{
    LVAL val;
    val = allocvector(STRING,btow_size(size+1));
    val->n_strlen = size;
    return (val);
}

#ifdef BIGNUMS
/* newbignum - allocate a new bignum */
LVAL newbignum P1C(unsigned, size)
{
  /* size of the sign field not included in n_vsize */
  BIGNUMDATA *x;
  LVAL val;
  xlsave1(val);
  val = allocvector(BIGNUM,btow_size((size+1)*sizeof(BIGNUMDATA)));
  val->n_bsize = size;
  x = getbignumarray(val);
  size++;
  while (size--) *x++ = 0;	/* set value to zero */
  xlpop();
  return val;
}
#endif	

/* cvsymbol - convert a string to a symbol */
LVAL cvsymbol P1C(char *, pname)
{
    LVAL val;
    xlsave1(val);
    val = allocvector(SYMBOL,SYMSIZE);
    setvalue(val,s_unbound);
    setfunction(val,s_unbound);
    setpname(val,cvstring(pname));
    setsnormal(val); /* L. Tierney */
    xlpop();
    return (val);
}

/* cvsubr - convert a function to a subr or fsubr */
LVAL cvsubr P3C(subrfun, fcn, int, type, int, offset)
{
    LVAL val;
    val = newnode(type);
    val->n_subr = fcn;
    val->n_offset = offset;
    return (val);
}

/* cvfile - convert a file pointer to a stream */
LVAL cvfile P2C(FILEP, fp, int, iomode)
{
    LVAL val;
    val = newnode(STREAM);
    setfile(val,fp);
    setsavech(val,'\0');
    val->n_sflags = iomode;
    val->n_cpos = 0;
    return (val);
}

#ifdef JMAC
/* cvfixnum - convert an integer to a fixnum node */
LVAL Cvfixnum P1C(FIXTYPE, n)
{
    LVAL val;
    val = newnode(FIXNUM);
    val->n_fixnum = n;
    return (val);
}
#else
/* cvfixnum - convert an integer to a fixnum node */
LVAL cvfixnum P1C(FIXTYPE, n)
{
    LVAL val;
    if (n >= SFIXMIN && n <= SFIXMAX)
	return (&fixseg->sg_nodes[(int)n-SFIXMIN]);
    if ((val = fnodes) == NIL) {
      val = newnode(FIXNUM);
    }
    else {
      fnodes = cdr(fnodes);
      setntype(val, FIXNUM);
      nfree--;
    }
    val->n_fixnum = n;
    return (val);
}
#endif

/* cvflonum - convert a floating point number to a flonum node */
LVAL cvflonum P1C(FLOTYPE, n)
{
    LVAL val;
    if ((val = fnodes) == NIL) {
      val = newnode(FLONUM);
    }
    else {
      fnodes = cdr(fnodes);
      setntype(val, FLONUM);
      nfree--;
    }
    val->n_flonum = n;
    return (val);
}

/* cvchar - convert an integer to a character node */
#ifdef JMAC
LVAL Cvchar P1C(int, n)
{
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return(NIL);    /* never executed */
}
#else
LVAL cvchar P1C(int, n)
{
    if (n >= CHARMIN && n <= CHARMAX)
	return (&charseg->sg_nodes[n-CHARMIN]);
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return NIL;   /* never executed but gets rid of warning message */
}
#endif

#ifdef BIGNUMS
/* cvbratio - convert a pair of bignums into a ratio node */
LVAL cvbratio P2C(LVAL, num, LVAL, denom)
{
  FIXTYPE nu, d;
  int fixtyped;
  LVAL n,m,r;

  if (cvtbigfixnum(num, &nu) && cvtbigfixnum(denom, &d))
    return cvratio(nu,d);
  xlstkcheck(5);
  xlprotect(num);
  xlprotect(denom);
  xlsave(n);
  xlsave(m);
  xlsave(r);

  if (zeropbignum(num)) {	/* zero is fixnum zero */
    xlpopn(5);
    return cvfixnum((FIXTYPE) 0);
  }
  if (getbignumsign(denom)) {	/* denominator must be positive */
    denom = copybignum(denom, 0);
    num = copybignum(num,!getbignumsign(num)); /* final sign */
  }
  n = copybignum(num, 0);	/* abs of numerator */
  m = denom;
  for (;;) {			/* get gcd */
    divbignum(m, n, &r);	/* use remainder only */
    if (zeropbignum(r)) break;
    m = n;
    n = r;
  }
  if ((!cvtbigfixnum(n, &d)) || d != 1) { /* can reduce */
    denom = divbignum(denom, n, &r);
    num = divbignum(num, n, &r);
  }
  if ((fixtyped = cvtbigfixnum(denom, &d)) != 0 && d == 1) {
    /* reduced to an integer */
    xlpopn(5);
    if (cvtbigfixnum(num, &nu)) return cvfixnum(nu);
    return num;
  }
  /* got value to return */
  r = newnode(RATIO);
  r->n_cdr = r->n_car = NIL; /* in case of garbage collect */
  r->n_cdr = (fixtyped ? cvfixnum(d) : denom);
  r->n_car = (cvtbigfixnum(num, &nu) ? cvfixnum(nu) : num);
  xlpopn(5);
  return (r);
}

/* cvratio - convert an integer pair to a ratio node */
LVAL cvratio P2C(FIXTYPE, num, FIXTYPE, denom)
{
  LVAL val;
  unsigned long n, m, r, nu, de;
  int sign;

  if (num == 0) return cvfixnum((FIXTYPE) 0); /* zero is int zero */
  if (denom < 0) {		/* denominator must be positive */
    if (denom == -1 && num == MINFIX) {
      xlsave1(val);
      val = cvtulongbignum((unsigned long)MAXFIX+1, FALSE);
      xlpop();
      return val;
    }
    denom = -denom;
    sign = num >= 0;
  }
  else
    sign = num < 0;
	
  if (num < 0) num = -num;
  n = nu = (unsigned long)(long)num;
  m = de = (unsigned long)(long)denom; /* reduce the ratio: compute GCD */
  for (;;) {
    if ((r = m % n) == 0) break;
    m = n;
    n = r;
  }
  if (n != 1) {
    de /= n;
    nu /= n;
  }
  if (de == 1)
    return cvfixnum(sign ? -(long)nu : (long)nu); /* reduced to integer */
  xlsave1(val);
  val = newnode(RATIO);
  val->n_cdr = val->n_car = NIL; /* in case of garbage collect */
  if ((nu == (unsigned long)MAXFIX+1 && sign==0))
    val->n_car = cvtulongbignum(nu, sign);
  else
    val->n_car = cvfixnum(sign ? -(long)nu : (long)nu);
  if (de == (unsigned long)MAXFIX+1)
    val->n_cdr = cvtulongbignum(de, FALSE);
  else
    val->n_cdr = cvfixnum(de);
  xlpop();
  return (val);
}
#endif

/* newustream - create a new unnamed stream */
LVAL newustream(V)
{
    LVAL val;
    val = newnode(USTREAM);
    sethead(val,NIL);
    settail(val,NIL);
    return (val);
}

/* newobject - allocate and initialize a new object */
LVAL newobject P2C(LVAL, cls, int, size)
{
    LVAL val;
    val = allocvector(OBJECT,size+1);
    setelement(val,0,cls);
    return (val);
}

/* newclosure - allocate and initialize a new closure */
LVAL newclosure P4C(LVAL, name, LVAL, type, LVAL, env, LVAL, fenv)
{
    LVAL val;
    val = allocvector(CLOSURE,CLOSIZE);
    setname(val,name);
    settype(val,type);
    setenvi(val,env);
    setfenv(val,fenv);
    return (val);
}

/* newstruct - allocate and initialize a new structure node */
LVAL newstruct P2C(LVAL, type, int, size)
{
    LVAL val;
    val = allocvector(STRUCT,size+1);
    setelement(val,0,type);
    return (val);
}

#ifdef BYTECODE
/* newcpsnode - allocate and initialize a new CPS node for compiler */
LVAL newcpsnode P1C(LVAL, type)
{
    LVAL val;
    val = allocvector(CPSNODE,CPSNODESIZE);
    setcpstype(val,type);
    return (val);
}

/* newbcode - allocate and initialize a new byte code vector */
LVAL newbcode P5C(LVAL, code, LVAL, jtab, LVAL, lits, LVAL, idx, LVAL, env)
{
    LVAL val;
    val = allocvector(BCODE,BCODESIZE);
    setbccode(val, code);
    setbcjtab(val, jtab);
    setbclits(val, lits);
    setbcidx(val, idx);
    setbcenv(val, env);
    return (val);
}
#endif /* BYTECODE */

#ifdef PACKAGES
/* newpackage - allocate and initialize a new package */
LVAL newpackage(V)
{
  LVAL val;
  xlsave1(val);
  val = allocvector(PACKAGE,PACKSIZE);
  setintsyms(val, newvector(HSIZE));
  setextsyms(val, newvector(HSIZE));
  xlpop();
  return (val);
}
#endif /* PACKAGES */

/* newvector - allocate and initialize a new vector node */
LVAL newvector P1C(unsigned, size)
{
  return(allocvector(VECTOR,size));
}

/* allocvector - allocate and initialize a new vector node */
LOCAL LVAL allocvector P2C(int, type, unsigned, size)
{
  LVAL vect, *p;
  int i;
  unsigned long bsize; /* changed to unsigned long - L. Tierney */

  xlsave1(vect);
  vect = newnode(type);
  vect->n_vsize = 0;
  if (size != 0) {
    bsize = wtob_size(size);
    check_alloc_threshold(bsize);
    if ((vect->n_vdata = (LVAL *)VALLOC(size)) == NULL) {
      gc();
      if ((vect->n_vdata = (LVAL *)VALLOC(size)) == NULL)
	xlabort("insufficient vector space");
    }
    vect->n_vsize = size;
    total += bsize;
  }

  /* set all the elements to NIL, except for STRINGs and TVECs */
  if (type != STRING && type != TVEC)
    for (i = 0, p = vect->n_vdata; i < size; i++)
      *p++ = NIL;

  xlpop();
  return (vect);
}

#ifdef XLISP_STAT
/* Added for internal allocated storage - L. Tierney */
#include "xlstat.h"

LOCAL char *IViewNewAData P4C(int, n, int, m, long *, size, int, reloc)
{
  char *addr;
  
  addr = (reloc) ? StRCalloc(n, m): StCalloc(n, m);
  *size = (reloc) ? StRSize(addr) : ((long) m) * ((long) n);
  return(addr);
}

/* newadata(n, m, reloc) - convert a string to a string node */
LVAL newadata P3C(int, n, int, m, int, reloc)
{
    LVAL val;
    long size;
    
    /*if (reloc) xlfail("relocatable allocated data not supported");*/

    size = ((long) m) * ((long) n);
    check_alloc_threshold(size);

    xlsave1(val);
    val = newnode(ADATA);
    setadreloc(val, reloc);

    /**** check what happens when asked for zero length */
    if ((val->n_adaddr = IViewNewAData(n, m, &size, reloc)) == NULL) {
	gc();  
	if ((val->n_adaddr = IViewNewAData(n, m, &size, reloc)) == NULL)
	    xlabort("insufficient memory");
    }
    val->n_adsize = size;
    total += size;

    xlpop();
    return (val);
}

VOID reallocaddata P3C(LVAL, val, int, n, int, m)
{
  char *addr;
  
  check_alloc_threshold(((long) n) * ((long) m)); /**** overestimate */
  if (! adatap(val) || ! getadreloc(val)) xlfail("not relocatable");
  addr = StRRealloc(getadaddr(val), n, m);
  if (addr == NULL) xlabort("allocation failed");
  val->n_adaddr = addr;
  total -= getadsize(val);
  val->n_adsize = StRSize(addr);
  total += getadsize(val);
}

VOID freeadata P1C(LVAL, val)
{
  if (! adatap(val)) xlfail("not a data object");
  if (getadreloc(val)) StRFree(getadaddr(val));
  else StFree(getadaddr(val));
  val->n_adaddr = NULL;
  total -= getadsize(val);
  val->n_adsize = 0;
  adjust_alloc_threshold();
}

/* Added for internal allocated storage - L. Tierney */
#endif /* XLISP_STAT */

/* newnode - allocate a new node */
#ifdef JMAC
LOCAL LVAL Newnode P1C(int, type)
{
    LVAL nnode;
  
    /* get a free node */
    gc();
    expand_node_space();
    if ((nnode = fnodes) == NIL)
        xlabort("insufficient node space");

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1L;
    
    /* initialize the new node */
    nnode->n_type = type;
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
    }
#else
LOCAL LVAL newnode P1C(int, type)
{
    LVAL nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
        gc();
        expand_node_space();
	if ((nnode = fnodes) == NIL)
	    xlabort("insufficient node space");
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1L;

    /* initialize the new node */
    setntype(nnode, type);
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
}
#endif

/* gc - garbage collect (only called here and in xlimage.c) */
VOID gc(V)
{
    LVAL **p,*ap,tmp;
    FRAMEP newfp;
    LVAL fun;
#ifdef TIMES
    unsigned long gccount = run_tick_count();
#endif

#ifdef STSZ
#if (GCSTMARGIN>0)
    if (STACKREPORT(fun)<GCSTMARGIN) { /* Do not try gc with less */
        dbgputstr("Insufficient stack left for GC  ");
        if (batchmode) xlfatal("uncaught error");
        xltoplevel(TRUE);
    }
#endif
#endif

    set_gc_cursor(TRUE); /* L. Tierney */

    /* print the start of the gc message */
    if (s_gcflag != NULL && getvalue(s_gcflag) != NIL) {
        /* print message on a fresh line */
        xlfreshline(getvalue(s_debugio));
	sprintf(buf,"[ gc: total %ld, ",nnodes);
	dbgputstr(buf); /* TAA MOD -- was std output */
    }

    /* $putpatch.c$: "MODULE_XLDMEM_C_GC" */

    /* mark the obarray, the argument list and the current environment */
    if (obarray != NULL)
	mark(obarray);
    if (xlenv != NIL)
	mark(xlenv);
    if (xlfenv != NIL)
	mark(xlfenv);
    if (xldenv != NIL)
	mark(xldenv);

    mark(NIL);
    mark(s_unbound);    /* TAA Mod 1/92 */

    /* mark the evaluation stack */
    for (p = xlstack; p < xlstktop; ++p)
	if ((tmp = **p) != NIL)
	    mark(tmp);

    /* mark the argument stack */
    for (ap = xlargstkbase; ap < xlsp; ++ap)
	if ((tmp = *ap) != NIL)
	    mark(tmp);

#ifdef MULVALS
    /* mark the results */
    {
      int i;
      for (i = 0; i < xlnumresults; i++)
	if ((tmp = xlresults[i]) != NIL)
	  mark(tmp);
    }
#endif /* MULVALS */

    /* sweep memory collecting all unmarked nodes */
    sweep();

    unmark_node(NIL);

    adjust_alloc_threshold();

    /* count the gc call */
    ++gccalls;

    /* call the *gc-hook* if necessary */
    if (s_gchook != NULL && ((fun = getvalue(s_gchook)) != NIL) ) {

        /* rebind hook function to NIL  TAA MOD */
        tmp = xldenv;
        xldbind(s_gchook,NIL);

	newfp = xlsp;
	pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	pusharg(fun);
	pusharg(cvfixnum((FIXTYPE)2));
	pusharg(cvfixnum((FIXTYPE)nnodes));
	pusharg(cvfixnum((FIXTYPE)nfree));
	xlfp = newfp;
	xlapply(2);

        /* unbind the symbol TAA MOD */
        xlunbind(tmp);
    }

    /* print the end of the gc message */
    if (s_gcflag != NULL && getvalue(s_gcflag) != NIL) {
	sprintf(buf,"%ld free ]\n",nfree);
	dbgputstr(buf); /* TAA MOD -- was std output */
    }

    set_gc_cursor(FALSE); /* L. Tierney */

#ifdef TIMES
    gctime += run_tick_count() - gccount;
#endif
}

/* mark - mark all accessible nodes */
LOCAL VOID mark P1C(LVAL, ptr)
{
  register LVAL this,prev,tmp;
  int type,i,n;

#ifdef STSZ /* can't recover from here */
#if (GCSTMARGIN>0)
    if (STACKREPORT(n) < GCMARGLO)
        xlfatal("Insufficient stack during GC");
#endif
#endif

  /* initialize */
  prev = NIL;
  this = ptr;

  /* mark this list */
  for (;;) {

    /* descend as far as we can */
    while (! node_marked(this)) {

      type = mark_and_type(this);

      /* check cons type nodes */
      if (type >= CONS && type < ARRAY) {
	if ((tmp = car(this)) != NIL) {
	  set_left(this);
	  rplaca(this,prev);
	}
	else if ((tmp = cdr(this)) != NIL)
	  rplacd(this,prev);
	else 				/* both sides nil */
	  break;
	prev = this;			/* step down the branch */
	this = tmp;
      }
      /* $putpatch.c$: "MODULE_XLDMEM_C_MARK" */
      else if (is_array_type(type)) {
	  for (i = 0, n = getsize(this); i < n; i++) 
	    if ((tmp = getelement(this,i)) != NIL)
	      mark(tmp);
	  break;
      }
      else if (type == NATPTR) {
	mark(cdr(this));
	break;
      }
      else
	break;
    }

    /* backup to a point where we can continue descending */
    for (;;) {

      /* make sure there is a previous node */
      if (prev != NIL) {
	if (came_from_left(prev)) {	/* came from left side */
	  unset_left(prev);
	  tmp = car(prev);
	  rplaca(prev,this);
	  if ((this = cdr(prev)) != NIL) {
	    rplacd(prev,tmp);
	    break;
	  }
	}
	else {				/* came from right side */
	  tmp = cdr(prev);
	  rplacd(prev,this);
	}
	this = prev;			/* step back up the branch */
	prev = tmp;
      }

      /* no previous node, must be done */
      else
	return;
    }
  }
}

/* sweep - sweep all unmarked nodes and add them to the free list */
LOCAL VOID sweep(V)
{
  SEGMENT *seg;
  LVAL p;
  int n;

  /* empty the free list */
  fnodes = NIL;
  nfree = 0L;

  /* add all unmarked nodes */
  for (seg = segs; seg != NULL; seg = seg->sg_next) {
    if (seg == fixseg || seg == charseg) {
      /* remove marks from segments */
      p = &seg->sg_nodes[0];
      for (n = seg->sg_size; --n >= 0;)
	unmark_node(p++);
      continue;
    }
    p = &seg->sg_nodes[0];

    for (n = seg->sg_size; --n >= 0; p++)
      if (node_marked(p))
	unmark_node(p);
      else {
	switch (gcntype(p)) {
	case STRING:
	case TVEC:
	  if (getstring(p) != NULL) {
	    long size = btow_size(getslength(p) + 1);
	    total -= size * sizeof(LVAL);
	    VRELEASE(getstring(p), size);
	  }
	  break;
#ifdef BIGNUMS
  case BIGNUM:
	  if (getbignumarray(p) != NULL) {
	    long size = btow_size((1+(long)getbignumsize(p))
				  *sizeof(BIGNUMDATA));
	    total -= size * sizeof(LVAL);
	    VRELEASE(getstring(p), size);
	  }
	  break;
#endif
#ifdef XLISP_STAT
	case ADATA:
	  if (getadaddr(p) != NULL) {
	    total -= getadsize(p);
	    if (getadreloc(p)) StRFree(getadaddr(p));
	    else StFree(getadaddr(p));
	  }
	  break;
#endif /* XLISP_STAT */
	case STREAM:
	  if (getfile(p) != CLOSED
	      && getfile(p) != STDIN
	      && getfile(p) != STDOUT
	      && getfile(p) != CONSOLE)/* taa fix - dont close stdio */
	    OSCLOSE(getfile(p));
	  break;
        /* $putpatch.c$: "MODULE_XLDMEM_C_SWEEP" */
	case SYMBOL:
	case OBJECT:
	case VECTOR:
	case CLOSURE:
	case STRUCT:
#ifdef BYTECODE
	case CPSNODE:
	case BCODE:
#endif /*BYTECODE */
#ifdef PACKAGES
	case PACKAGE:
#endif /* PACKAGES */
	  if (p->n_vsize) {
	    total -= wtob_size(p->n_vsize);
	    VRELEASE((char *) p->n_vdata, p->n_vsize);
	  }
	  break;
	}
	setntype(p, FREE);
	rplaca(p,NIL);
	rplacd(p,fnodes);
	fnodes = p;
	nfree++;
      }
  }
}

/* addseg - add a segment to the available memory */
LOCAL int addseg(V)
{
    SEGMENT *newseg;
    LVAL p;
    int n;

    /* allocate the new segment */
    if (anodes == 0 || (newseg = newsegment(anodes)) == NULL)
	return (FALSE);
    adjust_alloc_threshold();

    /* add each new node to the free list */
    p = &newseg->sg_nodes[0];
    for (n = anodes; --n >= 0; ++p) {
	rplacd(p,fnodes);
	fnodes = p;
    }
    nfree += (long)anodes;

    /* return successfully */
    return (TRUE);
}

/* newsegment - create a new segment (only called here and in xlimage.c) */
SEGMENT *newsegment P1C(int, n)
{
    SEGMENT *newseg;

#ifdef MACINTOSH /* L. Tierney */
    maximum_memory();
#endif /* MACINTOSH */

    /* allocate the new segment */
    if ((newseg = (SEGMENT *)CALLOC(1,segsize(n))) == NULL)
	return (NULL);

    /* initialize the new segment */
    newseg->sg_size = n;
    newseg->sg_next = NULL;
    if (segs != NULL)
	lastseg->sg_next = newseg;
    else
	segs = newseg;
    lastseg = newseg;

    /* update the statistics */
    total += (long)segsize(n);
    nnodes += (long)n;
    ++nsegs;

    /* return the new segment */
    return (newseg);
}

/* stats - print memory statistics */
LOCAL VOID stats(V)
{
    sprintf(buf,"Nodes:        %ld\n",nnodes); stdputstr(buf);
    sprintf(buf,"Free nodes:   %ld\n",nfree);  stdputstr(buf);
    sprintf(buf,"Segments:     %d\n",nsegs);   stdputstr(buf);
    sprintf(buf,"Allocate:     %d\n",anodes);  stdputstr(buf);
    sprintf(buf,"Total:        %ld\n",total);  stdputstr(buf);
    sprintf(buf,"Collections:  %ld\n",gccalls); stdputstr(buf);
/**** drop after debugging is done */
    sprintf(buf,"Threshold crosses: %ld\n", threshold_crossing_gccount);
    stdputstr(buf);
#ifdef TIMES
    sprintf(buf,"Time (sec):  %ld\n",gctime/ticks_per_second());
    stdputstr(buf);
#endif
}

/* xgc - xlisp function to force garbage collection */
LVAL xgc(V)
{
    /* make sure there aren't any arguments */
    xllastarg();

    /* garbage collect */
    gc();

    /* return nil */
    return (NIL);
}

/* xexpand - xlisp function to force memory expansion */
LVAL xexpand(V)
{
    LVAL num;
    FIXTYPE n,i;

    /* get the new number to allocate */
    if (moreargs()) {
	num = xlgafixnum();
	n = getfixnum(num);
        /* make sure there aren't any more arguments */
        xllastarg();
    }
    else
	n = 1;

    /* allocate more segments */
    for (i = 0; i < n; i++)
	if (!addseg())
	    break;

    /* return the number of segments added */
    return (cvfixnum((FIXTYPE)i));
}

/* xalloc - xlisp function to set the number of nodes to allocate */
LVAL xalloc(V)
{
    FIXTYPE n;  /* TAA MOD -- prevent overflow */
    int oldn;

#ifdef DEBUG
    /**** drop after debugging is done */
    if (symbolp(peekarg(0))) {
      LVAL arg;
  
      if (xlgetkeyarg(xlenter(":GC-EXPAND-FRACTION"), &arg)) {
	if (floatp(arg)) {
	  gc_expand_frac = getflonum(arg);
	  return(arg);
	}
      }
      return(NIL);
    }
#endif /* DEBUG */

    /* get the new number to allocate */
    n = getfixnum(xlgafixnum());

    /* make sure there aren't any more arguments */
    if (xlargc > 1) xltoomany();    /* but one more is OK, TAA MOD */

#ifdef DODO
    /**** put something like this in after debugging */
    /**** also need to be able to expand up to some level, */
    /**** adjust expansion parameters */
    /* Place limits on argument by clipping to reasonable values  TAA MOD */
    if (n > ((long)MAXSLEN - sizeof(SEGMENT))/sizeof(struct node)) 
        n = ((long)MAXSLEN - sizeof(SEGMENT))/sizeof(struct node);
    else if (n < 1000) 
        n = 1000;   /* arbitrary */
#endif /* DODO */

    /* set the new number of nodes to allocate */
    oldn = anodes;
    anodes = (int)n;

    /* return the old number */
    return (cvfixnum((FIXTYPE)oldn));
}

/* xmem - xlisp function to print memory statistics */
LVAL xmem(V)
{
    /* allow one argument for compatiblity with common lisp */
    if (xlargc > 1) xltoomany();    /* TAA Mod */

    /* print the statistics */
    stats();

    /* return nil */
    return (NIL);
}

#ifdef SAVERESTORE
/* xsave - save the memory image */
LVAL xsave(V)
{
    char *name;

    /* get the file name */
    name = getstring(xlgetfname());
    xllastarg();

    /* save the memory image */
    return (xlisave(name) ? s_true : NIL);
}

/* xrestore - restore a saved memory image */
LVAL xrestore(V)
{
#ifdef XLISP_STAT
    xlfail("restore not available");
    return(NIL); /* never returns */
#else
    char *name;

    /* get the file name */
    name = getstring(xlgetfname());
    xllastarg();

    /* restore the saved memory image */
    if (!xlirestore(name))
	return (NIL);

    /* return directly to the top level */
    dbgputstr("[ returning to the top level ]\n");  /* TAA MOD --was std out*/
    XL_LONGJMP(top_level,1);
    return (NIL);   /* never executed, but avoids warning message */
#endif /* XLISP_STAT */
}
#endif /* SAVERESTORE */

/* From XLISP-STAT, Copyright (c) 1988 Luke Tierney */

LVAL newicomplex P2C(FIXTYPE, real, FIXTYPE, imag)
{
  LVAL val, r, i;
  
  if (imag == 0) val = cvfixnum(real);
  else {
    xlstkcheck(2);
    xlsave(r);
    xlsave(i);
    r = cvfixnum(real);
    i = cvfixnum(imag);
    val = cons(r, i);
    setntype(val, COMPLEX);
    xlpopn(2);
  }
  return(val);
}

LVAL newdcomplex P2C(double, real, double, imag)
{
  LVAL val, r, i;
  
  xlstkcheck(2);
  xlsave(r);
  xlsave(i);
  r = cvflonum((FLOTYPE) real);
  i = cvflonum((FLOTYPE) imag);
  val = cons(r, i);
  setntype(val, COMPLEX);
  xlpopn(2);
  return(val);
}

#ifdef BIGNUMS
/* newcomplex - allocate and initialize a new object */
LVAL newcomplex P2C(LVAL, real, LVAL, imag)
{
  LVAL val;
  xlstkcheck(2);
  xlprotect(real);
  xlprotect(imag);
	
  if (! rationalp(real) || ! rationalp(imag)) {
    if (! floatp(real)) real = cvflonum(makefloat(real));
    if (! floatp(imag)) imag = cvflonum(makefloat(imag));
  }
  if (fixp(imag) && getfixnum(imag) == 0)
    val = real;
  else {
    val = newnode(COMPLEX);
    getreal(val) = real;
    getimag(val) = imag;
  }
  xlpopn(2);
  return(val);
}
#else
/* newcomplex - allocate and initialize a new object */
LVAL newcomplex P2C(LVAL, real, LVAL, imag)
{
  if (fixp(real) && fixp(imag))
    return(newicomplex(getfixnum(real), getfixnum(imag)));
  else
    return(newdcomplex(makefloat(real), makefloat(imag)));
}
#endif
#endif /* NEWGC */

#ifdef TIMES
/* gc_tick_count - total number of ticks spent in gc */
unsigned long gc_tick_count(V) { return(gctime); }
#endif

/* check_alloc_threshold - do gc if alloc would push total over threshold */
LOCAL VOID check_alloc_threshold P1C(long, size)
{
  if (alloc_threshold < total + size) {
    long tmp;
    static int bumped = 0;
#ifdef NEWGC
    tmp = (total + size + ALLOC_THRESHOLD_INCREMENT - 1);
    tmp /= ALLOC_THRESHOLD_INCREMENT;
    tmp *= ALLOC_THRESHOLD_INCREMENT;
    alloc_threshold = tmp;
    ggc(FALSE);
    bumped++;
    if (bumped > 4 || alloc_threshold < total + size) {
      tmp = (total + size + ALLOC_THRESHOLD_INCREMENT - 1);
      tmp /= ALLOC_THRESHOLD_INCREMENT;
      tmp *= ALLOC_THRESHOLD_INCREMENT;
      alloc_threshold = tmp;
      ggc(TRUE);
      bumped = 0;
    }
#else
    tmp = (total + size + ALLOC_THRESHOLD_INCREMENT - 1);
    tmp /= ALLOC_THRESHOLD_INCREMENT;
    tmp *= ALLOC_THRESHOLD_INCREMENT;
    alloc_threshold = tmp;
    gc();
#endif /* NEWGC */
    threshold_crossing_gccount++;
  }
}

/* adjust_alloc_threshold - adjust threshold up or down based on total */
LOCAL VOID adjust_alloc_threshold(V)
{
  long n, m;

  n = (total + ALLOC_THRESHOLD_INCREMENT - 1) / ALLOC_THRESHOLD_INCREMENT;
  m = alloc_threshold / ALLOC_THRESHOLD_INCREMENT;
  if (n > m) alloc_threshold = n * ALLOC_THRESHOLD_INCREMENT;
  else if (m > n + 2) alloc_threshold = (n + 2) * ALLOC_THRESHOLD_INCREMENT;
}

/* expand_node_space allocate new nodes if necessary */
LOCAL VOID expand_node_space(V)
{
  long needed;

  /* find the number of additional nodes needed */
  needed = (long) (gc_expand_frac * (nnodes - nfree)) + 1;

  /* allocate additional segments if necessary */
  if (anodes > 0)
      while (needed > nfree)
	if (! addseg())
	  break;
}   

/**** This scheme assumes sizeof(void *) <= 8 */
/**** For the MS Windoes version, malloc should be tuned properly */
/**** Should be integrated properly with GC; use know sizes for symbol, etc. */

#define PAGEUNITS 9
#define MEMUNITS 10
static ALLOCTYPE *memarray[MEMUNITS] = { NULL };

/* morevmem - allocate another page of storage to nunits */
LOCAL VOID morevmem P1C(int, nunits)
{
  ALLOCTYPE *p;
  unsigned long nbytes, nalloc;
  int n;

  nalloc = (8 << ((nunits < PAGEUNITS) ? PAGEUNITS : nunits));
  n = (nunits < PAGEUNITS) ? (1 << (PAGEUNITS - nunits)) : 1;
  nbytes = (8 << nunits);
  p = malloc(nalloc);
  if (p == NULL) xlabort("insufficient vector space");
  if (p != NULL) {
    while (n-- > 0) {
      *((char **) p) = memarray[nunits];
      memarray[nunits] = p;
      p = ((char *) p) + nbytes;
    }
  }
}

/* VALLOC - allocate space for a vector */
ALLOCTYPE *VALLOC P1C(unsigned long, size)
{
  ALLOCTYPE *p;
  unsigned long nbytes;
  int nunits;

  if (size > MAXVECLEN) xlfail("allocation too large"); /**** check this */

  /* find number of bytes, rounded to next multiple of 8 */
  nbytes = (size * sizeof(LVAL) + 7) & ~7;
  
  /* find memory unit -- max(0, ceiling(log2(nbytes)) - 3) */
  if (nbytes == 0) nunits = 0;
  else {
    register unsigned long shiftr = (nbytes - 1) >> 2;
    nunits = 0;
    while (shiftr >>= 1) nunits++;
  }

  if (nbytes == 0)
    p = NULL;
  else if (nunits < MEMUNITS) {
    char **pt;
    if (memarray[nunits] == NULL) morevmem(nunits);
    pt = (char **) memarray[nunits];
    if (pt != NULL) memarray[nunits] = pt[0];
    p = (char *) pt;
  }
  else p = malloc(size * sizeof(LVAL));
  if (p == NULL) xlabort("insufficient vector space");

  MEMSET((char *) p, 0, wtob_size(size));
  return(p);
}

/* VRELEASE - release space for a vector */
VOID VRELEASE P2C(ALLOCTYPE *, p, unsigned long, size)
{
  unsigned long nbytes;
  int nunits;

  if (p != NULL) {
    /* find number of bytes, rounded to next multiple of 8 */
    nbytes = (size * sizeof(LVAL) + 7) & ~7;
  
    /* find memory unit -- max(0, ceiling(log2(nbytes)) - 3) */
    if (nbytes == 0) nunits = 0;
    else {
      register unsigned long shiftr = (nbytes - 1) >> 2;
      nunits = 0;
      while (shiftr >>= 1) nunits++;
    }

    if (nunits < MEMUNITS) {
      *((char **) p) = memarray[nunits];
      memarray[nunits] = p;
    }
    else free(p);
  }
}

LVAL newtvec P2C(int, n, int, m)
{
  unsigned size;
  LVAL val;

  size = n * m;
  val = allocvector(TVEC,btow_size(size+1));
  val->n_strlen = size;
  return (val);
}

LVAL newrndstate P2C(LVAL, gen, LVAL, data)
{
  LVAL val;
  val = cons(gen, data);
  setntype(val, RNDSTATE);
  return(val);
}

#ifdef BYTECODE
LVAL newbcclosure P2C(LVAL, type, LVAL, code)
{
  LVAL val;
  val = cons(type, code);
  setntype(val, BCCLOSURE);
  return(val);
}
#endif /* BYTECODE */

static VOID check_finalize(V)
{
  LVAL last = NIL, next = finalize_registered, head, tail;

  while (consp(next)) {
    if (is_new_node(car(car(next)))) {
      head = next;
      tail = cdr(next);
      if (null(last))
        finalize_registered = tail;
      else
        Rplacd(last, tail);
      next = tail;
      Rplacd(head, finalize_pending);
      finalize_pending = head;
    }
    else {
      last = next;
      next = cdr(next);
    }
  }
}

static VOID do_finalize(V)
{
  CONTEXT cntxt;
  LVAL next;

  xlsave1(next);
  xlbegin(&cntxt,CF_UNWIND|CF_ERROR,NIL);
  XL_SETJMP(cntxt.c_jmpbuf);
  while (consp(finalize_pending)) {
    next = finalize_pending;
    finalize_pending = cdr(next);
    xlapp1(cdr(car(next)), car(car(next)));
  }
  xlend(&cntxt);
  xlpop();
}

static void check_weak_boxes()
{
  LVAL last = NIL, next = xlweakboxes;

  while (ntype(next) == WEAKBOX) {
    if (is_new_node(next)) {
      /* delete box from weak box list */
      LVAL tail = cdr(next);
      if (null(last))
        xlweakboxes = tail;
      else
        Rplacd(last, tail);
      next = tail;
    }
    else {
      if (is_new_node(car(next)))
	Rplaca(next, NIL);
      last = next;
      next = cdr(next);
    }
  }
}

LVAL xregfinal(V)
{
  LVAL arg, fun;
  arg = xlgetarg();
  fun = xlgetarg();
  xllastarg();
  finalize_registered = cons(cons(arg, fun), finalize_registered);
  return NIL;
}

LVAL xmkweakbox(V)
{
  LVAL val = xlgetarg();
  LVAL box = consa(val);
  xllastarg();
  ntype(box) = WEAKBOX;
  Rplacd(box, xlweakboxes);
  xlweakboxes = box;
  return box;
}

#define weakboxp(x) (ntype(x) == WEAKBOX)
#define xlgaweakbox()    (testarg(typearg(weakboxp)))
LVAL xweakboxval(V)
{
  LVAL box = xlgaweakbox();
  xllastarg();
  return car(box);
}

LVAL newnatptr P2C(ALLOCTYPE *, p, LVAL, v)
{
  LVAL val;
  xlprot1(v);
  val = newnode(NATPTR);
  car(val) = (LVAL) p;
  cdr(val) = v;
  xlpop();
  return val;
}
