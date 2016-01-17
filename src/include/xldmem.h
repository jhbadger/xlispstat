#ifndef XLDMEM_H
#define XLDMEM_H

/* xldmem.h - dynamic memory definitions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

/* small fixnum range */
#ifndef SFIXMIN
#define SFIXMIN		(-128)
#endif
#ifndef SFIXMAX
#define SFIXMAX		255
#endif
#define SFIXSIZE	(SFIXMAX-SFIXMIN+1)

/* character range */
#define CHARMIN		0
#define CHARMAX		255
#define CHARSIZE	(CHARMAX-CHARMIN+1)

/* new node access macros */
#define ntype(x)	((x)->n_type)
#define setntype(x,t)	((x)->n_type = (t))

/* cons access macros */
#define car(x)		((x)->n_car)
#define cdr(x)		((x)->n_cdr)
#ifdef NEWGC
#define Rplaca(x,y)	((x)->n_car = (y))
#define Rplacd(x,y)	((x)->n_cdr = (y))
#else
#define rplaca(x,y)	((x)->n_car = (y))
#define rplacd(x,y)	((x)->n_cdr = (y))
#endif /* NEWGC */

/* symbol access macros */
#define getvalue(x)      getelement(x,0)
#define setvalue(x,v)    setelement(x,0,v)
#define getfunction(x)   getelement(x,1)
#define setfunction(x,v) setelement(x,1,v)
#define getplist(x)      getelement(x,2)
#define setplist(x,v)    setelement(x,2,v)
#define getpname(x)      getelement(x,3)
#define setpname(x,v)    setelement(x,3,v)
#ifdef PACKAGES
#define getpackage(x)    getelement(x,4)
#define setpackage(x,v)  setelement(x,4,v)
#define SYMSIZE	         5
#else
#define SYMSIZE          4
#endif /* PACKAGES */

/* closure access macros */
#define getname(x)      getelement(x,0)
#define setname(x,v)    setelement(x,0,v)
#define gettype(x)      getelement(x,1)
#define settype(x,v)    setelement(x,1,v)
#define getargs(x)      getelement(x,2)
#define setargs(x,v)    setelement(x,2,v)
#define getoargs(x)     getelement(x,3)
#define setoargs(x,v)   setelement(x,3,v)
#define getrest(x)      getelement(x,4)
#define setrest(x,v)    setelement(x,4,v)
#define getkargs(x)     getelement(x,5)
#define setkargs(x,v)   setelement(x,5,v)
#define getaargs(x)     getelement(x,6)
#define setaargs(x,v)   setelement(x,6,v)
#define getbody(x)      getelement(x,7)
#define setbody(x,v)    setelement(x,7,v)
#define getenvi(x)      getelement(x,8)
#define setenvi(x,v)    setelement(x,8,v)
#define getfenv(x)      getelement(x,9)
#define setfenv(x,v)    setelement(x,9,v)
#define getlambda(x)    getelement(x,10)
#define setlambda(x,v)  setelement(x,10,v)
#define CLOSIZE         11

#ifdef BYTECODE
#define getbcctype(x)   car(x)
#define setbcctype(x,v) rplaca(x,v)
#define getbcccode(x)   cdr(x)
#define setbcccode(x,v) rplacd(x,v)
#endif /* BYTECODE */

/* vector access macros */
#define getsize(x)      ((x)->n_vsize)
#define getelement(x,i) ((x)->n_vdata[i])
#ifdef NEWGC
#define Setelement(x,i,v) ((x)->n_vdata[i] = (v))
#else
#define setelement(x,i,v) ((x)->n_vdata[i] = (v))
#endif /* NEWGC */

/* object access macros */
#define getclass(x)     getelement(x,0)
#define getivar(x,i)    getelement(x,i+1)
#define setivar(x,i,v)  setelement(x,i+1,v)

/* instance variable numbers for the class 'Class' */
#define MESSAGES        0       /* list of messages */
#define IVARS           1       /* list of instance variable names */
#define CVARS           2       /* list of class variable names */
#define CVALS           3       /* list of class variable values */
#define SUPERCLASS      4       /* pointer to the superclass */
#define IVARCNT         5       /* number of class instance variables */
#define IVARTOTAL       6       /* total number of instance variables */
#define PNAME           7       /* print name TAA Mod */
/* number of instance variables for the class 'Class' */
#define CLASSSIZE       8


/* subr/fsubr access macros */
#define getsubr(x)      ((x)->n_subr)
#define getoffset(x)    ((x)->n_offset)
#ifdef MULVALS
#define mulvalp(x)	 ((x)->n_mvflag)
#define setmulvalp(x, v) ((x)->n_mvflag = (v))
#endif /* MULVALS */

/* fixnum/flonum/char access macros */
#define getfixnum(x)    ((x)->n_fixnum)
#define getflonum(x)    ((x)->n_flonum)
#define getchcode(x)    ((x)->n_chcode)

/* complex number access macros */
#define getreal(x)      car(x)
#define getimag(x)      cdr(x)

#ifdef BIGNUMS
/* rational number access macros */
#define getnumer(x)     car(x)
#define getdenom(x)     cdr(x)

/* bignum access macros */
typedef unsigned short BIGNUMDATA;
#define getbignumarray(x) ((x)->n_bdata)
#define getbignumsize(x) ((x)->n_bsize)
#define getbignumsign(x) ((x)->n_bdata[0])
#endif

/* string access macros */
#define getstring(x)    ((x)->n_string)
#define getslength(x)   ((x)->n_strlen)
/* the following functions were TAA modifications */
#define getstringch(x,i) (((unsigned char *)((x)->n_string))[i])
#define setstringch(x,i,v) ((x)->n_string[i] = (char)(v))

/* tvec access macros */
#define gettvecdata(x)  ((ALLOCTYPE *) getstring(x))
#define gettlength(x)   getslength(x)
#define gettvectype(x) ((unsigned char *) (gettvecdata(x)))[gettlength(x)]
#define settvectype(x,t) \
  (((unsigned char *) (gettvecdata(x)))[gettlength(x)] = (unsigned char) (t))

enum {
  CD_CHARACTER = 0,
  CD_FIXTYPE,
  CD_FLOTYPE,
  CD_CXFIXTYPE,
  CD_CXFLOTYPE,
  CD_CHAR,
  CD_UCHAR,
  CD_SHORT,
  CD_INT,
  CD_LONG,
  CD_FLOAT,
  CD_DOUBLE,
  CD_COMPLEX,
  CD_DCOMPLEX,
  CD_TRUE
};

/* file stream access macros */
#define getfile(x)      ((x)->n_fp)
#define setfile(x,v)    ((x)->n_fp = (v))
#define getsavech(x)    ((x)->n_savech)
#define setsavech(x,v)  ((x)->n_savech = (v))

/* unnamed stream access macros */
#define gethead(x)      car(x)
#define sethead(x,v)    rplaca(x,v)
#define gettail(x)      cdr(x)
#define settail(x,v)    rplacd(x,v)

/* displaced array access macros */
#define getdarraydim(x)  car(x)
#define getdarraydata(x) cdr(x)
#define getdarrayrank(x) getsize(getdarraydim(x))

/* random state access macros */
#define getrndgen(x)     car(x)
#define setrndgen(x,v)   rplaca(x,v)
#define getrnddata(x)    cdr(x)
#define setrnddata(x,v)  rplacd(x,v)

/* allocated data access macros *//* L. Tierney */
#define getadaddr(x)    ((x)->n_adaddr)
#define getadreloc(x)   nuflags(x)
#define getadsize(x)    (((x)->n_adsize))
#define setadreloc(x,c) setnuflags(x,c)

/* native pointers *//* L. Tierney */
/* Use the CONS representation with pointer in the CAR cell */
#define getnpaddr(x)    ((ALLOCTYPE *) car(x))
#define setnpaddr(x,v)  (car(x) = (LVAL) (v))
#define getnpprot(x)    cdr(x)
#define setnpprot(x,v)  rplacd(x,v)

#ifdef BYTECODE
/* CPS node macros */
#define getcpstype(x)   getelement(x, 0)
#define setcpstype(x,v) setelement(x, 0, v)
#define CPSNODESIZE 8

/* byte code macros */
#define getbccode(x)    getelement(x, 0)
#define getbcjtab(x)    getelement(x, 1)
#define getbclits(x)    getelement(x, 2)
#define getbcidx(x)     getelement(x, 3)
#define getbcenv(x)     getelement(x, 4)
#define getbcname(x)    getelement(x, 5)
#define getbcdef(x)     getelement(x, 6)
#define setbccode(x, v) setelement(x, 0, v)
#define setbcjtab(x, v) setelement(x, 1, v)
#define setbclits(x, v) setelement(x, 2, v)
#define setbcidx(x, v)  setelement(x, 3, v)
#define setbcenv(x, v)  setelement(x, 4, v)
#define setbcname(x, v) setelement(x, 5, v)
#define setbcdef(x, v)  setelement(x, 6, v)
#define BCODESIZE 7
#endif /* BYTECODE */

#ifdef PACKAGES
/* package macros */
#define getintsyms(x)   getelement(x, 0)
#define getextsyms(x)   getelement(x, 1)
#define getshadowing(x) getelement(x, 2)
#define getuses(x)      getelement(x, 3)
#define getusedby(x)    getelement(x, 4)
#define getpacknames(x) getelement(x, 5)
#define setintsyms(x,v) setelement(x, 0, v)
#define setextsyms(x,v) setelement(x, 1, v)
#define setshadowing(x,v) setelement(x, 2, v)
#define setuses(x,v)    setelement(x, 3, v)
#define setusedby(x,v)  setelement(x, 4, v)
#define setpacknames(x,v) setelement(x, 5, v)
#define PACKSIZE 6
#endif /* PACKAGES */

/* node types */
#define FREE	0
#define SUBR	1
#define FSUBR	2
#define FIXNUM  4
#define FLONUM  5
#define STRING  6
#define STREAM  7
#define CHAR    8
#ifdef BIGNUMS
#define BIGNUM  9
#endif
#define ADATA   10
#define TVEC    11
#define NATPTR  12  /* native pointer */
#define WEAKBOX 13
/* Non-array types from CONS up use CAR and CDR fields */
/* This means that all types from CONS up have garbage collectable elements */
#define CONS	16
#define COMPLEX 17
#ifdef BIGNUMS
#define RATIO	18
#endif
#define USTREAM 19
#define DARRAY  20
#define RNDSTATE 21
#ifdef BYTECODE
#define BCCLOSURE 22
#endif /* BYTECODE */
#define ARRAY   32      /* arrayed types */
#define SYMBOL  (ARRAY+1)
#define OBJECT  (ARRAY+2)
#define VECTOR  (ARRAY+3)
#define CLOSURE (ARRAY+4)
#define STRUCT  (ARRAY+5)
#ifdef BYTECODE
#define CPSNODE (ARRAY+6)
#define BCODE   (ARRAY+7)
#endif /* BYTECODE */
#ifdef PACKAGES
#define PACKAGE (ARRAY+8)
#endif /* PACKAGES */
#define TYPEFIELD 0x3f
#ifdef NEWGC
#define NUMTYPES (ARRAY+9)
#endif /* NEWGC */

/* subr/fsubr node */
#define n_subr		n_info.n_xsubr.xs_subr
#define n_offset	n_info.n_xsubr.xs_offset
#define n_mvflag	n_info.n_xsubr.xs_mvflag

/* cons node */
#define n_car		n_info.n_xcons.xc_car
#define n_cdr		n_info.n_xcons.xc_cdr

/* fixnum node */
#define n_fixnum	n_info.n_xfixnum.xf_fixnum

/* flonum node */
#define n_flonum	n_info.n_xflonum.xf_flonum
/* character node */
#define n_chcode	n_info.n_xchar.xc_chcode

/* string node */
#define n_string	n_info.n_xstring.xs_string
#define n_strlen	n_info.n_xstring.xs_length

/* stream node */
#define n_fp		n_info.n_xstream.xs_fp
#define n_savech	n_info.n_xstream.xs_savech

#define S_READING       1   /* File is in reading mode */
#define S_WRITING       2   /* file is in writing mode */
#define S_FORREADING    4   /* File open for reading */
#define S_FORWRITING    8   /* file open for writing */
#define S_BINARY        16  /* file is binary file */
#define S_UNSIGNED      32  /* file is unsigned binary */

#define n_sflags        n_info.n_xstream.xs_flags
#define n_cpos          n_info.n_xstream.xs_cpos  /* position of char file*/
#define n_bsiz		n_info.n_xstream.xs_cpos  /* byte size of bin file*/

#ifdef BIGNUMS
/* bignum node */
#define n_bsize		n_info.n_xbignum.xb_length
#define n_bdata		n_info.n_xbignum.xb_data
#endif

/* vector/object node */
#define n_vsize		n_info.n_xvector.xv_size
#define n_vdata		n_info.n_xvector.xv_data

/* allocated data node *//* L. Tierney */
#define n_adaddr	n_info.n_xadata.xa_addr
#define n_adsize	n_info.n_xadata.xa_size

/* node structure */
typedef struct node {
#ifdef NEWGC
    union {
      struct node *n_next;	/* pointer for forwarding list */
      struct {
	short forward;		/* next doubly-linked list node offset */
	short backward;		/* previous doubly-linked list node offset */
      } n_gc_offsets;
    } n_gc;
    short n_base_offset;	/* offset from beginning of segment */
    unsigned char n_type;	/* type of node */
    unsigned char n_flags;	/* flag bits */
#else
/* 32 bit compilers that pack structures will do better with
   these chars at the end  */
#ifndef ALIGN32
    unsigned char n_type;	/* type of node */
    unsigned char n_flags;	/* flag bits */
#endif
#endif /* NEWGC */
    union ninfo { 		/* value */
	struct xsubr {		/* subr/fsubr node */
	    struct node *(*xs_subr) _((void));	/* function pointer */
	    short xs_offset;		/* offset into funtab */
#ifdef MULVALS
	    unsigned char xs_mvflag;		/* multiple value return */
#endif
	} n_xsubr;
	struct xcons {		/* cons node */
	    struct node *xc_car;	/* the car pointer */
	    struct node *xc_cdr;	/* the cdr pointer */
	} n_xcons;
	struct xfixnum {	/* fixnum node */
	    FIXTYPE xf_fixnum;		/* fixnum value */
	} n_xfixnum;
	struct xflonum {	/* flonum node */
	    FLOTYPE xf_flonum;		/* flonum value */
	} n_xflonum;
	struct xchar {		/* character node */
	    int xc_chcode;		/* character code */
	} n_xchar;
#ifdef BIGNUMS
	struct xbignum {	/* bignum node */
	  unsigned xb_length;	/* length of data in #BIGNUMDATAs */
	  BIGNUMDATA *xb_data;/* sign BIGNUMDATA followed by xb_length
				 BIGNUMDATAs */
	} n_xbignum;
#endif
	struct xstring {	/* string node */
	    unsigned xs_length;		/* string length */
	    char *xs_string;		/* string pointer */
	} n_xstring;
	struct xstream { 	/* stream node */
	    FILEP xs_fp;		/* the file pointer */
	    unsigned char xs_savech;	/* lookahead character */
	    char xs_flags;		/* read/write mode flags */
	    short xs_cpos;		/* character position in line */
	} n_xstream;
	struct xvector {	/* vector/object/symbol/structure node */
	    int xv_size;		/* vector size */
	    struct node **xv_data;	/* vector data */
	} n_xvector;
#ifdef XLISP_STAT
	struct xadata {
	    char *xa_addr;
	    long xa_size;
	} n_xadata;
#endif /* XLISP_STAT */
	/* $putpatch.c$: "MODULE_XLDMEM_H_NINFO" */
    } n_info;
#ifndef NEWGC
#ifdef ALIGN32
    unsigned char n_type;               /* type of node */
    unsigned char n_flags;
#endif /* ALIGN32 */
#endif /* NEWGC */
} *LVAL;

/* memory segment structure definition */
typedef struct segment {
    int sg_size;
    struct segment *sg_next;
    struct node sg_nodes[1];
} SEGMENT;

/* memory allocation functions */
#ifdef ANSI
#define ALLOCTYPE void
#else
#define ALLOCTYPE char
#endif /* ANSI */
extern VOID gc _((void));               /* do a garbage collect */
extern SEGMENT *newsegment _((int n));  /* create a new segment */
extern ALLOCTYPE *VALLOC _((unsigned long));
extern VOID VRELEASE _((ALLOCTYPE *p, unsigned long size));
#ifdef NEWGC
extern VOID sweep_free_nodes _((void));
extern VOID initialize_node _((LVAL node));
#endif /* NEWGC */
extern LVAL cons _((LVAL x, LVAL y));   /* (cons x y) */
extern LVAL cvsymbol _((char *pname));  /* convert a string to a symbol */
extern LVAL cvstring _((char *str));    /* convert a string */
extern LVAL cvfile _((FILEP fp, int flags));    /* convert a FILEP to a file */
extern LVAL cvsubr _((LVAL (*fcn) _((void)), int type, int offset));
                                /* convert a function to a subr/fsubr */
#ifdef JMAC
extern LVAL Cvfixnum _((FIXTYPE n));	/* convert a fixnum */
extern LVAL Cvchar _((int n));		/* convert a character */
#else
extern LVAL cvfixnum _((FIXTYPE n));    /* convert a fixnum */
extern LVAL cvchar _((int n));          /* convert a character */
#endif
extern LVAL cvflonum _((FLOTYPE n));    /* convert a flonum */

#ifdef BIGNUMS
extern LVAL cvratio _((FIXTYPE n, FIXTYPE d));  /* convert a ratio */
extern LVAL cvbratio _((LVAL n, LVAL d));  /* convert a ratio */
#endif

extern LVAL newstring _((unsigned size));   /* create a new string */
extern LVAL newvector _((unsigned size));   /* create a new vector */
extern LVAL newobject _((LVAL cls, int size));  /* create a new object */
extern LVAL newclosure _((LVAL name, LVAL type, LVAL env, LVAL fenv));
                                        /* create a new closure */
extern LVAL newustream _((void));       /* create a new unnamed stream */
extern LVAL newstruct _((LVAL type, int size)); /* create a new structure */
extern LVAL newcomplex _((LVAL r, LVAL i));   /* create a new complex number */
extern LVAL newicomplex _((FIXTYPE r, FIXTYPE i));
extern LVAL newdcomplex _((FLOTYPE r, FLOTYPE i));

#ifdef BIGNUMS
/* most functions are in xlbignum.c */
extern LVAL newbignum _((unsigned size));
extern LVAL cvtflonum _((LVAL num)); /* convert a rational to a float */
#endif

extern VOID defconstant _((LVAL sym, LVAL val));
extern LVAL newdarray _((LVAL dim, LVAL data)); /**** put in xldmem.c */
#ifdef XLISP_STAT
extern LVAL newadata _((int n, int m, int reloc));
extern VOID reallocaddata _((LVAL val, int n, int m));
extern VOID freeadata _((LVAL val));
#endif /* XLISP_STAT */
extern LVAL newtvec _((int n, int m));
extern LVAL newnatptr _((ALLOCTYPE *p, LVAL v));
extern LVAL newrndstate _((LVAL gen, LVAL m));
extern LVAL newbcclosure _((LVAL type, LVAL code));

#ifdef BYTECODE
extern LVAL newcpsnode _((LVAL type));
     /* create a new CPS node for compiler */
extern LVAL newbcode _((LVAL code, LVAL jtab, LVAL lits, LVAL idx, LVAL env));
     /* create a new byte code vector */
#endif /* BYTECODE */
#ifdef PACKAGES
extern LVAL newpackage _((void));	/* create a new package */
#endif /* PACKAGES */

#ifdef NEWGC
extern LVAL rplaca _((LVAL x, LVAL y)), rplacd _((LVAL x, LVAL y));
extern LVAL setelement _((LVAL x, int i, LVAL y));
#endif /* NEWGC */

/* node flags access macros */
#ifdef NEWGC
#define ngcflag1(x) ((x)->n_flags & 1)
#define setngcflag1(x) ((x)->n_flags |= 1)
#define unsetngcflag1(x) ((x)->n_flags &= ~1)
#define ngcflag2(x) ((x)->n_flags & 2)
#define setngcflag2(x) ((x)->n_flags |= 2)
#define unsetngcflag2(x) ((x)->n_flags &= ~2)
#define nuflags(x)	((x)->n_flags & 4)
#define setnuflags(x,t)	((t) ? ((x)->n_flags |= 4) : ((x)->n_flags &= ~4))

#define F_SPECIAL   4
#define F_CONSTANT  8

#define setsnormal(x)   ((x)->n_flags &= ~(F_SPECIAL | F_CONSTANT))
#define setsspecial(x)  ((x)->n_flags |= F_SPECIAL)
#define setsconstant(x) ((x)->n_flags |= (F_SPECIAL | F_CONSTANT))
#define constantp(x)    ((x)->n_flags & F_CONSTANT)
#define specialp(x)     ((x)->n_flags & F_SPECIAL)

#else

#define F_SPECIAL   1
#define F_CONSTANT  2
#define F_NORMAL    0

#define setsflags(x,c)	((x)->n_flags = (c))
#define setsnormal(x) setsflags(x, F_NORMAL)
#define setsspecial(x) setsflags(x, F_SPECIAL)
#define setsconstant(x) setsflags(x, F_CONSTANT | F_SPECIAL)
#define constantp(x)  ((x)->n_flags & F_CONSTANT)
#define specialp(x) ((x)->n_flags & F_SPECIAL)

#ifdef JMAC
/* Speed ups, reduce function calls for fixed characters and numbers	   */
/* Speed is exeptionaly noticed on machines with a large instruction cache */
/* No size effects here (JonnyG) */

extern SEGMENT *fixseg, *charseg;
extern FIXTYPE _tfixed;
extern int _tint;

#define cvfixnum(n) ((_tfixed = (n)), \
		((_tfixed > SFIXMIN && _tfixed < SFIXMAX) ? \
		&fixseg->sg_nodes[(int)_tfixed-SFIXMIN] : \
		Cvfixnum(_tfixed)))

#if (CHARMIN == 0)  /* eliminate a comparison */
#define cvchar(c) ((_tint = (c)), \
		(((unsigned)_tint) <= CHARMAX ? \
			&charseg->sg_nodes[_tint-CHARMIN] : \
		Cvchar(_tint)))
#else
#define cvchar(c) ((_tint = (c)), \
		((_tint >= CHARMIN && _tint <= CHARMAX) ? \
			&charseg->sg_nodes[_tint-CHARMIN] : \
		Cvchar(_tint)))
#endif
#endif
#define nuflags(x)	((x)->n_flags)
#define setnuflags(x,t)	((x)->n_flags = (t))
#endif /* NEWGC */

#define setsvalue(s,v)  (setvalue(s,v), setsspecial(s))
/* $putpatch.c$: "MODULE_XLDMEM_H_GLOBALS" */
#endif /* XLDMEM_H */
