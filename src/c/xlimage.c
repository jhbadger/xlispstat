/* xlimage - xlisp memory image save/restore functions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */
/* modified so that offset is in sizeof(node) units */

#include "xlisp.h"
#ifdef XLISP_STAT
#include "xlstat.h"
#include "version.h"
#endif /* XLISP_STAT */

#ifdef SAVERESTORE

#ifdef NEWGC
#define NUM_OFFSETS 6
#else
#define NUM_OFFSETS 0
#endif /* NEWGC */

#define FILENIL ((OFFTYPE)0)    /* value of NIL in a file */

/* external variables from xldmem.c */
extern struct segment *segs,*lastseg,*fixseg,*charseg;
extern int anodes,nsegs;
extern long nnodes;
#ifdef NEWGC
extern struct segment *lastpermseg;
#else
extern LVAL fnodes;
#endif /* NEWGC */

/* local variables */
static OFFTYPE off,foff;
static FILEP fp;

/* forward declarations */
LOCAL OFFTYPE readptr(V);
LOCAL OFFTYPE cvoptr P1H(LVAL);
LOCAL LVAL cviptr P1H(OFFTYPE);
LOCAL VOID freeimage(V);
LOCAL VOID setoffset(V);
LOCAL VOID writenode P1H(LVAL);
LOCAL VOID writeptr P1H(OFFTYPE);
LOCAL VOID readnode P2H(int, LVAL);

/* macro to convert between size in bytes and vector size */
#define btow_size(n) (((unsigned)(n)+(sizeof(LVAL)-1))/(unsigned)sizeof(LVAL))

/* xlisave - save the memory image */
int xlisave P1C(char *, fname)
{
    SEGMENT *seg;
    int n,i,max;
    LVAL p;
#ifndef MACINTOSH
    char fullname[STRMAX+1];

    /* default the extension */
    if (needsextension(fname)) {
	strcpy(fullname,fname);
	strcat(fullname,".wks");
	fname = fullname;
    }
#endif /* MACINTOSH */

    /* open the output file */
    if ((fp = OSBOPEN(fname,CREATE_WR)) == CLOSED)
	return (FALSE);

    disable_interrupts();

    /* first call the garbage collector to clean up memory */
    gc();
#ifdef NEWGC
    sweep_free_nodes();
#endif /* NEWGC */

#ifdef XLISP_STAT
    /* write out file type and version numbers as a validity check */
    OSWRITE("X1Ws",1,4,fp);
    writeptr((OFFTYPE) XLS_MAJOR_RELEASE);
    writeptr((OFFTYPE) XLS_MINOR_RELEASE);
    writeptr((OFFTYPE) XLS_SUBMINOR_RELEASE);
#ifdef _Windows
#ifdef WIN32
    writeptr((OFFTYPE) TRUE);
#else
    writeptr((OFFTYPE) FALSE);
#endif /* WIN32 */
#endif /* _Windows */
#else
    /* write out size of ftab (used as validity check) TAA MOD */
    writeptr((OFFTYPE)ftabsize);
#endif /* XLISP_STAT */

    /* write out the pointer to the unbound marker TAA MOD 1/93 */
    writeptr(cvoptr(s_unbound));

    /* write out the pointer to the *obarray* symbol */
    writeptr(cvoptr(obarray));

    /* write out components of NIL other than value, which must be NIL */
    writeptr(cvoptr(getfunction(NIL)));
    writeptr(cvoptr(getplist(NIL)));
    writeptr(cvoptr(getpname(NIL)));
#ifdef PACKAGES
    writeptr(cvoptr(getpackage(NIL)));
#endif

    /* write out the weak box list head */
    writeptr(cvoptr(xlweakboxes));

    /* setup the initial file offsets */
    off = foff = (OFFTYPE)2;

    /* write out all nodes that are still in use */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	p = &seg->sg_nodes[0] + NUM_OFFSETS;
	for (n = seg->sg_size - NUM_OFFSETS; --n >= 0; ++p, off++)
	    switch (ntype(p)) {
	    case FREE:
		break;
#ifdef BIGNUMS
	    case RATIO:
#endif
	    case COMPLEX:
	    case CONS:
	    case USTREAM:
	    case RNDSTATE:
#ifdef BYTECODE
	    case BCCLOSURE:
#endif /* BYTECODE */
	    case DARRAY:
	    case NATPTR:
	    case WEAKBOX:
		setoffset();
		OSPUTC(ntype(p),fp);
		if (ntype(p) != NATPTR)
		  writeptr(cvoptr(car(p)));
		writeptr(cvoptr(cdr(p)));
#ifdef NEWGC
		OSPUTC(p->n_flags,fp);
#endif /* NEWGC */
		foff++;
		break;
	    default:
		setoffset();
		writenode(p);
		break;
	    }
    }

    /* write the terminator */
    OSPUTC(FREE,fp);
    writeptr((OFFTYPE)0);

    /* write out data portion of SYMBOL/VECTOR/OBJECT/STRING/CLOSURE nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	p = &seg->sg_nodes[0] + NUM_OFFSETS;
	for (n = seg->sg_size - NUM_OFFSETS; --n >= 0; ++p)
	    switch (ntype(p)) {
	    /* $putpatch.c$: "MODULE_XLIMAGE_C_XLISAVE" */
	    case SYMBOL:
	    case OBJECT:
	    case VECTOR:
	    case CLOSURE:
	    case STRUCT:
#ifdef BYTECODE
	    case CPSNODE:
	    case BCODE:
#endif /* BYTECODE */
#ifdef PACKAGES
	    case PACKAGE:
#endif /* PACKAGES */
		max = getsize(p);
		for (i = 0; i < max; ++i)
		    writeptr(cvoptr(getelement(p,i)));
		break;
#ifdef BIGNUMS
	    case BIGNUM:
		max = (getbignumsize(p)+1)*sizeof(BIGNUMDATA);
		OSWRITE(getbignumarray(p),1,max,fp);
		break;
#endif
	    case STRING:
	    case TVEC:
		max = getslength(p)+1;
                OSWRITE(getstring(p),1,max,fp);
		break;
#ifdef XLISP_STAT
	    case ADATA:
		max = getadsize(p);
		 OSWRITE(getadaddr(p),1,max,fp);
		break;
#endif /* XLISP_STAT */
#ifdef FILETABLE
            case STREAM:
                if (getfile(p) > CONSOLE ) {
		    long offset;
		    strcpy(buf, filetab[getfile(p)].tname);
                    OSWRITE(buf,1,FNAMEMAX,fp);
                    offset = OSTELL(getfile(p));
                    OSWRITE(&offset,1,sizeof(long),fp);
	    }
                break;
#endif
	    }
    }

    /* close the output file */
    OSCLOSE(fp);

#ifdef MACINTOSH
    fsetfileinfo (fname, 'X1St', 'X1Ws'); 
#endif /* MACINTOSH */

    enable_interrupts();

    /* return successfully */
    return (TRUE);
}

/* xlirestore - restore a saved memory image */
int xlirestore P1C(char  *, fname)
{
    int n,i,max,type,size;
    SEGMENT *seg;
    LVAL p;
#ifndef MACINTOSH
    char fullname[STRMAX+1];

    /* default the extension */
    if (needsextension(fname)) {
	strncpy(fullname,fname,STRMAX-4);
	strcat(fullname,".wks");
	fname = fullname;
    }
#endif /* MACINTOSH */

    /* open the file */
#ifdef PATHNAMES
    if ((fp = ospopen(fname,FALSE)) == CLOSED)
#else
    if ((fp = OSBOPEN(fname,OPEN_RO)) == CLOSED)
#endif
	return (FALSE);

#ifdef XLISP_STAT
    /* Check for file and version validity */
    if (OSREAD(buf, 1, 4, fp) != 4 || strncmp(buf, "X1Ws", 4) != 0)
      xlfatal("bad image file");
    if (readptr() != (OFFTYPE) XLS_MAJOR_RELEASE ||
	readptr() != (OFFTYPE) XLS_MINOR_RELEASE ||
	readptr() != (OFFTYPE) XLS_SUBMINOR_RELEASE)
      xlfatal("image file version does not match system version");
#ifdef _Windows
#ifdef WIN32
    if (readptr() != (OFFTYPE) TRUE)
      xlfatal("not a Win32 image file");
#else
    if (readptr() != (OFFTYPE) FALSE)
      xlfatal("not a Win16 image file");
#endif /* WIN32 */
#endif /* _Windows */
#else
    /* Check for file validity  TAA MOD */
    if (readptr() != (OFFTYPE) ftabsize) {
        OSCLOSE(fp);    /* close it -- we failed */
        return (FALSE);
    }
#endif /* XLISP_STAT */

    disable_interrupts();

    /* free the old memory image */
    freeimage();

    /* initialize */
    off = (OFFTYPE)2;
    total = nnodes = nfree = 0L;
#ifndef NEWGC
    fnodes = NIL;
#endif /* NEWGC */
    segs = lastseg = NULL;
    nsegs = gccalls = 0;
#ifdef BIGNUMS
    n_bigzero=n_bigmone=NULL;   /* TAA fix 3/13/96 -- added */
#endif
    xlenv = xlfenv = xldenv = s_gchook = s_gcflag = NIL;
    xlstack = xlstkbase + EDEPTH;
    xlfp = xlsp = xlargstkbase;
    *xlsp++ = NIL;
    xlcontext = NULL;
#ifdef MULVALS  /* TAA BUG FIX 01/94 */
    xlnumresults = 0;
#endif

    /* create the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
	xlfatal("insufficient memory - fixnum segment");

    /* create the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
	xlfatal("insufficient memory - character segment");

#ifdef NEWGC
    /* set pointer to last permanent segment */
    lastpermseg = lastseg;
#endif /* NEWGC */

    /* read in the pointer to the unbound marker TAA MOD 1/93 */
    s_unbound = cviptr(readptr());

    /* read the pointer to the *obarray* symbol */
    obarray = cviptr(readptr());

    /* read components of NIL other than value, which must be NIL */
    setvalue(NIL, NIL);
    setfunction(NIL, cviptr(readptr()));
    setplist(NIL, cviptr(readptr()));
    setpname(NIL, cviptr(readptr()));
#ifdef PACKAGES
    setpackage(NIL, cviptr(readptr()));
#endif

    /* read in the weak box list head */
    xlweakboxes = cviptr(readptr());

    /* read each node */
    while ((type = OSGETC(fp)) >= 0) {
	switch (type) {
	case FREE:
	    if ((off = readptr()) == (OFFTYPE)0)
		goto done;
	    break;
#ifdef BIGNUMS
	case RATIO:
#endif
	case COMPLEX:
	case CONS:
	case USTREAM:
	case RNDSTATE:
#ifdef BYTECODE
	case BCCLOSURE:
#endif /* BYTECODE */
	case DARRAY:
	case NATPTR:
	case WEAKBOX:
	    p = cviptr(off);
	    setntype(p, type);
	    rplaca(p,type==NATPTR ? NULL : cviptr(readptr()));
	    rplacd(p,cviptr(readptr()));
#ifdef NEWGC
	    p->n_flags = OSGETC(fp);
	    initialize_node(p);
#endif /* NEWGC */
	    off++;
	    break;
	default:
	    readnode(type,cviptr(off));
	    off++;
	    break;
	}
    }
done:

    /* read the data portion of SYMBOL/VECTOR/OBJECT/STRING/CLOSURE nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	p = &seg->sg_nodes[0] + NUM_OFFSETS;
	for (n = seg->sg_size - NUM_OFFSETS; --n >= 0; ++p)
	    switch (ntype(p)) {
	    /* $putpatch.c$: "MODULE_XLIMAGE_C_XLIRESTORE" */
	    case SYMBOL:
	    case OBJECT:
	    case VECTOR:
	    case CLOSURE:
	    case STRUCT:
#ifdef BYTECODE
	    case CPSNODE:
	    case BCODE:
#endif /* BYTECODE */
#ifdef PACKAGES
	    case PACKAGE:
#endif /* PACKAGES */
		max = getsize(p);
		if (max != 0 && (p->n_vdata = (LVAL *)VALLOC(max)) == NULL)
		    xlfatal("insufficient memory - vector");
		total += (long)(max * sizeof(LVAL));
		for (i = 0; i < max; ++i)
		    setelement(p,i,cviptr(readptr()));
		break;
	    case STRING:
	    case TVEC:
		max = getslength(p) + 1;
		size = btow_size(max);
		if ((p->n_string = (char *)VALLOC(size)) == NULL)
		    xlfatal("insufficient memory - string");
		total += (long)max;
		if (OSREAD(getstring(p),1,max,fp) != max)
		    xlfatal("image file corrupted");
		break;
#ifdef XLISP_STAT
	    case ADATA:
		max = getadsize(p);
		p->n_adaddr = (getadreloc(p)) ? StRCalloc(1, max)
		                              : StCalloc(1, max);
		if (p->n_adaddr == NULL)
		    xlfatal("insufficient memory - string");
		total += (long)max;
		if (OSREAD(getadaddr(p),1,max,fp) != max)
		    xlfatal("image file corrupted");
		break;
#endif /* XLISP_STAT */
#ifdef BIGNUMS
	    case BIGNUM:
		max = (getbignumsize(p) + 1) * sizeof(BIGNUMDATA);
		size = btow_size(max);
		if ((p->n_string = (char *)VALLOC(size)) == NULL)
		  xlfatal("insufficient memory - bignum");
		total += (long)max;
		if (OSREAD(getbignumarray(p),1,max,fp)!=max)
		  xlfatal("image file corrupted");
		break;
#endif
	    case STREAM:
#ifdef FILETABLE
                if (getfile(p) > CONSOLE) { /* actual file to modify */
                    unsigned long fpos;
                    FILEP f;

                    if (OSREAD(buf, 1, FNAMEMAX, fp) != FNAMEMAX ||
                        OSREAD(&fpos, 1, sizeof(long), fp) != sizeof(long))
                        xlfatal("image file corrupted");
                    /* open file in same type, file must exist to succeed */
                    f = ((p->n_sflags & S_BINARY)? OSBOPEN : OSAOPEN)
                        (buf, (p->n_sflags&S_FORWRITING)? OPEN_UPDATE: OPEN_RO);
                    setfile(p, f);
                    if (f != CLOSED) {/* position to same point,
                                        or end if file too short */
                        OSSEEKEND(f);
                        if (OSTELL(f) > fpos) OSSEEK(f, fpos);
                    }
                }
                break;
#else
		setfile(p, CLOSED);
		break;
#endif
	    case SUBR:
	    case FSUBR:
		p->n_subr = funtab[getoffset(p)].fd_subr;
		break;
	    }
    }

    /* close the input file */
    OSCLOSE(fp);

    /* collect to initialize the free space */
    gc();

    /* lookup all of the symbols the interpreter uses */
    xlsymbols();

    enable_interrupts();

    /* return successfully */
    return (TRUE);
}

/* freeimage - free the current memory image */
LOCAL VOID freeimage(V)
{
    SEGMENT *seg,*next;
    FILEP fp;
    LVAL p;
    int n;

    /* free the data portion of SYMBOL/VECTOR/OBJECT/STRING nodes */
    for (seg = segs; seg != NULL; seg = next) {
	p = &seg->sg_nodes[0] + NUM_OFFSETS;
	for (n = seg->sg_size - NUM_OFFSETS; --n >= 0; ++p)
	    switch (ntype(p)) {
	    /* $putpatch.c$: "MODULE_XLIMAGE_C_FREEIMAGE" */
	    case SYMBOL:
	    case OBJECT:
	    case VECTOR:
	    case CLOSURE:
	    case STRUCT:
#ifdef BYTECODE
	    case CPSNODE:
	    case BCODE:
#endif /* BYTECODE */
#ifdef PACKAGES
	    case PACKAGE:
#endif /* PACKAGES */
		if (p->n_vsize)
		  VRELEASE((ALLOCTYPE *)p->n_vdata, p->n_vsize);
		break;
	    case STRING:
	    case TVEC:
		if (getstring(p)!=NULL)
		  VRELEASE(getstring(p),btow_size(getslength(p) + 1));
		break;
#ifdef BIGNUMS
	    case BIGNUM:
		if (getbignumarray(p)!=NULL)
		  VRELEASE(getbignumarray(p),
			   btow_size((getbignumsize(p)+1)*sizeof(BIGNUMDATA)));
		break;
#endif
#ifdef XLISP_STAT
	    case ADATA:
		if (getadaddr(p) != NULL)
		  if (getadreloc(p)) StRFree(getadaddr(p));
		  else StFree(getadaddr(p));
		break;
#endif /* XLISP_STAT */
	    case STREAM:
		if (((fp = getfile(p)) != CLOSED) &&
		    (fp != STDIN && fp != STDOUT && fp != CONSOLE))  /* TAA BUG FIX */
		  OSCLOSE(fp);
		break;
	    }
	next = seg->sg_next;
	MFREE(seg);
    }
}

/* setoffset - output a positioning command if nodes have been skipped */
LOCAL VOID setoffset(V)
{
    if (off != foff) {
	OSPUTC(FREE,fp);
	writeptr(off);
	foff = off;
    }
}

/* writenode - write a node to a file */
LOCAL VOID writenode P1C(LVAL, node)
{
    OSPUTC(ntype(node),fp);
    OSWRITE(&node->n_info, sizeof(union ninfo), 1, fp);
#ifdef NEWGC
    OSPUTC(node->n_flags,fp);
#else
#ifdef MULVALS
    OSPUTC(node->n_flags,fp);
#else
    if (ntype(node) == SYMBOL) OSPUTC(node->n_flags,fp);
#endif /* MULVALS */
#endif /* NEWGC */
    foff++;
}

/* writeptr - write a pointer to a file */
LOCAL VOID writeptr P1C(OFFTYPE, off)
{
    OSWRITE(&off, sizeof(OFFTYPE), 1, fp);
}

/* readnode - read a node */
LOCAL VOID readnode P2C(int, type, LVAL, node)
{
    setntype(node, type);
    if (OSREAD(&node->n_info, sizeof(union ninfo), 1, fp) != 1)
        xlfatal("image file corrupted");
#ifdef NEWGC
    node->n_flags = OSGETC(fp);
    initialize_node(node);
#else
#ifdef MULVALS
    node->n_flags = OSGETC(fp);
#else
    if (type == SYMBOL) node->n_flags = OSGETC(fp);
#endif /* MULVALS */
#endif /* NEWGC */
#ifdef HASHFCNS
    /* to get hash tables rehashed */
    if (type == STRUCT) setnuflags(node, TRUE);
#endif /* HASHFCNS */
}

/* readptr - read a pointer */
LOCAL OFFTYPE readptr(V)
{
    OFFTYPE off;
    if (OSREAD(&off, sizeof(OFFTYPE), 1, fp) != 1)
        xlfatal("image file corrupted");
    return (off);
}

/* cviptr - convert a pointer on input */
LOCAL LVAL cviptr P1C(OFFTYPE, o)
{
    OFFTYPE off = (OFFTYPE)2;
    SEGMENT *seg;

    /* check for nil */
    if (o == FILENIL)
	return (NIL);

    /* compute a pointer for this offset */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	if (o < off + (OFFTYPE)seg->sg_size - NUM_OFFSETS)
	    return (seg->sg_nodes + NUM_OFFSETS + (unsigned int)(o - off));
	off += (OFFTYPE)seg->sg_size - NUM_OFFSETS;
    }

    /* create new segments if necessary */
    for (;;) {

	/* create the next segment */
	if ((seg = newsegment(anodes)) == NULL)
	    xlfatal("insufficient memory - segment");

	/* check to see if the offset is in this segment */
	if (o < off + (OFFTYPE)seg->sg_size - NUM_OFFSETS)
	    return (seg->sg_nodes + NUM_OFFSETS + (unsigned int)(o - off));
	off += (OFFTYPE)seg->sg_size - NUM_OFFSETS;
    }
}

/* cvoptr - convert a pointer on output */
LOCAL OFFTYPE cvoptr P1C(LVAL, p)
{
    OFFTYPE off = (OFFTYPE)2;
    SEGMENT *seg;
    OFFTYPE np = CVPTR(p);

    /* check for nil */
    if (null(p))
	return (FILENIL);

    /* compute an offset for this pointer */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
      if (np >= CVPTR(&seg->sg_nodes[NUM_OFFSETS]) &&
	  np <  CVPTR(&seg->sg_nodes[seg->sg_size]))
	return (off + (p-(seg->sg_nodes+NUM_OFFSETS)));
      off += (OFFTYPE)seg->sg_size - NUM_OFFSETS;
    }

    /* pointer not within any segment */
    xlerror("bad pointer found during image save",p);
    return (0); /* fake out compiler warning */
}
#endif
