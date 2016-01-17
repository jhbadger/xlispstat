/* xsdynload - Dynamic loading and C function calling routines.        */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/* Calling conventions are based on the conventions given in the New S */
/* book. Calling conventions for dyn-load are based on a combination   */
/* KCL's si:faslink conventions and Bill Dunlap's dyn.load2 for New S. */
/*                                                                     */
/* The dynamic loading code is based on the KCL faslink function and   */
/* Bill Dunlap's dynamic loader for New S.                             */

#include "xlisp.h"
#ifdef FOREIGNCALL
#include "xlstat.h"

extern char *progname;
extern LVAL k_fortran, k_libflags, s_cfun_table;

typedef int  (*pfi_t)();     /* pointer to function returning integer. */
typedef LVAL (*pfl_t)();     /* pointer to function returning LVAL.    */

#define HASHSIZE 397

static int verbose;
static char lbuf[100];

#include "foreign.h"
#ifdef MEMPROT
#include <sys/mman.h>
#endif

/* forward declarations */
LOCAL char *get_caddress _((char *name));
#ifndef SHLIB_DYNLOAD
#ifdef STATIC_LOAD_ONLY
LOCAL link_and_load _((char *fname, char *libs, int fort));
#else
#ifndef HAS_OWN_DYNLOAD
LOCAL VOID link_and_load _((char *fname, char *libs, int fort));
LOCAL int code_size _((char *tmpfname, char *code_start));
LOCAL VOID read_code _((char *tmpfname, char *addr));
LOCAL VOID enter_symbols _((char *tmpfname));
#endif /* HAS_OWN_DYNLOAD */
#endif /* STATIC_LOAD_ONLY */
#endif /* SHLIB_DYNLOAD */


/************************************************************************/
/**                                                                    **/
/**           Public Allocation and Error Signalling Functions         **/
/**                                                                    **/
/************************************************************************/

static LVAL current_allocs = NULL;
#define fixup_current_allocs \
  { if (current_allocs == NULL) current_allocs = NIL; }

/* allocate space that will be garbage collected after return */
char *xscall_alloc(n, m)
     int n, m;
{
  LVAL adata;
  char *p = NULL;

  fixup_current_allocs;

  adata = newadata(n, m, FALSE);
  if (adata == NIL || (p = getadaddr(adata)) == NULL)
    xlfail("allocation failed");
  current_allocs = cons(adata, current_allocs);
  return(p);
}

/* error routint for use within C functions */
VOID xscall_fail(s) char *s; { xlfail(s); }

/************************************************************************/
/**                                                                    **/
/**                Lisp to C/FORTRAN Data Conversion                   **/
/**                                                                    **/
/************************************************************************/

#define IN 0
#define RE 1
#define MAXARGS 15

typedef struct {
  int type, size;
  char *addr;
} call_arg;

/* convert lisp argument to allocated pointer */
LOCAL call_arg lisp2arg(x)
     LVAL x;
{
  call_arg a;
  LVAL elem, data;
  int i;

  xlprot1(x);

  /* make sure x is a sequence and find its length */
  if (! seqp(x)) x = consa(x);
  a.size = seqlen(x);

  /* determine the mode of the data */
  for (i = 0, a.type = IN, data = x; i < a.size; i++) {
    elem = getnextelement(&data, i);
    if (floatp(elem)) a.type = RE;
#ifdef BIGNUMS
    else if (ratiop(elem)) a.type = RE;
#endif
    else if (! integerp(elem)) xlerror("not a real number", elem);
  }

  /* allocate space for the data */
  a.addr = xscall_alloc(a.size, (a.type == IN) ? sizeof(int) : sizeof(double));

  /* fill the space */
  for (i = 0, data = x; i < a.size; i++) {
    elem = getnextelement(&data, i);
    if (a.type == IN) ((int *) a.addr)[i] = getfixnum(elem);
#if !(defined(ibm032) && defined(__HIGHC__))
    else ((double *) a.addr)[i] = makefloat(elem);
#else /* avoid bug in hc 2.1n C compiler on IBM RT running AOS 4.3 */
    else {
      double *dbl = &((double *)a.addr)[i] ;
      *dbl = makefloat(elem) ;
    }
#endif
  }
  
  xlpop();
  return(a);
}

/* copy allocated pointer back to new lisp list */
LOCAL LVAL arg2lisp(a)
     call_arg a;
{
  LVAL x, next;
  int i;

  xlsave1(x);
  x = mklist(a.size, NIL);
  for (i = 0, next = x; i < a.size; i++, next = cdr(next)) {
    if (a.type == IN) rplaca(next, cvfixnum((FIXTYPE) ((int *) a.addr)[i]));
    else rplaca(next, cvflonum((FLOTYPE) ((double *) a.addr)[i]));
  }
  xlpop();
  return(x);
}

/************************************************************************/
/**                                                                    **/
/**                Foreign Function Call Functions                     **/
/**                                                                    **/
/************************************************************************/

LOCAL LVAL call_foreign(which)
     int which;
{
  LVAL result, name, old_allocs, next;
  call_arg args[MAXARGS], *pargs;
  int nargs;
  int (*routine)();
  char *get_caddress();
  char *pattern;

  fixup_current_allocs;

  xlstkcheck(3);
  xlsave(old_allocs);
  xlprotect(current_allocs);
  xlsave(result);
  old_allocs = current_allocs;
  current_allocs = NIL;

  /* get the routine pointer */
  name = xlgastring();
  pattern = (which == 'C') ? INTERNAL_CNAME_PATTERN : INTERNAL_FNAME_PATTERN;
  sprintf(lbuf, pattern, getstring(name));
  routine = (pfi_t) get_caddress(lbuf);
  if (! routine) xlerror("can't find internal symbol by this name", name);

  /* convert the arguments to allocated pointers */
  for (nargs = 0; moreargs(); nargs++) {
    if (nargs >= MAXARGS) xlfail("too many arguments");
    args[nargs] = lisp2arg(xlgetarg());
  }

  /* make the call -- there must be a better way to do this */
  switch (nargs) {
  case  0: routine(); break;
  case  1: routine(args[0].addr); break;
  case  2: routine(args[0].addr, args[1].addr); break;
  case  3: routine(args[0].addr, args[1].addr, args[2].addr); break;
  case  4: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr);
    break;
  case  5: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr);
    break;
  case  6:
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr,
	    args[4].addr, args[5].addr); 
    break;
  case  7:
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr,
	    args[4].addr, args[5].addr, args[6].addr); 
    break;
  case  8:
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr, args[5].addr, args[6].addr, args[7].addr); 
    break;
  case  9: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr, args[5].addr, args[6].addr, args[7].addr, 
	    args[8].addr); 
    break;
  case 10: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr, args[5].addr, args[6].addr, args[7].addr, 
	    args[8].addr, args[9].addr); 
    break;
  case 11: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr, args[5].addr, args[6].addr, args[7].addr, 
	    args[8].addr, args[9].addr, args[10].addr); 
    break;
  case 12: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr, args[5].addr, args[6].addr, args[7].addr, 
	    args[8].addr, args[9].addr, args[10].addr, args[11].addr); 
    break;
  case 13: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr, args[5].addr, args[6].addr, args[7].addr, 
	    args[8].addr, args[9].addr, args[10].addr, args[11].addr,
	    args[12].addr); 
    break;
  case 14: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr, args[5].addr, args[6].addr, args[7].addr, 
	    args[8].addr, args[9].addr, args[10].addr, args[11].addr,
	    args[12].addr,  args[13].addr); 
    break;
  case 15: 
    routine(args[0].addr, args[1].addr, args[2].addr, args[3].addr, 
	    args[4].addr, args[5].addr, args[6].addr, args[7].addr, 
	    args[8].addr, args[9].addr, args[10].addr, args[11].addr,
	    args[12].addr,  args[13].addr, args[14].addr); 
    break;
  }
  
  /* convert the pointers back to lists, grouped in a list */
  result = (nargs > 0) ? mklist(nargs, NIL) : NIL;
  for (next = result, pargs = args; consp(next); next = cdr(next), pargs++)
    rplaca(next, arg2lisp(*pargs));
  
  current_allocs = old_allocs;
  xlpopn(3);

  return(result);
}

/* CALL-CFUN */
LVAL xscall_cfun() { return(call_foreign('C')); }

/* CALL-FSUB */
LVAL xscall_fsub() { return(call_foreign('F')); }

/* CALL-LFUN */
LVAL xscall_lfun()
{
  LVAL name, old_allocs, result;
  LVAL (*routine)();
  char *get_caddress();
  
  fixup_current_allocs;

  xlstkcheck(2);
  xlsave(old_allocs);
  xlprotect(current_allocs);
  old_allocs = current_allocs;
  current_allocs = NIL;

  name = xlgastring();
  sprintf(lbuf, INTERNAL_CNAME_PATTERN, getstring(name));

  routine = (pfl_t) get_caddress(lbuf);
  if (! routine) xlerror("can't find internal symbol by this name", name);
  result = routine();
  current_allocs = old_allocs;
  xlpopn(2);

  return(result);
}

/************************************************************************/
/**                                                                    **/
/**                   Fake COFF ldfcn's for BSD                        **/
/**                                                                    **/
/************************************************************************/

#ifdef STDBSD

#define LDFILE FILE
#define SYMENT struct nlist
#define SUCCESS TRUE
#define FAILURE FALSE
#define LDNAMELIMIT 100
#define AOUTHDR struct exec
#define SCNHDR AOUTHDR
#define FREAD fread

static struct exec header;
static char ldnamebuf[LDNAMELIMIT];

LOCAL LDFILE *ldopen(name, dummy)
     char *name, *dummy;
{
  LDFILE *fp;

  if ((fp = fopen(name, "r")) == NULL) xlfail("cannot open ld file");
  if (fread((char *) &header, sizeof(header), 1, fp) != 1 ||
      feof(fp) || ferror(fp)) {
    fclose(fp);
    fp = NULL;
  }
  return(fp);
}

LOCAL ldtbread(fp, i, psym)
     LDFILE *fp;
     int i;
     SYMENT *psym;
{
  if (i < 0 || i >= header.a_syms / sizeof(SYMENT)) return(FAILURE);
  if (fseek(fp, N_SYMOFF(header) + i * sizeof(SYMENT), 0) < 0) return(FAILURE);
  if (fread((char *) psym, sizeof(SYMENT), 1, fp) != 1 ||
      feof(fp) || ferror(fp)) return(FAILURE);
  return(SUCCESS);
}

LOCAL char *ldgetname(fp, psym)
     LDFILE *fp;
     SYMENT *psym;
{
  char *bp = ldnamebuf;
  long which = psym->n_un.n_strx;
  int i = 0;

  *bp = '\0';
  if (which) {
    ok_fseek(fp, N_STROFF(header) + which, 0);
    while ((*bp++ = getc(fp)) != '\0') 
      if (++i >= LDNAMELIMIT) xlfail("name too long for ld buffer");
  }
  return(ldnamebuf);
}

LOCAL ldohseek(fp)
     LDFILE *fp;
{
  if (fseek(fp, 0, 0) < 0) return(FAILURE);
  else return(SUCCESS);
}

LOCAL ldclose(fp)
     LDFILE *fp;
{
  fclose(fp);
  return(SUCCESS);
}

#endif /* STDBSD */

/************************************************************************/
/**                                                                    **/
/**                   Dynamic Loading Functions                        **/
/**                                                                    **/
/************************************************************************/

#ifndef SHLIB_DYNLOAD
#define round_up(a, d) ((long)(a)%(d) ? (d)*((long)(a)/(d) + 1) : (long)(a))

#ifdef STDBSD
#define SYMVALUE(sym) ((char *) ((sym).n_value))
#ifndef SYM_IS_GLOBAL_FUNCTION
#define SYM_IS_GLOBAL_FUNCTION(ldptr,symbol) \
  (((symbol).n_type & N_TYPE) == N_TEXT  && ((symbol).n_type & N_EXT))
#endif /* SYM_IS_GLOBAL_FUNCTION */
#endif /* STDBSD */
#endif /* SHLIB_DYNLOAD */

/* DYN-LOAD function */
LVAL xsdynload()
{
  char *name, *libs;
  LVAL flag, arg;
  int fort;

  name = (char *) getstring(xlgastring());
  if (! xlgetkeyarg(k_verbose, &flag)) flag = (VERBDFLT) ? s_true : NIL;
  verbose = flag != NIL;
  if (! xlgetkeyarg(k_fortran, &flag)) flag = NIL;
  fort = flag != NIL;
  if (xlgetkeyarg(k_libflags, &arg) && stringp(arg))
    libs = (char *) getstring(arg);
  else libs = "";

  link_and_load(name, libs, fort);

  return(s_true);
}

#ifndef SHLIB_DYNLOAD
LOCAL VOID enter_csymbol(name, addr)
     char *name, *addr;
{
  LVAL table, list, entry;
  int i;
  static initialized = FALSE;

  if (! initialized) {
    setvalue(s_cfun_table, newvector(HASHSIZE));
    initialized = TRUE;
  }
  
  table = getvalue(s_cfun_table);
  if (vectorp(table)) {
    i = hash(name, getsize(table));
    
    /* see if name is already in the table; replace its value if it is */
    for (list = getelement(table, i); consp(list); list = cdr(list)) {
      entry = car(list);
      if (stringp(car(entry)) && strcmp(name, getstring(car(entry))) == 0) {
	rplacd(entry, cvfixnum((FIXTYPE) addr));
	return;
      }
    }

    /* otherwise (not returned yet) make a new entry */
    entry = cons(NIL, NIL);
    setelement(table, i, cons(entry, getelement(table, i)));
    rplaca(entry, cvstring(name));
    rplacd(entry, cvfixnum((FIXTYPE) addr));
  }
}

LOCAL char *find_hash_entry(name)
     char *name;
{
  LVAL table, entry, list;
  int i;

  if (! symbolp(s_cfun_table)) return(NULL);

  table = getvalue(s_cfun_table);
  if (vectorp(table)) {
    i = hash(name, getsize(table));
    for (list = getelement(table, i); consp(list); list = cdr(list)) {
      entry = car(list);
      if (stringp(car(entry)) && strcmp(name, getstring(car(entry))) == 0)
	return((fixp(cdr(entry))) ? (char *) getfixnum(cdr(entry)) : NULL);
    }
  }
  return (NULL);
}

LOCAL char *get_caddress(name)
     char *name;
{
  struct nlist nl[2];
  char *addr;

  if ((addr = find_hash_entry(name)) != NULL) return(addr);
  else {
#ifdef COFF_FORMAT
    nl[0].n_name = name;
    nl[1].n_name = "";
#else
    nl[0].n_un.n_name = name;
    nl[1].n_un.n_name = "";
#endif
    if (nlist(progname, nl) == -1)
      xlfail("file not found or invalid name list");
    if((addr = (char *) nl[0].n_value) != NULL) {
      enter_csymbol(name, addr);
      return(addr);
    }
    else return (NULL);
  }
}

#ifdef STATIC_LOAD_ONLY
LOCAL link_and_load(fname, libs, fort)
     char *fname, *libs;
     int fort;
{
  xlfail("dynamic loading not available on this system");
}
#else
#ifndef HAS_OWN_DYNLOAD
LOCAL VOID link_and_load(fname, libs, fort)
     char *fname, *libs;
     int fort;
{
  char tmpfname[TMPNAMESIZE];
  char *code_start, *addr, *syslibs;
  int size, size_guess;
  
  /* make the libstring, the tempfile name and the initial code space */
  syslibs = (fort) ? FLIBS : CLIBS;
  sprintf(tmpfname, TMPPATTERN, getpid());
  size_guess = MIN_ALLOC;
  addr = calloc(1, size_guess);
  if (addr == NULL) xlfail("can't make initial code allocation");
  code_start = (char *) round_up(addr, PAGE_SIZE);
  size_guess -= (long) (code_start - addr); 
#ifdef MEMPROT
  mprotect(code_start, size_guess,(PROT_READ|PROT_WRITE|PROT_EXEC));
#endif

  /* do an incremental load of the file and libs against xlisp */
  sprintf(buf, LDPATTERN, 
	  progname, (char *) code_start, fname, libs, syslibs, tmpfname);
  if (verbose) printf("first ld pass\n%s\n", buf);
  if (system(buf) != 0) {
    free(addr);
    xlfail("link failed");
  }

  /* check the code size and redo the load if needed */
  size = code_size(tmpfname, code_start);
  if (size_guess < size) {
    free(addr);
    addr = calloc(1, size + PAGE_SIZE);
    if (addr == NULL) xlfail("can't make code allocation");
    code_start = (char *) round_up(addr, PAGE_SIZE);
#ifdef MEMPROT
    mprotect(code_start, size, (PROT_READ|PROT_WRITE|PROT_EXEC));
#endif
    sprintf(buf, LDPATTERN, 
	    progname, (char *) code_start, fname, libs, syslibs, tmpfname);
    if (verbose) printf("second ld pass\n%s\n", buf);
    if (system(buf) != 0) { 
      free(addr); 
      xlfail("link failed"); 
    }
    if (size < code_size(tmpfname, code_start)) {
      free(addr);
      xlfail("can't figure out tempfile size");
    }
  }

  /* read in the object file */
  if (verbose) printf("reading in the code ..."); fflush(stdout);
  read_code(tmpfname, code_start);
  if (verbose) printf("done\n");

  /* enter the external symbols into the hash table */
  if (verbose) printf("entering symbols..."); fflush(stdout);
  enter_symbols(tmpfname);
  if (verbose) printf("done\n");

  /* unlink the tempfile */
  unlink(tmpfname);
}

LOCAL int code_size(tmpfname, code_start)
     char *tmpfname, *code_start;
{
  LDFILE *fp;
  AOUTHDR header;
  SCNHDR scnheader;
  int size;

  if ((fp = ldopen(tmpfname, NULL)) == NULL)
    xlfail("cannot open temporary ld file");

  if (ldohseek(fp) == FAILURE) xlfail("could not seek to a.out header");
  if (FREAD((char *) &header, sizeof(header), 1, fp) < 1)
    xlfail("could not read a.out header");

#ifdef COFF_FORMAT
  /* read last section header and measure size from code start */
  /* section numbers begin with one!                           */
  if (ldshread(fp, (unsigned short) N_SECTIONS(fp), &scnheader)==FAILURE)
    xlfail("cannot read object file section");
  size = SCN_ADDR(fp, scnheader) + SCN_LENGTH(fp, scnheader) 
    - (long) code_start;
#else
  size = header.a_text + header.a_data + header.a_bss;
#endif /* COFF_FORMAT */

  if (ldclose(fp) == FAILURE) xlfail("cannot close tempfile");
  return(size);
}

LOCAL VOID read_code(tmpfname, addr)
     char *tmpfname, *addr;
{
  LDFILE *fp;
  AOUTHDR header;
  SCNHDR scnheader;
  int size, i;

  if ((fp = ldopen(tmpfname, NULL)) == NULL)
    xlfail("cannot open temporary ld file");
  
  if (ldohseek(fp) == FAILURE) xlfail("could not seek to a.out header");
  if (FREAD((char *) &header, sizeof(header), 1, fp) < 1)
    xlfail("could not read a.out header");

#ifdef COFF_FORMAT
  /* read in code and data sections, zero out bss sections */
  /* zeroing should not be needed since space came from    */
  /* calloc, but it can't hurt.                            */
  /* section numbers begin with one!                       */
  for (i = 1 ; i <= N_SECTIONS(fp) ; i++) {
    if (ldshread(fp, (unsigned short) i, &scnheader)==FAILURE)
      xlfail("cannot read object file section");
    if (SCN_IS_BSS(fp, scnheader))
      bzero((char *) SCN_ADDR(fp, scnheader),
	    (int) SCN_LENGTH(fp, scnheader));
    else {
      if (FSEEK(fp, SCN_FILE_LOC(fp, scnheader), 0) == -1)
	xlfail("could not seek to object file section");
      if (FREAD((char *) SCN_ADDR(fp, scnheader), 1,
		(int) SCN_LENGTH(fp, scnheader), fp)
	  < SCN_LENGTH(fp, scnheader))
	xlfail("could not read object file section");
    }
  }
#else
  ok_fseek(fp, (long) N_TXTOFF(header), 0);
  size = header.a_text + header.a_data;
  ok_fread((char *) addr, 1, size, fp);
#endif
  
  if (ldclose(fp) == FAILURE) xlfail("cannot close tempfile");
}

LOCAL VOID enter_symbols(tmpfname)
     char *tmpfname;
{
  LDFILE *input;
  SYMENT symbol;
  char *symname, *symaddr;
  int i;

  /* open the file */
  if ((input = ldopen(tmpfname, NULL)) == NULL)
    xlfail("cannot open tempfile for symbol reading");

  /* process symbols while they last */
  i = 0;
  while (ldtbread(input, i, &symbol) == SUCCESS) {
    i++;
    if (SYM_IS_GLOBAL_FUNCTION(input, symbol)) {
      symname = ldgetname(input, &symbol);
      symaddr = SYMVALUE(symbol);
      enter_csymbol(symname, symaddr);
    }
  }
  if (ldclose(input) == FAILURE) xlfail("cannot close tempfile");
}

/************************************************************************/
/**                                                                    **/
/**                       Utility Functions                            **/
/**                                                                    **/
/************************************************************************/

LOCAL VOID ok_fread(ptr, size, nitems, stream)
     char *ptr;
     int size, nitems;
     FILE *stream;
{
  if (fread(ptr, size, nitems, stream) != nitems ||
      feof(stream) || ferror(stream))
    xlfail("error while reading disk file");
}

LOCAL VOID ok_fseek(stream, offset, ptrname)
     FILE *stream;
     long offset;
     int ptrname;
{
  if (fseek(stream, offset, ptrname) < 0)
    xlfail("error while seeking on disk file");
}
#endif /* HAS_OWN_DYNLOAD */
#endif /* STATIC_LOAD_ONLY */
#endif /* SHLIB_DYNLOAD */
#endif /* FOREIGNCALL */
