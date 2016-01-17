/* xlglobals - xlisp global variables */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* symbols */
struct node isnil;

LVAL s_true=NULL,obarray=NULL;
LVAL s_otherwise=NULL;
LVAL s_features=NULL,s_typespec=NULL;
LVAL s_unbound=NIL,s_dot=NULL;
LVAL s_quote=NULL,s_identity=NULL,s_function=NULL;
LVAL s_bquote=NULL,s_comma=NULL,s_comat=NULL;
LVAL s_evalhook=NULL,s_applyhook=NULL,s_tracelist=NULL;
LVAL s_lambda=NULL,s_macro=NULL;
LVAL s_destructbind = NULL;
LVAL s_stdin=NULL,s_stdout=NULL,s_stderr=NULL,s_debugio=NULL,s_traceout=NULL;
LVAL s_rtable=NULL;
LVAL a_readpw=NULL;
LVAL s_tracenable=NULL,s_tlimit=NULL,s_breakenable=NULL;
LVAL s_baktraceprargs=NULL;
LVAL s_setf=NULL,s_setfl=NULL,s_car=NULL,s_cdr=NULL,s_nth=NULL,s_apply=NULL;
LVAL s_aref=NULL,s_get=NULL,s_getf=NULL;
LVAL s_row_major_aref=NULL;
LVAL s_svalue=NULL,s_sfunction=NULL,s_splist=NULL;
LVAL s_eq=NULL,s_eql=NULL,s_equal=NULL,s_gcflag=NULL,s_gchook=NULL;
#ifdef BIGNUMS
LVAL s_readbase=NULL, s_printbase=NULL;
#else
LVAL s_ifmt=NULL;
#endif
LVAL s_ffmt=NULL;
LVAL s_batchmode=NULL;
LVAL s_1plus=NULL,s_2plus=NULL,s_3plus=NULL;
LVAL s_1star=NULL,s_2star=NULL,s_3star=NULL;
LVAL s_minus=NULL,s_printcase=NULL;
LVAL s_printlevel=NULL, s_printlength=NULL;
LVAL s_printgensym=NULL, s_printreadably=NULL, s_printescape=NULL;
LVAL s_dispmacros=NULL;
LVAL s_strtypep=NULL, s_mkstruct=NULL, s_cpystruct=NULL;
LVAL s_prntfunc=NULL,s_strinclude=NULL,s_strconstruct=NULL;
LVAL s_strref=NULL, s_strset=NULL;
LVAL s_x=NULL, s_s=NULL, s_sslots=NULL;
LVAL s_elt = NULL;
LVAL a_list=NULL, a_number=NULL, a_null=NULL, a_atom=NULL, a_anystream=NULL;
LVAL s_and=NULL, s_or=NULL, s_not=NULL, s_satisfies=NULL, s_member=NULL;
LVAL a_struct = NULL;
LVAL s_read_suppress=NULL;
#ifdef HASHFCNS
LVAL s_gethash = NULL, a_hashtable = NULL;
#endif
LVAL a_complex = NULL, a_rndstate = NULL, s_rndstate = NULL;
#ifdef READTABLECASE
LVAL s_rtcase=NULL;
#endif
#ifdef IEEEFP
LVAL s_posinfinity=NULL, s_neginfinity=NULL, s_notanumber=NULL;
#endif
#ifdef PACKAGES
LVAL xlisppack=NULL,xlkeypack=NULL,xluserpack=NULL,s_package=NULL;
LVAL s_printsympack=NULL;
#endif /* PACKAGES */
#ifdef CONDITIONS
LVAL s_condition_hook=NULL,s_error=NULL,s_cerror=NULL,s_signal=NULL;
LVAL s_warn=NULL,s_break=NULL,s_debug=NULL;
LVAL s_unboundvar=NULL,s_unboundfun=NULL,k_name=NULL;
#endif /* CONDITIONS */
LVAL s_intaction=NULL;
#ifdef PRINTCIRCLE
LVAL s_printcircle = NULL, s_prcircdat = NULL, s_rdcircdat = NULL;
#endif /* PRINTCIRCLE */
LVAL s_load=NULL;

/* keywords */
LVAL k_and=NULL, k_or=NULL, k_not=NULL;
LVAL k_test=NULL,k_tnot=NULL;
LVAL k_wspace=NULL,k_const=NULL,k_nmacro=NULL,k_tmacro=NULL;
LVAL k_sescape=NULL,k_mescape=NULL;
LVAL k_direction=NULL,k_input=NULL,k_output=NULL;
LVAL k_start=NULL,k_end=NULL,k_1start=NULL,k_1end=NULL;
LVAL k_2start=NULL,k_2end=NULL,k_fromend=NULL,k_count=NULL;
LVAL k_verbose=NULL,k_print=NULL;
LVAL k_upcase=NULL,k_downcase=NULL,k_capitalize=NULL;
LVAL k_io=NULL, k_elementtype=NULL;
LVAL s_termio=NULL, k_exist=NULL, k_nexist=NULL, k_error=NULL;
LVAL k_rename=NULL, k_newversion=NULL, k_overwrite=NULL, k_append=NULL;
LVAL k_supersede=NULL, k_rendel=NULL, k_probe=NULL, k_create=NULL;
LVAL k_concname=NULL, k_include=NULL, k_prntfunc=NULL, k_construct=NULL;
LVAL k_predicate=NULL;
LVAL k_initelem=NULL, k_initcont=NULL,  k_displacedto=NULL;
LVAL k_allow_other_keys = NULL;	 /* TAA added 9/93 */
LVAL k_ivalue=NULL;
#ifdef KEYARG
LVAL k_key=NULL;
#endif
#ifdef HASHFCNS
LVAL k_size = NULL,k_rhthresh=NULL,k_rhsize;
#endif
#ifdef READTABLECASE
LVAL k_preserve=NULL,k_invert=NULL;
#endif
#ifdef PACKAGES
LVAL k_nicknames=NULL, k_use=NULL;
#ifdef MULVALS
LVAL k_internal=NULL, k_external=NULL, k_inherited=NULL;
#endif /* MULVALS */
#endif /* PACKAGES */

/* lambda list keywords */
LVAL lk_optional=NULL,lk_rest=NULL,lk_key=NULL,lk_aux=NULL;
LVAL lk_whole=NULL,lk_body=NULL,lk_environment=NULL;
LVAL lk_allow_other_keys=NULL;

/* type names */
LVAL a_subr=NULL,a_fsubr=NULL;
LVAL a_cons=NULL,a_symbol=NULL,a_fixnum=NULL,a_flonum=NULL;
LVAL a_string=NULL,a_object=NULL,a_stream=NULL,a_vector=NULL;
LVAL a_closure=NULL,a_char=NULL,a_ustream=NULL;
LVAL a_integer=NULL, a_real=NULL;
LVAL a_array = NULL;           /* L. Tierney */
#ifdef BYTECODE
LVAL a_bcclosure = NULL;
LVAL a_cpsnode = NULL;
LVAL a_bcode = NULL;
LVAL s_stdrtable = NULL;
#endif /* BYTECODE */
#ifdef BIGNUMS
LVAL a_ratio=NULL;
LVAL a_rational=NULL;
LVAL a_bignum=NULL;
LVAL a_unbyte=NULL, a_sbyte=NULL;
LVAL n_bigzero=NULL, n_bigmone=NULL;
#endif
#ifdef PACKAGES
LVAL a_package=NULL;
#endif /* PACKAGES */
LVAL k_symbol_macro=NULL;
#ifdef XLISP_STAT
LVAL a_adata=NULL;
#endif /* XLISP_STAT */
LVAL a_ptr=NULL;
LVAL a_weakbox=NULL;
LVAL a_tvec=NULL;
LVAL s_c_char=NULL,s_c_short=NULL,s_c_int=NULL,s_c_long=NULL,s_c_float=NULL,
  s_c_double=NULL,s_c_complex=NULL,s_c_dcomplex=NULL,s_make_array=NULL,
  s_c_uchar=NULL;

/* Object system */
LVAL s_self=NULL,k_new=NULL,k_isnew=NULL;
LVAL k_prin1=NULL;
LVAL cls_class=NULL,cls_object=NULL;
#ifdef MSDOS
LVAL k_data=NULL, k_type=NULL, k_item=NULL, k_timeout=NULL, k_request=NULL;
#endif /* MSDOS */
#ifdef MACINTOSH
LVAL k_appllistlabel=NULL, k_canswitch=NULL, k_data=NULL;
LVAL k_object=NULL, k_prompt=NULL, k_signature=NULL, k_timeout=NULL;
LVAL k_type=NULL, k_waitreply=NULL, k_zone=NULL;
#ifndef CONDITIONS
LVAL k_name=NULL;
#endif /* CONDITIONS */
#endif /* MACINTOSH */

/* documentation symbols */
LVAL s_keepdocs, s_fundoc, s_vardoc;

/* keyword handling symbols */
LVAL s_strict_keywords = NULL;

/* evaluation variables */
LVAL **xlstack = NULL,**xlstkbase = NULL,**xlstktop = NULL;
LVAL xlenv=NULL,xlfenv=NULL,xldenv=NULL;

/* argument stack */
LVAL *xlargstkbase = NULL;	/* argument stack base */
LVAL *xlargstktop = NULL;	/* argument stack top */
LVAL *xlfp = NULL;		/* argument frame pointer */
LVAL *xlsp = NULL;		/* argument stack pointer */
LVAL *xlargv = NULL;		/* current argument vector */
int xlargc = 0;			/* current argument count */

/* exception handling variables */
CONTEXT *xlcontext = NULL;	/* current exception handler */
CONTEXT *xltarget = NULL;	/* target context (for xljump) */
LVAL xlvalue=NULL;		/* exception value (for xljump) */
int xlmask=0;			/* exception type (for xljump) */
#ifdef MULVALS
int xlnumresults = 0;		/* number of values */
LVAL *xlresults;		/* multiple values array */
#endif /* MULVALS */

/* Garbage collection reporting */
long gccalls=0;
long nfree=0, total=0;

/* Weak box list */
LVAL xlweakboxes = NIL;

/* debugging variables */
int xldebug = 0;		/* debug level */
int xlsample = 0;		/* control character sample rate */
int xltrcindent = 0;		/* trace indent level */

/* gensym variables */
char gsprefix[STRMAX+1] = { 'G',0 };	/* gensym prefix string */
FIXTYPE gsnumber = 1;		/* gensym number */

/* i/o variables */
FILEP tfp = CLOSED;		/* transcript file pointer */
int redirectout = FALSE;	/* output is redirected */
int redirectin = FALSE;		/* input is redirected */
int batchmode = FALSE;		/* running a batch process */
int lposition = 0;              /* postition in screen */

/* From Luke Tierney, 9/93 */
/* startup functions and command line symbols */
LVAL s_startup_functions=NULL, s_command_line=NULL;
LVAL s_loadfileargs=NULL, s_toplevelloop=NULL, s_exit_functions = NULL;

/* general purpose string buffer */
char buf[STRMAX*2+1] = { 0 };

/* Number of remaining nodes */
long nnodes;

/* printing level and length */
int plevel, plength;

#ifdef STSZ
/* For stack checking */
int stackwarn=FALSE;	/* is TRUE when warning given */
int marghi=MARGLO;	    /* stackleft for warning */
#endif

#ifdef XLISP_STAT
/* command line path */
char *defaultpath=NULL;
#endif /* XLISP_STAT */

#ifdef FILETABLE
FILETABLETYPE filetab[FTABSIZE] = { {0,0} }; /* Just a small initializer */
					     /* to ensure it's all zeros */
#endif

#ifdef ASCII8
#ifdef ANSI8
/* ANSI table different than IBM ASCII */
char ascii8tbl[256]= {
/* 0x0 - 0x1f */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
/* 0x20 - 0x3f */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
/* 0x40 - 0x5f */
    0,	 UC8, UC8, UC8, UC8, UC8, UC8, UC8,
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8,
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8,
    UC8, UC8, UC8, 0,	0,   0,	  0,   0,
/* 0x60 - 0x7f */
    0,	 LU8, LU8, LU8, LU8, LU8, LU8, LU8,
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8,
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8,
    LU8, LU8, LU8, 0,	0,   0,	  0,   0,
/* 0x80 - 0xbf */
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
/* 0xc0 - 0xdf */
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8,
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8,
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, 0,
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, 0,
/* 0xe0 - 0xff */
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8,
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8,
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, 0,
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, 0};

unsigned char ascii8cnv[]= {
	 'a', 'b', 'c', 'd', 'e', 'f', 'g',
    'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z', 0, 0, 0, 0, 0,
    0,	 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', 0, 0, 0, 0, 0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0,	 0,   0,   0,	0,   0,	  0,   0,
    0Xe0,0Xe1,0Xe2,0Xe3,0Xe4,0Xe5,0Xe6,0Xe7,
    0Xe8,0Xe9,0Xea,0Xeb,0Xec,0Xed,0Xee,0Xef,
    0Xf0,0Xf1,0Xf2,0Xf3,0Xf4,0Xf5,0Xf6,0,
    0Xf8,0Xf9,0Xfa,0Xfb,0Xfc,0Xfd,0Xfe,0,
    0Xc0,0Xc1,0Xc2,0Xc3,0Xc4,0Xc5,0Xc6,0Xc7,
    0Xc8,0Xc9,0Xca,0Xcb,0Xcc,0Xcd,0Xce,0Xcf,
    0Xd0,0Xd1,0Xd2,0Xd3,0Xd4,0Xd5,0Xd6,0,
    0Xd8,0Xd9,0Xda,0Xdb,0Xdc,0Xdd,0Xde,0};

#else
/* conversion tables for 8 bit ASCII */
char ascii8tbl[256]= { 
/* 0x0 - 0x1f */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0,
/* 0x20 - 0x3f */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0,
/* 0x40 - 0x5f */
    0,   UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, 0,   0,   0,   0,   0,
/* 0x60 - 0x7f */
    0,   LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, 0,   0,   0,   0,   0,
/* msb set */
    UC8, LU8, LU8, LC8, LU8, LC8, LU8, LU8,
    LC8, LC8, LC8, LC8, LC8, LC8, UC8, UC8,
    UC8, LU8, UC8, LU8, LC8, LC8, LC8, LC8,
    LC8, UC8, UC8, 0,   0,   0,   0,   0,
    LC8, LC8, LC8, LC8, LU8, UC8 };

unsigned char ascii8cnv[101]= {
         'a', 'b', 'c', 'd', 'e', 'f', 'g',
    'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z', 0, 0, 0, 0, 0,
    0,   'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', 0, 0, 0, 0, 0,
    0x87, 0x9a, 0x90, 0x83, 0x8e, 0x85, 0x8f, 0x80,
    0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x84, 0x86,
    0x82, 0x92, 0x91, 0x99, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x93, 0x81, 0,    0,    0,    0,    0,
    0xa0, 0xa1, 0xa2, 0xa3, 0xa5, 0xa4};
#endif
#endif

/* $putpatch.c$: "MODULE_XLGLOB_C_GLOBALS" */
