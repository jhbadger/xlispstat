/* xlglob.h -- external declarations for global variables */

/* symbols */
XLGLOBAL struct node isnil;

XLGLOBAL LVAL s_true,obarray;
XLGLOBAL LVAL s_features;
XLGLOBAL LVAL s_unbound,s_dot;
XLGLOBAL LVAL s_quote,s_identity,s_function;
XLGLOBAL LVAL s_bquote,s_comma,s_comat;
XLGLOBAL LVAL s_evalhook,s_applyhook,s_tracelist;
XLGLOBAL LVAL s_lambda,s_macro;
XLGLOBAL LVAL s_stdin,s_stdout,s_stderr,s_debugio,s_traceout;
XLGLOBAL LVAL s_rtable;
XLGLOBAL LVAL s_tracenable,s_tlimit,s_breakenable;
XLGLOBAL LVAL s_setf,s_setfl,s_car,s_cdr,s_apply,s_nth;
XLGLOBAL LVAL s_aref,s_get,s_getf;
XLGLOBAL LVAL s_svalue,s_sfunction,s_splist;
XLGLOBAL LVAL s_eql,s_gcflag,s_gchook;
#ifdef BIGNUMS
XLGLOBAL LVAL s_readbase, s_printbase;
#else
XLGLOBAL LVAL s_ifmt;
#endif
XLGLOBAL LVAL s_ffmt;
XLGLOBAL LVAL s_1plus,s_2plus,s_3plus;
XLGLOBAL LVAL s_1star,s_2star,s_3star;
XLGLOBAL LVAL s_minus,s_printcase;
XLGLOBAL LVAL s_printlevel, s_printlength;
XLGLOBAL LVAL s_dispmacros;
XLGLOBAL LVAL s_strtypep, s_mkstruct, s_cpystruct;
XLGLOBAL LVAL s_prntfunc;
XLGLOBAL LVAL s_strref, s_strset;
XLGLOBAL LVAL s_x, s_s, s_sslots;
XLGLOBAL LVAL s_elt;
XLGLOBAL LVAL a_list, a_number, a_null, a_atom, a_anystream;
XLGLOBAL LVAL s_and, s_or, s_not, s_satisfies, s_member;
XLGLOBAL LVAL a_struct;
XLGLOBAL LVAL s_read_suppress;
#ifdef DOSINPUT
XLGLOBAL LVAL s_dosinput;
#endif
#ifdef HASHFCNS
XLGLOBAL LVAL s_gethash, a_hashtable;
#endif
XLGLOBAL LVAL a_complex;
#ifdef READTABLECASE
XLGLOBAL LVAL s_rtcase;
#endif
#ifdef PACKAGES
XLGLOBAL LVAL xlisppack, xlkeypack, xluserpack, s_package;
#endif /* PACKAGES */
XLGLOBAL LVAL s_load;

/* keywords */
XLGLOBAL LVAL k_and, k_or, k_not;
XLGLOBAL LVAL k_test,k_tnot;
XLGLOBAL LVAL k_wspace,k_const,k_nmacro,k_tmacro;
XLGLOBAL LVAL k_sescape,k_mescape;
XLGLOBAL LVAL k_direction,k_input,k_output;
XLGLOBAL LVAL k_start,k_end,k_1start,k_1end;
XLGLOBAL LVAL k_2start,k_2end,k_count;
XLGLOBAL LVAL k_verbose,k_print;
XLGLOBAL LVAL k_upcase,k_downcase,k_capitalize;
XLGLOBAL LVAL k_io, k_elementtype;
XLGLOBAL LVAL s_termio, k_exist, k_nexist, k_error;
XLGLOBAL LVAL k_rename, k_newversion, k_overwrite, k_append;
XLGLOBAL LVAL k_supersede, k_rendel, k_probe, k_create;
XLGLOBAL LVAL k_concname, k_include, k_prntfunc;
XLGLOBAL LVAL k_initelem, k_initcont;
XLGLOBAL LVAL k_allow_other_keys;	 /* TAA added 9/93 */
XLGLOBAL LVAL k_displacedto;
#ifdef REDUCE
XLGLOBAL LVAL k_ivalue;
#endif
#ifdef KEYARG
XLGLOBAL LVAL k_key;
#endif
#ifdef HASHFCNS
XLGLOBAL LVAL k_size;
#endif
#ifdef RANDOM
XLGLOBAL LVAL k_data;
#endif
#ifdef READTABLECASE
XLGLOBAL LVAL k_preserve,k_invert;
#endif
#ifdef PACKAGES
XLGLOBAL LVAL k_nicknames, k_use;
#ifdef MULVALS
XLGLOBAL LVAL k_internal, k_external, k_inherited;
#endif /* MULVALS */
#endif /* PACKAGES */
#ifdef CONDITIONS
XLGLOBAL LVAL s_condition_hook;
XLGLOBAL LVAL s_error, s_cerror, s_signal, s_warn, s_break, s_debug;
XLGLOBAL LVAL s_unboundvar, s_unboundfun;
XLGLOBAL LVAL k_name;
#endif /* CONDITIONS */
XLGLOBAL LVAL s_keepdocs, s_fundoc, s_aref,s_row_major_aref, s_otherwise;
XLGLOBAL LVAL s_intaction, s_baktraceprargs, s_strict_keywords;
XLGLOBAL LVAL s_destructbind;
XLGLOBAL LVAL s_typespec, s_batchmode, k_fromend;
XLGLOBAL LVAL s_eq,s_equal;
XLGLOBAL LVAL s_printgensym,s_printreadably,s_printescape;
XLGLOBAL LVAL k_construct, k_predicate;
XLGLOBAL LVAL s_strinclude, s_strconstruct;
#ifdef BYTECODE
XLGLOBAL LVAL a_bcclosure, a_cpsnode, a_bcode, s_stdrtable;
#endif /* BYTECODE */
#ifdef XLISP_STAT
XLGLOBAL LVAL a_adata;
XLGLOBAL char *defaultpath;
#endif /* XLISP_STAT */
XLGLOBAL LVAL a_ptr;
XLGLOBAL LVAL a_weakbox;
#ifdef HASHFCNS
XLGLOBAL LVAL k_rhthresh, k_rhsize;
#endif
XLGLOBAL LVAL a_rndstate, s_rndstate;
XLGLOBAL LVAL a_array;        /* L. Tierney */
XLGLOBAL LVAL s_vardoc;
XLGLOBAL LVAL a_tvec;
XLGLOBAL LVAL s_c_char, s_c_short, s_c_int, s_c_long, s_c_float, s_c_double;
XLGLOBAL LVAL s_c_complex, s_c_dcomplex, s_make_array, s_c_uchar;
#ifdef IEEEFP
XLGLOBAL LVAL s_posinfinity, s_neginfinity, s_notanumber;
#endif
#ifdef PACKAGES
XLGLOBAL LVAL s_printsympack;
#endif /* PACKAGES */
#ifdef PRINTCIRCLE
XLGLOBAL LVAL s_printcircle, s_prcircdat, s_rdcircdat;
#endif /* PRINTCIRCLE */

/* lambda list keywords */
XLGLOBAL LVAL lk_optional,lk_rest,lk_key,lk_aux;
XLGLOBAL LVAL lk_allow_other_keys;
XLGLOBAL LVAL lk_whole,lk_body,lk_environment;

/* read type -- preserve whitespace?*/
XLGLOBAL LVAL a_readpw;

/* type names */
XLGLOBAL LVAL a_subr,a_fsubr;
XLGLOBAL LVAL a_cons,a_symbol,a_fixnum,a_flonum;
XLGLOBAL LVAL a_string,a_object,a_stream,a_vector;
XLGLOBAL LVAL a_closure,a_char,a_ustream;
XLGLOBAL LVAL a_integer, a_real;
#ifdef BIGNUMS
XLGLOBAL LVAL a_ratio;
XLGLOBAL LVAL a_rational;
XLGLOBAL LVAL a_bignum;
XLGLOBAL LVAL a_unbyte, a_sbyte;
XLGLOBAL LVAL n_bigzero, n_bigmone;
#endif
#ifdef PACKAGES
XLGLOBAL LVAL a_package;
#endif /* PACKAGES */
XLGLOBAL LVAL k_symbol_macro;

/* Object system */
XLGLOBAL LVAL s_self,k_new,k_isnew;
XLGLOBAL LVAL k_prin1;
XLGLOBAL LVAL cls_class,cls_object;

#ifdef MSDOS
XLGLOBAL LVAL k_data, k_type, k_item, k_timeout, k_request;
#endif /* MSDOS */
#ifdef MACINTOSH
XLGLOBAL LVAL k_appllistlabel, k_canswitch, k_data, k_name, k_object, k_prompt;
XLGLOBAL LVAL k_signature, k_timeout, k_type, k_waitreply, k_zone;
#endif /* MACINTOSH */

/* evaluation variables */
XLGLOBAL LVAL **xlstktop;             /* top of the evaluation stack */
XLGLOBAL LVAL **xlstkbase;            /* base of the evaluation stack */
XLGLOBAL LVAL **xlstack;	      /* evaluation stack pointer */
XLGLOBAL LVAL xlenv,xlfenv,xldenv;    /* environment pointers */

/* argument stack */
XLGLOBAL LVAL *xlargstkbase;    /* base of the argument stack */
XLGLOBAL LVAL *xlargstktop;     /* top of the argument stack */
XLGLOBAL LVAL *xlfp;		/* argument frame pointer */
XLGLOBAL LVAL *xlsp;		/* argument stack pointer */
XLGLOBAL LVAL *xlargv;		/* current argument vector */
XLGLOBAL int xlargc;		/* current argument count */

/* continuation stack */
#ifdef BYTECODE
XLGLOBAL CONTINUATIONP xlcontinuation_stack, xlcstop, xlcsend;
XLGLOBAL LVAL *vsbase;
#define vstop xlsp
#endif /* BYTECODE */

/* exception handling variables */
XLGLOBAL CONTEXT *xlcontext;	/* current exception handler */
XLGLOBAL CONTEXT *xltarget;	/* target context (for xljump) */
XLGLOBAL LVAL xlvalue;		/* exception value (for xljump) */
XLGLOBAL int xlmask;		/* exception type (for xljump) */
#ifdef MULVALS
XLGLOBAL int xlnumresults;	/* number of values */
XLGLOBAL LVAL *xlresults;       /* multiple values array */
#endif /* MULVALS */

/* Garbage collection reporting variables */
XLGLOBAL long gccalls, nfree, total;

/* Weak box list */
XLGLOBAL LVAL xlweakboxes;

/* debugging variables */
XLGLOBAL int xldebug;	   /* debug level */
XLGLOBAL int xlsample;	   /* control character sample rate */
XLGLOBAL int xltrcindent;  /* trace indent level */

/* gensym variables */
XLGLOBAL char gsprefix[];     /* gensym prefix string */
XLGLOBAL FIXTYPE gsnumber;    /* gensym number */

/* i/o variables */
XLGLOBAL FILEP tfp;	    /* transcript file pointer */
XLGLOBAL int redirectout;   /* output is redirected */
XLGLOBAL int redirectin;    /* input is redirected */
XLGLOBAL int batchmode;	    /* running a batch process */
XLGLOBAL int lposition;	    /* postition in screen */

/* From Luke Tierney, 9/93 */
/* startup functions and command line symbols */
XLGLOBAL LVAL s_startup_functions, s_command_line;
XLGLOBAL LVAL s_loadfileargs, s_toplevelloop, s_exit_functions;

/* general purpose string buffer */
XLGLOBAL char buf[];

/* remaining nodes */
XLGLOBAL long nnodes;

/* printing level and length */
XLGLOBAL int plevel, plength;

#ifdef STSZ
/* For stack checking */
XLGLOBAL int stackwarn;	    /* is TRUE when warning given */
XLGLOBAL int marghi;	    /* stackleft for warning */
#endif

#ifdef FILETABLE
XLGLOBAL FILETABLETYPE filetab[FTABSIZE];
#endif

#ifdef ASCII8
/* conversion tables for 8 bit ASCII */
XLGLOBAL char ascii8tbl[];
XLGLOBAL unsigned char ascii8cnv[];
#endif

/* defined in xlisp.c */
#ifdef SAVERESTORE
XLGLOBAL XL_JMP_BUF top_level;
#endif

/* defined in xlftab.c */
/* XLGLOBALal variable from xlftab.c */
#if defined(_Windows) && ! defined(WIN32)   /* L. Tierney */
    XLGLOBAL FUNDEF far funtab[];
#else
    XLGLOBAL FUNDEF funtab[];
#endif
XLGLOBAL int ftabsize;	/* TAA MOD -- added validity check */

/* $putpatch.c$: "MODULE_XLGLOB_H_GLOBALS" */
