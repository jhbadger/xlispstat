/* xlinit.c - xlisp initialization module */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"
#ifdef XLISP_STAT
#include "xlstat.h"
#endif

#ifdef MACINTOSH
extern int hasAppleEvents;
#endif /* MACINTOSH */

/* Forward declarations */
LOCAL VOID initwks(V);
#ifdef IEEEFP
LOCAL double compute_infinity P2H(double, double);
#endif

#ifdef PACKAGES
/* enter an internal rather than external symbol */
#define ienter(x) xlintern(x, xlisppack)
#else
#define ienter(x) xlenter(x)
#endif

/* $putpatch.c$: "MODULE_XLINIT_C_GLOBALS" */

/* xlinit - xlisp initialization routine */
int xlinit P1C(char *, resfile) /* TAA Mod -- return true if load of init.lsp needed */
{
    /* initialize xlisp (must be in this order) */
    xlminit();  /* initialize xldmem.c */
    xldinit();  /* initialize xldbug.c */
#ifdef BYTECODE
    init_bytecode();
#endif /* BYTECODE */

    /* finish initializing */
#ifdef SAVERESTORE
    if (*resfile=='\0' || !xlirestore(resfile)) {
        initwks();
        /* $putpatch.c$: "MODULE_XLINIT_C_XLINIT" */
        return TRUE;
    }
    return FALSE;
#else
    initwks();
    /* $putpatch.c$: "MODULE_XLINIT_C_XLINIT" */
    return TRUE;
#endif
}

/* initwks - build an initial workspace */
LOCAL VOID initwks(V)
{
    FUNDEF *p;
    int i;
    
    xlsinit();  /* initialize xlsym.c */
    xlsymbols();/* enter all symbols used by the interpreter */
    xlrinit();  /* initialize xlread.c */
    xloinit();  /* initialize xlobj.c */

    /* setup defaults */

    /*can't mark as unbound until #<unbound> created*/
    setfunction(s_unbound, s_unbound);
#ifdef PACKAGES
    setfunction(s_package, s_unbound);
#else
    setfunction(obarray, s_unbound);
#endif /* PACKAGES */
    setfunction(NIL, s_unbound);

    setsvalue(a_readpw, NIL);           /* Don't preserve white space */

    setsvalue(s_evalhook, NIL);         /* no evalhook function */
    setsvalue(s_applyhook, NIL);        /* no applyhook function */
    setsvalue(s_tracelist, NIL);        /* no functions being traced */
    setsvalue(s_tracenable, NIL);       /* traceback disabled */
    setsvalue(s_tlimit, NIL);           /* trace limit infinite */
    setsvalue(s_breakenable, NIL);      /* don't enter break loop on errors */
    setsvalue(s_gcflag, NIL);           /* don't show gc information */
    setsvalue(s_gchook, NIL);           /* no gc hook active */
    setsvalue(s_baktraceprargs, s_true);/* print args in baktrace */

    setsvalue(s_features, NIL);         /* initial set of features */
    setvalue(s_features, cons(xlenter(":XLISP"), getvalue(s_features)));
    setvalue(s_features, cons(xlenter(":XLISP-PLUS"), getvalue(s_features)));
#ifdef XLISP_STAT
    setvalue(s_features, cons(xlenter(":XLISP-STAT"), getvalue(s_features)));
#endif /* XLISP_STAT */
#ifdef MACINTOSH
    setvalue(s_features, cons(xlenter(":MACINTOSH"), getvalue(s_features)));
    if (hasAppleEvents)
      setvalue(s_features, cons(xlenter(":APPLE-EVENTS"), getvalue(s_features)));
#endif /* MACINTOSH */
#ifdef SUNVIEW
    setvalue(s_features, cons(xlenter(":SUNVIEW"), getvalue(s_features)));
#endif /* SUNVIEW */
#ifdef X11WINDOWS
    setvalue(s_features, cons(xlenter(":X11"), getvalue(s_features)));
#endif /* X11WINDOWS */
#ifdef UNIX
    setvalue(s_features, cons(xlenter(":UNIX"), getvalue(s_features)));
#endif /* UNIX */
#ifdef MSDOS
    setvalue(s_features, cons(xlenter(":MSDOS"), getvalue(s_features)));
#endif /* MSDOS */
#ifdef WIN32
    setvalue(s_features, cons(xlenter(":WIN32"), getvalue(s_features)));
#endif /* WIN32 */
#ifdef AMIGA
    setvalue(s_features, cons(xlenter(":AMIGA"), getvalue(s_features)));
#endif /* AMIGA */

#ifdef BIGNUMS
    setsvalue(s_readbase, NIL);		/* default read base (decimal) */
    setsvalue(s_printbase, NIL);	/* default print base (decimal) */
#else
    setsvalue(s_ifmt, NIL);             /* default integer print format */
#endif
    setsvalue(s_ffmt, NIL);             /* float print format */

    setsvalue(s_printcase, k_upcase);   /* upper case output of symbols */
    setsvalue(s_printlevel, NIL);       /* printing depth is infinite */
    setsvalue(s_printlength, NIL);      /* printing length is infinite */
    setsvalue(s_printgensym, s_true);   /* print uninterned symbols with #: */
    setsvalue(s_printreadably, NIL);    /* print readable representations */
    setsvalue(s_printescape, s_true);   /* print excape characters */
    setsvalue(s_read_suppress, NIL);    /* do not suppress read operations */
#ifdef PRINTCIRCLE
    setsvalue(s_printcircle, NIL);      /* do not detect circles */
#endif /* PRINTCIRCLE */
#ifdef READTABLECASE
    setsvalue(s_rtcase, k_upcase);      /* read converting to uppercase */
#endif
    setsvalue(s_dispmacros, NIL);       /* don't displace macros */
    setsvalue(s_startup_functions, NIL);/* no startup functions */
    setsvalue(s_keepdocs,s_true);       /* keep doc strings */
    setsvalue(s_strict_keywords,s_true);/* enforce strict keyword use */
#ifdef CONDITIONS
    setsvalue(s_condition_hook, NIL);   /* no condition hook */
#endif /* CONDITIONS */
    setsvalue(s_loadfileargs, s_true);  /* load command line files on start */

    /* install the built-in functions and special forms */
    for (i = 0, p = funtab; (p->fd_subr) != (LVAL(*) _((void)))NULL; ++i, ++p)
        if (p->fd_name != NULL)
            xlsubr(p->fd_name,p->fd_type,p->fd_subr,i);

    /* add some synonyms */
    setfunction(xlenter("NOT"), getfunction(xlenter("NULL")));
    setfunction(xlenter("FIRST"), getfunction(xlenter("CAR")));
    setfunction(xlenter("SECOND"), getfunction(xlenter("CADR")));
    setfunction(xlenter("THIRD"), getfunction(xlenter("CADDR")));
    setfunction(xlenter("FOURTH"), getfunction(xlenter("CADDDR")));
    setfunction(xlenter("REST"), getfunction(xlenter("CDR")));

    /* set the initial top level function */
    setsvalue(s_toplevelloop, getfunction(xlenter("TOP-LEVEL-LOOP")));

    /* set the initial interrupt action */
    setsvalue(s_intaction, getfunction(xlenter("TOP-LEVEL")));

#ifdef BYTECODE
    /* initial version of CHECK-FSL-VERSION for bootstrapping */
    setfunction(ienter("CHECK-FSL-VERSION"), getfunction(xlenter("LIST")));
#endif /* BYTECODE */

#ifdef XLISP_STAT
    init_objects();
#endif /* XLISP_STAT */
#ifdef PACKAGES
    setvalue(s_package, xluserpack);
#endif /* PACKAGES */
}

/* xlsymbols - enter all of the symbols used by the interpreter */
VOID xlsymbols(V)
{
    LVAL sym;
#ifdef PACKAGES
    LVAL oldpack;

    /* find the system packages */
    xlisppack = xlfindpackage("XLISP");
    xlkeypack = xlfindpackage("KEYWORD");
    xluserpack = xlfindpackage("USER");
    xlfindsymbol("*PACKAGE*", xlisppack, &s_package);
    oldpack = getvalue(s_package);
    setvalue(s_package,xlisppack);
#endif /* PACKAGES */

    /* make the unbound variable indicator (must be first) */
    /* TAA MOD 1/93 -- now not interned */

    if (s_unbound == NIL) { /* don't make twice */
        s_unbound = xlmakesym("U"); /* name doesn't really matter */
        setvalue(s_unbound, s_unbound);
    }

    /* put NIL in oblist */
#ifdef PACKAGES
    setpackage(NIL, xlisppack);
    xlimport(NIL,xlisppack);
    xlexport(NIL,xlisppack);
#else
    {   /* duplicate code in xlenter, with different ending */
        char *name= "NIL";
        LVAL array = getvalue(obarray);
        int i = hash(name, HSIZE);
        
        for (sym = getelement(array,i); consp(sym); sym = cdr(sym))
            if (STRCMP(name, getstring(getpname(car(sym)))) == 0)
                goto noEnterNecessary;

        sym = consd(getelement(array,i));
        rplaca(sym, NIL);
        setelement(array, i, sym);
noEnterNecessary: ;
    }
#endif /* PACKAGES */

    /* enter the 't' symbol */
    s_true = xlenter("T");
    defconstant(s_true, s_true);        /* TAA mod -- was setvalue */

    /* enter some other constants */
#ifdef TIMES
    sym = xlenter("INTERNAL-TIME-UNITS-PER-SECOND");
    defconstant(sym, cvfixnum((FIXTYPE) ticks_per_second()));
#endif
    sym = xlenter("PI");
    defconstant(sym, cvflonum((FLOTYPE) PI));
#ifdef IEEEFP
    {
      double posinfinity, neginfinity, notanumber;

      /**** it may be necessary to turn off signals here  and above */
      posinfinity = compute_infinity(1.0, 0.0);
      neginfinity = -posinfinity;
      notanumber = posinfinity + neginfinity;
      s_posinfinity = xlenter("POSITIVE-INFINITY");
      s_neginfinity = xlenter("NEGATIVE-INFINITY");
      s_notanumber  = xlenter("NOT-A-NUMBER");
      defconstant(s_posinfinity, cvflonum((FLOTYPE) posinfinity));
      defconstant(s_neginfinity, cvflonum((FLOTYPE) neginfinity));
      defconstant(s_notanumber,  cvflonum((FLOTYPE) notanumber));
    }
#endif

    /* enter some important symbols */
    s_dot       = xlenter(".");
    s_quote     = xlenter("QUOTE");
    s_identity  = xlenter("IDENTITY");
    s_function  = xlenter("FUNCTION");
    s_bquote    = xlenter("BACKQUOTE");
    s_comma     = xlenter("COMMA");
    s_comat     = xlenter("COMMA-AT");
    s_lambda    = xlenter("LAMBDA");
    s_macro     = xlenter("MACRO");
    s_eq        = xlenter("EQ");
    s_eql       = xlenter("EQL");
    s_equal     = xlenter("EQUAL");
    s_features  = xlenter("*FEATURES*");
#ifdef BIGNUMS
    s_readbase  = xlenter("*READ-BASE*");
    s_printbase = xlenter("*PRINT-BASE*");
#else
    s_ifmt      = xlenter("*INTEGER-FORMAT*");
#endif
    s_ffmt      = xlenter("*FLOAT-FORMAT*");
    s_otherwise = xlenter("OTHERWISE");
    s_destructbind = xlenter("DESTRUCTURING-BIND");

    s_batchmode = xlenter("*BATCH-MODE*");
    setsvalue(s_batchmode, batchmode ? s_true : NIL);

    /* symbols set by the read-eval-print loop */
    s_1plus     = xlenter("+");   setsvalue(s_1plus,NIL);
    s_2plus     = xlenter("++");  setsvalue(s_2plus,NIL);
    s_3plus     = xlenter("+++"); setsvalue(s_3plus,NIL);
    s_1star     = xlenter("*");   setsvalue(s_1star,NIL);
    s_2star     = xlenter("**");  setsvalue(s_2star,NIL);
    s_3star     = xlenter("***"); setsvalue(s_3star,NIL);
    s_minus     = xlenter("-");   setsvalue(s_minus,NIL);

    /* enter setf place specifiers */
    s_setf      = xlenter("*SETF*");
    s_setfl     = xlenter("*SETF-LAMBDA*");  /* TAA added 7/92 */
    s_getf      = xlenter("GETF");           /* TAA added 7/93 */
    s_car       = xlenter("CAR");
    s_cdr       = xlenter("CDR");
    s_nth       = xlenter("NTH");
    s_aref      = xlenter("AREF");
    s_row_major_aref = xlenter("ROW-MAJOR-AREF");
    s_get       = xlenter("GET");
    s_svalue    = xlenter("SYMBOL-VALUE");
    s_sfunction = xlenter("SYMBOL-FUNCTION");
    s_splist    = xlenter("SYMBOL-PLIST");
    s_elt       = xlenter("ELT");
    s_apply     = xlenter("APPLY");
#ifdef HASHFCNS
    s_gethash   = xlenter("GETHASH");
#endif
    s_read_suppress = xlenter("*READ-SUPPRESS*");

    /* property for use by deftype */
    s_typespec  = xlenter("*TYPE-SPEC*");

    /* enter the readtable variable and keywords */
    s_rtable    = xlenter("*READTABLE*");
#ifdef BYTECODE
    s_stdrtable = xlenter("*STANDARD-READTABLE*");
#endif /* BYTECODE */
    k_wspace    = xlenter(":WHITE-SPACE");
    k_const     = xlenter(":CONSTITUENT");
    k_nmacro    = xlenter(":NMACRO");
    k_tmacro    = xlenter(":TMACRO");
    k_sescape   = xlenter(":SESCAPE");
    k_mescape   = xlenter(":MESCAPE");

    /* enter parameter list keywords */
    k_test      = xlenter(":TEST");
    k_tnot      = xlenter(":TEST-NOT");

    /* "open" keywords */
    k_direction = xlenter(":DIRECTION");
    k_input     = xlenter(":INPUT");
    k_output    = xlenter(":OUTPUT");
    k_io        = xlenter(":IO");
    k_probe     = xlenter(":PROBE");
    k_elementtype = xlenter(":ELEMENT-TYPE");
    k_exist     = xlenter(":IF-EXISTS");
    k_nexist    = xlenter(":IF-DOES-NOT-EXIST");
    k_error     = xlenter(":ERROR");
    k_rename    = xlenter(":RENAME");
    k_newversion = xlenter(":NEW-VERSION");
    k_overwrite = xlenter(":OVERWRITE");
    k_append    = xlenter(":APPEND");
    k_supersede = xlenter(":SUPERSEDE");
    k_rendel    = xlenter(":RENAME-AND-DELETE");
    k_create    = xlenter(":CREATE");

    /* enter *print-case* symbol and keywords */
    s_printcase = xlenter("*PRINT-CASE*");
    k_upcase    = xlenter(":UPCASE");
    k_downcase  = xlenter(":DOWNCASE");
    k_capitalize= xlenter(":CAPITALIZE");
#ifdef PRINTCIRCLE
    s_printcircle=xlenter("*PRINT-CIRCLE*");
    s_prcircdat = xlenter("*PRINT-CIRCLE-DATA*");
    setsvalue(s_prcircdat, s_unbound);
    s_rdcircdat = xlenter("*READ-CIRCLE-DATA*");
    setsvalue(s_rdcircdat, s_unbound);
#endif /* PRINTCIRCLE */

#ifdef READTABLECASE
    /* enter *readtable-case* symbol and keywords */
    s_rtcase    = xlenter("*READTABLE-CASE*");
    k_preserve  = xlenter(":PRESERVE");
    k_invert    = xlenter(":INVERT");
#endif

    /* more printing symbols */
    s_printlevel= xlenter("*PRINT-LEVEL*");
    s_printlength = xlenter("*PRINT-LENGTH*");
    s_printgensym = xlenter("*PRINT-GENSYM*");
    s_printreadably = xlenter("*PRINT-READABLY*");
    s_printescape = xlenter("*PRINT-ESCAPE*");

    s_load = xlenter("LOAD");

    /* other keywords */
    k_start     = xlenter(":START");
    k_end       = xlenter(":END");
    k_1start    = xlenter(":START1");
    k_1end      = xlenter(":END1");
    k_2start    = xlenter(":START2");
    k_2end      = xlenter(":END2");
    k_fromend   = xlenter(":FROM-END");
    k_verbose   = xlenter(":VERBOSE");
    k_print     = xlenter(":PRINT");
    k_count     = xlenter(":COUNT");
    k_concname  = xlenter(":CONC-NAME"); /* TAA-- added to save xlenters */
    k_include   = xlenter(":INCLUDE");
    k_prntfunc  = xlenter(":PRINT-FUNCTION");
    k_construct = xlenter(":CONSTRUCTOR");
    k_predicate = xlenter(":PREDICATE");
    k_initelem  = xlenter(":INITIAL-ELEMENT");
    k_initcont  = xlenter(":INITIAL-CONTENTS");
    k_displacedto = xlenter(":DISPLACED-TO");
    k_allow_other_keys = xlenter(":ALLOW-OTHER-KEYS"); /* TAA added 9/93 */

#ifdef KEYARG   
    k_key       = xlenter(":KEY");
#endif

    k_ivalue    = xlenter(":INITIAL-VALUE");

#ifdef HASHFCNS
    k_size = xlenter(":SIZE");
    k_rhthresh = xlenter(":REHASH-THRESHOLD");
    k_rhsize = xlenter(":REHASH-SIZE");
#endif

#ifdef PACKAGES
    k_nicknames = xlenter(":NICKNAMES");
    k_use = xlenter(":USE");
#ifdef MULVALS
    k_internal = xlenter(":INTERNAL");
    k_external = xlenter(":EXTERNAL");
    k_inherited = xlenter(":INHERITED");
#endif /* MULVALS */
#endif /* PACKAGES */

    /* Startup variables (from L. Tierney 9/93) */
    s_startup_functions = xlenter("*STARTUP-FUNCTIONS*");
    s_command_line = xlenter("*COMMAND-LINE*");
    s_loadfileargs = xlenter("*LOAD-FILE-ARGUMENTS*");
    s_toplevelloop = xlenter("*TOP-LEVEL-LOOP*");
    s_exit_functions = xlenter("SYSTEM::*EXIT-FUNCTIONS*");
    setvalue(s_exit_functions, NIL);

    /* enter lambda list keywords */
    lk_optional = xlenter("&OPTIONAL");
    lk_rest     = xlenter("&REST");
    lk_key      = xlenter("&KEY");
    lk_aux      = xlenter("&AUX");
    lk_allow_other_keys = xlenter("&ALLOW-OTHER-KEYS");
    lk_whole    = xlenter("&WHOLE");
    lk_body     = xlenter("&BODY");
    lk_environment = xlenter("&ENVIRONMENT");

    /* enter *standard-input*, *standard-output* and *error-output* */
    /* TAA Modified so that stderr (CONSOLE) is used if no redirection */

    s_stderr = xlenter("*ERROR-OUTPUT*");
    setsvalue(s_stderr,cvfile(CONSOLE,S_FORREADING|S_FORWRITING));
    s_termio = xlenter("*TERMINAL-IO*");
    setsvalue(s_termio,getvalue(s_stderr));
    s_stdin = xlenter("*STANDARD-INPUT*");
    setsvalue(s_stdin,redirectin ? 
        cvfile(STDIN,S_FORREADING): getvalue(s_stderr));
    s_stdout = xlenter("*STANDARD-OUTPUT*");
    setsvalue(s_stdout,redirectout ? 
        cvfile(STDOUT,S_FORWRITING): getvalue(s_stderr));

    /* enter *debug-io* and *trace-output* */
    s_debugio = xlenter("*DEBUG-IO*");
    setsvalue(s_debugio,getvalue(s_stderr));
    s_traceout = xlenter("*TRACE-OUTPUT*");
    setsvalue(s_traceout,getvalue(s_stderr));

    /* enter the eval and apply hook variables */
    s_evalhook = xlenter("*EVALHOOK*");
    s_applyhook = xlenter("*APPLYHOOK*");

    /* enter the symbol pointing to the list of functions being traced */
    s_tracelist = xlenter("*TRACELIST*");

    /* enter the error traceback and the error break enable flags */
    s_tracenable = xlenter("*TRACENABLE*");
    s_tlimit = xlenter("*TRACELIMIT*");
    s_breakenable = xlenter("*BREAKENABLE*");
    s_baktraceprargs = xlenter("*BAKTRACE-PRINT-ARGUMENTS*");

    /* enter symbols to control printing of garbage collection messages */
    /* Added set's so gc works during initialization. L. Tierney */
    s_gcflag = xlenter("*GC-FLAG*");
    setvalue(s_gcflag,NIL);             /* don't show gc information */
    s_gchook = xlenter("*GC-HOOK*");
    setvalue(s_gchook,NIL);             /* no gc hook active */

    /* enter symbol to control displacing of macros with expanded version */
    s_dispmacros = xlenter("*DISPLACE-MACROS*");

    /* enter a copyright notice into the oblist */
    sym = xlenter("**Copyright-1988-by-David-Betz**");
    setsvalue(sym,s_true);

#ifdef CONDITIONS
    /* enter condition hook and symbols*/
    s_condition_hook = xlenter("*CONDITION-HOOK*");
    s_error          = xlenter("ERROR");
    s_cerror         = xlenter("CERROR");
    s_signal         = xlenter("SIGNAL");
    s_warn           = xlenter("WARN");
    s_break          = xlenter("BREAK");
    s_debug          = xlenter("DEBUG");
    s_unboundvar     = xlenter("UNBOUND-VARIABLE");
    s_unboundfun     = xlenter("UNDEFINED-FUNCTION");
    k_name           = xlenter(":NAME");
#endif /* CONDITIONS */

    /* interrupt key action */
    s_intaction = xlenter("*INTERRUPT-ACTION*");

    /* enter type names */
    a_subr      = xlenter("SUBR");
    a_fsubr     = xlenter("FSUBR");
    a_cons      = xlenter("CONS");
    a_symbol    = xlenter("SYMBOL");
    a_fixnum    = xlenter("FIXNUM");
    a_flonum    = xlenter("FLOAT");
    a_string    = xlenter("STRING");
    a_object    = xlenter("OBJECT");
    a_stream    = xlenter("FILE-STREAM");
    a_vector    = xlenter("VECTOR");        /* L. Tierney */
    a_closure   = xlenter("CLOSURE");
    a_char      = xlenter("CHARACTER");
    a_ustream   = xlenter("UNNAMED-STREAM");
    a_list      = xlenter("LIST");
    a_number    = xlenter("NUMBER");
    a_null      = xlenter("NULL");
    a_atom      = xlenter("ATOM");
    a_anystream = xlenter("STREAM");
    s_and       = xlenter("AND");
    s_or        = xlenter("OR");
    s_not       = xlenter("NOT");
    k_and       = xlenter(":AND");
    k_or        = xlenter(":OR");
    k_not       = xlenter(":NOT");
    s_satisfies = xlenter("SATISFIES");
    s_member    = xlenter("MEMBER");
    a_struct    = xlenter("STRUCTURE");
    a_complex   = xlenter("COMPLEX");
    a_rndstate  = xlenter("RANDOM-STATE");
    a_array     = xlenter("ARRAY");         /* L. Tierney */
#ifdef HASHFCNS
    a_hashtable = xlenter("HASH-TABLE");
#endif
    a_integer   = xlenter("INTEGER");
    a_real	= xlenter("REAL");
#ifdef BIGNUMS
    a_ratio     = xlenter("RATIO");
    a_rational  = xlenter("RATIONAL");
    a_bignum    = xlenter("BIGNUM");
    a_unbyte	= xlenter("UNSIGNED-BYTE");
    a_sbyte	= xlenter("SIGNED-BYTE");
#endif
#ifdef BYTECODE
    a_bcclosure = xlenter("BYTE-CODE-CLOSURE");
    a_cpsnode   = xlenter("CPS-NODE");
    a_bcode     = xlenter("BYTE-CODE");
#endif /* BYTECODE */
#ifdef PACKAGES
    a_package   = xlenter("PACKAGE");
#endif /* PACKAGES */
    k_symbol_macro = xlenter(":SYMBOL-MACRO");
#ifdef XLISP_STAT
    a_adata     = xlenter("ALLOCATED-DATA");
#endif /* XLISP_STAT */
    a_ptr        = xlenter("SYSTEM:POINTER");
    a_weakbox    = xlenter("SYSTEM:WEAK-BOX");
    a_tvec       = xlenter("TYPED-VECTOR");
    s_c_char     = xlenter("C-CHAR");
    s_c_uchar    = xlenter("C-UCHAR");
    s_c_short    = xlenter("C-SHORT");
    s_c_int      = xlenter("C-INT");
    s_c_long     = xlenter("C-LONG");
    s_c_float    = xlenter("C-FLOAT");
    s_c_double   = xlenter("C-DOUBLE");
    s_c_complex  = xlenter("C-COMPLEX");
    s_c_dcomplex = xlenter("C-DCOMPLEX");
    s_make_array = xlenter("MAKE-ARRAY");

    /* struct feature symbols */
    s_strtypep  = ienter("%STRUCT-TYPE-P");
    s_mkstruct  = ienter("%MAKE-STRUCT");
    s_cpystruct = ienter("%COPY-STRUCT");
    s_strref    = ienter("%STRUCT-REF");
    s_strset    = ienter("%STRUCT-SET");
    s_x         = ienter("X");
    s_s         = ienter("S");
    s_prntfunc  = xlenter("*STRUCT-PRINT-FUNCTION*");
    s_sslots    = xlenter("*STRUCT-SLOTS*");
    s_strinclude= xlenter("*STRUCT-INCLUDE*");
    s_strconstruct= xlenter("*STRUCT-CONSTRUCTOR*");
    a_readpw    = ienter("*PRESERVE-WHITESPACE*");

    s_keepdocs = xlenter("*KEEP-DOCUMENTATION-STRINGS*");
    s_fundoc = xlenter("FUNCTION-DOCUMENTATION");
    s_vardoc = xlenter("VARIABLE-DOCUMENTATION");

    s_strict_keywords = xlenter("*STRICT-KEYWORDS*");

    s_rndstate     = xlenter("*RANDOM-STATE*");
#ifdef PACKAGES
    s_printsympack = xlenter("*PRINT-SYMBOL-PACKAGE*");
    setsvalue(s_printsympack, NIL);
#endif /* PACKAGES */

#ifdef MSDOS
    k_data = xlenter(":DATA");
    k_type = xlenter(":TYPE");
    k_item = xlenter(":ITEM");
    k_timeout = xlenter(":TIMEOUT");
    k_request = xlenter(":REQUEST");
#endif /* MSDOS */

#ifdef MACINTOSH
    k_appllistlabel = xlenter(":APPLICATION-LIST-LABEL");
    k_canswitch = xlenter(":CAN-SWITCH-LAYER");
    k_data = xlenter(":DATA");
    k_name = xlenter(":NAME");
    k_object = xlenter(":OBJECT");
    k_prompt = xlenter(":PROMPT");
    k_signature = xlenter(":SIGNATURE");
    k_timeout = xlenter(":TIMEOUT");
    k_type = xlenter(":TYPE");
    k_waitreply = xlenter(":WAIT-REPLY");
    k_zone = xlenter(":ZONE");
#endif /* MACINTOSH */

    /* add the object-oriented programming symbols and os specific stuff */
    obsymbols();        /* object-oriented programming symbols */
    ossymbols();        /* os specific symbols */
    initrndstate();
#ifdef BYTECODE
    bcsymbols();
#endif /* BYTECODE */
    /* $putpatch.c$: "MODULE_XLINIT_C_XLSYMBOLS" */
#ifdef PACKAGES
    setvalue(s_package, oldpack);
#endif /* PACKAGES */
#ifdef BIGNUMS
    {
        LVAL sym = ienter("*big0*");
        if (!boundp(sym)) 
            defconstant(sym, n_bigzero=cvtfixbignum(0L));
        else
            n_bigzero = getvalue(sym);
        sym = ienter("*big-1*");
        if (!boundp(sym))
            defconstant(sym, n_bigmone=cvtfixbignum(-1L));
        else
            n_bigmone = getvalue(sym);
    }
#endif
}

#ifdef IEEEFP
LOCAL double compute_infinity P2C(double, one, double, zero)
{
  return(one / zero);
}
#endif
