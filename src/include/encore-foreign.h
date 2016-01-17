#include <filehdr.h>

#ifdef n_offset
#undef n_offset
#endif

#include <syms.h>
#include <ldfcn.h>

extern SYMENT *lookupsym();
extern char *ldgetname();

#define INTERNAL_CNAME_PATTERN "_%s"
#define INTERNAL_FNAME_PATTERN "_%s_"
#define HAS_OWN_DYNLOAD
#define VERBDFLT TRUE
#define COFF_FORMAT

#define SYMVALUE(sym) ((char *) ((sym).n_value))

#define SYM_IS_GLOBAL_FUNCTION(ldptr,symbol) \
        ((symbol).n_sclass == C_EXT && (symbol).n_scnum > 0)

static char *libraries[] = {
  "/lib/libc.a",
  "/usr/lib/libm.a",
  "/usr/lib/libF77.a",
  "/usr/lib/libI77.a",
  "/usr/lib/libU77.a",
  NULL
  };

static link_and_load(fname, libs, fort)
     char *fname, *libs;
     int fort;
{
  char *code_start;
  static int inited = FALSE;
  LDFILE *fp;
  SYMENT symbol, *sym;
  char *symname, *symaddr;
  int i;

  if (! inited && initsyms(progname) == 0)
    xlfail("couldn't initialize symbol table");
  else inited = TRUE;

  /* load the code */
  if (dynload(fname, &code_start, 0L, libraries) <= 0) xlfail("load failed");
  
  /* Enter the symbols.                                              */
  /* Assumes the value of the syment returned by lookupsym gives the */
  /* offset from code_start.                                         */
  if ((fp = ldopen(fname, NULL)) == NULL)
    xlfail("cannot open object file for symbol reading");
  i = 0;
  while (ldtbread(fp, i, &symbol) == SUCCESS) {
    i++;
    if (SYM_IS_GLOBAL_FUNCTION(input, symbol)) {
      symname = ldgetname(fp, &symbol);
      sym = lookupsym(symname);
      if (sym != NULL) {
	symaddr = code_start + (long) SYMVALUE(*sym);
	enter_csymbol(symname, symaddr);
      }
    }
  }
  if (ldclose(fp) == FAILURE) xlfail("cannot close object file");
}

#undef n_name
#include <nlist.h>
