#ifdef CLOSED
#undef CLOSED
#endif

#include <a.out.h>
#include <ldfcn.h>

#define INTERNAL_CNAME_PATTERN "%s"
#define INTERNAL_FNAME_PATTERN "%s_"
#define COFF_FORMAT
#define CLIBS "-lm -lc"
#define FLIBS "-lm -lc -lF77 -lI77 -lU77"
#define LDPATTERN "ld -d -N -x -A %s -G 0 -T %x %s %s %s -o %s"
#define TMPPATTERN "/tmp/xlispdyn%d"
#define TMPNAMESIZE 32
#define PAGE_SIZE 4096
#define MIN_ALLOC 10000 + PAGE_SIZE
#define VERBDFLT TRUE

#define SYMENT SYMR
#define SYMVALUE(sym) ((char *) ((sym).value))
#define N_SECTIONS(ldptr) HEADER(ldptr).f_nscns
#define SCN_ADDR(ldptr,section_header) (section_header).s_vaddr
#define SCN_LENGTH(ldptr,section_header) (section_header).s_size
#define SCN_FILE_LOC(ldptr,section_header) ((section_header).s_scnptr)
#define SCN_IS_BSS(ldptr,section_header) \
        (strcmp(section_header.s_name, ".bss") == 0 \
        || strcmp(section_header.s_name, ".sbss") == 0)

#define SYM_IS_GLOBAL_FUNCTION(ldptr,symbol) \
        ((symbol).sc == scText && (symbol).st == stProc)

#ifdef FREAD
#undef FREAD
#endif
#define FREAD FREADM

extern char *ldgetname();

#define MEMPROT
