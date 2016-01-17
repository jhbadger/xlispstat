#include <a.out.h>

#define STDBSD
#define SYM_IS_GLOBAL_FUNCTION(ldptr,symbol) \
  (((symbol).n_type & N_TYPE) == N_DATA  && ((symbol).n_type & N_EXT))

#define INTERNAL_CNAME_PATTERN "_%s"
#define INTERNAL_FNAME_PATTERN "_%s_"
#define CLIBS "-lm -lc"
#define FLIBS "-lm -lc -lF77 -lI77 -lU77"
#define LDPATTERN "ld -d -N -x -A %s -T %x %s %s %s -o %s"
#define TMPPATTERN "/tmp/xlispdyn%d"
#define TMPNAMESIZE 32
#define PAGE_SIZE 4096
#define MIN_ALLOC 10000 + PAGE_SIZE
#define VERBDFLT TRUE
