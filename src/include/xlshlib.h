#ifdef _Windows
#define XLGLOBAL __declspec(dllimport)
#endif

#include "xlisp.h"
#define MVSUBR (SUBR + TYPEFIELD + 1)

#define MAKEVERSION(major,minor) ((1L<<16) * major + minor)
#define XLSHLIB_SYSVERSION {MAKEVERSION(0,1),MAKEVERSION(0,0)}
#define XLSHLIB_VERSION_INFO(maj_cur,min_cur,maj_old,min_old) \
  XLSHLIB_SYSVERSION, \
  {MAKEVERSION(maj_cur,min_cur),MAKEVERSION(maj_old,min_old)}

struct version_info { long current, oldest; };

typedef struct { char *name; FIXTYPE val; } FIXCONSTDEF;
typedef struct { char *name; FLOTYPE val; } FLOCONSTDEF;
typedef struct { char *name; char *val; } STRCONSTDEF;
typedef struct { char *name; unsigned long val; } ULONGCONSTDEF;

typedef struct {
  struct version_info sysversion;
  struct version_info modversion;
  FUNDEF *funs;
  FIXCONSTDEF *fixconsts;
  FLOCONSTDEF *floconsts;
  STRCONSTDEF *strconsts;
  ULONGCONSTDEF *ulongconsts;
} xlshlib_modinfo_t;
