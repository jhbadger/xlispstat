#define ANSI
#define IEEEFP
#define ADEPTH 8000
#define EDEPTH 8000
#define IEEEFP
#define STSZ 6 * 32768
#ifdef powerc
#  define HAVE_DLOPEN 1
#  define SHAREDLIBS
#else
#  define HAVE_DLOPEN 0
#endif
