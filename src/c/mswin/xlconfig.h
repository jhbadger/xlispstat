#define ANSI
#define EDEPTH 4000
#define ADEPTH 5000
#define IEEEFP
#ifdef _WIN32
#  define HAVE_DLOPEN 1
#  define SHAREDLIBS
#  ifndef WIN32
#    define WIN32
#  endif
#  define STSZ win32stsz
#  define WIN32S_STSZ 70000
#else
#  define HAVE_DLOPEN 0
/* defines from sys\stat.h not included for Win16 for some reason */
#  define S_ISDIR(m)  ((m) & S_IFDIR)
#  define S_ISCHR(m)  ((m) & S_IFCHR)
#  define S_ISBLK(m)  ((m) & S_IFBLK)
#  define S_ISREG(m)  ((m) & S_IFREG)
#  define S_ISFIFO(m) ((m) & S_IFIFO)
#  define STSZ 20000
#endif

#ifndef _Windows
#  define _Windows
#endif
