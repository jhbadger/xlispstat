/* X11BSDstuff.c - X11 BSD interface routines for xlisp */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
/*                                                                     */
/* Some modifications included from WINTERP                            */
/* WINTERP 1.0 Copyright 1989 Hewlett-Packard Company (by Niels Mayer).*/

#include "xlisp.h"
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include "version.h"
/***** probably needs to be protected by check that dirent is available */
#include <dirent.h>
#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif

extern VOID StPollEvent(V);
extern int StBlockForInput(V);
extern VOID statsymbols(V);
extern VOID StX11Finish(V);
extern int StHasWindows(V);
extern VOID StX11ReleaseButton(V);
extern VOID StX11DialogReset(V);
extern VOID StGWResetBuffer(V);

/* externals */
extern FILEP tfp;
extern LVAL s_breakenable;
#ifdef CONDITIONS
extern LVAL s_condition_hook;
#endif /* CONDITIONS */

/* forward declarations */
LOCAL VOID reset_input _((void));
LOCAL int x11_get_char _((void));
LOCAL int line_available _((void));

/* static variables to protect gc from interrupt */
static int in_gc = 0, gc_interrupt = FALSE;
static time_t time_stamp;

/* -- local variables */
#define LBSIZE  200
static  char    lbuf[LBSIZE];
static  int     lindex;
static  int     lcount;

LOCAL VOID intercatch _((int)), fpecatch _((int));

VOID osinit(name)
  char *name;
{
  time_stamp = time((time_t *) 0);
  
  disable_interrupts();
#ifdef __FreeBSD__
  fpsetmask(0);
#endif
  if (signal(SIGINT, SIG_IGN) != SIG_IGN)
    signal(SIGINT, intercatch);
  signal(SIGFPE, fpecatch);
  printf("%s\n", name);
  printf("XLISP-STAT Release %d.%d.%d%s.\n",
	 XLS_MAJOR_RELEASE, XLS_MINOR_RELEASE, XLS_SUBMINOR_RELEASE,
	 XLS_RELEASE_STATUS);
  printf("Copyright (c) 1989-1999, by Luke Tierney.\n\n");
  lposition = 0;
  lindex  = 0;
  lcount  = 0;
}

VOID osfinish()
{
  CONTEXT cntxt;
  xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
  if (XL_SETJMP(cntxt.c_jmpbuf))
    exit(0);
  if (s_breakenable != NULL) setvalue(s_breakenable, NIL);
#ifdef CONDITIONS
  if (s_condition_hook != NULL) setvalue(s_condition_hook, NIL);
#endif /* CONDITIONS */
  StX11Finish();
}

VOID osreset()
{
  reset_input();
  if (StHasWindows()) {
    StGWResetBuffer();
    StX11DialogReset();
    StX11ReleaseButton();
  }
  in_gc = 0;
}

VOID xoserror(msg)
     char *msg;
{
  char line[STRMAX],*p;
  sprintf(line,"error: %s\n",msg);
  for (p = line; *p != '\0'; ++p)
    ostputc(*p);
}

#ifdef DODO
FILEP osaopen(name,mode)
  char *name,*mode;
{
    return (fopen(name,mode));
}

FILEP osbopen(name,mode)
  char *name,*mode;
{
    char nmode[4];
    strcpy(nmode,mode); strcat(nmode,"b");
    return (fopen(name,nmode));
}

int osclose(fp)
  FILEP fp;
{
    return (fclose(fp));
}
#endif /* DODO */

#ifdef FILETABLE
extern VOID gc();

int truename(name, rname)
char        *name,*rname;
{
    char *cp;
    char pathbuf[FNAMEMAX+1];   /* copy of path part of name */
    char curdir[FNAMEMAX+1];    /* current directory */
    char *fname;        /* pointer to file name part of name */
    
    /* parse any drive specifier */

    /* check for absolute path (good news!) */
    
    if (*name == '/') {
        strcpy(rname, name);
    }
    else {
        strcpy(pathbuf, name);
        if ((cp = strrchr(pathbuf, '/')) != NULL) { /* path present */
            cp[1] = 0;
            fname = strrchr(name, '/') + 1;
        }
        else {
            pathbuf[0] = 0;
            fname = name;
        }

        /* get the current directory of the selected drive */
        
        getcwd(curdir, FNAMEMAX);

        /* peel off "../"s */
        while (strncmp(pathbuf, "../", 3) == 0) {
            if (*curdir == 0) return FALSE;     /* already at root */
            strcpy(pathbuf, pathbuf+3);
            if ((cp=strrchr(curdir+1, '/')) != NULL)
                *cp = 0;    /* peel one depth of directories */
            else
                *curdir = 0;    /* peeled back to root */
        }
        
        /* allow for a "./" */
        if (strncmp(pathbuf, "./", 2) == 0)
            strcpy(pathbuf, pathbuf+2);
        
        /* final name is /curdir/pathbuf/fname */

        if ((int)(strlen(pathbuf)+strlen(curdir)+strlen(fname)+4) > FNAMEMAX)
            return FALSE;
        
        if (*curdir)
            sprintf(rname, "%s/%s%s", curdir, pathbuf, fname);
        else
            sprintf(rname, "/%s%s", pathbuf, fname);
    }
    
    return TRUE;
}

int getslot()
{
    int i=0;
    
    for (; i < FTABSIZE; i++)   /* look for available slot */
        if (filetab[i].fp == NULL) return i;
    
    gc();   /* is this safe??????? */

    for (; i < FTABSIZE; i++) /* try again -- maybe one has been freed */
        if (filetab[i].fp == NULL) return i;

    xlfail("too many open files");
    
    return 0;   /* never returns */
}


FILEP osopen(name, mode)
  char *name, *mode;
{
    int i=getslot();
    char namebuf[FNAMEMAX+1];
    FILE *fp;
    
    if (!truename((char *)name, namebuf))
        strcpy(namebuf, name);  /* should not happen */

    if ((filetab[i].tname = (char *)malloc(strlen(namebuf)+1)) == NULL) {
        xlfail("insufficient memory");
    }
    
    
    if ((fp = fopen(name,mode)) == NULL) {
        free(filetab[i].tname);
        return CLOSED;
    }

    filetab[i].fp = fp;

    strcpy(filetab[i].tname, namebuf);

    return i;
}
    
VOID osclose(f)
  FILEP f;
{
    if (filetab[f].fp != NULL)
        fclose(filetab[f].fp);
    /* remind stdin/stdout/stderr */
    if (f>2 && filetab[f].tname != NULL)
      free(filetab[f].tname);
    filetab[f].tname = NULL;
    filetab[f].fp = NULL;
}

int osmtime(fname, mtime)
     char *fname;
     time_t *mtime;
{
  struct stat s;

  if (stat(fname, &s))
    return -1;
  *mtime = s.st_mtime;
  return 0;
}

/* internal version of directory function */
LVAL dirlist P1C(char *, name)
{
  LVAL val;
  DIR *dir;

  disable_interrupts();
  xlsave1(val);
  val = NIL;
  if ((dir = opendir(name))) {
    struct dirent *dentry;
    while ((dentry = readdir(dir)))
      val = cons(cvstring(dentry->d_name), val);
    closedir(dir);
  }
  xlpop();
  enable_interrupts();

  return val;
}
#endif /* FILETABLE */

#ifdef DODO
int osagetc(fp)
  FILEP fp;
{
    return (getc(fp));
}

int osbgetc(fp)
  FILEP fp;
{
    return (getc(fp));
}

int osaputc(ch,fp)
  int ch; FILEP fp;
{
    return (putc(ch,fp));
}

int osbputc(ch,fp)
  int ch; FILEP fp;
{
    return (putc(ch,fp));
}
#endif /* DODO */

LOCAL int xostgetc()
{
  int ch;
  ch = x11_get_char();
  if (ch == '\n') lposition = 0;
  return(ch);
}

char *xfgets(s, n, iop)
     char *s;
     int n;
     FILEP iop;
{
  int c = 0;
  char *cs;

  cs = s;
  while (--n > 0 && (c = xostgetc()) != EOF) {
    *cs++ = c;
    if (c == '\n') break;
  }
  if (c == EOF && cs==s) return(NULL);
  *cs++ = '\0';
  return(s);
}

/* -- ostgetc - get a character from the terminal */
int ostgetc()
{
  while(--lcount < 0 ) {
    if ( xfgets(lbuf,LBSIZE,stdin) == NULL )
      return( EOF );
    
    lcount = strlen( lbuf );
    if (tfp!=CLOSED) OSWRITE(lbuf,1,lcount,tfp);

    lindex = 0;
    lposition = 0;
  }
  return( lbuf[lindex++] );
}

VOID ostputc(ch)
  int ch;
{
  putchar(ch);
  if (tfp != CLOSED)
    OSPUTC(ch,tfp);
  if (ch == '\n') lposition = 0;
  else lposition++;
}

VOID osflush()
{
  lindex = lcount = 0;
}

VOID osforce(fp)
     FILEP fp;
{
#ifdef FILETABLE
  if (fp == CONSOLE) fflush(stdout);
  else fflush(filetab[fp].fp);
#else
  if (fp == CONSOLE) fflush(stdout);
  else fflush(fp);
#endif /* FILETABLE */
}

VOID oscheck()
{
}

VOID ossymbols()
{
  statsymbols();
}

#ifdef DODO
VOID osfinit()
{
  statfinit();
}
#endif /* DODO */

LOCAL VOID intercatch(arg)
     int arg;
{
  signal(SIGINT, intercatch);
  if (in_gc > 0) gc_interrupt = TRUE;
  else xlsigint();
}

LOCAL VOID fpecatch(arg)
     int arg;
{
  signal(SIGFPE, fpecatch);
  xlfail("floating point error");
}

int max(x, y)
     int x, y;
{
  return((x > y) ? x : y);
}

int min(x, y)
     int x, y;
{
  return((x < y) ? x : y);
}

VOID set_gc_cursor(on) 
     int on;
{
  if (on) disable_interrupts();
  else enable_interrupts();
}

VOID disable_interrupts()
{
  in_gc++;
}

VOID enable_interrupts()
{
  if (gc_interrupt && in_gc == 1) {
    gc_interrupt = FALSE;
    in_gc = 0;
    xlsigint();
  }
  else if (in_gc > 0) in_gc--;
}

VOID SysBeep(n)
     int n;
{
  n = n / 10 - 1;
  do {
    printf("\007");
  } while (n-- > 0);
  fflush(stdout);
}

#ifndef CLK_TCK
#ifndef HZ
#define HZ 60
#endif
#define CLK_TCK HZ
#endif /* CLK_TCK */

#ifdef NODIFFTIME
#ifndef difftime
#define difftime(x,y) (((unsigned long) (x)) - ((unsigned long) (y)))
#endif
#endif

unsigned long ticks_per_second() { return((unsigned long)(CLK_TCK)); }

unsigned long run_tick_count()
{
  struct tms tm;

  times(&tm);
  
  return((unsigned long) tm.tms_utime + (unsigned long) tm.tms_stime);
}

unsigned long real_tick_count()
{
  return((unsigned long) (CLK_TCK * difftime(time((time_t *) 0), time_stamp)));
}

unsigned long system_tick_count()
{
  return((unsigned long) time((time_t *) 0));
}

/* The following routines will have to be changed for non-BSD systems. At
 * present they use a BSD-specific ioctl call as well as file structure
 * information to determine when terminal input characters are available.
 * While no characters are available events are processed. 
 *
 * An alternative polling system can be constructed using alarm or
 * setitimer. The alarm aproach is used in the S X11 and NeWS drivers.
 */

#ifdef DODO
LOCAL int x11_get_char()
{
  int ch;

  fflush(stdout);
  do {
    StPollEvent();
  } while (! char_available());
  ch =  getchar();
  return(ch);
}

LOCAL int char_available()
{
  FILE *file = stdin;
  int c = 0;
  
  if (file->_cnt > 0) return(TRUE);
  ioctl(fileno(file), FIONREAD, &c);
  if (c > 0) return(TRUE);
  else return(StBlockForInput());
}
#endif /* DODO */

static int in_a_line = FALSE;

LOCAL VOID reset_input() { in_a_line = FALSE; }

LOCAL int x11_get_char()
{
  int ch;

  fflush(stdout);
  if (! in_a_line) {
    do {
      StPollEvent();
    } while (! line_available());
  }
  ch =  getchar();
  in_a_line = (ch == '\n') ? FALSE : TRUE;
  return(ch);
}

LOCAL int line_available()
{
  int result;
  fd_set readmask;
  static struct timeval tv = {0, 0};
  static int ndfs = 0;

  if (ndfs == 0) ndfs = fileno(stdin) + 1; /*** is this right? ***/

  FD_ZERO(&readmask);
  FD_SET(fileno(stdin), &readmask);

  result = select(ndfs, (int *) &readmask, NULL, NULL, &tv);
  if (result > 0) return(TRUE);
  /* *** should merge the select here with the one for blocking ***/
  else return(StBlockForInput());
}

/* 
 * This routine is used to flush input to the tty during modal dialogs.
 * The ioctl call seems to do the job on BDS systems but may not exist
 * (in this form) on other systems. The ifdef is a hack to see if the
 * call is available.
 */
VOID StX11FlushStdin()
{
#ifdef TIOCFLUSH
  int c = 0;
  ioctl(fileno(stdin), TIOCFLUSH, &c);
#else
  fflush(stdin);
#endif
}

extern char *getenv();

VOID get_directory(s)
     char *s;
{
  char *libdir;
  int n;

  libdir = getenv("XLISPLIB");
  if (libdir == NULL) libdir = "";
  strcpy(s, libdir);
  n = strlen(s);
  if (n > 0 && s[n - 1] != '/')
    strcat(s, "/");
}

int renamebackup(name) char *name; { return(TRUE); }

/* xgetwd - builtin function GET-WORKING-DIRECTORY */
LVAL xgetwd()
{
  xllastarg();
  if (! getcwd(buf, FNAMEMAX))
    return NIL;
  else
    return cvstring(buf);
}

/* xsetwd - builtin function SET-WORKING-DIRECTORY */
LVAL xsetwd()
{
  char *dir = getstring(xlgastring());
  xllastarg();
  if (chdir(dir))
    return NIL;
  else
    return s_true;
}

#ifdef USEMATHERR
int matherr P1C(struct exception *, e)
{
  return 1;
}
#endif

#ifdef NOMEMMOVE
VOID memmove P3C(char *, s1, char *, s2, int, n)
{
  if (s1 < s2)
    while (n--)
      *s1++ = *s2++;
  else {
    s1 += (n-1);
    s2 += (n-1);
    while (n--)
      *s1-- = *s2--;
  }
}
#endif /* NOMEMMOVE */
