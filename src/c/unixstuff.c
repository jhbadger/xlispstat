/* unixstuff.c - unix interface routines for xlisp                     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
/*                                                                     */
/* Some modifications included from WINTERP                            */
/* WINTERP 1.0 Copyright 1989 Hewlett-Packard Company (by Niels Mayer).*/

#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/stat.h>
/***** probably needs to be protected by check that dirent is available */
#include <dirent.h>
#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif

#include "xlisp.h"
#include "osdefs.h"
#ifdef XLISP_STAT
#include "version.h"
#endif

#define LBSIZE  200
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

/* static variables to protect gc from interrupt */
static int in_gc = 0, gc_interrupt = FALSE;
static time_t time_stamp;

/* -- local variables */
static  char    lbuf[LBSIZE];
static  int     lindex;
static  int     lcount;

/* Function prototupes */
LOCAL int xostgetc _((void));
LOCAL char *xfgets _((char *s, int n));
LOCAL VOID intercatch _((int));
LOCAL VOID fpecatch _((int));

#ifndef XLISP_STAT
LVAL xsystem()
{
  char *cmd;
  int status;
  LVAL stream = NIL;
  FILE *p;
  int ch;

  cmd = (char *) getstring(xlgastring());
  if (moreargs()) {
    stream = xlgetarg();
    if (stream == s_true)
      stream = getvalue(s_stdout);
    else if (!streamp(stream) && !ustreamp(stream))
      xlbadtype(stream);
  }
  
  if (stream == NIL) {
    status = system(cmd);
    if (status == 127) xlfail("shell could not execute command");
  }
  else {
    if ((p = popen(cmd, "r")) == NULL)
      xlfail("could not execute command");
    while ((ch = getc(p)) != EOF) xlputc(stream, ch);
    status = pclose(p);
  }
  return(cvfixnum((FIXTYPE) status));
}
#endif /* XLISP_STAT */

/* -- osinit - initialize */
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
#ifdef XLISP_STAT
  printf("%s\n", name);
  printf("XLISP-STAT Release %d.%d.%d%s.\n",
	 XLS_MAJOR_RELEASE, XLS_MINOR_RELEASE, XLS_SUBMINOR_RELEASE,
	 XLS_RELEASE_STATUS);
  printf("Copyright (c) 1989-1999, by Luke Tierney.\n\n");
#else
  fprintf(stderr,"%s\nUNIX version\n", name);
#endif /* XLISP_STAT */
  lposition = 0;
  lindex  = 0;
  lcount  = 0;
}

/* -- osfinish - clean up before returning to the operating system */
VOID osfinish()
{
}

VOID osreset()
{
  in_gc = 0;
}

/* -- xoserror - print an error message */
VOID xoserror(msg)
     char *msg;
{
  char line[STRMAX],*p;
  sprintf(line,"error: %s\n",msg);
  for (p = line; *p != '\0'; ++p)
    ostputc(*p);
}

#ifdef FILETABLE
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

#ifdef PATHNAMES
/* ospopen - open using a search path */
FILEP ospopen(name, ascii)
char *name;
int ascii;  /* value not used in UNIX */
{
    char *getenv();
    FILEP fp;
    char *path = getenv(PATHNAMES);
    char *newnamep;
    char ch;
    char newname[256];

    /* don't do a thing if user specifies explicit path */
    if (strchr(name,'/') != NULL || path == NULL)
        return OSAOPEN(name, "r");

    do {
        if (*path == '\0')  /* no more paths to check */
            /* check current directory just in case */
            return OSAOPEN(name, "r");

        newnamep = newname;
        while ((ch = *path++) != '\0' && ch != ':' && ch != ' ')
            *newnamep++ = ch;

    if (ch == '\0') path--;

        if (*(newnamep-1) != '/')
            *newnamep++ = '/';  /* final path separator needed */
        *newnamep = '\0';

        strcat(newname, name);
        fp = OSAOPEN(newname, "r");
    } while (fp == CLOSED); /* not yet found */

    return fp;
}
#endif

/* rename argument file as backup, return success name */
/* For new systems -- if cannot do it, just return TRUE! */
int renamebackup(filename)
     char *filename;
{
#ifdef XLISP_STAT
  return(TRUE);
#else
  char *bufp, ch=0;

  strcpy(buf, filename);  /* make copy with .bak extension */

  bufp = &buf[strlen(buf)];   /* point to terminator */
  while (bufp > buf && (ch = *--bufp) != '.' && ch != '/') ;


  if (ch == '.') strcpy(bufp, ".bak");
  else strcat(buf, ".bak");

  unlink(buf);

  return !rename(filename, buf);
#endif /* XLISP_STAT */
}

/* -- ostgetc - get a character from the terminal */
int ostgetc()
{
  while(--lcount < 0 ) {
    if ( xfgets(lbuf,LBSIZE) == NULL )
      return( EOF );

    lcount = strlen( lbuf );
    if (tfp!=CLOSED) OSWRITE(lbuf,1,lcount,tfp);

    lindex = 0;
    lposition = 0;
  }
  return( lbuf[lindex++] );
}

/* -- ostputc - put a character to the terminal */
VOID ostputc(ch)
     int ch;
{
  if (ch == '\n') lposition = 0;
  else lposition++;

  putchar(ch);

  /* -- output the char to the transcript file */
  if (tfp != CLOSED)
    OSPUTC(ch, tfp);
}

/* -- osflush - flush the terminal input buffer */
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

/* -- ossymbols - enter os-specific symbols */
VOID ossymbols()
{
#ifdef XLISP_STAT
  statsymbols();
#endif
}

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

LOCAL int xostgetc()
{
  int ch;
  ch = getchar();
  if (ch == '\n') lposition = 0;
  return(ch);
}

LOCAL char *xfgets(s, n)
     char *s;
     int n;
{
  int c;
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

#ifdef XLISP_STAT
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
#endif /* XLISP_STAT */

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

#ifdef XLISP_STAT
VOID SysBeep(n)
     int n;
{
  n = n / 10 - 1;
  do {
    printf("\007");
  } while (n-- > 0);
  fflush(stdout);
}
#endif /* XLISP_STAT */

#ifdef TIMES
/***********************************************************************/
/**                                                                   **/
/**                  Time and Environment Functions                   **/
/**                                                                   **/
/***********************************************************************/

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

#ifndef XLISP_STAT
LVAL xtime()
{
  LVAL expr, result;
  unsigned long tm, rtm;
  double dtm, rdtm;

  /* get the expression to evaluate */
  expr = xlgetarg();
  xllastarg();

  tm = run_tick_count();
  rtm = real_tick_count();
  result = xleval(expr);
  tm = run_tick_count() - tm;
  rtm = real_tick_count() - rtm;
  dtm = (tm > 0) ? tm : -tm;
  rdtm = (rtm > 0) ? rtm : -rtm;
  sprintf(buf, "CPU %.2f sec., Real %.2f sec.\n", dtm / ticks_per_second(),
                                            rdtm / ticks_per_second());
  trcputstr(buf);
  return(result);
}

LVAL xruntime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) run_tick_count()));
}

LVAL xrealtime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) real_tick_count()));
}

LVAL xgctime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) gc_tick_count()));
}
#endif /* XLISP_STAT */
#endif /* TIMES */

#ifdef XLISP_STAT
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
#endif /* XLISP_STAT */

#ifdef STSZ
int stackreport() { return (xlargstktop - xlsp); }
#endif

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

/* internal version of directory function */
/***** probably needs to be protected by check that dirent is available */
/***** need to drop non-files, i.e. directories, using stat() call */
#include <dirent.h>

LVAL xdirectory()
{
  LVAL name, val;
  DIR *dir;

  name = xlgastring();
  xllastarg();

  xlsave1(val);
  val = NIL;
  if ((dir = opendir(getstring(name)))) {
    struct dirent *dentry;
    while ((dentry = readdir(dir)))
      val = cons(cvstring(dentry->d_name), val);
    /* protect this with an unwind-protect? */
    closedir(dir);
  }
  xlpop();

  return val;
}
