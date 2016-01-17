/* mswstuff.c - ms-windows specific routines */

#include <dos.h>
#include <signal.h>
#include <mem.h>
#include <dir.h>
#include <time.h>
#include <float.h>
#include <sys/stat.h>
#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"
#include "wxlisp.h"
#include "ledit.h"
#include "winutils.h"
#include "version.h"

#define LBSIZE 200

/* external variables */
extern FILEP tfp;
extern int errno;
extern int Exiting;
extern char *iniFile;
extern LVAL s_event_queue, s_in_callback;

/* local variables */
static char lbuf[LBSIZE];
static int lindex;
static int lcount;

#ifdef XLISP_ONLY
static long rseed = 1L;
#endif

static char *xfgets _((char *s, int n));
static void errcatch(int);

char *stackbase;
int xlisp_mono;

/* osinit - initialize */
void osinit(banner)
  char *banner;
{
#ifdef STSZ
    stackbase = (char *)&banner;    /* find base of stack */
#endif

  GetPrivateProfileString("Graphics", "COLOR", "", buf, STRMAX, iniFile);
  if (! stricmp(buf, "off") || ! stricmp(buf, "no") || getenv("XLISPMONO"))
    xlisp_mono = TRUE;
  else
    xlisp_mono = FALSE;

  sprintf(lbuf, "%s\n",banner);
  TTYPutStr(lbuf);
  sprintf(lbuf, "XLISP-STAT Release %d.%d.%d%s.\n",
	 XLS_MAJOR_RELEASE, XLS_MINOR_RELEASE, XLS_SUBMINOR_RELEASE,
	 XLS_RELEASE_STATUS);
  TTYPutStr(lbuf);
  TTYPutStr("Copyright (c) 1989-1999, by Luke Tierney.\n\n");

  lposition = 0;
  lindex = 0;
  lcount = 0;

#ifndef MinGW32
  _control87(MCW_EM, MCW_EM);
#endif
  signal(SIGABRT, errcatch);
  signal(SIGFPE, errcatch);
  signal(SIGILL, errcatch);
  signal(SIGSEGV, errcatch);
}

/* osfinish - clean up before returning to the operating system */
void osfinish()
{
  CONTEXT cntxt;
  xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
  if (XL_SETJMP(cntxt.c_jmpbuf))
    exit(0); /*** this probably needs to be done differently */
  if (s_breakenable != NULL) setvalue(s_breakenable, NIL);
#ifdef CONDITIONS
  if (s_condition_hook != NULL) setvalue(s_condition_hook, NIL);
#endif /* CONDITIONS */
  Exiting = TRUE;
  PostQuitMessage(0);
  ExitXLS();
}

/* xoserror - print an error message */
void xoserror(msg)
  char *msg;
{
  WarningBox(msg);
}

# ifdef XLISP_ONLY
/* osrand - return a random number between 0 and n-1 */
int osrand(n)
  int n;
{
    long k1;

    /* make sure we don't get stuck at zero */
    if (rseed == 0L) rseed = 1L;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
	rseed += 2147483647L;

    /* return a random number between 0 and n-1 */
    return ((int)(rseed % (long)n));
}
#endif /* XLISP_ONLY */

#ifdef MinGW32
int truename(char *name, char *rname)
{
  if (GetFullPathName(name, FNAMEMAX + 1, rname, NULL) == 0)
    return FALSE;
  else {
    char *p;
    /* lowercase the whole string */
    for (p = rname; *p != 0; p++)
        if (isupper(*p))
	  *p = (char) tolower(*p);
    return TRUE;
  }
}
#else
int truename(char *name, char *rname)
{
    int i;
    char *cp;
    int drive;          /* drive letter */
    char pathbuf[FNAMEMAX+1];   /* copy of path part of name */
    char curdir[FNAMEMAX+1];    /* current directory of drive */
    char *fname;        /* pointer to file name part of name */
    
    /* use backslashes consistantly */
    
    for (cp = name; (cp = strchr(cp, '/')) != NULL; *cp = '\\') ;
    
    /* parse any drive specifier */

    if ((cp = strrchr(name, ':')) != NULL) {
        if (cp != name+1 || !isalpha(*name)) return FALSE;
        drive = toupper(*name);
        name = cp+1;            /* name now excludes drivespec */
    }
    else {
        drive = 'A' + getdisk();
    }
    
    /* check for absolute path (good news!) */
    
    if (*name == '\\') {
        sprintf(rname,"%c:%s",drive,name);
    }
    else {
        strcpy(pathbuf, name);
        if ((cp = strrchr(pathbuf, '\\')) != NULL) {    /* path present */
            cp[1] = 0;
            fname = strrchr(name, '\\') + 1;
        }
        else {
            pathbuf[0] = 0;
            fname = name;
        }

        /* get the current directory of the selected drive */
        
        if (getcurdir(drive - 'A' + 1, curdir))
            return FALSE;    /* invalid drive */

        /* peel off "..\"s */
        while (strncmp(pathbuf, "..\\", 3) == 0) {
            if (*curdir == 0) return FALSE;     /* already at root */
            strcpy(pathbuf, pathbuf+3);
            if ((cp=strrchr(curdir, '\\')) != NULL)
                *cp = 0;    /* peel one depth of directories */
            else
                *curdir = 0;    /* peeled back to root */
        }
        
        /* allow for a ".\" */
        if (strncmp(pathbuf, ".\\", 2) == 0)
            strcpy(pathbuf, pathbuf+2);
        
        /* final name is drive:\curdir\pathbuf\fname */

        if (strlen(pathbuf)+strlen(curdir)+strlen(fname)+4 > FNAMEMAX) 
            return FALSE;
        
        if (*curdir)
            sprintf(rname, "%c:\\%s\\%s%s", drive, curdir, pathbuf, fname);
        else
            sprintf(rname, "%c:\\%s%s", drive, pathbuf, fname);
    }
    
    /* lowercase the whole string */

    for (cp = rname; (i = *cp) != 0; cp++) {
        if (isupper(i)) *cp = (char) tolower(i);
    }
    
    return TRUE;
}
#endif

int getslot(VOID)
{
    int i=0;

    for (; i < FTABSIZE; i++)   /* look for available slot */
        if (filetab[i].fp == NULL) return i;

    gc();   /* is this safe??????? */

    for (i=0; i < FTABSIZE; i++) /* try again -- maybe one has been freed */
        if (filetab[i].fp == NULL) return i;

    xlfail("too many open files");

    return 0;   /* never returns */
}


FILEP osaopen(const char *name, const char *mode)
{
    int i=getslot();
    char namebuf[FNAMEMAX+1];
    FILE *fp;

    if (!truename((char *)name, namebuf))
        strcpy(namebuf, name);  /* should not happen */

    if ((filetab[i].tname = malloc(strlen(namebuf)+1)) == NULL) {
        xlfail("insufficient memory");
    }
    
    
    if ((fp = fopen(name,mode)) == NULL) {
        free(filetab[i].tname);
        return CLOSED;
    }

    filetab[i].fp = fp;

    strcpy(filetab[i].tname, namebuf);

    /* calculate mode to re-open file */
    if (mode[0]=='w') {
        strcpy(filetab[i].reopenmode, "r+");
        if (mode[strlen(mode)-1]=='b') strcat(filetab[i].reopenmode, "b");
    }
    else strcpy(filetab[i].reopenmode, mode);

    return i;
}


FILEP osbopen(const char *name, const char *mode)
{
    char bmode[10];

    strcpy(bmode,mode); strcat(bmode,"b");  

    return osaopen(name, bmode);
}

VOID osclose(FILEP f)
{
    fclose(filetab[f].fp);
    free(filetab[f].tname);
    filetab[f].tname = NULL;
    filetab[f].fp = NULL;
}

/* ostgetc - get a character from the terminal */
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

LOCAL char *xfgets(s, n)
     char *s;
     int n;
{
  int c;
  char *cs;

  cs = s;
  while (--n > 0 && (c = TTYGetC()) != EOF) {
    *cs++ = (char) c;
    if (c == '\n') break;
  }
  if (c == EOF && cs==s) return(NULL);
  *cs++ = '\0';
  return(s);
}

/* ostputc - put a character to the terminal */
void ostputc(ch)
  int ch;
{
  if (ch == '\n') lposition = 0;
  else lposition++;

  if (tfp != CLOSED) OSPUTC(ch,tfp);
  TTYPutC(ch);
}

/* osflush - flush the terminal input buffer */
void osflush()
{
  TTYFlush();
  lindex = lcount = 0;
}

VOID osforce(fp)
     FILEP fp;
{
  if (fp == CONSOLE)
    TTYFlush();
  else
    fflush(filetab[fp].fp);
}

void ossymbols()
{
  statsymbols();
}

void osfinit()
{
}

#ifdef max
#undef max
#endif

int max(x, y)
     int x, y;
{
  return((x > y) ? x : y);
}

#ifdef min
#undef min
#endif
int min(x, y)
     int x, y;
{
  return((x < y) ? x : y);
}

LVAL string2stream(char *s)
{
  LVAL stream, newustream();
  xlsave1(stream);
  stream = newustream();
  for (; *s != '\0'; s++) xlputc(stream, *s);
  xlpop();
  return(stream);
}

LVAL readevalstream(LVAL stream)
{
  LVAL expr, oldenv, oldfenv, val;
  CONTEXT cntxt;

  if (! ustreamp(stream)) xlfail("not a ustream");

  /* protect some pointers */
  xlstkcheck(4);
  xlprotect(stream);
  xlsave(expr);
  xlsave(oldenv);
  xlsave(oldfenv);

  /* set the lexical environment to null */
  oldenv = xlenv; xlenv = NIL;
  oldfenv = xlfenv; xlfenv = NIL;
  val = NIL;

  /* read and evaluate each expression in the stream */
  xlbegin(&cntxt,CF_ERROR,s_true);
  if (! XL_SETJMP(cntxt.c_jmpbuf)) {
    while (xlread(stream,&expr,FALSE,FALSE)) {
      val = xleval(expr);
    }
  }
  xlend(&cntxt);

  /* reset the environment */
  xlenv = oldenv;
  xlfenv = oldfenv;
  xlpopn(4);
  return val;
}

static void errcatch(int sig)
{
  signal(sig, errcatch);
  switch(sig) {
  case SIGABRT: xlfail("Abnormal termination signal -- time to bail out");
  case SIGFPE:  xlfail("Floating point exception.");
  case SIGILL:  xlfail("Illegal instruction -- time to bail out");
  case SIGSEGV: xlfail("Segment violation -- time to bail out");
  default: xexit();
  }
}

#if defined(__TURBOC__) && (__TURBOC__ >= 0x0400)
int _FARFUNC _matherr (struct exception *e)
#pragma argsused
{
  return 1;
}
#else
int __cdecl matherr(struct exception *e)
{
  return(1);
}
#endif

void osreset()
{
#ifndef NOGRAPHICS
  MSWResetMenus();
  MSWResetDialogs();
  MSWResetGraphics();
#endif /* NOGRAPHICS */
}

#ifdef NOGRAPHICS
set_gc_cursor() {}
#endif /* NOGRAPHICS */

#ifndef HZ
#define HZ 60
#endif

unsigned long ticks_per_second() { return((unsigned long) HZ); }

unsigned long real_tick_count()
{
  return((unsigned long) ((HZ / CLK_TCK) * clock()));
}

unsigned long run_tick_count()
{
  return real_tick_count();
}

unsigned long system_tick_count()
{
  return((unsigned long) (time((unsigned long *) NULL)));
}

int renamebackup(char *name)
#pragma argsused
{
  return(TRUE);
}

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

VOID disable_interrupts() {}

VOID enable_interrupts() {}

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
/***** probably needs to be protected by check that dirent is available */
/***** need to drop non-files, i.e. directories, using stat() call */
#include <dirent.h>

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

void get_directory(char *s)
{
  int n;
  char *dir = getenv("XLISPLIB");
  if (dir == NULL) dir = "";
  strcpy(s, dir);
  n = strlen(s);
  if (n > 0 && s[n - 1] != '\\')
    strcat(s, "\\");
}

int MSWAnyEventsQueued()
{
  LVAL queue = getvalue(s_event_queue);
  return consp(queue) ? TRUE : FALSE;
}

void MSWDoEventQueue(void)
{
  LVAL task, queue, oldenv, oldfenv, olddenv;

  queue = getvalue(s_event_queue);
  if (consp(queue)) {
    olddenv = xldenv;
    xldbind(s_in_callback, s_true);

    /* set the lexical environment to null */
    xlstkcheck(2);
    xlsave(oldenv);
    xlsave(oldfenv);
    oldenv = xlenv; xlenv = NIL;
    oldfenv = xlfenv; xlfenv = NIL;

    task = car(queue);
    setvalue(s_event_queue, cdr(queue));
    xleval(task);

    /* reset the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;
    xlpopn(2);

    xlunbind(olddenv);
  }
}

#ifdef BIGNUMS
/* We need this because the one that Borland supplies doesn't work for
    exp values out of range */
unsigned char infp[8] = {0,0,0,0,0,0,0xf0, 0x7f};
unsigned char infn[8] = {0,0,0,0,0,0,0xf0, 0xff};

double myldexp(double val, int exp) {
    if (exp > DBL_MAX_EXP)
        return (val > 0 ? *(double *)&infp : *(double *)&infn);
    if (exp < DBL_MIN_EXP) return 0.0;
    return ldexp(val, exp);
}
#endif
