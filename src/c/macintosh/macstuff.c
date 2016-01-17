/* macstuff.c - macintosh interface routines for xlisp-stat */

/* This should enable use of GetAppFiles, etc., with universal headers */
#if ! defined(powerc)
# ifndef OBSOLETE
#  define OBSOLETE
# endif
#endif

#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"
#include "dirent.h"
#include "version.h"

#include <EPPC.h>
#include <AppleEvents.h>
#include <Aliases.h>
#include <Script.h>
#include <Notification.h>
#include <stat.h>

# define arrow qd.arrow

#ifdef applec
# ifdef mc68881
#  ifndef _MC68881_
#   define _MC68881_
#  endif _MC68881_
# else
#  ifdef _MC68881_
#   undef _MC68881_
#  endif _MC68881_
# endif /* mc68881 */
#endif /* applec */
#ifdef THINK_C
# if (__option(mc68881))
#  ifndef _MC68881_
#   define _MC68881_
#  endif _MC68881_
# else
#  ifdef _MC68881_
#   undef _MC68881_
#  endif _MC68881_
# endif
#endif /* THINK_C */
#ifdef __MWERKS__
# if (defined(__MC68881__) && __MC68881__)
#  ifndef _MC68881_
#   define _MC68881_
#  endif _MC68881_
# else
#  ifdef _MC68881_
#   undef _MC68881_
#  endif _MC68881_
# endif
#endif /* __MWERKS__ */

# include "TransEdit1.h"

/* resource numbers */
#define GC_CURS_RES 1800

/* externals */
extern int in_send;
extern LVAL s_input_stream, s_event_queue, s_use_notifier;
extern LVAL s_in_callback;
extern char *resfile;

/* forward declarations */
#ifdef _MC68881_
static VOID check_MC68881 _((void));
#endif
static pascal VOID background_tasks _((void));
static VOID waitforline _((void));
static VOID warn_low_memory _((long space));
#ifdef FILETABLE
static FILEP osopen(const char *name, const char *mode);
#endif /* FILETABLE */
static char *xfgets _((char *s, int n));
static int xostgetc _((void));
static pascal Boolean key_hook _((EventRecord *evt));
static pascal void do_suspend_resume _((Boolean inForground));
static pascal OSErr handleOAPP _((AppleEvent *evt, AppleEvent *reply, long ref));
static pascal OSErr handleODOC _((AppleEvent *evt, AppleEvent *reply, long ref));
static pascal OSErr handlePDOC _((AppleEvent *evt, AppleEvent *reply, long ref));
static pascal OSErr handleQUIT _((AppleEvent *evt, AppleEvent *reply, long ref));
static pascal OSErr handleDOSC _((AppleEvent *evt, AppleEvent *reply, long ref));
static pascal void  handleAE _((EventRecord *evt));
static pascal Boolean AEidle _((EventRecord *evt, long *sleep, RgnHandle *mregion));
static OSErr RequiredCheck _((AppleEvent *evt));
static OSErr findprocessbyname _((char *name, ProcessSerialNumber *psn));
static OSErr findprocessbytype _((OSType type, ProcessSerialNumber *psn));
static LVAL makeprocessinfo _((ProcessSerialNumber *psn));
static OSErr getpsn _((LVAL data, ProcessSerialNumber *psn));
static LVAL makeaetarget _((PortInfoRec *pir, LocationNameRec *lnr));
static OSErr getaetarget _((LVAL data, TargetID *target));
static LVAL nreverse _((LVAL ));
static pascal Boolean browsefilter _((LocationNameRec *lnr, PortInfoRec *pir));
static int getprefstring _((char *name, char *s));
static short getpreffontnum _((char *name, short dflt));
static short getpreffontsize _((char *name, short dflt));
static Style getpreffontstyle _((char *name, Style dflt));

/* global variables */
DirSpec default_dir;
int hasAppleEvents;
short editFontNum, editFontSize;
short listenerFontNum, listenerFontSize;
short graphFontNum, graphFontSize;
Style editFontStyle, listenerFontStyle, graphFontStyle;
int color_allowed = TRUE;

/* local variables */
static time_t time_stamp;
WindowPtr ttyWind;
static LVAL input_stream;
/* -- local variables */
#define LBSIZE  200
static  char    lbuf[LBSIZE];
static  int     lindex;
static  int     lcount;
static char homedir[FNAMEMAX+1];
static int initialized = FALSE;
static int hasWNE;
static int initialAEdone = FALSE;
static int quit_event_received = FALSE;

static NMRec noterec;
static int note_pending = FALSE;

#define AE_OK 0
#define AE_ERROR 1
#define AE_UNWIND 2
static int apple_event_status = AE_OK;

static short num_app_files = 0;
static FileSpec *app_files = nil;

#ifdef STSZ
char *stackbase;
#endif

#define C2PSTRING(x,y) CintoPstring(x, y, sizeof y, FALSE)

static int getnextword(char *s, char *buf)
{
  int n = 0;
  unsigned char ch;
  int have_word;

  while ((ch = *s) != '\0' && isspace(ch)) { n++; s++; }
  have_word = ((ch = *s) != '\0' && ! isspace(ch)) ? TRUE : FALSE;
  while ((ch = *s) != '\0' && ! isspace(ch)) { n++; *buf++ = *s++; }
  *buf = '\0';
  return (have_word > 0) ? n : 0;
}

int getprefstring(char *name, char *s)
{
  FILE *fp;
  DirSpec dir;
  int result = FALSE, count;
  char *pbuf = buf;
  
  GetDir(&dir);
  SetDir(&default_dir);
  fp = fopen("XLS Preferences", "r");
  SetDir(&dir);

  if (fp != NULL) {
    char nbuf[129];
    while (fgets(pbuf, STRMAX, fp)) {
      if ((count = getnextword(pbuf, nbuf)) == 0)
        break;
      pbuf += count;
      if (strcmp(name, nbuf) == 0) {
        if ((count = getnextword(pbuf, nbuf)) == 0)
          break;
        strcpy(s, nbuf);
        pbuf += count;
        while ((count = getnextword(pbuf, nbuf)) != 0) {
          strcat(s, " ");
          strcat(s, nbuf);
          pbuf += count;
        }
        result = TRUE;
        break;
      }
      pbuf = buf;
    }
    fclose(fp);
  }
  return result;
}

static short getpreffontnum(char *name, short dflt)
{
  short fnum;
  
  if (getprefstring(name, buf)) {
    CtoPstr(buf);
    GetFNum((StringPtr) buf, &fnum);
    if (fnum == 0)
      return dflt;
    else
      return fnum;
  }
  else
    return dflt;
}

static short getpreffontsize(char *name, short dflt)
{
  int size;
  
  if (getprefstring(name, buf) && sscanf(buf, "%d", &size) != EOF)
    return (short) size;
  else
    return dflt;
}

static Style getpreffontstyle(char *name, Style dflt)
{
  Style style;
  
  if (getprefstring(name, buf)) {
    char sbuf[129], *pbuf;
    int count;
    style = normal;
    pbuf = buf;
    while ((count = getnextword(pbuf, sbuf)) != 0) {
      pbuf += count;
      switch (isupper(sbuf[0]) ? tolower(sbuf[0]) : sbuf[0]) {
      case 'b': style |= bold;   break;
      case 'i': style |= italic; break;
      case 'u': style |= underline;
      case 'o': style |= outline;
      case 's': style |= shadow;
      case 'c': style |= condense;
      case 'e': style |= extend;
      }
    }
    return style;
  }
  else
    return dflt;
}

static AEEventHandlerUPP handleOAPPUPP;
static AEEventHandlerUPP handleODOCUPP;
static AEEventHandlerUPP handlePDOCUPP;
static AEEventHandlerUPP handleQUITUPP;
static AEEventHandlerUPP handleDOSCUPP;

VOID osinit(char *name)
{
  long size;
  SkelInitParams initParams;
  
#ifdef STSZ
  stackbase = (char *)&name;    /* find base of stack */
#endif
  
  time_stamp = time((time_t *) 0);

  SkelGetInitParams (&initParams);
  if ((char *) GetApplLimit() - (char *) GetZone() < 1200000)
	initParams.skelStackAdjust = 2048;
  else
#ifdef STSZ
    initParams.skelStackAdjust = STSZ;
#else
    initParams.skelStackAdjust = - 6 * 32768;
#endif
  SkelInit (&initParams);
  SetEWindowCreator ('X1St');
  SkelSetMenuHook (UpdateLispMenus);
  SkelSetEventHook (key_hook);
  SkelSetSuspendResume(do_suspend_resume);
  SkelSetIdle(background_tasks);
  SkelSetWaitTimes(2, 60);
  hasWNE = SkelQuery(skelQHasWNE);
  hasAppleEvents = SkelQuery(skelQHasAppleEvents);
  
#ifdef _MC68881_
  check_MC68881();
#endif _MC68881_
  MaxMem(&size);
  {
    CursHandle c = GetCursor(watchCursor);
    if (c != nil) SetCursor(*c);
  }
  
  /* find the default volume and its path name */
  /* For pre System 7 also find the startup files at this time */
  if (hasAppleEvents) {
    ProcessInfoRec pir;
    ProcessSerialNumber psn;
    FSSpec fsSpec;
    
    psn.highLongOfPSN = 0;
    psn.lowLongOfPSN = kCurrentProcess;    
    
    pir.processInfoLength = sizeof(ProcessInfoRec);
    pir.processName = nil;
    pir.processAppSpec = &fsSpec;
    
    GetProcessInformation(&psn, &pir);
    default_dir.vRefNum = fsSpec.vRefNum;
    default_dir.parID = fsSpec.parID;
    if (! getwd(homedir)) strcpy(homedir, "");
  }
#ifndef powerc
  else {
    Handle h;
    short print, i;
    AppFile a;
    DirSpec dir;
    
    GetAppParms((StringPtr) buf, &default_dir.vRefNum, &h);
    default_dir.parID = 0;
    CountAppFiles(&print, &num_app_files);
    if (print) exit(1);

    if (num_app_files > 0) {
      app_files = (FileSpec *) CALLOC(num_app_files, sizeof(FileSpec));          
      for (i = 0; i < num_app_files; i++) {
        GetAppFiles(i + 1, &a);
        app_files[i].dir.vRefNum = a.vRefNum;
        app_files[i].dir.parID = 0;
        app_files[i].type = a.fType;
        PtoCstr((StringPtr) a.fName);
        strcpy(app_files[i].name, (char *) a.fName);
      }
    }

    GetDir(&dir);
    SetDir(&default_dir);
    if (! getwd(homedir)) strcpy(homedir, "");
    SetDir(&dir);
  }
#endif /* powerc */

  /* install apple event handlers and wait for initial high level event */
  if (hasAppleEvents) {
    handleOAPPUPP = NewAEEventHandlerProc((ProcPtr) handleOAPP);
    handleODOCUPP = NewAEEventHandlerProc((ProcPtr) handleODOC);
    handlePDOCUPP = NewAEEventHandlerProc((ProcPtr) handlePDOC);
    handleQUITUPP = NewAEEventHandlerProc((ProcPtr) handleQUIT);
    handleDOSCUPP = NewAEEventHandlerProc((ProcPtr) handleDOSC);
    AEInstallEventHandler(kCoreEventClass, kAEOpenApplication, handleOAPPUPP, 0, FALSE);
    AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments,   handleODOCUPP, 0, FALSE);
    AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments,  handlePDOCUPP, 0, FALSE);
    AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, handleQUITUPP, 0, FALSE);
    AEInstallEventHandler('misc', 'dosc', handleDOSCUPP, 0, FALSE);
    SkelSetAEHandler(handleAE);
    
    while (! initialAEdone) {
      EventRecord evt;
      if (WaitNextEvent(highLevelEventMask + keyDownMask, &evt, 0, nil)) {
        if (SkelCmdPeriod(&evt))
          initialAEdone = TRUE;
        else if (evt.what == kHighLevelEvent) {
          AEProcessAppleEvent(&evt);
          if (quit_event_received)
            exit(0);
          initialAEdone = TRUE;
        }
      }
    }
  }

  /* set some defaults from the preferences file */
  editFontNum = getpreffontnum("EditFontName", 4);
  editFontSize = getpreffontsize("EditFontSize", 9);
  editFontStyle = getpreffontstyle("EditFontStyle", normal);
  listenerFontNum = getpreffontnum("ListenerFontName", editFontNum);
  listenerFontSize = getpreffontsize("ListenerFontSize", editFontSize);
  listenerFontStyle = getpreffontstyle("ListenerFontStyle", editFontStyle);
  graphFontNum = getpreffontnum("GraphFontName", 0);
  graphFontSize = getpreffontsize("GraphFontSize", 9);
  graphFontStyle = getpreffontstyle("GraphFontStyle", normal);
  if (getprefstring("Workspace", buf)) {
    resfile = malloc(strlen(buf) + 1);
    strcpy(resfile, buf);
  }
  if (getprefstring("Color", buf)) {
    int i, n;
    for (i = 0, n = strlen(buf); i < n; i++)
      if (isupper(buf[i]))
        buf[i] = tolower(buf[i]);
    if (! strcmp(buf, "off") || ! strcmp(buf, "no"))
      color_allowed = FALSE;
  }

  /* make the listener window */
  if (ttyWind == nil) {
    Rect r;
    SetRect(&r, 10, 40, 500, 320);
    make_listener_window(r);
  }

  /* initialize the reader */
  lposition = 0;
  lindex  = 0;
  lcount  = 0;

  /* display the startup message */
  TtyPrint(name);
  sprintf(buf, "\nXLISP-STAT Release %d.%d.%d%s.\n",
	  XLS_MAJOR_RELEASE, XLS_MINOR_RELEASE, XLS_SUBMINOR_RELEASE,
	  XLS_RELEASE_STATUS);
  TtyPrint(buf);
  sprintf(buf, "Copyright (c) 1989-1999, by Luke Tierney.\n\n"); 
  TtyPrint(buf);
}

VOID osfinish(void)
{
  CONTEXT cntxt;
  xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
  if (XL_SETJMP(cntxt.c_jmpbuf))
    exit(0);
  if (s_breakenable != NULL) setvalue(s_breakenable, NIL);
#ifdef CONDITIONS
  if (s_condition_hook != NULL) setvalue(s_condition_hook, NIL);
#endif /* CONDITIONS */
  while(! ClobberEWindows())	/* deal with any open edit windows */
    SysBeep(10);
  SkelCleanup ();		/* throw away windows and menus */
  exit(0);
}

VOID osreset(void)
{
  in_send = FALSE;
  StGWResetBuffer();
}

VOID xoserror(char *msg)
{
  Str255 pbuf;
  NotifyIfInBackground();
  C2PSTRING(msg, pbuf);
  FakeAlert (pbuf, "\p", "\p", "\p", 1, 1, 0, "\pOK", "\p", "\p");
}

FILEP osaopen(const char *name, const char *mode)
{
#ifdef FILETABLE
  return osopen(name, mode);
#else
  return fopen(name,mode);
#endif
}

FILEP osbopen(const char *name, const char *mode)
{
  char nmode[4];
  strcpy(nmode, mode);
  strcat(nmode, "b");
#ifdef FILETABLE
  return osopen(name, nmode);
#else
  return fopen(name, nmode);
#endif
}

#ifdef FILETABLE
extern VOID gc();

int truename(char *name, char *rname)
{
  char *cp;
  char pathbuf[FNAMEMAX+1];   /* copy of path part of name */
  char curdir[FNAMEMAX+1];    /* current directory */
  char *fname;                /* pointer to file name part of name */

  /* check for absolute path (good news!) */
  if (strrchr(name, ':') && *name != ':') {
    strcpy(rname, name);
  }
  else {
    strcpy(pathbuf, name);
    if ((cp = strrchr(pathbuf, ':')) != NULL) { /* path present */
      cp[1] = 0;
      fname = strrchr(name, ':') + 1;
    }
    else {
      pathbuf[0] = 0;
      fname = name;
    }

    /* get the current directory of the selected drive */
    if (! getwd(curdir))
      return FALSE;

    /* peel off "::"s */
    while (strncmp(pathbuf, "::", 2) == 0) {
      if (*curdir == 0) return FALSE;     /* already at root */
      strcpy(pathbuf, pathbuf+1);
      if (((cp = strrchr(curdir, ':')) - curdir) + 1 == strlen(curdir))
        *cp = 0;    /* get rid of trailing ':' */
      if ((cp=strrchr(curdir, ':')) != NULL)
        cp[1] = 0;    /* peel one depth of directories */
      else {
        *curdir = 0;    /* peeled back to root */
        if (pathbuf[0] == ':' && pathbuf[1] == 0)
          return(FALSE);  /**** ambiguous desktop reference -- not quite right */
        }
      }
        
    /* allow for a ':' */
    if (*pathbuf == ':')
      strcpy(pathbuf, pathbuf+1);
        
    /* final name is curdir:pathbuf:fname */
    if ((int)(strlen(pathbuf)+strlen(curdir)+strlen(fname)+4) > FNAMEMAX) 
      return FALSE;
        
    if (*curdir)
      sprintf(rname, "%s%s%s", curdir, pathbuf, fname);
    else
      sprintf(rname, "%s%s", pathbuf, fname);
  }
  return TRUE;
}

int getslot(void)
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

LOCAL FILEP osopen(const char *name, const char *mode)
{
  int i=getslot();
  char namebuf[FNAMEMAX+1];
  FILE *fp;

  if (!truename((char *)name, namebuf))
    strcpy(namebuf, name);  /* should not happen */

  if ((filetab[i].tname = (char *)malloc(strlen(namebuf)+1)) == NULL) {
    xlfail("insufficient memory");
  }

  /* MPW fopen seems to mess up the creation when Apple events are on */
  if (mode[0] == 'w' || mode[0] == 'a') {
	IOParam pb;
	Str255 pbuf;

    C2PSTRING(name, pbuf);

	pb.ioNamePtr = pbuf;
	pb.ioVRefNum = 0;
	pb.ioVersNum = 0;
	pb.ioPermssn = fsWrPerm;
	pb.ioMisc = 0;

    PBCreateSync((ParmBlkPtr) &pb);
  } 

  if ((fp = fopen(name,mode)) == NULL) {
  	free(filetab[i].tname);
    return CLOSED;
  }
    
  filetab[i].fp = fp;

 if (mode[0] == 'w' || mode[0] == 'a') {
   strcpy(buf, name);
   fsetfileinfo (buf, 'X1St', 'TEXT');
  }
  
  strcpy(filetab[i].tname, namebuf);

  return i;
}
    
VOID osclose(FILEP f)
{
    if (filetab[f].fp != NULL)
        fclose(filetab[f].fp);
    /* remind stdin/stdout/stderr */
    if (f>2 && filetab[f].tname != NULL)
        free(filetab[f].tname);
    filetab[f].tname = NULL;
    filetab[f].fp = NULL;
}

int osmtime(char *fname, time_t *mtime)
{
  Str255 nbuf;
  CInfoPBRec pb;
  HFileInfo *hpb = (HFileInfo *) &pb;
  
  C2PSTRING(fname, nbuf);
	
  /* setup the parameter block and make a synchronous PB call */
  hpb->ioCompletion = nil;
  hpb->ioVRefNum = hpb->ioFDirIndex = 0;
  hpb->ioNamePtr = nbuf;
  hpb->ioDirID = 0L;

  if(PBGetCatInfoSync(&pb))
    return -1;

  *mtime = hpb->ioFlMdDat;
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
      val = cons(cvstring((char *) dentry->d_name), val);
    closedir(dir);
  }
  xlpop();
  enable_interrupts();

  return val;
}
#endif /* FILETABLE */

/* -- ostgetc - get a character from the terminal */
int ostgetc(void)
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

LOCAL char *xfgets(char *s, int n)
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

int xostgetc(void)
{
  int ch;

  while (! ustreamp(input_stream) || (ch = xlgetc(input_stream)) == EOF)
    waitforline();
  return (ch == RETURNCHAR) ? ch = '\n' : ch;
}

VOID ostputc(int ch)
{
  if (ch == '\n') lposition = 0;
  else lposition++;

  if (tfp != CLOSED) OSPUTC(ch, tfp);
  ch = (ch == '\n') ? RETURNCHAR : ch;
  TtyPutc(ch);
}

/* osflush - flush the input line buffer */
VOID osflush(void)
{
  while (ustreamp(input_stream) && xlgetc(input_stream) != EOF)
    ;
  lindex = lcount = 0;
}

VOID osforce(FILEP fp)
{
#ifdef FILETABLE
  if (fp == CONSOLE) TtyFlush();
  else fflush(filetab[fp].fp);
#else
  if (fp == CONSOLE) TtyFlush();
  else fflush(fp);
#endif /* FILETABLE */
}

/* oscheck - check for control characters during execution */
/* command-period is the interrupt for the mac             */
/* also allows switching in and out of background          */
#define bgMask osMask + mDownMask + activMask + updateMask
VOID oscheck(void)
{
  EventRecord theEvent;
  static EventRecord lastEvent;
  int in_callback = null(getvalue(s_in_callback)) ? FALSE : TRUE;

  if (! initialized) return;
  
  if (EventAvail(autoKeyMask + bgMask, &theEvent)) {
    switch(theEvent.what) {
    case autoKey:
      if (theEvent.when == lastEvent.when) {
        GetNextEvent(autoKeyMask, &theEvent);
        if (SkelCmdPeriod(&theEvent)) {
	      FlushEvents (everyEvent - diskMask, 0 );      
          xlsigint();
        }
      }
      else lastEvent = theEvent;
      break;
    case osEvt:
    case mouseDown:
    case activateEvt:
    case updateEvt:
      if (hasWNE && ! in_callback) {
        while (WaitNextEvent (bgMask, &theEvent, 0, nil))
          SkelRouteEvent (&theEvent);
      }
      break;
    }
  }
  if (hasWNE && ! in_callback && ! SkelQuery(skelQInForeground)) {
    if (WaitNextEvent (bgMask, &theEvent, 60, nil)) {
      SkelRouteEvent (&theEvent);
    }
  }
}

VOID ossymbols(void)
{
  statsymbols();
}

/*
  Show a window if it's not visible.  Select the window FIRST, then
  show it, so that it comes up in front.  Otherwise it will be drawn
  in back then brought to the front, which is ugly.
*/

VOID MyShowWindow (WindowPtr wind)
{
  SelectWindow(wind);
  ShowWindow(wind);
}

/*
  Fancier version of CtoPstr. This one allows a buffer to be given and
  makes sure the string is no longer than the specified buffer length
  minus one. The return value specifies whether an overflow occurred.
  Also converts newlines if requested.
*/
int CintoPstring(const char *cp, StringPtr pp, int ppsize, int cvtnl)
{
  int n = strlen(cp);
  int overflow = n > ppsize - 1 ? TRUE : FALSE;
  if (overflow)
  	n = ppsize - 1;
  pp[0] = n;
  memmove(pp + 1, cp, n);
  if (cvtnl)
    for (pp++; n > 0; n--, pp++)
      if (*pp == '\n')
        *pp = RETURNCHAR;
  return overflow;
}

LOCAL VOID waitforline(void)
{
  initialized = TRUE;
  NotifyIfInBackground();
  SkelEventLoop ();
}
	
VOID getttyline(LVAL s)
{
  input_stream = getvalue(s_input_stream);
  SkelStopEventLoop();
}

static pascal VOID background_tasks(void)
{
  LVAL task, queue, oldenv, oldfenv, olddenv;
    
  if (s_event_queue == NULL)
    return;

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

VOID set_gc_cursor(int on)
{
  static Cursor;
  CursHandle c;
  
  if (on && (c = GetCursor(GC_CURS_RES)) != nil) SetCursor(*c);
  else {
    SetCursor(&arrow);
	/* oscheck(); */
    warn_low_memory(75000);
  }
}

VOID enable_interrupts(void) {}
VOID disable_interrupts(void) {}

max(int x, int y)
{
  return((x > y) ? x : y);
}

min(int x, int y)
{
  return((x < y) ? x : y);
}

#define TIMEGAP 120L
#ifdef applec
#define WARNSTRING "Memory is down to %ldK. \nClose plots and \"undef\" variables."
#endif /* applec */
#ifdef THINK_C
#define WARNSTRING "Memory is down to %ldK. \rClose plots and \"undef\" variables."
#endif /* THINK_C */
#ifdef __MWERKS__
#define WARNSTRING "Memory is down to %ldK. \rClose plots and \"undef\" variables."
#endif /* applec */

static VOID warn_low_memory(long space)
{
  char s[100];
  unsigned long tm;
  static unsigned long old_tm = 0L;
  static int inited = FALSE;
  long free;
  
  if (FreeMem() < space) {
    MaxMem(&free);
    free = FreeMem();
    if (free < space) {
      tm = time(NULL);
      if (! inited || tm > old_tm + TIMEGAP) {
        old_tm = tm;
        NotifyIfInBackground();
        sprintf(s, (char *) WARNSTRING, free / 1000);
		CtoPstr(s);
        FakeAlert ((StringPtr) s, "\p", "\p", "\p", 1, 1, 0, "\pOK", "\p", "\p");
      }
      inited = TRUE;
    }
  }
}

VOID maximum_memory(void)
{
  long size;
  MaxMem(&size);
}

#ifdef _MC68881_
static VOID check_MC68881(void)
{
  SysEnvRec r;

  SysEnvirons(1, &r);
  if (! r.hasFPU) {
    NotifyIfInBackground();
    FakeAlert ("\pThis version requres the MC68881", "\p", "\p", "\p",
               1, 1, 0, "\pOK", "\p", "\p");
    exit(1);
  }
}
#endif _MC68881_

#ifndef HZ
#define HZ 60
#endif

unsigned long ticks_per_second(void) { return((unsigned long) HZ); }

unsigned long run_tick_count(void)
{
  return((unsigned long) TickCount());
}

unsigned long real_tick_count(void) 
{
  return((unsigned long) (60L * (time(NULL) - time_stamp)));
}

unsigned long system_tick_count(void)
{
  return((unsigned long) (time(NULL)));
}

void get_directory(char *s)
{
  strcpy(s, homedir);
}

#undef SysBeep
VOID SYSBEEPMPW(int x)
{
  SysBeep(x);
}

int renamebackup(char *name) { return(TRUE); }

#ifndef applec
VOID fsetfileinfo (char *fname, OSType creator, OSType type)
{
  FInfo finfo;
  FSSpec fsspec;
  DirSpec dir;
  Str255 pbuf;

  GetDir(&dir);
  C2PSTRING(fname, pbuf);
  if (hasAppleEvents) {
	FSMakeFSSpec(dir.vRefNum, dir.parID, pbuf, &fsspec);
	FSpGetFInfo(&fsspec, &finfo);
  }
  else
    GetFInfo(pbuf, dir.vRefNum, &finfo);
  finfo.fdCreator = creator;
  finfo.fdType = type;
  if (hasAppleEvents)
    FSpSetFInfo(&fsspec, &finfo);
  else
    SetFInfo(pbuf, dir.vRefNum, &finfo);
}
#endif /* applec */

LOCAL pascal Boolean key_hook(EventRecord *evt)
{
   WindowPtr w;
   
   if (evt->what == keyDown || evt->what == autoKey) {
     if (evt->modifiers & cmdKey) {
       if (SkelCmdPeriod(evt))
         xlsigint();
       return FALSE;
     }
     else if (IsEWindow((w = FrontWindow()))) {
       return edit_key_filter(w, evt->message & charCodeMask);
     }
     else
       return FALSE;
   }
   else
     return FALSE;
 }
 
 static pascal void do_suspend_resume(Boolean inForeground)
 {
   SkelActivate(FrontWindow(), inForeground);
   if (inForeground && note_pending) {
     NMRemove(&noterec);
     if (noterec.nmIcon)
       ReleaseResource(noterec.nmIcon);
     note_pending = FALSE;
   }
 }
 
/* macxlinit - find alternate startup workspace, if supplied, and call clinit */
int macxlinit(char *resfile)
{
  FileSpec a, res;
  int i;

  res.dir = default_dir;
  strcpy(res.name, resfile);
          
  for (i = 0; i < num_app_files; i++) {
    a = app_files[i];
    if (a.type == 'X1Ws') {
	  res = a;
	  break;
    }
  }
    
  SetDir(&res.dir);
  i = xlinit(res.name);
  return i;
}

/* macloadinits - load startup files (also used for ODOC while running) */
VOID macloadinits(void)
{
  FileSpec a;
  int i;

  initialized = TRUE;

  for (i = 0; i < num_app_files; i++) {
    a = app_files[i];
    if (a.type == 'TEXT') {
	  SetDir(&a.dir);
	  if (!xsload(a.name,TRUE,FALSE))
	    xlerror("can't load file",cvstring(a.name));
	}
  }
  num_app_files = 0;
  if (app_files != nil)
    MFREE(app_files);
}

/**** these should be moved somewhere generic */
/* xgetwd - builtin function GET-WORKING-DIRECTORY */
LVAL xgetwd(void)
{
  xllastarg();
  if (! getwd(buf))
    return NIL;
  else
    return cvstring(buf);
}

/* xsetwd - builtin function SET-WORKING-DIRECTORY */
LVAL xsetwd(void)
{
  char *dir = getstring(xlgastring());
  xllastarg();
  if (chdir(dir))
    return NIL;
  else
    return s_true;
}

/* string_to_type - convert 4-character string to corresponding character constant */
OSType string_to_type(char *s)
{
   OSType v;
   int n, c;
   
   for (n = 4, v = 0; n > 0 && ((c = *s) != 0); n--, s++)
     v = 256 * v + c;
   return v;
}

/* xstrostype - Lisp function to convert string to OSType */
LVAL xstrtoostype(void)
{
  char *s = getstring(xlgastring());
  xllastarg();
  return cvfixnum((FIXTYPE) string_to_type(s));
}

/* nreverse the result */
static LVAL nreverse(LVAL x)
{
  LVAL val, nextr;

  for (val = NIL; consp(x);) {
	nextr = cdr(x);
	rplacd(x, val);
	val = x;
	x = nextr;
  }
  return val;
}


/* get directory information */
OSErr GetDir(DirSpec *d)
{
  if (hasAppleEvents) {
    WDPBRec pb;
    OSErr err;
    
    pb.ioCompletion = nil;
    pb.ioNamePtr = nil;
    if ((err = PBHGetVolSync(&pb)) != noErr)
      return err;
    else {
      d->vRefNum = pb.ioVRefNum;
      d->parID = pb.ioWDDirID;
      return noErr;
    }
  }
  else {
    d->parID = 0;
    return GetVol((StringPtr) buf, &d->vRefNum);
  }
}

/* set directory information */
OSErr SetDir(DirSpec *d)
{
  if (hasAppleEvents) {
    WDPBRec pb;

    pb.ioCompletion = nil;
    pb.ioNamePtr = nil;
    pb.ioVRefNum = d->vRefNum;
    pb.ioWDDirID = d->parID;
    return PBHSetVolSync(&pb);
  }
  else
    return SetVol(nil, d->vRefNum);
}

void NotifyIfInBackground(void)
{
  if (SkelQuery(skelQInForeground) || s_use_notifier == NULL)
    return;
  
  if (! note_pending && hasAppleEvents && ! null(getvalue(s_use_notifier))) {
    noterec.qType = nmType;
    noterec.nmMark = 1;
    noterec.nmIcon = GetResource('SICN', 128);
    HNoPurge(noterec.nmIcon);
    noterec.nmSound = (Handle) -1L;
    noterec.nmStr = nil;
    noterec.nmResp = nil;
    noterec.nmRefCon = 0;
    
    if (NMInstall(&noterec) == noErr)
      note_pending = TRUE;
  }
  while (hasWNE && ! SkelQuery(skelQInForeground)) {
    EventRecord evt;
    if (WaitNextEvent (bgMask, &evt, 60, nil))
      SkelRouteEvent (&evt);
  }
}


/**********************************************************************************/
/**********************************************************************************/
/**                                                                              **/
/**                         Process Management Routines                          **/
/**                                                                              **/
/**********************************************************************************/
/**********************************************************************************/

static OSErr findprocessbyname(char *name, ProcessSerialNumber *psn)
{
  ProcessInfoRec pir;
  FSSpec fsSpec;
  
  psn->highLongOfPSN = 0;
  psn->lowLongOfPSN = kNoProcess;
  pir.processInfoLength = sizeof(ProcessInfoRec);
  pir.processName = (StringPtr) buf;
  pir.processAppSpec  = &fsSpec;

  while (GetNextProcess(psn) != procNotFound) {
    if (GetProcessInformation(psn, &pir) != noErr)
      return procNotFound;
    PtoCstr((StringPtr) buf);
    if (strcmp(buf, name) == 0)
      return noErr;
  }
  
  return procNotFound;
}

static OSErr findprocessbytype(OSType type, ProcessSerialNumber *psn)
{
  ProcessInfoRec pir;
  FSSpec fsSpec;
  
  psn->highLongOfPSN = 0;
  psn->lowLongOfPSN = kNoProcess;
  pir.processInfoLength = sizeof(ProcessInfoRec);
  pir.processName = (StringPtr) buf;
  pir.processAppSpec  = &fsSpec;

  while (GetNextProcess(psn) != procNotFound) {
    if (GetProcessInformation(psn, &pir) != noErr)
      return procNotFound;
    if (pir.processSignature == type)
      return noErr;
  }
  
  return procNotFound;
}

/* create lisp representation of process info from serial number */
static LVAL makeprocessinfo(ProcessSerialNumber *psn)
{
  ProcessInfoRec pir;
  FSSpec fsSpec;
  char typestr[5];
  LVAL val;
  
  pir.processInfoLength = sizeof(ProcessInfoRec);
  pir.processName = (StringPtr) buf;
  pir.processAppSpec  = &fsSpec;

  if (GetProcessInformation(psn, &pir) != noErr)
    return NIL;
  
  xlsave1(val);
  val = consa(cvfixnum((FIXTYPE) pir.processActiveTime));
  val = cons(cvfixnum((FIXTYPE) pir.processLaunchDate), val);
  val = cons(cons(cvfixnum((FIXTYPE) pir.processNumber.highLongOfPSN),
                  cvfixnum((FIXTYPE) pir.processNumber.lowLongOfPSN)),
             val);
  MEMCPY(typestr, (char *) &(pir.processSignature), 4);
  typestr[4] = 0;
  val = cons(cvstring(typestr), val);
  PtoCstr((StringPtr) buf);
  val = cons(cvstring(buf), val);
  xlpop();
  
  return val;
}

/* convert process name, creator code, or process info to process serial number */
static OSErr getpsn(LVAL data, ProcessSerialNumber *psn)
{
  if (data == s_true) {
    psn->highLongOfPSN = 0;
    psn->lowLongOfPSN = kCurrentProcess;
    return noErr;
  }
  else if (stringp(data))
    return findprocessbyname(getstring(data), psn);
  else if (fixp(data))
    return findprocessbytype(getfixnum(data), psn);
  else {
    int i;
    for (i = 2; consp(data) && i > 0; i--)
      data = cdr(data);
    if (i != 0)
      return procNotFound;
    if (consp(car(data)))
      data = car(data);
    else
      return procNotFound;
    if (fixp(car(data)) && fixp(cdr(data))) {
      psn->highLongOfPSN = getfixnum(car(data));
      psn->lowLongOfPSN = getfixnum(cdr(data));
      return noErr;
    }
    else
      return procNotFound;
  }
}

/* get the front process and return process info */
LVAL xgetfrontprocess(void)
{
  ProcessSerialNumber psn;
  
  if (! hasAppleEvents)
    return NIL;
    
  if (GetFrontProcess(&psn) == noErr)
    return makeprocessinfo(&psn);
  else
    return NIL;
}

/* set the front process from process name, creator ID, or info and wake up process */
LVAL xsetfrontprocess(void)
{
  ProcessSerialNumber psn;
  LVAL arg;
  int wake;
  
  if (! hasAppleEvents)
    return NIL;
    
  arg = (moreargs()) ? xlgetarg() : s_true;
  wake = (moreargs()) ? (null(xlgetarg()) ? TRUE : FALSE) : TRUE;
  xllastarg();
  
  if (getpsn(arg, &psn) != noErr)
    return NIL;
    
  if (SetFrontProcess(&psn) != noErr)
    return NIL;
  else if (wake && (WakeUpProcess(&psn) != noErr))
    return NIL;
  else return s_true;
}

/* get list of process information for all processes */
LVAL xgetprocesslist(void)
{
  ProcessSerialNumber psn;
  LVAL val;
  
  if (! hasAppleEvents)
    return NIL;
    
  psn.highLongOfPSN = 0;
  psn.lowLongOfPSN = kNoProcess;
  
  xlsave1(val);
  val = NIL;
  while (GetNextProcess(&psn) != procNotFound)
    val = cons(makeprocessinfo(&psn), val);
  xlpop();
  return nreverse(val);
}

/* launch an application */
LVAL xlaunchapp(void)
{
  LaunchParamBlockRec launchParams;
  LaunchFlags lflags;
  FSSpec fSpec;
  LVAL arg;
  int signal;
   
  if (! hasAppleEvents)
    return NIL;
    
  arg = xlgetarg();
  if (moreargs() && null(xlgetarg()))
    lflags = launchContinue + launchNoFileFlags + launchDontSwitch;
  else
    lflags = launchContinue + launchNoFileFlags;
  if (moreargs() && ! null(xlgetarg()))
    signal = TRUE;
  else
    signal = FALSE;
  xllastarg();

  if (stringp(arg)) {
    DirSpec dir;
    Str255 pbuf;

    GetDir(&dir);
	C2PSTRING(getstring(arg), pbuf);
	if (FSMakeFSSpec(dir.vRefNum, dir.parID, pbuf, &fSpec) != noErr)
	  return NIL;
  }
  else if (fixp(arg)) {
	DTPBRec db;
	FInfo finfo;
	
	db.ioCompletion = nil;
	db.ioNamePtr = nil;
    db.ioVRefNum = -1;  /* search the boot volume only */
	  
    if (PBDTGetPath(&db) != noErr) {
      if (signal)
        xlfail("can't access desktop database");
      else
	    return NIL;
	}
	  
	db.ioIndex = 0;
	db.ioFileCreator = getfixnum(arg);
	db.ioNamePtr = (StringPtr) fSpec.name;
	  
	/**** fix to look for newest that exitst? */
	/**** fix to look at all volumes? */
    do {
      if (PBDTGetAPPLSync(&db) != noErr) {
        if (signal)
          xlerror("can't find application with this signature", arg);
        else
	      return NIL;
	  }
	  
	  fSpec.vRefNum = db.ioVRefNum;
	  fSpec.parID = db.ioAPPLParID;
	  db.ioIndex++;
	} while(FSpGetFInfo(&fSpec, &finfo) != noErr);
  }
  else
	xlbadtype(arg);

  launchParams.launchBlockID = extendedBlock;
  launchParams.launchEPBLength = extendedBlockLen;
  launchParams.launchFileFlags = 0;
  launchParams.launchControlFlags = lflags;
  launchParams.launchAppSpec = &fSpec;
  launchParams.launchAppParameters = nil;
	 
  if (LaunchApplication(&launchParams) != noErr) {
    if (signal) {
      PtoCstr(fSpec.name);
      xlerror("can't launch application", cvstring((char *) fSpec.name));
    }
    else
      return NIL;
  }
  else
    return makeprocessinfo(&launchParams.launchProcessSN);
  return NIL; /* not reached */
}

/**********************************************************************************/
/**********************************************************************************/
/**                                                                              **/
/**                            Apple Event Routines                              **/
/**                                                                              **/
/**********************************************************************************/
/**********************************************************************************/

/* make lisp representation of target info from port and location info */
static LVAL makeaetarget(PortInfoRec *pir, LocationNameRec *lnr)
{
  LVAL nval, lval, val;
  char type[5];
    
  xlstkcheck(3);
  xlsave(nval);
  xlsave(lval);
  xlsave(val);
    
  lval = nval = NIL;
  switch (pir->name.portKindSelector) {
  case ppcByCreatorAndType:
    type[4] = 0;
    MEMCPY(type, (char *) &(pir->name.u.port.portType), 4);
    nval = consa(cvstring(type));
    MEMCPY(type, (char *) &(pir->name.u.port.portCreator), 4);
    nval = cons(cvstring(type), nval);
    break;
  case ppcByString:
    PtoCstr(pir->name.u.portTypeStr);
    nval = cvstring((char *) pir->name.u.portTypeStr);
    CtoPstr((char *) pir->name.u.portTypeStr);
    break;
  }
  nval = consa(nval);
  PtoCstr(pir->name.name);
  nval = cons(cvstring((char *) pir->name.name), nval);
  CtoPstr((char *) pir->name.name);
  nval = cons(cvfixnum((FIXTYPE) pir->name.nameScript), nval);
  nval = consa(nval);
  nval = cons(pir->authRequired ? s_true : NIL, nval);

  switch (lnr->locationKindSelector) {
  case ppcNoLocation:
    lval = s_true;
    break;
  case ppcNBPLocation:
    PtoCstr(lnr->u.nbpEntity.zoneStr);
    lval = consa(cvstring((char *) lnr->u.nbpEntity.zoneStr));
    CtoPstr((char *) lnr->u.nbpEntity.zoneStr);
    PtoCstr(lnr->u.nbpEntity.typeStr);
    lval = cons(cvstring((char *) lnr->u.nbpEntity.typeStr), lval);
    CtoPstr((char *) lnr->u.nbpEntity.typeStr);
    PtoCstr(lnr->u.nbpEntity.objStr);
    lval = cons(cvstring((char *) lnr->u.nbpEntity.objStr), lval);
    CtoPstr((char *) lnr->u.nbpEntity.objStr);
    break;
  case ppcNBPTypeLocation:
    PtoCstr(lnr->u.nbpType);
    lval = cvstring((char *) lnr->u.nbpType);
    CtoPstr((char *) lnr->u.nbpType);
    break;
  }
    
  val = cons(nval, consa(lval));
  xlpopn(3);
  return val;
}

/* fill in target ID from lisp target info */
static OSErr getaetarget(LVAL data, TargetID *target)
{
  LVAL nval, lval;
  
  if (consp(data) && consp(cdr(data)) && null(cdr(cdr(data)))) {
    nval = car(data);
    lval = car(cdr(data));
    
    /* handle the port info part */
    if (consp(nval) && consp(cdr(nval)) && null(cdr(cdr(nval)))) {

      /* get the name structure */
      nval = car(cdr(nval));
        
      /* pull out the script type */
      if (consp(nval) && fixp(car(nval))) {
        target->name.nameScript = getfixnum(car(nval));
        nval = cdr(nval);
      }
      else
        return procNotFound;
       
      /* pull out the name string */
      if (consp(nval) && stringp(car(nval))) {
        C2PSTRING(getstring(car(nval)), target->name.name);
        nval = cdr(nval);
      }
      else
        return procNotFound;
        
      /* handle the port kind selector */
      if (consp(nval) && null(cdr(nval))) {
        nval = car(nval);
        if (stringp(nval)) {
          target->name.portKindSelector = ppcByString;
          C2PSTRING(getstring(nval), target->name.u.portTypeStr);
        }
        else if (consp(nval) && consp(cdr(nval)) && null(cdr(cdr(nval)))) {
          target->name.portKindSelector = ppcByCreatorAndType;
          if (stringp(car(nval)) && stringp(car(cdr(nval)))) {
            target->name.u.port.portCreator = string_to_type(getstring(car(nval)));
            target->name.u.port.portType = string_to_type(getstring(car(cdr(nval))));
          }
          else procNotFound;
        }
        else
          return procNotFound;
       }
      else
        return procNotFound;
    }
    else
      return procNotFound;
    
    /* handle the location specifier */
    if (lval == s_true) {
      target->location.locationKindSelector = ppcNoLocation;
      return noErr;
    }
    else if (stringp(lval)) {
      target->location.locationKindSelector = ppcNBPTypeLocation;
      C2PSTRING(getstring(lval), target->location.u.nbpType);
      return noErr;
    }
    else {
      if (consp(lval) && stringp(car(lval))) {
        target->location.locationKindSelector = ppcNBPLocation;
        C2PSTRING(getstring(car(lval)), target->location.u.nbpEntity.objStr);
        lval = cdr(lval);
        if (consp(lval) && stringp(car(lval))) {
          C2PSTRING(getstring(car(lval)), target->location.u.nbpEntity.typeStr);
          lval = cdr(lval);
          if (consp(lval) && stringp(car(lval))) {
            C2PSTRING(getstring(car(lval)), target->location.u.nbpEntity.zoneStr);
            return noErr;
          }
          else
            return procNotFound;
        }
        else
          return procNotFound;
      }
      else
        return procNotFound;
    }
  }
  else
    return procNotFound;
}

/* return list of target info's for specified machine, type, and zone */
LVAL xgettargetlist(void)
{
  IPCListPortsPBRec ipcrec;
  PPCPortRec ppcrec;
  LocationNameRec locrec;
  PortInfoRec pinfo[1];
  LVAL tmp, val;
  char *object = nil, *type = "PPCToolBox", *zone = "*";
  OSErr err;
  
  if (! hasAppleEvents)
    return NIL;
    
  if (xlgetkeyarg(k_object, &tmp) && stringp(tmp))
    object = getstring(tmp);
  if (xlgetkeyarg(k_type, &tmp) && stringp(tmp))
    type = getstring(tmp);
  if (xlgetkeyarg(k_zone, &tmp) && stringp(tmp))
    zone = getstring(tmp);
  xllastkey();
  
  ppcrec.nameScript = smRoman;
  C2PSTRING("=", ppcrec.name);
  ppcrec.portKindSelector = ppcByString;
  C2PSTRING("=", ppcrec.u.portTypeStr);
  
  if (object == nil)
    locrec.locationKindSelector = ppcNoLocation;
  else {
    locrec.locationKindSelector = ppcNBPLocation;
    C2PSTRING(object, locrec.u.nbpEntity.objStr);
    C2PSTRING(type, locrec.u.nbpEntity.typeStr);
    C2PSTRING(zone, locrec.u.nbpEntity.zoneStr);
  }
  ipcrec.ioCompletion = nil;
  ipcrec.startIndex = 0;
  ipcrec.requestCount = 1;
  ipcrec.portName = &ppcrec;
  ipcrec.locationName = &locrec;
  ipcrec.bufferPtr = pinfo;
  
  xlsave1(val);
  val = NIL;
  while ((err = IPCListPortsSync(&ipcrec)) == noErr && ipcrec.actualCount == 1) {
    val = cons(makeaetarget(pinfo, &locrec), val);
    ipcrec.startIndex++;
  }
  xlpop()
  
  return nreverse(val);
}

/* return first target of specified name or signature */
LVAL xgettarget(void)
{
  LVAL arg, list, info, val;
  char *name;
  OSType type;
  
  if (! hasAppleEvents)
    return NIL;
    
  arg = xlgetarg();
  if (stringp(arg))
    name = getstring(arg);
  else if (fixp(arg)) {
    name = nil;
    type = getfixnum(arg);
  }
  else
    xlbadtype(arg);
    
  xlstkcheck(2)
  xlsave(list);
  xlsave(info);
  for (list = xgettargetlist(), val = NIL; consp(list); list = cdr(list)) {
    info = xlcallsubr1(xcadar, car(list));
    if (name != nil) {
      info = xlcallsubr1(xcadr, info);
      if (stringp(info) && strcmp(name, getstring(info)) == 0) {
        val = car(list);
        break;
      }
    }
    else {
      info = xlcallsubr1(xcaddr, info);
      if (consp(info) && fixp(car(info)) && type == getfixnum(info)) {
        val = car(list);
        break;
      }
      else if (stringp(info) && getslength(info) == 8 &&
               type == string_to_type(getstring(info))) {
        val = car(list);
        break;
      }
    }
  }
  xlpopn(2);
  
  return val;
}
  
static OSType browsetype;
static char *browsename;

/* filter function to restrict browser to specified name or signature */
static pascal Boolean browsefilter(LocationNameRec *lnr, PortInfoRec *pir)
{
  if (browsename != nil) {
    char *s = (char *) pir->name.name;
    int n = *((unsigned char *) s);
    
    MEMCPY(buf, s + 1, n);
    buf[n] = 0;
    return (strcmp(buf, browsename) == 0) ? TRUE : FALSE;
  }
  else if (browsetype != 0) {
    OSType signature;
    
    if (pir->name.portKindSelector == ppcByCreatorAndType)
      signature = pir->name.u.port.portCreator;
    else {
      /* AppleEvent Manager uses "XYZWepNN" with 'XYZW' the creator and NN a number */
      char *s;
      s = (char *) pir->name.u.portTypeStr;
      if (*s != 8)
        return FALSE;
      MEMCPY((char *) &signature, s + 1, 4);
    }
    return (signature == browsetype) ? TRUE : FALSE;
  }
  else
    return TRUE;
}

static PPCFilterUPP browsefilterUPP = NULL;

/* use PPCBrowser to get target info */
LVAL xbrowsetargets(void)
{
  PortInfoRec pir;
  LocationNameRec lnr;
  Str255 prompt, appstr, typestr;
  LVAL tmp;
  
  if (! hasAppleEvents)
    return NIL;
    
  if (xlgetkeyarg(k_prompt, &tmp) && stringp(tmp))
    C2PSTRING(getstring(tmp), prompt);
  else
    C2PSTRING("", prompt);
  if (xlgetkeyarg(k_appllistlabel, &tmp) && stringp(tmp))
    C2PSTRING(getstring(tmp), appstr);
  else
    C2PSTRING("", appstr);
  if (xlgetkeyarg(k_type, &tmp) && stringp(tmp))
    C2PSTRING(getstring(tmp), typestr);
  else
    C2PSTRING("", typestr);
  if (xlgetkeyarg(k_name, &tmp) && stringp(tmp))
    browsename = getstring(tmp);
  else
    browsename = nil;
  if (xlgetkeyarg(k_signature, &tmp) && (stringp(tmp) || fixp(tmp)))
    browsetype = (fixp(tmp)) ? getfixnum(tmp) : string_to_type(getstring(tmp));
  else
    browsetype = 0;
  xllastkey();
  
  NotifyIfInBackground();
  if (! browsefilterUPP)
    browsefilterUPP = NewPPCFilterProc((ProcPtr) browsefilterUPP);
  if (PPCBrowser(prompt, appstr, FALSE, &lnr, &pir, browsefilterUPP, typestr) != noErr)
    return NIL;
  else
    return makeaetarget(&pir, &lnr);
}

static AEIdleUPP aeIdleUPP = NULL;

/* send an apple event */
LVAL xsendappleevent(void)
{
  AEAddressDesc addr;
  TargetID target;
  AppleEvent event, reply;
  OSType class, etype, sign, type;
  OSErr err;
  long eventerr, size, timeout;
  LVAL addrarg, data, val, tmp;
  AESendMode mode;
  ProcessSerialNumber psn;
  int old_status, new_status, wait_reply;

  if (! hasAppleEvents)
    return NIL;
    
  /* get the event class and type */
  tmp = xlgetarg();
  if (fixp(tmp))
    class = getfixnum(tmp);
  else if (stringp(tmp))
    class = string_to_type(getstring(tmp));
  else
    xlbadtype(tmp);
  tmp = xlgetarg();
  if (fixp(tmp))
    etype = getfixnum(tmp);
  else if (stringp(tmp))
    etype = string_to_type(getstring(tmp));
  else
    xlbadtype(tmp);
  
  /* get the address specifier */
  addrarg = xlgetarg();
    
  /* get the :DATA keyword argument */
  if (! xlgetkeyarg(k_data, &data))
    data = NIL;
    
  /* get the :WAIT-REPLY keyword argument */
  if (xlgetkeyarg(k_waitreply, &tmp) && null(tmp)) {
    wait_reply = FALSE;
    mode = kAENoReply;
  }
  else {
    wait_reply = TRUE;
    mode = kAEWaitReply;
  }

  /* get the :TIMEOUT keyword argument */
  if (xlgetkeyarg(k_timeout, &tmp)) {
    if (tmp == s_true)
      timeout = kAEDefaultTimeout;
    else if (fixp(tmp) && getfixnum(tmp) >= 0)
      timeout = getfixnum(tmp);
    else if (null(tmp))
      timeout = kNoTimeOut;
    else
      xlbadtype(tmp);
  }
  else if (wait_reply)
    timeout = kAEDefaultTimeout;
  else
    timeout = kNoTimeOut;

  /* get the :CAN-SWITCH-LAYER keyword argument */
  if (xlgetkeyarg(k_canswitch, &tmp) && ! null(tmp))
    mode += kAECanSwitchLayer;
    
  xllastkey();
  
  if (fixp(addrarg)) {
    sign = (OSType) getfixnum(addrarg);
    err = AECreateDesc(typeApplSignature, (Ptr) &sign, sizeof(OSType), &addr);
  }
  else if (getpsn(addrarg, &psn) == noErr)
    err = AECreateDesc(typeProcessSerialNumber, (Ptr) &psn, sizeof(psn), &addr);
  else if (getaetarget(addrarg, &target) == noErr)
    err = AECreateDesc(typeTargetID, (Ptr) &target, sizeof(target), &addr);
  else
    xlbadtype(addrarg);    
  if (err != noErr)
    xlerror("can't create address", cvfixnum((FIXTYPE) err));

  err = AECreateAppleEvent(class, etype, &addr,
                           kAutoGenerateReturnID, kAnyTransactionID, &event);
  AEDisposeDesc(&addr);
  if (err != noErr)
    xlerror("can't create event record", cvfixnum((FIXTYPE) err));
  
  if (stringp(data)) {
    if ((err = AEPutParamPtr(&event, keyDirectObject, typeChar,
                             getstring(data), getslength(data))) != noErr) {
      AEDisposeDesc(&event);
      xlerror("can't add parameter", cvfixnum((FIXTYPE) err));
    }
  }
  else if (consp(data)) {
    LVAL next, list, key, item, type;
    for (next = data; consp(next); next = cdr(next)) {
      list = car(next);
      if (! (consp(list) && consp(cdr(list)))) {
        AEDisposeDesc(&event);
        xlbadtype(data);
      }
      key = car(list);
      item = car(cdr(list));
      type = consp(cdr(cdr(list))) ? car(cdr(cdr(list))) : NIL;
      if (! stringp(key)) {
        AEDisposeDesc(&event);
        xlbadtype(data);
      }
      switch (ntype(item)) {
      case STRING:
        if ((err = AEPutParamPtr(&event, string_to_type(getstring(key)), typeChar,
                                 getstring(item), getslength(item))) != noErr) {
          AEDisposeDesc(&event);
          xlerror("can't add parameter", cvfixnum((FIXTYPE) err));
        }
        break;
      case FIXNUM:
        if ((err = AEPutParamPtr(&event, string_to_type(getstring(key)), typeInteger,
                                 (Ptr) &getfixnum(item), sizeof(FIXTYPE))) != noErr) {
          AEDisposeDesc(&event);
          xlerror("can't add parameter", cvfixnum((FIXTYPE) err));
        }
        break;
      case FLONUM:
        if ((err = AEPutParamPtr(&event, string_to_type(getstring(key)), typeFloat,
                                 (Ptr) &getflonum(item), sizeof(FLOTYPE))) != noErr) {
          AEDisposeDesc(&event);
          xlerror("can't add parameter", cvfixnum((FIXTYPE) err));
        }
        break;
      default:
        AEDisposeDesc(&event);
        xlbadtype(data);
      }
    }
  }
  else if (!null(data)) {
    AEDisposeDesc(&event);
    xlbadtype(data);
  }
  
  reply.descriptorType = typeNull;
  reply.dataHandle = nil;

  old_status = apple_event_status;
  apple_event_status = AE_OK;
  
  if (! aeIdleUPP)
    aeIdleUPP = NewAEIdleProc((ProcPtr) AEidle);
  err = AESend(&event, &reply, mode, kAENormalPriority, timeout, aeIdleUPP, nil);
  AEDisposeDesc(&event);

  new_status = apple_event_status;
  apple_event_status = old_status;

  if (wait_reply) {
    if (err != noErr) {
      AEDisposeDesc(&reply);
      xlerror("send failed", cvfixnum((FIXTYPE) err));
    }
    if (AEGetParamPtr(&reply, keyErrorNumber, typeLongInteger, &type,
                      (Ptr) &eventerr, sizeof(long), &size) != errAEDescNotFound &&
        eventerr != noErr) {
      AEDisposeDesc(&reply);
      xlerror("error code received", cvfixnum((FIXTYPE) eventerr));
    }
    if (new_status != AE_OK) {
      AEDisposeDesc(&reply);
      switch(new_status) {
      case AE_ERROR:  xlfail("error handling apple event"); break;
      case AE_UNWIND: xljump(xltarget, xlmask, xlvalue);    break;
      }
    }
    
    if (AESizeOfParam(&reply, keyDirectObject, &type, &size) != errAEDescNotFound) {
      val = newstring(size);
      if ((err = AEGetParamPtr(&reply, keyDirectObject, typeChar, &type,
                              getstring(val), size, &size)) != noErr) {
        AEDisposeDesc(&reply);
        xlerror("can't extract result", cvfixnum((FIXTYPE) err));
      }
    }
    else
      val = NIL;
  }
  else
    val = NIL;
  
  AEDisposeDesc(&reply);
  return val;
}

/* handler for Open Application Apple Event */
static pascal OSErr handleOAPP(AppleEvent *evt, AppleEvent *reply, long ref)
{
  return RequiredCheck(evt);
}

/* handler for Open Documents Apple Event */
static pascal OSErr handleODOC(AppleEvent *evt, AppleEvent *reply, long ref)
{
  OSErr err;
  AEDescList dl;
  FSSpec fsSpec;
  AEKeyword key;
  DescType type;
  FInfo fInfo;
  Size size;
  long n, i;
  
  if ((err = AEGetParamDesc(evt, keyDirectObject, typeAEList, &dl)) != noErr)
    return err;

  if ((err = RequiredCheck(evt)) != noErr)
    return err;
    
  if ((err = AECountItems(&dl, &n)) != noErr)
    return err;
    
  num_app_files = n;
  app_files = (FileSpec *) CALLOC(num_app_files, sizeof(FileSpec));          

  for (i = 0; i < num_app_files; i++) {
    if ((err = AEGetNthPtr(&dl, i + 1, typeFSS, &key, &type,
                             (Ptr) &fsSpec, sizeof(FSSpec), &size)) != noErr) {
      num_app_files = i;
      AEDisposeDesc(&dl);
      return err;
    }

    app_files[i].dir.vRefNum = fsSpec.vRefNum;
    app_files[i].dir.parID = fsSpec.parID;
    FSpGetFInfo(&fsSpec, &fInfo);
    app_files[i].type = fInfo.fdType;
    PtoCstr((StringPtr) fsSpec.name);
    strcpy(app_files[i].name, (char *) fsSpec.name);
  }
  
  AEDisposeDesc(&dl);
  return noErr;
}

/* handler for Print Documents Apple Event */
static pascal OSErr handlePDOC(AppleEvent *evt, AppleEvent *reply, long ref)
{
  return errAEEventNotHandled;
}

/* handler for Quit Application Apple Event */
static pascal OSErr handleQUIT(AppleEvent *evt, AppleEvent *reply, long ref)
{
  OSErr err;
  
  if ((err = RequiredCheck(evt)) != noErr)
    return err;
  else {
    quit_event_received = TRUE;
    return noErr;
  }
}

/* handler for Do Script Apple Event */
static pascal OSErr handleDOSC(AppleEvent *evt, AppleEvent *reply, long ref)
{
  OSErr err;
  AEDesc argDesc;
  DescType type;
  long size;
  LVAL arg, olddenv;
  CONTEXT ucntxt, ecntxt;
  
  if (! initialAEdone)
    return errAEEventNotHandled;
  
  /**** does this work if param needs to be coerced to string? */
  if ((err = AEGetParamDesc(evt, keyDirectObject, typeChar, &argDesc)) != noErr)
    return noErr;
    
  if ((err = AESizeOfParam(evt, keyDirectObject, &type, &size)) != noErr) {
    AEDisposeDesc(&argDesc);
    return err;
  }
  
  if ((err = AEGetParamPtr(evt, keyDirectObject, type, &type, buf, size, &size)) !=
      noErr) {
    AEDisposeDesc(&argDesc);
    return err;
  }

  if ((err = RequiredCheck(evt)) != noErr) {
    AEDisposeDesc(&argDesc);
    return err;
  }

  /* Since it is possible for a GO to occur in a hander and the new method of  */
  /* passing GO targets uses the mask, two separate contexts seem to be needed */
  /* since it is no longer possible to tell reliably whether the jump is an    */
  /* error or a GO to the 8th tag (I think).                                   */
  xlbegin(&ucntxt, CF_UNWIND, NIL);
  if (XL_SETJMP(ucntxt.c_jmpbuf)) {
    apple_event_status = AE_UNWIND;
    /**** make a better reply here */
    err = errAEEventNotHandled;
  }
  else {
    xlbegin(&ecntxt, CF_ERROR, s_true);
    if (XL_SETJMP(ecntxt.c_jmpbuf)) {
      apple_event_status = AE_ERROR;
      err = errAEEventNotHandled;
    }
    else {
      olddenv = xldenv;
      xldbind(s_breakenable, NIL);
      xlsave1(arg);
      arg = newstring(size);
      MEMCPY(getstring(arg), (char *) *argDesc.dataHandle, size);

      arg = xlcallsubr1(xmkstrinput, arg);
      arg = xlcallsubr1(xread, arg);
      arg = xlcallsubr1(xeval, arg);
  
      if (reply->dataHandle != nil) {
        LVAL val;
    
        xlsave1(val);
        val = newustream();
        xlcallsubr2(xprin1, arg, val);
        val = xlcallsubr1(xgetstroutput, val);
        err = AEPutParamPtr(reply, keyDirectObject, typeChar,
                            getstring(val), getslength(val));
        xlpop();
      }
      xlpop();
      xlunbind(olddenv);
    }
    xlend(&ecntxt);
  }
  xlend(&ucntxt);
  
  AEDisposeDesc(&argDesc);
  return err;
}

/* apple event handler for TransSkel */
static pascal void handleAE(EventRecord *evt)
{
  int old_status, new_status;
  
  old_status = apple_event_status;
  apple_event_status = AE_OK;
  
  AEProcessAppleEvent(evt);
  if (quit_event_received)
    xexit();
    
  new_status = apple_event_status;
  apple_event_status = old_status;
  
  switch(new_status) {
  case AE_ERROR:  xlfail("error handling apple event"); break;
  case AE_UNWIND: xljump(xltarget, xlmask, xlvalue);    break;
  }
  
  if (num_app_files > 0)
    macloadinits();
}

/* idle function for AESend */
static pascal Boolean AEidle(EventRecord *evt, long *sleep, RgnHandle *mrgn)
{
  EventRecord testevt;
  
  if (GetOSEvent(keyDownMask + autoKeyMask, &testevt) && SkelCmdPeriod(&testevt))
    return TRUE;

  SkelRouteEvent(evt);
  *mrgn = nil;
  *sleep = GetDblTime();
  return FALSE;
}

/* check that all required apple event parameters have been obtained */
static OSErr RequiredCheck(AppleEvent *evt)
{
  OSErr err;
  DescType type;
  Size size;
  
  err = AEGetAttributePtr(evt, keyMissedKeywordAttr, typeWildCard,
                          &type, nil, 0, &size);
  if (err == errAEDescNotFound)
    return noErr;
  else if (err == noErr)
    return errAEEventNotHandled;
  else
    return err;
}

LVAL xppcstartup(void)
{
#ifdef DODO /**** can this be made to work? */
  PPCOpenPBRec openrec;
  PPCStartPBRec startrec;
  TargetID target;
  int needperm;
  LVAL arg;
  OSErr err;
  
  arg = xlgetarg();
  xllastarg();
  
  if (getaetarget(arg, &target) != noErr)
    xlbadtype(arg);
  
  needperm = (consp(arg) && consp(car(arg)) && null(car(car(arg)))) ? FALSE : TRUE;
  
  if (GetDefaultUser(&startrec.userRefNum, (StringPtr) buf) != noErr)
    startrec.userRefNum = 0;
  if (needperm && startrec.userRefNum == 0)
    xlfail("authorization required");
  
  openrec.serviceType = ppcServiceRealTime;
  openrec.resFlag = 0;
  openrec.portName = &target.name;
  openrec.locationName = &target.location;
  openrec.networkVisible = TRUE;
  
  if ((err = PPCOpen(&openrec, FALSE)) != noErr)
    xlerror("can't open port", cvfixnum((FIXTYPE) err));
  
  startrec.ioCompletion = nil;
  startrec.portRefNum = openrec.portRefNum;
  startrec.serviceType = ppcServiceRealTime;
  startrec.resFlag = 0;
  startrec.portName = &target.name;
  startrec.locationName = &target.location;;
  startrec.userData = 0;
  
  if ((err = PPCStart(&startrec, FALSE)) != noErr)
    xlerror("can't start session", cvfixnum((FIXTYPE) err));
  else
    return consa(cvfixnum((FIXTYPE) startrec.sessRefNum));
#else
  return NIL;
#endif /* DODO */
}
