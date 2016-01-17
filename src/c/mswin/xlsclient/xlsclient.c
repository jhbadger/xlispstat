/* xlsclient.c -- DDE command line client for XLISP-STAT. */
/* Copyright (c) 1999, by Luke Tierney.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

/* The thread-based IO design is adapted from Rterm and Tcl. */

#include <windows.h>
#include <ddeml.h>
#include <stdio.h>
#include <setjmp.h>

#define ErrorPrint(str) fprintf(stderr, "Error: %s\n", str)
#define FatalError(str) do { ErrorPrint(str); exit(1); } while (0)

#define SERVICE_NAME "XLISP-STAT"
#define TOPIC_NAME "CMDLINE"

#define BUFFER_SIZE 10000
char buf[BUFFER_SIZE];
char line[BUFFER_SIZE];
size_t bufpos;
int mych;

HANDLE ready_for_input = NULL;
HANDLE input_available = NULL;
BOOL interrupted = FALSE;
BOOL executing = FALSE;
BOOL done = FALSE;
DWORD mainThreadID;

jmp_buf read_reset;
#define RESET_INTERRUPT 1
#define RESET_OVERFLOW 2

int mygetch(void)
{
  int c;
  static size_t linesize = 0;
  static size_t linepos = 0;
  static BOOL inited = FALSE;
  MSG msg;

  if (done) return EOF;

  if (linepos >= linesize) {
    if (! inited) {
      SetEvent(ready_for_input);
      inited = TRUE;
    }
    while (GetMessage(&msg, NULL, 0, 0)) {
      if (WaitForSingleObject(input_available, 0) == WAIT_OBJECT_0)
        break;
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
    if (interrupted) {
      interrupted = FALSE;
      longjmp(read_reset, RESET_INTERRUPT);
    }
    else {
      linesize = strlen(line);
      linepos = 0;
      SetEvent(ready_for_input);
    }
  }
  return line[linepos++];
}

void buffer_putc(int c)
{
  if (bufpos < BUFFER_SIZE)
    buf[bufpos++] = c;
  else longjmp(read_reset, RESET_OVERFLOW);
}

BOOL eat_single_comment(void)
{
  int c;
  while (TRUE) {
    c = mygetch();
    if (c == EOF) return FALSE;
    buffer_putc(c);
    if (c == '\n') return TRUE;
  }
}

BOOL eat_multi_comment(void)
{
  int c;
  while (TRUE) {
    c = mygetch();
    if (c == EOF) return FALSE;
    buffer_putc(c);
    switch (c) {
    case '|':
      c = mygetch();
      if (c == EOF) return FALSE;
      buffer_putc(c);
      if (c == '#') return TRUE;
      break;
    case '#':
      c = mygetch();
      if (c == EOF) return FALSE;
      buffer_putc(c);
      if (c == '|' && ! eat_multi_comment()) return FALSE;
      break;
    }
  }
}

BOOL read_expression(void)
{
  int c, parcount;

  switch (setjmp(read_reset)) { /**** is this legit? */
  case RESET_INTERRUPT: ErrorPrint("user interrupt"); break;
  case RESET_OVERFLOW: ErrorPrint("buffer overflow"); break;
  }

  bufpos = 0;
  parcount = 0;
  while (TRUE) {
    c = mygetch();
    if (c == EOF) goto eof;
    buffer_putc(c);
    switch (c) {
    case '\n': if (parcount <= 0) goto complete; break;
    case '(': parcount++; break;
    case ')': parcount--; if (parcount <= 0) goto complete; break;
    case ';': if (! eat_single_comment()) goto eof; break;
    case '#':
      c = mygetch();
      if (c == EOF) goto eof;
      buffer_putc(c);
      switch (c) {
      case '(': parcount++; break;
      case '|': if (! eat_multi_comment()) goto eof; break;
      case '\\':
        c = mygetch();
        if (c == EOF) goto eof;
        buffer_putc(c);
        break;
      }
      break;
    }
  }
  complete:
    buffer_putc('\0');
    return TRUE;
  eof:
    buffer_putc('\0');
    return FALSE;
}

BOOL WINAPI signal_handler(DWORD type)
{
  switch (type) {
  case CTRL_C_EVENT:
  case CTRL_BREAK_EVENT:
    if (executing)
      fprintf(stderr,
              "**** XLISP-STAT is executing a command.\n"
              "**** To interrupt it, do a BREAK in the XLISP-STAT window\n");
    else {
      interrupted = TRUE;
      SetEvent(input_available);
      PostThreadMessage(mainThreadID, WM_NULL, 0, 0);
    }
    break;
  }
  return TRUE;
}

DWORD WINAPI readline(LPVOID dummy)
#pragma argsused
{
  do {
    WaitForSingleObject(ready_for_input, INFINITE);
    done = fgets(line, sizeof(line), stdin) == NULL;
    SetEvent(input_available);
    PostThreadMessage(mainThreadID, WM_NULL, 0, 0);
  } while (! done);
  return FALSE;
}

HDDEDATA CALLBACK DdeProc(UINT type, UINT fmt, HCONV hconv,
                          HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
                          DWORD dw1, DWORD dw2)
#pragma argsused
{
  return (HDDEDATA) 0;
}

/**** I can only tell if I am executing if I do the parsing here. */
HDDEDATA DdeExecute(char *buf, HCONV hconv, DWORD timeout)
{
  HDDEDATA hdata;
  executing = TRUE;
  hdata = DdeClientTransaction((LPVOID) (buf), strlen(buf) + 1, hconv, NULL,
                               CF_TEXT, XTYP_EXECUTE, timeout, NULL);
  executing = FALSE;
  return hdata;
}

void DdeRequestAndWrite(HCONV hconv, HSZ value, DWORD timeout)
{
  HDDEDATA hdata;
  hdata = DdeClientTransaction(NULL, 0, hconv, value, CF_TEXT,
                               XTYP_REQUEST, timeout, NULL);
  if (hdata != NULL) {
    DWORD size;
    char *p = (char *) DdeAccessData(hdata, &size);
    if (p != NULL && memchr(p, 0, size) != NULL) {
      fputs(p, stdout);
      fflush(stdout);
    }
    else ErrorPrint("bad value string");
    DdeUnaccessData(hdata);
    DdeFreeDataHandle(hdata);
  }
  else ErrorPrint("value request failed");
}

int main(int argc, char *argv[])
{
  DWORD ddeInst = 0;
  HSZ service, topic, value;
  char *servicename;
  char *topicname;
  DWORD read_thread_id;
  HANDLE read_thread = NULL;
  HSZ hszbreak;
  HCONV hconv;
  DWORD timeout = 60000L;

  mainThreadID = GetCurrentThreadId();

  /* get the service and topic names */
  switch (argc) {
  case 1: topicname = TOPIC_NAME; servicename = SERVICE_NAME; break;
  case 2: topicname = TOPIC_NAME; servicename = argv[1]; break;
  case 3: servicename = argv[1]; topicname = argv[2]; break;
  default: fprintf(stderr, "usage: %s [service [topic]]\n", argv[0]); exit(1);
  }

  /* initialize DDEML */
  switch (DdeInitialize(&ddeInst,(PFNCALLBACK)DdeProc,APPCMD_CLIENTONLY,0)) {
  case DMLERR_NO_ERROR: break;
  case DMLERR_DLL_USAGE: FatalError("DLL usage");
  case DMLERR_INVALIDPARAMETER: FatalError("invalid parameter");
  case DMLERR_SYS_ERROR: FatalError("invalid parameter");
  default: FatalError("unknown error");
  }
  if (ddeInst == 0) FatalError("DDE initialization failed");

  /* initialize the synchronization events and start the reader thread */
  if (SetConsoleCtrlHandler(signal_handler, TRUE) == FALSE ||
      (ready_for_input = CreateEvent(NULL, FALSE, FALSE, NULL)) == NULL ||
      (input_available = CreateEvent(NULL, FALSE, FALSE, NULL)) == NULL ||
      (read_thread = CreateThread(NULL, 0, readline, NULL,
                                  0, &read_thread_id)) == NULL)
    goto cleanup;

  /* connect to the server */
  service = DdeCreateStringHandle(ddeInst, servicename, CP_WINANSI);
  topic = DdeCreateStringHandle(ddeInst, topicname, CP_WINANSI);
  hconv = DdeConnect(ddeInst, service, topic, NULL);
  if (service != NULL) DdeFreeStringHandle(ddeInst, service);
  if (topic != NULL) DdeFreeStringHandle(ddeInst, topic);
  if (hconv == NULL) FatalError("can't connect to the XLISP-STAT DDE server");

  /* initialize the string handles for the value and break request */
  value = DdeCreateStringHandle(ddeInst, "VALUE", CP_WINANSI);
  hszbreak = DdeCreateStringHandle(ddeInst, "BREAK", CP_WINANSI);

  /* process standard input until it is empty */
  DdeRequestAndWrite(hconv, value, timeout);
  while (read_expression()) {
    if (DdeExecute(buf, hconv, timeout))
      DdeRequestAndWrite(hconv, value, timeout);
    else ErrorPrint("execute failed");
  }

 cleanup:
  /* clean up the reader thread and synchronization events */
  if (read_thread != NULL) CloseHandle(read_thread);
  if (ready_for_input != NULL) CloseHandle(ready_for_input);
  if (input_available != NULL) CloseHandle(input_available);

  /* close the connection and drop DDEML */
  DdeDisconnect(hconv);
  if (value != NULL) DdeFreeStringHandle(ddeInst, value);
  if (hszbreak != NULL) DdeFreeStringHandle(ddeInst, hszbreak);
  DdeUninitialize(ddeInst);
  return 0;
}
