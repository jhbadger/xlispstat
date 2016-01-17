#include "xlshlib.h"
#include <limits.h>
#include "sock.h"

#ifndef OPEN_MAX
#  define OPEN_MAX 64
#endif

static int sock[OPEN_MAX];
static int sock_inited = FALSE;

#define SOCK_MAX OPEN_MAX

static void cleanup(void)
{
  int i;
  for (i = 0; i < SOCK_MAX; i++)
    if (sock[i] != -1) {
      Sock_close(sock[i], NULL);
      sock[i] = -1;
    }
}

static LVAL enter_sock(int fd)
{
  if (fd == -1)
    return NIL;
  else {
    int i;
    for (i = 0; i < SOCK_MAX; i++)
      if (sock[i] == -1) {
        sock[i] = fd;
        return cvfixnum((FIXTYPE) fd);
      }
    Sock_close(fd, NULL);
    return NIL;
  }
}

static LVAL close_sock(int fd)
{
  int i;
  for (i = 0; i < SOCK_MAX; i++)
    if (sock[i] == fd) {
      sock[i] = -1;
      return Sock_close(fd, NULL) == -1 ? NIL : s_true;
    }
  return NIL;
}

static void check_init(void)
{
  if (! sock_inited) {
    int i;
    for (i = 0; i < SOCK_MAX; i++)
      sock[i] = -1;
    Sock_init();
    sock_inited = TRUE;
    atexit(cleanup);
  }
}

LVAL xsockopen()
{
  int port = getfixnum(xlgafixnum());
  xllastarg();
  check_init();
  return enter_sock(Sock_open(port, NULL));
}

LVAL xsocklisten()
{
  int sock = getfixnum(xlgafixnum());
  xllastarg();
  check_init();
  return enter_sock(Sock_listen(sock, NULL, 0, NULL));
}

LVAL xsockconnect()
{
  int port = getfixnum(xlgafixnum());
  char *serv = getstring(xlgastring());
  xllastarg();
  check_init();
  return enter_sock(Sock_connect(port, serv, NULL));
}

LVAL xsockclose()
{
  int sock = getfixnum(xlgafixnum());
  xllastarg();
  return close_sock(sock);
}

LVAL xsockread()
{
  ssize_t n;
  int port = getfixnum(xlgafixnum());
  LVAL buf = xlgastring();
  xllastarg();
  check_init();
  n = Sock_read(port, getstring(buf), getslength(buf), NULL);
  return n == -1 ? NIL : cvfixnum((FIXTYPE) n);
}

LVAL xsockwrite()
{
  ssize_t n;
  int port, start, end, len;
  LVAL buf;
  port = getfixnum(xlgafixnum());
  buf = xlgastring();
  start = getfixnum(xlgafixnum());
  end = getfixnum(xlgafixnum());
  xllastarg();
  check_init();
  len = getslength(buf);
  if (end > len)
    end = len;
  if (start < 0)
    start = 0;
  if (end < start)
    return NIL;
  n = Sock_write(port, getstring(buf) + start, end - start, NULL);
  return n == -1 ? NIL : cvfixnum((FIXTYPE) n);
}

#ifdef UNIX
/* Under X11 after a fork() the next call to XSync() hangs. I'll try
   to figure this out but for now if you want to use fork you can't
   use graphics -- just undefine DISPLAY before tarting xlisp.  I'm
   sure it isn't the fork as such but rather something like the
   attempt of both processes to wait on the display that is the
   problem. But I don't know exactly what it is. */
#ifdef XLISP_STAT
#include "xlgraph.h"
#endif /* XLISP_STAT */
#include <signal.h>
#include <sys/wait.h>
static void sig_child(int sig)
{
  int stat;
  while (waitpid(-1, &stat, WNOHANG) > 0);
}

static int sig_fork_inited = FALSE;

LVAL xsockfork()
{
  pid_t pid;
  xllastarg();
#ifdef XLISP_STAT
  if (StHasWindows())
    xlfail("can't fork under X11 (at least for now)");
#endif /* XLISP_STAT */
  if (! sig_fork_inited) {
    struct sigaction sa;
    sa.sa_handler = sig_child;
    sa.sa_flags = 0;
    sigaction(SIGCHLD, &sa, NULL);
    sig_fork_inited = TRUE;
  }
  pid = fork();
  return pid == -1 ? NIL : cvfixnum((FIXTYPE) pid);
}
#endif /* UNIX */

static FUNDEF myfuns[] = {
  { "SOCKETS::SOCK-OPEN", SUBR, xsockopen },
  { "SOCKETS::SOCK-LISTEN", SUBR, xsocklisten },
  { "SOCKETS::SOCK-CONNECT", SUBR, xsockconnect },
  { "SOCKETS::SOCK-CLOSE", SUBR, xsockclose },
  { "SOCKETS::BASE-SOCK-READ", SUBR, xsockread },
  { "SOCKETS::BASE-SOCK-WRITE", SUBR, xsockwrite },
#ifdef UNIX
  { "SOCKETS::FORK", SUBR, xsockfork },
#endif
  { NULL, 0, NULL }
};

static xlshlib_modinfo_t myinfo = {
  XLSHLIB_VERSION_INFO(0,1,0,1),
  myfuns,
  NULL,
  NULL,
  NULL
};

xlshlib_modinfo_t *xlsock__init() { return &myinfo; }
