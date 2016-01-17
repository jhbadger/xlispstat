#if defined(__MWERKS__) && defined(macintosh)
#  define MACINTOSH
#elif defined(_Windows)
   typedef long ssize_t
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#ifndef MACINTOSH
#  include <sys/types.h>
#endif
#include <limits.h>
#include <stdarg.h>
#include "sock.h"

#define BLKSIZE  1024
#ifndef MAX_CANON
#  define MAX_CANON 128
#endif

static void serverr(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(1);
}

static void message(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fflush(stderr);
}

#ifdef __MWERKS__
#include <console.h>
void main(void)
{
  char **argv;
  int argc = ccommand(&argv);
#else
void main(int argc, char *argv[])
{
#endif
  Sock_port_t portnumber;
  int listenfd, communfd;
  char remote[MAX_CANON];
  char buf[BLKSIZE];
  ssize_t bytesread, byteswritten;
  int i;
       
  if (argc != 2)
    serverr("Usage: %s port\n", argv[0]);
  portnumber = (Sock_port_t) atoi(argv[1]);
       
  if (Sock_init() != 0)
    serverr("Sock initialization failed");

  if ((listenfd = Sock_open(portnumber, NULL)) < 0)
    serverr("Unable to establish a port connection");

  if ((communfd = Sock_listen(listenfd, remote, MAX_CANON, NULL)) < 0)
    serverr("Failure to listen on server");
  message("Connection has been made to %s\n", remote);

  while((bytesread = Sock_read(communfd, buf, BLKSIZE, NULL)) > 0) {
    byteswritten = Sock_write(communfd, buf, bytesread, NULL);
    if (byteswritten != bytesread) {
      Sock_close(communfd, NULL);
      Sock_close(listenfd, NULL);
      serverr("Error writing %ld bytes, %ld bytes written\n",
              (long) bytesread, (long) byteswritten);
    }
  }
  message("Connection closed by client\n");

  Sock_close(communfd, NULL);
  Sock_close(listenfd, NULL);
}
