#if defined(__MWERKS__) && defined(macintosh)
#  define MACINTOSH
#elif defined(_Windows)
   typedef long ssize_t
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#ifndef MACINTOSH
#  include <sys/types.h>
#endif
#include "sock.h"

#define BLKSIZE  1024

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
  Sock_port_t portnumber = 7;
  int sockfd;
  ssize_t bytesread, i;
  ssize_t byteswritten;
  char buf[BLKSIZE];
  char *hostname = "localhost";

  switch (argc) {
  case 3: portnumber = atoi(argv[2]);
  case 2: hostname = argv[1];
  case 1: break;
  default:
    fprintf(stderr, "Usage: %s host port\n", argv[0]);
    exit(1);
  }

  if (Sock_init() != 0) {
    fprintf(stderr, "Sock initialization failed");
    exit(1);
  }
 
  if ((sockfd = Sock_connect(portnumber, hostname, NULL)) < 0) {
    perror("Unable to establish an Internet connection");
    exit(1);
  }
  fprintf(stderr, "Connection has been made to %s\n", hostname);
 
  for ( ; ; ) {
    for (bytesread = 0; bytesread < BLKSIZE; bytesread++) {
      int ch = getc(stdin);
      if (ch == EOF)
        break;
      else if (ch == '\n') {
        buf[bytesread++] = ch;
        break;
      }
      else
        buf[bytesread] = ch;
    }
    if (bytesread <= 0) break;
    else {
      byteswritten = Sock_write(sockfd, buf, bytesread, NULL);
      if (byteswritten != bytesread) {
        fprintf(stderr,
                "Error writing %ld bytes, %ld bytes written\n",
                (long)bytesread, (long)byteswritten);
        break;
      }
    }
    bytesread = Sock_read(sockfd, buf, BLKSIZE, NULL);
    for (i = 0; i < bytesread; i++)
      putc(buf[i], stdout);
  }

  Sock_close(sockfd, NULL);
  exit(0);
}
