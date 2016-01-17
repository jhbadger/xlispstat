#ifdef _Windows

#include <windows.h>
#include <windowsx.h>
#include <mem.h>

extern char *Lmalloc(unsigned long);
extern char *Lrealloc(char *, unsigned long);
extern void Lfree(char *);

char *NewSegment()
{
  HANDLE h;
  h = GlobalAlloc(GMEM_FIXED, 0x010000L);
  return ((h) ? GlobalLock(h) : 0);
}

void FreeSegment(char *s)
{
  GlobalUnlockPtr(s);
  GlobalFree(GlobalPtrHandle(s));
}

char *malloc(unsigned n) { return(Lmalloc(n)); }
char *realloc(char *s, unsigned n) { return(Lrealloc(s, n)); }
void free(char *s) { Lfree(s); }

char *calloc(unsigned n, unsigned m)
{
  char *s;
  long size;
  size = ((long) n) * ((long) m);
  s = Lmalloc(size);
  if (s) memset(s, 0, (size_t) size);
  return(s);
}

#else
extern char *Lmalloc(), *Lrealloc();
extern void Lfree();

extern char *malloc();

char *NewSegment() { return(malloc(0x010000)); }
void FreeSegment(s) char *s; { free(s); }

main()
{
  int i, n;
  char *s;

  n = 10000;
  s = Lmalloc(n);
  for (i = 0; i < n; i++) s[i] = 1;
  s = Lrealloc(s, 2 * n);
  for (i = 0; i < 2 * n; i++) s[i] = 1;
  s = Lrealloc(s, n);
  for (i = 0; i < n; i++) s[i] = 1;
}
#endif
