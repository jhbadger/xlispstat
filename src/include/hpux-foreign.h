#define SHLIB_DYNLOAD

#define INTERNAL_CNAME_PATTERN "%s"
#define INTERNAL_FNAME_PATTERN "%s" /**** check this */
#define VERBDFLT TRUE

LOCAL VOID link_and_load _((char *fname, char *libs, int fort));
LOCAL char *get_caddress _((char *name));

#include <dl.h>

/**** This code ought to cache the addresses found, but I won't bother
until I revise the way dynamic loading works */

LOCAL VOID link_and_load(fname, libs, fort)
     char *fname, *libs;
     int fort;
{
  shl_t handle;
  
  handle = shl_load(fname, BIND_DEFERRED, 0L);
  if (handle == NULL) {
    sprintf(buf, "can't open %s", fname);
    xlfail(buf);
  }
}

LOCAL char *get_caddress(name)
     char *name;
{
  shl_t handle;
  char *f;

  handle = NULL;
  if (shl_findsym(&handle, name, TYPE_PROCEDURE, &f) != 0) {
    handle = PROG_HANDLE;
    if (shl_findsym(&handle, name, TYPE_PROCEDURE, &f) != 0)
      f = NULL;
  }
  return(f);
}
