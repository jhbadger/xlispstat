#define SHLIB_DYNLOAD

#define INTERNAL_CNAME_PATTERN "%s"
#define INTERNAL_FNAME_PATTERN "%s_" /**** check this */
#define VERBDFLT TRUE

LOCAL VOID link_and_load _((char *fname, char *libs, int fort));
LOCAL char *get_caddress _((char *name));

#include <dlfcn.h>

/**** This code ought to cache the addresses found, but I won't bother
until I revise the way dynamic loading works */

LOCAL VOID link_and_load(fname, libs, fort)
     char *fname, *libs;
     int fort;
{
  static initialized = FALSE;
  void *handle;

  if (! initialized) {
    setvalue(s_cfun_table, NIL);
    initialized = TRUE;
  }  
  
  handle = dlopen(fname, RTLD_LAZY);
  if (handle == NULL) {
    sprintf(buf, "can't open %s", fname);
    xlfail(buf);
  }
  setvalue(s_cfun_table,
	   cons(cvfixnum((FIXTYPE) handle), getvalue(s_cfun_table)));
}

LOCAL char *get_caddress(name)
     char *name;
{
  LVAL next;
  void *handle;
  char *f;
  static int initialized = FALSE;
  static void *prog_handle = NULL;

  for (next = getvalue(s_cfun_table); consp(next); next = cdr(next)) {
    handle = (void *) getfixnum(car(next));
    f = dlsym(handle, name);
    if (f != NULL) return(f);
  }
  if (! initialized) {
    prog_handle = dlopen(NULL, RTLD_LAZY);
    initialized = TRUE;
  }
  f = dlsym(prog_handle, name);
  return(f);
}
