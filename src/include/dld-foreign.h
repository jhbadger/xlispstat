#define SHLIB_DYNLOAD /* not really */

#define INTERNAL_CNAME_PATTERN "%s"
#define INTERNAL_FNAME_PATTERN "%s_" /**** check this */
#define VERBDFLT TRUE

LOCAL VOID link_and_load _((char *fname, char *libs, int fort));
LOCAL char *get_caddress _((char *name));

/*#include <dld.h>*/
/* I don't want to bother looking for this include file, so I'll just */
/* declare the routines I need */
extern int dld_init(), dld_link();
extern unsigned long dld_get_func();

/**** This code ought to cache the addresses found, but I won't bother
until I revise the way dynamic loading works */

static int dld_initialized = FALSE;

LOCAL VOID link_and_load(fname, libs, fort)
     char *fname, *libs;
     int fort;
{
  if (! dld_initialized) {
    if (dld_init(progname))
      xlfail("dld initialization failed");
    dld_initialized = TRUE;
  }
  
  if (dld_link(fname)) {
    sprintf(buf, "can't open %s", fname);
    xlfail(buf);
  }
}

LOCAL char *get_caddress(name)
     char *name;
{
  LVAL next;
  void *handle;
  char *f;

  if (dld_initialized) 
    return((char *) dld_get_func(name));
  else
    return(NULL);
}
