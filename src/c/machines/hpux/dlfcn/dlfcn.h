#include <dl.h>

#define RTLD_LAZY (BIND_DEFERRED | BIND_NONFATAL)
#define RTLD_NOW BIND_IMMEDIATE

void *dlopen(const char *, int);
void *dlsym(void *, const char *);
int dlclose(void *);
char *dlerror(void);

