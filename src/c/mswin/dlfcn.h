#define RTLD_LAZY 0
#define RTLD_NOW  1

void *dlopen(const char *, int);
void *dlsym(void *, const char *);
int dlclose(void *);
char *dlerror(void);

