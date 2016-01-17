typedef struct {
  char *name;
  int numfunctions;
  VOID (**functions) _((int));
} MODULE;

extern MODULE xlmodules[];
extern int xlnummodules;
extern int xlcurrentmodule;

VOID init_modules _((void));
