LVAL xlw_lookup_type(char *tname);
LVAL xlgacptr(LVAL type, int null_ok);
void *xlgacptraddr(LVAL type, int null_ok);
LVAL cvcptr(LVAL type, void *v, LVAL data);
LVAL xlw_make_cptr(LVAL type, size_t elsize);
LVAL xlw_cast_cptr(LVAL type);
LVAL xlw_offset_cptr(LVAL type, size_t elsize);

#define DECLARE_CPTR_TYPE(t) static LVAL xlw_##t##_type_tag=NULL;
#define CPTR_TYPE(t) \
  (xlw_##t##_type_tag == NULL ? \
   xlw_##t##_type_tag = xlw_lookup_type(#t) : xlw_##t##_type_tag)
#define cptrp(x) (consp(x)&&stringp(car(x))&&natptrp(cdr(x)))
#define getcptype(x) car(x)
#define getcpptr(x) cdr(x)
#define getcpaddr(x) getnpaddr(getcpptr(x))
#define getcpprot(x) getnpprot(getcpptr(x))
#define newcptr(x,y) cons(x,y)
#define cptr_type_p(p,t) \
  (cptrp(p) && \
   (getcptype(p) == (t) || getcptype(p) == CPTR_TYPE(void)))
