#ifdef IEEEFP
#define IEEELO 1
#define IEEEHI 0

#define UINT32 unsigned long

#define ieeehi(x) ((UINT32 *)(&(x)))[IEEEHI]
#define ieeelo(x) ((UINT32 *)(&(x)))[IEEELO]

int isnan(x)
     double x;
{
  return (((ieeehi(x) & 0x7FF00000L) == 0x7FF00000L)
	  && ((ieeehi(x) & 0xFFFFFL) != 0 || ieeelo(x) != 0));
}
#endif /* IEEEFP */
