/* fake sys/types.h file for Macintosh CW Pro 2 */
typedef long off_t;
#define bcopy(from,to,n) memcpy(to,from,n)
