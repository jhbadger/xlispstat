#include <stdio.h>
#include <sys/types.h>
#include <regex.h>

int match(char *string, char *pattern)
{
  int i;
  regex_t re;
  char buf[256];

  i=regcomp(&re, pattern, REG_EXTENDED|REG_NOSUB);
  if (i != 0) {
    (void)regerror(i,&re,buf,sizeof buf);
    printf("%s\n",buf);
    return(0);                       /* report error */
  }
  i = regexec(&re, string, (size_t) 0, NULL, 0);
  regfree(&re);
  if (i != 0) {
    (void)regerror(i,&re,buf,sizeof buf);
    printf("%s\n",buf);
    return(0);                       /* report error */
  }
  return(1);
}

void main()
{
  printf("%s\n", match("ABCDE", "[A-Z]*") ? "success" : "failure");
}
