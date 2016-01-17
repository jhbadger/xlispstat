define(XLS_CHECK_DEFINE,
[echo checking for $1
AC_PROGRAM_EGREP(yes,
[#ifdef $1
  yes
#endif
], $2)
])dnl
