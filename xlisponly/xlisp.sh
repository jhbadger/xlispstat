XLISPLIB=LIBDIR export XLISPLIB
if test -f xlisp.wks; then WKS="xlisp.wks";
else WKS="${XLISPLIB}/xlisp.wks";
fi
if test -f xlisp.bin; then XLISP=xlisp.bin;
else XLISP="${XLISPLIB}/xlisp.bin"
fi
exec ${XLISP} -w${WKS} $*
