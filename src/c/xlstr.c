/* xlstr - xlisp string and character built-in functions */
/* Copyright (c) 1989, by David Michael Betz.                            */
/* You may give out copies of this software; for conditions see the file */
/* COPYING included with this distribution.                              */

#include "xlisp.h"

/* local definitions */
#define fix(n)	cvfixnum((FIXTYPE)(n))
#define TLEFT	1
#define TRIGHT	2

/* Function prototypes */
LOCAL VOID getbounds P5H(LVAL, LVAL, LVAL, unsigned *, unsigned *);
LOCAL LVAL strcompare P2H(int, int);
LOCAL LVAL changecase P2H(int, int);
LOCAL int inbag P2H(int, LVAL);
LOCAL LVAL trim P1H(int);
LOCAL LVAL chrcompare P2H(int, int);

/* getbounds - get the start and end bounds of a string */
LOCAL VOID getbounds P5C(LVAL, str, LVAL, skey, LVAL, ekey,
                         unsigned *, pstart, unsigned *, pend)
{
    LVAL arg;
    unsigned len;
    FIXTYPE n;

    /* get the length of the string */
    len = getslength(str);

    /* get the starting index */
    if (xlgkfixnum(skey,&arg)) {
	*pstart = (unsigned) (n = getfixnum(arg));
	if (n < 0 || n > (FIXTYPE)len)
	    xlerror("string index out of bounds",arg);
    }
    else
	*pstart = 0;

    /* get the ending index */
    if (xlgetkeyarg(ekey, &arg) && arg != NIL) {
	if (!fixp(arg)) xlbadtype(arg);
	*pend = (unsigned)(n = getfixnum(arg));
	if (n < 0 || n > (FIXTYPE)len)
	    xlerror("string index out of bounds",arg);
    }
    else
	*pend = len;

    /* make sure the start is less than or equal to the end */
    if (*pstart > *pend)
	xlerror("starting index error",cvfixnum((FIXTYPE)*pstart));
}

/* strcompare - compare strings */
LOCAL LVAL strcompare P2C(int, fcn, int, icase)
{
    unsigned start1,end1,start2,end2;
    int ch1,ch2;
    unsigned char *p1, *p2;
    LVAL str1,str2;

    /* get the strings */
    str1 = xlgastrorsym();
    str2 = xlgastrorsym();

    /* get the substring specifiers */
    getbounds(str1,k_1start,k_1end,&start1,&end1);
    getbounds(str2,k_2start,k_2end,&start2,&end2);

    xllastkey();

    /* setup the string pointers */
    p1 = (unsigned char *) &getstring(str1)[start1];
    p2 = (unsigned char *) &getstring(str2)[start2];

    /* compare the strings */
    for (; start1 < end1 && start2 < end2; ++start1,++start2) {
	ch1 = *p1++;
	ch2 = *p2++;
	if (icase) {
	    if (ISUPPER(ch1)) ch1 = TOLOWER(ch1);
	    if (ISUPPER(ch2)) ch2 = TOLOWER(ch2);
	}
	if (ch1 != ch2)
	    switch (fcn) {
	    case '<':	return (ch1 < ch2 ? fix(start1) : NIL);
	    case 'L':	return (ch1 <= ch2 ? fix(start1) : NIL);
	    case '=':	return (NIL);
	    case '#':	return (fix(start1));
	    case 'G':	return (ch1 >= ch2 ? fix(start1) : NIL);
	    case '>':	return (ch1 > ch2 ? fix(start1) : NIL);
	    }
    }

    /* check the termination condition */
    switch (fcn) {
    case '<':	return (start1 >= end1 && start2 < end2 ? fix(start1) : NIL);
    case 'L':	return (start1 >= end1 ? fix(start1) : NIL);
    case '=':	return (start1 >= end1 && start2 >= end2 ? s_true : NIL);
    case '#':	return (start1 >= end1 && start2 >= end2 ? NIL : fix(start1));
    case 'G':	return (start2 >= end2 ? fix(start1) : NIL);
    case '>':	return (start2 >= end2 && start1 < end1 ? fix(start1) : NIL);
    }
    return (NIL);   /* avoid compiler warning */
}

/* string comparision functions */
LVAL xstrlss(V) { return (strcompare('<',FALSE)); } /* string< */
LVAL xstrleq(V) { return (strcompare('L',FALSE)); } /* string<= */
LVAL xstreql(V) { return (strcompare('=',FALSE)); } /* string= */
LVAL xstrneq(V) { return (strcompare('#',FALSE)); } /* string/= */
LVAL xstrgeq(V) { return (strcompare('G',FALSE)); } /* string>= */
LVAL xstrgtr(V) { return (strcompare('>',FALSE)); } /* string> */

/* string comparison functions (not case sensitive) */
LVAL xstrilss(V) { return (strcompare('<',TRUE)); } /* string-lessp */
LVAL xstrileq(V) { return (strcompare('L',TRUE)); } /* string-not-greaterp */
LVAL xstrieql(V) { return (strcompare('=',TRUE)); } /* string-equal */
LVAL xstrineq(V) { return (strcompare('#',TRUE)); } /* string-not-equal */
LVAL xstrigeq(V) { return (strcompare('G',TRUE)); } /* string-not-lessp */
LVAL xstrigtr(V) { return (strcompare('>',TRUE)); } /* string-greaterp */

/* changecase - change case */
LOCAL LVAL changecase P2C(int, fcn, int, destructive)
{
    char *srcp, *dstp;
    unsigned start,end,len,i;
    int ch;
    int lastspace = TRUE;
    LVAL src,dst;

    /* get the string */
    src = (destructive? xlgastring() : xlgastrorsym());

    /* get the substring specifiers */
    getbounds(src,k_start,k_end,&start,&end);
    len = getslength(src);

    xllastkey();

    /* make a destination string */
    dst = (destructive ? src : newstring(len));

    /* setup the string pointers */
    srcp = getstring(src);
    dstp = getstring(dst);

    /* copy the source to the destination */
    for (i = 0; i < len; ++i) {
	ch = *srcp++;
	if (i >= start && i < end)
	    switch (fcn) {
	    case 'U':	if (ISLOWER(ch)) ch = TOUPPER(ch); break;
	    case 'D':	if (ISUPPER(ch)) ch = TOLOWER(ch); break;
	    case 'C':   if (lastspace && ISLOWER(ch)) ch = TOUPPER(ch);
			if (!lastspace && ISUPPER(ch)) ch = TOLOWER(ch);
			lastspace = !ISLOWERA(ch) && !ISUPPER(ch);
			break;
	    }
	*dstp++ = (char) ch;
    }
    *dstp = '\0';

    /* return the new string */
    return (dst);
}

/* case conversion functions */
LVAL xupcase(V)   { return (changecase('U',FALSE)); }
LVAL xdowncase(V) { return (changecase('D',FALSE)); }
LVAL xcapcase(V)  { return (changecase('C',FALSE)); }

/* destructive case conversion functions */
LVAL xnupcase(V)   { return (changecase('U',TRUE)); }
LVAL xndowncase(V) { return (changecase('D',TRUE)); }
LVAL xncapcase(V)  { return (changecase('C',TRUE)); }

/* inbag - test if a character is in a bag */
LOCAL int inbag P2C(int, ch, LVAL, bag)
{
                                    /* TAA MOD -- rewritten for \0 */
                                    /*            and chars >= 128 */
    char *p = getstring(bag);
    unsigned len =getslength(bag);

    while (len--)
	if (*p++ == ch)
	    return (TRUE);
    return (FALSE);
}

/* trim - trim character from a string */
LOCAL LVAL trim P1C(int, fcn)
{
    char *leftp, *rightp, *dstp;
    LVAL bag,src,dst;

    /* get the bag and the string */
    bag = xlgaseq();
    src = xlgastrorsym();
    xllastarg();

    xlprot1(bag);
    bag = coerce_to_tvec(bag, a_char);

    /* setup the string pointers */
    leftp = getstring(src);
    rightp = leftp + getslength(src) - 1;

    /* trim leading characters */
    if (fcn & TLEFT)
	while (leftp <= rightp && inbag(*leftp,bag))
	    ++leftp;

    /* trim character from the right */
    if (fcn & TRIGHT)
	while (rightp >= leftp && inbag(*rightp,bag))
	    --rightp;

    /* make a destination string and setup the pointer */
    dst = newstring(((unsigned)(rightp-leftp))+1);
    dstp = getstring(dst);

    /* copy the source to the destination */
    while (leftp <= rightp)
	*dstp++ = *leftp++;
    *dstp = '\0';

    xlpop();

    /* return the new string */
    return (dst);
}

/* trim functions */
LVAL xtrim(V)      { return (trim(TLEFT|TRIGHT)); }
LVAL xlefttrim(V)  { return (trim(TLEFT)); }
LVAL xrighttrim(V) { return (trim(TRIGHT)); }


/* xstring - return a string consisting of a single character */
LVAL xstring(V)
{
    LVAL arg,val;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check the argument type */
    switch (ntype(arg)) {
    case STRING:
	return (arg);
    case SYMBOL:
	return (getpname(arg));
    case CHAR:
	/* Changed 10/94 to allow string '\000' */
	val = newstring(1);
        val->n_string[0] = (char)getchcode(arg);
        val->n_string[1] = '\0';
        return (val);
    case FIXNUM:
	/* Changed 10/94 to allow string 0 */
	val = newstring(1);
        val->n_string[0] = (char)getfixnum(arg);
        val->n_string[1] = '\0';
        return (val);
    default:
	xlbadtype(arg);
	return (NIL);   /* avoid compiler warning */
    }
}

/* xchar - extract a character from a string */
LVAL xchar(V)
{
    LVAL str,num;
    FIXTYPE n;

    /* get the string and the index */
    str = xlgastring();
    num = xlgafixnum();
    xllastarg();

    /* range check the index */
    if ((n = getfixnum(num)) < 0 || n >= (FIXTYPE)getslength(str))
	xlerror("index out of range",num);

    /* return the character */
    return (cvchar(getstringch(str,(unsigned int)n)));
}

/* xcharint - convert a character to an integer */
LVAL xcharint(V)
{
    LVAL arg;
    arg = xlgachar();
    xllastarg();
    return (cvfixnum((FIXTYPE)getchcode(arg)));
}

/* xintchar - convert an integer to a character */
LVAL xintchar(V)
{
    LVAL arg;
    arg = xlgafixnum();
    xllastarg();
    return (cvchar((int)getfixnum(arg)));
}

/* xcharcode - built-in function 'char-code' */
/* TAA mod so that result is 7 bit ascii code */
LVAL xcharcode(V)
{
    int ch;
    ch = 0x7f  & getchcode(xlgachar());
    xllastarg();
    return (cvfixnum((FIXTYPE)ch));
}

/* xcodechar - built-in function 'code-char' */
/* like int-char except range must be 0-127 */
LVAL xcodechar(V)
{
    LVAL arg;
    FIXTYPE ch;
#ifdef __SASC__
    FIXTYPE testch;
#endif

    arg = xlgafixnum(); ch = getfixnum(arg);
    xllastarg();

#ifdef __SASC__
    /* On MVS/CMS, convert EBCDIC character to ASCII for subsequent */
    /* test - Dave Rivers (rivers@ponds.uucp) */
    testch = etoa((unsigned char)ch);
    return (testch >= 0 && testch <= 127 ? cvchar((int)ch) : NIL);
#else
    return (ch >= 0 && ch <= 127 ? cvchar((int)ch) : NIL);
#endif
}

/* xuppercasep - built-in function 'upper-case-p' */
LVAL xuppercasep(V)
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISUPPER(ch) ? s_true : NIL);
}

/* xlowercasep - built-in function 'lower-case-p' */
LVAL xlowercasep(V)
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISLOWERA(ch) ? s_true : NIL);
}

/* xbothcasep - built-in function 'both-case-p' */
LVAL xbothcasep(V)
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISUPPER(ch) || ISLOWER(ch) ? s_true : NIL);
}

/* xdigitp - built-in function 'digit-char-p' */
LVAL xdigitp(V)
{
    int ch;
    FIXTYPE radix = 10;
    ch = getchcode(xlgachar());
    if (moreargs()) {
        radix = getfixnum(xlgafixnum());
        if (radix < 1 || radix > 36) xlfail("radix out of range");
    }
    xllastarg();

    if (isdigit(ch)) ch = ch - '0';
    else if (ISUPPER(ch)) ch = ch - 'A' + 10;
    else if (ISLOWER(ch)) ch = ch - 'a' + 10;
    else return NIL;

    return (ch < radix ? cvfixnum((FIXTYPE) ch) : NIL);
}

/* xchupcase - built-in function 'char-upcase' */
LVAL xchupcase(V)
{
    LVAL arg;
    int ch;
    arg = xlgachar(); ch = getchcode(arg);
    xllastarg();
    return (ISLOWER(ch) ? cvchar(TOUPPER(ch)) : arg);
}

/* xchdowncase - built-in function 'char-downcase' */
LVAL xchdowncase(V)
{
    LVAL arg;
    int ch;
    arg = xlgachar(); ch = getchcode(arg);
    xllastarg();
    return (ISUPPER(ch) ? cvchar(TOLOWER(ch)) : arg);
}

/* xdigitchar - built-in function 'digit-char' */
LVAL xdigitchar(V)
{
    FIXTYPE n, radix = 10;
    n = getfixnum(xlgafixnum());
    if (moreargs()) {
        radix = getfixnum(xlgafixnum());
        if (radix < 1 || radix > 36) xlfail("radix out of range");
    }
    if (moreargs()) xlgetarg(); /* read and ignore font argument */
    xllastarg();
    return (n >= 0 && n < radix ? cvchar((int) n + (n < 10 ? '0' : 'A' - 10))
                                : NIL);
}

/* xalphanumericp - built-in function 'alphanumericp' */
LVAL xalphanumericp(V)
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISUPPER(ch) || ISLOWERA(ch) || isdigit(ch) ? s_true : NIL);
}

/* xalphacharp - built-in function 'alpha-char-p' */
LVAL xalphacharp(V)
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISUPPER(ch) || ISLOWERA(ch) ? s_true : NIL);
}

/* chrcompare - compare characters */
LOCAL LVAL chrcompare P2C(int, fcn, int, icase)
{
    int ch1,ch2,icmp;
    LVAL arg;

    /* get the characters */
    arg = xlgachar(); ch1 = getchcode(arg);

    /* convert to lowercase if case insensitive */
    if (icase && ISUPPER(ch1))
	ch1 = TOLOWER(ch1);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && moreargs(); ch1 = ch2) {

	/* get the next argument */
	arg = xlgachar(); ch2 = getchcode(arg);

	/* convert to lowercase if case insensitive */
	if (icase && ISUPPER(ch2))
	    ch2 = TOLOWER(ch2);

	/* compare the characters */
	switch (fcn) {
	case '<':	icmp = (ch1 < ch2); break;
	case 'L':	icmp = (ch1 <= ch2); break;
	case '=':	icmp = (ch1 == ch2); break;
	case '#':	icmp = (ch1 != ch2); break;
	case 'G':	icmp = (ch1 >= ch2); break;
	case '>':	icmp = (ch1 > ch2); break;
	}
    }

    /* return the result */
    return (icmp ? s_true : NIL);
}

/* character comparision functions */
LVAL xchrlss(V) { return (chrcompare('<',FALSE)); } /* char< */
LVAL xchrleq(V) { return (chrcompare('L',FALSE)); } /* char<= */
LVAL xchreql(V) { return (chrcompare('=',FALSE)); } /* char= */
LVAL xchrneq(V) { return (chrcompare('#',FALSE)); } /* char/= */
LVAL xchrgeq(V) { return (chrcompare('G',FALSE)); } /* char>= */
LVAL xchrgtr(V) { return (chrcompare('>',FALSE)); } /* char> */

/* character comparision functions (case insensitive) */
LVAL xchrilss(V) { return (chrcompare('<',TRUE)); } /* char-lessp */
LVAL xchrileq(V) { return (chrcompare('L',TRUE)); } /* char-not-greaterp */
LVAL xchrieql(V) { return (chrcompare('=',TRUE)); } /* char-equalp */
LVAL xchrineq(V) { return (chrcompare('#',TRUE)); } /* char-not-equalp */
LVAL xchrigeq(V) { return (chrcompare('G',TRUE)); } /* char-not-lessp */
LVAL xchrigtr(V) { return (chrcompare('>',TRUE)); } /* char-greaterp */

LVAL xmkstring(V)
{
  int n, i;
  char c = ' ';
  LVAL arg, result;

  arg = xlgafixnum();
  n = getfixnum(arg);
  if (n < 0) xlerror("Not a nonnegative integer", arg);
  
  if (xlgetkeyarg(k_initelem, &arg)) {
    if (! charp(arg)) xlbadtype(arg);
    c = getchcode(arg);
  }
  result = newstring(n);
  for (i = 0; i < n; i++)
    setstringch(result, i, c);
  return(result);
}
