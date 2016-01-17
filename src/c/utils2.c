/* utilities2 - basic utility functions                                */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL s_in_callback;
  
/**************************************************************************/
/**                                                                      **/
/**                          Utility Functions                           **/
/**                                                                      **/
/**************************************************************************/

LVAL integer_list_2 P2C(int, a, int, b)
{
  LVAL list, temp;
  
  xlstkcheck(2);
  xlsave(temp);
  xlsave(list);
  temp = cvfixnum((FIXTYPE) b); list = consa(temp);
  temp = cvfixnum((FIXTYPE) a); list = cons(temp, list);
  xlpopn(2);
  return(list);
}

LVAL integer_list_3 P3C(int, a, int, b, int, c)
{
  LVAL list, temp;
  
  xlstkcheck(2);
  xlsave(temp);
  xlsave(list);
  temp = cvfixnum((FIXTYPE) c); list = consa(temp);
  temp = cvfixnum((FIXTYPE) b); list = cons(temp, list);
  temp = cvfixnum((FIXTYPE) a); list = cons(temp, list);
  xlpopn(2);
  return(list);
}

LVAL integer_list_4 P4C(int, a, int, b, int, c, int, d)
{
  LVAL list, temp;
  
  xlstkcheck(2);
  xlsave(temp);
  xlsave(list);
  temp = cvfixnum((FIXTYPE) d); list = consa(temp);
  temp = cvfixnum((FIXTYPE) c); list = cons(temp, list);
  temp = cvfixnum((FIXTYPE) b); list = cons(temp, list);
  temp = cvfixnum((FIXTYPE) a); list = cons(temp, list);
  xlpopn(2);
  return(list);
}

LVAL send_message P2C(LVAL, object, LVAL, msg)
{
  LVAL argv[2];
  
  argv[0] = object;
  argv[1] = msg;
  return(xscallsubrvec(xmsend, 2, argv));
}

LVAL send_callback_message P2C(LVAL, object, LVAL, msg)
{
  LVAL val, olddenv;

  olddenv = xldenv;
  xldbind(s_in_callback, s_true);
  val = send_message(object, msg);
  xlunbind(olddenv);
  return val;
}

LVAL send_message1 P3C(LVAL, object, LVAL, msg, int, a)
{
  LVAL La, result, argv[3];
  
  xlsave(La);
  La = cvfixnum((FIXTYPE) a);
  argv[0] = object;
  argv[1] = msg;
  argv[2] = La;
  result = xscallsubrvec(xmsend, 3, argv);
  xlpop();
  return(result);
}

LVAL send_callback_message1 P3C(LVAL, object, LVAL, msg, int, a)
{
  LVAL val, olddenv;

  olddenv = xldenv;
  xldbind(s_in_callback, s_true);
  val = send_message1(object, msg, a);
  xlunbind(olddenv);
  return val;
}

LVAL send_message_1L P3C(LVAL, object, LVAL, symbol, LVAL, value)
{
  LVAL argv[3];
  
  argv[0] = object;
  argv[1] = symbol;
  argv[2] = value;
  return(xscallsubrvec(xmsend, 3, argv));
}

LVAL send_callback_message_1L P3C(LVAL, object, LVAL, msg, LVAL, value)
{
  LVAL val, olddenv;

  olddenv = xldenv;
  xldbind(s_in_callback, s_true);
  val = send_message_1L(object, msg, value);
  xlunbind(olddenv);
  return val;
}

LVAL apply_send P3C(LVAL, object, LVAL, symbol, LVAL, args)
{
  LVAL result;

  xlprot1(args);
  args = cons(symbol, args);
  args = cons(object, args);
  result = xsapplysubr(xmsend, args);
  xlpop();
  return(result);
}

LVAL double_list_2 P2C(double, a, double, b)
{
  LVAL list, temp;
  
  xlstkcheck(2);
  xlsave(temp);
  xlsave(list);
  temp = cvflonum((FLOTYPE) b); list = consa(temp);
  temp = cvflonum((FLOTYPE) a); list = cons(temp, list);
  xlpopn(2);
  return(list);
}

LVAL xssysbeep(V)
{
  int count = 10;
  if (moreargs()) count = getfixnum(xlgafixnum());
  xllastarg();
  
  SysBeep(count);
  return(NIL);
}

