/* objects - Additional object functions                               */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL xlenv, xlfenv, xlvalue;
extern LVAL s_true, sk_own, s_lambda, sk_print, s_stdout, s_tracelist,
  s_self, s_documentation, s_instance_slots, s_proto_name, 
  sk_dispose, s_unbound, s_set_slot_hook, s_message_hook;
extern LVAL s_lambda, xlenv, xlfenv, xldenv;

/* external functions */
extern VOID doenter P3H(LVAL, int, FRAMEP);
extern VOID doexit P2H(LVAL, LVAL);

/* macros to handle tracing *//**** fix to allow NIL? */
#define trenter(sym,argc,argv) {if (!null(sym)) doenter(sym,argc,argv);}
#define trexit(sym,val) {if (!null(sym)) doexit(sym,val);}

/* forward declarations */
LOCAL VOID make_prototype P6H(LVAL, LVAL, LVAL, LVAL, LVAL, int);


/**** make sure this is cleared often enough */
/**** make sure this is right too */
/**** clear every so often on gc?? */
/**** think about optimal cache size */
/**** think about other implementations, hashing functions */
/**** check distribution of hashed indices */
#define CACHED_METHODS
#ifdef CACHED_METHODS
#define METHOD_CACHE_SIZE 199

LVAL s_method_cache;

LOCAL VOID clear_method_cache(V);
#endif /* CACHED_METHODS */

/***********************************************************************/
/**                                                                   **/
/**                          CLASS Definitions                        **/
/**                                                                   **/
/***********************************************************************/

/* instance variable numbers for the class 'CLASS' */
# define CVARS		  2 /* list of class variable names */
# define CVALS		  3 /* list of class variable values */
# define SUPERCLASS   4
# define IVARTOTAL	  6

/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

/* Built in KIND-OF-P function */
LVAL xskind_of_p(V)
{
  LVAL x, y;
  x = xlgetarg();
  y = xlgetarg();
  xllastarg();
  
  return((kind_of_p(x, y)) ? s_true : NIL);
}

LVAL xsobject_null_method(V) { return(NIL); }

/***********************************************************************/
/***********************************************************************/
/***                                                                 ***/
/***                         New Object System                       ***/
/***                                                                 ***/
/***********************************************************************/
/***********************************************************************/
#define OBJECT_SIZE 4
#define getslots(x) getelement(x, 1)
#define getmethods(x) getelement(x, 2)
#define getparents(x) getelement(x, 3)
#define getpreclist(x) getelement(x, 4)
#define setslots(x, v) setelement(x, 1, v)
#define setmethods(w, v) setelement(x, 2, v)
#define setparents(x, v) setelement(x, 3, v)
#define setpreclist(x, v) setelement(x, 4, v)

static LVAL object_class, root_object;
static LVAL s_hardware_object_proto, s_proto;
int in_send = FALSE;

/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

/* get SELF for the current message; signal an error if not in a message */
static LVAL get_self(V)
{
#ifdef BYTECODE
  LVAL p = getvalue(s_self);
#else
  LVAL p = xlxgetvalue(s_self);
#endif /* BYTECODE */
  
  if (! objectp(p)) xlerror("bad object", p);
  return(p);
}

#ifdef DODO
/* simple form of EQUAL test */
static equal P2C(LVAL, x, LVAL, y)
{
  if (x == y) return(TRUE);
  else if (consp(x) && consp(y)
           && equal(car(x), car(y)) && equal(cdr(x), cdr(y)))
    return(TRUE);
  else return(FALSE);
}
#endif /* DODO */

/* check if x is a member of list; use simple equal test */
static int is_member P2C(LVAL, x, LVAL, list)
{
  int result = FALSE;
  
  for (; ! result && consp(list); list = cdr(list))
    if (equal(x, car(list))) result = TRUE;
  return(result);
}

/* check if list contains any duplicates */
static int has_duplicates P1C(LVAL, list)
{
  int result = FALSE;
  
  for (; ! result && consp(list); list = cdr(list))
    if (is_member(car(list), cdr(list))) result = TRUE;
  return(result);
}

/* destructively delete duplicates from list x */
static LVAL delete_duplicates P1C(LVAL, x)
{
  LVAL last, result;

  if (x == NIL) return(NIL);
  else if (consp(x)) {
    for (; consp(x) && is_member(car(x), cdr(x)); x = cdr(x)) ;

    result = x;

    for (last = x, x = cdr(x); consp(x); x = cdr(x))
      if (is_member(car(x), cdr(x))) rplacd(last, cdr(x));
      else last = x;
  }
  else return xlerror("not a list", x);
  return(result);
}

/* destructively append y to x */
static LVAL append_list P2C(LVAL, x, LVAL, y)
{
  LVAL result;
  
  if (x == NIL) result = y;
  else if (consp(x)) {
    result = x;
    for (; consp(cdr(x)); x = cdr(x)) ;
    rplacd(x, y);
  }
  else return xlerror("not a list", x);
  return(result);
}

/* destructively delete x from list */
static LVAL delete P2C(LVAL, x, LVAL, list)
{
  return(xlcallsubr2(xdelete, x, list));
}

/***********************************************************************/
/**                                                                   **/
/**                Predicate and Stack Access Functions               **/
/**                                                                   **/
/***********************************************************************/

static LVAL check_object P1C(LVAL, object)
{
  if (! objectp(object)) xlerror("bad object", object);
  return(object);
}

int kind_of_p P2C(LVAL, x, LVAL, y)
{
  if (! objectp(x) || ! objectp(y)) return(FALSE);
  return(is_member(y, getpreclist(x)));
}

/***********************************************************************/
/**                                                                   **/
/**                    Precedence List Functions                      **/
/**                                                                   **/
/***********************************************************************/

/* find set of object and ancestors */
static LVAL find_SC P1C(LVAL, object)
{
  return(copylist(getpreclist(check_object(object))));
}

/* find set of object and ancestors */
static LVAL find_S P1C(LVAL, object)
{
  LVAL result, parents;
  
  xlstkcheck(2);
  xlprotect(object);
  xlsave(result);
  parents = getparents(object);
  for (result = NIL; consp(parents); parents = cdr(parents))
    result = append_list(find_SC(car(parents)), result);
  result = cons(object, result);
  result = delete_duplicates(result);
  xlpopn(2);
  return(result);
}

/* find local precedence ordering */
static LVAL find_RC P1C(LVAL, object)
{
  LVAL list, next;
  
  xlstkcheck(2);
  xlprotect(object);
  xlsave(list);
  list = copylist(getparents(check_object(object)));
  for (next = list; consp(next); next = cdr(next)) {
    rplaca(next, cons(object, car(next)));
    object = cdr(car(next));
  }
  xlpopn(2);
  return(list);
}

/* find partial precedence ordering */
static LVAL find_R P1C(LVAL, S)
{
  LVAL result;
  
  xlstkcheck(2);
  xlprotect(S);
  xlsave(result);
  for (result = NIL; consp(S); S = cdr(S))
    result = append_list(result, find_RC(car(S)));
  result = delete_duplicates(result);
  xlpopn(2);
  return(result);
}

/* check if x has a predecessor according to R */
static int has_predecessor P2C(LVAL, x, LVAL, R)
{
  int result = FALSE;
  
  for (; ! result && consp(R); R = cdr(R))
    if (consp(car(R)) && x == cdr(car(R))) result = TRUE;
  return(result);
}

/* find list of objects in S without predecessors, by R */
static LVAL find_no_predecessor_list P2C(LVAL, S, LVAL, R)
{
  LVAL result;
  
  xlstkcheck(3);
  xlprotect(S);
  xlprotect(R);
  xlsave(result);
  for (result = NIL; consp(S); S = cdr(S))
    if (! has_predecessor(car(S), R))
      result = cons(car(S), result);
  xlpopn(3);
  return(result);
}

/* find the position of child, if any, of x in P, the list found so far */
static int child_position P2C(LVAL, x, LVAL, P)
{
  int count;
  
  for (count = 0; consp(P); P = cdr(P), count++)
    if (is_member(x, getparents(car(P)))) return(count);
  return(-1);
}

/* find the next object in the precedence list from objects with no */
/* predecessor and current list.                                    */
static LVAL next_object P2C(LVAL, no_preds, LVAL, P)
{
  LVAL result;
  int count, tcount;

  if (! consp(no_preds)) result = NIL;
  else if (! consp(cdr(no_preds))) result = car(no_preds);
  else {
    for (count = -1, result = NIL; consp(no_preds); no_preds = cdr(no_preds)) {
      tcount = child_position(car(no_preds), P);
      if (tcount > count) {
        result = car(no_preds);
        count = tcount;
      }
    }
  }
  return(result);
}

/* remove object x from S */
static LVAL trim_S P2C(LVAL, x, LVAL, S)
{
  LVAL next;
  
  while (consp(S) && x == car(S)) S = cdr(S);
  for (next = S; consp(S) && consp(cdr(next));)
    if (x == car(cdr(next))) rplacd(next, cdr(cdr(next)));
    else next = cdr(next);
  return(S);
}

/* remove all pairs containing x from R. x is assumed to have no */
/* predecessors, so only the first position is checked.          */
static LVAL trim_R P2C(LVAL, x, LVAL, R)
{
  LVAL next;
  
  while (consp(R) && consp(car(R)) && x == car(car(R))) R = cdr(R);
  for (next = R; consp(R) && consp(cdr(next));)
    if (consp(car(next)) && x == car(car(cdr(next))))
      rplacd(next, cdr(cdr(next)));
    else next = cdr(next);
  return(R);
}

/* calculat the object's precedence list */
static LVAL precedence_list P1C(LVAL, object)
{
  LVAL R, S, P, no_preds, next;
  
  check_object(object);
  xlstkcheck(5);
  xlprotect(object);
  xlsave(R);
  xlsave(S);
  xlsave(P);
  xlsave(no_preds);
  S = find_S(object);
  R = find_R(S);
  P = NIL;
  while (consp(S)) {
    no_preds = find_no_predecessor_list(S, R);
    next = next_object(no_preds, P);
    if (next == NIL) xlfail("inconsistent precedence order");
    else {
      P = append_list(P, consa(next));
      S = trim_S(next, S);
      R = trim_R(next, R);
    }
  }
  xlpopn(5);
  return(P);
}

/***********************************************************************/
/**                                                                   **/
/**                  Object Construction Functions                    **/
/**                                                                   **/
/***********************************************************************/

static LVAL calculate_preclist P1C(LVAL, object)
{
  LVAL result, parent, parents;
  
  parents = getparents(check_object(object));
  if (consp(parents)) {
    xlstkcheck(2);
    xlprotect(object);
    xlsave(result);
    if (! consp(cdr(parents))) {
      parent = check_object(car(parents));
      result = getpreclist(parent);
      result = cons(object, result);
    }
    else result = precedence_list(object);
    xlpopn(2);
  }
  else xlerror("bad parent list", parents);
  return(result);
}

static VOID check_parents P1C(LVAL, parents)
{
  if (parents == NIL) return;
  else if (objectp(parents)) return;
  else if (consp(parents)) {
    for (; consp(parents); parents = cdr(parents))
      check_object(car(parents));
  }
  else xlerror("bad parents", parents);
  if (consp(parents) && has_duplicates(parents))
    xlfail("parents may not contain duplicates");
}

static LVAL make_object P2C(LVAL, parents, LVAL, object)
{
  check_parents(parents);
  
  xlstkcheck(2);
  xlprotect(parents);
  xlprotect(object);
  
  if (! objectp(object))
    object = newobject(object_class, OBJECT_SIZE);

  setpreclist(object, getpreclist(root_object));
  if (parents == NIL) setparents(object, consa(root_object));
  else if (objectp(parents)) setparents(object, consa(parents));
  else setparents(object, parents);
  
  setpreclist(object, calculate_preclist(object));
  xlpopn(2);
  return(object);
}

LVAL xsmake_object(V)
{
  LVAL parents, object;
  
  xlsave1(parents);
  parents = makearglist(xlargc, xlargv);
  object = make_object(parents, NIL);
  xlpop();
  return(object);
}

LVAL xsreparent_object(V)
{
  LVAL parents, object;
  object = xlgaobject();

#ifdef CACHED_METHODS
  clear_method_cache();
#endif /* CACHED_METHODS */

  xlsave1(parents);
  if (kind_of_p(object, getvalue(s_hardware_object_proto)))
    send_message(object, sk_dispose);
  parents = makearglist(xlargc, xlargv);
  object = make_object(parents, object);
  xlpop();
  return(object);
}

/***********************************************************************/
/**                                                                   **/
/**                      Slot Access Functions                        **/
/**                                                                   **/
/***********************************************************************/

#define make_slot_entry(x, y) cons((x), (y))
#define slot_entry_p(x) consp((x))
#define slot_entry_key(x) car((x))
#define slot_entry_value(x) cdr((x))
#define set_slot_entry_value(x, v) rplacd((x), (v))

static LVAL find_own_slot P2C(LVAL, x, LVAL, slot)
{
  LVAL slots;
  
  if (! objectp(x)) return(NIL);
  for (slots = getslots(x); consp(slots); slots = cdr(slots))
    if (slot_entry_p(car(slots)) && slot_entry_key(car(slots)) == slot) 
      return(car(slots));
  return(NIL);
}

static LVAL find_slot P2C(LVAL, x, LVAL, slot)
{
  LVAL slot_entry, preclist;
  
  if (! objectp(x)) slot_entry = NIL;
  else {
    for (slot_entry = NIL, preclist = getpreclist(x);
         slot_entry == NIL && consp(preclist);
         preclist = cdr(preclist))
      slot_entry = find_own_slot(car(preclist), slot);
  }    
  return(slot_entry);
}

static VOID add_slot P3C(LVAL, x, LVAL, slot, LVAL, value)
{
  LVAL slot_entry;
  
  xlstkcheck(3);
  xlprotect(x);
  xlprotect(slot);
  xlprotect(value);
  check_object(x);
  
  if (! symbolp(slot)) xlerror("not a symbol", slot);
  slot_entry = find_own_slot(x, slot);
  if (slot_entry != NIL) set_slot_entry_value(slot_entry, value);
  else {
    xlsave1(slot_entry);
    slot_entry = make_slot_entry(slot, value);
    setslots(x, cons(slot_entry, getslots(x)));
    xlpop();
  }
  xlpopn(3);
}

static LVAL delete_slot P2C(LVAL, x, LVAL, slot)
{
  LVAL entry, slots;
  
  if (! objectp(x)) return(NIL);
  else {
    entry = find_own_slot(x, slot);
    if (entry == NIL) return(NIL);
    else {
      slots = getslots(x);
      setslots(x, delete(entry, slots));
      return(s_true);
    }
  }
}

LVAL slot_value P2C(LVAL, x, LVAL, slot)
{
  LVAL slot_entry;
  
  check_object(x);
  slot_entry = find_slot(x, slot);
  if (slot_entry_p(slot_entry)) return(slot_entry_value(slot_entry));
  else xlerror("no slot by this name", slot);
  /* not reached */
  return(NIL);
}

/*#define CONSTRAINTHOOKS*/

#ifdef CONSTRAINTHOOKS
LOCAL VOID check_hooks P3C(LVAL, object, LVAL, sym, LVAL, slot)
{
  LVAL hook, hooksym, olddenv;
  
  hooksym = (slot) ? s_set_slot_hook : s_message_hook;
  hook = getvalue(hooksym);
  if (hook != s_unbound && hook != NIL) {
    /* rebind the hook function to nil */
    olddenv = xldenv;
    xldbind(hooksym,NIL);

    xsfuncall2(hook, object, sym);

    /* unbind the hook symbol */
    xlunbind(olddenv);
  }
}
#endif /* CONSTRAINTHOOKS */

LVAL set_slot_value P3C(LVAL, x, LVAL, slot, LVAL, value)
{
  LVAL slot_entry;
  
  check_object(x);
  slot_entry = find_own_slot(x, slot);
  if (slot_entry_p(slot_entry)) {
    set_slot_entry_value(slot_entry, value);
#ifdef CONSTRAINTHOOKS
    check_hooks(x, slot_entry_key(slot_entry), TRUE);
#endif /* CONSTRAINTHOOKS */
  }
  else {
    if (find_slot(x, slot) != NIL)
      xlerror("object does not own slot", slot);
    else xlerror("no slot by this name", slot);
  }
  return(value);
}

LVAL xshas_slot(V)
{
  LVAL x, slot, own, slot_entry;
  
  x = xlgaobject();
  slot = xlgasymbol();
  if (! xlgetkeyarg(sk_own, &own)) own = NIL;
  
  slot_entry = (own == NIL) ? find_slot(x, slot) : find_own_slot(x, slot);
  return((slot_entry != NIL) ? s_true : NIL);
}

LVAL xsadd_slot(V)
{
  LVAL x, slot, value;
  
  x = xlgaobject();
  slot = xlgasymbol();
  value = (moreargs()) ? xlgetarg() : NIL;
  xllastarg();
  
  add_slot(x, slot, value);
  return(value);
}

LVAL xsdelete_slot(V)
{
  LVAL x, slot;
  
  x = xlgaobject();
  slot = xlgasymbol();
  xllastarg();
  
  return(delete_slot(x, slot));
}

LVAL xsslot_value(V)
{
  LVAL x, slot, value = NULL;
  int set = FALSE;
  
  x = get_self(); /*xlgaobject();*/
  slot = xlgasymbol();
  if (moreargs()) {
    set = TRUE;
    value = xlgetarg();
  }
  xllastarg();
  
  if (set) return(set_slot_value(x, slot, value));
  else return(slot_value(x, slot));
}

/***********************************************************************/
/**                                                                   **/
/**                    Method Access Functions                        **/
/**                                                                   **/
/***********************************************************************/

#define make_method_entry(x, y) cons((x), (y))
#define method_entry_p(x) consp((x))
#define method_entry_key(x) car((x))
#define method_entry_method(x) cdr((x))
#define set_method_entry_method(x, v) rplacd((x), (v))

static LVAL find_own_method P2C(LVAL, x, LVAL, selector)
{
  LVAL methods;
  
  if (! objectp(x)) return(NIL);
  for (methods = getmethods(x); consp(methods); methods = cdr(methods))
    if (method_entry_p(car(methods)) 
        && method_entry_key(car(methods)) == selector)
      return(car(methods));
  return(NIL);
}

static LVAL find_method P2C(LVAL, x, LVAL, selector)
{
  LVAL method_entry, preclist;
  
  if (! objectp(x)) method_entry = NIL;
  else {
    for (method_entry = NIL, preclist = getpreclist(x);
         method_entry == NIL && consp(preclist);
         preclist = cdr(preclist))
      method_entry = find_own_method(car(preclist), selector);
  }    
  return(method_entry);
}

static VOID add_method P3C(LVAL, x, LVAL, selector, LVAL, method)
{
  LVAL method_entry;
  
  xlstkcheck(3);
  xlprotect(x);
  xlprotect(selector);
  xlprotect(method);
  
#ifdef CACHED_METHODS
  clear_method_cache();
#endif /* CACHED_METHODS */

  check_object(x);
  if (! symbolp(selector)) xlerror("not a symbol", selector);
  switch (ntype(method)) {
  case BCCLOSURE:
    if (getbcname(getbcccode(method)) == NIL)
      setbcname(getbcccode(method), selector);
    break;
  case CLOSURE:
    if (getname(method) == NIL)
      setname(method, selector);
    break;
  }
  method_entry = find_own_method(x, selector);
  if (method_entry != NIL)
    set_method_entry_method(method_entry, method);
  else {
    xlsave1(method_entry);
    method_entry = make_method_entry(selector, method);
    setmethods(x, cons(method_entry, getmethods(x)));
    xlpop();
  }
  xlpopn(3);
}

static LVAL delete_method P2C(LVAL, x, LVAL, selector)
{
  LVAL entry, methods;
  
  if (! objectp(x)) return(NIL);
  else {
#ifdef CACHED_METHODS
    clear_method_cache();
#endif /* CACHED_METHODS */

    entry = find_own_method(x, selector);
    if (entry == NIL) return(NIL);
    else {
      methods = getmethods(x);
      setmethods(x, delete(entry, methods));
      return(s_true);
    }
  }
}

static LVAL message_method P2C(LVAL, x, LVAL, selector)
{
  LVAL method_entry;
  
  check_object(x);
  method_entry = find_method(x, selector);
  if (method_entry_p(method_entry)) 
    return(method_entry_method(method_entry));
  else xlfail("no method for this selector");
  /* not reached */
  return(NIL);
}

#ifdef DODO
static LVAL set_message_method P3C(LVAL, x, LVAL, selector, LVAL, method)
{
  LVAL method_entry;
  
  check_object(x);
  method_entry = find_method(x, selector);
  if (method_entry_p(method_entry))
    set_method_entry_method(method_entry, method);
  else xlfail("no method for this selector");
  return(method);
}
#endif /* DODO */
  
LVAL xshas_method(V)
{
  LVAL x, selector, own, method_entry;
  
  x = xlgaobject();
  selector = xlgasymbol();
  if (! xlgetkeyarg(sk_own, &own)) own = NIL;
  
  method_entry = (own == NIL)
               ? find_method(x, selector) : find_own_method(x, selector);
  return((method_entry != NIL) ? s_true : NIL);
}

LVAL xsadd_method(V)
{
  LVAL x, selector, method;
  
  x = xlgaobject();
  selector = xlgasymbol();
  method = (moreargs()) ? xlgetarg() : NIL;
  xllastarg();
  
  add_method(x, selector, method);
  return(method);
}

LVAL xsdelete_method(V)
{
  LVAL x, selector;
  
  x = xlgaobject();
  selector = xlgasymbol();
  xllastarg();
  
  return(delete_method(x, selector));
}

LVAL xsmessage_method(V)
{
  LVAL x, selector;
  
  x = xlgaobject();
  selector = xlgasymbol();
  xllastarg();
  
  return(message_method(x, selector));
}

/***********************************************************************/
/**                                                                   **/
/**                    Message Sending Functions                      **/
/**                                                                   **/
/***********************************************************************/

static LVAL current_preclist = NIL;
static LVAL current_selector = NIL;

#ifdef CACHED_METHODS
static int cache_cleared = TRUE;

#define mhash(x, y) \
  (((((unsigned long) CVPTR(x)) << 2) ^ ((unsigned long) CVPTR(y))) \
  % METHOD_CACHE_SIZE)
/*
#define mhash(x, y) \
  ((((unsigned long) (x)) ^ (((unsigned long) y) >> 4)) % METHOD_CACHE_SIZE)
*/

LOCAL VOID clear_method_cache(V)
{
  LVAL cache;
  int i, n;

  if (! cache_cleared) {
    cache = getvalue(s_method_cache);
    n = getsize(cache);
    for (i = 0; i < n; i++) setelement(cache, i, NIL);
  }
}

LOCAL LVAL find_cached_method P1C(LVAL, selector)
{
  LVAL keylist, preclist, method_entry, cache, clist, centry;
  int index;

  /* skip leading entries in precedence list that have no methods */
  for (preclist = current_preclist;
       consp(preclist) && !consp(getmethods(car(preclist)));
       preclist = cdr(preclist));
  
  /* look for a cached method */
  cache = getvalue(s_method_cache);
  for (clist = getelement(cache, mhash(preclist, selector));
       consp(clist);
       clist = cdr(clist)) {
    centry = car(clist);
    if (preclist == car(centry)
	&& selector == method_entry_key(car(cdr(centry)))) {
      current_preclist = cdr(cdr(centry));
      return(car(cdr(centry)));
    }
  }

  /* no cached method found -- do it the hard way */
  for (keylist = preclist; consp(preclist); preclist = cdr(preclist)) {
    method_entry = find_own_method(car(preclist), selector);
    if (! null(method_entry)) {
      xlsave1(centry);
      centry = cons(method_entry, preclist);
      centry = cons(keylist,centry);
      index = mhash(keylist, selector);
      clist = cons(centry, getelement(cache, index));
      setelement(cache, index, clist);
      cache_cleared = FALSE;
      current_preclist = preclist;
      xlpop();
      return(method_entry);
    }
  }
  return(NIL);
}
#endif /* CACHED_METHODS */

/*#define SAFEMESS*/
#ifndef SAFEMESS
static LVAL callmethod P4C(LVAL, method, LVAL, object, int, argc, LVAL *, argv)
{
  LVAL *newfp;
    
  /* build a new argument stack frame */
  if (xlsp + 4 + argc > xlargstktop) xlargstkoverflow();
  newfp = xlsp;
  *xlsp++ = cvfixnum((FIXTYPE)(newfp - xlfp));
  *xlsp++ = method;
  *xlsp++ = cvfixnum((FIXTYPE) (argc + 1));

  /* copy the arguments */
  *xlsp++ = object;
  MEMCPY(xlsp, argv, sizeof(LVAL) * argc);
  xlsp += argc;

  /* establish the new stack frame */
  xlfp = newfp;

  return(xlapply(argc + 1));
}
#endif /* SAFEMESS */

static LVAL sendmsg P2C(LVAL, object, LVAL, selector)
{
  LVAL method_entry, method = NULL, old_preclist, val, old_selector;
#ifndef CACHED_METHODS
  LVAL preclist;
#endif /*  */
#ifdef BYTECODE
  LVAL olddenv;
#endif /* BYTECODE */
  LVAL tracing = NIL;
#ifdef SAFEMESS
  LVAL args;
#endif

  old_selector = current_selector;
  current_selector = selector;

#ifdef BYTECODE
  /***** bind SELF dynamically -- should be different special variable */
  olddenv = xldenv;
  xldbind(s_self, object);
#endif /* BYTECODE */

  /* look for the message in the precedence list */
  old_preclist = current_preclist;
#ifdef CACHED_METHODS
  method_entry = find_cached_method(selector);
#else
  for (method_entry = NIL, preclist = current_preclist;
       method_entry == NIL && consp(preclist);
       preclist = cdr(preclist)) {
    method_entry = find_own_method(car(preclist), selector);
    current_preclist = preclist;
  }
#endif /* CACHED_METHODS */
  if (method_entry == NIL)
    xlerror("no method for this message", selector);
  /* else if (! method_entry_p(method_entry)) xlfail("bad method entry"); */
  else method = method_entry_method(method_entry);
   
  /* invoke the method */
  if (getvalue(s_tracelist) && is_member(selector,getvalue(s_tracelist)))
    tracing = selector;
  trenter(tracing,xlargc,xlargv);
#ifdef SAFEMESS
  xlsave1(args);
  args = makearglist(xlargc, xlargv);
  args = cons(object, args);
  val = xlapply(pushargs(method, args));
  xlpop();
#else
/*#define FASTMESS*/
#ifdef FASTMESS
  {
    LVAL *p, *oldfp, *oldsp, *oldargv;
    int i, oldargc;

    switch (xlargv - xlfp) {
    case 3: /* call-next -- selector was not on the stack */
      oldsp = xlsp;
      oldfp = xlfp;

      /* shift the arguments up by one */
      if (xlsp >= xlargstktop) xlargstkoverflow();
      for (p = xlargv + xlargc - 1; p >= xlargv; p--) p[1] = p[0];
      xlsp++;
      xlargc++;

      /* install the object as first argument */
      xlargv[0] = object;
      xlfp[2] = cvfixnum((FIXTYPE) xlargc);

      /* overwrite the function in the current call frame with the method */
      xlfp[1] = method;

      /* execute the call */
      val = xlapply(xlargc);
      xlsp = oldsp;
      xlfp = oldfp;
      break;
    case 5:
      oldsp = xlsp;
      oldfp = xlfp;
      oldargv = xlargv;
      oldargc = xlargc;

      /* decrement xlargv and shift the arguments down by one */
      xlargv--;
      for (i = 0, p = xlargv + 1; i < xlargc; i++, p++) xlargv[i] = *p;
      xlargc++;

      /* install the object as first argument */
      xlargv--;
      xlargv[0] = object;
      xlfp[2] = cvfixnum((FIXTYPE) xlargc);

      /* overwrite the function in the current call frame with the method */
      xlfp[1] = method;

      /* execute the call */
      val = xlapply(xlargc);
      xlsp = oldsp;
      xlfp = oldfp;
      xlargv = oldargv;
      xlargc = oldargc;
      break;
    default: /* shouldn't happen */
      stdputstr("default send \n");
      val = callmethod(method, object, xlargc, xlargv);
    }
  }
#else
  val = callmethod(method, object, xlargc, xlargv);
#endif /* FASTMESS */
#endif /* SAFEMESS */
  trexit(tracing,val);
  
#ifdef BYTECODE
  /*** unbind SELF */
  xlunbind(olddenv);
#endif /* BYTECODE */

  current_preclist = old_preclist;
  current_selector = old_selector;
#ifdef CONSTRAINTHOOKS
  check_hooks(object, method_entry_key(method_entry), FALSE);
#endif /* CONSTRAINTHOOKS */
  return(val);
}

/* send message with arguments on the stack */
LVAL send_message_stk P2C(LVAL, object, LVAL, selector)
{
  LVAL old_preclist, result;
  int old_in_send = in_send;
  
  old_preclist = current_preclist;
  current_preclist = getpreclist(object);
  in_send = TRUE;
  result = sendmsg(object, selector);
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}


/* xmsendsuper - send a message to the superobject of an object */
LVAL xmsendsuper(V)
{
  LVAL old_preclist, object, result;
  int old_in_send = in_send;
  
  object = get_self();
  old_preclist = current_preclist;
  if (! consp(current_preclist))
    xlfail("no more objects in precedence list");
  current_preclist = cdr(current_preclist);
  in_send = TRUE;
  result = sendmsg(object, xlgasymbol());
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}

/* xscall_next - call inherited version of current method */
LVAL xscall_next(V)
{
  LVAL old_preclist, object, result;
  int old_in_send = in_send;
  
  object = get_self();
  old_preclist = current_preclist;
  if (! consp(current_preclist))
    xlfail("no more objects in precedence list");
  current_preclist = cdr(current_preclist);
  in_send = TRUE;
  result = sendmsg(object, current_selector);
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}

LVAL xmsend(V)
{
  LVAL object, old_preclist, result;
  int old_in_send = in_send;
  
  object = xlgaobject();
  if (! objectp(object)) return(NIL);

  old_preclist = current_preclist;
  current_preclist = getpreclist(object);
  in_send = TRUE;
  result = sendmsg(object, xlgasymbol());
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}
  
LVAL xscall_method(V) 
{
  LVAL object, self, old_preclist, result;
  int old_in_send = in_send;
  
  object = xlgaobject();
  self = get_self();
  old_preclist = current_preclist;
  current_preclist = getpreclist(object);
  in_send = TRUE;
  result = sendmsg(self, xlgasymbol());
  current_preclist = old_preclist;
  in_send = old_in_send;
  return(result);
}
  
VOID print_mobject P2C(LVAL, object, LVAL, stream)
{
  send_message_1L(object, sk_print, stream);
}

LVAL xsshow_object(V)
{
  LVAL x, fptr;
  
  x = xlgaobject();
  fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
  xllastarg();
  
  xlputstr(fptr, "Slots = "); xlprint(fptr, getslots(x), TRUE); xlterpri(fptr);
  xlputstr(fptr, "Methods = "); xlprint(fptr, getmethods(x), TRUE); xlterpri(fptr);
  xlputstr(fptr, "Parents = "); xlprint(fptr, getparents(x), TRUE); xlterpri(fptr);
  xlputstr(fptr, "Precedence List = "); xlprint(fptr, getpreclist(x), TRUE); xlterpri(fptr);
  return(NIL);
}

LVAL xsparents(V)
{
  LVAL x;
  
  x = xlgaobject();
  xllastarg();
  
  return(copylist(getparents(x)));
}

LVAL xsprecedence_list(V)
{
  LVAL x;
  
  x = xlgaobject();
  xllastarg();
  
  return(copylist(getpreclist(x)));
}

static LVAL get_cars P1C(LVAL, x)
{
  LVAL next;
  
  for (next = x; consp(next); next = cdr(next))
  	if (consp(car(next)))
  	  rplaca(next, car(car(next)));
  return(x);
}

LVAL xsobject_methods(V)
{
  LVAL x;
  
  x = xlgaobject();
  xllastarg();
  
  return(get_cars(copylist(getmethods(x))));
}

LVAL xsobject_slots(V)
{
  LVAL x;
  
  x = xlgaobject();
  xllastarg();
  
  return(get_cars(copylist(getslots(x))));
}

VOID statobsymbols(V)
{
  object_class = getvalue(xlenter("OBJECT"));
  root_object = getvalue(xlenter("*OBJECT*"));
  s_hardware_object_proto = xlenter("HARDWARE-OBJECT-PROTO");
  s_proto = xlenter("PROTO");
#ifdef CACHED_METHODS
  s_method_cache = xlenter("*METHOD-CACHE*");
  setvalue(s_method_cache, newvector(METHOD_CACHE_SIZE));
#endif /* CACHED_METHODS */
}

int lex_slot_value P3C(LVAL, object, LVAL, sym, LVAL *, pval)
{
  int has = (find_slot(object, sym) != NIL);
  if (has) *pval = slot_value(object, sym);
  return(has);
}

VOID object_isnew P1C(LVAL, object)
{
  LVAL slots, sym, ksym;
  int i;

  for (slots = getslots(object); consp(slots); slots = cdr(slots)) {
    sym = car(car(slots));
    if (! symbolp(sym)) xlerror("bad slot entry", car(slots));
#ifdef PACKAGES
    ksym = xlintern(getstring(getpname(sym)), xlkeypack);
#else
    sprintf(buf, ":%s", getstring(getpname(sym)));
    ksym = xlenter(buf);
#endif /* PACKAGES */
    /* go through the keys but don't modify or change xlarg[cv] */
    for (i = 0; i + 1 < xlargc; i += 2) {
      if (ksym == xlargv[i]) {
	set_slot_value(object, sym, xlargv[i + 1]);
	break;
      }
    }
  }
}

LVAL xsobject_isnew(V)
{
  LVAL object;

  object = xlgaobject();
  object_isnew(object);
  return(object);
}
  
#define FIRST_METHOD_OFFSET 560

/* xsaddmsg - add a message to an object */
VOID xsaddmsg P2C(LVAL, object, char *, str)
{
  LVAL fcn;
  static offset = FIRST_METHOD_OFFSET;

  xlsave1(fcn);
  fcn = cvsubr(funtab[offset].fd_subr,funtab[offset].fd_type,offset);
  add_method(object, xlenter(str), fcn);
  xlpop();
  
  offset++;
}

VOID xsaddslot P2C(LVAL, object, char *, str)
{
  add_slot(object, xlenter(str), NIL);
}

LVAL xsnewproto P2C(char *, str, LVAL, parents)
{
  LVAL sym = xlenter(str), object;

  xlsave1(object);
  object = make_object(parents, NIL);
  make_prototype(object, sym, NIL, NIL, NIL, TRUE);
  xlpop();
  
  return(object);
}

LVAL init_root_object(V)
{
  LVAL s__object_ = xlenter("*OBJECT*");
  
  object_class = getvalue(xlenter("OBJECT"));
  root_object = newobject(object_class, OBJECT_SIZE);
  setvalue(s__object_, root_object);
  setpreclist(root_object, consa(root_object));
  
  add_slot(root_object, s_instance_slots, NIL);
  add_slot(root_object, s_proto_name, s__object_);
  return(root_object);
}

static LVAL find_documentation P3C(LVAL, x, LVAL, sym, int, add)
{
  LVAL doc;
  
  if (! objectp(x)) return(NIL);
  doc = find_own_slot(x, s_documentation);
  if (doc == NIL && add) add_slot(x, s_documentation, NIL);
  if (consp(doc)) doc = cdr(doc);
  for (; consp(doc); doc = cdr(doc))
    if (consp(car(doc)) && car(car(doc)) == sym) return(car(doc));
  return(NIL);
}

/* x should be protected from gc before calling add_slot */
static VOID add_documentation P3C(LVAL, x, LVAL, sym, LVAL, value)
{
  LVAL doc_entry;
  
  xlstkcheck(3);
  xlprotect(x);
  xlprotect(sym);
  xlprotect(value);
  check_object(x);
  if (! symbolp(sym)) xlerror("not a symbol", sym);
  doc_entry = find_documentation(x, sym, TRUE);
  if (doc_entry != NIL) rplacd(doc_entry, value);
  else {
    xlsave1(doc_entry);
    doc_entry = cons(sym, value);
    set_slot_value(x,
                   s_documentation,
                   cons(doc_entry, slot_value(x, s_documentation)));
    xlpop();
  }
  xlpopn(3);
}

static LVAL get_documentation P2C(LVAL, x, LVAL, sym)
{
  LVAL doc_entry;
  
  check_object(x);
  doc_entry = find_documentation(x, sym, FALSE);
  return (consp(doc_entry) ? cdr(doc_entry) : NIL);
}

LVAL xsobject_documentation(V)
{
  LVAL x, sym, val;
  
  x = xlgaobject();
  sym = xlgasymbol();
  if (moreargs()) {
    val = xlgetarg();
    add_documentation(x, sym, val);
  }
  return(get_documentation(x, sym));
}
  

LVAL xsdefmeth(V)
{
  LVAL object, sym, fargs, arglist, fcn;
  
  xlstkcheck(3);
  xlsave(fargs);
  xlsave(arglist);
  xlsave(fcn);
  object = xleval(xlgetarg());
  sym = xlgasymbol();
  fargs = xlgalist();
  arglist = makearglist(xlargc,xlargv);

  if (! objectp(object)) xlerror("bad object", object);

  /* install documentation string */
  if (consp(arglist) && stringp(car(arglist)) && consp(cdr(arglist))) {
    add_documentation(object, sym, car(arglist));
    arglist = cdr(arglist);
  }

  /* create a new function definition */
  fargs = cons(s_self, fargs);
  fcn = xlclose(sym, s_lambda, fargs, arglist, xlenv, xlfenv);

  /* add the method to the object */
  add_method(object, sym, fcn);
  
  /* restore the stack and return the symbol */
  xlpopn(3);
  return (sym);
}

/***********************************************************************/
/**                                                                   **/
/**                  Prototype Construction Functions                 **/
/**                                                                   **/
/***********************************************************************/

static LVAL instance_slots P2C(LVAL, x, LVAL, slots)
{
  LVAL parents = getparents(x), result, sym, temp, tail;
  
  xlsave1(result);
  result = copylist(slots);
  result = delete_duplicates(result);
  for (tail = result; consp(tail) && consp(cdr(tail)); tail = cdr(tail));

  for (; consp(parents); parents = cdr(parents)) {
    for (temp = slot_value(car(parents), s_instance_slots);
         consp(temp);
         temp = cdr(temp)) {
      sym = car(temp);
      if (! is_member(sym, result)) {
        if (result == NIL) {
          result = consa(sym);
          tail = result;
        }
        else {
          rplacd(tail, consa(sym));
          tail = cdr(tail);
        }
      }
    }
  }
  xlpop();
  
  return(result);
}

static LVAL get_initial_slot_value P2C(LVAL, object, LVAL, slot)
{
  LVAL entry = find_slot(object, slot);
  return((entry != NIL) ? cdr(entry) : NIL);
}

LOCAL VOID make_prototype P6C(LVAL, object, LVAL, name, LVAL, ivars, LVAL, cvars, LVAL, doc, int, set)
{
  LVAL slot;
  
  xlprot1(ivars);
  
  ivars = instance_slots(object, ivars);
  add_slot(object, s_instance_slots, ivars);
  add_slot(object, s_proto_name, name);
  
  for (; consp(ivars); ivars = cdr(ivars)) {
    slot = car(ivars);
    add_slot(object, slot, get_initial_slot_value(object, slot));
  }
  
  for (; consp(cvars); cvars = cdr(cvars)) 
    add_slot(object, car(cvars), NIL);
    
  if (doc != NIL && stringp(doc))
    add_documentation(object, s_proto, doc);
    
  if (set) setvalue(name, object);

  xlpop();
}

VOID xsaddinstanceslot P2C(LVAL, x, char *, s)
{
  LVAL sym = xlenter(s), ivars = slot_value(x, s_instance_slots);
  
  if (! is_member(sym, ivars)) {
    add_slot(x, sym, get_initial_slot_value(x, sym));
    set_slot_value(x, s_instance_slots, cons(sym, ivars));
  }
}

VOID xssetslotval P3C(LVAL, x, char *, s, LVAL, val)
{
  set_slot_value(x, xlenter(s), val);
}

LVAL xsdefproto(V)
{
  LVAL object, name, ivars, cvars, parents, doc;
  
  xlstkcheck(5);
  xlsave(object);
  xlsave(ivars);
  xlsave(cvars);
  xlsave(parents);
  xlsave(doc);
  
  name = xlgasymbol();
  ivars = (moreargs()) ? xleval(ivars = xlgetarg()) : NIL;
  cvars = (moreargs()) ? xleval(cvars = xlgetarg()) : NIL;
  parents = (moreargs()) ? xleval(parents = xlgetarg()) : NIL;
  doc = (moreargs()) ? xleval(doc = xlgetarg()) : NIL;
  
  if (! listp(parents)) parents = consa(parents);
  object = make_object(parents, NIL);
  make_prototype(object, name, ivars, cvars, doc, TRUE);
  
  xlpopn(5);
  return(name);
}

LVAL xsmakeproto(V)
{
  LVAL object, name, ivars;
  
  object = xlgaobject();
  name = xlgasymbol();
  ivars = (moreargs()) ? xlgetarg() : NIL;
  
  make_prototype(object, name, ivars, NIL, NIL, FALSE);
  
  return(object);
}

LVAL clanswer (V) { return(NIL); }
LVAL clmethod (V) { return(NIL); }
LVAL clisnew (V) { return(NIL); }
LVAL clnew (V) { return(NIL); }
VOID obsymbols (V) {}
LVAL obclass (V) { return(NIL); }
LVAL obshow (V) { return(NIL); }
LVAL obisnew (V) { return(NIL); }
LVAL xsend (V) { return(NIL); }
int xlobgetvalue P3C(LVAL, a, LVAL, b, LVAL *, c) { return(FALSE); }
int xlobsetvalue P3C(LVAL, a, LVAL, b, LVAL, c)   { return(FALSE); }
LVAL xsendsuper (V) { return(NIL); }
VOID xloinit (V) {}
LVAL obprin1(V) { return(NIL); }
