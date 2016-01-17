/* xsiview2 - XLISP interface to IVIEW dynamic graphics package.       */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"

/* external functions */
#ifdef USESTRINGS
extern double IViewStringValue(), IViewStringTransformedValue();
#endif /* USESTRINGS */
  
/* external variables */
extern LVAL s_true;
extern LVAL s_invisible, s_normal, s_hilited, s_selected;
extern LVAL s_solid, s_dashed;
extern LVAL sk_point_labels;
extern LVAL s_left, s_center, s_right, s_top, s_bottom;
extern LVAL sk_draw, sk_redraw, sk_redraw_content,sk_scale;

/* static global variables */
static IVIEW_WINDOW wind;
static int data_type, coordinate_type, info_type;

/**************************************************************************/
/**                                                                      **/
/**                    General IView Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

static LVAL number_of P1C(int, what)
{
  IVIEW_WINDOW w;
  int val = 0;
  
  w = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  xllastarg();
  
  switch(what) {
  case 'V': val = IViewNumVariables(w); break;
  case 'P': val = IViewNumPoints(w);    break;
  case 'L': val = IViewNumLines(w);     break;
#ifdef USESTRINGS
  case 'S': val = IViewNumStrings(w);   break;
#endif /* USESTRINGS */
  }
  
  return(cvfixnum((FIXTYPE) val));
}

LVAL iview_num_variables(V) { return(number_of('V')); }

static LVAL base_coordinate(V)
{
  int var, point, set = FALSE;
  double value = 0.0;
  LVAL result = NULL;
  
  var = getfixnum(xlgafixnum());
  point = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    switch (coordinate_type) {
    case 'V': value = makefloat(xlgetarg()); break;
    case 'S': xlfail("can't set screen coordinate directly");
    case 'T': xlfail("can't set transformed coordinate directly");
    default:  xlfail("unknown coordinate type");
    }
  }
  
  if (set)
    switch (data_type) {
    case 'P': IViewSetPointValue(wind, var, point, value);  break;
    case 'L': IViewSetLineValue(wind, var, point, value);   break;
#ifdef USESTRINGS
    case 'S': IViewSetStringValue(wind, var, point, value); break;
#endif /* USESTRINGS */
    }
  
  switch (data_type) {
  case 'P': 
    if (coordinate_type == 'V')
      result = cvflonum((FLOTYPE) IViewPointValue(wind, var, point));
    else if (coordinate_type == 'S')
      result = cvfixnum((FIXTYPE) IViewPointScreenValue(wind, var, point));
    else
      result = cvflonum((FLOTYPE) IViewPointTransformedValue(wind, var, point));
    break;
  case 'L':
    if (coordinate_type == 'V')
      result = cvflonum((FLOTYPE) IViewLineValue(wind, var, point));
    else if (coordinate_type == 'S')
      result = cvfixnum((FIXTYPE) IViewLineScreenValue(wind, var, point));
    else
      result = cvflonum((FLOTYPE) IViewLineTransformedValue(wind, var, point));
    break;
#ifdef USESTRINGS
  case 'S':
    if (coordinate_type == 'V')
      result = cvflonum((FLOTYPE) IViewStringValue(wind, var, point));
    else if (coordinate_type == 'S')
      result = cvfixnum((FIXTYPE) IViewStringScreenValue(wind, var, point));
    else
      result = cvflonum((FLOTYPE) IViewStringTransformedValue(wind, var, point));
    break;
#endif /* USESTRINGS */
  }
  return(result);
}

static LVAL coordinate(V)
{
  return(recursive_subr_map_elements(base_coordinate, coordinate));
}

static LVAL basic_data_coordinate P2C(int, type, int, action)
{
  wind = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  data_type = type;
  coordinate_type = action;
  return(coordinate());
}

static LVAL base_mask(V)
{
  int point, masked = 0, set = FALSE;
  
  point = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    masked = (xlgetarg() != NIL) ? TRUE : FALSE;
  }
  
  if (set)
    switch (data_type) {
    case 'P': IViewSetPointMask(wind, point, masked);  break;
    case 'L': IViewSetLineMask(wind, point, masked);   break;
#ifdef USESTRINGS
    case 'S': IViewSetStringMask(wind, point, masked); break;
#endif /* USESTRINGS */
    }
  
  switch (data_type) {
    case 'P': masked = IViewPointMasked(wind, point);  break;
    case 'L': masked = IViewLineMasked(wind, point);   break;
#ifdef USESTRINGS
    case 'S': masked = IViewStringMasked(wind, point); break;
#endif /* USESTRINGS */
  }
  return((masked) ? s_true : NIL);
}

static LVAL mask(V)
{
  return(recursive_subr_map_elements(base_mask, mask));
}

static LVAL basic_data_mask P1C(int, type)
{
  wind = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  data_type = type;
  return(mask());
}

static LVAL base_color(V)
{
  int point, color = 0, set = FALSE;
  LVAL arg;
  
  point = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    arg = xlgetarg();
    color = (arg != NIL) ? decode_lisp_color(arg) : NOCOLOR;
  }
  
  if (set)
    switch (data_type) {
    case 'P': IViewSetPointColor(wind, point, color);  break;
    case 'L': IViewSetLineColor(wind, point, color);   break;
#ifdef USESTRINGS
    case 'S': IViewSetStringColor(wind, point, color); break;
#endif /* USESTRINGS */
    }
  
  switch (data_type) {
    case 'P': color = IViewPointColor(wind, point);  break;
    case 'L': color = IViewLineColor(wind, point);   break;
#ifdef USESTRINGS
    case 'S': color = IViewStringColor(wind, point); break;
#endif /* USESTRINGS */
  }
  return((color != NOCOLOR) ? encode_lisp_color(color) : NIL);
}

static LVAL color(V)
{
  return(recursive_subr_map_elements(base_color, color));
}

static LVAL basic_data_color P1C(int, type)
{
  wind = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  data_type = type;
  return(color());
}

/**************************************************************************/
/**                                                                      **/
/**                      IView Point Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_num_points(V) { return(number_of('P')); }

LVAL iview_point_coordinate(V)        { return(basic_data_coordinate('P', 'V')); }
LVAL iview_point_screen_coordinate(V) { return(basic_data_coordinate('P', 'S')); }
LVAL iview_point_transformed_coordinate(V) { return(basic_data_coordinate('P', 'T')); }

LVAL iview_point_masked(V) { return(basic_data_mask('P')); }
LVAL iview_point_color(V) { return(basic_data_color('P')); }

static LVAL base_point_info(V)
{
  int point, marked = 0, sym, hsym, set = FALSE;
  char *label = NULL;
  PointState state = pointNormal;
  LVAL arg, result = NULL;
  
  /* get the arguments */
  point = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    switch(info_type) {
    case 'S':
    case 's':
      arg = xlgasymbol();
      if (arg == s_invisible) state = pointInvisible;
      else if (arg == s_normal) state = pointNormal;
      else if (arg == s_hilited) state = pointHilited;
      else if (arg == s_selected) state = pointSelected;
      else xlerror("unknown point state", arg);
      break;
    case 'M': marked = (xlgetarg() != NIL) ? TRUE : FALSE; break;
    case 'L': label = (char *) getstring(xlgastring());  break;
    case 'X':
      arg = xlgetarg();
      if (symbolp(arg)) decode_point_symbol(arg, &sym, &hsym);
      else {
        if (! fixp(arg)) xlbadtype(arg);
        sym = getfixnum(arg);
        hsym = getfixnum(xlgafixnum());
      }
      break;
    }
  }
  
  /* set the new state if value was supplied */
  if (set)
    switch (info_type) {
    case 'S': IViewSetPointState(wind, point, state);       break;
    case 's': IViewSetPointScreenState(wind, point, state); break;
    case 'M': IViewSetPointMark(wind, point, marked);       break;
    case 'L': IViewSetPointLabel(wind, point, label);       break;
    case 'X': IViewSetPointSymbol(wind, point, sym, hsym);  break;
    }
  
  /* get the current state */
  switch (info_type) {
  case 'S': state = IViewPointState(wind, point);           break;
  case 's': state = IViewPointScreenState(wind, point);     break;
  case 'M': marked = IViewPointMarked(wind, point);         break;
  case 'L': label = IViewPointLabel(wind, point);           break;
  case 'X': IViewGetPointSymbol(wind, point, &sym, &hsym);  break;
  }
  
  /* code the current state as a lisp object */
  switch (info_type) {
  case 'S':
  case 's':
    switch (state) {
    case pointInvisible: result = s_invisible; break;
    case pointNormal:    result = s_normal;    break;
    case pointHilited:   result = s_hilited;   break;
    case pointSelected:  result = s_selected;  break;
    default: xlfail("unknown point state");
    }
    break;
  case 'M': result = (marked) ? s_true : NIL; break;
  case 'L': 
    if (label == NULL) result =cvstring("");
    else result = cvstring(label);
    break;
  case 'X': result = encode_point_symbol(sym, hsym); break;
  }
  
  /* return the current state */
  return(result);
}

static LVAL point_info(V)
{
  return(recursive_subr_map_elements(base_point_info, point_info));
}

static LVAL internal_point_info P1C(int, type)
{
  wind = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  if (type == 'S' && xlargc > 1) IViewCheckLinks(wind);
  info_type = type;
  return(point_info());
}

LVAL iview_point_state(V)        { return(internal_point_info('S')); }
LVAL iview_point_screen_state(V) { return(internal_point_info('s')); }
LVAL iview_point_marked(V)       { return(internal_point_info('M')); }
LVAL iview_point_label(V)        { return(internal_point_info('L')); }
LVAL iview_point_symbol(V)       { return(internal_point_info('X')); }


/**************************************************************************/
/**                                                                      **/
/**                      IView Line Data Functions                       **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_num_lines(V) { return(number_of('L')); }

LVAL iview_line_coordinate(V)        { return(basic_data_coordinate('L', 'V')); }
LVAL iview_line_screen_coordinate(V) { return(basic_data_coordinate('L', 'S')); }
LVAL iview_line_transformed_coordinate(V) { return(basic_data_coordinate('L', 'T')); }

LVAL iview_line_masked(V) { return(basic_data_mask('L')); }
LVAL iview_line_color(V) { return(basic_data_color('L')); }

static LVAL base_line_info(V)
{
  int line, next = 0, type = 0, width, set = FALSE;
  LVAL arg, result = NULL;
  
  /* get the arguments */
  line = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    switch(info_type) {
    case 'N':
	  arg = xlgetarg();
	  next = (fixp(arg)) ? getfixnum(arg) : -1;
	  break;
    case 'T':
      arg = xlgasymbol();
      if (arg == s_solid) type = 0;
      else if (arg == s_dashed) type = 1;
      else xlerror("unknown line type", arg);
      break;
    case 'P':
      width = getfixnum(xlgafixnum());
    }
  }
  
  /* set the new state if value was supplied */
  if (set)
    switch (info_type) {
    case 'N': IViewSetNextLine(wind, line, next);   break;
    case 'T': IViewSetLineType(wind, line, type);   break;
    case 'P': IViewSetLineWidth(wind, line, width); break;
    }
  
  /* get the current state */
  switch (info_type) {
  case 'N': next = IViewNextLine(wind, line);                 break;
  case 'T': type = IViewLineType(wind, line);                 break;
  case 'P': IViewGetLineWidth(wind, line, &width); break;
  }
  
  /* code the current state as a lisp object */
  switch (info_type) {
  case 'N': result = (next >= 0) ? cvfixnum((FIXTYPE) next) : NIL; break;
  case 'T':
    if (type == 0) result = s_solid;
    else result = s_dashed;
    break;
  case 'P': result = cvfixnum((FIXTYPE) width); break;
  }
  
  /* return the current state */
  return(result);
}

static LVAL line_info(V)
{
  return(recursive_subr_map_elements(base_line_info, line_info));
}

static LVAL internal_line_info P1C(int, type)
{
  wind = (IVIEW_WINDOW) get_iview_address(xlgaobject());
  info_type = type;
  return(line_info());
}

LVAL iview_line_next(V)  { return(internal_line_info('N')); }
LVAL iview_line_type(V)  { return(internal_line_info('T')); }
LVAL iview_line_width(V) { return(internal_line_info('P')); }

#ifdef USESTRINGS
/**************************************************************************/
/**                                                                      **/
/**                     IView String Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_num_strings(V) { return(number_of('S')); }

LVAL iview_string_coordinate(V)        { return(basic_data_coordinate('S', 'V')); }
LVAL iview_string_screen_coordinate(V) { return(basic_data_coordinate('S', 'S')); }
LVAL iview_string_transformed_coordinate(V) { return(basic_data_coordinate('S', 'T')); }

LVAL iview_string_masked(V) { return(basic_data_mask('S')); }
LVAL iview_string_color(V) { return(basic_data_color('S')); }

static LVAL base_string_modifiers(V)
{
  int string, up, h, v, set = FALSE;
  LVAL arg, temp, result;
  
  /* get the arguments */
  string = getfixnum(xlgafixnum());
  if (moreargs()) {
    set = TRUE;
    up = (xlgetarg() != NIL) ? TRUE : FALSE;
    arg = xlgasymbol();
    if (arg == s_left) h = 0;
    else if (arg == s_center) h = 1;
    else if (arg == s_right) h = 2;
    else xlerror("unknown string justification mode", arg);
    arg = xlgasymbol();
    if (arg == s_bottom) v = 0;
    else if (arg == s_top) v = 1;
    else xlerror("unknown string justification mode", arg);
  }
  
  /* set the new state if value was supplied */
  if (set) IViewSetStringModifiers(wind, string, up, h, v);
    
  /* get the current state */
  IViewGetStringModifiers(wind, string, &up, &h, &v);
  
  /* code the current state as a lisp object */
  xlsave1(result);
  switch (v) {
  case 0: temp = s_bottom; break;
  case 1: temp = s_top; break;
  default: xlfail("unknown string justification mode");
  }
  result = consa(temp);
  switch(h) {
  case 0: temp = s_left; break;
  case 1: temp = s_center; break;
  case 2: temp = s_right; break;
  default: xlfail("unknown string justification mode");
  }
  result = cons(temp, result);
  temp = (up) ? s_true : NIL;
  result = cons(temp, result);
  xlpop();
  
  /* return the current state */
  return(result);
}

static LVAL string_modifiers(V)
{
  return(recursive_subr_map_elements(base_string_modifiers, string_modifiers));
}

static LVAL internal_string_modifiers(V)
{
  wind = get_iview_address(xlgaobject());
  return(string_modifiers());
}

LVAL iview_string_modifiers(V) { return(internal_string_modifiers()); }
#endif /* USESTRINGS */
