/* macwindows - Macintosh window functions                             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

# define screenBits qd.screenBits
#ifdef applec
VOID CtoPstr(s) char *s; {c2pstr(s);}
VOID PtoCstr(s) StringPtr s; {p2cstr(s);}
#endif /* applec */

/* external variables */
extern LVAL s_true, sk_update, sk_close, sk_activate;
extern int color_allowed;

/**************************************************************************/
/**                                                                      **/
/**                           Utility Functions                          **/
/**                                                                      **/
/**************************************************************************/

static GrafPtr current_port(void)
{
  GrafPtr port;
  
  GetPort(&port);
  return(port);
}

/**************************************************************************/
/**                                                                      **/
/**                        Window Data Functions                         **/
/**                                                                      **/
/**************************************************************************/

LVAL get_window_object(WindowPtr w)
{
  WindowData data;
  
  data = (WindowData) get_window_data(w);
  if (data == nil || ! objectp((LVAL) data->object)) return(NIL);
  else return((LVAL) data->object);
}

VOID set_window_object(WindowPtr w, LVAL object)
{
  WindowData data;
  
  data = (WindowData) get_window_data(w);
  if (data == nil) return;
  else data->object = (char *) object;
}

/**************************************************************************/
/**                                                                      **/
/**                       Standard Event Handlers                        **/
/**                                                                      **/
/**************************************************************************/

pascal VOID mac_update_action(Boolean resized)
{
  LVAL object = get_window_object(current_port());

  if (! objectp(object)) return;
  send_message_1L(object, sk_update, (resized) ? s_true : NIL);
}

pascal VOID mac_activate_action(Boolean active)
{
  LVAL object = get_window_object(current_port());
  
  if (! objectp(object)) return;
  send_message_1L(object, sk_activate, (active) ? s_true : NIL);
}

pascal VOID mac_close_action(void)
{
  LVAL object = get_window_object(current_port());
  
  if (! objectp(object)) return;
  send_message(object, sk_close);
}

/**************************************************************************/
/**                                                                      **/
/**                 General Window Information Functions                 **/
/**                                                                      **/
/**************************************************************************/

VOID StShowWindow(WindowPtr w)
{
  MyShowWindow(w);
}

VOID StHideWindow(WindowPtr w)
{
  HideWindow(w);
}

VOID StWSetTitle(WindowPtr w, char *str)
{
  Str255 pbuf;
  CintoPstring(str, pbuf, sizeof pbuf, FALSE);
  SetWTitle(w, pbuf);
}

/**************************************************************************/
/**                                                                      **/
/**                         Screen Info Functions                        **/
/**                                                                      **/
/**************************************************************************/

VOID StGetScreenSize(int *width, int *height)
{
  if (width != nil) *width = screenBits.bounds.right - screenBits.bounds.left;
  if (height != nil) *height = screenBits.bounds.bottom - screenBits.bounds.top - GetMBarHeight();
}

int StScreenHasColor(void)
{
  SysEnvRec r;
  GDHandle gd;
  static inited = FALSE, has_color = FALSE;

  if (! inited) {
    SysEnvirons(1, &r);
    if ((int) r.hasColorQD && color_allowed) {
      gd = GetGDevice();
      has_color = ((*(*gd)->gdPMap)->pixelSize > 1) ? TRUE : FALSE;
    }
    else has_color = FALSE;
    inited = TRUE;
  }
  return(has_color);
}

int StHasWindows(void) { return(TRUE); }
VOID StFlushGraphics(void) {}
