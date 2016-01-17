/* edit - Macintosh editing functions                                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"
#include "TransEdit1.h"

/* external variables */
extern LVAL sk_show, sk_allocate, s_istream, s_ostream, s_input_stream,
  s_input_enabled, s_hardware_address, sk_go_away, s_go_away, sk_dispose,
  s_title, s_bind_to_file;
extern WindowPtr ttyWind;
extern int hasAppleEvents;

#define dfltLeft 10
#define dfltTop 40
#define dfltWidth 490
#define dfltHeight 280

# define check_edit_window_address(d) valid_edit_window_address(slot_value(d, s_hardware_address))

/* forward declarations */
LOCAL int match(char *, char *, long);

/***********************************************************************/
/**                                                                   **/
/**                          Utilitiy Functions                       **/
/**                                                                   **/
/***********************************************************************/

LVAL xssetvolume(void)
{
  DirSpec newdir, olddir;
  LVAL val;
  
  GetDir(&olddir);
  if (moreargs()) {
    LVAL arg = xlgalist();
    if (! fixp(car(arg)) || ! fixp(cdr(arg)))
      xlbadtype(arg);
    newdir.vRefNum = getfixnum(car(arg));
    newdir.parID = getfixnum(cdr(arg));
  }
  else {
    newdir = default_dir;
  }
  xllastarg();
  
  SetDir(&newdir);
  xlsave1(val);
  val = consd(cvfixnum((FIXTYPE) olddir.parID));
  rplaca(val, cvfixnum((FIXTYPE) olddir.vRefNum));
  xlpop();
  return val;
}

LVAL xsopenfiledialog(void)
{
  SFTypeList myTypes;
  Point p;
  short ntypes;
  LVAL result;
  Boolean setvol = TRUE;
  LVAL arg, next;;
  
  myTypes[0]='TEXT';
  ntypes = 1;
  
  if (moreargs()) {
    setvol = (xlgetarg() != NIL) ? TRUE : FALSE;
    if (moreargs()) {
      arg = xlgetarg();
      if (null(arg))
        ntypes = -1;
      else if (stringp(arg))
        myTypes[0] = string_to_type(getstring(arg));
      else if (consp(arg))
        for (ntypes = 0, next = arg; consp(next) && ntypes < 4; next = cdr(next)) {
          if (! stringp(car(next)))
            xlbadtype(arg);
          myTypes[ntypes] = string_to_type(getstring(car(next)));
          ntypes++;
        }
      else
        xlbadtype(arg);
    }       
  }
  xllastarg();
	
  NotifyIfInBackground();
  if (hasAppleEvents) {
    StandardFileReply reply;
    
    StandardGetFile(nil, ntypes, myTypes, &reply);
    if (reply.sfGood) {
      PtoCstr((StringPtr) reply.sfFile.name);
      result = cvstring((char *) &reply.sfFile.name);
      if (setvol) {
        DirSpec dir;
        dir.vRefNum = reply.sfFile.vRefNum;
        dir.parID = reply.sfFile.parID;
        SetDir(&dir);
      }
    }
    else result = NIL;
  }
  else {
    SFReply reply;

    SetPt(&p, 82, 90);
    SFGetFile( p, "\p", 0L, ntypes, myTypes, 0L, &reply );
    if (reply.good) {
      PtoCstr((StringPtr) &reply.fName);
      result = cvstring((char *) &reply.fName);
      if (setvol) SetVol(nil, reply.vRefNum);
    }
    else result = NIL;
  }
  return(result);
}

LVAL xssetfiledialog(void)
{
  LVAL result;
  char *prompt, *dflt = "";
  Str255 pprompt, pdflt;
  Boolean setvol = TRUE;
  
  prompt = (char *) getstring(xlgastring());
  if (moreargs())
    dflt = (char *) getstring(xlgastring());
  if (moreargs())
    setvol = (xlgetarg() != NIL) ? TRUE : FALSE;
  xllastarg();
  
  NotifyIfInBackground();
  CintoPstring(prompt, pprompt, sizeof pprompt, FALSE);
  CintoPstring(dflt, pdflt, sizeof pdflt, FALSE);
  if (hasAppleEvents) {
    StandardFileReply reply;
    
    StandardPutFile(pprompt, pdflt, &reply);
    if (reply.sfGood) {
      PtoCstr((StringPtr) reply.sfFile.name);
      result = cvstring((char *) &reply.sfFile.name);
      if (setvol) {
        DirSpec dir;
        dir.vRefNum = reply.sfFile.vRefNum;
        dir.parID = reply.sfFile.parID;
        SetDir(&dir);
      }
    }
    else result = NIL;
  }
  else {
    SFReply reply;
    Point p;

    SetPt(&p, 82, 90);
    SFPutFile(p, pprompt, pdflt, nil, &reply);
    if (reply.good) {
      PtoCstr((StringPtr) &reply.fName);
      result = cvstring((char *) &reply.fName);
      if (setvol) SetVol(nil, reply.vRefNum);
    }
    else result = NIL;
  }
	
  return(result);
}

LVAL xsfrontwindow(void) { return(get_hardware_object_by_address((CPTR) FrontWindow())); }

LVAL xshidefrontwindow(void)
{
	WindowPeek wPeek;
	
	xllastarg();
	
	if ((wPeek = (WindowPeek) FrontWindow()) != nil) {
		if (wPeek->windowKind < 0)
	    	CloseDeskAcc(wPeek->windowKind);
	    else
	      	HideWindow(FrontWindow());
	}
	return(NIL);
}

LVAL xssystem_edit(void)
{
  int item;
  
  item = getfixnum(xlgafixnum());
  xllastarg();
  
  return((SystemEdit(item)) ? s_true : NIL);
}

/***********************************************************************/
/**                                                                   **/
/**                          Listener Methods                         **/
/**                                                                   **/
/***********************************************************************/

LVAL xslistener_isnew(void)
{
  LVAL object = xlgaobject();
  
  if (xsboolkey(sk_show, TRUE)) send_message(object, sk_allocate);
  if (slot_value(object, s_istream) == NIL)
    set_slot_value(object, s_istream, getvalue(s_input_stream));
  if (slot_value(object, s_ostream) == NIL)
    set_slot_value(object, s_ostream, newustream());
  set_slot_value(object, s_input_enabled, s_true);
  return(object);
}

LVAL xslistener_allocate(void)
{
  LVAL object = xlgaobject();
  WindowData data;
  
  if (get_window_data(ttyWind) == nil) {
    data = (WindowData) StCalloc(sizeof(struct window_data), 1);
    set_window_data(ttyWind, data);
  }
  if (slot_value(object, s_hardware_address) != NIL)
    standard_hardware_clobber(object);
  set_edit_window_address((CPTR) ttyWind, object);
  set_window_object(ttyWind, object);
  return(object);
}

static LVAL clip_stream(void)
{
  LVAL stream;
  int numChar, count;
  char **scrapH, ch;

  TEFromScrap ();
  numChar = TEGetScrapLength();
  scrapH = TEScrapHandle();

  xlsave1(stream);

  stream = newustream();

  for (count = 0; count < numChar; count++) {
    ch = (*scrapH)[count];
    if (ch == '\r') ch = '\n';
    xlputc(stream, ch);
  }
  
  xlpop();	
  return(stream);
}

/***********************************************************************/
/**                                                                   **/
/**                         Edit Window Methods                       **/
/**                                                                   **/
/***********************************************************************/

LVAL xsedit_window_isnew(void)
{
  LVAL object, value;
  
  object = xlgaobject();
  object_isnew(object);
    
  if (slot_value(object, s_ostream) == NIL)
    set_slot_value(object, s_ostream, newustream());
  set_slot_value(object, s_input_enabled, s_true);
  if (! xlgetkeyarg(sk_go_away, &value)) value = s_true;
  set_slot_value(object, s_go_away, value);
  
  if (xsboolkey(sk_show, TRUE)) send_message(object, sk_allocate);
  return(object);
}

LVAL xsedit_window_allocate(void)
{
  LVAL object, value;
  Rect bounds;
  char *title;
  int visible, goAway, bindToFile;
  WindowPtr behind;
  long refNum;
  WindowPtr w;
  WindowData data;
  int left, top, width, height;
  Str255 ptitle;
  object = xlgaobject();
  if (check_edit_window_address(object))
    send_message(object, sk_dispose);

  left = dfltLeft;
  top = dfltTop;
  width = dfltWidth;
  height = dfltHeight;
  get_window_bounds(object, &left, &top, &width, &height);
  SetRect(&bounds, left, top, left + width, top + height);

  value = slot_value(object, s_title);
  title = stringp(value) ? (char *) getstring(value) : "";
  if (strlen(title) == 0) title = nil;
  goAway = slot_value(object, s_go_away) != NIL;
  visible = TRUE;
  behind = (WindowPtr) -1;
  refNum = (long) nil;
  bindToFile = (slot_value(object, s_bind_to_file) != NIL) ? TRUE : FALSE;
  
  if (title != nil)
    CintoPstring(title, ptitle, sizeof ptitle, FALSE);

  w = NewEWindow (&bounds, (StringPtr) (title == nil ? nil : ptitle), visible, behind, 
                  goAway, refNum, bindToFile);
  if (w == NULL) return(NIL);
  else {
    SetEWindowStyle (w, editFontNum, editFontSize, editFontStyle, 0, teJustLeft);
    set_edit_window_procs(w);
    set_edit_window_address((CPTR) w, object);
    data = (WindowData) StCalloc(sizeof(struct window_data), 1);
    set_window_data(w, data);
    set_window_object(w, object);
    return(object);
  }
}

static LVAL edit_window_edit(int which)
{
  WindowPtr w;

  w = (WindowPtr) get_edit_window_address(xlgetarg());
  xllastarg();
   
  if (IsEWindow(w) && w == FrontWindow()) {
    if (which == 3 || which == 5) adjust_insert(w);
    EWindowEditOp(which);
    if (w == ttyWind && which == 5) return_action(GetEWindowTE(w));
    if (w == ttyWind) SetEWindowDirty(w, FALSE);
  }
  return(NIL);
}

LVAL xsedit_window_cut(void)   { return(edit_window_edit(3)); }
LVAL xsedit_window_copy(void)  { return(edit_window_edit(4)); }
LVAL xsedit_window_paste(void) { return(edit_window_edit(5)); }

static LVAL ewindow_action(int action)
{
  WindowPtr w;
  int result;

  w = (WindowPtr) get_edit_window_address(xlgetarg());
  xllastarg();
  if (w == ttyWind) return(NIL);
  
  switch (action) {
  case 'R': result = EWindowRevert(w); break;
  case 'S': result = EWindowSave(w); break;
  case 'A': result = EWindowSaveAs(w); break;
  case 'C': result = EWindowSaveCopy(w); break;
  }
  return((result) ? s_true : NIL);
}

LVAL xsedit_window_revert(void)    { return(ewindow_action('R')); }
LVAL xsedit_window_save(void)      { return(ewindow_action('S')); }
LVAL xsedit_window_save_as(void)   { return(ewindow_action('A')); }
LVAL xsedit_window_save_copy(void) { return(ewindow_action('C')); }

LVAL xsedit_window_selection_stream(void)
{
  WindowPtr w;
  TEHandle te;
	
  w = (moreargs()) ? (WindowPtr) get_edit_window_address(xlgetarg()) : nil;
  xllastarg();

  if (w != nil) {
    te = GetEWindowTE(w);
    if (te == nil) xlfail("not an edit window");
    TECopy (te);
    ZeroScrap ();
    TEToScrap ();
  }
  return(clip_stream());
}

LVAL xsedit_window_remove(void)
{
  WindowPtr w;
  LVAL object;
  int result;
  WindowData data;
  
  object = xlgaobject();
  if (check_edit_window_address(object)) {
    w = (WindowPtr) get_edit_window_address(object);
    data = (WindowData) get_window_data(w);
    if (w == ttyWind) result = FALSE;
    else result = (! IsEWindow(w) || EWindowClose(w));
    if (result) StFree(data);
  }
  else result = TRUE;
  if (result) standard_hardware_clobber(object);
  return((result) ? s_true : NIL);
}

LVAL xsedit_window_find(void)
{
  WindowPtr w;
  int result = FALSE;
  char **ptext, *text, *s;
  long i, n, len, start;
  TEHandle te;
  
  w = (WindowPtr) get_edit_window_address(xlgaobject());
  s = (char *) getstring(xlgastring());

  if (IsEWindow(w) && (te = GetEWindowTE(w)) != nil) {
    n = (*te)->teLength;
    start = (*te)->selEnd;
    ptext = (char **) TEGetText(te);
    len = strlen(s);
    for (i = start; i < n - len; i++) {
      text = *ptext + i;
      if (match(s, text, len)) {
        TESetSelect (i, i + len, te);
        result = TRUE;
        break;
      }
    }
  }
  if (result) EWindowOverhaul(w, TRUE, FALSE, FALSE);
  return(result ? s_true : NIL);
}

LOCAL int match(char *s1, char *s2, long n)
{
  while (n-- > 0)
    if (toupper(*s1++) != toupper(*s2++)) return(FALSE);
  return(TRUE);
}
