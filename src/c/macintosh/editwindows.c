/* edit - Macintosh editing functions                                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"
#include "TransEdit1.h"

# define MIN(x,y) (((x) < (y)) ? (x) : (y))

# define BUFMAX STRMAX
# define MAXCOUNT 10000L
# define KEEPCOUNT 5000L

extern WindowPtr ttyWind;
extern LVAL s_true, sk_remove, s_input_stream, s_input_enabled;
static char ttybuf[BUFMAX];
static int ttybufcount = 0, ttyfixcount = 0;

# define enter 3
# ifndef shiftKey
# define shiftKey 0x0200
# endif

/* forward declarations */
LOCAL int paste_stream(WindowPtr w, LVAL stream, int input);
LOCAL VOID flush_window(WindowPtr w, long byteCount);
LOCAL VOID adjust_fixed_count(WindowPtr w);
LOCAL int input_enabled(WindowPtr w);
LOCAL VOID tty_return(void);
LOCAL int GetInputLine(void);
LOCAL int check_parens(unsigned char *s, int n);
LOCAL VOID pardelay(void);
LOCAL VOID do_tab(TEHandle teEdit, int start);
LOCAL VOID fix_blanks(TEHandle teEdit, int nblanks, int curline);
LOCAL int at_text_end(TEHandle teEdit);
LOCAL int last_is_return(TEHandle teEdit);
LOCAL int num_to_skip(unsigned char *s, int n);
LOCAL int is_special(unsigned char *s, int n);
LOCAL pascal VOID close_listener(void); 
LOCAL pascal VOID tty_key(void);
LOCAL pascal VOID edit_idle(void);
LOCAL pascal VOID close_edit(void);
LOCAL VOID AdjusrDisplay(WindowPtr w);

LOCAL VOID AdjustDisplay(WindowPtr w)
{
  GrafPtr port;
  GetPort(&port);
  EWindowAdjustDisplay (w);
  SetPort(port);
}

static LVAL get_output_stream(WindowPtr w)
{
  LVAL object = get_window_object(w), s_output_stream = xlenter("OUTPUT-STREAM");
  LVAL stream = NIL;

  if (objectp(object)) lex_slot_value(object, s_output_stream, &stream);
  return(ustreamp(stream) ? stream :NIL);
}
  
static LVAL get_input_stream(void)
{
  if (ttyWind == nil) return(NIL);
  else return(getvalue(s_input_stream));
}
  
static VOID flush_output_stream(WindowPtr w)
{
  LVAL stream = get_output_stream(w);
  int ch;
  TEHandle te = GetEWindowTE(w);
  
  if (w == ttyWind) TtyFlush();
  if (stream != NIL && (ch = xlgetc(stream)) != EOF) {
    xlungetc(stream, ch);
    paste_stream(w, stream, FALSE);
    if ((w == ttyWind || ttyWind == nil) && (*te)->teLength > MAXCOUNT)
      flush_window(w, (*te)->teLength - KEEPCOUNT);
    adjust_fixed_count(w);
  }
}

LOCAL VOID flush_window(WindowPtr w, long byteCount)
{
  TEHandle te = GetEWindowTE(w);
  
  TESetSelect (0L, byteCount, te);          /* select text */
  TEDelete (te);                            /* clobber it */
  TESetSelect((*te)->teLength, (*te)->teLength, te);
  adjust_fixed_count(w);
  AdjustDisplay(w);
}

static VOID InsertText(char *buf, long count, TEHandle te)
{
  if ((*te)->teLength + count > 32000) xlfail("Buffer is too big");
  else TEInsert(buf, count, te);
}

static get_fixed_count(WindowPtr w, int *count)
{
  if (w == ttyWind || ttyWind == nil) {
    if (count != nil) *count = ttyfixcount;
    return(TRUE);
  }
  else return(FALSE);
}

static VOID set_fixed_count(WindowPtr w, int count)
{
  if (w == ttyWind || ttyWind == nil) ttyfixcount = count;
}

VOID adjust_insert(WindowPtr w)
{
  int count;
  TEHandle te = GetEWindowTE(w);
  
  flush_output_stream(w);
  if (get_fixed_count(w, &count) && (*te)->selStart < count) {
    TESetSelect((*te)->teLength, (*te)->teLength, te);
  }
}

LOCAL VOID adjust_fixed_count(WindowPtr w)
{
  int count;
  TEHandle te = GetEWindowTE(w);

  if (get_fixed_count(w, &count)) {
    set_fixed_count(w, (*te)->teLength);
  }
  if (get_fixed_count(w, &count) && (*te)->selStart < count) {
    TESetSelect((*te)->teLength, (*te)->teLength, te);
  }
}

LOCAL int paste_stream(WindowPtr w, LVAL stream, int input)
{
  int buffpos = 0, ch;

  if (! IsEWindow(w) || stream == NIL) return(FALSE);
  
  while((ch = xlgetc(stream)) != EOF) {
    if (ch == '\n') ch = RETURNCHAR;
    buf[buffpos++] = ch;
    if (buffpos > BUFMAX) {
      InsertText(buf, (long) buffpos, GetEWindowTE(w));
      buffpos = 0;
    }
  }
  InsertText(buf, (long) buffpos, GetEWindowTE(w));
  AdjustDisplay(w);
  if (input && w == ttyWind) return_action(GetEWindowTE(w));
  if (! input_enabled(w) || w == ttyWind) SetEWindowDirty(w, FALSE);
  return(TRUE);
}

static paste_string(WindowPtr w, char *str, int input)
{
  int buffpos = 0, ch;
  long len;
  
  if (! IsEWindow(w) || str == nil) return(FALSE);

  len = strlen(str);
  while(len -- > 0) {
    ch = *str++;
    if (ch == '\n') ch = RETURNCHAR;
    buf[buffpos++] = ch;
    if (buffpos > BUFMAX) {
      InsertText(buf, (long) buffpos, GetEWindowTE(w));
      buffpos = 0;
    }
  }
  InsertText(buf, (long) buffpos, GetEWindowTE(w));
  AdjustDisplay(w);
  if (input && w == ttyWind) return_action(GetEWindowTE(w));
  return(TRUE);
  AdjustDisplay(w);
  if (input && w == ttyWind) return_action(GetEWindowTE(w));
  if (! input_enabled(w) || w == ttyWind) SetEWindowDirty(w, FALSE);
  return(TRUE);
}

LOCAL int input_enabled(WindowPtr w)
{
  LVAL enabled = s_true, object = get_window_object(w);
  
  if (w == ttyWind) return(TRUE);
  if (objectp(object)) lex_slot_value(object, s_input_enabled, &enabled);
  return((enabled != NIL));
}

static VOID tty_enter(void)
{
  TEHandle te = GetEWindowTE(ttyWind);

  if (te == nil) return;
  adjust_insert(ttyWind);
  if ((*te)->selStart < (*te)->teLength)
    TESetSelect((*te)->teLength, (*te)->teLength, te);
  else tty_return();
}

LOCAL VOID tty_return(void)
{
  TEHandle te = GetEWindowTE(ttyWind);

  if (te == nil) return;
  adjust_insert(ttyWind);
  TEKey (RETURNCHAR, te);
  AdjustDisplay(ttyWind);
  return_action(te);
}

VOID TtyPutc(int c)
{
  if (c == '\n') c = RETURNCHAR;
  ttybuf[ttybufcount++] = c;
  if (c == RETURNCHAR || ttybufcount >= (BUFMAX - 1)) TtyFlush();
}

VOID TtyPrint(char *s)
{
  while (strlen(s) > 0) TtyPutc(*s++);
  TtyFlush();
}

VOID TtyFlush(void)
{
  TEHandle te = GetEWindowTE(ttyWind);
  int count;
  
  if (ttybufcount > 0) {
    if (get_fixed_count(ttyWind, &count) && (*te)->selStart < count) {
      TESetSelect((*te)->teLength, (*te)->teLength, te);
    }
    adjust_fixed_count(ttyWind);
    InsertText(ttybuf, (long) ttybufcount, GetEWindowTE(ttyWind));
    ttybufcount = 0;
    if ((*te)->teLength > MAXCOUNT) {
      flush_window(ttyWind, (*te)->teLength - KEEPCOUNT);
    }
    TESetSelect((*te)->teLength, (*te)->teLength, te);
    adjust_fixed_count(ttyWind);
    AdjustDisplay(ttyWind);
  }
}

LOCAL int GetInputLine(void)
{
  int i, has_fixed, count;
  unsigned char **mytext;
  TEHandle te = GetEWindowTE(ttyWind);
  LVAL stream = get_input_stream();
  
  mytext = (unsigned char **)TEGetText(te);
  has_fixed = get_fixed_count(ttyWind, &count);
  if (! has_fixed) count = 0;
  if (ustreamp(stream) && check_parens(*mytext + count, (*te)->teLength - count)) {
    for (i = count; i < (*te)->teLength; i++)
      xlputc(stream, (*mytext)[i]);
    if (has_fixed) adjust_fixed_count(ttyWind);
    return(TRUE);
  }
  else return(FALSE);
}

LOCAL int check_parens(unsigned char *s, int n)
{
  int parcount = 0, inquotes = FALSE, incomment = FALSE;
  char ch;
   
  while (n-- > 0) {
    ch = *s++;
    switch (ch) {
    case  '"': inquotes = ! inquotes; break;
    case  ';': if (! inquotes) incomment = TRUE; break;
    case '\r':
    case '\n': incomment = FALSE; break;
    case  '(': if (! inquotes && ! incomment) parcount++; break;
    case  ')': if (! inquotes && ! incomment) parcount--; break;
    }
  }
  return (parcount <= 0);
}

LVAL xsedit_window_paste_stream(void)
{
  LVAL stream;
  WindowPtr w;

  w = (WindowPtr) get_edit_window_address(xlgetarg());
  stream = xlgetfile(FALSE);
  xllastarg();

  adjust_insert(w);
  return((paste_stream(w, stream, TRUE)) ? s_true : NIL);
}

LVAL xsedit_window_paste_string(void)
{
  LVAL string;
  WindowPtr w;

  w = (WindowPtr) get_edit_window_address(xlgetarg());
  string = xlgastring();
  xllastarg();

  adjust_insert(w);
  return((paste_string(w, getstring(string), TRUE)) ? s_true : NIL);
}

LVAL xsedit_window_flush_window(void)
{
  WindowPtr w;
  long count;
  
  w = (WindowPtr) get_edit_window_address(xlgetarg());
  count = (moreargs()) ? getfixnum(xlgafixnum()) : 32767;
  xllastarg();

  flush_window(w, count);
  return(NIL);
}

static flash_matching_paren(TEHandle teEdit, int start)
{
  int parcount = 0, inquotes = FALSE, sel, par;
  unsigned char ch, *s;
  
  sel = (*teEdit)->selStart;
  s = *((unsigned char **)TEGetText(teEdit)) + sel - 1;
  par = sel;
  do {
    par--;
    ch = *s--;
    switch (ch) {
    case  '"': inquotes = ! inquotes; break;
    case  '(': if (! inquotes) parcount--; break;
    case  ')': if (! inquotes) parcount++; break;
    }
  } while (par > start && parcount > 0);
  if (ch == '(' && parcount == 0) {
    TESetSelect(par, par + 1, teEdit);
	pardelay();
    TESetSelect(sel, sel, teEdit);
  }
  return (parcount <= 0);
}

LOCAL VOID pardelay(void)
{
  unsigned long t = 5, f;
  Delay(t, &f);
}

LOCAL VOID do_tab(TEHandle teEdit, int start)
{
  int sel, curline, lastline, pos, nblanks, inquote, parcount;
  unsigned char *s, ch;
  
  /* for an edit window get rid of the inserted tab 
  if (teEdit != TTY.teEdit) TEKey('\b', teEdit);*/
      
  sel = (*teEdit)->selStart;
  s = *((unsigned char **)TEGetText(teEdit));

  /* find beginning of the line */
  curline = sel;
  while (curline > start && s[curline - 1] != RETURNCHAR) curline--;
  if (curline == start) return;

  /* find unmatched paren */
  parcount = 0;
  inquote = FALSE;
  pos = curline;
  while (parcount >= 0 && --pos >= start) {
    ch = s[pos];
    switch (ch) {
    case ')': if (! inquote) parcount++; break;
    case '(': if (! inquote) parcount--; break;
    case '"': inquote = ! inquote; break;
    }
  }
  if (parcount == 0) return;
  
  /* find beginning of the previous line */
  lastline = pos;
  while (lastline > 0 && s[lastline - 1] != RETURNCHAR) lastline--;

  /* skip forward an s-expression or to first non blank */
  pos += num_to_skip(s + pos, curline - pos);

  if (pos > curline) pos = curline;
  nblanks = pos - lastline;

  /* adjust for the number of blanks already present, replace tabs by blanks */
  for (pos = curline; 
       pos < (*teEdit)->teLength && (s[pos] == ' ' || s[pos] == '\t');
       nblanks--, pos++)
    if (s[pos] == '\t') s[pos] = ' ';
  
  /* insert or delete the appropriate number of blanks */
  if (nblanks == 0) return;
  
  sel += nblanks;
  if (pos > (*teEdit)->teLength) pos = (*teEdit)->teLength;
  TESetSelect(pos, pos, teEdit);
  fix_blanks(teEdit, nblanks, curline);
  if (sel > (*teEdit)->teLength) sel = (*teEdit)->teLength;
  TESetSelect(sel, sel, teEdit);
}

LOCAL VOID fix_blanks(TEHandle teEdit, int nblanks, int curline)
{
  int i;
  
  if (nblanks > 0) {
    for (i = 0; i < nblanks; i++) buf[i] = ' ';
    TESetSelect(curline, curline, teEdit);
    TEInsert(buf, (long) nblanks, teEdit);
  }
  else {
    TESetSelect(curline, curline - nblanks, teEdit);
    TECut(teEdit);
  }
}

LOCAL int at_text_end(TEHandle teEdit)
{
  int i, result = TRUE;
  unsigned char *s = *((unsigned char **)TEGetText(teEdit));
  
  for (i = (*teEdit)->selStart; result && i < (*teEdit)->teLength; i++)
    if (! isspace(s[i])) result = FALSE;
  return(result);
}

LOCAL int last_is_return(TEHandle teEdit)
{
  int i;
  unsigned char *s = *((unsigned char **)TEGetText(teEdit));
  
  for (i = (*teEdit)->selStart - 1; i >= 0 && isspace(s[i]); i--)
    ;
  i = MIN((*teEdit)->selStart - 1, i + 1);
  return(s[i] == RETURNCHAR);
}

VOID return_action(TEHandle te)
{	
  if (at_text_end(te) && last_is_return(te) && GetInputLine()) {
	flush_output_stream(ttyWind);
    getttyline(get_input_stream());
  }
}

LOCAL int num_to_skip(unsigned char *s, int n)
{
  unsigned char str[4];
  int i, pos, oldpos;
  
  pos = 0;
  
  if (n <= 0) pos = 0;
  else if (*s == '(') {
  
    s++; n--; pos = 1;
    
    /* skip blanks */
    while (n > 0 && (*s == ' ' || *s == '\t')) { s++; n--; pos++; }
    
    /* check for end of line or list or lisp comment*/
    if (n > 0 && *s != RETURNCHAR && *s != ';' && *s != '(') {
    
      /* check for special symbols */
      for (i = 0; i < 3 && i < n; i++)
        str[i] = toupper(s[i]);
      str[i] = '\0';
      if (is_special(s, n) /* strcmp(str, "DEF") == 0 || strcmp(str, "LET") == 0 
          || strcmp(str, "FLE") == 0 */ )
        pos = 2;
      else {
        
        /* skip over the s-expression */
        oldpos = pos;
        while (n > 0 && *s != ' ' && *s != '\t' && *s != RETURNCHAR)
          { s++; n--; pos++; }
          
        /* check for another s expression */
        for (i = 0; n > 0 && (*s == ' ' || *s == '\t'); s++, n--, i++) ;
        if (n == 0 || *s == RETURNCHAR)
		  pos = (oldpos == pos) ? oldpos + 1 : oldpos;
        else pos += i;
      }
    }
  }
  else {
    
    /* skip over any blanks */
    for (i = 0; n > 0 && (*s == ' ' || *s == '\t'); s++, n--, i++) ;
    if (n > 0 && *s != RETURNCHAR) pos += i;
  }
  return(pos);
}

LOCAL int is_special(unsigned char *s, int n)
{
  char str[10];
  int i;
  
  for (i = 0; i < n && i < 9; i++) str[i] = toupper(s[i]);  
  str[i] = '\0';

  if (n >= 5 && strncmp(str, "DEFUN", 5) == 0) return(TRUE);
  if (n >= 8 && strncmp(str, "DEFMACRO", 8) == 0) return(TRUE);
  if (n >= 7 && strncmp(str, "DEFMETH", 7) == 0) return(TRUE);
  if (n >= 8 && strncmp(str, "DEFPROTO", 8) == 0) return(TRUE);
  if (n >= 3 && strncmp(str, "LET", 3) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "FLET", 4) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "COND", 4) == 0) return(TRUE);
  if (n >= 4 && strncmp(str, "CASE", 4) == 0) return(TRUE);
  if (n >= 6 && strncmp(str, "LABELS", 6) == 0) return(TRUE);
  if (n >= 6 && strncmp(str, "LAMBDA", 6) == 0) return(TRUE);
  return(FALSE);
}

LOCAL pascal VOID close_listener(void) 
{
  HideWindow(ttyWind);
}

Boolean edit_key_filter(WindowPtr editWind, int c)
{
  TEHandle editTE;
  int count, has_fixed;
    
  editTE = GetEWindowTE(editWind);
  if (editWind == nil) return(TRUE);
  has_fixed = get_fixed_count(editWind, &count);
  if (! has_fixed) count = 0;
  
  if (! input_enabled(editWind)) return (TRUE);
  switch (c) {
  case '\034':
  case '\035':
  case '\036':
  case '\037': return(FALSE);  /* arrow keys */
  case '\t':   adjust_insert(editWind); do_tab(editTE, count); return(TRUE);
  case '\b':
    adjust_insert(editWind); 
    if ((*editTE)->selStart > count
        || ((*editTE)->selStart == count 
        && (*editTE)->selStart < (*editTE)->selEnd)) {
       return(FALSE);
    }
    else return(TRUE);
  case enter: if (editWind == ttyWind && ttyWind != nil) tty_enter(); return(TRUE);
  case RETURNCHAR:
    if (editWind == ttyWind) {
      tty_return();
      return(TRUE);
    }
    else {
      adjust_insert(editWind);
      return(FALSE);
    }
  case ')':
  	adjust_insert(editWind);
    TEKey(c, editTE);
    flash_matching_paren(editTE, count);
    return(TRUE);
  default:
      adjust_insert(editWind);
      return(FALSE);
  }
}

LOCAL pascal VOID tty_key(void)
{
  SetEWindowDirty (ttyWind, FALSE);
}

LOCAL pascal VOID edit_idle (void)
{
  GrafPtr port;
  TEHandle editTE;
  
  GetPort(&port);
  flush_output_stream(port);
  if (! input_enabled(port)) {
    editTE = GetEWindowTE(port);
    if (editTE != nil) TEDeactivate (editTE);
  }
}

LOCAL pascal VOID close_edit(void)
{
  GrafPtr w;
  LVAL object;
  
  GetPort(&w);
  object = get_window_object(w);
  if (objectp(object)) send_message(object, sk_remove);
}

VOID make_listener_window(Rect r)
{
  ttyWind = NewEWindow (&r, "\pXLISP-STAT", TRUE, (WindowPtr) -1L, TRUE, 0L, FALSE);
  SetEWindowProcs(ttyWind, tty_key, nil, close_listener, edit_idle);
  SetEWindowStyle (ttyWind,
                   listenerFontNum, listenerFontSize, listenerFontStyle,
                   0, teJustLeft);
}
 
 VOID set_edit_window_procs(WindowPtr w)
{
  if (IsEWindow(w)) {
    SetEWindowProcs(w, nil, nil, close_edit, edit_idle);
  }
}
    
LVAL xsedit_window_update(void)   { return(NIL); }
LVAL xsedit_window_activate(void) { return(NIL); }
