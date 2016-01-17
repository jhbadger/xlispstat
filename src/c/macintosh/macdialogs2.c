/* macdialogs2 - Low Level Dialog Objects for Macintosh                */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
# include ":dialogs.h"

/* forward declarations */
LOCAL int FindItemHit _((Point pt, DialogPtr theDialog));
LOCAL VOID SetClusterValue _((DialogPtr theDialog, int item));
LOCAL int max_line_size _((char *s));

extern LVAL s_text_length;
static int jump_to_top;

/* layout definitions */
# define FONT_HEIGHT 16
# define FONT_WIDTH_GUESS 9
# define FONT_POINTS 12

# define BUTTON_HEIGHT 20
# define BUTTON_WIDTH 90
# define BUTTON_PAD 15

# define TOGGLE_PAD 20
# define TOGGLE_HEIGHT FONT_HEIGHT - 2

# define EDIT_TEXT_PAD 20
# define EDIT_TEXT_HEIGHT 20
# define STATIC_TEXT_PAD 20
# define STATIC_TEXT_HEIGHT FONT_HEIGHT

# define CHOICE_HEIGHT 20
# define CHOICE_PAD 20

# define SCROLL_WIDTH 180
# define SCROLL_HEIGHT 16

# define LIST_ITEM_HEIGHT 16
# define MAX_LIST_ROWS 12

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                       Internal Dialog Functions                   **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

static GrafPort GP;
static GrafPtr port = nil;

static Point StringSize(char *s)
{
  Point pt;
  GrafPtr savePort;
  
  if (port == nil) {
    GetPort(&savePort);
    port = &GP;
    OpenPort(port);
    port->txFont = systemFont;
    port->txSize = FONT_POINTS;
    SetPort(savePort);
  }
  
  pt.v = FONT_HEIGHT;
  if (port == nil) pt.h = FONT_WIDTH_GUESS * strlen(s);
  else {
    Str255 pbuf;
    GetPort(&savePort);
    SetPort(port);
    CintoPstring(s, pbuf, sizeof pbuf, FALSE);
    pt.h = StringWidth(pbuf);
    SetPort(savePort);
  }
  return(pt);
}

/***********************************************************************/
/**                                                                   **/
/**                         Callback Functions                        **/
/**                                                                   **/
/***********************************************************************/

static Boolean listClick(int item, EventRecord *event, DialogPtr theDialog)
{
  LVAL object;
  int itemType;
  ListHandle theItem;
  Point pt;
  Boolean list_click = false, double_click;
  GrafPtr savePort;
  DialogItemData *data;
  
  if (item > 1 && item <= DialogItemCount(theDialog) 
      && event->what == mouseDown) {
    data = GetDialogItemData(theDialog);
    theItem = (ListHandle) data[item - 1].itemHandle;
    itemType = data[item - 1].type;
    object = data[item - 1].object;
    
    if (itemType == LIST_ITEM) {
      list_click = TRUE;
      pt = event->where;
      GetPort(&savePort);
      SetPort(theDialog);
      GlobalToLocal(&pt);
      SetPort(savePort);
      double_click = LClick(pt, event->modifiers, theItem);
      if (double_click) send_message_1L(object, sk_do_action, s_true);
      else send_callback_message(object, sk_do_action);
    }
  }  
  return(list_click);
}

static pascal Boolean modalFilter(DialogPtr theDialog, EventRecord *theEvent, short *itemHit)
{
  char c;
  char result;
  
  c = theEvent->message & charCodeMask;
  if ((theEvent->modifiers & cmdKey) && (c == '.')) {
    jump_to_top = TRUE;
    result = (char) TRUE;
    *itemHit = 0;
  }
  else if (theEvent->what == keyDown && c == RETURNCHAR) {
  	result = TRUE;
  	*itemHit = ((DialogPeek) theDialog)->aDefItem;
  }
  else {
    *itemHit = FindItemHit(theEvent->where, theDialog);
    result = (char) listClick(*itemHit, theEvent, theDialog);
  }
  return(result);
}

/* Action proc for scroll bars. */
static pascal void scrollAction(ControlHandle theControl, short partCode)
{
  LVAL scroll = (LVAL) GetControlReference(theControl);
  int page, value;
  GrafPtr savePort;
  
  if (! scroll_item_p(scroll)) return;
  page = getfixnum(slot_value(scroll, s_page_increment));
  
  value = GetControlValue(theControl);
  switch (partCode) {
  case kControlUpButtonPart:   value--; break;
  case kControlDownButtonPart: value++; break;
  case kControlPageUpPart:     value -= page; break;
  case kControlPageDownPart:   value += page; break;
  }
  SetControlValue(theControl, value);
  set_slot_value(scroll, s_value, cvfixnum((FIXTYPE) value));
  
  GetPort(&savePort);
  send_message(scroll, sk_scroll_action);
  SetPort(savePort);
}

static ControlActionUPP scrollActionUPP = NULL;

static TrackScroll(DialogPtr theDialog, int item)
{
  DialogItemData *data;
  ControlHandle theScroll;
  LVAL scroll;
  Point pt;
  int part, value;
  GrafPtr port;
    
  data = GetDialogItemData(theDialog);
  theScroll = (ControlHandle) data[item - 1].itemHandle;
  
  GetPort(&port);
  SetPort(theDialog);
  GetMouse(&pt);
  SetPort(port);
  part = TestControl(theScroll, pt);
  if (part == kControlIndicatorPart) {
  	TrackControl(theScroll, pt, nil);
	scroll = (LVAL) GetControlReference(theScroll);
	value = GetControlValue(theScroll);
    set_slot_value(scroll, s_value, cvfixnum((FIXTYPE) value));
  }
  else if (part != 0) {
    if (! scrollActionUPP)
      scrollActionUPP = NewControlActionProc((ProcPtr) scrollAction);
    TrackControl(theScroll, pt, scrollActionUPP);
  }
  FlushEvents(mDownMask | mUpMask, 0);
  
  return (part != kControlIndicatorPart);
}

pascal VOID doDialog(DialogPtr dlog, short theItem)
{
  LVAL item;
  DialogItemData *data;
  int type;
  ControlHandle itemHandle;
  EventRecord *theEvent;

  data = GetDialogItemData(dlog);
  item = data[theItem - 1].object;
  type = data[theItem - 1].type;
  itemHandle = (ControlHandle) data[theItem - 1].itemHandle;
  
  switch (type) {
  case TOGGLE_ITEM: 
    SetControlValue(itemHandle, ! GetControlValue(itemHandle));
    set_slot_value(item, s_value, (GetControlValue(itemHandle)) ? s_true : NIL);
    break;
  case CHOICE_ITEM:
    SetClusterValue(dlog, theItem);
    break;
  case SCROLL_ITEM:
    if (TrackScroll(dlog, theItem)) return; /* no message unless in thumb */
    break;
  }
  
  theEvent = SkelGetCurrentEvent();

  if (objectp(item) && theItem > 1 
      && ! listClick(theItem, theEvent, dlog)) {
    send_message(item, sk_do_action);
  }
}

pascal VOID closeDialog(void)
{
  GrafPtr port;
  LVAL object;
    
  GetPort(&port);
  object = GetDialogObject(port);
  send_message(object, sk_close);
}

pascal VOID clobberDialog(void)
{
  GrafPtr port;
  LVAL object;
  Handle ref;
  DialogItemData *data;
  int i;
    
  GetPort(&port);
  object = GetDialogObject(port);
  ref = (Handle) GetWRefCon(port);
  if (ref != nil) {
    for (i = 0; i < DialogItemCount(port); i++)
      data = GetDialogItemData(port);
      switch(data[i].type) {
      case SCROLL_ITEM: DisposeControl((ControlHandle) data[i].itemHandle); break;
      case LIST_ITEM:   LDispose((ListHandle) data[i].itemHandle); break;
      }
    DisposeHandle(ref);
  }
  DisposeDialog(port);
  if (objectp(object)) standard_hardware_clobber(object);
}

LOCAL int FindItemHit(Point pt, DialogPtr theDialog)
{
  GrafPtr savePort;
  short itemType;
  int i, itemHit;
  Handle theItem;
  Rect r;
  
  GetPort(&savePort);
  SetPort(theDialog);
  GlobalToLocal(&pt);
  SetPort(savePort);
  
  itemHit = 0;
  for (i = 1; i <= DialogItemCount(theDialog) && itemHit == 0; i++) {
    GetDialogItem(theDialog, i, &itemType, &theItem, &r);
    if (PtInRect(pt, &r))  itemHit = i;
  }
  return(itemHit);
}

LOCAL VOID SetClusterValue(DialogPtr theDialog, int item)
{
  int i, n, leader;
  DialogItemData *data;
  ControlHandle itemHandle;
  
  data = GetDialogItemData(theDialog);
  leader = data[item - 1].clusterLeader;
  n = data[item - 1].clusterSize + leader;
  itemHandle = (ControlHandle) data[item - 1].itemHandle;
  
  for (i = leader; i < n; i++) {
    data = GetDialogItemData(theDialog);
    SetControlValue((ControlHandle) data[i - 1].itemHandle, 0);
  }
  SetControlValue(itemHandle, 1);
}


/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                        Public Dialog Functions                    **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/***********************************************************************/
/**                                                                   **/
/**                           Dialog Functions                        **/
/**                                                                   **/
/***********************************************************************/

static ModalFilterUPP modalFilterUPP = NULL;

LVAL DialogGetModalItem(LVAL dialog)
{
  DialogPtr theDialog;
  LVAL item;
  short itemNumber;
  int type;
  DialogItemData *data;
  ControlHandle itemHandle;
  
  theDialog = (DialogPtr) GETDIALOGADDRESS(dialog);
  if (theDialog == nil) xlfail("the dialog is not visible");
  
  SetCursor(&arrow);
  MyShowWindow(theDialog);
  NotifyIfInBackground();
  if (! modalFilterUPP)
    modalFilterUPP = NewModalFilterProc((ProcPtr) modalFilter);
  ModalDialog(modalFilterUPP, &itemNumber);
  if (jump_to_top) {
    jump_to_top = false;
	FlushEvents (everyEvent - diskMask, 0 );
    xlsigint();
  }

  if (itemNumber < 1 || itemNumber > DialogItemCount(theDialog))
    xlfail("invalid item number");

  data = GetDialogItemData(theDialog);
  type = data[itemNumber - 1].type;
  itemHandle = (ControlHandle) data[itemNumber - 1].itemHandle;
  item = data[itemNumber - 1].object;
  
  switch (type) {
  case TOGGLE_ITEM: 
    SetControlValue(itemHandle, ! GetControlValue(itemHandle));
    set_slot_value(item, s_value, (GetControlValue(itemHandle)) ? s_true : NIL);
    break;
  case CHOICE_ITEM:
    SetClusterValue(theDialog, itemNumber);
    break;
  case SCROLL_ITEM:
    TrackScroll(theDialog, itemNumber);
    break;
  }
  
  return(item);
}

/***********************************************************************/
/**                                                                   **/
/**                         Button Item Functions                     **/
/**                                                                   **/
/***********************************************************************/

VOID DialogButtonGetDefaultSize(LVAL item, int *width, int *height)
{
  LVAL text = slot_value(item, s_text);
  Point sz;
  
  if (! stringp(text)) xlerror("not a string", text);
  sz = StringSize(getstring(text));
  
  if (width != nil) *width = max(sz.h + BUTTON_PAD, BUTTON_WIDTH);
  if (height != nil) *height = BUTTON_HEIGHT;
}

/***********************************************************************/
/**                                                                   **/
/**                         Toggle Item Functions                     **/
/**                                                                   **/
/***********************************************************************/

VOID DialogToggleGetDefaultSize(LVAL item, int *width, int *height)
{
  Point sz;
  sz = StringSize(getstring(slot_value(item, s_text)));
  if (width != nil) *width = sz.h + TOGGLE_PAD;
  if (height != nil) *height = TOGGLE_HEIGHT;
}

LVAL DialogToggleItemValue(LVAL item, int set, LVAL value)
{
  LVAL dialog;
  DialogItemData itemData;
  DialogPtr theDialog;

  dialog = slot_value(item, s_dialog);
  if (set) set_slot_value(item, s_value, (value != NIL) ?  s_true : NIL);

  theDialog = (DialogPtr) (null(dialog) ? nil : GETDIALOGADDRESS(dialog));
  if (theDialog != nil) {
    itemData = FindItemData(theDialog, item);
    if (set) SetControlValue((ControlHandle) itemData.itemHandle,
	                     (value != NIL) ? TRUE : FALSE);
  }
  return(slot_value(item, s_value));
}

/***********************************************************************/
/**                                                                   **/
/**                         Text Item Functions                       **/
/**                                                                   **/
/***********************************************************************/

VOID DialogTextGetDefaultSize(LVAL item, int *width, int *height)
{
  Point sz;
  LVAL text = slot_value(item, s_text);
  LVAL text_length = slot_value(item, s_text_length);
  int w = 0;
  char *s;
  
  if (stringp(text)) {
    w = max_line_size(getstring(text));
    s = (char *) getstring(text);
    for (sz.v = STATIC_TEXT_HEIGHT; *s != '\0'; s++)
      if (*s == '\n' || *s == '\r') sz.v += STATIC_TEXT_HEIGHT;
  }
  if (fixp(text_length)) {
    sz = StringSize("M");
    w = max((int) (getfixnum(text_length) * sz.h), w);
  }
  if (slot_value(item, s_editable) != NIL) {
    if (width != nil) *width = w + EDIT_TEXT_PAD;
    if (height != nil) *height = EDIT_TEXT_HEIGHT;
  }
  else {
    if (width != nil) *width = w + STATIC_TEXT_PAD;
    if (height != nil) *height = sz.v /* STATIC_TEXT_HEIGHT */;
  }
}

LVAL DialogTextItemText(LVAL item, int set, char *text)
{
  LVAL dialog;
  DialogItemData itemData;
  DialogPtr theDialog;

  
  if (set) set_slot_value(item, s_text, cvstring(text));
  dialog = slot_value(item, s_dialog);
  theDialog = (DialogPtr) (null(dialog) ? nil : GETDIALOGADDRESS(dialog));

  if (theDialog != nil) {
    itemData = FindItemData(theDialog, item);
    if (set) {
      Str255 pbuf;
      CintoPstring(text, pbuf, sizeof pbuf, TRUE);
      SetDialogItemText(itemData.itemHandle, pbuf);
    }
    GetDialogItemText(itemData.itemHandle, (StringPtr) buf);
    PtoCstr((StringPtr) buf);
    set_slot_value(item, s_text, cvstring(buf));
  }
  return(slot_value(item, s_text));
}

/***********************************************************************/
/**                                                                   **/
/**                         Choice Item Functions                     **/
/**                                                                   **/
/***********************************************************************/

VOID DialogChoiceGetDefaultSize(LVAL item, int *width, int *height)
{
  Point sz, pt;
  LVAL text = slot_value(item, s_text);
  
  for (sz.h = 0, sz.v = 0; consp(text); text = cdr(text)) {
    pt = StringSize(getstring(car(text)));
    sz.h = max(sz.h, pt.h);
    sz.v += CHOICE_HEIGHT;
  }
  if (width != nil) *width = sz.h + CHOICE_PAD;
  if (height != nil) *height = sz.v;
}

LVAL DialogChoiceItemValue(LVAL item, int set, int value)
{
  LVAL result, dialog;
  DialogItemData itemData, *data;
  DialogPtr theDialog;
  int leader, i, n;
  
  if (set) set_slot_value(item, s_value, cvfixnum((FIXTYPE) value));
  
  dialog = slot_value(item, s_dialog);
  theDialog = (DialogPtr) (null(dialog) ? nil : GETDIALOGADDRESS(dialog));
  if (theDialog != nil) {
    itemData = FindItemData(theDialog, item);
    leader = itemData.clusterLeader;
    n = itemData.clusterSize;
  
    if (set) {
      if (value < 0 || value >= n) xlfail("value out of range");
      for (i = 0; i < n; i++) {
        data = GetDialogItemData(theDialog);
        itemData = data[leader + i - 1];
        SetControlValue((ControlHandle) itemData.itemHandle, 0);
      }
      data = GetDialogItemData(theDialog);
      itemData = data[leader + value - 1];
      SetControlValue((ControlHandle) itemData.itemHandle, 1);
    }
    result = NIL;
    for (i = 0; i < n && result == NIL; i++) {
      data = GetDialogItemData(theDialog);
      itemData = data[leader + i - 1];  
      result = (GetControlValue((ControlHandle) itemData.itemHandle))
             ? cvfixnum((FIXTYPE) i) : NIL;
    }
    set_slot_value(item, s_value, result);
  }
  
  return(slot_value(item, s_value));
}

/***********************************************************************/
/**                                                                   **/
/**                         Scroll Item Functions                     **/
/**                                                                   **/
/***********************************************************************/

VOID DialogScrollGetDefaultSize(LVAL item, int *width, int *height)
{
  if (width != nil) *width = SCROLL_WIDTH;
  if (height != nil) *height = SCROLL_HEIGHT;
}

static LVAL scroll_item_value(LVAL item, int set, int value, int which)
{
  LVAL dialog, result, slot;
  DialogItemData itemData;
  DialogPtr theDialog;
  
  dialog = slot_value(item, s_dialog);
  theDialog = (DialogPtr) (null(dialog) ? nil : GETDIALOGADDRESS(dialog));

  if (theDialog != nil) {
    itemData = FindItemData(theDialog, item);
  
    switch (which) {
    case 'V':
      if (set) SetControlValue((ControlHandle) itemData.itemHandle, value);
      value = GetControlValue((ControlHandle) itemData.itemHandle);
      break;
    case 'H':
      if (set) SetControlMaximum((ControlHandle) itemData.itemHandle, value);
      value = GetControlMaximum((ControlHandle) itemData.itemHandle);
      break;
    case 'L':
      if (set) SetControlMinimum((ControlHandle) itemData.itemHandle, value);
      value = GetControlMinimum((ControlHandle) itemData.itemHandle);
      break;
    }
  }
  switch (which) {
  case 'V': slot = s_value; break;
  case 'H': slot = s_max_value; break;
  case 'L': slot = s_min_value; break;
  }
  if (set || theDialog != nil) {
    result = cvfixnum((FIXTYPE) value);
    set_slot_value(item, slot, result);
  }
  else result = slot_value(item, slot);
  
  return (result);
}

LVAL DialogScrollItemValue(LVAL item, int set, int value)
{
  return(scroll_item_value(item, set, value, 'V'));
}

LVAL DialogScrollItemMax(LVAL item, int set, int value)
{
  return(scroll_item_value(item, set, value, 'H'));
}

LVAL DialogScrollItemMin(LVAL item, int set, int value)
{
  return(scroll_item_value(item, set, value, 'L'));
}

/***********************************************************************/
/**                                                                   **/
/**                          List Item Functions                      **/
/**                                                                   **/
/***********************************************************************/

VOID DialogListGetDefaultSize(LVAL item, int *width, int *height)
{
  LVAL columns = slot_value(item, s_columns);
  LVAL data = slot_value(item, s_list_data);
  Point sz;

  if (seqp(data)) sz.v = LIST_ITEM_HEIGHT * seqlen(data);
  else if (matrixp(data)) 
    sz.v = 16 * numrows(data);
  sz.v = min(sz.v, MAX_LIST_ROWS * LIST_ITEM_HEIGHT);
  if (matrixp(data) && numcols(data) > getfixnum(columns))
    sz.v += 15;
  sz.h = 150 * getfixnum(columns);
  if (width != nil) *width = sz.h;
  if (height != nil) *height = sz.v;
}

VOID DialogListItemSetText(LVAL item, LVAL index, char *text)
{
  LVAL dialog, listData;
  DialogItemData itemData;
  DialogPtr theDialog;
  Point cell;
  ListHandle theList;
  int temp;
    
  listData = slot_value(item, s_list_data);
  if (matrixp(listData)) {
    cell = ListToPoint(index);
  	temp = cell.h; cell.h = cell.v; cell.v = temp;
  }
  else {
  	if (! fixp(index)) xlerror("not an integer", index);
  	cell.h = 0; cell.v = getfixnum(index);
  }

  dialog = slot_value(item, s_dialog);
  theDialog = (DialogPtr) (null(dialog) ? nil : GETDIALOGADDRESS(dialog));
  if (theDialog != nil) {
    itemData = FindItemData(theDialog, item);
    theList = (ListHandle) itemData.itemHandle;

    strcpy(buf, text);
    truncateListEntry(buf);
    LSetCell(buf, strlen(buf), cell, theList);
    check_alloc((char *) theList, TRUE);
  }
}

LVAL DialogListItemSelection(LVAL item, int set, LVAL index)
{
  LVAL dialog, result, listData;
  DialogPtr theDialog;
  DialogItemData itemData;
  Point cell, new_cell;
  int unselect = false;
  ListHandle theList;
  int temp;
  
  listData = slot_value(item, s_list_data);
  if (set) {
  	if (index == NIL) unselect = TRUE;
  	else {
  	  unselect = false;
  	  if (matrixp(listData)) {
  	    cell = ListToPoint(index);
  	    temp = cell.h; cell.h = cell.v; cell.v = temp;
  	  }
  	  else {
  	    if (! fixp(index)) xlerror("not an integer", index);
  	    cell.h = 0; cell.v = getfixnum(index);
  	  }
  	}
  }
  new_cell = cell;
  
  dialog = slot_value(item, s_dialog);
  theDialog = (DialogPtr) (null(dialog) ? nil : GETDIALOGADDRESS(dialog));
  if (theDialog != nil) {
    itemData = FindItemData(theDialog, item);
    theList = (ListHandle) itemData.itemHandle;
  
    cell.h = 0; cell.v = 0;
    if (set && LGetSelect(TRUE, &cell, theList))
      LSetSelect(false, cell, theList);
    if (set && ! unselect) LSetSelect(TRUE, new_cell, theList);

    cell.h = 0; cell.v = 0;
    if (LGetSelect(TRUE, &cell, theList)) {
      if (unselect) {
        LSetSelect(false, cell, theList);
        result = NIL;
      }
      else {
        if (matrixp(listData)) result = integer_list_2(cell.v, cell.h);
        else result = cvfixnum((FIXTYPE) cell.v);
      }
    }
    else result = NIL;
  }
  else result = NIL;
  
  return(result);
}

static int max_line_size(char *s)
{
  char *bp;
  int w;
  Point sz;
  
  for (w = 0; *s != '\0'; *s++) {
    for (bp = buf; *s != '\0' && *s != '\r' && *s != '\n'; s++, bp++)
      *bp = *s;
    *bp = '\0';
    sz = StringSize(buf);
    w = max(w, sz.h);
    if (*s == '\0') break;
  }
  return(w);
}
