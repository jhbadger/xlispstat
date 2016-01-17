/* macdialogs1 - Low Level Dialog Objects for Macintosh                */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include ":dialogs.h"

/* forward declarations */
LOCAL VOID SetDialogItemData _((LVAL dialog));
LOCAL int count_hardware_items _((LVAL items)); 
LOCAL VOID zero_ptr _((char *p, int n));
LOCAL VOID InstallItemList _((DialogPtr theDialog, LVAL items));
LOCAL int FindItemType _((LVAL item));
LOCAL VOID InstallItem _((DialogPtr theDialog, LVAL item));
LOCAL VOID InstallButtonItem _((DialogPtr theDialog, LVAL item));
LOCAL VOID InstallToggleItem _((DialogPtr theDialog, LVAL item));
LOCAL VOID InstallTextItem _((DialogPtr theDialog, LVAL item));
LOCAL VOID InstallChoiceItem _((DialogPtr theDialog, LVAL item));
LOCAL VOID InstallScrollItem _((DialogPtr theDialog, LVAL item));
LOCAL pascal void drawList _((DialogPtr theDialog, short item));
LOCAL VOID InstallListItem _((DialogPtr theDialog, LVAL item));
LOCAL VOID InstallControlItem _((DialogPtr theDialog, LVAL item, int which));
LOCAL pascal Boolean filter(DialogPtr dlog, EventRecord *evt, short *item);


/* layout definitions */
# define MAX_ENTRY_LENGTH 30
# define SCROLL_MIN 0
# define SCROLL_MAX 100
# define CHOICE_HEIGHT 20


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

DialogItemData *GetDialogItemData(DialogPtr theDialog)
{
  char *p;
  
  p = (char *) *GetDialogData(theDialog);
  return((DialogItemData *) (p + sizeof(DialogData)));
}

DialogItemData FindItemData(DialogPtr theDialog, LVAL item)
{
  int n = DialogItemCount(theDialog);
  DialogItemData *data, *itemData;
  
  data = GetDialogItemData(theDialog);
  for (itemData = nil; n > 0 && itemData == nil; n--, data++)
    if (data->object == item) itemData = data;
  
  return(*itemData);
}

VOID truncateListEntry(char *s)
{
  if (strlen(s) > MAX_ENTRY_LENGTH) {
    s = s + MAX_ENTRY_LENGTH - 3;
    s[0] = '.'; s[1] = '.'; s[2] = '.'; s[3] = '\0';
  }
}
 
VOID check_alloc(char *p, int check_nil)
{
  if (check_nil && p == nil)
    xlfail("allocation failed - null pointer or handle retirned");
  if (MemError())
    xlfail("allocation failed - MemError reported");
}

/***********************************************************************/
/**                                                                   **/
/**                         DIALOG-PROTO Methods                      **/
/**                                                                   **/
/***********************************************************************/

static VOID MakeDialogItemData(LVAL dialog, Handle *items, Handle *data)
{
  int numItems;

  numItems = count_hardware_items(slot_value(dialog, s_items)) + 1;
  *items = NewHandle(2 + 14 * numItems);
  check_alloc((char *) *items, TRUE);
  zero_ptr(**items, 2 + 14 * numItems);
  **(short **)(*items) = numItems - 1;
  
  *data = NewHandle(sizeof(DialogData) + numItems * sizeof(DialogItemData));
  check_alloc((char *) *data, TRUE);
  zero_ptr(**data, sizeof(DialogData) + numItems * sizeof(DialogItemData));
}

static pascal void OutlineDefaultButton(DialogPtr theDialog, short item)
{
  Rect r;
  short itemType;
  Handle theItem;
  
  item = ((DialogPeek) theDialog)->aDefItem;
  if (item < 1 || item > DialogItemCount(theDialog)) return;
  
  GetDialogItem(theDialog, item, &itemType, &theItem, &r);
  if (itemType != ctrlItem + btnCtrl) return;
  
  PenSize(3, 3);
  InsetRect(&r, -4, -4);
  FrameRoundRect(&r, 16, 16);
}

static ControlActionUPP OutlineDefaultButtonUPP = NULL;

LOCAL VOID SetDialogItemData(LVAL dialog)
{
  DialogPtr theDialog;
  Rect r;
  DialogItemData *data;
  DialogData *dialogData;
  LVAL items = slot_value(dialog, s_items);
  
  theDialog = (DialogPtr) GETDIALOGADDRESS(dialog);
  if (theDialog == nil) xlfail("dialog not allocated");
  
  SetDialogObject(theDialog, dialog);
  
  SetRect(&r, 0, 0, 0, 0);
  if (! OutlineDefaultButtonUPP)
    OutlineDefaultButtonUPP = NewControlActionProc((ProcPtr) OutlineDefaultButton);
  SetDialogItem(theDialog, 1, userItem, (Handle) OutlineDefaultButtonUPP, &r);
  ((DialogPeek) theDialog)->aDefItem = 1;
  
  data = GetDialogItemData(theDialog);
  dialogData = *GetDialogData(theDialog);
  data[0].type = NULL_ITEM;
  data[0].itemNumber = 1;
  data[0].itemHandle = nil;
  dialogData->count = 1;
  
  InstallItemList(theDialog, items);
}

LOCAL VOID InstallItemList(DialogPtr theDialog, LVAL items)
{
  for (; consp(items); items = cdr(items))
    if (consp(car(items))) InstallItemList(theDialog, car(items));
    else InstallItem(theDialog, car(items));
}

LOCAL int FindItemType(LVAL item)
{
  if (consp(item)) return(ITEM_LIST);
  else if (button_item_p(item)) return(BUTTON_ITEM);
  else if (toggle_item_p(item)) return(TOGGLE_ITEM);
  else if (text_item_p(item)) return(TEXT_ITEM);
  else if (choice_item_p(item)) return(CHOICE_ITEM);
  else if (scroll_item_p(item)) return(SCROLL_ITEM);
  else if (list_item_p(item)) return(LIST_ITEM);
  else xlfail("item of unknown type");
  return 0; /* not reached */
}
  
LOCAL VOID InstallItem(DialogPtr theDialog, LVAL item)
{
  int type;
  
  if (! dialog_item_p(item)) xlerror("not a dialog item", item);
  
  type = FindItemType(item);
  
  switch (type) {
  case BUTTON_ITEM: InstallButtonItem(theDialog, item); break;
  case TOGGLE_ITEM: InstallToggleItem(theDialog, item); break;
  case CHOICE_ITEM: InstallChoiceItem(theDialog, item); break;
  case MESSAGE_ITEM:
  case TEXT_ITEM:   InstallTextItem(theDialog, item); break;
  case SCROLL_ITEM: InstallScrollItem(theDialog, item); break;
  case REAL_SCROLL_ITEM:
  case LIST_ITEM:   InstallListItem(theDialog, item); break;
  default: xlfail("unkown item type");
  }
}

LOCAL VOID InstallButtonItem(DialogPtr theDialog, LVAL item)
{
  InstallControlItem(theDialog, item, BUTTON_ITEM);
}

LOCAL VOID InstallToggleItem(DialogPtr theDialog, LVAL item)
{
  InstallControlItem(theDialog, item, TOGGLE_ITEM);
}

LOCAL VOID InstallTextItem(DialogPtr theDialog, LVAL item)
{
  Rect r;
  DialogItemData *data;
  DialogData *dialogData;
  Handle theItem;
  int type, itemIndex;
  char *text;
  Point loc, size;
  Str255 pbuf;
  
  dialogData = *GetDialogData(theDialog);
  itemIndex = ++(dialogData->count);
  
  if (! stringp(slot_value(item, s_text))) 
    xlerror("not a string", slot_value(item, s_text));
  text = (char *) getstring(slot_value(item, s_text));
    
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  SetRect(&r, loc.h, loc.v, loc.h + size.h, loc.v + size.v);
    
  type = (! null(slot_value(item, s_editable))) ? editText : statText;
  
  theItem = NewHandle(1);
  SetDialogItem(theDialog, itemIndex, type, theItem, &r);
  check_alloc((char *) theItem, TRUE);
  CintoPstring(text, pbuf, sizeof pbuf, TRUE);
  SetDialogItemText(theItem, pbuf);
  if (type == editText) SelectDialogItemText(theDialog, itemIndex, 0, strlen(text));
  check_alloc((char *) theItem, TRUE);

  data = GetDialogItemData(theDialog);
  data[itemIndex - 1].type = TEXT_ITEM;
  data[itemIndex - 1].itemNumber = itemIndex;
  data[itemIndex - 1].itemHandle = theItem;
  data[itemIndex - 1].object = item;
}

LOCAL VOID InstallChoiceItem(DialogPtr theDialog, LVAL item)
{
  LVAL titles, temp;
  Rect r;
  DialogItemData *data;
  DialogData *dialogData;
  Handle theItem;
  int type, procID, itemIndex, clusterLeader, clusterSize, initial;
  char *text;
  Point loc, size;
  
  dialogData = *GetDialogData(theDialog);
  
  titles = slot_value(item, s_text);
    
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  size.v = CHOICE_HEIGHT;
    
  type = ctrlItem + radCtrl;
  procID = radioButProc;
  
  if (! listp(titles)) xlerror("not a list", titles);
  clusterLeader = dialogData->count + 1;
  clusterSize = llength(titles);
  for (; consp(titles); titles = cdr(titles)) {
    if (! stringp(car(titles))) xlerror("not a string", car(titles));
    text = (char *) getstring(car(titles));

    dialogData = *GetDialogData(theDialog);
    itemIndex = ++(dialogData->count);
    SetRect(&r, loc.h, loc.v, loc.h + size.h, loc.v + size.v);
    loc.v += CHOICE_HEIGHT;
    CtoPstr(text);
    theItem = (Handle) NewControl(theDialog, &r, (StringPtr) text, TRUE,
	                              0, 0, 1, procID, (long) item);
    PtoCstr((StringPtr) text);
    check_alloc((char *) theItem, TRUE);
    SetDialogItem(theDialog, itemIndex, type, theItem, &r);

    data = GetDialogItemData(theDialog);
    data[itemIndex - 1].type = CHOICE_ITEM;
    data[itemIndex - 1].itemNumber = itemIndex;
    data[itemIndex - 1].itemHandle = theItem;
    data[itemIndex - 1].object = item;
    data[itemIndex - 1].clusterLeader = clusterLeader;
    data[itemIndex - 1].clusterSize = clusterSize;
  }
  data = GetDialogItemData(theDialog);
  temp = slot_value(item, s_value);
  initial = (fixp(temp)) ? getfixnum(temp) : 0;
  initial = ((initial >= 0 && initial < clusterSize) ? initial : 0) + clusterLeader - 1;
  SetControlValue((ControlHandle) data[initial].itemHandle, 1);
}

LOCAL VOID InstallScrollItem(DialogPtr theDialog, LVAL item)
{
  Rect r;
  DialogItemData *data;
  DialogData *dialogData;
  Handle theItem;
  int low, high, value;
  int type, procID, itemIndex;
  Point loc, size;
  LVAL temp;
  
  dialogData = *GetDialogData(theDialog);
  itemIndex = ++(dialogData->count);
  
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
    
  type = userItem;
  procID = scrollBarProc;
  
  temp = slot_value(item, s_min_value);
  low = fixp(temp) ? getfixnum(temp) : SCROLL_MIN;
  temp = slot_value(item, s_max_value);
  high = fixp(temp) ? getfixnum(temp) : SCROLL_MAX;
  temp = slot_value(item, s_value);
  value = (fixp(temp)) ? getfixnum(temp) : low;

  SetRect(&r, loc.h, loc.v, loc.h + size.h, loc.v + size.v);
  theItem = (Handle) NewControl(theDialog, &r, "\p", TRUE,
                                value, low, high, procID, (long) item);
  check_alloc((char *) theItem, TRUE);
  SetDialogItem(theDialog, itemIndex, type, nil, &r);

  data = GetDialogItemData(theDialog);
  data[itemIndex - 1].type = SCROLL_ITEM;
  data[itemIndex - 1].itemNumber = itemIndex;
  data[itemIndex - 1].itemHandle = theItem;
  data[itemIndex - 1].object = item;
}

LOCAL pascal void drawList(DialogPtr theDialog, short item)
{
  Rect r;
  DialogItemData *data;
  int type;
  ListHandle theList;
  
  data = GetDialogItemData(theDialog);
  theList = (ListHandle) data[item - 1].itemHandle;
  type = data[item - 1].type;
  
  if (type == LIST_ITEM) {
    LUpdate(thePort->visRgn, theList);
    r = (*theList)->rView;
    InsetRect(&r, -1, -1);
    FrameRect(&r);
  }
}

static ControlActionUPP drawListUPP = NULL;

LOCAL VOID InstallListItem(DialogPtr theDialog, LVAL item)
{
  Rect r, b;
  DialogItemData *data;
  DialogData *dialogData;
  ListHandle theItem;
  int type, itemIndex, columns, n, m, i;
  Point loc, size, cell;
  LVAL listData, next, temp;
  Boolean vscroll, hscroll;
  char *s;
  
  dialogData = *GetDialogData(theDialog);
  itemIndex = ++(dialogData->count);
  
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
    
  type = userItem;
  
  listData = slot_value(item, s_list_data);
  
  if (seqp(listData)) {
    n = seqlen(listData);
    m = 1;
  }
  else if (matrixp(listData)) {
    n = numrows(listData);
    m = numcols(listData);
  }
  else xlerror("this form of data is not yet supported", listData);
  temp = slot_value(item, s_columns);
  if (! fixp(temp) || getfixnum(temp) < 1) columns = 1;
  else columns = getfixnum(temp);
  hscroll = (columns < m) ? TRUE : FALSE;
  vscroll = (n * 16 > size.v - ((hscroll) ? 15 : 0)) ? TRUE : FALSE;

  SetRect(&b, 0, 0, columns, n);
  SetRect(&r, loc.h, loc.v, loc.h + size.h, loc.v + size.v);
  if (vscroll) r.right -= 15;
  if (hscroll) r.bottom -= 15;
  
  cell.h = 0; cell.v = 0;
  theItem = LNew(&r, &b, cell, 0, theDialog, TRUE, FALSE, hscroll, vscroll);
  check_alloc((char *) theItem, TRUE);
  if (slot_value(item, s_multiple) != NIL)
    (*theItem)->selFlags = lExtendDrag | lNoDisjoint;
  else (*theItem)->selFlags = lOnlyOne;

  if (columns < m) LAddColumn(m - columns, columns, theItem);
  if (vscroll) r.right += 15;
  if (hscroll) r.bottom += 15;
  check_alloc((char *) theItem, TRUE);
  
  if (! drawListUPP)
    drawListUPP = NewControlActionProc((ProcPtr) drawList);
  SetDialogItem(theDialog, itemIndex, userItem, (Handle) drawListUPP, &r);

  if (darrayp(listData))
    listData = getdarraydata(listData);
  for (cell.v = 0, i = 0; cell.v < n; cell.v++)
    for (cell.h = 0; cell.h < m; cell.h++, i++) {
      next = getnextelement(&listData, i);
      s = (stringp(next)) ? (char *) getstring(next) : "";
      strcpy(buf, s);
      truncateListEntry(buf);
      LSetCell(buf, strlen(buf), cell, theItem);
      check_alloc((char *) theItem, TRUE);
    }

  data = GetDialogItemData(theDialog);
  data[itemIndex - 1].type = LIST_ITEM;
  data[itemIndex - 1].itemNumber = itemIndex;
  data[itemIndex - 1].itemHandle = (Handle) theItem;
  data[itemIndex - 1].object = item;
}

LOCAL VOID InstallControlItem(DialogPtr theDialog, LVAL item, int which)
{
  Rect r;
  DialogItemData *data;
  DialogData *dialogData;
  Handle theItem;
  int type, value, low, high, procID, itemIndex;
  char *text;
  Point loc, size;
  LVAL val;
  
  dialogData = *GetDialogData(theDialog);
  itemIndex = ++(dialogData->count);
  
  if (! stringp(slot_value(item, s_text))) 
    xlerror("not a string", slot_value(item, s_text));
  text = (char *) getstring(slot_value(item, s_text));
    
  loc = ListToPoint(slot_value(item, s_location));
  size = ListToPoint(slot_value(item, s_size));
  SetRect(&r, loc.h, loc.v, loc.h + size.h, loc.v + size.v);
    
  value = 0; low = 0; high = 1; 
  switch (which) {
  case BUTTON_ITEM: procID = pushButProc; type = ctrlItem + btnCtrl; break;
  case TOGGLE_ITEM: procID = checkBoxProc; type = ctrlItem + chkCtrl; break;
  default: xlfail("unknown item type");
  }
  
  CtoPstr(text);
  theItem = (Handle) NewControl(theDialog, &r, (StringPtr) text, TRUE,
                                value, low, high, procID, (long) item);
  PtoCstr((StringPtr) text);
  check_alloc((char *) theItem, TRUE);
  SetDialogItem(theDialog, itemIndex, type, theItem, &r);

  data = GetDialogItemData(theDialog);
  data[itemIndex - 1].type = which;
  data[itemIndex - 1].itemNumber = itemIndex;
  data[itemIndex - 1].itemHandle = theItem;
  data[itemIndex - 1].object = item;
  
  if (which == TOGGLE_ITEM) {
    val = slot_value(item, s_value);
    SetControlValue((ControlHandle) theItem, (val != NIL));
  }
}

static VOID zero_ptr(char *p, int n)
{
  while (n-- > 0) *p++ = 0;
}

LOCAL int count_hardware_items(LVAL items)
{ 
  LVAL temp;
  int n;
  
  if (! consp (items)) return(0);
  else 
    for (n = 0; consp(items); items = cdr(items))
      switch(FindItemType(car(items))) {
      case CHOICE_ITEM: 
        temp = slot_value(car(items), s_text);
        if (! consp(temp)) xlerror("not a list", temp);
        n += llength(temp);
        break;
      case ITEM_LIST: n += count_hardware_items(car(items)); break;
      default: n += 1;
      }
  return(n);
}

LOCAL pascal Boolean filter(DialogPtr dlog, EventRecord *evt, short *item)
{
  if (evt->what == activateEvt) {
    LVAL object;
    object = GetDialogObject(dlog);
    send_message_1L(object,
                    sk_activate,
                    ((evt->modifiers & activeFlag) != 0) ? s_true : NIL);
  }
  return FALSE;
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                       Public Dialog Functions                     **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

VOID DialogAllocate(LVAL dialog)
{
  DialogPtr theDialog;
  Point loc, size;
  Rect bounds;
  char *title;
  Boolean visible, goAway;
  int type, result;
  WindowPtr behind;
  Handle items, ref;
  GrafPtr savePort;
  
  if (check_dialog_address(dialog)) DialogRemove(dialog);
    
  if (! stringp(slot_value(dialog, s_title))) 
    xlerror("not a string", slot_value(dialog, s_title));
  title = (char *) getstring(slot_value(dialog, s_title));
  
  loc = ListToPoint(slot_value(dialog, s_location));
  loc.v += GetMBarHeight();
  size = ListToPoint(slot_value(dialog, s_size));
  SetRect(&bounds, loc.h, loc.v, loc.h + size.h, loc.v + size.v);
  
  visible = TRUE;
  goAway = (slot_value(dialog, s_go_away) != NIL) ? TRUE : FALSE;
  
  if (slot_value(dialog, s_type) == s_modeless)
    type = noGrowDocProc;
  else type = dBoxProc;
  
  behind = (WindowPtr) -1;
  
  MakeDialogItemData(dialog, &items, &ref);
    
  CtoPstr(title);
  theDialog = NewDialog(nil, &bounds, (StringPtr) title, FALSE, type,
                        behind, goAway, (long) ref, items);
  PtoCstr((StringPtr) title);
  check_alloc((char *) theDialog, TRUE);
  GetPort(&savePort);
  result = SkelDialog(theDialog, filter, doDialog, closeDialog, clobberDialog);
  SetPort(savePort);
  if (! result) xlfail("allocation Failed");
  set_dialog_address((CPTR) theDialog, dialog);

  SetDialogItemData(dialog);
  DialogSetDefaultButton(dialog, slot_value(dialog, s_default_button));
  if (visible) MyShowWindow(theDialog);
}

VOID DialogRemove(LVAL dialog)
{
  if (check_dialog_address(dialog))
  	SkelRmveDlog((DialogPtr) GETDIALOGADDRESS(dialog));
  if (objectp(dialog)) standard_hardware_clobber(dialog);
}

VOID DialogSetDefaultButton(LVAL dialog, LVAL item)
{
  DialogItemData itemData;
  DialogPtr theDialog;
  GrafPtr savePort;
  int noDflt;
  
  if (item != NIL && ! button_item_p(item))
    xlerror("not a button item", item);

  set_slot_value(dialog, s_default_button, item);
    
  theDialog = (DialogPtr) GETDIALOGADDRESS(dialog);
  if (theDialog != nil) {
    noDflt = (((DialogPeek) theDialog)->aDefItem == 1);
  
    if (item == NIL)
      ((DialogPeek) theDialog)->aDefItem = 1;
    else {
      itemData = FindItemData(theDialog, item);
      ((DialogPeek) theDialog)->aDefItem = itemData.itemNumber;
    }
  
  
    if (! noDflt) {
      GetPort(&savePort);
      SetPort(theDialog);
      EraseRect(&theDialog->portRect);
      SetPort(savePort);
      DrawDialog(theDialog);
    }
  }
}
