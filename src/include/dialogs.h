/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xlstat.h"
#include "xlgraph.h"

XLGLOBAL LVAL s_title, s_type, s_go_away, s_items, s_default_button, 
  s_true, s_text, s_location, s_size, s_action, s_dialog,
  s_hardware_address, s_multiple, s_min_value, s_max_value, 
  s_page_increment, s_editable, s_value, s_list_data, s_columns,
  sk_do_action, sk_scroll_action, s_dialog_proto, s_dialog_item_proto,
  sk_action, s_button_item_proto, s_toggle_item_proto, s_text_item_proto,
  sk_editable, s_choice_item_proto, s_scroll_item_proto,
  sk_min_value, sk_max_value, sk_page_increment, s_list_item_proto,
  sk_columns, s_modeless, sk_allocate, s_modal, sk_close, sk_activate;

extern Rect ListToRect _((LVAL list));
extern Point ListToPoint _((LVAL list));
extern LVAL PointToList _((Point p));
extern VOID DialogAllocate _((LVAL dialog));
extern VOID DialogRemove _((LVAL dialog));
extern LVAL DialogGetModalItem _((LVAL dialog));

extern LVAL DialogToggleItemValue(),
  DialogTextItemText(), DialogChoiceItemValue(), DialogScrollItemValue(),
  DialogScrollItemMax(), DialogScrollItemMin(), DialogListItemSelection();
extern VOID DialogSetDefaultButton _((LVAL dialog, LVAL item));

/***********************************************************************/
/**                                                                   **/
/**                    DIALOG-ITEM-PROTO Definitions                  **/
/**                                                                   **/
/***********************************************************************/

/* dialog item types */
# define NULL_ITEM        0
# define BUTTON_ITEM      1
# define TOGGLE_ITEM      2
# define CHOICE_ITEM      3
# define MESSAGE_ITEM     4
# define TEXT_ITEM        5
# define SCROLL_ITEM      6
# define REAL_SCROLL_ITEM 7
# define LIST_ITEM        8
# define ITEM_LIST        9

# define has_item_location(i) (slot_value(i, s_location) != NIL)
# define has_item_size(i) (slot_value(i, s_size) != NIL)
# define check_dialog_address(d) valid_dialog_address(slot_value(d, s_hardware_address))

/***********************************************************************/
/*                    Machine Dependent Definitions                    */
/***********************************************************************/

#ifdef MACINTOSH
typedef struct {
  LVAL object;
  int count;
} DialogData;

typedef struct {
  int type;
  int itemNumber, clusterLeader, clusterSize;
  Handle itemHandle;
  LVAL object;
} DialogItemData;

#define DialogItemCount(d) (**(short **)(((DialogPeek) d)->items) + 1)
#define GetDialogObject(d) ((*((DialogData **) GetWRefCon(d)))->object)
#define SetDialogObject(d, obj) (GetDialogObject(d) = obj)
#define GetDialogData(d) ((DialogData **) GetWRefCon(d))

extern DialogItemData *GetDialogItemData _((DialogPtr theDialog));
extern DialogItemData FindItemData _((DialogPtr theDialog, LVAL item));
extern VOID check_alloc _((char *p, int check_nil));
extern VOID truncateListEntry _((char *s));
#endif /* MACINTOSH */

#ifdef _Windows
typedef struct {
  LVAL object;
  int count, dflt;
  // item data;
} DialogData;

typedef struct {
  int type;
  int itemNumber, clusterLeader, clusterSize;
  HWND itemHandle;
  LVAL object;
} DialogItemData;

#define SETDIALOGOBJECT(w, d) SetWindowLong((HWND) (w), 0, (LONG) (d))
#define GETDIALOGOBJECT(w) ((LVAL) GetWindowLong((HWND) (w), 0))
#define SETDIALOGDATA(w, d) SetWindowLong((HWND) (w), 4, (LONG) (d))
#define GETDIALOGDATA(w) ((HANDLE) GetWindowLong((HWND) (w), 4))
#define GETITEMDATA(d) ((DialogItemData *) (((DialogData *) (d)) + 1))

#define MIN_DLG_WIDTH 50
#define MIN_DLG_HEIGHT 20
#define MIN_BUTTON_STRLEN 10

#define STATIC_TEXT_PAD 4
#define EDIT_TEXT_PAD 10

#define SCROLL_WIDTH 180
#define SCROLL_MIN 0
#define SCROLL_MAX 100
#define SCROLL_PAGE 5

#define LIST_ITEM_PAD 2
#define MAX_LIST_ROWS 12
#define LIST_COL_CHARS 15   // ### 20 ?
#define MAX_ENTRY_LENGTH 30

#define ITEM_INDEX_BASE 1024
#endif /* _Windows */


