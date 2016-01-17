/* objects - Additional object functions                               */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"
#include "xlstat.h"

/* external variables */
extern LVAL s_true, s_selecting;

VOID init_objects(V)
{
  LVAL root_object = init_root_object();
  LVAL hardware_object, window;
#ifdef MACINTOSH
  LVAL edit_window, listener;
#endif /* MACINTOSH */
  LVAL menu;
#ifdef MACINTOSH
  LVAL apple_menu;
#endif /* MACINTOSH */
  LVAL menu_item, dialog, dialog_item, button_item, toggle_item;
  LVAL text_item, choice_item, scroll_item, list_item;
  LVAL iview_window, iview, iview_spin, iview_scatmat, iview_list;
  LVAL iview_hist, scatterplot, compound_data;
  
  /* *OBJECT* */
  xsaddmsg(root_object, ":GET-METHOD");
  xsaddmsg(root_object, ":REPARENT");
  xsaddmsg(root_object, ":HAS-SLOT");
  xsaddmsg(root_object, ":HAS-METHOD");
  xsaddmsg(root_object, ":ADD-SLOT");
  xsaddmsg(root_object, ":DELETE-SLOT");
  xsaddmsg(root_object, ":ADD-METHOD");
  xsaddmsg(root_object, ":DELETE-METHOD");
  xsaddmsg(root_object, ":SHOW");
  xsaddmsg(root_object, ":ISNEW");
  xsaddmsg(root_object, ":PARENTS");
  xsaddmsg(root_object, ":PRECEDENCE-LIST");
  xsaddmsg(root_object, ":OWN-SLOTS");
  xsaddmsg(root_object, ":OWN-METHODS");
  xsaddmsg(root_object, ":INTERNAL-DOC");
  xsaddmsg(root_object, ":MAKE-PROTOTYPE");
  
#ifndef NOGRAPHICS
  /* HARDWARE-OBJECT-PROTO */
  hardware_object = xsnewproto("HARDWARE-OBJECT-PROTO", root_object);
  xsaddinstanceslot(hardware_object, "HARDWARE-ADDRESS");
  xsaddinstanceslot(hardware_object, "SUBORDINATES");
  xsaddmsg(hardware_object, ":CLOBBER");
  xsaddmsg(hardware_object, ":ALLOCATE");
  xsaddmsg(hardware_object, ":DISPOSE");
  
  /* WINDOW-PROTO */
  window = xsnewproto("WINDOW-PROTO", hardware_object);
  xsaddinstanceslot(window, "GO-AWAY");
  xsaddinstanceslot(window, "SIZE");
  xsaddinstanceslot(window, "LOCATION");
  xsaddinstanceslot(window, "TITLE");
  xsaddmsg(window, ":SHOW-WINDOW");
  xsaddmsg(window, ":HIDE-WINDOW");
  xsaddmsg(window, ":CLOSE");
  xsaddmsg(window, ":TITLE");
  xsaddmsg(window, ":LOCATION");
  xsaddmsg(window, ":SIZE");
  xsaddmsg(window, ":FRAME-LOCATION");
  xsaddmsg(window, ":FRAME-SIZE");
  xsaddmsg(window, ":UNDO");
  xsaddmsg(window, ":CUT-TO-CLIP");
  xsaddmsg(window, ":COPY-TO-CLIP");
  xsaddmsg(window, ":PASTE-FROM-CLIP");
  xsaddmsg(window, ":CLEAR");
  xsaddmsg(window, ":PASTE-STREAM");
  xsaddmsg(window, ":PASTE-STRING");
  xsaddmsg(window, ":SELECTION-STREAM");
  xsaddmsg(window, ":ACTIVATE");
  xsaddmsg(window, ":UPDATE");
  xsaddmsg(window, ":FIND");
  
#ifdef MACINTOSH  
  /* EDIT-WINDOW-PROTO */
  edit_window = xsnewproto("EDIT-WINDOW-PROTO", window);
  xsaddinstanceslot(edit_window, "BIND-TO-FILE");
  xsaddinstanceslot(edit_window, "OUTPUT-STREAM");
  xsaddinstanceslot(edit_window, "INPUT-ENABLED");
  xsaddmsg(edit_window, ":ISNEW");
  xsaddmsg(edit_window, ":ALLOCATE");
  xsaddmsg(edit_window, ":CUT-TO-CLIP");
  xsaddmsg(edit_window, ":COPY-TO-CLIP");
  xsaddmsg(edit_window, ":PASTE-FROM-CLIP");
  xsaddmsg(edit_window, ":REVERT");
  xsaddmsg(edit_window, ":SAVE");
  xsaddmsg(edit_window, ":SAVE-AS");
  xsaddmsg(edit_window, ":SAVE-COPY");
  xsaddmsg(edit_window, ":PASTE-STREAM");
  xsaddmsg(edit_window, ":PASTE-STRING");
  xsaddmsg(edit_window, ":FLUSH-WINDOW");
  xsaddmsg(edit_window, ":SELECTION-STREAM");
  xsaddmsg(edit_window, ":CLOSE");
  xsaddmsg(edit_window, ":REMOVE");
  xsaddmsg(edit_window, ":DISPOSE");
  xsaddmsg(edit_window, ":ACTIVATE");
  xsaddmsg(edit_window, ":UPDATE");
  xsaddmsg(edit_window, ":FIND-STRING");

  /* LISTENER-PROTO */
  listener = xsnewproto("LISTENER-PROTO", edit_window);
  xsaddinstanceslot(listener, "INPUT-STREAM");
  xsaddmsg(listener, ":ISNEW");
  xsaddmsg(listener, ":ALLOCATE");
  xsaddmsg(listener, ":CLOSE");
  xsaddmsg(listener, ":REMOVE");
  xsaddmsg(listener, ":DISPOSE");
#endif /* MACINTOSH */

  /* MENU-PROTO */
  menu = xsnewproto("MENU-PROTO", hardware_object);
  xsaddinstanceslot(menu, "ID");
  xsaddinstanceslot(menu, "ENABLED");
  xsaddinstanceslot(menu, "ITEMS");
  xsaddinstanceslot(menu, "TITLE");
  xsaddslot(menu, "MENU-LIST");
  xsaddmsg(menu, ":ISNEW");
  xsaddmsg(menu, ":ALLOCATE");
  xsaddmsg(menu, ":DISPOSE");
  xsaddmsg(menu, ":INSTALL");
  xsaddmsg(menu, ":REMOVE");
  xsaddmsg(menu, ":ENABLED");
  xsaddmsg(menu, ":UPDATE");
  xsaddmsg(menu, ":ALLOCATED-P");
  xsaddmsg(menu, ":TITLE");
  xsaddmsg(menu, ":ITEMS");
  xsaddmsg(menu, ":INSTALLED-P");
  xsaddmsg(menu, ":APPEND-ITEMS");
  xsaddmsg(menu, ":DELETE-ITEMS");
  xsaddmsg(menu, ":SELECT");
  xsaddmsg(menu, ":POPUP");

#ifdef MACINTOSH
  /* APPLE-MENU-PROTO */
  apple_menu = xsnewproto("APPLE-MENU-PROTO", menu);
  xsaddmsg(apple_menu, ":ISNEW");
  xsaddmsg(apple_menu, ":SELECT");
#endif /* MACINTOSH */

  /* MENU-ITEM-PROTO */
  menu_item = xsnewproto("MENU-ITEM-PROTO", root_object);
  xsaddinstanceslot(menu_item, "MENU");
  xsaddinstanceslot(menu_item, "ENABLED");
  xsaddinstanceslot(menu_item, "ACTION");
  xsaddinstanceslot(menu_item, "STYLE");
  xsaddinstanceslot(menu_item, "MARK");
  xsaddinstanceslot(menu_item, "KEY");
  xsaddinstanceslot(menu_item, "TITLE");
  xsaddmsg(menu_item, ":ISNEW");
  xsaddmsg(menu_item, ":TITLE");
  xsaddmsg(menu_item, ":KEY");
  xsaddmsg(menu_item, ":MARK");
  xsaddmsg(menu_item, ":STYLE");
  xsaddmsg(menu_item, ":ACTION");
  xsaddmsg(menu_item, ":ENABLED");
  xsaddmsg(menu_item, ":INSTALLED-P");
  xsaddmsg(menu_item, ":UPDATE");
  xsaddmsg(menu_item, ":DO-ACTION");

  /* DIALOG-PROTO */
  dialog = xsnewproto("DIALOG-PROTO", window);
  xsaddinstanceslot(dialog, "DEFAULT-BUTTON");
  xsaddinstanceslot(dialog, "ITEMS");
  xsaddinstanceslot(dialog, "TYPE");
  xsaddslot(dialog, "DIALOG-LIST");
  xsaddmsg(dialog, ":ISNEW");
  xsaddmsg(dialog, ":ALLOCATE");
  xsaddmsg(dialog, ":REMOVE");
  xsaddmsg(dialog, ":DISPOSE");
  xsaddmsg(dialog, ":CLOSE");
  xsaddmsg(dialog, ":ALLOCATED-P");
  xsaddmsg(dialog, ":DEFAULT-BUTTON");
  xsaddmsg(dialog, ":MODAL-DIALOG");

  /* DIALOG-ITEM-PROTO */
  dialog_item = xsnewproto("DIALOG-ITEM-PROTO", root_object);
  xsaddinstanceslot(dialog_item, "DIALOG");
  xsaddinstanceslot(dialog_item, "ACTION");
  xsaddinstanceslot(dialog_item, "SIZE");
  xsaddinstanceslot(dialog_item, "LOCATION");
  xsaddinstanceslot(dialog_item, "TEXT");
#ifdef X11WINDOWS
  xsaddinstanceslot(dialog_item, "WINDOW-ID");
#endif /* X11WINDOWS */
  xsaddmsg(dialog_item, ":DO-ACTION");
  xsaddmsg(dialog_item, ":ACTION");

  /* BUTTON-ITEM-PROTO */
  button_item = xsnewproto("BUTTON-ITEM-PROTO", dialog_item);
  xsaddmsg(button_item, ":ISNEW");

  /* TOGGLE-ITEM-PROTO */
  toggle_item = xsnewproto("TOGGLE-ITEM-PROTO", dialog_item);
  xsaddinstanceslot(toggle_item, "VALUE");
  xsaddmsg(toggle_item, ":ISNEW");
  xsaddmsg(toggle_item, ":VALUE");

  /* TEXT-ITEM-PROTO */
  text_item = xsnewproto("TEXT-ITEM-PROTO", dialog_item);
  xsaddinstanceslot(text_item, "TEXT-LENGTH");
  xsaddinstanceslot(text_item, "EDITABLE");
  xsaddmsg(text_item, ":ISNEW");
  xsaddmsg(text_item, ":TEXT");

  /* CHOICE-ITEM-PROTO */
  choice_item = xsnewproto("CHOICE-ITEM-PROTO", dialog_item);
  xsaddinstanceslot(choice_item, "VALUE");
  xsaddmsg(choice_item, ":ISNEW");
  xsaddmsg(choice_item, ":VALUE");

  /* SCROLL-ITEM-PROTO */
  scroll_item = xsnewproto("SCROLL-ITEM-PROTO", dialog_item);
  xsaddinstanceslot(scroll_item, "VALUE");
  xsaddinstanceslot(scroll_item, "PAGE-INCREMENT");
  xsaddinstanceslot(scroll_item, "MAX-VALUE");
  xsaddinstanceslot(scroll_item, "MIN-VALUE");
  xsaddmsg(scroll_item, ":ISNEW");
  xsaddmsg(scroll_item, ":VALUE");
  xsaddmsg(scroll_item, ":MAX-VALUE");
  xsaddmsg(scroll_item, ":MIN-VALUE");
  xsaddmsg(scroll_item, ":SCROLL-ACTION");

  /* LIST-ITEM-PROTO */
  list_item = xsnewproto("LIST-ITEM-PROTO", dialog_item);
  xsaddinstanceslot(list_item, "COLUMNS");
  xsaddinstanceslot(list_item, "LIST-DATA");
  xsaddinstanceslot(list_item, "MULTIPLE");
  xsaddinstanceslot(list_item, "INTERNALS");
  xsaddmsg(list_item, ":ISNEW");
  xsaddmsg(list_item, ":DO-ACTION");
  xsaddmsg(list_item, ":SET-TEXT");
  xsaddmsg(list_item, ":SELECTION");

  /* GRAPH-WINDOW-PROTO */
  iview_window = xsnewproto("GRAPH-WINDOW-PROTO", window);
  xsaddinstanceslot(iview_window, "INTERNALS");
  xsaddinstanceslot(iview_window, "MENU-TITLE");
  xsaddinstanceslot(iview_window, "HAS-V-SCROLL");
  xsaddinstanceslot(iview_window, "HAS-H-SCROLL");
  xsaddinstanceslot(iview_window, "BLACK-ON-WHITE");
  xsaddinstanceslot(iview_window, "MENU");
  xssetslotval(iview_window, "GO-AWAY", s_true);
  xssetslotval(iview_window, "BLACK-ON-WHITE", s_true);
  initialize_graph_window(iview_window);

  xsaddmsg(iview_window, ":ISNEW");
  xsaddmsg(iview_window, ":ALLOCATE");

  xsaddmsg(iview_window, ":IDLE-ON");

  xsaddmsg(iview_window, ":MENU");

  xsaddmsg(iview_window, ":UPDATE");
  xsaddmsg(iview_window, ":ACTIVATE");
  xsaddmsg(iview_window, ":REMOVE");
  xsaddmsg(iview_window, ":DISPOSE");
  xsaddmsg(iview_window, ":CLOSE");
  xsaddmsg(iview_window, ":WHILE-BUTTON-DOWN");
  xsaddmsg(iview_window, ":SHOW-WINDOW");
  xsaddmsg(iview_window, ":NEW-MENU");
  xsaddmsg(iview_window, ":RESIZE");
  xsaddmsg(iview_window, ":REDRAW");
  xsaddmsg(iview_window, ":DO-IDLE");
  xsaddmsg(iview_window, ":DO-CLICK");
  xsaddmsg(iview_window, ":DO-MOTION");
  xsaddmsg(iview_window, ":DO-KEY");
  xsaddmsg(iview_window, ":DO-BUTTON-DOWN");
  
  xsaddmsg(iview_window, ":CANVAS-WIDTH");
  xsaddmsg(iview_window, ":CANVAS-HEIGHT");
  xsaddmsg(iview_window, ":LINE-TYPE");
  xsaddmsg(iview_window, ":DRAW-MODE");
  xsaddmsg(iview_window, ":DRAW-COLOR");
  xsaddmsg(iview_window, ":BACK-COLOR");
  xsaddmsg(iview_window, ":USE-COLOR");
  xsaddmsg(iview_window, ":REVERSE-COLORS");
  xsaddmsg(iview_window, ":VIEW-RECT");
  xsaddmsg(iview_window, ":LINE-WIDTH");
  xsaddmsg(iview_window, ":CLIP-RECT");
  xsaddmsg(iview_window, ":CURSOR");

  xsaddmsg(iview_window, ":HAS-H-SCROLL");
  xsaddmsg(iview_window, ":HAS-V-SCROLL");
  xsaddmsg(iview_window, ":SCROLL");
  xsaddmsg(iview_window, ":H-SCROLL-INCS");
  xsaddmsg(iview_window, ":V-SCROLL-INCS");

  xsaddmsg(iview_window, ":DRAW-LINE");
  xsaddmsg(iview_window, ":DRAW-POINT");
  xsaddmsg(iview_window, ":ERASE-RECT");
  xsaddmsg(iview_window, ":FRAME-RECT");
  xsaddmsg(iview_window, ":PAINT-RECT");
  xsaddmsg(iview_window, ":ERASE-OVAL");
  xsaddmsg(iview_window, ":FRAME-OVAL");
  xsaddmsg(iview_window, ":PAINT-OVAL");
  xsaddmsg(iview_window, ":ERASE-ARC");
  xsaddmsg(iview_window, ":FRAME-ARC");
  xsaddmsg(iview_window, ":PAINT-ARC");
  xsaddmsg(iview_window, ":ERASE-POLY");
  xsaddmsg(iview_window, ":FRAME-POLY");
  xsaddmsg(iview_window, ":PAINT-POLY");

  xsaddmsg(iview_window, ":TEXT-ASCENT");
  xsaddmsg(iview_window, ":TEXT-DESCENT");
  xsaddmsg(iview_window, ":TEXT-WIDTH");
  xsaddmsg(iview_window, ":DRAW-STRING");
  xsaddmsg(iview_window, ":DRAW-STRING-UP");
  xsaddmsg(iview_window, ":DRAW-TEXT");
  xsaddmsg(iview_window, ":DRAW-TEXT-UP");

  xsaddmsg(iview_window, ":DRAW-SYMBOL");
  xsaddmsg(iview_window, ":REPLACE-SYMBOL");

  xsaddmsg(iview_window, ":START-BUFFERING");
  xsaddmsg(iview_window, ":BUFFER-TO-SCREEN");

#ifdef MACINTOSH
  xsaddmsg(iview_window, ":COPY-TO-CLIP");
#endif /* MACINTOSH */
  xsaddmsg(iview_window, ":DRAG-GREY-RECT");
  xsaddmsg(iview_window, ":IMAGE-TO-FILE");
  xsaddmsg(iview_window, ":DRAW-BITMAP");
  
  /* GRAPH-PROTO */
  iview = xsnewproto("GRAPH-PROTO", iview_window);
  xsaddinstanceslot(iview, "SLICERS");
  xsaddinstanceslot(iview, "MOUSE-MODE");
  xsaddinstanceslot(iview, "FIXED-ASPECT");
  xsaddinstanceslot(iview, "SHOWING-LABELS");
  xsaddinstanceslot(iview, "VARIABLE-LABELS");
  xsaddinstanceslot(iview, "NUMBER-OF-VARIABLES");
  xsaddinstanceslot(iview, "MENU-TEMPLATE");
  xsaddinstanceslot(iview, "OPTIONS-TEMPLATE");
  xsaddinstanceslot(iview, "SCALE-TYPE");
  xsaddinstanceslot(iview, "OVERLAYS");
  xssetslotval(iview, "MOUSE-MODE", s_selecting);
  xsaddslot(iview, "MODE-LIST");
  initialize_graph(iview);

  xsaddmsg(iview, ":ISNEW");
  xsaddmsg(iview, ":ALLOCATE");

  xsaddmsg(iview, ":RESIZE");
  xsaddmsg(iview, ":REDRAW");
  xsaddmsg(iview, ":REDRAW-BACKGROUND");
  xsaddmsg(iview, ":CLEAR-CONTENT");
  xsaddmsg(iview, ":REDRAW-CONTENT");
  xsaddmsg(iview, ":REDRAW-OVERLAYS");
  xsaddmsg(iview, ":RESIZE-OVERLAYS");
  xsaddmsg(iview, ":OVERLAY-CLICK");
  xsaddmsg(iview, ":ADJUST-SCREEN");
  xsaddmsg(iview, ":ADJUST-POINTS-IN-RECT");
  xsaddmsg(iview, ":ADJUST-SCREEN-POINT");
  xsaddmsg(iview, ":MARK-POINTS-IN-RECT");

  xsaddmsg(iview, ":CONTENT-RECT");
  xsaddmsg(iview, ":CONTENT-ORIGIN");
  xsaddmsg(iview, ":CONTENT-VARIABLES");
  xsaddmsg(iview, ":CLICK-RANGE");
  xsaddmsg(iview, ":MOUSE-MODE");
  xsaddmsg(iview, ":SHOWING-LABELS");
  xsaddmsg(iview, ":MARGIN");
  xsaddmsg(iview, ":FIXED-ASPECT");
  xsaddmsg(iview, ":NEEDS-ADJUSTING");

  xsaddmsg(iview, ":X-AXIS");
  xsaddmsg(iview, ":Y-AXIS");

  xsaddmsg(iview, ":BRUSH");
  xsaddmsg(iview, ":ERASE-BRUSH");
  xsaddmsg(iview, ":DRAW-BRUSH");
  xsaddmsg(iview, ":MOVE-BRUSH");
  xsaddmsg(iview, ":RESIZE-BRUSH");

  xsaddmsg(iview, ":DO-CLICK");
  xsaddmsg(iview, ":DO-MOTION");
  xsaddmsg(iview, ":DO-SELECT-CLICK");
  xsaddmsg(iview, ":DO-BRUSH-CLICK");
  xsaddmsg(iview, ":DO-BRUSH-MOTION");
  xsaddmsg(iview, ":UNSELECT-ALL-POINTS");
  xsaddmsg(iview, ":ERASE-SELECTION");
  xsaddmsg(iview, ":MASK-SELECTION");
  xsaddmsg(iview, ":UNMASK-ALL-POINTS");
  xsaddmsg(iview, ":POINTS-SHOWING");
  xsaddmsg(iview, ":POINTS-HILITED");
  xsaddmsg(iview, ":POINTS-SELECTED");
  xsaddmsg(iview, ":SELECTION");
  xsaddmsg(iview, ":SHOW-ALL-POINTS");
  xsaddmsg(iview, ":ALL-POINTS-SHOWING-P");
  xsaddmsg(iview, ":ALL-POINTS-UNMASKED-P");
  xsaddmsg(iview, ":ANY-POINTS-SELECTED-P");

  xsaddmsg(iview, ":LINKED");
  xsaddmsg(iview, ":LINKS");

  xsaddmsg(iview, ":NUM-VARIABLES");
  xsaddmsg(iview, ":VARIABLE-LABEL");
  xsaddmsg(iview, ":RANGE");
  xsaddmsg(iview, ":SCALED-RANGE");
  xsaddmsg(iview, ":CANVAS-RANGE");
  xsaddmsg(iview, ":TRANSFORMATION");
  xsaddmsg(iview, ":APPLY-TRANSFORMATION");

  xsaddmsg(iview, ":ADD-POINTS");
  xsaddmsg(iview, ":CLEAR-POINTS");
  xsaddmsg(iview, ":NUM-POINTS");
  xsaddmsg(iview, ":POINT-COORDINATE");
  xsaddmsg(iview, ":POINT-CANVAS-COORDINATE");
  xsaddmsg(iview, ":POINT-TRANSFORMED-COORDINATE");
  xsaddmsg(iview, ":POINT-MASKED");
  xsaddmsg(iview, ":POINT-COLOR");
  xsaddmsg(iview, ":POINT-STATE");
  xsaddmsg(iview, ":LAST-POINT-STATE");
  xsaddmsg(iview, ":POINT-MARKED");
  xsaddmsg(iview, ":POINT-LABEL");
  xsaddmsg(iview, ":POINT-SYMBOL");
  xsaddmsg(iview, ":POINT-SELECTED");
  xsaddmsg(iview, ":POINT-HILITED");
  xsaddmsg(iview, ":POINT-SHOWING");

  xsaddmsg(iview, ":ADD-LINES");
  xsaddmsg(iview, ":CLEAR-LINES");
  xsaddmsg(iview, ":NUM-LINES");
  xsaddmsg(iview, ":LINESTART-COORDINATE");
  xsaddmsg(iview, ":LINESTART-CANVAS-COORDINATE");
  xsaddmsg(iview, ":LINESTART-TRANSFORMED-COORDINATE");
  xsaddmsg(iview, ":LINESTART-MASKED");
  xsaddmsg(iview, ":LINESTART-COLOR");
  xsaddmsg(iview, ":LINESTART-NEXT");
  xsaddmsg(iview, ":LINESTART-TYPE");
  xsaddmsg(iview, ":LINESTART-WIDTH");

#ifdef USESTRINGS
  xsaddmsg(iview, ":ADD-STRINGS");
  xsaddmsg(iview, ":CLEAR-STRINGS");
  xsaddmsg(iview, ":NUM-STRINGS");
  xsaddmsg(iview, ":STRING-COORDINATE");
  xsaddmsg(iview, ":STRING-CANVAS-COORDINATE");
  xsaddmsg(iview, ":STRING-TRANSFORMED-COORDINATE");
  xsaddmsg(iview, ":STRING-MASKED");
  xsaddmsg(iview, ":STRING-COLOR");
  xsaddmsg(iview, ":STRING-MODIFIERS");
#endif /* USESTRINGS */
  
  xsaddmsg(iview, ":DRAW-DATA-POINTS");
  xsaddmsg(iview, ":DRAW-DATA-LINES");
#ifdef USESTRINGS
  xsaddmsg(iview, ":DRAW-DATA-STRINGS");
#endif /* USESTRINGS */

  xsaddmsg(iview, ":ROTATE-2");

  xsaddmsg(iview, ":ADJUST-TO-DATA");
  xsaddmsg(iview, ":VISIBLE-RANGE");
  xsaddmsg(iview, ":SCALE-TO-RANGE");
  xsaddmsg(iview, ":SCALE");
  xsaddmsg(iview, ":SHIFT");

  xsaddmsg(iview, ":CLEAR-MASKS");
  xsaddmsg(iview, ":SLICE-VARIABLE");
  xsaddmsg(iview, ":REAL-TO-CANVAS");
  xsaddmsg(iview, ":CANVAS-TO-REAL");
  xsaddmsg(iview, ":SCALED-TO-CANVAS");
  xsaddmsg(iview, ":CANVAS-TO-SCALED");
  xsaddmsg(iview, ":POINTS-IN-RECT");
  xsaddmsg(iview, ":ADJUST-DEPTH-CUING");

  /* SPIN-PROTO */
  iview_spin = xsnewproto("SPIN-PROTO", iview);
  xsaddinstanceslot(iview_spin, "SHOWING-AXES");
  xsaddinstanceslot(iview_spin, "DEPTH-CUING");
  xsaddinstanceslot(iview_spin, "CONTENT-VARIABLES");
  xsaddinstanceslot(iview_spin, "ROTATION-TYPE");
  xsaddinstanceslot(iview_spin, "ROTATION-ANGLE");
  xsaddinstanceslot(iview_spin, "ROTATION-CONTROLS");
  initialize_graph(iview_spin);

  xsaddmsg(iview_spin, ":ALLOCATE");

  xsaddmsg(iview_spin, ":CONTENT-VARIABLES");
  xsaddmsg(iview_spin, ":SHOWING-AXES");
  xsaddmsg(iview_spin, ":DEPTH-CUING");
  xsaddmsg(iview_spin, ":RESIZE");
  xsaddmsg(iview_spin, ":REDRAW-CONTENT");  
  xsaddmsg(iview_spin, ":DO-IDLE");
  xsaddmsg(iview_spin, ":ANGLE");
  xsaddmsg(iview_spin, ":ROTATE");
  xsaddmsg(iview_spin, ":DRAW-AXES");

  /* SCATMAT-PROTO */
  iview_scatmat = xsnewproto("SCATMAT-PROTO", iview);
  initialize_graph(iview_scatmat);

  xsaddmsg(iview_scatmat, ":ALLOCATE");

  xsaddmsg(iview_scatmat, ":RESIZE");
  xsaddmsg(iview_scatmat, ":REDRAW-BACKGROUND");
  xsaddmsg(iview_scatmat, ":REDRAW-CONTENT");
  xsaddmsg(iview_scatmat, ":DO-CLICK");
  xsaddmsg(iview_scatmat, ":DO-MOTION");
  xsaddmsg(iview_scatmat, ":ADD-POINTS");
  xsaddmsg(iview_scatmat, ":ADD-LINES");
#ifdef USESTRINGS
  xsaddmsg(iview_scatmat, ":ADD-STRINGS");
#endif /* USESTRINGS */
  xsaddmsg(iview_scatmat, ":ADJUST-SCREEN-POINT");
  xsaddmsg(iview_scatmat, ":ADJUST-POINTS-IN-RECT");
  xsaddmsg(iview_scatmat, ":MARK-POINTS-IN-RECT");
  
  /* NAME-LIST-PROTO */
  iview_list = xsnewproto("NAME-LIST-PROTO", iview);
  initialize_graph(iview_list);

  xsaddmsg(iview_list, ":ALLOCATE");

  xsaddmsg(iview_list, ":REDRAW-BACKGROUND");
  xsaddmsg(iview_list, ":REDRAW-CONTENT");
  xsaddmsg(iview_list, ":ADD-POINTS");
  xsaddmsg(iview_list, ":ADJUST-SCREEN-POINT");
  xsaddmsg(iview_list, ":ADJUST-POINTS-IN-RECT");
  xsaddmsg(iview_list, ":MARK-POINTS-IN-RECT");

  /* HISTOGRAM-PROTO */
  iview_hist = xsnewproto("HISTOGRAM-PROTO", iview);
  xsaddinstanceslot(iview_hist, "HISTOGRAM-INTERNALS");
  initialize_graph(iview_hist);
  newhistinternals(iview_hist);

  xsaddmsg(iview_hist, ":ISNEW");
  xsaddmsg(iview_hist, ":ALLOCATE");

  xsaddmsg(iview_hist, ":ADD-POINTS");
  xsaddmsg(iview_hist, ":CLEAR-POINTS");
  xsaddmsg(iview_hist, ":RESIZE");
  xsaddmsg(iview_hist, ":REDRAW-CONTENT");
  xsaddmsg(iview_hist, ":ADJUST-SCREEN");
  xsaddmsg(iview_hist, ":NUM-BINS");
  xsaddmsg(iview_hist, ":BIN-COUNTS");
  xsaddmsg(iview_hist, ":ADJUST-TO-DATA");
  xsaddmsg(iview_hist, ":ADJUST-SCREEN-POINT");
  xsaddmsg(iview_hist, ":ADJUST-POINTS-IN-RECT");
  xsaddmsg(iview_hist, ":MARK-POINTS-IN-RECT");

  /* SCATTERPLOT-PROTO */
  scatterplot = xsnewproto("SCATTERPLOT-PROTO", iview);
  initialize_graph(scatterplot);

  xsaddmsg(scatterplot, ":ADD-POINTS");
  xsaddmsg(scatterplot, ":ADD-LINES");
#ifdef USESTRINGS
  xsaddmsg(scatterplot, ":ADD-STRINGS");
#endif /* USESTRINGS */
  xsaddmsg(scatterplot, ":ADJUST-TO-DATA");
#endif /* NOGRAPHICS */

  /* COMPOUND-DATA-PROTO */
  compound_data = xsnewproto("COMPOUND-DATA-PROTO", root_object);
  xsaddmsg(compound_data, ":DATA-LENGTH");
  xsaddmsg(compound_data, ":DATA-SEQ");
  xsaddmsg(compound_data, ":MAKE-DATA");
  xsaddmsg(compound_data, ":SELECT-DATA");
}
