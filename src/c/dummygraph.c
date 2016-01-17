/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

StInitGraphics() {}

StScreenHasColor() { return(0); }
StGetScreenSize() {}
StFlushGraphics(){}
StWSetTitle() {}
StHideWindow() {}
StShowWindow() {}
StHasWindows() { return(0); }

StWSetLocation() { xlfail("not supported"); }
StWGetLocation() { xlfail("not supported"); }
StWGetSize() { xlfail("not supported"); }
StWSetSize() { xlfail("not supported"); }
StGWSetSize() { xlfail("not supported"); }
StGWShowWindow() { xlfail("not supported"); }
IViewWindowGetObject() { xlfail("not supported"); }
IViewWindowWinInfo() { xlfail("not supported"); }
StGWWinInfoSize() { return(1); }
StGWInitWinInfo() {}
IViewGetNewBrushSize() { xlfail("not supported"); }
StGWInitialDraw() { xlfail("not supported"); }
StGWFrameRect() { xlfail("not supported"); }
StGWDrawMode() { xlfail("not supported"); }
StGWSetDrawMode() { xlfail("not supported"); }
StGWLineType() { xlfail("not supported"); }
StGWSetLineType() { xlfail("not supported"); }
StGWWhileButtonDown() { xlfail("not supported"); }

IViewWindowNew() { xlfail("not supported"); }
StGWSetIdleOn() { xlfail("not supported"); }
StGWSetObject() {}
StGWSetDrawColor() { xlfail("not supported"); }
StGWSetBackColor() { xlfail("not supported"); }
StGWGetScreenSize() { xlfail("not supported"); }
StGWSetHasHscroll() { xlfail("not supported"); }
StGWSetHasVscroll() { xlfail("not supported"); }
StGWIdleOn() { xlfail("not supported"); }
StGWRemove() { xlfail("not supported"); }
StGWSetTitle() { xlfail("not supported"); }
StGWSetUseColor() { xlfail("not supported"); }
StGWCanvasWidth() { xlfail("not supported"); }
StGWCanvasHeight() { xlfail("not supported"); }
StGWDrawColor() { xlfail("not supported"); }
StGWBackColor() { xlfail("not supported"); }
StGWUseColor() { xlfail("not supported"); }
StGWReverseColors() { xlfail("not supported"); }
StGWGetViewRect() { xlfail("not supported"); }
StGWSetLineWidth() { xlfail("not supported"); }
StGWGetLineWidth() { xlfail("not supported"); }
StGWHasHscroll() { xlfail("not supported"); }
StGWHasVscroll() { xlfail("not supported"); }
StGWSetScroll() { xlfail("not supported"); }
StGWGetScroll() { xlfail("not supported"); }
StGWSetHscrollIncs() { xlfail("not supported"); }
StGWSetVscrollIncs() { xlfail("not supported"); }
StGWGetHscrollIncs() { xlfail("not supported"); }
StGWGetVscrollIncs() { xlfail("not supported"); }
StGWDrawLine() { xlfail("not supported"); }
StGWEraseRect() { xlfail("not supported"); }
StGWPaintRect() { xlfail("not supported"); }
StGWEraseOval() { xlfail("not supported"); }
StGWFrameOval() { xlfail("not supported"); }
StGWPaintOval() { xlfail("not supported"); }
StGWTextAscent() { xlfail("not supported"); }
StGWTextWidth() { xlfail("not supported"); }
StGWDrawStringUp() { xlfail("not supported"); }
StGWDrawString() { xlfail("not supported"); }
StGWDrawTextUp() { xlfail("not supported"); }
StGWDrawText() { xlfail("not supported"); }
StGWDrawSymbol() { xlfail("not supported"); }
StGWReplaceSymbol() { xlfail("not supported"); }
StGWStartBuffering() { xlfail("not supported"); }
StGWBufferToScreen() { xlfail("not supported"); }
StGWSetClipRect() { xlfail("not supported"); }
StGWGetClipRect() { xlfail("not supported"); }
StGWSetCursor() { xlfail("not supported"); }
StGWCursor() { xlfail("not supported"); }
StGWResetBuffer() { xlfail("not supported"); }
StGWSetColRefCon() {}
StGWGetColRefCon() { xlfail("not supported"); }
StGWSetSymRefCon() {}
StGWGetSymRefCon() { xlfail("not supported"); }
StGWSetCursRefCon() {}
StGWGetCursRefCon() { xlfail("not supported"); }
StGWFreeCursor() { xlfail("not supported"); }
StGWMakeCursor() { xlfail("not supported"); }
StGWMakeResCursor() { xlfail("not supported"); }
StGWErasePoly() { xlfail("not supported"); }
StGWPaintArc() { xlfail("not supported"); }
StGWEraseArc() { xlfail("not supported"); }
StGWFrameArc() { xlfail("not supported"); }
StGWFreeColor() { xlfail("not supported"); }
StGWMakeColor() { xlfail("not supported"); }
StGWTextDescent() { xlfail("not supported"); }
StGWFramePoly() { xlfail("not supported"); }
StGWPaintPoly() { xlfail("not supported"); }
StGWGetRefCon() { xlfail("not supported"); }
StGWSetRefCon() { xlfail("not supported"); }
StGWSetFreeMem() { xlfail("not supported"); }
StGWIsActive() { xlfail("not supported"); }
StGWGetSymbolSize() { xlfail("not supported"); }
StGWDumpImage() { xlfail("not supported"); }
StGWDrawPoint() { xlfail("not supported"); }
StGWDrawBitmap() { xlfail("not supported"); }

DialogRemove() { xlfail("not supported"); }
DialogAllocate() { xlfail("not supported"); }
DialogSetDefaultButton() { xlfail("not supported"); }
DialogGetModalItem() { xlfail("not supported"); }
DialogButtonGetDefaultSize() { xlfail("not supported"); }
DialogToggleGetDefaultSize() { xlfail("not supported"); }
DialogToggleItemValue() { xlfail("not supported"); }
DialogTextGetDefaultSize() { xlfail("not supported"); }
DialogTextItemText() { xlfail("not supported"); }
DialogChoiceGetDefaultSize() { xlfail("not supported"); }
DialogChoiceItemValue() { xlfail("not supported"); }
DialogScrollGetDefaultSize() { xlfail("not supported"); }
DialogScrollItemValue() { xlfail("not supported"); }
DialogScrollItemMax() { xlfail("not supported"); }
DialogScrollItemMin() { xlfail("not supported"); }
DialogListGetDefaultSize() { xlfail("not supported"); }
DialogListItemSetText() { xlfail("not supported"); }
DialogListItemSelection() { xlfail("not supported"); }

StMObDisposeMach () {}
StMObDeleteItem () {}
StMObSetItemProp () {}
StMObAppendItems () {}
StMObAllocateMach () {}
StMObRemove () {}
StMObInstalled () { return(0); }
StMObEnable () {}
StMObInstall () {}
StMObPopup () {}

