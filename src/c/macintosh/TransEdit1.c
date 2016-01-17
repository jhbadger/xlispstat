/*
 * - Check that SaveFile puts correct initial name in.
 * - Make sure that filename gets initialized properly in all cases.
 */

/*
 * TransEdit.c version 3.05 - TransSkel plug-in module supporting an
 * arbitrary number of generic edit windows.  Each window may be
 * bound to a file.
 * 
 * *** Requires FakeAlert.c for proper linking! ***
 *
 * TransSkel and TransEdit are public domain.  For more information,
 * contact:
 *
 * 			Paul DuBois
 * 			Wisconsin Regional Primate Research Center
 * 			1220 Capitol Court
 * 			Madison, WI  53715-1299  USA
 *
 * Internet:	dubois@primate.wisc.edu
 *		
 *
 * This version of TransEdit is written for THINK C 6.0.
 * THINK C is a trademark of:
 *
 * 			Symantec Corporation
 * 			10201 Torre Avenue
 * 			Cupertino, CA 95014  USA
 *
 * History
 * 08/25/86	Genesis.  Beta version.
 * 09/15/86
 * - Changed to allow arbitrary number of windows.
 *
 * 11/04/86 Release 1.0
 * - Added conditional stuff to allow compilation in single- or
 * multiple-window mode.
 *
 * 01/17/87 Release 1.01
 * - The window type when a new window is created is documentProc+8
 * now, so that the window will have a zoom box on a machine with
 * 128K ROMS.  Default file name in SaveFile() is initialized after
 * SyncAllGlobals() - fixing bug found by Owen Hartnett.
 *
 * 01/29/89 Release 2.0
 * - Converted to work with TransSkel 2.0.  2-byte and
 * 4-byte integer types are typedef'ed to Integer and LongInt to
 * ease porting.  Added SystemEdit() check to Edit menu handler.
 *
 * 16 Jun 92 Release 3.01
 * - Ported for TransSkel 3.00. Basically this amounted to adding
 * function prototypes.
 * - Use real curly quotes in FakeAlert() messages.
 *
 * 05 Jun 93 Release 3.02
 * - Conversion for THINK C 6.0.
 *
 * 08 Jun 93 Release 3.03
 * - Took out all the stuff to allow compiling to handle only a single
 * edit window.  The savings in bytes of object code is no longer worth
 * the extra source code complexity.  It's also unnecessary because I no longer
 * maintain a window list, since I ...
 * - Reimplemented linked list holding edit window data using TransSkel's
 * window property functions (new in TS 3.00).  The property type is
 * skelWPropEditWind, and the data value is a handle to the window data
 * structure.
 * - Took out all the "register" declarations.  The compiler's smart enough
 * now that they don't make any difference, so they're just clutter.
 * 05 Jul 93
 * - Redid FakeAlert() calls to correspond better to new button positioning.
 * (See FakeAlert.c remarks.)
 * - Fixed Activate() and Update() so that when a window goes inactive,
 * the scroll bar is hidden (and only the frame drawn).  Previously, the
 * scroll bar was just made inactive, which doesn't quite conform to the user
 * interface guidelines.
 * 18 Dec 93
 * - Untitled windows have "untitled" rather than "Untitled" in title bar,
 * and first untitled window isn't given a number, in accordance with Apple
 * guidelines.
 * - Replaced SFGetFile()/SFPutFile() with SFPGetFil()/SFPPutFile() so can
 * use SkelDlogFilter().
 * 21 Dec 93
 * - Use color grafports when available.
 * 04 Jan 94
 * - Undid Integer/LongInt type stuff back to short/long.
 *
 * 18 Jan 94 Release 3.04
 * - FakeAlert() code revamped.  See FakeAlert.c.
 * 19 Jan 94
 * - Window creation routines do better checks for allocation failures.
 * - Much rewriting to eliminate many global variables.  No more SyncGlobals().
 * 20 Jan 94
 * - Fixed bug with missing SkelRmveDlogFilter() after SFPGetFile() call.
 * 23 Jan 94
 * - Fixed bug in EWindowEditOp(); edit window doesn't need to be marked dirty
 * for Copy operation.
 *
 * 21 Feb 94 Release 3.05
 * - Updated for TransSkel 3.11.
 * - Converted interface to be Pascal-compatible.
 */

# include	"TransSkel1.h"

# include	"TransEdit1.h"


# define	normalHilite	0
# define	dimHilite		255

/*
 * Maximum size of file we'll attempt to read.  The value leaves
 * a little cushion against the TextEdit limit of 32767.
 */

# define	maxFileSize		32000


# define	WindowIsActive(w)	((WindowPeek) (w))->hilited


/*
 * New(TypeName) returns handle to new object, for any TypeName.
 * If there is insufficient memory, the result is nil.
 */

# define	New(type)	(type **) NewHandle ((Size) sizeof (type))


# define	enter			3
# define	cr				13


typedef enum			/* Edit menu item numbers */
{
	undo = 1,
	/* --- */
	cut = 3,
	copy,
	paste,
	clear		/* (it's ok if the host doesn't have this item) */
};


/*
 * Default values for edit window text display characteristics
 * and event notification procedures.  Used when new edit windows
 * are created.
 */

static short	e_font = kFontIDMonaco;			/* default font                 */
static short	e_size = 9;						/* default pointsize            */
static short	e_style = normal;				/* default style -- L. Tierney  */
static short	e_wrap = 0;						/* default word wrap (on)       */
static short	e_just = teJustLeft;			/* default justification        */
static TEditKeyProcPtr		e_key = nil;		/* default key procedure        */
static TEditActivateProcPtr	e_activate = nil;	/* default activation procedure */
static TEditCloseProcPtr	e_close = nil;		/* default close procedure      */
static TEditCloseProcPtr	e_idle = nil;		/* default idle procedure -- L. Tierney */


/*
 * Edit window document record.  A handle to a window's document
 * record is stored in the window's property list.
 */

typedef struct DocRecord	DocRecord, *DocPtr, **DocHandle;

struct DocRecord
{
	WindowPtr		editWind;	/* the edit window                   */
	Boolean			bound;		/* whether window is bound to file   */
	SFReply			editFile;	/* file it's bound to, if bound true */
	TEHandle		editTE;		/* window text                       */
	Boolean			dirty;		/* whether text modified since save  */
	ControlHandle	eScroll;	/* scroll bar                        */
	short			visLines;	/* # lines visible in window, max    */
	TEditKeyProcPtr			eKey;		/* key click notifier        */
	TEditActivateProcPtr	eActivate;	/* activate event notifier   */
	TEditCloseProcPtr		eClose;		/* close notifier            */
	TEditIdleProcPtr		eIdle;		/* idle action -- L. Tierney */
};


/*
 * Macros for accessing parts of a document record, given a document
 * handle.
 */

# define	DocWind(doc)			((**doc).editWind)
# define	DocBound(doc)			((**doc).bound)
# define	DocFile(doc)			((**doc).editFile)
# define	DocTE(doc)				((**doc).editTE)
# define	DocDirty(doc)			((**doc).dirty)
# define	DocScroll(doc)			((**doc).eScroll)
# define	DocVisLines(doc)		((**doc).visLines)
# define	DocKeyProc(doc)			((**doc).eKey)
# define	DocActivateProc(doc)	((**doc).eActivate)
# define	DocCloseProc(doc)		((**doc).eClose)
# define	DocIdleProc(doc)		((**doc).eIdle) /* L. Tierney */


static Point	dlogWhere = { 70, 100 };	/* GetFile/PutFile location */
static OSType	creator = 'TEDT';			/* default file creator */

static RgnHandle	clipRgn = (RgnHandle) nil;


/* -------------------------------------------------------------------- */
/*				Miscellaneous Internal (private) Routines				*/
/* -------------------------------------------------------------------- */


static void
ErrMesg (StringPtr s)
{
	(void) FakeAlert (s, "\p", "\p", "\p", 1, 1, 0, "\pOK", "\p", "\p");
}


/*
 * Save and restore the current window's clip region
 */

static void
SaveClipRgn (void)
{
	clipRgn = NewRgn ();
	GetClip (clipRgn);
}


static void
RestoreClipRgn (void)
{
	SetClip (clipRgn);
	DisposeRgn (clipRgn);
}


/*
 * Draw grow box in lower right hand corner of window.
 */

static void
DrawGrowBox (WindowPtr w)
{
Rect	r;

	SaveClipRgn ();
	r = w->portRect;
	r.left = r.right - 15;		/* draw only in corner */
	r.top = r.bottom - 15;
	ClipRect (&r);
	DrawGrowIcon (w);
	RestoreClipRgn ();
}


/*
 * Generate title for new untitled document.  First one is
 * unnumbered, others are numbered.
 */

static void
Untitled (StringPtr	name)
{
static long	num = 0;
Str255		numBuf;

	BlockMove ("\puntitled", name, 9L);
	if (++num > 1)
	{
		name[++name[0]] = ' ';
		NumToString (num, numBuf);
		BlockMove (&numBuf[1], &name[name[0]+1], (long) numBuf[0]);
		name[0] += numBuf[0];
	}
}


/* -------------------------------------------------------------------- */
/*			Lowest-level Internal (Private) Edit Window Routines		*/
/* -------------------------------------------------------------------- */


/*
 * Get document handle associated with edit window.
 * Return nil if window isn't a known edit window.
 */

static DocHandle
WindDocHandle (WindowPtr w)
{
SkelWindPropHandle	ph;
DocHandle	doc = (DocHandle) nil;

	if (w != (WindowPtr) nil)
	{
		ph = SkelGetWindProp (w, skelWPropEditWind);
		if (ph != (SkelWindPropHandle) nil)
			doc = (DocHandle) (**ph).skelWPropData;
	}
	return (doc);
}


/* -------------------------------------------------------------------- */
/*					Internal (private) Display Routines					*/
/* -------------------------------------------------------------------- */


/*
 * Calculate the dimensions of the editing rectangle for a window
 * (which is assumed to be the current port).  (The viewRect and
 * destRect are the same size.)  Assumes the port, text font and
 * text size are all set properly.  The viewRect is sized so that
 * an integral number of lines can be displayed in it, i.e., so that
 * a partial line never shows at the bottom.  If that's not done,
 * funny things can happen to the caret.
 */

static void
CalcEditRect (WindowPtr w, Rect *r)
{
FontInfo	f;
short		lineHeight;

	GetFontInfo (&f);
	lineHeight = f.ascent + f.descent + f.leading;
	*r = w->portRect;
	r->left += 4;
	r->right -= 17;			/* leave room for scroll bar + 2 */
	r->top += 2;
	r->bottom = r->top + ((r->bottom - r->top - 2) / lineHeight) * lineHeight;
}


/*
 * Calculate the dimensions of the scroll bar rectangle for a
 * window.  Make sure that the edges overlap the window frame and
 * the grow box.
 */

static void
CalcScrollRect (WindowPtr w, Rect *r)
{
	*r = w->portRect;
	++r->right;
	--r->top;
	r->left = r->right - 16;
	r->bottom -= 14;
}


/*
 * Return true if the mouse is in the non-scrollbar part of an
 * edit window.
 */

static Boolean
PtInText (WindowPtr w, Point pt)
{
Rect	r;

	r = w->portRect;
	r.right -= 15;
	return (PtInRect (pt, &r));
}


/*
 * Set the cursor appropriately.  If theCursor == iBeamCursor, check
 * that it's really in the text area of an edit window (and if not
 * set the cursor to an arrow instead).  Otherwise, set the cursor
 * to the given type (usually a watch).
 *
 * Pass -1 for theCursor to set the cursor to the arrow.
 */

static void
FixCursor (short theCursor)
{
WindowPtr	w;
GrafPtr		savePort;
Point		pt;

	if (theCursor == iBeamCursor)			/* check whether there's an edit */
	{										/* window in front and if so,    */
		theCursor = -1;						/* whether the cursor's in its   */
		w = FrontWindow ();					/* text area                     */
		if (IsEWindow (w))
		{
			GetPort (&savePort);
			SetPort (w);
			GetMouse (&pt);
			if (PtInText (w, pt))
				theCursor = iBeamCursor;
			SetPort (savePort);
		}
	}
	SetCursor (theCursor == -1 ? &arrow : *GetCursor (theCursor));
}


/*
 * Calculate the number of lines currently scrolled off
 * the top of an edit record.
 */

static short
LinesOffTop (TEHandle hTE)
{
	return (((**hTE).viewRect.top - (**hTE).destRect.top) / (**hTE).lineHeight);
}


/*
 * Return the line number that the caret (or the beginning of
 * the currently selected text) is in.  Value returned is in
 * the range 0..(**editTE).nLines.  If = (**editTE).nLines, the
 * caret is past the last line.  The only special case to watch out
 * for is when the caret is at the very end of the text.  If the
 * last character is not a carriage return, then the caret is on
 * the (nLines-1)th line, not the (nLines)th line.
 *
 * (This really should do a binary search for speed.)
 */

static short
LineWithCaret (TEHandle hTE)
{
short	i;
short	nLines;
short	teLength;
short	selStart;
short	lineStart;

	selStart = (**hTE).selStart;
	nLines = (**hTE).nLines;
	teLength = (**hTE).teLength;

	if (selStart == teLength)
	{
		if (teLength != 0 && (*((**hTE).hText))[teLength-1] != cr)
			return (nLines - 1);
		return (nLines);
	}

	for (i = 0; /* empty */; ++i)
	{
		if ((lineStart = (**hTE).lineStarts[i]) >= selStart)
		{
			if (lineStart != selStart)
				--i;
			return (i);
		}
	}
}


/*
 * Return the number of the last displayable line.  That's one
 * more than nLines if the text is empty or the last character
 * is a carriage return.
 */

static short
LastLine (TEHandle hTE)
{
short	nLines;
short	teLength;

	nLines = (**hTE).nLines;
	teLength = (**hTE).teLength;

	if (teLength == 0 || (*((**hTE).hText))[teLength-1] == cr)
		nLines++;
	return (nLines);
}


/*
 * Highlight the scroll bar properly.  This means that it's not
 * made active if the window itself isn't active, even if
 * there's enough text to fill the window.
 */

static void
HiliteScroll (DocHandle doc)
{
WindowPtr	w;
ControlHandle	scroll;
short	hilite;

	w = DocWind (doc);
	scroll = DocScroll (doc);
	hilite = (WindowIsActive (w) && GetControlMaximum (scroll) > 0
				? normalHilite : dimHilite);
	HiliteControl (scroll, hilite);
}


/*
 * Set scroll bar current value (but only if it's different than
 * the current value, to avoid needless flashing).
 */

static void
SetScrollValue (ControlHandle scroll, short value)
{
	if (GetControlValue (scroll) != value)
		SetControlValue (scroll, value);
}


/*
 * Set the maximum value of the scroll bar.  It's set so that if
 * there's more text than fits in the window, the bottom line can
 * be scrolled up at least a little below the bottom of the window.
 *
 * The shenanigans with topLines and scrollableLines have to do with
 * the case where there may be less text than fills the window, but
 * most of it's scrolled off the top.  This can happen when you
 * scroll a bunch of stuff up, then delete everything visible in
 * the window.
 */

static void
SetScrollMax (DocHandle doc)
{
ControlHandle	scroll;
TEHandle		hTE;
short	topLines;
short	scrollableLines;
short	max;

	scroll = DocScroll (doc);
	hTE = DocTE (doc);

	topLines = LinesOffTop (hTE);
	scrollableLines = LastLine (hTE) - DocVisLines (doc);;
	max = (topLines > scrollableLines ? topLines : scrollableLines);

	if (max < 0)
		max = 0;

	if (max != GetControlMaximum (scroll))
	{
		SetControlMaximum (scroll, max);
		HiliteScroll (doc);
	}
}


/*
 * Scroll to the correct position.  lDelta is the
 * amount to CHANGE the current scroll setting by.
 * Positive scrolls the text up, negative down.
 */

static void
ScrollText (DocHandle doc, short lDelta)
{
ControlHandle	scroll;
TEHandle	hTE;
short	topVisLine;
short	newTopVisLine;

	scroll = DocScroll (doc);
	hTE = DocTE (doc);

	topVisLine = LinesOffTop (hTE);
    newTopVisLine = topVisLine + lDelta;
    if (newTopVisLine < 0)					/* clip to range */
    	newTopVisLine = 0;
    if (newTopVisLine > GetControlMaximum (scroll))
		newTopVisLine = GetControlMaximum (scroll);
    SetScrollValue (scroll, newTopVisLine);
    TEScroll (0, (topVisLine - newTopVisLine ) * (**hTE).lineHeight, hTE);
}


/*
 * Scroll to home position without redrawing.
 */

static void
ScrollToHome (TEHandle hTE)
{
Rect	r;

	r = (**hTE).destRect;
	OffsetRect (&r, 0, 2 - r.top);
	(**hTE).destRect = r;
}


/*
 * ClikLoop proc for autoscrolling text when the mouse is dragged out
 * of the text view rectangle.
 *
 * The clipping region has to be set to include the scroll bar,
 * because whenever this proc is called, TextEdit has the region set
 * to the view rectangle - if it's not reset, changes to the scroll
 * bar won't show up!
 *
 * AutoScroll() uses scrollDoc, which must be set in Mouse() before
 * calling TEClick(), which calls AutoScroll().
 */


static DocHandle	scrollDoc;
static short		scrollPart;

static pascal Boolean
AutoScroll (void)
{
WindowPtr	w;
TEHandle	hTE;
Point	p;
Rect	r;

	w = DocWind (scrollDoc);
	hTE = DocTE (scrollDoc);

	SaveClipRgn ();
	ClipRect (&w->portRect);
	GetMouse (&p);
	r = (**hTE).viewRect;
	if (p.v < r.top)
		ScrollText (scrollDoc, -1);	/* back one line */
	else if (p.v > r.bottom)
		ScrollText (scrollDoc, 1);	/* forward one line */
	RestoreClipRgn ();
	return (true);			/* true = 'keep tracking mouse' */
}


/*
 * Filter proc for tracking mousedown in scroll bar.
 *
 * I suspect odd scrolling may occur for hits in paging regions if
 * the window is allowed to size such that less than two lines show.
 *
 * TrackScroll() uses scrollDoc and scrollPart, which must be set in
 * Mouse() before calling TrackControl(), which calls TrackScroll().
 * scrollPart is the original part code in which the mousedown occurred.
 */

static pascal void
TrackScroll (ControlHandle theScroll, short partCode)
{
short	lDelta;
short	visLines = DocVisLines (scrollDoc);

    if (partCode == scrollPart)	/* still in same part? */
    {
        switch (partCode)
        {
            case kControlUpButtonPart: lDelta = -1; break;
            case kControlDownButtonPart: lDelta = 1; break;
            case kControlPageUpPart: lDelta = -(visLines - 1); break;
            case kControlPageDownPart: lDelta = visLines - 1; break;
        }
        ScrollText (scrollDoc, lDelta);
    }
}


/*
 * Set the scroll bar properly and adjust the text in the
 * window so that the line containing the caret is visible.
 * If the line with the caret is more than a line outside of
 * the viewRect, try to place it in the middle of the window.
 *
 * Yes, it is necessary to call SetScrollMax() at the end.
 */

static void
AdjustDisplay (DocHandle doc)
{
ControlHandle	scroll;
TEHandle	hTE;
short		visLines;
short	caretLine;
short	topVisLine;
short	d;

	scroll = DocScroll (doc);
	hTE = DocTE (doc);
	visLines = DocVisLines (doc);

	SetScrollMax (doc);
	caretLine = LineWithCaret (hTE);
	topVisLine = LinesOffTop (hTE);
	if ((d = caretLine - topVisLine) < 0)
		ScrollText (doc, d == -1 ? -1 : d - visLines / 2);
	else if (( d = caretLine - (topVisLine + visLines - 1)) > 0)
		ScrollText (doc, d == 1 ? 1 : d + visLines / 2);
	else
		SetScrollValue (scroll, topVisLine);
	SetScrollMax (doc);	/* might have changed from scrolling */
}


/*
 * Overhaul the entire display.  This is called after catastrophic
 * events, such as resizing the window, or changes to the word
 * wrap style.  It makes sure the view and destination rectangles
 * are sized properly, and that the bottom line of text never
 * scrolls up past the bottom line of the window (if there's
 * enough to fill the window), and that the scroll bar max and
 * current values are set properly.
 *
 * Resizing the dest rect just means resetting the right edge
 * (the top is NOT reset), since text might be scrolled off the
 * top (i.e., destRect.top != 0).
 *
 * Doesn't redraw the control, though!
 */

static void
OverhaulDisplay (DocHandle doc, Boolean showCaret, Boolean recalc)
{
TEHandle	hTE;
Rect		r;

	hTE = DocTE (doc);
	r = (**hTE).viewRect;		/* erase current viewRect */
	EraseRect (&r);
	CalcEditRect (DocWind (doc), &r);		/* calculate new viewRect */
	(**hTE).destRect.right = r.right;
	(**hTE).viewRect = r;
	if (recalc)
		TECalText (hTE);			/* recalculate line starts */
	DocVisLines (doc) = (r.bottom - r.top) / (**hTE).lineHeight;

	/*
	 * If there is text, but none of it is visible in the window
	 * (it's all scrolled off the top), pull some down.
	 */

	if (showCaret)
		AdjustDisplay (doc);
	else
		SetScrollMax (doc);

	TEUpdate (&r, hTE);
}


/* ---------------------------------------------------------------- */
/*						Window Handler Routines						*/
/* ---------------------------------------------------------------- */


/*
 * Handle mouse clicks in window.  The viewRect is never tested
 * directly, because if it were, clicks along the top, left and
 * bottom edges of the window wouldn't register.
 */

static ControlActionUPP TrackScrollUPP = NULL;

static pascal void
Mouse (Point thePt, long t, short mods)
{
WindowPtr	w;
DocHandle	doc;
ControlHandle	scroll;
short	thePart;
short	oldCtlValue;

	GetPort (&w);
	doc = WindDocHandle (w);
	scroll = DocScroll (doc);

	if ((thePart = TestControl (scroll, thePt)) == kControlIndicatorPart)
	{
		oldCtlValue = GetControlValue (scroll);
		if (TrackControl (scroll, thePt, nil) == kControlIndicatorPart)
			ScrollText (doc, GetControlValue (scroll) - oldCtlValue);
	}
	else if (thePart != 0)
	{
		scrollDoc = doc;		/* set globals for TrackScroll */
		scrollPart = thePart;
		if (! TrackScrollUPP)
			TrackScrollUPP = NewControlActionProc((ProcPtr) TrackScroll);
		(void) TrackControl (scroll, thePt, TrackScrollUPP);
	}
	else if (PtInText (w, thePt))
	{
		scrollDoc = doc;		/* set global for AutoScroll */
		TEClick (thePt, (mods & shiftKey) != 0, DocTE (doc));
	}

	SetScrollMax (doc);
}


/*
 * Handle key clicks in window
 */

static pascal void
Key (short c, short code, short mods)
{
WindowPtr	w;
DocHandle	doc;

	w = FrontWindow(); /* changed from GetPort (&w) -- L. Tierney */
	doc = WindDocHandle (w);

	if (c != enter)
		TEKey (c, DocTE (doc));
	AdjustDisplay (doc);
	DocDirty (doc) = true;
	if (DocKeyProc (doc) != nil)	/* report event to the host */
		(*DocKeyProc (doc)) ();
}


/*
 * Update window.  The update event might be in response to a
 * window resizing.  If so, move and resize the scroll bar,
 * and recalculate the text display.
 *
 * The ValidRect call is done because the HideControl adds the
 * control bounds box to the update region - which would generate
 * another update event!  Since everything is redrawn below anyway,
 * the ValidRect is used to cancel the update.
 */

static pascal void
Update (Boolean resized)
{
WindowPtr	w;
DocHandle	doc;
ControlHandle	scroll;
TEHandle	hTE;
Rect	r;

	GetPort (&w);
	doc = WindDocHandle (w);
	scroll = DocScroll (doc);
	hTE = DocTE (doc);

	if (resized)
	{
		r = w->portRect;
		EraseRect (&r);
		HideControl (scroll);
		r = (**scroll).contrlRect;
		ValidRect (&r);
		CalcScrollRect (w, &r);
		SizeControl (scroll, 16, r.bottom - r.top);
		MoveControl (scroll, r.left, r.top);
		OverhaulDisplay (doc, false, (**hTE).crOnly >= 0);
		ShowControl (scroll);
	}
	else
	{
		OverhaulDisplay (doc, false, false);
		if (WindowIsActive (w))
			DrawControls (w);	/* redraw scroll bar */
		else
		{
			/* draw outline of scroll, erase interior */
			r = (**scroll).contrlRect;
			FrameRect (&r);
			InsetRect (&r, 1, 1);
			EraseRect (&r);
		}
	}

	DrawGrowBox (w);
}


/*
 * When the window comes active, highlight the scroll bar appropriately.
 * When the window is deactivated, hide the scroll bar (this is drawn
 * immediately rather than invalidating the rectangle and waiting for
 * Update(), because that just seems too slow).
 *
 * Redraw the grow box in any case.  Set the cursor (FixCursor avoids
 * changing it from an ibeam to an arrow back to an ibeam, in the case
 * where one edit window is going inactive and another is coming
 * active).
 *
 * Report the event to the host.
 */

static pascal void
Activate (Boolean active)
{
WindowPtr	w;
DocHandle	doc;
ControlHandle	scroll;
TEHandle	hTE;
RgnHandle	oldClip;
Rect		r;

	GetPort (&w);
	doc = WindDocHandle (w);
	scroll = DocScroll (doc);
	hTE = DocTE (doc);

	DrawGrowBox (w);
	if (active)
	{
   		TEActivate (hTE);
		HiliteScroll (doc);
		ShowControl (scroll);
	}
	else
	{
		TEDeactivate (hTE);
		/* hide scroll but don't show it being hidden */
		oldClip = NewRgn ();
		GetClip (oldClip);
		SetRect (&r, 0, 0, 0, 0);
		ClipRect (&r);
		HideControl (scroll);
		SetClip (oldClip);
		DisposeRgn (oldClip);
		/* now erase inside of scroll (but not outline, to avoid flicker) */
		r = (**scroll).contrlRect;		/* erase scroll */
		InsetRect (&r, 1, 1);			/* but not outline */
		EraseRect (&r);
	}
	FixCursor (iBeamCursor);
	if (DocActivateProc (doc) != nil)	/* report event to the host */
		(*DocActivateProc (doc)) (active);
}


/*
 * Close box was clicked.  If user specified notify proc, call it.
 * Otherwise do default close operation (ask about saving if dirty,
 * etc.).
 */

static pascal void
Close (void)
{
WindowPtr	w;
DocHandle	doc;

	GetPort (&w);
	doc = WindDocHandle (w);

	if (DocCloseProc (doc) != nil)
		(*DocCloseProc (doc)) ();
	else
		(void) EWindowClose (w);
}


/*
 * Clobber an edit window.  This routine is written defensively on the
 * assumption that not all pieces of a complete edit window are present.
 * This allows it to be called by SkelRmveWind() during window creation
 * attempts if allocations fail.
 *
 * The window's skelWPropEditWind property structure will be disposed
 * of by TransSkel, but the data associated with it (returned by
 * WindDocHandle()) must be disposed of here.
 *
 * At this point it's too late to back out if any changes have been
 * made to the text.
 */

static pascal void
Clobber (void)
{
WindowPtr	w;
DocHandle	doc;
TEHandle	hTE;

	GetPort (&w);
	doc = WindDocHandle (w);

	/*
	 * Toss document record and any pieces that exist
	 */
	if (doc != (DocHandle) nil)
	{
		if ((hTE = DocTE (doc)) != (TEHandle) nil)
			TEDispose (hTE);						/* toss text record */
		DisposeHandle ((Handle) doc);
	}
	DisposeWindow (w);			/* toss window (scroll bar, too) */
	FixCursor (iBeamCursor);
}


/*
 * Blink the caret and make sure the cursor's an i-beam when it's
 * in the non-scrollbar part of the window.
 */

static pascal void
Idle (void)
{
WindowPtr	w;
DocHandle	doc;

	GetPort (&w);
	doc = WindDocHandle (w);

	if (DocIdleProc (doc) != nil)
		(*DocIdleProc (doc)) ();    /* L. Tierney */
	TEIdle (DocTE (doc));			/* blink that caret! */
	FixCursor (iBeamCursor);
}


/* ---------------------------------------------------------------- */
/*						Internal File Routines						*/
/* ---------------------------------------------------------------- */


/*
 * Save the contents of the edit window.  If there is no file bound
 * to the window, ask for a file name.  If askForFile is true, ask
 * for a name even if the window is currently bound to a file.  If
 * bindToFile is true, bind the window to the file written to (if
 * that's different than the currently bound file), and clear the
 * window's dirty flag.
 *
 * Return true if the file was written without error.  Return false
 * if (a) user was asked for name and clicked Cancel (b) there was
 * some error writing the file.  In the latter case, the window is
 * not bound to any new name given by user.
 *
 * Always returns false if the window isn't an edit window.  This
 * simplifies EWindowSave, EWindowSaveAs, EWindowSaveCopy.  (They
 * don't do the test.)
 */

static Boolean
SaveFile (WindowPtr w, Boolean askForFile, Boolean bindToFile)
{
DocHandle	doc;
TEHandle	hTE;
short	f;
FInfo	fndrInfo;	/* finder info */
SFReply	tmpFile;
Handle	hText;
long	count;
OSErr	result;
Boolean	haveNewFile = false;

	if (!IsEWindow (w))
		return (false);

	doc = WindDocHandle (w);
	hTE = DocTE (doc);

	tmpFile = DocFile (doc);					/* initialize default name */
	if (DocBound (doc) == false || askForFile)
	{
		/*SFPPutFile (dlogWhere, "\pSave file as:", editFile.fName, nil, &tmpFile,
				putDlgID, SkelDlogFilter (nil, false));*/
		SFPPutFile (dlogWhere, "\pSave file as:", tmpFile.fName, nil, &tmpFile,
				putDlgID, SkelDlogFilter (nil, false));
		SkelRmveDlogFilter ();
		SkelDoUpdates ();
		if (!tmpFile.good)
			return (false);
		else
		{
			haveNewFile = true;
			if (GetFInfo (tmpFile.fName, tmpFile.vRefNum, &fndrInfo)
													== noErr) /* exists */
			{
				if (fndrInfo.fdType != 'TEXT')
				{
					ErrMesg ("\pNot a TEXT File");
					return (false);
				}
			}
			else	/* doesn't exist.  create it. */
			{
				if (Create (tmpFile.fName, tmpFile.vRefNum,
							creator, 'TEXT') != noErr)
				{
					ErrMesg ("\pCan't Create");
					return (false);
				}
			}
		}
	}
	
	if (FSOpen (tmpFile.fName, tmpFile.vRefNum, &f) != noErr)
		ErrMesg ("\pCan't Open");
	else
	{
		FixCursor (watchCursor);
		(void) SetFPos (f, fsFromStart, 0L);
		hText = (**hTE).hText;
		HLock (hText);
		count = (**hTE).teLength;
		result = FSWrite (f, &count, *hText);
		(void) GetFPos (f, &count);
		(void) SetEOF (f, count);
		(void) FSClose (f);
		(void) FlushVol (nil, tmpFile.vRefNum);
		HUnlock (hText);
		FixCursor (iBeamCursor);
		if (result == noErr)
		{
			if (bindToFile)
			{
				DocDirty (doc) = false;
				if (haveNewFile)	/* name different than current */
				{
					SetWTitle (w, tmpFile.fName);
					DocBound (doc) = true;
					DocFile (doc) = tmpFile;
				}
			}
			return (true);
		}
		ErrMesg ("\pWrite error!");
	}
	return (false);
}


/*
 * Read file from disk.  Return value indicates whether or not the
 * file was read in successfully.
 */

static Boolean
ReadFile (DocHandle doc)
{
TEHandle	hTE;
SFReply	sfReply;
Boolean	result = false;
short	f;
long	len;
Handle	h;

	hTE = DocTE (doc);
	sfReply = DocFile (doc);
	FixCursor (watchCursor);
	if (FSOpen (sfReply.fName, sfReply.vRefNum, &f) != noErr)
		ErrMesg ("\pCouldn't open file");
	else
	{
		(void) GetEOF (f, &len);
		if (len >= maxFileSize)
			ErrMesg ("\pFile is too big");
		else
		{
			h = (Handle) TEGetText (hTE);
			SetHandleSize (h, len);
			HLock (h);
			(void) FSRead (f, &len, *h);
			HUnlock (h);
			(**hTE).teLength = len;
			TESetSelect (0L, 0L, hTE);	/* set caret at start */
			DocDirty (doc) = false;
			result = true;
		}
		(void) FSClose (f);
	}
	FixCursor (iBeamCursor);
	return (result);
}


/* ------------------------------------------------------------ */
/*			Interface (Public) Lowest-level Routines			*/
/* ------------------------------------------------------------ */


/*
 * Return true/false to indicate whether the window is really an
 * edit window.
 */

pascal Boolean
IsEWindow (WindowPtr w)
{
	return ((Boolean) (WindDocHandle (w) != nil));
}


/*
 * Return true/false to indicate whether the text associated with
 * the window has been changed since the last save/revert (or since
 * created, if not bound to file).
 */

pascal Boolean
IsEWindowDirty (WindowPtr w)
{
DocHandle	doc;

	if ((doc = WindDocHandle (w)) != nil)
		return (DocDirty (doc));
	return (false);
}


/*
 * Return a handle to the TextEdit record associated with the edit
 * window, or nil if it's not an edit window
 */

pascal TEHandle
GetEWindowTE (WindowPtr w)
{
DocHandle	doc;

	return ((doc = WindDocHandle (w)) == nil ? nil : DocTE (doc));
}


/*
 * Return true/false depending on whether or not the editor is
 * bound to a file, and a copy of the file info in the second
 * argument.  Pass nil for fileInfo if only want the return status.
 * Returns false if it's not an edit window.
 */

pascal Boolean
GetEWindowFile (WindowPtr w, SFReply *fileInfo)
{
DocHandle	doc;

	if ((doc = WindDocHandle (w)) != nil)
	{
		if (fileInfo != nil)
			*fileInfo = DocFile (doc);
		return (DocBound (doc));
	}
	return (false);
}


/* ---------------------------------------------------------------- */
/*					Interface Display Routines						*/
/* ---------------------------------------------------------------- */


/*
 * Install event notification procedures for an edit window.
 */

pascal void
SetEWindowProcs (WindowPtr w, TEditKeyProcPtr pKey,
					TEditActivateProcPtr pActivate, TEditCloseProcPtr pClose,
					TEditIdleProcPtr pIdle)
{
DocHandle	doc;

	if (w == nil)			/* reset window creation defaults */
	{
		e_key = pKey;
		e_activate = pActivate;
		e_close = pClose;
		e_idle = pIdle; /* L. Tierney */
	}
	else if ((doc = WindDocHandle (w)) != (DocHandle) nil)
	{
		DocKeyProc (doc) = pKey;
		DocActivateProc (doc) = pActivate;
		DocCloseProc (doc) = pClose;
		DocIdleProc (doc) = pIdle; /* L. Tierney */
	}
}


/*
 * Change the text display characteristics of an edit window
 * and redisplay it.
 *
 * Scroll to home position before overhauling, because although
 * the overhaul sets the viewRect to display an integral number
 * of lines, there's no guarantee that the destRect offset will
 * also be integral except at home position.  Clipping is set to
 * an empty rect so the scroll doesn't show.
 */

pascal void
SetEWindowStyle (WindowPtr w, short font,
				short size, Style style, short wrap, short just)
				/* added style parameter -- L. Tierney */
{
DocHandle	doc;
GrafPtr		savePort;
FontInfo	f;
TEHandle	hTE;
short		oldWrap;

	if (w == nil)			/* reset window creation defaults */
	{
		e_font = font;
		e_size = size;
		e_style = style;   /* L. Tierney */
		e_wrap = wrap;
		e_just = just;
		return;
	}

	if ((doc = WindDocHandle (w)) != (DocHandle) nil)
	{
		GetPort (&savePort);
		SetPort (w);
		hTE = DocTE (doc);
		GetPort (&savePort);
		ScrollToHome (hTE);

		oldWrap = (**hTE).crOnly;
		(**hTE).crOnly = wrap;	/* set word wrap */
		TESetAlignment (just, hTE);	/* set justification */

		TextFont (font);	 	/* set the font and point size */
		TextSize (size);		/* of text record */
		TextFace (style);		/* text style -- L. Tierney */
		GetFontInfo (&f);
		(**hTE).lineHeight = f.ascent + f.descent + f.leading;
		(**hTE).fontAscent = f.ascent;
		(**hTE).txFont = font;
		(**hTE).txSize = size;
		(**hTE).txFace = style;/* text style -- L. Tierney */

		OverhaulDisplay (doc, true, (oldWrap >= 0 || wrap >= 0));

		SetPort (savePort);
	}
}


/*
 * Redo display.  Does not save current port.  This is used by hosts
 * that mess with the text externally to TransEdit.  The arguments
 * determine whether the text is scrolled to show the line with the
 * caret, whether the lineStarts are recalculated, and whether or not
 * the text should be marked dirty.
 */

pascal void
EWindowOverhaul (WindowPtr w, Boolean showCaret,
						Boolean recalc, Boolean dirty)
{
DocHandle	doc;

	if ((doc = WindDocHandle (w)) != (DocHandle) nil)
	{
		OverhaulDisplay (doc, showCaret, recalc);
		DrawControls (w);
		DocDirty (doc) = dirty;
	}
}


/* ---------------------------------------------------------------- */
/*						Interface Menu Routine						*/
/* ---------------------------------------------------------------- */


/*
 * Do Edit menu selection.  This is only valid if an edit
 * window is frontmost.  Cut, Paste, and Clear cause the window
 * to be marked dirty.  Copy does not.  Undo is unimplemented.
 */

pascal void
EWindowEditOp (short item)
{
DocHandle	doc;
TEHandle	hTE;
Boolean		modified = false;

	if (SystemEdit (item - 1))
		return;

	if ((doc = WindDocHandle (FrontWindow ())) == (DocHandle) nil)
		return;				/* not an edit window */

	hTE = DocTE (doc);

	switch (item)
	{
	/*
	 * cut selection, put in TE Scrap, clear clipboard and put
	 * TE scrap in it
	 */
	case cut:
		TECut (hTE);
		(void) ZeroScrap ();
		(void) TEToScrap ();
		modified = true;
		break;

	/*
	 * copy selection to TE Scrap, clear clipboard and put
	 * TE scrap in it
	 */
	case copy:
		TECopy (hTE);
		(void) ZeroScrap ();
		(void) TEToScrap ();
		break;

	/*
	 * get clipboard into TE scrap, put TE scrap into edit record
	 */
	case paste:
		(void) TEFromScrap ();
		TEPaste (hTE);
		modified = true;
		break;

	/*
	 * delete selection without putting into TE scrap or clipboard
	 */
	case clear:
		TEDelete (hTE);
		modified = true;
		break;
	}

	AdjustDisplay (doc);
	if (modified)
		DocDirty (doc) = true;
}


/* ---------------------------------------------------------------- */
/*						Interface File Routines						*/
/* ---------------------------------------------------------------- */


/*
 * Set file creator for any files created by TransEdit
 */

pascal void
SetEWindowCreator (OSType creat)
{
	creator = creat;
}



/*
 * Save the contents of the given window
 */

pascal Boolean
EWindowSave (WindowPtr w)
{
	return (SaveFile (w,		/* window to save */
					  false,	/* don't ask for file if have one */
					  true));	/* bind to new file if one given */
}


/*
 * Save the contents of the given window under a new name
 * and bind to that name.
 */

pascal Boolean
EWindowSaveAs (WindowPtr w)
{
	return (SaveFile (w,		/* window to save */
					  true,		/* ask for file even if have one */
					  true));	/* bind to new file if one given */
}


/*
 * Save the contents of the given window under a new name, but
 * don't bind to the name.
 */

pascal Boolean
EWindowSaveCopy (WindowPtr w)
{
	return (SaveFile (w,		/* window to save */
					  true,		/* ask for file even if have one */
					  false));	/* don't bind to file */
}


/*
 * Close the window.  If it's dirty and is either bound to a file
 * or (if not bound) has some text in it, ask about saving it first,
 * giving user option of saving changes, tossing them, or
 * cancelling altogether.
 *
 * Return true if the file was saved and the window closed, false if
 * user cancelled or there was an error.
 */

pascal Boolean
EWindowClose (WindowPtr w)
{
DocHandle	doc;
TEHandle	hTE;
SFReply		sfReply;

	if ((doc = WindDocHandle (w)) == (DocHandle) nil)
		return (false);
	hTE = DocTE (doc);
	sfReply = DocFile (doc);

	if ( (DocBound (doc) || (**hTE).teLength > 0) && DocDirty (doc))
	{
		switch (FakeAlert ("\pSave changes to \322", sfReply.fName,
				"\p\323?", "\p", 3, 1, 2,
				"\pSave", "\pCancel", "\pDon\325t Save"))	/* ask whether to save */
		{

		case 1:
			if (SaveFile (w,	/* window to save */
						  false,	/* don't ask for name */
						  false)	/* don't bind to name */
					== false)
				return (false);	/* canceled or error - cancel Close */
			break;

		case 2:			/* cancel Close */
			return (false);

		case 3:			/* toss changes */
			break;
		}
	}
	SkelRmveWind (w);
	return (true);
}


/*
 * Revert to saved version of file on disk.  The window must be an edit
 * window, and must be bound to a file.  Returns false if one of these
 * conditions is not met, or if they are met but there was an error
 * reading the file.
 *
 * The window need not be dirty; if it is, the user is asked
 * whether to really revert.
 */

pascal Boolean
EWindowRevert (WindowPtr w)
{
DocHandle	doc;
SFReply		sfReply;

	if ((doc = WindDocHandle (w)) == (DocHandle) nil)
		return (false);
	if (!DocBound (doc))
		return (false);		/* no file to revert to */
	if (DocDirty (doc))
	{
		sfReply = DocFile (doc);
		if (FakeAlert ("\p\322", sfReply.fName,
				"\p\323 has been changed.  Really revert?",
				"\p", 2, 1, 1, "\pCancel", "\pRevert", "\p") == 1)
			return (false);
	}
	if (ReadFile (doc) == false)
		return (false);
	ScrollToHome (DocTE (doc));
	OverhaulDisplay (doc, true, true);
	DrawControls (w);
	ValidRect (&w->portRect);
	return (true);
}


/* ---------------------------------------------------------------- */
/*			Interface Initialization/Termination Routines			*/
/* ---------------------------------------------------------------- */


/*
 * Set up the SFReply record for a document.  Ask for a file and use
 * the file selected if bindToFile is true.  Otherwise use title for
 * the name if it's non-nil, or make the document untitled if title is
 * nil or the empty string.
 *
 * Copy the name into the file info structure even if the window is
 * unbound, because the Save operations expect to find it there as the
 * most likely name to use.
 */

static Boolean
SetupFile (StringPtr title, Boolean bindToFile, SFReply *sfReply)
{
Str255		s;
OSType		type = 'TEXT';

	sfReply->vRefNum = 0;
	if (bindToFile)
	{
		SFPGetFile (dlogWhere, "\p", nil, 1, &type, nil, sfReply,
				getDlgID, SkelDlogFilter (nil, false));
		SkelRmveDlogFilter ();
		return (sfReply->good);
	}
	if (title == nil || title[0] == 0)
	{
		Untitled (s);
		title = s;
	}
	BlockMove (title, sfReply->fName, (long) (title[0] + 1));
	return (true);
}


/*
 * Create and initialize an edit window and the associated data
 * structures.  If the window and data cannot be allocated, destroy
 * the window and return nil.  Otherwise return the window.
 *
 * The caller should set and restore the port before and after calling
 * SetupDocWind().
 */

static TEClickLoopUPP AutoScrollUPP = NULL;

static WindowPtr
SetupDocWind (WindowPtr w, Boolean visible, Boolean bindToFile, SFReply *sfReply)
{
DocHandle	doc;
SkelWindPropHandle	prop;
ControlHandle	scroll;
TEHandle	hTE;
Rect		r;

	if (!SkelWindow (w,		/* the window */
				Mouse,		/* mouse click handler */
				Key,		/* key click handler */
				Update,		/* window updating procedure */
				Activate,	/* window activate/deactivate procedure */
				Close,		/* window close procedure */
				Clobber,	/* window disposal procedure */
				Idle,		/* idle proc */
				true))		/* idle only when frontmost */
	{
		DisposeWindow (w);
		return (nil);
	}

	/*
	 * After this point SkelRmveWind() can be called to remove the window
	 * if any allocations fail.
	 */

	/*
	 * Get new document record, attach to window property list.
	 * Also make document record point to window.
	 */

	if (!SkelAddWindProp (w, skelWPropEditWind, (long) 0L))
	{
		SkelRmveWind (w);
		return (nil);
	}
	doc = New (DocRecord);
	if (doc == (DocHandle) nil)
	{
		SkelRmveWind (w);
		return (nil);
	}
	prop = SkelGetWindProp (w, skelWPropEditWind);
	(**prop).skelWPropData = (long) doc;
	DocWind (doc) = w;

	/*
	 * Build the scroll bar.  Make sure the borders overlap the
	 * window frame and the frame of the grow box.
	 */

	CalcScrollRect (w, &r);
	scroll = NewControl (w, &r, "\p", true, 0, 0, 0, scrollBarProc, 0L);
	DocScroll (doc) = scroll;

	/*
	 * Create the TE record used for text display.  Use default
	 * characteristics.
	 */

	CalcEditRect (w, &r);
	hTE = TENew (&r, &r);
	DocTE (doc) = hTE;
	if (! AutoScrollUPP)
		AutoScrollUPP = NewTEClickLoopProc((ProcPtr) AutoScroll);
	TESetClickLoop ((TEClickLoopUPP) AutoScrollUPP, hTE);			/* set autoscroll proc */

	if (scroll == (ControlHandle) nil || hTE == (TEHandle) nil)
	{
		SkelRmveWind (w);
		return (nil);
	}

	/*
	 * Install default event notification procedures, font characteristics.
	 */

	SetEWindowProcs (w, e_key, e_activate, e_close, e_idle); /* idle added -- L. Tierney */
	SetEWindowStyle (w, e_font, e_size, e_style, e_wrap, e_just);

	DocFile (doc) = *sfReply;
	DocBound (doc) = bindToFile;
	if (bindToFile && !ReadFile (doc))
	{
		SkelRmveWind (w);
		return (nil);
	}
	DocDirty (doc) = false;

	/*
	 * Show window if specified as visible, and return a pointer to it.
	 */

	OverhaulDisplay (doc, true, true);
	if (visible)
		ShowWindow (w);

	return (w);
}


/*
 * Initialize the window and associated data structures.
 * Return window pointer or nil if some sort of error.
 *
 * Preserves the current port.  If the window is visible,
 * an activate event will follow, at which point the port
 * will be set to the window.
 *
 * If a file is to be solicited, ask for it before creating the
 * window to avoid the following problem:  If you create a window
 * to be frontmost but invisible, putting up the getfile dialog
 * after creating the window causes the window to go behind the
 * first visible window when the file dialog goes away.
 */

pascal WindowPtr
NewEWindow (Rect *bounds, StringPtr title, Boolean visible,
			WindowPtr behind, Boolean goAway,
			long refNum, Boolean bindToFile)
{
WindowPtr	w;
GrafPtr		savePort;
SFReply		sfReply;

	if (!SetupFile (title, bindToFile, &sfReply))	/* user cancelled */
		return (nil);

	if (SkelQuery (skelQHasColorQD))
	{
		w = NewCWindow (nil, bounds, sfReply.fName, false, documentProc + 8,
								behind, goAway, refNum);
	}
	else
	{
		w = NewWindow (nil, bounds, sfReply.fName, false, documentProc + 8,
								behind, goAway, refNum);
	}

	if (w != (WindowPtr) nil)
	{
		GetPort (&savePort);
		w = SetupDocWind (w, visible, bindToFile, &sfReply);	/* nil if allocation failed */
		SetPort (savePort);
	}

  	return (w);
}


/*
 * GetNewEWindow() is the resource equivalent of NewEWindow().  It reads in the
 * 'WIND' template and yanks window creation values out of it rather than using
 * GetWindow() since the latter would create the window visible right away.
 * NewEWindow() doesn't show the window until after the file has been read in, if
 * a file is to be read.  procID value from resource is ignored.
 */

/* 'WIND' resource structure */

typedef struct WTmpl WTmpl, **WTHandle;

struct WTmpl
{
	Rect	bounds;
	short	procId;
	short	visible;
	short	goAway;
	long	refCon;
	Str255	title;
};


pascal WindowPtr
GetNewEWindow (short resourceNum, WindowPtr behind, Boolean bindToFile)
{
WTHandle	h;
WindowPtr	w;

	h = (WTHandle) GetResource ('WIND', resourceNum);
	if (h == (WTHandle) nil)
		return ((WindowPtr) nil);
	MoveHHi ((Handle) h);
	HLock ((Handle) h);
	w = NewEWindow (&((**h).bounds), (**h).title, (Boolean) ((**h).visible != 0),
					behind, (Boolean) ((**h).goAway != 0), (**h).refCon, bindToFile);
	HUnlock ((Handle) h);
	return (w);
}


/*
 * Look through the list of windows, shutting down all the edit
 * windows.  If any window is dirty, ask user about saving it first.
 * If the user cancels on any such request, ClobberEWindows returns
 * false.  If all edit windows are shut down, return true.  It is
 * then safe for the host to exit.
 *
 * When a window *is* shut down, have to start looking through the
 * window list again, since w no longer points anywhere
 * meaningful.
 */

pascal Boolean
ClobberEWindows (void)
{
WindowPtr	w;

	for (;;)
	{
		for (w = FrontWindow ();
				w != nil;
					w = (WindowPtr) ((WindowPeek) w)->nextWindow)
		{
			if (IsEWindow (w))
				break;
		}
		if (w == nil)
			return (true);		/* all edit windows are shut down */

		if (w != FrontWindow ())
		{
			SelectWindow (w);
			ShowWindow (w);
			EWindowOverhaul (w, false, false, IsEWindowDirty (w));
			SetPort (w);
			ValidRect (&w->portRect);
		}

		if (EWindowClose (w) == false)
			return (false);		/* cancel or error */
	}
}

/**** Xlisp-Stat Additions -- L. Tierney */

/* change the dirty status */
pascal void
SetEWindowDirty (WindowPtr w, Boolean dirty)
{
DocHandle	doc;

	if ((doc = WindDocHandle (w)) != nil)
		DocDirty (doc) = dirty;
}

/* show caret */
pascal void EWindowAdjustDisplay(WindowPtr w)
{
DocHandle	doc;

	if ((doc = WindDocHandle (w)) != nil)
		AdjustDisplay(doc);
}
