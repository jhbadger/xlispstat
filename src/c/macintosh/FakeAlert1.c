/*
 * In-memory item list for dialog with five items:
 *
 * 1	Button 1
 * 2	Button 2
 * 3	Button 3
 * 4	caution icon
 * 5	"^0^1^2^3" (static text)
 * 6	user item (for outlining default button)
 *
 * The caller of FakeAlert passes the four strings that are to be
 * substituted into the first item, the number of buttons that
 * should be used, and the titles to put into each button.
 * A copy of the item list is hacked to use the right number of
 * buttons.
 *
 * Thanks to Erik Kilk and Jason Haines.  Some of the stuff to do
 * this is modified from code they wrote.
 *
 * 17 Jun 92
 * - Set the aDefItem field of the dialog template, so that return/enter
 * key properly equivalence to clicking the default button.
 * 05 Jun 93
 * - Conversion for THINK C 6.0.
 * 05 Jul 93
 * - Repositioned buttons.  All are now in single line.  Button 1 is
 * rightmost, button 2 in middle, and button 3 is leftmost.  As more
 * buttons are used, they extend more to the left.
 * 18 Dec 93
 * - Took out all the "register" declarations.  The compiler's smart enough
 * now that they don't make any difference, so they're just clutter.
 * - Item list handle made unpurgable while ModalDialog() is processing
 * dialog and purgable afterward.
 * 18 Jan 94
 * - Redid item list to provide a user item.  Default button outlining
 * done using SkelSetDlogButtonOutliner().
 * - Unused buttons are now hidden.  No more messing with changing the number
 * of items in the item list.
 * - Buttons positioned more in line with Apple guidelines.
 * - Alert window positioned on the front window's parent device now.
 * - FakeAlert() takes a parameter indicating the Cancel button.  This allows
 * it to do key mapping for Escape and Command-period.
 * 20 Jan 94
 * - Added a caution icon to the alert item list.
 */

# include	"TransSkel1.h"

# include	"TransEdit1.h"


/* in-memory item list */


typedef enum
{
	button1Item = 1,
	button2Item,
	button3Item,
	cautionIconItem,
	staticTextItem,
	outlineItem
};


static short	itemList [] =
{
	5,					/* max number of items - 1 */

	/*
	 * first button
	 */

	0, 0,								/* reserve a long for item handle */
	105, 260, 125, 340,					/* display rectangle */
	((ctrlItem+btnCtrl) << 8) | 0,		/* pushButton, title 0 bytes long */

	/*
	 * second button
	 */

	0, 0,								/* reserve a long for item handle */
	105, 167, 125, 247,					/* display rectangle */
	((ctrlItem+btnCtrl) << 8) | 0,		/* pushButton, title 0 bytes long */

	/*
	 * third button
	 */

	0, 0,								/* reserve a long for item handle */
	105, 20, 125, 100,					/* display rectangle */
	((ctrlItem+btnCtrl) << 8) | 0,		/* pushButton, title 0 bytes long */

	/*
	 * icon item
	 */

	0, 0,								/* reserve a long for item handle */
	10, 10, 42, 42,						/* display rectangle */
	((iconItem+itemDisable) << 8) | 2,	/* icon (disabled), icon id 2 bytes long */
	2,				/* icon 2 */

	/*
	 * statText item
	 */

	0, 0,								/* reserve a long for item handle */
	10, 52, 58, 340,					/* display rectangle */
	((statText+itemDisable) << 8) | 8,	/* statText (disabled), text 8 bytes long */
	'^0', '^1',		/* ^0^1^2^3 */
	'^2', '^3',

	/*
	 * user item for default button outlining
	 */

	0, 0,								/* reserve a long for item handle */
	0, 0, 10, 10,							/* display rectangle (fake) */
	((userItem+itemDisable) << 8) | 0,	/* userItem (disabled), title 0 bytes long */
};


/*
 * Hide a dialog button item by shifting it off into space.  HideDItem()
 * does the same thing but unfortunately is not present on 64K ROM
 * machines. Inside Macintosh IV-59 says you can hide an item by adding
 * 16384 to the left and right coordinates of the bounding rectangle.
 * However, that doesn't seem to work unless the control itself is moved
 * as well.
 */

static void
HideDlogButton (DialogPtr dlog, short item)
{
Rect	r;

	SkelGetDlogRect (dlog, item, &r);
	OffsetRect (&r, 16384, 0);
	SkelSetDlogRect (dlog, item, &r);
	MoveControl (SkelGetDlogCtl (dlog, item), 16384, 0);
}


/*
 * Fake an alert, using an in-memory window and item list.
 * The message to be presented is constructed from the first
 * four arguments.  nButtons is the number of buttons to use,
 * defButton is the default button, the next three args are
 * the titles to put into the buttons.  The return value is
 * the button number (1..nButtons).  This must be interpreted
 * by the caller, since the buttons may be given arbitrary
 * titles.
 *
 * nButtons should be between 1 and 3, inclusive.
 * defButton should be between 1 and nButtons, inclusive.
 * cancelButton should be between 1 and nButtons, inclusive, or
 * 0 if there's no cancel button.
 */

pascal short
FakeAlert (StringPtr s1, StringPtr s2, StringPtr s3, StringPtr s4,
			short nButtons, short defButton, short cancelButton,
			StringPtr t1, StringPtr t2, StringPtr t3)
{
ModalFilterUPP	filter;
GrafPtr		savePort;
DialogPtr	dlog;
Handle		iListHandle;
Rect		bounds;
short		item;

	InitCursor ();
	GetPort (&savePort);
	iListHandle = NewHandle (512L);
	HLock (iListHandle);
	BlockMove (&itemList, *iListHandle, 512L);
	HUnlock (iListHandle);
	HNoPurge (iListHandle);
	SetRect (&bounds, 0, 0, 350, 135);
	dlog = NewDialog (nil, &bounds, "\p", false, dBoxProc, (WindowPtr) -1L,
							false, 0L, iListHandle);
	SkelPositionWindow (dlog, skelPositionOnParentDevice,
							FixRatio (1, 2), FixRatio (1, 5));
	((DialogPeek) dlog)->aDefItem = defButton;
	SkelSetDlogButtonOutliner (dlog, outlineItem);

	SetPort (dlog);

	if (nButtons < 3)				/* hide unused buttons */
	{
		HideDlogButton (dlog, button3Item);
		if (nButtons < 2)
			HideDlogButton (dlog, button2Item);
	}

	switch (nButtons)				/* set button titles */
	{
	case 3:
		SetControlTitle (SkelGetDlogCtl (dlog, button3Item), t3);
		/* fall through... */
	case 2:
		SetControlTitle (SkelGetDlogCtl (dlog, button2Item), t2);
		/* fall through... */
	case 1:
		SetControlTitle (SkelGetDlogCtl (dlog, button1Item), t1);
	}

	ParamText (s1, s2, s3, s4);		/* construct message */
	ShowWindow (dlog);

	/*
	 * Given a nil filter proc, ModalDialog() normally returns 1 if Return or
	 * Enter are hit.  This can be incorrect since the default item need not
	 * be one for the "alert" being presented here.  This problem does not
	 * actually occur since (i) a non-nil filter is installed by SkelDlogFilter()
	 * and (ii) the filter used by SkelDlogFilter() returns the item number in
	 * the aDefItem field.
	 */

	filter = SkelDlogFilter (nil, true);
	if (cancelButton != 0)
		SkelDlogCancelItem (cancelButton);
	ModalDialog (filter, &item);
	SkelRmveDlogFilter ();
	HPurge (iListHandle);
	DisposeDialog (dlog);	/* this also disposes of iListHandle */
	SetPort (savePort);
	return (item);
}
