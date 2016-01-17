/*
 * TransSkel - Transportable Macintosh application skeleton
 * Release 3.17
 *
 * Please report problems to Paul DuBois.
 *
 *
 * TransSkel is in the public domain and was originally written by:
 *
 * 			Paul DuBois
 * 			Wisconsin Regional Primate Research Center
 * 			1220 Capitol Court
 * 			Madison, WI  53715-1299  USA
 *
 * Internet:	dubois@primate.wisc.edu
 *			
 * Additional changes were made by:
 *	
 * 			Owen Hartnett
 * 			OHM Software Company
 * 			163 Richard Drive
 * 			Tiverton, RI 02878  USA
 * 		
 * Internet:	omh@cs.brown.edu
 * UUCP:		uunet!brunix!omh	
 *
 * Owen is also responsible for the port to THINK Pascal.
 *
 * Bob Schumaker joined the cast between versions 2.0 and 3.0.
 *
 * 			Bob Schumaker
 * 			The AMIX Corporation
 * 			1881 Landings Drive
 * 			Mountain View, CA 94043-0848
 *
 * Internet:	bob@markets.amix.com
 * UUCP:		{sun, uunet, netcom}!markets!bob
 * CIS:			72227,2103
 * AOL:			BSchumaker
 *
 * This version of TransSkel is written for THINK C 6.0.1.
 * THINK C is a trademark of:
 *
 * 	Symantec Corporation
 * 	10201 Torre Avenue
 * 	Cupertino, CA 95014  USA
 *
 * Reference key:
 * IM		Inside Macintosh
 * TN		Macintosh Technical Notes
 * MHIG		Macintosh Human Interface Guidelines
 * HIN		Human Interface Notes
 * PGMF		Programmer's Guide to MultiFinder (APDA)
 * TPN		TransSkel Programmer's Notes
 *
 * Recent history is given below.  Earlier change history is in TSHistory.
 * If you've been writing applications with an earlier release, READ THAT FILE!
 *
 * 06 Feb 94 Release 3.10
 * - Modified entire library source so that it can be compiled with either
 * C-compatible or Pascal-compatible bindings.  This allows a binary library
 * to be compiled that can be linked into THINK Pascal applications while
 * preserving compatibility with existing TransSkel C applications.  This
 * modification was suggested by Lionel Cons, who also contributed a working
 * prototype and the major portion of the Pascal header file.
 * - Fixed cast bug in SkelEventLoop().
 * 07 Feb 94
 * - SkelApple() now takes an empty string or nil to indicate no application item,
 * rather than nil only.
 * 08 Feb 94
 * - SkelPositionWindow() positions windows using the structure rectangle rather
 * than the content rectangle now.  Give better positioning.
 *
 * 17 Feb 94 Release 3.11
 * - New function SkelTestRectVisible() to check whether or not a
 * rectangle is completely contained within the desktop region.
 * - SkelAlert() uses SkelTestRectVisible() to check whether an alert will be
 * entirely visible when it's supposed to be positioned on the parent window.
 * If not, it's positioned on the parent device instead.
 * - When position type is skelPositionOnParentWind, SkelGetReferenceRect() now
 * returns the structure rectangle rather than the content rectangle.
 * - SkelPositionWindow() now accounts for difference between width of structure
 * and content rectangle of window to be positioned.
 * 20 Feb 94
 * - Pascal bindings for the interface functions are now standard.  C bindings
 * are no longer used.
 *
 * 14 Apr 94 Release 3.12
 * - Added SkelDlogTracksCursor() function.
 *
 * 22 Apr 94 Release 3.13
 * - New function SkelGetWindPropData().  Note caveat in comment preceding
 * function definition.
 * 23 Apr 94
 * - Started adding support for movable modal dialogs.
 * - Added variable hasGetWVariant used to tell if GetWVariant() exists
 * for determining whether a dialog is a movable modal or not.
 * - Standard dialog filter passes command-clicks in drag region of underlying
 * windows to TransSkel's event router.
 * 25 Apr 94
 * - Command-period isn't passed to menu routines any more.  Instead the key
 * handler is called.  The key handler can notice the Cancel and respond
 * accordingly if it wants to.
 * - All dialog events for modeless and movable modeless dialog are now handled
 * in SkelRouteEvent().  (Null events, mouse and key clicks, updates, activates.)
 * - Since dialog support is now integrated into the main event routing, the
 * old DoDialog() function obsolete.
 * - The supportDialogs #define is also obsolete.
 * - The dialog event mask is also obsolete, so SkelSetDlogMask() and
 * SkelGetDlogMask() are obsolete, too, and have been dropped.
 * - Command-clicks in drag region of underlying windows while movable modal
 * dialog is frontmost are processed.
 * 26 Apr 94
 * - New routines SkelIsDlog() and SkelIsMMDlog() for determining whether a
 * window is a dialog or movable modal dialog, respectively.
 * - Movable modal dialogs can now be registered with SkelDialog().  The property
 * added by the routine is now skelWPropModeless or skelWPropMovableModal depending
 * on the type of dialog window.
 * - Renamed SkelRouteEvent() to RouteEvent().  SkelRouteEvent() is now a call
 * to RouteEvent() plus the stuff that used to be in SkelEventLoop() for polling
 * window-specific idle-time procedures.  The effect is that idle procedures that
 * are supposed to run even for windows that are not frontmost no longer freeze
 * when a modal dialog came up (if you use SkelDlogFilter()).
 * 27 Apr 94
 * - Dropped the distinctions between GetWDHandler(), GetDHandler(), and
 * GetWHandler().  With more pervasive dialog support, the instances where it
 * really matters whether a dialog or non-dialog window is being handled are
 * rare.  GetWHandler() now suffices everywhere.  Renamed DetachWDHandler() and
 * oldWDHandler to DetachWHandler() and oldWHandler for consistency.
 * - GetWHandler() now returns faster when the argument is nil.
 * - Revised the argument structure of SkelDialog() to allow an event filter
 * to be specified.
 * - Bug fix:  SkelAddWindProp() was passing propData as the propType argument
 * to SkelGetWindProp()  when checking to see if a property of the given type
 * already existed.
 * 29 Apr 94
 * - New routine SkelDlogMapKeyToButton() for helping to map return, enter,
 * escape and command-period to default or cancel button in dialogs.
 * - Window-specific idle-time routines now run when application is in background.
 
 * 02 May 94 Release 3.14
 * Changed the skelResumeProc member of SkelInitParams structure from type
 * ResumeProcPtr to type SkelResumeProcPtr.  ResumeProcPtr is disappearing from
 * Apple's header files, since you're only supposed to pass nil to InitDialogs()
 * under System 7 now.  This should help TransSkel compile under THINK C 7.
 *
 * 03 May 94 Release 3.15
 * - Fixed bug in SkelDlogFilter.c/ModalFilterYD() causing machine lockup
 * if SkelDlogFilterYD() was used.
 *
 * 04 May 94 Release 3.16
 * - Fixed bug in RouteEvent() causing crashes if application idle-time function
 * changed the front window.
 *
 * 15 May 94 Release 3.17
 * - Changed references to QuickDraw globals so they're written in terms of the
 * qd struct.  E.g., thePort -> qd.thePort, gray -> qd.gray.
 * - Redid some pattern references so they'll compile whether or not
 * dangerousPattern is defined, and if universal headers are used.
 * 25 May 94
 * - Try to cast the argument to InitDialogs() properly depending on whether or
 * not the universal headers are used.
 */


# include	<Traps.h>
# include	<Gestalt.h>
# include	<EPPC.h>


/*
 * TransSkel1.h contains defines, typedefs, and public function
 * prototypes
 */

# include	"TransSkel1.h" /**** use modified header file -- L. Tierney */


/*
 * New(TypeName) returns handle to new object, for any TypeName.
 * If there is insufficient memory, the result is nil.
 */

# define	New(type)	(type **) NewHandle ((Size) sizeof (type))


/* -------------- */
/* Internal types */
/* -------------- */


/*
 * Private data types for window and menu handlers
 */

typedef struct WHandler	WHandler, *WHPtr, **WHHandle;

struct WHandler
{
	WindowPtr				whWind;		/* window/dialog to handle */
	SkelWindMouseProcPtr	whMouse;	/* mouse-click handler */
	SkelWindKeyProcPtr		whKey;		/* key-click handler */
	SkelWindUpdateProcPtr	whUpdate;	/* update handler */
	SkelWindActivateProcPtr	whActivate;	/* activate event handler */
	SkelWindCloseProcPtr	whClose;	/* close "event" handler */
	SkelWindClobberProcPtr	whClobber;	/* window disposal proc */
	SkelWindIdleProcPtr		whIdle;		/* main loop idle proc */
	SkelWindZoomProcPtr		whZoom;		/* zoom proc */
	SkelWindSelectProcPtr	whSelect;	/* item selection proc (dialog) */
	ModalFilterProcPtr		whFilter;	/* event filter proc (dialog) */
	Rect		whGrow;					/* limits on window sizing */
	Boolean		whSized;				/* true = window was resized */
	Boolean		whFrontOnly;			/* idle only when window active */
	short		whFlags;				/* various flags */
	SkelWindPropHandle	whProperties;	/* property list */
	WHHandle	whNext;					/* next window handler */
};

typedef struct MHandler	MHandler, *MHPtr, **MHHandle;

struct MHandler
{
	short					mhID;		/* menu id */
	SkelMenuSelectProcPtr	mhSelect;	/* item selection handler */
	SkelMenuClobberProcPtr	mhClobber;	/* menu disposal proc */
	Boolean		mhSubMenu;				/* whether submenu */
	MHHandle	mhNext;					/* next menu handler */
};


/* ------------------------------------------- */
/* Prototypes for internal (private) functions */
/* ------------------------------------------- */

static WHHandle GetWHandler (WindowPtr w);
static void DetachWHandler (WHHandle wh);

static void RouteEvent (EventRecord *evt);

static void DoMenuCommand (long command);
static void DoMenuHook (void);

static void DoMouse (WHHandle h, EventRecord *evt);
static void DoKey (WHHandle h, char ch, unsigned char code, short mods);
static void DoUpdate (EventRecord *evt);
static void DoActivate (EventRecord *evt);
static void DoClose (WHHandle h);
static void DoClobber (WHHandle h);
static void DoDlogEvt (DialogPtr dlog, EventRecord *evt);
static Boolean DoDlogFilter (DialogPtr dlog, EventRecord *evt);

static void DoGrow (WHHandle h, Point startPt);
static void DoZoom (WHHandle h, short partCode);


/* ------------------ */
/* Internal variables */
/* ------------------ */


/*
 * Window and menu handler variables.
 *
 * whList and mhList are the lists of window and menu handlers.
 * mhClobOnRmve is true if the menu handler disposal proc
 * is to be called when a handler is removed.  It is temporarily set
 * false when handlers are installed for menus that already
 * have handlers - the old handler is removed WITHOUT calling the
 * disposal proc.  The effect is to replace the handler for the menu
 * without destroying the menu itself.
 *
 * dragRect determines the limits on window dragging.  It is set in
 * SkelInit() to the bounding box of the desktop region inset by 4 pixels.
 *
 * growRect contains the default limits on window sizing.  It is set in
 * SkelInit().  The lower limits on window sizing of 80 pixels both directions
 * is sufficient to allow text windows room to draw a grow box and scroll
 * bars without having the thumb and arrows overlap.  The upper limits are
 * determined from the screen size. (Probably incorrectly for the case of > 1
 * screen.)
 * These default values may be changed if with SkelGrowBounds if they are
 * not appropriate.
 *
 * zoomProc is the default zoom procedure to use if the window does not have
 * one of its own.  zoomProc may be nil, in which case the default is to zoom
 * to just about full window size.
 *
 * mhDrawBarOnRmve determines whether the menu bar is redrawn by
 * SkelRmveMenu() after taking a menu out of the menu bar.  Normally
 * it's true, but SkelClobber() sets it false temporarily to avoid
 * flicker as each menu is removed.
 */


static WHHandle	whList = (WHHandle) nil;
static Rect		dragRect;
static Rect		growRect;
static SkelWindZoomProcPtr	zoomProc = (SkelWindZoomProcPtr) nil;


static MHHandle	mhList = (MHHandle) nil;
static Boolean	mhClobOnRmve = true;
static Boolean	mhDrawBarOnRmve = true;


/*
 * Miscellaneous
 *
 * - skelEnv contains SysEnvirons() information.
 * - sysVersion contains the system software version.
 * - hasGestalt is true if Gestalt() is supported.
 * - has64KROM is true if the current machine has the 64K ROM.
 * - hasGetWVariant is true if GetWVariant() is supported.
 * - mBarHeight is menu bar height.  Window sizing, zooming and dragging
 * code takes this into account.  Initialized in SkelInit(), which see
 * for teeth-gnashing over such a simple thing.
 * - doneFlag determines when SkelEventLoop() returns.  It is set by calling
 * SkelStopEventLoop(), which is how the host requests a halt.
 * - pIdle points to a background procedure, to be run during event
 * processing.  Set it with SkelSetIdle().  If nil, there's no
 * procedure.
 * - pEvent points to an event-inspecting hook, to be run whenever an
 * event occurs.  Set it with SkelSetEventHook().  If nil, there's no
 * procedure.
 * - eventMask controls the event types requested by GetNextEvent() or
 * WaitNextEvent() in SkelEventLoop().
 * - pMenuHook points to a procedure called whenever a menu selection is about
 * to be executed.  nil if no hook.
 * - diskInitPt is the location at which the disk initialization dialog
 * appears, if an uninitialized disk is inserted.
 * - eventModifiers is the value of the modifiers field of the current event.
 * - eventPtr points to the current event (nil if none seen yet).
 * - defInitParams contains the default SkelInit() parameters if caller passes
 * nil.
 */

static SysEnvRec	skelEnv;
static long	sysVersion = 0;
static Boolean	hasGestalt;
static Boolean	has64KROM;
static Boolean	hasGetWVariant;
static short	mBarHeight;
static short	doneFlag;
static short	eventMask = everyEvent ^ keyUpMask;
static short	eventModifiers = 0;
static EventRecord	*eventPtr = (EventRecord *) nil;
static Point	diskInitPt = { /* v = */ 120, /* h = */ 100 };

static SkelIdleProcPtr			pIdle = (SkelIdleProcPtr) nil;
static SkelEventHookProcPtr		pEvent = (SkelEventHookProcPtr) nil;
static SkelMenuHookProcPtr		pMenuHook = (SkelMenuHookProcPtr) nil;

static SkelInitParams	defInitParams =
{
	6,							/* no. of times to call MoreMasters() */
	(GrowZoneProcPtr) nil,		/* GrowZone proc */
	(SkelResumeProcPtr) nil,	/* resume proc */
	0L							/* stack adjustment */
};

/*
 * Multitasking support stuff
 *
 * hasWNE is true if WaitNextEvent() is available.
 *
 * inForeground is true if application is running in foreground (not
 * suspended).  Initially true, per PGMF 3-1.
 *
 * getFrontClicks indicates whether the application wants to receive
 * content-area clicks that bring it to the foreground.
 *
 * fgWaitTime and bgWaitTime are WaitNextEvent() times for foreground and
 * background states.
 */

static Boolean	hasWNE;
static Boolean	inForeground = true;
static long	fgWaitTime = 6L;			/* 0.1 seconds */
static long	bgWaitTime = 300L;			/* 5.0 seconds */
static Boolean	getFrontClicks = false;
static SkelSuspendResumeProcPtr	pSuspendResume = (SkelSuspendResumeProcPtr) nil;
static SkelClipCvtProcPtr	pClipCvt = (SkelClipCvtProcPtr) nil;

static WindowPtr	oldWindow = (WindowPtr) nil; 	
static WHHandle		oldWHandler = (WHHandle) nil;

/*
 * Apple Event support
 */

static Boolean	hasAppleEvents = 0;
static SkelAEHandlerProcPtr	pAEHandler = (SkelAEHandlerProcPtr) nil;


/* --------------------------- */
/* Initialization and shutdown */
/* --------------------------- */

/*
 * Initialize the various Macintosh Managers and lots of other stuff.
 *
 * FlushEvents does NOT toss disk insert events; this is so disks
 * inserted while the application is starting up don't result
 * in dead drives.
 *
 * initParams contains initialization parameters:
 * - the number of times to call MoreMasters
 * - the address of a grow zone procedure to call if memory allocation
 * problems occur (nil if none to be used)
 * - the address of a resume procedure to pass to InitDialogs()
 * (nil if none is to be used)
 * - amount to adjust the application stack size by (default 0; no adjustment)
 *
 * if initParams is nil, defaults are used.
 */

pascal void
SkelInit (SkelInitParamsPtr initParams)
{
EventRecord	dummyEvent;
Handle		h;
long		result;
short		i;

	if (initParams == (SkelInitParams *) nil)
		initParams = &defInitParams;

	if (initParams->skelGzProc != (GrowZoneProcPtr) nil)
        /* It would be better to deallocate this UPP, but isn't worth the effort */
		SetGrowZone (NewGrowZoneProc((ProcPtr) initParams->skelGzProc));

	SetApplLimit (GetApplLimit () - initParams->skelStackAdjust);

	MaxApplZone ();

	for (i = 0; i < initParams->skelMoreMasters; i++)
		MoreMasters ();

	FlushEvents (everyEvent - diskMask, 0 );
	InitGraf (&thePort);
	InitFonts ();
	InitWindows ();
	InitMenus ();
	TEInit ();
/*
 * Cast argument according to whether or not universal headers are used
 */
	InitDialogs (initParams->skelResumeProc);
	InitCursor ();

	(void) SysEnvirons (1, &skelEnv);
	
	sysVersion = (long) skelEnv.systemVersion;

	has64KROM = (skelEnv.machineType == envMac || skelEnv.machineType == envXL);

	/*
	 * If 64K ROM machine, use hard-coded value of 20.  Otherwise use
	 * Script Manager routine GetMBarHeight().  (This assumes, just to be
	 * safe, that GetMBarHeight() glue doesn't return 20 on 64K ROM systems,
	 * which it very well may.  The low memory variable MBarHeight (0x0BAA)
	 * isn't used because it doesn't exist on 64K ROM machines (TN OV 4, p.7).
	 */

	mBarHeight = (has64KROM ? 20 : GetMBarHeight ());

	/*
	 * Determine whether WaitNextEvent() is implemented (TN's OV 16 and TN TB 14)
	 */

	if (has64KROM)
		hasWNE = false;
	else
		hasWNE = SkelTrapAvailable (_WaitNextEvent);

	hasGestalt = SkelTrapAvailable (_Gestalt);
	hasAppleEvents = hasGestalt
					&& Gestalt (gestaltAppleEventsAttr, &result) == noErr
					&& (result & (1 << gestaltAppleEventsPresent));

	/*
	 * Determine whether GetWVariant() exists for checking whether a dialog is
	 * a movable modal or not.  The variant code can be gotten other ways, but
	 * the existence of trap precedes the existence of movalable modal windows,
	 * so if the trap doesn't exist, movable modals aren't likely to, either.
	 */

	hasGetWVariant = SkelTrapAvailable (_GetWVariant);

	/*
	 * Check whether application wants to get "bring to front" clicks.
	 */

	if ((h = GetResource ('SIZE', -1)) != (Handle) nil)
	{
		getFrontClicks = (((**(short **) h) & 0x200) != 0);
		ReleaseResource (h);
	}

	/*
	 * Window dragging limits are determined from bounding box of desktop.
	 * Upper limits of window sizing are related to that.  Both can involve
	 * multiple monitors, and should allow for menu bar.  dragRect is inset
	 * so as to leave at least 4 pixels of window title bar visible in both
	 * directions (IM I-289).
	 *
	 * GetGrayRgn() bounding box gives desktop extents.  On 64K ROM
	 * machines, GetGrayRgn() might not be present; could use GrayRgn
	 * bounding box, but use qd.screenBits.bounds - menu bar, to avoid
	 * low memory access.  The two should be equivalent.
	 */

	if (has64KROM)
	{
		dragRect = screenBits.bounds;
		dragRect.top += mBarHeight;
	}
	else
	{
		/* GetGrayRgn () already takes menu bar into account */
		dragRect = (**GetGrayRgn ()).rgnBBox;
	}

	SetRect (&growRect, 80, 80,
				dragRect.right - dragRect.left,
				dragRect.bottom - dragRect.top);

	InsetRect (&dragRect, 4, 4);

	/* let application come to front in multitasking environment, TN TB 35, p.8 */

	(void) EventAvail (everyEvent, &dummyEvent);
	(void) EventAvail (everyEvent, &dummyEvent);
	(void) EventAvail (everyEvent, &dummyEvent);
	(void) EventAvail (everyEvent, &dummyEvent);
}


/*
 * Copy the default initialization parameters into the structure
 * pointed to by initParams.
 */

pascal void
SkelGetInitParams (SkelInitParamsPtr initParams)
{
	*initParams = defInitParams;
}


/*
 * Clobber all the menu, window and dialog handlers.  Tell SkelRmveMenu()
 * not to redraw menu bar so it doesn't flicker as menus are removed,
 * then redraw it manually.
 *
 * Before removing window handlers, hide all the windows.  Do this from
 * back to front (more esthetic and speedier).  If a window belongs to a DA,
 * close the DA.  (For early systems (e.g., 4.1), if you leave a DA open,
 * the system crashes the next time you try to open that DA.)
 */

pascal void
SkelCleanup (void)
{
Boolean	oldFlag;
short	theKind;
WindowPeek	w;
WindowPtr	lastVis;

	for (;;)
	{
		lastVis = (WindowPtr) nil;
		for (w = (WindowPeek) FrontWindow (); w != (WindowPeek) nil; w = w->nextWindow)
		{
			if (w->visible)
				lastVis = (WindowPtr) w;
		}
		if (lastVis == (WindowPtr) nil)		/* no more visible windows */
			break;
		if (lastVis != (WindowPtr) nil)
		{
			theKind = ((WindowPeek) lastVis)->windowKind;
			if (theKind < 0)				/* DA, close it */
				CloseDeskAcc (theKind);
			else
				HideWindow (lastVis);
		}
	}

	while (whList != (WHHandle) nil)
		SkelRmveWind ((**whList).whWind);

	oldFlag = mhDrawBarOnRmve;
	mhDrawBarOnRmve = false;
	while (mhList != (MHHandle) nil)
		SkelRmveMenu (GetMenuHandle((**mhList).mhID));
	mhDrawBarOnRmve = oldFlag;
	DrawMenuBar ();
}


/* ----------------------------------- */
/* Execution environment interrogation */
/* ----------------------------------- */



#define trapMask	0x0800

static short
NumToolboxTraps (void)
{
	if (NGetTrapAddress (_InitGraf, ToolTrap)
		== NGetTrapAddress (0xaa6e, ToolTrap))
		return (0x200);
	return (0x400);
}


static TrapType
GetTrapType (short theTrap)
{
	return ((theTrap & trapMask) ? ToolTrap : OSTrap);
}


pascal Boolean
SkelTrapAvailable (short theTrap)
{
TrapType	tType;

	if ((tType = GetTrapType (theTrap)) == ToolTrap)
	{
		theTrap &= 0x07ff;
		if (theTrap >= NumToolboxTraps ())
			theTrap = _Unimplemented;
	}
	return (NGetTrapAddress (theTrap, tType)
				!= NGetTrapAddress (_Unimplemented, ToolTrap));
}


/*
 * Query the TransSkel execution environment.  Shouldn't be called until
 * after SkelInit() has been called.  Result is undefined if selector isn't
 * legal.
 */

pascal long
SkelQuery (short selector)
{
long	result;
Rect	r;
RgnHandle	rgn;

	switch (selector)
	{
	case skelQVersion:
		result = ((long) skelMajorRelease << 16) | skelMinorRelease;
		break;
	case skelQSysVersion:
		result = sysVersion;
		break;
	case skelQHasWNE:
		result = hasWNE ? 1 : 0;
		break;
	case skelQHas64KROM:
		result = has64KROM ? 1 : 0;
		break;
	case skelQMBarHeight:
		result = mBarHeight;
		break;
	case skelQHasColorQD:
		result = skelEnv.hasColorQD ? 1 : 0;
		break;
	case skelQQDVersion:
		/* get QuickDraw version number */
		if (!hasGestalt
			|| Gestalt (gestaltQuickdrawVersion, &result) != noErr)
			result = 0;					/* assume original QuickDraw */
		break;
	case skelQInForeground:
		result = inForeground ? 1 : 0;
		break;
	case skelQHasGestalt:
		result = hasGestalt ? 1 : 0;
		break;
	case skelQHasAppleEvents:
		result = hasAppleEvents ? 1 : 0;
		break;
	case skelQGrayRgn:
		rgn = NewRgn ();
		if (rgn != (RgnHandle) nil)
		{
			if (has64KROM)
			{
				r = screenBits.bounds;
				r.top += mBarHeight;
				RectRgn (rgn, &r);
			}
			else
			{
				/* GetGrayRgn () already takes menu bar into account */
				CopyRgn (GetGrayRgn (), rgn);
			}
		}
		result = (long) rgn;
		break;
	default:
		/* result is undefined! */
		break;
	}
	return (result);
}

/* ------------------------------------- */
/* Event loop initiation and termination */
/* ------------------------------------- */


/*
 * Main event loop.
 *
 * - Take care of DA's with SystemTask() if necessary.
 * - Get an event.
 * - Pass event to event router.
 *	
 * doneFlag is restored to its previous value upon exit.  This allows
 * SkelEventLoop() to be called recursively.
 */

pascal void
SkelEventLoop (void)
{
EventRecord	evt;
Boolean		oldDoneFlag;
long		waitTime;

	oldDoneFlag = doneFlag;		/* save in case this is a recursive call */
	doneFlag = false;			/* set for this call */
	while (!doneFlag)
	{
		if (hasWNE)
		{
			waitTime = (inForeground ? fgWaitTime : bgWaitTime);
			(void) WaitNextEvent (eventMask, &evt, waitTime, nil);
		}
		else
		{
			/*
			 * On some early versions of the system software, it cannot
			 * be assumed that the event contains a null event if the
			 * GetNextEvent() return value is false.  GetNextEvent() calls
			 * SystemEvent() to handle some DA events, and returns false
			 * if the event was handled.  However, in such cases the event
			 * record may still have the event that occurred, *not* a null
			 * event.  To avoid problems later with misinterpreting the
			 * event as non-null, force it to look like a null event.
			 */
			SystemTask ();
			if (!GetNextEvent (eventMask, &evt))
				evt.what = nullEvent;
		}

			SkelRouteEvent (&evt);
	}
	doneFlag = oldDoneFlag;	/* restore in case this was a recursive call */
}


/*
 * Tell current instance of SkelEventLoop() to drop dead
 */

pascal void
SkelStopEventLoop (void)
{
	doneFlag = true;
}


/* ----------------- */
/* Event dispatching */
/* ----------------- */


/*
 * Route a single event and run window idle procedures.
 *
 * If the event is a null-event, call the "no-event" handler for the front
 * window and for any other windows with idle procedures that are always
 * supposed to run.  This is done in such a way that it is safe for idle
 * procs to remove the window handler for their own window if they want
 * (unlikely, but...).
 */

pascal void
SkelRouteEvent (EventRecord *evt)
{
WHHandle	wh, wh2;
GrafPtr		tmpPort;
WindowPtr	w;
SkelWindIdleProcPtr	p;

	RouteEvent (evt);

	/*
	 * Run applicable window idle procs.  Make sure to save and restore
	 * the port, since idle procs for the non-active window may be run.
	 */

	if (evt->what == nullEvent)
	{
		GetPort (&tmpPort);
		for (wh = whList; wh != (WHHandle) nil; wh = wh2)
		{
			wh2 = (**wh).whNext;
			w = (**wh).whWind;
			if (w == FrontWindow () || !(**wh).whFrontOnly)
			{
				if ((p = (**wh).whIdle) != (SkelWindIdleProcPtr) nil)
				{
					if (!hasWNE)
						SystemTask ();
					SetPort (w);
					(*p) ();
				}
			}
		}
		SetPort (tmpPort);
	}
}


/*
 * General event dispatch routine.
 *
 * If there is an event-handling hook and it handles the event, the
 * event is not further processed here.  Otherwise, run the application's idle
 * time procedure if the event is a null event, then process the event.
 *
 * Null events are sent through DialogSelect() if a dialog is active.  This
 * is necessary to make sure  the caret blinks if a dialog has any editText
 * items.
 *
 * Network events are not supported as per the deprecation in TN NW 07.
 * Application-defined events 1, 2 and 3 are not handled, either.
 */

static void
RouteEvent (EventRecord *evt)
{
Point		evtPt;
GrafPtr		evtPort;
short		evtPart;
short		evtMods;
char		evtChar;
long		evtMsge;
unsigned char evtCode;
WHHandle	wh;
WindowPtr	frontWind;
Boolean		frontIsDlog;
short		osMsge;
Boolean		osResume;
Boolean		osClipCvt;
Rect		r1, r2;
WStateData	**wdh;
SignedByte	state;

	/* save values for SkelGetCurrentEvent() and SkelGetModifiers() */

	eventPtr = evt;
	eventModifiers = evt->modifiers;

	/* don't bother handling event below if event hook does so here */
	
	if (pEvent != (SkelEventHookProcPtr) nil && (*pEvent) (evt))
		return;

	frontWind = FrontWindow ();
	frontIsDlog = SkelIsDlog (frontWind);

	evtPt = evt->where;
	evtMods = evt->modifiers;
	evtMsge = evt->message;

	switch (evt->what)
	{
	case nullEvent:
		/*
		 * Run the application idle-time function.  If the front window is
		 * a dialog window, pass the event to the dialog event handler; this
		 * is necessary to make the caret blink if it has an edit text item.
		 * Don't use frontWind after calling the idle-time function, since
		 * the function might change the front window!
		 */
		if (pIdle != (SkelIdleProcPtr) nil)
			(*pIdle) ();
		if (SkelIsDlog (FrontWindow ()))
			DoDlogEvt (FrontWindow (), evt);
		break;

	/*
	 * Mouse click.  Get the window in which the click occurred, and
	 * the part of the window.
	 */
	case mouseDown:
		evtPart = FindWindow (evtPt, &evtPort);
		wh = GetWHandler (evtPort);

		/*
		 * Beep if a click occurs outside of a movable modal dialog box.
		 * Exceptions: allow clicks in menu bar, and command-clicks in
		 * drag region of underlying windows.
		 */

		if (SkelIsMMDlog (frontWind)
			&& !PtInRgn (evtPt, ((WindowPeek) frontWind)->strucRgn))
		{
			if (evtPart != inMenuBar
				&& !(evtPart == inDrag && evtPort != frontWind && (evtMods & cmdKey)))
			{
				SysBeep (1);
				break;
			}
		}

		switch (evtPart)
		{

		/*
		 * Click in desk accessory window.  Pass back to the system.
		 */
		case inSysWindow:
			SystemClick (evt, evtPort);
			break;

		/*
		 * Click in menu bar.  Track the mouse and execute
		 * selected command, if any.
		 */
		case inMenuBar:
			DoMenuHook ();
			DoMenuCommand (MenuSelect (evtPt));
			break;

		/*
		 * Click in grow box.  Resize window.
		 */
		case inGrow:
			DoGrow (wh, evtPt);
			break;

		/*
		 * Click in title bar.  Drag the window around.
		 * Problem fix:  DragWindow() seems to call StillDown()
		 * first, so that clicks in drag regions while machine is
		 * busy don't otherwise bring window to front if the mouse
		 * is already up by the time DragWindow() is called.  So the
		 * window is selected first to make sure it's at least
		 * activated (unless the command key is down, IM I-289).
		 *
		 * Also offset the window's userState by the amount of the drag
		 * (it'd be simpler to set it to the final content rect but the
		 * window might be in zoomed state rather than user state).
		 */
		case inDrag:
			if (evtPort != frontWind && (evtMods & cmdKey) == 0)
				SelectWindow (evtPort);
			SkelGetWindContentRect (evtPort, &r1);		/* post-drag position */
			DragWindow (evtPort, evtPt, &dragRect);
			SkelGetWindContentRect (evtPort, &r2);		/* post-drag position */
			wdh = (WStateData **)(((WindowPeek) evtPort)->dataHandle);
			state = HGetState ((Handle) wdh);
			HLock ((Handle) wdh);
			OffsetRect (&(**wdh).userState, r2.left - r1.left, r2.top - r1.top);
			HSetState ((Handle) wdh, state);
			break;

		/*
		 * Click in close box.  Call the close proc if the window
		 * has one.
		 */
		case inGoAway:
			if (TrackGoAway (evtPort, evtPt))
				DoClose (wh);
			break;

		/*
		 * Click in zoom box.  Track the click and then zoom the
		 * window if necessary.
		 */
		case inZoomIn:
		case inZoomOut:
			if (TrackBox (evtPort, evtPt, evtPart))
				DoZoom (wh, evtPart);
			break;

		/*
		 * Click in content region.  If the window wasn't frontmost
		 * (active), just select it, otherwise pass the click to the
		 * window's mouse click handler.  Exception: if the application
		 * wants to receive content clicks event in non-frontmost windows,
		 * select the window and "repeat" the click.
		 */
		case inContent:
			if (evtPort != frontWind)
			{
				SelectWindow (evtPort);
				if (!getFrontClicks)	/* don't pass click to handler */
					break;
				SetPort (evtPort);
			}
			if (frontIsDlog)
				DoDlogEvt (evtPort, evt);
			else
				DoMouse (wh, evt);
			break;

		}
		break;	/* mouseDown */

	/*
	 * Key down event.  If the command key was down, process as menu
	 * item selection, otherwise pass the character and the modifiers
	 * flags to the active window's key handler.
	 *
	 * Command-period is not supposed to be used as a menu-item equivalent.
	 * Consequently, that's noticed as a special case and not passed to
	 * the menu routines.
	 */
	case keyDown:
	case autoKey:
		evtChar = evtMsge & charCodeMask;
		evtCode = (evtMsge & keyCodeMask) >> 8;	/* hope bit 7 isn't set! */

		if ((evtMods & cmdKey) && !SkelCmdPeriod (evt))	/* try menu equivalent */
		{
			DoMenuHook ();
			if (mhList) DoMenuCommand (MenuKey (evtChar));
			break;
		}

		if (frontIsDlog)
			DoDlogEvt (frontWind, evt);
		else
			DoKey (GetWHandler (frontWind), evtChar, evtCode, evtMods);
		break;

	/*
	 * Key up event.  Key-ups are signified by setting the high bit
	 * of the key code.  This never executes unless the application
	 * changes the system event mask *and* the TransSkel event mask.
	 */
	case keyUp:
		evtChar = evtMsge & charCodeMask;			/* probably 0? */
		evtCode = ((evtMsge & keyCodeMask) >> 8) | 0x80;

		if (frontIsDlog)
			DoDlogEvt (frontWind, evt);
		else
			DoKey (GetWHandler (frontWind), evtChar, evtCode, evtMods);
		break;

	/*
	 * Update a window.
	 */
	case updateEvt:
		DoUpdate (evt);
		break;

	/*
	 * Activate or deactivate a window.
	 */
	case activateEvt:
		DoActivate (evt);
		break;

	/*
	 * handle inserts of uninitialized disks.  Deactivate the frontmost
	 * window since the disk-init dialog doesn't do anything with
	 * activate events for other windows.
	 */
	case diskEvt:
		if (HiWord (evtMsge) != noErr)
		{
			SkelActivate (FrontWindow (), false);
			DILoad ();
			(void) DIBadMount (diskInitPt, evtMsge);
			DIUnload ();
		}
		break;

	case osEvt:				/* aka app4Evt aka MultiFinder event */
		/* rip the message field into constituent parts */
		osMsge = ((evtMsge >> 24) & 0xff);			/* high byte */
		osResume = (Boolean) ((evtMsge & resumeFlag) != 0);
		osClipCvt = (Boolean) ((evtMsge & convertClipboardFlag) != 0);

		switch (osMsge)
		{
		case suspendResumeMessage:
			/*
			 * Tell application it's being suspended or resumed
			 * Tell application to convert scrap if necessary
			 */
		
			inForeground = osResume;
			if (pSuspendResume != (SkelSuspendResumeProcPtr) nil)
				(*pSuspendResume) (inForeground);
			if (!osResume)			/* always convert on suspend */
				osClipCvt = true;
			if (osClipCvt && pClipCvt != (SkelClipCvtProcPtr) nil)
				(*pClipCvt) (inForeground);
			break;

		case mouseMovedMessage:
			/* recompute mouse region -- not implemented */
			break;

		/*
		 * 0xfd is a child-died event -- not implemented here since it's
		 * only had limited use, e.g., by certain debuggers.  The child pid
		 * is byte 2 ((evtMsge >> 16) & 0xff)
		case 0xfd:
			break;
		 */

		default:				/* other OS event */
			/* pass event to catch-all handler -- not implemented */
			break;
		}
		break;

	case kHighLevelEvent:
		if (pAEHandler != (SkelAEHandlerProcPtr) nil)
			(*pAEHandler) (evt);
		break;
	}
}


/*
 * Activate or deactivate a window by synthesizing a fake
 * activate event and sending it through the event router.
 * Useful for activating a window when you don't know its
 * activate function.
 */

pascal void
SkelActivate (WindowPtr w, Boolean active)
{
EventRecord	evt;

	if (w != (WindowPtr) nil)
	{
		evt.what = activateEvt;
		evt.modifiers = active ? activeFlag : 0;
		evt.when = TickCount ();
		SetPt (&evt.where, 0, 0);
		evt.message = (long) w;
		SkelRouteEvent (&evt);
	}
}


/*
 * Call a window's close procedure.  Useful for closing a window when you
 * don't know its close function.
 *
 * This function knows how to close Desk Accessories.
 */

pascal void
SkelClose (WindowPtr w)
{
	if (w != (WindowPtr) nil)
	{
		if (((WindowPeek) w)->windowKind < 0)		/* DA window */
			CloseDeskAcc (((WindowPeek) w)->windowKind);
		else
			DoClose (GetWHandler (w));
	}
}


/*
 * Set the TransSkel event mask.  Does not have anything to do with the
 * system event mask.  See TPN 3.
 */

pascal void
SkelSetEventMask (short mask)
{
	eventMask = mask;
}


/*
 * Return the event mask.
 */

pascal short
SkelGetEventMask (void)
{
	return (eventMask);
}


/*
 * Install an idle-time task.  If p is nil, the current task is
 * disabled.
 */

pascal void
SkelSetIdle (SkelIdleProcPtr p)
{
	pIdle = p;
}


/*
 * Return the current idle-time task.  Return nil if none.
 */

pascal SkelIdleProcPtr
SkelGetIdle (void)
{
	return (pIdle);
}


/*
 * Install an event-inspecting hook.  If p is nil, the hook is
 * disabled.
 */

pascal void
SkelSetEventHook (SkelEventHookProcPtr p)
{
	pEvent = p;
}


/*
 * Return the current event-inspecting hook.  Return nil if none.
 */

pascal SkelEventHookProcPtr
SkelGetEventHook (void)
{
	return (pEvent);
}


pascal void
SkelSetSuspendResume (SkelSuspendResumeProcPtr p)
{
	pSuspendResume = p;
}


pascal SkelSuspendResumeProcPtr
SkelGetSuspendResume (void)
{
	return (pSuspendResume);
}


pascal void
SkelSetClipCvt (SkelClipCvtProcPtr p)
{
	pClipCvt = p;
}


pascal SkelClipCvtProcPtr
SkelGetClipCvt (void)
{
	return (pClipCvt);
}


pascal void
SkelSetWaitTimes (long fgTime, long bgTime)
{
	fgWaitTime = fgTime;
	bgWaitTime = bgTime;
}


pascal void
SkelGetWaitTimes (long *pFgTime, long *pBgTime)
{
	if (pFgTime != (long) nil)
		*pFgTime = fgWaitTime;
	if (pBgTime != (long) nil)
		*pBgTime = bgWaitTime;
}


pascal EventRecord *
SkelGetCurrentEvent (void)
{
	return (eventPtr);
}


pascal short
SkelGetModifiers (void)
{
	return (eventModifiers);
}


pascal void
SkelSetAEHandler (SkelAEHandlerProcPtr p)
{
	pAEHandler = p;
}


pascal SkelAEHandlerProcPtr
SkelGetAEHandler (void)
{
	return (pAEHandler);
}


/* -------------------------------------------------------------------- */
/*					Window-handler event routing routines				*/
/*																		*/
/*	See manual for discussion of port-setting behavior: the current		*/
/*	port is set to a window when it becomes active in DoActivate().		*/
/* -------------------------------------------------------------------- */


/*
 * Process dialog event.  dlog is the dialog to which the event applies.
 * Give the filter a chance at the event first.  If the filter doesn't
 * handle it, pass the event to DialogSelect().  If DialogSelect() selects
 * an item, pass the item to the window's item selection function, if
 * there is one.  This is used to dispose of dialog events that aren't
 * handled in some other more direct fashion.
 */


static void
DoDlogEvt (DialogPtr dlog, EventRecord *evt)
{
short		item;
WHHandle	wh;
SkelWindSelectProcPtr	select;

	if (DoDlogFilter (dlog, evt))
		return;

	if (DialogSelect (evt, &dlog, &item)
	   && (wh = GetWHandler (dlog)) != (WHHandle) nil
	   && (select = (**wh).whSelect) != (SkelWindSelectProcPtr) nil)
	{
		(*select) (dlog, item);
	}
}

/*
 * Run a dialog's filter function to give the filter first chance
 * at the event.
 *
 * The filter function returns false if it doesn't handle the event.
 * It returns true if it handled the event, in which case it should
 * set the item parameter.  The item will be passed to the dialog's
 * item selection function.
 *
 * If the filter function returns true, look up the handler again
 * just in case the filter function also called SkelRmveDlog().
 * If it did, the handler will have become invalid.  Looking it
 * up again avoids disaster.
 */

static Boolean
DoDlogFilter (DialogPtr dlog, EventRecord *evt)
{
short		item;
WHHandle	wh;
SkelWindSelectProcPtr	select;
ModalFilterProcPtr		filter;
Boolean	result = false;

	if ((wh = GetWHandler (dlog)) != (WHHandle) nil
		&& (filter = (**wh).whFilter) != (ModalFilterProcPtr) nil)
	{
		if ((*filter) (dlog, evt, &item))
		{
			if ((wh = GetWHandler (dlog)) != (WHHandle) nil
				&& (select = (**wh).whSelect) != (SkelWindSelectProcPtr) nil)
				(*select) (dlog, item);
			result = true;
		}
	}
	return (result);
}


/*
 * Pass local mouse coordinates, click time, and the modifiers flag
 * word to the handler.  Should not be necessary to set the port, as
 * the click is passed to the active window's handler.
 */

static void
DoMouse (WHHandle h, EventRecord *evt)
{
Point	thePt;

	if (h != (WHHandle) nil && (**h).whMouse != (SkelWindMouseProcPtr) nil)
	{
		SetPort((**h).whWind);
		thePt = evt->where;	/* make local copy */
		GlobalToLocal (&thePt);
		(*(**h).whMouse) (thePt, evt->when, evt->modifiers);
	}
}


/*
 * Pass the character code, key code and the modifiers flag word to
 * the handler. Should not be necessary to set the port, as the click
 * is passed to the active window's handler.
 */

static void
DoKey (WHHandle h, char c, unsigned char code, short mods)
{
	if (h != (WHHandle) nil && (**h).whKey != (SkelWindKeyProcPtr) nil) {
		SetPort((**h).whWind);
		(*(**h).whKey) ((short) c, (short) code, mods);
	}
}


/*
 * Call the window updating procedure, passing to it an indicator whether
 * the window has been resized or not.  Then clear the flag, assuming
 * the update proc took whatever action was necessary to respond to
 * resizing.
 *
 * The Begin/EndUpdate stuff is done to clear the update region even if
 * the handler doesn't have any update proc.  Otherwise the Window
 * Manager will keep generating update events for the window, stalling  
 * updates of other windows.
 *
 * For dialog windows, UpdtDialog() does the normal item updating.  The
 * filter procedure can take care of non-item drawing, such as a bold
 * outline around a default button.
 *
 * Saves, sets, and restore the port, since it's not always the
 * active window that is updated.
 */

static void
DoUpdate (EventRecord *evt)
{
WHHandle	h;
GrafPtr	port;
GrafPtr	tmpPort;

	port = (WindowPtr) evt->message;

	GetPort (&tmpPort);
	SetPort (port);
	BeginUpdate (port);
	if (SkelIsDlog (port))
	{
		if (!DoDlogFilter (port, evt))
			UpdateDialog (port, port->visRgn);	/* let Dialog Manager finish update */
	}
	else
	{
		h = GetWHandler (port);
		if (h != (WHHandle) nil)
		{
			if ((**h).whUpdate != (SkelWindUpdateProcPtr) nil)
				(*(**h).whUpdate) ((**h).whSized);
			(**h).whSized = false;
		}
	}
	EndUpdate (port);
	SetPort (tmpPort);
}


/*
 * Pass activate/deactivate notification to handler.  On activate,
 * set the port to the window coming active.  Normally this is done by
 * the user clicking in a window.
 *
 * *** BUT ***
 * Under certain conditions, a deactivate may be generated for a window
 * that has never had the port set to it by a preceding activate.  For
 * instance, if an application puts up window A, then window B in front
 * of A, then starts processing events, the first events will be a
 * deactivate for A and an activate for B.  Therefore, since it can't be
 * assumed that an activate ever set the port to A, the port needs to be
 * set for deactivates as well so drawing occurs in the correct port.
 *
 * This matters not a whit for the more usual cases that occur.  If a
 * deactivate for one window is followed by an activate for another, the
 * port will still be switched properly to the newly active window.  If
 * no activate follows the deactivate, the deactivated window is the last
 * one, and it doesn't matter what the port ends up set to, anyway.
 *
 * On deactivate, port is saved and restored in case deactivate is due to
 * a modal dialog having been brought in front and port changed to it
 * explicitly by the application.  The deactivate shouldn't leave the port
 * changed away from the dialog!
 *
 * For dialogs, DoDlogEvt() is called, allowing DialogSelect() to do
 * whatever it does for dialog activates.  The handler's activate procedure
 * is called in addition to this (e.g., to hilite controls or text selections,
 * adjust menus).
 */

static void
DoActivate (EventRecord *evt)
{
WHHandle	h;
GrafPtr	port;
GrafPtr	tmpPort;
Boolean	active;

	active = (evt->modifiers & activeFlag);
	port = (WindowPtr) evt->message;


	GetPort (&tmpPort);	/* save so can restore if deactivate */
	SetPort (port);
	if (SkelIsDlog (port))
		DoDlogEvt (port, evt);
	else
	{
		h = GetWHandler (port);
		if (h != (WHHandle) nil)
		{
			if ((**h).whActivate != (SkelWindActivateProcPtr) nil)
				(*(**h).whActivate) (active);
		}
	}
	if (!active)
		SetPort (tmpPort);
}


/*
 * Execute a window handler's close box proc.  The close proc for
 * handlers for temp windows that want to remove themselves when the
 * window is closed can call SkelRmveWind to dispose of the window
 * and remove the handler from the window handler list.  Thus, windows
 * may be dynamically created and destroyed without filling up the
 * handler list with a bunch of invalid handlers.
 *
 * If the handler doesn't have a close proc, just hide the window.
 * The host should provide some way of reopening the window (perhaps
 * a menu selection).  Otherwise the window will be lost from user
 * control if it is hidden, since it won't receive user-initiated
 * events.
 *
 * This is called both for regular and dialog windows.
 *
 * Normally this is invoked because the close box of the active window
 * is clicked, in which case the port will be set to the window.  However,
 * SkelClose() allows the application to close an aritrary window, not just
 * the frontmost one -- so the port is saved and restored.
  */

static void
DoClose (WHHandle h)
{
GrafPtr	tmpPort;

	if (h != (WHHandle) nil)
	{
		GetPort (&tmpPort);
		SetPort ((**h).whWind);
		if ((**h).whClose != (SkelWindCloseProcPtr) nil)
			(*(**h).whClose) ();
		else
			HideWindow ((**h).whWind);
		SetPort (tmpPort);
	}
}


/*
 * Execute a window handler's clobber proc.  This is called both
 * for regular and dialog windows.
 *
 * Saves sets, and restores the port, since any window (not just active
 * one) may be clobbered at any time.
 *
 * Don't need to check whether handler is nil, as in other handler
 * procedures, since this is only called by SkelRmveWind with a
 * known-valid handler.
 */

static void
DoClobber (WHHandle h)
{
GrafPtr	tmpPort;

	GetPort (&tmpPort);
	SetPort ((**h).whWind);
	if ((**h).whClobber != (SkelWindClobberProcPtr) nil)
		(*(**h).whClobber) ();
	SetPort (tmpPort);
}


/*
 * Handlers for window events not requiring application handler routines
 * to be called.
 */


/*
 * Have either zoomed a window or sized it manually.  Invalidate
 * it to force an update and set the 'resized' flag in the window
 * handler true.  The port is assumed to be set to the port that changed
 * size.  Handler is assumed non-nil.
 */

static void
TriggerUpdate (WHHandle h)
{
GrafPtr	port = (**h).whWind;

	InvalRect (&port->portRect);
	(**h).whSized = true;
}

/* Extrnal interface to TriggerUpdate -- L. Tierney */
pascal void SkelTriggerUpdate (WindowPtr w)
{
WHHandle h;
GrafPtr savePort;

	GetPort(&savePort);
	SetPort(w);
	h = GetWHandler(w);
	if (h != nil)
	  TriggerUpdate(h);
	SetPort(savePort);
}

/*
 * Size a window, using the grow limits in the handler record.
 *
 * The portRect is invalidated to force an update event.  The window's
 * update handler procedure should check the parameter passed to it to
 * check whether the window has changed size, if it needs to adjust
 * itself to the new size.  THIS IS A CONVENTION.  Update procs must
 * notice grow "events", there is no procedure specifically for that.
 *
 * The clipping rectangle is not reset.  If the host application
 * keeps the clipping set equal to the portRect or something similar,
 * then it will have to arrange to treat window growing with more
 * care.
 *
 * Since the grow region of only the active window may be clicked,
 * it should not be necessary to set the port.
 */

static void
DoGrow (WHHandle h, Point startPt)
{
GrafPtr	growPort;
Rect	growRect;
long	growRes;
GrafPtr	tmpPort;                            /* port savig added -- L. Tierney */

	if (h != (WHHandle) nil)
	{
		growPort = (**h).whWind;
		growRect = (**h).whGrow;

		/* growRes will be zero if the size was not actually changed */
	
		GetPort (&tmpPort);					/* save port and set to */
		SetPort (growPort);				    /* window to grow -- L. Tierney */
		if (growRes = GrowWindow (growPort, startPt, &growRect))
		{
			SizeWindow (growPort, LoWord (growRes), HiWord (growRes), false);
			TriggerUpdate (h);
		}
		SetPort (tmpPort);		            /* restore original port -- L. Tierney */
	}
}


/*
 * Zoom the current window.  Very similar to DoGrow, but window
 * is erased before zooming for nicer visual effect (per IM IV-50,
 * TN TB 30, p.4).
 * 
 * Normally, since only the active window has a visible zoom box and
 * TransSkel sets the port to active window, this routine is triggered
 * by user-initiated clicks in zoom box and the port will be set to
 * the zoomed window.
 *
 * However, it is possible for zooms to be software initiated by the
 * application itself on any window; for such cases the port needs
 * to be saved and set before the zoom and restored afterward.
 */

static void
DoZoom (WHHandle h, short zoomDir)
{
GrafPtr	w;
GrafPtr	tmpPort;
Rect	r, growRect;

	if (h != (WHHandle) nil)
	{
		w = (**h).whWind;
		GetPort (&tmpPort);					/* save port and set to */
		SetPort (w);						/* zoomed window */
		if ((**h).whZoom != (SkelWindZoomProcPtr) nil)
			((**h).whZoom) (w, zoomDir);	/* custom zoom proc */
		else if (zoomProc != (SkelWindZoomProcPtr) nil)
			(*zoomProc) (w, zoomDir);		/* custom default zoom proc */
		else								/* default zooming */
		{
			EraseRect (&w->portRect);
			if (zoomDir == inZoomOut)	/* zooming to default state */
			{
				/*
				 * Get the usable area of the device containing most of the
				 * window.  (Can ignore the result because the rect is always
				 * correct.  Pass nil for device parameter because it's
				 * irrelevant.)  Then adjust rect for title bar height, and
				 * inset it slightly.
				 */
				(void) SkelGetWindowDevice (w, (GDHandle *) nil, &r);
				r.top += SkelGetWindTitleHeight (w) - 1;
				/* leave 3-pixel border */
				InsetRect (&r, 3, 3);
				/* clip to grow limits */
				growRect = (**h).whGrow;
				growRect.left = growRect.top = 0;
				OffsetRect (&growRect, r.left, r.top);
				SectRect (&r, &growRect, &r);
				(**(WStateData **)(((WindowPeek)w)->dataHandle)).stdState = r;
			}
			ZoomWindow (w, zoomDir, false);
		}
		SetPort (tmpPort);		/* restore original port */
		TriggerUpdate (h);
	}
}


/* --------------------------------------------------------- */
/* Window handler installation/removal/modification routines */
/* --------------------------------------------------------- */


/*
 * Install handler for a window and set current port to it.  Remove
 * any previous handler for it.  Pass the following parameters:
 *
 * w
 *		Pointer to the window to be handled.  Must be created by host.
 * doMouse
 *		Proc to handle mouse clicks in window.  The proc will be
 * 		passed the point (in local coordinates), the time of the
 * 		click, and the modifier flags word.
 * doKey
 *		Proc to handle key clicks in window.  The proc will be passed
 * 		the character and the modifier flags word.
 * doUpdate
 *		Proc for updating window.  TransSkel brackets calls to update
 * 		procs with calls to BeginUpdate and EndUpdate, so the visRgn
 * 		is set up correctly.  A flag is passed indicating whether the
 * 		window was resized or not.  BY CONVENTION, the entire portRect
 * 		is invalidated when the window is resized or zoomed.  That way,
 * 		the handler's update proc can redraw the entire content region
 * 		without interference from BeginUpdate/EndUpdate.  The flag
 * 		is set to false after the update proc is called; the
 * 		assumption is made that the proc will notice the resizing and
 * 		respond appropriately.
 * doActivate
 *		Proc to execute when window is activated or deactivated.
 * 		A boolean is passed to it which is true if the window is
 * 		coming active, false if it's going inactive.
 * doClose
 *		Proc to execute when mouse clicked in close box.  Useful
 * 		mainly to temp window handlers that want to know when to
 * 		self-destruct (with SkelRmveWind).
 * doClobber
 *		Proc for disposal of handler's data structures
 * doWIdle
 *		Proc to execute when no events are pending.
 * idleFrontOnly
 *		True if doWIdle should execute on no events only when
 * 		w is frontmost, false if executes all the time.  Note
 * 		that if it always goes, everything else may be slowed down!
 *
 * If a particular procedure is not needed (e.g., key events are
 * not processed by a handler), pass nil in place of the appropriate
 * procedure address.
 *
 * Return true if successful, false if no handler could be allocated.
 * If false is returned, the port will not have been changed.
 */
 
pascal Boolean
SkelWindow (WindowPtr w,
			SkelWindMouseProcPtr doMouse,
			SkelWindKeyProcPtr doKey,
			SkelWindUpdateProcPtr doUpdate,
			SkelWindActivateProcPtr doActivate,
			SkelWindCloseProcPtr doClose,
			SkelWindClobberProcPtr doClobber,
			SkelWindIdleProcPtr doWIdle,
			Boolean idleFrontOnly)
{
WHHandle	whNew, whCur;
SkelWindPropHandle	wph = (SkelWindPropHandle) nil;

	/* Get new handler immediately, fail if can't allocate */

	if ((whNew = New (WHandler)) == (WHHandle) nil)
		return (false);

	/*
	 * If there's a current handler for the window, remove it, but first
	 * grab the property list from it so it can be transferred to the new
	 * handler.
	 */
	
	if ((whCur = GetWHandler (w)) != (WHHandle) nil)
	{
		wph = (**whCur).whProperties;
		(**whCur).whProperties = (SkelWindPropHandle) nil;
		DetachWHandler (whCur);
	}

	/*
	 * Attach new handler to list of handlers.  It is attached to the
	 * beginning of the list, which is simpler; the order is presumably
	 * irrelevant to the host, anyway.
	 *
	 * Then fill in handler fields (including properties attached to any
	 * previous handler).
	 */

	(**whNew).whNext = whList;
	whList = whNew;

	(**whNew).whWind = w;
	(**whNew).whMouse = doMouse;
	(**whNew).whKey = doKey;
	(**whNew).whUpdate = doUpdate;
	(**whNew).whActivate = doActivate;
	(**whNew).whClose = doClose;
	(**whNew).whClobber = doClobber;
	(**whNew).whZoom = (SkelWindZoomProcPtr) nil;
	(**whNew).whIdle = doWIdle;
	(**whNew).whGrow = growRect;
	(**whNew).whSized = false;
	(**whNew).whFrontOnly = idleFrontOnly;
	(**whNew).whFlags = 0;
	(**whNew).whProperties = wph;
	(**whNew).whSelect = (SkelWindSelectProcPtr) nil;
	(**whNew).whFilter = (ModalFilterProcPtr) nil;
	SetPort (w);

	return (true);
}


/*
 * Remove a window handler.  This calls the handler's window disposal
 * routine and then takes the handler out of the handler list and
 * disposes of it (including its property list).
 *
 * SkelRmveWind is also called by SkelRmveDlog.
 */

pascal void
SkelRmveWind (WindowPtr w)
{
WHHandle	h;

	if ((h = GetWHandler (w)) == (WHHandle) nil)
		return;

	DoClobber (h);								/* call disposal routine */
	SkelRmveWindProp (w, skelWPropAll);	/* toss properties */

	DetachWHandler (h);	/* remove handler for window from list */
}


/*
 * Install a handler for a modeless or movable modal dialog window and set
 * the port to it.  Remove any previous handler for it. SkelDialog calls
 * SkelWindow as a subsidiary to install a window handler, then sets
 * the event procedure on return.  A property is also added to the window
 * to indicate that it's a modeless or movable modal dialog.
 *
 * Pass the following parameters:
 *
 * dlog
 *		Pointer to the dialog to be handled.  Must be created by host.
 * doFilter
 *		Filter procedure to look at events before they are otherwise
 *		processed.
 * doSelect
 *		Procedure to execute when an item is "selected" (e.g., a mouse
 *		click occurs in it).
 * doClose
 *		Procedure to execute when mouse clicked in close box.  Useful
 * 		mainly to dialog handlers that want to know when to
 * 		self-destruct (with SkelRmveDlog).
 * doClobber
 *		Procedure for disposal of handler's data structures
 *
 * If a particular procedure is not needed, pass nil in place of
 * the appropriate procedure address.
 *
 * Return true if successful, false if no handler could be allocated.
 * If false is returned, the port will not have been changed.
 */

pascal Boolean
SkelDialog (DialogPtr dlog,
			ModalFilterProcPtr doFilter,
			SkelWindSelectProcPtr doSelect,
			SkelWindCloseProcPtr doClose,
			SkelWindClobberProcPtr doClobber)
{
WHHandle	wh;
short	propType;

	if (!SkelWindow (dlog, nil, nil, nil, nil, doClose, doClobber, nil, true))
		return (false);

	propType = (SkelIsMMDlog (dlog) ? skelWPropMovableModal : skelWPropModeless);
	if (!SkelAddWindProp (dlog, propType, (long) 0))
	{
		SkelRmveDlog (dlog);
		return (false);
	}

	wh = GetWHandler (dlog);
	(**wh).whSelect = doSelect;
	(**wh).whFilter = doFilter;
	return (true);
}


/*
 * Remove a dialog and its handler
 */

pascal void
SkelRmveDlog (DialogPtr dlog)
{
	SkelRmveWind (dlog);
}


/*
 * Override the default sizing limits for a window, or, if w
 * is nil, reset the default limits used by SkelWindow.
 */

pascal void
SkelSetGrowBounds (WindowPtr w, short hLo, short vLo, short hHi, short vHi)
{
WHHandle	wh;
Rect		r;

	if (w == (WindowPtr) nil)
		SetRect (&growRect, hLo, vLo, hHi, vHi);
	else if ((wh = GetWHandler (w)) != (WHHandle) nil)
	{
		SetRect (&r, hLo, vLo, hHi, vHi);
		(**wh).whGrow = r;
	}
}


pascal void 
SkelSetZoom (WindowPtr w, SkelWindZoomProcPtr pZoom)
{
WHHandle	h;

	if (w == (WindowPtr) nil)
		zoomProc = pZoom;
	else if ((h = GetWHandler (w)) != (WHHandle) nil)
		(**h).whZoom = pZoom;
}


/*
 * Return zoom proc associated with window, nil if there isn't one.
 * Return default zoom proc if window is nil.
 */

pascal SkelWindZoomProcPtr
SkelGetZoom (WindowPtr w)
{
WHHandle	h;

	if (w == (WindowPtr) nil)
		return (zoomProc);
	if ((h = GetWHandler (w)) != (WHHandle) nil)
		return ((**h).whZoom);
	return ((SkelWindZoomProcPtr) nil);
}


pascal Boolean
SkelWindowRegistered (WindowPtr w)
{
	return ((Boolean) (GetWHandler (w) != (WHHandle) nil));
}


/*
 * Routines to determine whether a given window is a dialog, or a movable
 * modal dialog.  Safe to pass nil.
 */

pascal Boolean
SkelIsDlog (WindowPtr w)
{
	return (w != (WindowPtr) nil && ((WindowPeek)w)->windowKind == dialogKind);
}


pascal Boolean
SkelIsMMDlog (WindowPtr w)
{
	return (SkelIsDlog (w) && hasGetWVariant && GetWVariant (w) == movableDBoxProc);
}


/* ------------------------ */
/* Handler finders/removers */
/* ------------------------ */

/*
 * Get handler associated with a window.
 *
 * Return nil if window doesn't belong to any known handler.
 *
 * This routine is absolutely fundamental to TransSkel.
 */


static WHHandle
GetWHandler (WindowPtr w)
{
WHHandle	h;

	if (w == (WindowPtr) nil)
		return ((WHHandle) nil);

	if (w == oldWindow) 
		return (oldWHandler);		/* return handler of cached window */

	for (h = whList; h != (WHHandle) nil; h = (**h).whNext)
	{
		if ((**h).whWind == w)
		{
			oldWindow = w;	/* set cached window and handler */
			oldWHandler = h;
			return (h);
		}
	}
	return ((WHHandle) nil);
}


/*
 * Detach a handler from the handler list and destroy it.
 *
 * Clear window cache variable, just in case it points to the window
 * whose hander is being destroyed (and thus has become invalid).
 */

static void
DetachWHandler (WHHandle wh)
{
WHHandle	h, h2;

	if (whList != (WHHandle) nil)		/* if list empty, ignore */
	{
		if (whList == wh)				/* is it the first element? */
		{
			h2 = whList;
			whList = (**whList).whNext;
		}
		else for (h = whList; h != (WHHandle) nil; h = h2)
		{
			h2 = (**h).whNext;
			if (h2 == (WHHandle) nil)
				return;					/* handler not in list! (huh?) */
			if (h2 == wh)				/* found it */
			{
				(**h).whNext = (**h2).whNext;
				break;
			}
		}
		DisposeHandle ((Handle) h2);		/* get rid of handler record */
	}

	oldWindow = (WindowPtr) nil;	/* clear window cache variables */
	oldWHandler = (WHHandle) nil;
}


/* ------------------------------------------------------- */
/* Menu handler installation/removal/modification routines */
/* ------------------------------------------------------- */


/*
 * Install handler for a menu.  Remove any previous handler for it.
 * Pass the following parameters:
 *
 * theMenu
 *		Handle to the menu to be handled.  Must be created by host.
 * doSelect
 *		Proc that handles selection of items from menu.  If this is
 * 		nil, the menu is installed, but nothing happens when items
 * 		are selected from it.
 * doClobber
 *		Proc for disposal of handler's data structures.  Usually
 * 		nil for menus that remain in menu bar until program
 * 		termination.
 * subMenu
 *		True if the menu is a submenu (not installed in menu bar).
 * drawBar
 *		True if menu bar is to be drawn after menu is installed.
 * 		(Ignored if the menu is a submenu.)
 *
 * Return true if successful, false if no handler could be allocated.
 */

pascal Boolean
SkelMenu (MenuHandle m,
			SkelMenuSelectProcPtr doSelect,
			SkelMenuClobberProcPtr doClobber,
			Boolean subMenu,
			Boolean drawBar)
{
MHHandle	mh;
Boolean		oldFlag;

	oldFlag = mhClobOnRmve;		/* remove any previous handler for */
	mhClobOnRmve = false;		/* menu, without redrawing menu bar */
	SkelRmveMenu (m);
	mhClobOnRmve = oldFlag;

	if ((mh = New (MHandler)) != (MHHandle) nil)
	{
		(**mh).mhNext = mhList;
		mhList = mh;
		(**mh).mhID = (**m).menuID;	/* get menu id number */
		(**mh).mhSelect = doSelect;			/* install selection handler */
		(**mh).mhClobber = doClobber;		/* install disposal handler */
		(**mh).mhSubMenu = subMenu;			/* set submenu flag */
		/* install menu in menu bar if not a submenu */
		InsertMenu (m, subMenu ? -1 : 0);
	}
	if (drawBar && !subMenu)
		DrawMenuBar ();
	return ((Boolean) (mh != (MHHandle) nil));
}


/*
 * Remove a menu handler.  This calls the handler's menu disposal
 * routine and then takes the handler out of the handler list and
 * disposes of it.  The menu bar is redrawn if the menu was not a
 * submenu and the global redraw flag hasn't been cleared.
 *
 * The menu MUST be deleted from the menu bar before calling the
 * clobber proc.  Otherwise the menu bar will end up filled with
 * garbage if the menu was allocated with NewMenu (IM I-352).
 */

pascal void
SkelRmveMenu (MenuHandle m)
{
short	mID;
MHHandle	h, h2;
SkelMenuClobberProcPtr	p;

	mID = (**m).menuID;
	if (mhList != (MHHandle) nil)			/* if list empty, ignore */
	{
		if ((**mhList).mhID == mID)	/* is it the first element? */
		{
			h2 = mhList;
			mhList = (**mhList).mhNext;
		}
		else
		{
			for (h = mhList; h != (MHHandle) nil; h = h2)
			{
				h2 = (**h).mhNext;
				if (h2 == (MHHandle) nil)
					return;						/* menu not in list! */
				if ((**h2).mhID == mID)			/* found it */
				{
					(**h).mhNext = (**h2).mhNext;
					break;
				}
			}
		}
		DeleteMenu (mID);
		if (mhDrawBarOnRmve && !(**h2).mhSubMenu)
			DrawMenuBar ();
		if (mhClobOnRmve
			&& (p = (**h2).mhClobber) != (SkelMenuClobberProcPtr) nil)
				(*p) (m);					/* call disposal routine */
		DisposeHandle ((Handle) h2);		/* get rid of handler record */
	}
}


/*
 * General menu-selection handler.  Just passes selection to the handler's
 * select routine.  If the select routine is nil, selecting items from
 * the menu is a nop.
 */

static void
DoMenuCommand (long command)
{
short		menu;
short		item;
MHHandle	mh;

	menu = HiWord (command);
	item = LoWord (command);
	for (mh = mhList; mh != (MHHandle) nil; mh = (**mh).mhNext)
	{
		if (menu == (**mh).mhID && (**mh).mhSelect != (SkelMenuSelectProcPtr) nil)
		{
			(*(**mh).mhSelect) (item, menu);
			break;
		}
	}
	HiliteMenu (0);		/* command done, turn off menu hiliting */
}


/*
 * Menu is about to be pulled down or command-key executed.  Call menu
 * hook if there is one so application can set menus/items appropriately.
 */

static void
DoMenuHook (void)
{
	if (pMenuHook != (SkelMenuHookProcPtr) nil)
		(*pMenuHook) ();
}


pascal void
SkelSetMenuHook (SkelMenuHookProcPtr p)
{
	pMenuHook = p;
}


pascal SkelMenuHookProcPtr
SkelGetMenuHook (void)
{
	return (pMenuHook);
}


/* ------------------------ */
/* Window property routines */
/* ------------------------ */


/*
 * Add a property to a window.  Fail if the window is unregistered
 * or can't allocate memory for a new property structure.  If the
 * window already has such a property, fail.
 *
 * Returns a handle to the new property for success, nil for failure.
 */

pascal Boolean
SkelAddWindProp (WindowPtr w, short propType, long propData)
{
WHHandle		wh;
SkelWindPropHandle	ph;

	if (propType == skelWPropAll)
		return (false);
	if ((ph = SkelGetWindProp (w, propType)) != (SkelWindPropHandle) nil)
		return (false);
	/* if window is unregistered, or can't allocate structure, fail */
	if ((wh = GetWHandler (w)) == (WHHandle) nil
		|| (ph = New (SkelWindProperty)) == (SkelWindPropHandle) nil)
		return (false);
	(**ph).skelWPropType = propType;
	(**ph).skelWPropData = propData;
	(**ph).skelWPropNext = (**wh).whProperties;
	(**wh).whProperties = ph;
	return (true);
}


/*
 * Remove a window property.  Does nothing if the window isn't
 * registered or if the window doesn't have the given property.
 *
 * If propType is skelWPropAll, SkelRmveWindProp() calls itself
 * recursively to remove all the properties on a window.  This
 * means that if you put skelWPropAll into the skelWPropType field
 * of a property, you'll get an infinite loop here.
 */

pascal void
SkelRmveWindProp (WindowPtr w, short propType)
{
WHHandle		wh;
SkelWindPropHandle	ph, ph2, pNext;

	if ((wh = GetWHandler (w)) == (WHHandle) nil
		|| (ph = SkelGetWindProp (w, propType)) == (SkelWindPropHandle) nil)
		return;

	if (propType == skelWPropAll)	/* remove all properties */
	{
		while ((ph = (**wh).whProperties) != (SkelWindPropHandle) nil)
			SkelRmveWindProp (w, (**ph).skelWPropType);
		return;
	}

	/* remove particular property */
	if ((ph2 = (**wh).whProperties) == ph)	/* remove first in list */
		(**wh).whProperties = (**ph).skelWPropNext;
	else
	{
		while ((pNext = (**ph2).skelWPropNext) != (SkelWindPropHandle) nil)
		{
			if (pNext == ph)
			{
				(**ph2).skelWPropNext = (**ph).skelWPropNext;
				break;
			}
			ph2 = pNext;
		}
	}
	DisposeHandle ((Handle) ph);
}


/*
 * Find the given property for the window.  Fail if window is
 * unregistered or has no such property.
 */

pascal SkelWindPropHandle
SkelGetWindProp (WindowPtr w, short propType)
{
WHHandle			wh;
SkelWindPropHandle	ph = (SkelWindPropHandle) nil;

	if ((wh = GetWHandler (w)) != (WHHandle) nil)
	{
		if (propType == skelWPropAll)	/* return head of list */
			ph = (**wh).whProperties;
		else for (ph = (**wh).whProperties; ph != (SkelWindPropHandle) nil; ph = (**ph).skelWPropNext)
		{
			if ((**ph).skelWPropType == propType)
				break;
		}
	}
	return (ph);
}


/*
 * Find the data value for a given property for the window.  Return 0 if window
 * is unregistered or has no such property.
 *
 * If you need to be able to distinquish an error return from a valid zero-value
 * data value, you should call SkelGetWindProp() instead, check for an error,
 * and extract the data value if there was no error.
 *
 * skelWPropAll is not a valid properly type for this call.
 */

pascal long
SkelGetWindPropData (WindowPtr w, short propType)
{
SkelWindPropHandle	ph;

	ph = SkelGetWindProp (w, propType);
	if (ph != (SkelWindPropHandle) nil)
		return ((**ph).skelWPropData);
	return (0);
}

/*
 * SkelCmdPeriod.c -- Figure out, in an internationally compatible way, whether
 * an event is a command-period key event.
 *
 * See TN TE 23, International Cancelling, for the rationale on how this works.
 */

# include	<Script.h>

# include	"TransSkel1.h"


# define	kMaskModifiers	0xfe00
# define	kMaskVirtualKey	0x0000ff00
# define	kMaskAscii1		0x00ff0000
# define	kMaskAscii2		0x000000ff
# define	period			'.'


pascal Boolean
SkelCmdPeriod (EventRecord *evt)
{
short	keyCode;
long	virtualKey;
long	keyCId;
long	keyInfo;
long	state;
long	lowChar, highChar;
Handle	hKCHR;

	if (evt->what == keyDown || evt->what == autoKey)
	{
		if (evt->modifiers & cmdKey)			/* cmd key is down */
		{
			/* find out ASCII equivalent for key */
			virtualKey = (evt->message & kMaskVirtualKey) >> 8;
			/* "and" out command key, "or" in the virtual key */
			keyCode = (evt->modifiers & kMaskModifiers) | virtualKey;
			keyCId = GetScriptVariable (GetScriptManagerVariable (smKeyScript), smScriptKeys);
			hKCHR = GetResource ('KCHR', keyCId);
			if (hKCHR != (Handle) nil)
			{
				state = 0;
				/* don't bother locking because KeyTrans doesn't move memory */
				keyInfo = KeyTranslate (*hKCHR, keyCode, (void *) &state);
				ReleaseResource (hKCHR);
			}
			else
				keyInfo = evt->message;

			lowChar = keyInfo & kMaskAscii2;
			highChar = (keyInfo & kMaskAscii1) >> 16;
			if (lowChar == period || highChar == period)
				return (true);
		}
	}
	return (false);
}

/*
 * Get the device containing most of a window's content rectangle and the
 * largest usable rectangle on that device.  If the device is the main
 * device, the rectangle is adjusted not to contain the menu bar area.
 *
 * The result is used by TransSkel for window zooming.  Normally, the
 * caller adjusts the top of the rectangle to account for the title bar
 * height, then insets it by a few pixels in order to leave a little
 * space around the window edges.  SkelGetWindowDevice() itself does
 * not account for the title bar height.  That responsibility is
 * left with the caller, which can call SkelGetWindTitleHeight() to
 * find this out.
 *
 * Returns true if the window overlaps some device in its current
 * position.  False can be returned, for instance, if an application
 * saves document window positions and a document is saved while
 * positioned on a second monitor, then opened on a system that doesn't
 * have a second monitor.
 *
 * The returned device value will be nil on systems that don't have GDevices
 * (i.e.,, that don't support Color QuickDraw), even if the function result
 * is true.
 *
 * If the window does not overlap any device, the device and devRect arguments
 * are filled in with the values for the main device.  The rectangle can
 * be used to position the window so it can be made visible.
 *
 * If the caller is not interested in the device or rectangle, nil
 * can be passed instead of the address of a variable.
 *
 * References: TN TB 30, HIN 6, HIN 7.
 */

# include	"TransSkel1.h"


pascal Boolean
SkelGetWindowDevice (WindowPtr wind, GDHandle *gd, Rect *devRect)
{
Rect	r;
Boolean	isMain;
Boolean	result;

	/* get window content rect in global coordinates */

	SkelGetWindContentRect (wind, &r);
	result = SkelGetRectDevice (&r, gd, devRect, &isMain);
	if (isMain && devRect != (Rect *) nil)
		devRect->top += SkelQuery (skelQMBarHeight);
	return (result);
}

/*
 * Determine height of a window's title bar.  This is determined as the
 * difference between the top of the window's structure and content rects.
 *
 * This function will not necessarily work for windows with strange shapes
 * or that have a title bar on the side.
 */

# include	"TransSkel1.h"


pascal short
SkelGetWindTitleHeight (WindowPtr w)
{
Rect	content;
Rect	structure;

	SkelGetWindContentRect (w, &content);
	SkelGetWindStructureRect (w, &structure);
	return (content.top - structure.top);
}

/*
 * Calculate content or structure rectangle for a window. These are
 * core routines because they are called by other core routines,
 * e.g., SkelGetWindowDevice() and SkelGetWindTitleHeight ().
 */

# include	"TransSkel1.h"

# define	kTranslate	0x4000


/*
 * Get content rectangle and convert it to global coordinates
 */

pascal void
SkelGetWindContentRect (WindowPtr w, Rect *rp)
{
GrafPtr	oldPort;

	GetPort (&oldPort);
	SetPort (w);
	*rp = w->portRect;
	LocalToGlobal (&topLeft (*rp));
	LocalToGlobal (&botRight (*rp));
	SetPort (oldPort);
}


/*
 * Get structure rectangle.  This is already in global coordinates, but the
 * tricky part is that it isn't valid if the window is invisible.
 *
 * If window's visible, the structure region's valid, so get the bounding box.
 *
 * If the window's not visible, fling it out into space, make it visible, get
 * the structure region bounding box, make it invisible again and restore it to
 * its normal position.  Use ShowHide() for this since  it doesn't change the
 * window's hiliting or position in the stacking order.  The rectangle
 * calculated this way has to be moved back, too, since it's obtained when the
 * window is in flung position.
 *
 * I have seen similar code that also saves and restored the window's userState,
 * but Inside Macintosh (Toolbox Essentials, p. 4-70) explicitly states that
 * the userState isn't modified when you just move a window, so I don't see the
 * point.
 */

pascal void
SkelGetWindStructureRect (WindowPtr w, Rect *rp)
{
Rect	content;

	if (((WindowPeek) w)->visible)
		*rp = (**(* (WindowPeek) w).strucRgn).rgnBBox;
	else
	{
		SkelGetWindContentRect (w, &content);				/* get upper-left coords */
		MoveWindow (w, kTranslate, content.top, false);		/* fling window */
		ShowHide (w, true);
		*rp = (**(* (WindowPeek) w).strucRgn).rgnBBox;
		ShowHide (w, false);
		MoveWindow (w, content.left, content.top, false);	/* unfling window */
		OffsetRect (rp, content.left - kTranslate, 0);		/* unfling struct rect */
	}
}

/*
 * Given a rectangle, determine the following values:
 * - Which device contains more of the rectangle than any other
 * - The device rectangle (this includes the menu bar area if the device
 * is the main device)
 * - Whether or not the device is the main device
 *
 * These values are stuffed into the arguments, which are passed as
 * variable addresses.  If you're not interested in a particular value,
 * pass nil for the corresponding argument.
 *
 * The return value if true if the rectangle overlaps some device,
 * false if it lies outside all devices.  If the rectangle overlaps no
 * device, non-nil arguments are filled in with the main device, the main
 * device rect, and true, respectively.  This is useful, e.g., for callers
 * that may want to reposition a window if its content rectangle isn't
 * visible on some monitor.
 *
 * The returned device value will be nil on systems that don't have GDevices
 * (i.e.,, that don't support Color QuickDraw), even if the function result
 * is true.
 *
 * References: TN TB 30.
 */

# include	"TransSkel1.h"


pascal Boolean
SkelGetRectDevice (Rect *rp, GDHandle *rGD, Rect *devRect, Boolean *isMain)
{
GDHandle	gd, curGD;
Rect		gdRect, curRect, iSectRect;
long		maxArea, area;
Boolean		main = false;
Boolean		result;
		
	gd = (GDHandle) nil;				/* no device for rectangle known yet */

	if (!SkelQuery (skelQHasColorQD))
	{
		/*
		 * No Color QuickDraw implies only one screen, which is therefore
		 * the main device.  Test rectangle against full screen, setting
		 * result true if they intersect.
		 */
		main = true;
		gdRect = screenBits.bounds;
		result = SectRect (rp, &gdRect, &iSectRect);
	}
	else
	{
		/* determine device having maximal overlap with r */

		maxArea = 0;
		for (curGD = GetDeviceList (); curGD != (GDHandle) nil; curGD = GetNextDevice (curGD))
		{
			/* only consider active screen devices */
			if (!TestDeviceAttribute (curGD, screenDevice)
					|| !TestDeviceAttribute (curGD, screenActive))
				continue;
			curRect = (**curGD).gdRect;
			if (!SectRect (rp, &curRect, &iSectRect))
				continue;
			area = (long) (iSectRect.right - iSectRect.left)
					* (long) (iSectRect.bottom - iSectRect.top);
			if (maxArea < area)
			{
				maxArea = area;
				gd = curGD;
				gdRect = curRect;
				result = true;	/* rectangle overlaps some device */
			}
		}
		if (gd == (GDHandle) nil)	/* rectangle overlaps no device, use main */
		{
			gd = GetMainDevice ();
			gdRect = (**gd).gdRect;
			result = false;
		}
		main = (gd == GetMainDevice ());
	}

	/* fill in non-nil arguments */

	if (rGD != (GDHandle *) nil)
		*rGD = gd;
	if (devRect != (Rect *) nil)
		*devRect = gdRect;
	if (isMain != (Boolean *) nil)
		*isMain = main;

	return (result);
}

/*
 * 09 Jun 92
 * - Check whether default button is actually *active* when return/enter
 * is typed.  Sheesh.
 * 11 Jun 92
 * - Added flag argument to SkelDlogFilter() allowing caller to determine
 * whether or not standard filter should do return/enter processing.  Since
 * this is now under control of caller, the test is whether the flag is true,
 * rather than whether there's a dialog-specific filter or not.
 * 04 Nov 93
 * - Made the filter function check whether there's an event hook installed
 * and pass the event to it if so, before dealing with the event.
 * 03 Jan 94
 * - Added functions SkelDlogDefaultItem() and SkelDlogCancelItem() for
 * specifying which dialog items are the default and cancel items.  This
 * allows (i) a different item than aDefItem to be used for the OK button
 * and (ii) keyboard equivalents for the Cancel item to be handled
 * automatically by the standard filter.
 * 04 Jan 94
 * - Added function SkelDlogFilterYD() for installing filters that can be used
 * for calls like the System 7 Standard File dialogs CustomGetFile() and
 * CustomPutFile().
 * - Changed type of SkelDlogFilter() from SkelDlogFilterProcPtr to
 * ModalFilterProcPtr.  (They're equivalent, and compiler now treats them
 * that way, so there's no need for SkelDlogFilterProcPtr anymore.)
 * 07 Jan 94
 * - StandardFilter() now only checks keyDown, not autoKey; if the keypress
 * is one it would respond to, the dialog will end up being dismissed and there
 * is no opportunity for autoKey to occur.
 * 12 Jan 94
 * - Stopped using CallPascalB(); the compiler's smart enough now to figure out
 * when Pascal calling conventions need to be used with (*func)(args) syntax.
 * 13 Jan 94
 * - Formerly when a key was typed that maps to a button, the standard filter
 * checked the button to see if it was active.  If so, it flashed the button and
 * returned the item.  Otherwise it returned false.  This leads to the behavior
 * that the key ends up in the current edittext item if there is one when the
 * button is inactive and not otherwise.  Now the filter flashes the button and
 * returns true if the button is active, but turns the event into a null event
 * if the button's inactive so the key doesn't get processed further.
 * 14 Apr 94
 * - Added stuff for testing whether the pointer is in an edit field and
 * changing the pointer to an I-beam if so.  This is enabled/disabled by
 * calling SkelDlogTracksCursor().
 * - Also remembered to actually initialize the cancelItem field of the
 * FilterInfo struct when a new one is pushed on the stack...
 * 23 Apr 94
 * - Changed stack depth from 10 to 5.  No more than 2 alerts/dialogs should
 * appear at once, anyway, so even 5 is excessive.
 * - Pass mouse click events to regular event router if the click is in the
 * drag region of an underlying window and the command-key is down.  This
 * allows those windows to be dragged around in their current plane under
 * modal dialogs.  (As per Apple's Human Interface Guidelines.)
 * 26 Apr 94
 * - Send null events to SkelRouteEvent(), without claiming to have handled
 * them.  This helps keep window-specific idle procedures running if they're
 * supposed to run even when the window isn't frontmost.
 * 29 Apr 94
 * - Bug fix: TestDlogButton() wasn't checking properly to see whether or not
 * a button was disabled.  Fixed.
 * - Removed TestDlogButton() and key mapping code from StandardFilter() and
 * repackaged it at SkelDlogMapKeyToButton() because the key mapping operation
 * is one that's useful from modeless and movable modal dialog filters, too.
 * - On update or activate, instead of testing whether the window to which the
 * event applies is registered with TransSkel, just check whether it's the modal
 * being handled.  If not, pass the event to the router.  Even if the event's
 * for another dialog, the router will now handle it correctly, whether the
 * window is registered or not.
 * 03 May 94
 * - Introduced a bug in ModalFilterYD() in release 3.13 by putting an extra
 * semicolon at the end of the first "if" line.  Resulted in machine lockups.
 * Fixed.
 */

# include	"TransSkel1.h"


/*
 * Filter function stack. Used so that the filter function mechanism
 * can be reentrant (multiple simultaneous alerts and dialogs).  Maximum
 * reentrancy level is controlled by maxStack.
 *
 * The filter field is a generic pointer because functions of different
 * types are assigned to it.
 *
 * The defaultItem and cancelItem fields are positive to indicate an
 * item to which return/enter and cmd-period/escape should be mapped.
 * They are zero to indicate no key mapping.  defaultItem can be
 * negative to indicate that return/enter should be mapped to the
 * aDefItem item in the dialog/alert record.
 */

# define	maxStack	5


typedef	struct FilterInfo	FilterInfo;

struct FilterInfo
{
	ProcPtr	filter;
	short	defaultItem;
	short	cancelItem;
	Boolean	trackCursor;
};


static short	top = -1;
static FilterInfo filterStack[maxStack];


/*
 * Standard filter function.  The regular alert- or dialog-specific filter is
 * piggybacked onto this.  The standard filter intercepts events that would
 * be ignored otherwise (currently this includes OS events, command clicks
 * in drag region of underlying windows, and update and activate/deactivate
 * events for windows other than the given dialog) and shoves them through
 * the normal TransSkel event dispatch mechanism.
 *
 * The standard filter also performs key-to-button mapping, returning the
 * default button when return/enter is typed, and/or the cancel button
 * when cmd-period/escape is typed.
 *
 * Returns false even for an event that's handled, if it doesn't result in
 * an item hit, but map the event to a null event so the Dialog Manager doesn't
 * try to do anything further with it.
 */

static pascal Boolean 
StandardFilter (DialogPtr d, EventRecord *e, short *item)
{
FilterInfo	*info;
short	what = e->what;
SkelEventHookProcPtr	p;
GrafPtr	port;

	if ((p = SkelGetEventHook ()) != (SkelEventHookProcPtr) nil)
	{
		if ((*p) (e))	/* call hook, and return if hook handles event */
			return (true);
	}

	info = &filterStack[top];

	/*
	 * If the event is a null event and the filter should make the cursor an
	 * I-beam when the pointer's in an edit text field, check that.
	 */

	if (what == nullEvent)
	{
		if (info->trackCursor)
			SkelSetDlogCursor (d);
		SkelRouteEvent (e);
		return (false);
	}

	if (what == osEvt)				/* MultiFinder event */
	{
		SkelRouteEvent (e);
		e->what = nullEvent;
		return (false);
	}

	/*
	 * Command clicks in the drag region of underlying windows are processed,
	 * allowing those windows to be dragged  around in their current plane under
	 * the dialog.  This doesn't test whether the port is the same as the dialog
	 * because modals don't have a drag region.
	 */
	if (what == mouseDown && (e->modifiers & cmdKey)
			&& FindWindow (e->where, &port) == inDrag)
	{
		SkelRouteEvent (e);
		e->what = nullEvent;
		return (false);
	}

	if (what == updateEvt || what == activateEvt)
	{
		if (d != (DialogPtr) e->message)
		{
			SkelRouteEvent (e);
			e->what = nullEvent;
			return (false);
		}
	}

	/*
	 * If event is a key-press, handle key-to-button mapping if necessay.
	 */

	if (what == keyDown)
	{
		if (SkelDlogMapKeyToButton (d, e, item, info->defaultItem, info->cancelItem))
			return (true);
	}
	return (false);
}


/*
 * Run ModalDialog()-style filter.
 *
 * If dialog-specific filter doesn't handle the event, pass it to
 * StandardFilter().
 */
 
static pascal Boolean 
ModalFilter (DialogPtr d, EventRecord *e, short *item)
{
FilterInfo	*info;

	info = &filterStack[top];
	if (info->filter != nil
		&& (*(ModalFilterProcPtr) info->filter) (d, e, item))
		return (true);
	if (StandardFilter (d, e, item))
		return (true);
	return (false);
}


/*
 * Run ModalDialog()-style filter that takes a data argument.
 *
 * If dialog-specific filter doesn't handle the event, pass it to
 * StandardFilter().
 */
 
static pascal Boolean 
ModalFilterYD (DialogPtr d, EventRecord *e, short *item, void *data)
{
FilterInfo	*info;

	info = &filterStack[top];
	if (info->filter != nil
		&& (*(ModalFilterYDProcPtr) info->filter) (d, e, item, data))
		return (true);
	if (StandardFilter (d, e, item))
		return (true);
	return (false);
}


/*
 * Install a ModalDialog()-style filter, or a filter that takes a data
 * argument.
 *
 * In each case, remember the filter specified by the caller, so it can be
 * invoked later.  Return a pointer to a function that invokes the filter
 * specified by the caller, and the standard filter if necessary.  The caller
 * passes the pointer returned to an alert or dialog call that takes a filter
 * function argument.
 *
 * doReturn is true if the standard filter should try to handle return/enter
 * key as a synonym for clicking the default button if that button is active.
 */


static void
InstallNewFilter (ProcPtr filter, Boolean doReturn)
{
FilterInfo	*info;

	info = &filterStack[++top];
	info->filter = filter;
	info->defaultItem = (doReturn ? -1 : 0);
	info->cancelItem = 0;
	info->trackCursor = false;
}


static ModalFilterUPP modalFilterUPP = NULL;

pascal ModalFilterUPP
SkelDlogFilter (ModalFilterProcPtr filter, Boolean doReturn)
{
	InstallNewFilter ((ProcPtr) filter, doReturn);
	if (! modalFilterUPP)
		modalFilterUPP = NewModalFilterProc((ProcPtr) ModalFilter);
	return (modalFilterUPP);
}


pascal ModalFilterYDProcPtr
SkelDlogFilterYD (ModalFilterYDProcPtr filter, Boolean doReturn)
{
	InstallNewFilter ((ProcPtr) filter, doReturn);
	return (ModalFilterYD);
}


/*
 * Remove the last filter function (which reinstalls the previous one
 * as a side effect).
 */

pascal void
SkelRmveDlogFilter (void)
{
	--top;
}


/*
 * Set default or cancel item for the current filter
 */

pascal void
SkelDlogDefaultItem (short item)
{
	if (top >= 0)
		filterStack[top].defaultItem = item;
}


pascal void
SkelDlogCancelItem (short item)
{
	if (top >= 0)
		filterStack[top].cancelItem = item;
}


pascal void
SkelDlogTracksCursor (Boolean track)
{
	if (top >= 0)
		filterStack[top].trackCursor = track;
}
/*
 * SkelDoEvents (mask) - process all pending events of types indicated in mask
 * SkelDoUpdates () - process all pending update events
 *
 * These routines may be called any time subsequent to the call of SkelInit().
 */

# include	"TransSkel1.h"


static short	sdeMask;


/*
 * Make sure any events of proper type are processed before
 * proceeding.  I.e., wait until there are no more events of the
 * type we're waiting for, then terminate SkelDoEvents().
 */

static pascal void
CheckEvents (void)
{
EventRecord	event;

	if (!EventAvail (sdeMask, &event))
		SkelStopEventLoop ();
}


/*
 * Process all events of type(s) given in mask.  It is possible to call this
 * recursively.
 * Operation:
 * - Save current SkelDoEvents() mask, current TransSkel event mask, and
 * current background procedure.
 * - Install the new mask into TransSkel and save a copy in a local variable.
 * Install a new background procedure that checks whether any events of the
 * desired type(s) are available or not.
 * - Call SkelMain() to initiate an event loop.  The background task calls
 * SkelWhoa() to terminate SkelMain() when there are no more events of
 * interest available.
 * - Restore the previous background procedure and TransSkel mask, and
 * previous SkelDoEvents() mask.  The latter is necessary in case this is
 * a recursive call.
 */

pascal void
SkelDoEvents (short mask)			/* can be called recursively */
{
short	oldSdeMask;
short	oldTSMask;
SkelIdleProcPtr	oldIdle;

	oldIdle = SkelGetIdle ();			/* get current idle proc */
	oldTSMask = SkelGetEventMask ();	/* and event mask */
	oldSdeMask = sdeMask;				/* and SkelDoEvents() processing types */

	SkelSetIdle (CheckEvents);			/* install new idle & mask */
	SkelSetEventMask (mask);
	sdeMask = mask;						/* <- so CheckEvents can find mask */

	SkelEventLoop ();					/* handle given event types only */

	SkelSetIdle (oldIdle);				/* restore stuff that was changed */
	SkelSetEventMask (oldTSMask);
	sdeMask = oldSdeMask;
}


pascal void
SkelDoUpdates (void)
{
	SkelDoEvents (updateMask);
}

/*
 * SkelDlogMapKeyToButton ()
 *
 * This routine looks at an event to see whether or not it's a key event
 * that should be mapped onto an item hit in a dialog's default or cancel
 * button.
 *
 * defaultItem is 0 if no return/enter mapping should be done, > 0 to
 * explicitly specify the item that return/enter map to, < 0 to map
 * return/enter to the item specified as the default in the dialog record.
 *
 * cancelItem is 0 if no escape/cmd-period mapping should be done, > 0 to
 * explicitly specify the item that escape/cmd-period map to.
 *
 * If the key maps to a button item, the button is flashed for visual
 * feedback if it is hilited normally (not dimmed).  If the button is enabled,
 * the item parameter is set to the item number and the function returns true.
 * If the key doesn't map to an item or the button is disabled, the function
 * returns false.
 *
 * If the key maps to a button, but the button isn't hilited properly or is
 * disabled, the event is mapped to a null event so that nothing else is done
 * with it.  This is done based on the assumption that if the caller is trying
 * to do key mapping, it doesn't want the mapped keys to get into dialog edit
 * text items.
 */

# include	"TransSkel1.h"


# define	returnKey	13
# define	enterKey	3
# define	escapeKey	27


# define	normalHilite	0
# define	dimHilite		255


/*
 * Function that checks whether a dialog item is a button.  If so,
 * flash it.  In addition, if it's active, return true to indicate an
 * item hit.  Otherwise return false.
 */

static Boolean
TestDlogButton (DialogPtr d, short item)
{
ControlHandle	ctrl;
Handle	itemHandle;
short	itemType;
Rect	itemRect;

	GetDialogItem (d, item, &itemType, &itemHandle, &itemRect);
	if ((itemType & (ctrlItem + btnCtrl)) == (ctrlItem + btnCtrl))
	{
		ctrl = (ControlHandle) itemHandle;
		if ((**ctrl).contrlHilite == normalHilite)
		{
			SkelFlashButton (ctrl);
			if ((itemType & itemDisable) == 0)
				return (true);
		}
	}
	return (false);
}


pascal Boolean
SkelDlogMapKeyToButton (DialogPtr dlog, EventRecord *evt, short *item,
								short defaultItem, short cancelItem)
{
char	c;
short	i;

	c = evt->message & charCodeMask;
	if (c == returnKey || c == enterKey)
	{
		i = defaultItem;
		if (i != 0)
		{
			if (i < 0)
				i = ((DialogPeek) dlog)->aDefItem;
			if (TestDlogButton (dlog, i))
			{
				*item = i;
				return (true);
			}
			evt->what = nullEvent;
		}
	}
	if (c == escapeKey || SkelCmdPeriod (evt))
	{
		*item = cancelItem;
		if (*item > 0)
		{
			if (TestDlogButton (dlog, *item))
				return (true);
			evt->what = nullEvent;
		}
	}
	return (false);
}

/*
 * Set cursor to I-beam if it's in an edit text item of the given dialog.
 *
 * Does nothing if the FindDItem() trap doesn't exist (which it doesn't in
 * versions of system software prior to 3.2).
 */

# include	<Traps.h>

# include	"TransSkel1.h"



pascal void
SkelSetDlogCursor (DialogPtr d)
{
GrafPtr	savePort;
Point	pt;
short	i;

	if (SkelTrapAvailable (_FindDItem))
	{
		/*
		 * Get cursor local coordinates.  One assumes the port will be set
		 * to the dialog, but you never know...
		 */
		GetPort (&savePort);
		SetPort (d);
		GetMouse (&pt);
		SetPort (savePort);
		i = FindDialogItem (d, pt) + 1;
		if (i > 0 && (SkelGetDlogType (d, i) & editText) == 0)
				i = 0;
		if (i > 0)
			SetCursor (*GetCursor (iBeamCursor));
		else
			InitCursor ();
	}
}
/*
 * Get type of dialog item.
 */


# include	"TransSkel1.h"


pascal short
SkelGetDlogType (DialogPtr d, short item)
{
short	type;
Handle	h;
Rect	r;

	GetDialogItem (d, item, &type, &h, &r);
	return (type);
}

/*
 * Flash a push button to simulate a click in it.
 */

# include	"TransSkel1.h"


# define	hiliteClicks	8

pascal void
SkelFlashButton (ControlHandle ctrl)
{
short	oldHilite;
unsigned long	dummy;

	oldHilite = (**ctrl).contrlHilite;
	HiliteControl (ctrl, kControlButtonPart);		/* flash it */
	Delay ((long) hiliteClicks, &dummy);
	HiliteControl (ctrl, oldHilite);
}

/*
 * Position a window according to the given position type, using the
 * given positioning ratios.
 *
 * Position types:
 *	skelPositionNone -- leave window in current position
 *	skelPositionOnMainDevice -- position on main device
 *	skelPositionOnParentWindow -- position on frontmost visible window
 *	skelPositionOnParentScreen -- position on screen of frontmost visible window
 *
 * If there's no frontmost window, positions that use it default to
 * skelPositionOnMainDevice.
 *
 * For best results, window should not be visible.  Otherwise you'll end
 * up moving it while it's visible.
 *
 * 08 Feb 94
 * - Position window using structure rather than content rectangle.
 */

# include	"TransSkel1.h"


pascal void
SkelPositionWindow (WindowPtr w, short positionType,
							Fixed hRatio, Fixed vRatio)
{
Rect	contentRect, structRect, refRect;
short	hDiff, vDiff;

	if (positionType == skelPositionNone)	/* leave window as is */
		return;

	/* get rect to use as reference against which to position window rect */
	SkelGetReferenceRect (&refRect, positionType);

	/*
	 * Use structure rect as the rect to be positioned, but when moving window,
	 * offset by difference between upper left of structure and content rects,
	 * since MoveWindow() positions the content rect to the given position.
	 */
	SkelGetWindContentRect (w, &contentRect);
	SkelGetWindStructureRect (w, &structRect);
	hDiff = contentRect.left - structRect.left;
	vDiff = contentRect.top - structRect.top;
	SkelPositionRect (&refRect, &structRect, hRatio, vRatio);
	MoveWindow (w,
				structRect.left + hDiff,
				structRect.top + vDiff,
				false);
}

/*
 * Get bounding rectangle of a dialog item.
 */

# include	"TransSkel1.h"


pascal void
SkelGetDlogRect (DialogPtr d, short item, Rect *r)
{
short	type;
Handle	h;

	GetDialogItem (d, item, &type, &h, r);
}
/*
 * Set bounding rectangle of a dialog item.
 */

# include	"TransSkel1.h"


pascal void
SkelSetDlogRect (DialogPtr d, short item, Rect *r)
{
short	type;
Handle	h;
Rect	rOld;

	GetDialogItem (d, item, &type, &h, &rOld);
	SetDialogItem (d, item, type, h, r);
}
/*
 * Return a handle to the control associated with a dialog item.
 */

# include	"TransSkel1.h"


pascal ControlHandle
SkelGetDlogCtl (DialogPtr d, short item)
{
short	type;
Handle	h;
Rect	r;

	GetDialogItem (d, item, &type, &h, &r);
	return ((ControlHandle) h);
}
/*
 * Associate a button-outlining function with the given item, which should
 * be a user item.  The item outlined will be the default item, and should
 * be a push button.  The user item bounding rectangle is positioned and
 * sized to surround the default item.
 *
 * There's a subtle point here -- the outline drawing proc is called when the
 * user item rect is becomes invalid, but the drawing proc bases its calculations
 * on the rect for the default button item.  This works because the rect it
 * calculates based on the button rect is identical to that of the user item.
 *
 * If you change the button's hiliting state, you should make sure the outline
 * is redrawn by invalidating its bounding rectangle.  You can avoid unnecessary
 * redrawing by using SkelSetDlogCtlHilite() like so:
 *
 *	if (SkelSetDlogCtlHilite (dlog, buttonItem, newHilite)
 *	{
 *		SkelGetDlogRect (dlog, outlineItem, &r);
 *		InvalRect (&r);
 *	}
 */

# include	"TransSkel1.h"


/*
 * Draw heavy outline around default dialog button.
 */

static pascal void
DrawDlogButtonOutline (DialogPtr d, short item)
{
	SkelDrawButtonOutline (SkelGetDlogCtl (d, ((DialogPeek) d)->aDefItem));
}


static ControlActionUPP DrawDlogButtonOutlineUPP = NULL;

pascal void
SkelSetDlogButtonOutliner (DialogPtr d, short item)
{
short	type;
Handle	h;
Rect	r, rJunk;
short	defItem;

	/* find default item bounding rectangle */
	defItem = ((DialogPeek) d)->aDefItem;
	GetDialogItem (d, defItem, &type, &h, &r);

	/* get user item, position rectangle, and install draw proc using it */
	GetDialogItem (d, item, &type, &h, &rJunk);
	InsetRect (&r, -4, -4);
    if (! DrawDlogButtonOutlineUPP)
      DrawDlogButtonOutlineUPP = NewControlActionProc((ProcPtr) DrawDlogButtonOutline);
	SetDialogItem (d, item, type, (Handle) DrawDlogButtonOutlineUPP, &r);
}
/*
 * Draw a heavy outline around a push button.  Draw the outline in black
 * unless the button is inactive, in which case draw it gray.  When drawing
 * gray outlines, use a true RGB gray if the monitor supports it and the button
 * belongs to a color GrafPort, and pattern gray otherwise.  This matches how
 * the Control Manager draws titles in dimmed buttons.
 *
 * It's absolutely astonishing how much work is necessary to draw a gray
 * outline. This is evidenced not only by all the code below, but also by
 * the efforted expended in SkelGetRectDevice() to make sure the gray is
 * computed using the characteristics of the proper device.  It's a wonder
 * it doesn't take 10 minutes to draw dimmed outlines.
 */

# include	<Palettes.h>		/* for GetGray() */

# include	"TransSkel1.h"


# define	normalHilite	0
# define	dimHilite		255


pascal void
SkelDrawButtonOutline (ControlHandle ctrl)
{
GrafPtr		oldPort;
PenState	penState;
Rect		r, r2;
short		curvature;
Boolean		haveRGBGray;
RGBColor	fgColor, newFgColor, bgColor;
GDHandle	gDev;

	GetPort (&oldPort);
	SetPort ((**ctrl).contrlOwner);
	GetPenState (&penState);
	PenNormal ();
	r = (**ctrl).contrlRect;
	InsetRect (&r, -4, -4);
	curvature = (r.bottom - r.top) / 2 + 2;
	PenSize (3, 3);
	if ((**ctrl).contrlHilite == normalHilite)	/* button active, draw black */
		FrameRoundRect (&r, curvature, curvature);
	else										/* button inactive, draw gray */
	{
		/*
		 * Try to get RGB gray value appropriate for the device on which
		 * the button is displayed if have color GrafPort.
		 */
		haveRGBGray = false;
		if (((CGrafPtr) (**ctrl).contrlOwner)->portVersion & 0xc000)
		{
			/* convert rect to global coordinates */
			r2 = r;
			LocalToGlobal (&topLeft (r2));
			LocalToGlobal (&botRight (r2));
			(void) SkelGetRectDevice (&r2, &gDev, (Rect *) nil, (Boolean *) nil);
			/* test unnecessary unless for some reason rect isn't on any device! */
			if (gDev != (GDHandle) nil)
			{
				GetBackColor (&bgColor);
				GetForeColor (&fgColor);
				newFgColor = fgColor;
				haveRGBGray = GetGray (gDev, &bgColor, &newFgColor);
			}
		}
		/*
		 * Draw using colored gray if possible, else using pattern gray
		 */

		if (haveRGBGray)
			RGBForeColor (&newFgColor);
		else
			PenPat (&gray);
		FrameRoundRect (&r, curvature, curvature);
		if (haveRGBGray)
			RGBForeColor (&fgColor);
	}
	SetPenState (&penState);
	SetPort (oldPort);
}
/*
 * SkelPositionRect()
 *
 * Position a rectangle relative to reference rectangle so that space above/below
 * and to left/right of rectangle that's moved maintains ratio given by hRatio and
 * vRatio.  This is useful for establishing initial window positions or positioning
 * alerts/dialogs.
 *
 * "inside" isn't quite the right word, since the moved rectangle need not
 * actually be smaller than the reference rectangle.
 *
 * Examples:
 *
 * Center a rectangle inside the reference rectangle:
 *	SkelPositionRect (&ref, &r, FixRatio (1, 2), FixRatio (1, 2));
 *
 * Leave 1/3 of vertical space above positioned rectangle, 2/3 of space below:
 *	SkelPositionRect (&ref, &r, FixRatio (1, 2), FixRatio (1, 3));
 *
 * Algorithm may not work correctly if rects have negative coordinates.
 */

# include	<FixMath.h>

# include	"TransSkel1.h"


pascal void
SkelPositionRect (Rect *refRect, Rect *r, Fixed hRatio, Fixed vRatio)
{
short	hOff, vOff;

	/* align topleft of rects (simplifies calculations) */

	OffsetRect (r, refRect->left - r->left, refRect->top - r->top);

	/* calculate offsets in each direction for given ratios */

	hOff = Fix2Long (FixMul (Long2Fix ((long) (refRect->right - r->right)), hRatio));
	vOff = Fix2Long (FixMul (Long2Fix ((long) (refRect->bottom - r->bottom)), vRatio));

	/* move rect by appropriate amount */

	OffsetRect (r, hOff, vOff);
}
/*
 * Get a reference rectangle for window positioning.
 *
 * Reference rect for position types:
 *	skelPositionNone -- no reference rect
 *	skelPositionOnMainDevice -- usable area on main device
 *	skelPositionOnParentWindow -- content rect of frontmost visible window
 *	skelPositionOnParentDevice -- usable area on screen of frontmost visible window
 *
 * If there's no frontmost window, positions that use it default to
 * skelPositionOnMainDevice.
 *
 * Result for position skelPositionNone is same as for skelPositionOnMainDevice
 * just so that result isn't undefined, but caller would be better off to handle
 * skelPositionNone case itself.
 *
 * 18 Feb 94
 * - Return structure rect rather than content rect of parent window when
 * position type is skelPositionOnParentWindow.
 */

# include	"TransSkel1.h"


pascal void
SkelGetReferenceRect (Rect *r, short positionType)
{
WindowPtr	frontWind = FrontWindow ();

	/*
	 * Assume default positioning will be with reference to main device.
	 * This will also be used as the fallback for positionings that use
	 * FrontWindow() if FrontWindow() is nil.
	 */

	SkelGetMainDeviceRect (r);

	if (positionType == skelPositionNone)	/* leave window as is */
		return;

	/*
	 * Find frontmost visible window
	 */
	frontWind = FrontWindow ();
	while (frontWind != (WindowPtr) nil && !((WindowPeek) frontWind)->visible)
		frontWind = (WindowPtr) ((WindowPeek) frontWind)->nextWindow;

	switch (positionType)
	{
	case skelPositionOnParentWindow:
		if (frontWind != (WindowPtr) nil)
			SkelGetWindStructureRect (frontWind, r);
		break;
	case skelPositionOnParentDevice:
		if (frontWind != (WindowPtr) nil)
			(void) SkelGetWindowDevice (frontWind, (GDHandle *) nil, r);
		break;
	}
}
/*
 * Get usable area on main device.  Does not include menu bar; if that's
 * of interest, do this:
 *
 *		SkelGetMainDeviceRect (&r);
 *		r.top -= SkelQuery (skelQMBarHeight);
 */

# include	"TransSkel1.h"


pascal void
SkelGetMainDeviceRect (Rect *r)
{
	if (!SkelQuery (skelQHasColorQD))	/* no devices, use screenBits */
		*r = screenBits.bounds;
	else
		*r = (**GetMainDevice ()).gdRect;
	r->top += SkelQuery (skelQMBarHeight);
}
