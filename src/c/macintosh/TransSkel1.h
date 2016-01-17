/*
 * prototypes for TransSkel public routines
 *
 * Written for THINK C 6.0.1.
 *
 * Assumes that compiler understands nested prototypes.
 *
 * You must have THINK C Language Extensions turned on in the
 * Edit/Options.../Language Settings dialog (so that the "pascal" keyword
 * is recognized, for one thing).
 *
 * for:			TransSkel 3.17
 * last edit:	01 Jun 94
 */

# ifndef	__TRANSSKEL_H__

# define	__TRANSSKEL_H__

#ifdef __MWERKS__
#define arrow qd.arrow
#define screenBits qd.screenBits
#define thePort qd.thePort
#define gray qd.gray
#endif /* __MWERKS__ */

#ifdef applec
# include       <Dialogs.h>
# include       <Events.h>
# include       <OSEvents.h>
# include       <Menus.h>
# include       <Memory.h>
# include       <DiskInit.h>
# include       <ToolUtils.h>
# include       <Fonts.h>
# include       <Desk.h>
# include       <Packages.h>
# include       <Resources.h>
# include       <Script.h>
# include       <Traps.h>
# include       <Scrap.h>
# include		<StandardFile.h>
# ifndef _Gestalt
#  define _Gestalt 0xA1AD /**** needed with 3.3.3? */
# endif
# define        nil                     0L
# define        arrow                   qd.arrow
# define		thePort					qd.thePort
# define		screenBits				qd.screenBits
# define		gray					qd.gray
# define        topLeft(r)              (((Point *) &(r))[0])
# define        botRight(r)             (((Point *) &(r))[1])
#endif /* applec */

# define	skelMajorRelease	3
# define	skelMinorRelease	17


/* window property numbers */

# define	skelWPropAll			0		/* pseudo-property */
# define	skelWPropModeless		1		/* modeless dialog */
# define	skelWPropModal			2		/* modal dialog */
# define	skelWPropTool			3		/* tool window */
# define	skelWPropMovableModal	4		/* movable modal dialog */
# define	skelWPropHelp			5		/* help window */
# define	skelWPropText			6		/* text window */
# define	skelWPropDisplayWind	7		/* TransDisplay window */
# define	skelWPropEditWind		8		/* TransEdit window */
# define	skelWPropApplBase		256		/* general-use prop num base */


/* SkelQuery() query selectors */

# define	skelQVersion		1			/* TransSkel version */
# define	skelQSysVersion		2			/* System software version */
# define	skelQHasWNE			3			/* whether has WaitNextEvent() */
# define	skelQMBarHeight		4			/* menu bar height */
# define	skelQHas64KROM		5			/* whether has 64K ROM */
# define	skelQHasColorQD		6			/* whether has Color QuickDraw */
# define	skelQInForeground	7			/* whether in foreground */
# define	skelQHasGestalt		8			/* whether has Gestalt() */
# define	skelQHasAppleEvents	9			/* whether has Apple Events */
# define	skelQQDVersion		10			/* QuickDraw version */
# define	skelQGrayRgn		11			/* Desktop region */


/* window property types */

typedef struct SkelWindProperty	SkelWindProperty, **SkelWindPropHandle;

struct SkelWindProperty
{
	short				skelWPropType;
	long				skelWPropData;
	SkelWindPropHandle	skelWPropNext;
};


/*
 * Initialization parameters
 *
 * SkelResumeProcPtr is the same as ResumeProcPtr, but the latter is
 * disappearing from Apple's header files because it's not supposed
 * to be used under System 7 anymore.
 */

typedef struct SkelInitParams SkelInitParams, *SkelInitParamsPtr;
typedef pascal void (*SkelResumeProcPtr)(void);

struct SkelInitParams
{
	short				skelMoreMasters;
	GrowZoneProcPtr		skelGzProc;
	SkelResumeProcPtr	skelResumeProc;
	Size				skelStackAdjust;
};


/*
 * typedefs for pointers to various sorts of functions used by TransSkel
 * routines.
 */

typedef pascal void (*SkelIdleProcPtr) (void);
typedef	pascal Boolean (*SkelEventHookProcPtr) (EventRecord *);
typedef pascal void (*SkelSuspendResumeProcPtr) (Boolean inForeground);
typedef pascal void (*SkelClipCvtProcPtr) (Boolean inForeground);
typedef pascal void (*SkelAEHandlerProcPtr) (EventRecord *);
typedef	pascal void (*SkelDlogItemProcPtr) (DialogPtr d, short item);


/* ------------- */
/* Core routines */
/* ------------- */

/* initialization/termination routines */

pascal void SkelGetInitParams (SkelInitParamsPtr initParams);
pascal void SkelInit (SkelInitParamsPtr initParams);
pascal void SkelCleanup (void);

/* event-loop-related routines */

pascal void SkelEventLoop (void);
pascal void SkelStopEventLoop (void);
pascal void SkelRouteEvent (EventRecord *evt);
pascal void SkelActivate (WindowPtr w, Boolean active);
pascal void SkelClose (WindowPtr w);
pascal EventRecord *SkelGetCurrentEvent (void);
pascal short SkelGetModifiers (void);
pascal void SkelSetEventMask (short mask);
pascal short SkelGetEventMask (void);
pascal void SkelSetDlogMask (short mask);
pascal short SkelGetDlogMask (void);
pascal void SkelSetIdle (SkelIdleProcPtr p);
pascal SkelIdleProcPtr SkelGetIdle (void);
pascal void SkelSetEventHook (SkelEventHookProcPtr p);
pascal SkelEventHookProcPtr SkelGetEventHook (void);
pascal void SkelSetSuspendResume (SkelSuspendResumeProcPtr p);
pascal SkelSuspendResumeProcPtr SkelGetSuspendResume (void);
pascal void SkelSetClipCvt (SkelClipCvtProcPtr p);
pascal SkelClipCvtProcPtr SkelGetClipCvt (void);
pascal void SkelSetWaitTimes (long fgTime, long bgTime);
pascal void SkelGetWaitTimes (long *pFgTime, long *pBgTime);

pascal void SkelSetAEHandler (SkelAEHandlerProcPtr p);
pascal SkelAEHandlerProcPtr SkelGetAEHandler (void);

pascal void SkelTriggerUpdate (WindowPtr w); /* added to force resize -- L. Tierney */


/* window handling routines */

typedef	pascal void (*SkelWindMouseProcPtr) (Point where, long when, short modifiers);
/*
 * Key handler needs special treatment because for prototyped functions
 * (which TransSkel uses), THINK C passes character arguments in the *high*
 * byte of a two-byte stack value.  To make sure the values are passed in the
 * low byte from either C or Pascal key handlers, the first two arguments are
 * passed in shorts.  The Pascal key procedure should
 * look like this:
 *     procedure Key (c: char; code: Integer; modifiers: Integer);
 */
typedef	pascal void (*SkelWindKeyProcPtr) (short c, short code, short modifiers);
typedef	pascal void (*SkelWindUpdateProcPtr) (Boolean resized);
typedef	pascal void (*SkelWindActivateProcPtr) (Boolean active);
typedef	pascal void (*SkelWindCloseProcPtr) (void);
typedef	pascal void (*SkelWindClobberProcPtr) (void);
typedef	pascal void (*SkelWindIdleProcPtr) (void);

typedef	pascal void (*SkelWindSelectProcPtr) (DialogPtr dlog, short item);

typedef	pascal void (*SkelWindZoomProcPtr) (WindowPtr w, short zoomDir);

pascal Boolean SkelWindow (WindowPtr w,
					SkelWindMouseProcPtr doMouse,
					SkelWindKeyProcPtr doKey,
					SkelWindUpdateProcPtr doUpdate,
					SkelWindActivateProcPtr doActivate,
					SkelWindCloseProcPtr doClose,
					SkelWindClobberProcPtr doClobber,
					SkelWindIdleProcPtr doIdle,
					Boolean idleFrontOnly);
pascal Boolean SkelDialog (DialogPtr dlog,
					ModalFilterProcPtr doFilter,
					SkelWindSelectProcPtr doSelect,
					SkelWindCloseProcPtr doClose,
					SkelWindClobberProcPtr doClobber);
pascal void SkelRmveWind (WindowPtr w);
pascal void SkelRmveDlog (DialogPtr dlog);
pascal Boolean SkelWindowRegistered (WindowPtr w);
pascal Boolean SkelIsDlog (WindowPtr w);
pascal Boolean SkelIsMMDlog (WindowPtr w);
pascal void SkelSetGrowBounds (WindowPtr w,
							short hLo, short vLo,
							short hHi, short vHi);
pascal void SkelSetZoom (WindowPtr w, SkelWindZoomProcPtr pZoom);
pascal SkelWindZoomProcPtr SkelGetZoom (WindowPtr w);
pascal Boolean SkelGetRectDevice (Rect *rp, GDHandle *gd, Rect *devRect, Boolean *isMain);
pascal Boolean SkelGetWindowDevice (WindowPtr w, GDHandle *gd, Rect *devRect);
pascal void SkelGetWindContentRect (WindowPtr w, Rect *rp);
pascal void SkelGetWindStructureRect (WindowPtr w, Rect *rp);
pascal short SkelGetWindTitleHeight (WindowPtr w);

pascal Boolean SkelAddWindProp (WindowPtr w, short propType, long propData);
pascal void SkelRmveWindProp (WindowPtr w, short propType);
pascal SkelWindPropHandle SkelGetWindProp (WindowPtr w, short propType);
pascal long SkelGetWindPropData (WindowPtr w, short propType);


/* menu handling routines */

/**** Added menu argument -- L. Tierney */
typedef	pascal void (*SkelMenuSelectProcPtr) (short item, short menu);
typedef	pascal void (*SkelMenuClobberProcPtr) (MenuHandle m);
typedef pascal void (*SkelMenuHookProcPtr) (void);

pascal Boolean SkelMenu (MenuHandle m,
					SkelMenuSelectProcPtr doSelect,
					SkelMenuClobberProcPtr doClobber,
					Boolean subMenu,
					Boolean drawBar);
pascal void SkelRmveMenu (MenuHandle m);

pascal void SkelSetMenuHook (SkelMenuHookProcPtr p);
pascal SkelMenuHookProcPtr SkelGetMenuHook (void);


/* environment information routines */

pascal long SkelQuery (short selector);
pascal Boolean SkelTrapAvailable (short theTrap);


/* ------------------ */
/* Auxiliary routines */
/* ------------------ */

# define	skelAppleMenuID		128

pascal void SkelApple (StringPtr items, SkelMenuSelectProcPtr pSelect);

pascal void SkelDoEvents (short mask);
pascal void SkelDoUpdates (void);

pascal ModalFilterUPP SkelDlogFilter (ModalFilterProcPtr filter, Boolean doReturn);
pascal ModalFilterYDProcPtr SkelDlogFilterYD (ModalFilterYDProcPtr filter, Boolean doReturn);
pascal void SkelRmveDlogFilter (void);
pascal void SkelDlogDefaultItem (short item);
pascal void SkelDlogCancelItem (short item);
pascal void SkelDlogTracksCursor (Boolean track);

pascal Boolean SkelCmdPeriod (EventRecord *evt);


/* -------------------- */
/* Convenience routines */
/* -------------------- */

/* positioning types for SkelGetReferenceRect()/SkelPositionWindow() */

# define	skelPositionNone			0	/* leave as is */
# define	skelPositionOnMainDevice	1	/* position on main device */
# define	skelPositionOnParentWindow	2	/* position on FrontWindow() */
# define	skelPositionOnParentDevice	3	/* position on FrontWindow()'s device */

pascal void SkelGetMainDeviceRect (Rect *r);
pascal void SkelPositionRect (Rect *refRect, Rect *r, Fixed hRatio, Fixed vRatio);
pascal void SkelGetReferenceRect (Rect *r, short positionType);
pascal void SkelPositionWindow (WindowPtr w, short positionType, Fixed hRatio, Fixed vRatio);
pascal Boolean SkelTestRectVisible (Rect *r);

/* alert presentation routines */

pascal short SkelAlert (short alrtResNum, ModalFilterProcPtr filter, short positionType);
pascal void SkelSetAlertPosRatios (Fixed hRatio, Fixed vRatio);
pascal void SkelGetAlertPosRatios (Fixed *hRatio, Fixed *vRatio);

/* control manipulation routines */

pascal Boolean SkelHiliteControl (ControlHandle ctrl, short hilite);
pascal void SkelDrawButtonOutline (ControlHandle ctrl);
pascal void SkelEraseButtonOutline (ControlHandle ctrl);
pascal void SkelFlashButton (ControlHandle ctrl);
pascal short SkelToggleCtlValue (ControlHandle ctrl);

/* dialog item manipulation routines */

pascal ControlHandle SkelGetDlogCtl (DialogPtr d, short item);
pascal Boolean SkelSetDlogCtlHilite (DialogPtr d, short item, short hilite);
pascal short SkelGetDlogCtlHilite (DialogPtr d, short item);
pascal void SkelSetDlogCtlValue (DialogPtr d, short item, short value);
pascal short SkelGetDlogCtlValue (DialogPtr d, short item);
pascal short SkelToggleDlogCtlValue (DialogPtr d, short item);
pascal void SkelSetDlogCtlRefCon (DialogPtr d, short item, long value);
pascal long SkelGetDlogCtlRefCon (DialogPtr d, short item);
pascal void SkelSetDlogStr (DialogPtr d, short item, StringPtr str);
pascal void SkelGetDlogStr (DialogPtr d, short item, StringPtr str);
pascal void SkelSetDlogRect (DialogPtr d, short item, Rect *r);
pascal void SkelGetDlogRect (DialogPtr d, short item, Rect *r);
pascal void SkelSetDlogProc (DialogPtr d, short item, SkelDlogItemProcPtr p);
pascal SkelDlogItemProcPtr SkelGetDlogProc (DialogPtr d, short item);
pascal void SkelSetDlogType (DialogPtr d, short item, short type);
pascal short SkelGetDlogType (DialogPtr d, short item);
pascal void SkelSetDlogRadioButtonSet (DialogPtr dlog, short first, short last, short choice);
pascal void SkelSetDlogButtonOutliner (DialogPtr d, short item);
pascal void SkelSetDlogCursor (DialogPtr d);
pascal Boolean SkelDlogMapKeyToButton (DialogPtr d, EventRecord *evt, short *item,
										short defaultItem, short cancelItem);

pascal void SkelPause (long ticks);

# endif /* __TRANSSKEL_H__ */
