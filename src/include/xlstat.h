#ifndef XLSTAT_H
#define XLSTAT_H

/* xlstat.h - External declarations and defines for XLISP-STAT.        */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "osdefs.h"
#include "iview.h"

typedef struct window_data {
  char *object;
  int idleOn, frontOnly;
  int mouse_x, mouse_y;
} *WindowData;

#define get_window_data(w)    ((WindowData *) GetWRefCon(w))
#define set_window_data(w, d) SetWRefCon(w, (long) d)

#define integer int
#define doublereal double

#ifndef ANSI
extern char *calloc(), *malloc(), *realloc();
#endif /* ANSI */

#ifdef MSDOS
#ifdef _Windows
#define CPTR HANDLE
#else
typedef long CPTR;
#endif
#else
typedef char *CPTR;
#endif /* MSDOS */

/* ...stuff.c */
extern int min _((int x, int y));
extern int max _((int x, int y));
extern VOID get_directory _((char *s));
#ifdef MACINTOSH
extern VOID maximum_memory _((void));
#ifndef applec
extern VOID fsetfileinfo  _((char *fname, OSType creator, OSType type));
#endif /* applec */
#endif /* MACINTOSH */

/* basics.c */
extern LVAL copyarray _((LVAL array));

/* betab.c */
extern VOID betabase _((double *x, double *a, double *b, int *gia,
			int *gib, double *cdf));
extern double ppbeta _((double p, double a, double b, int *ifault));

/* bivnor.c */
extern double bivnor _((double ah, double ak, double r));

/* cfft.c */
extern int cfft _((int n, double *c, double *wsave, int isign));

/* cholesky.c */
extern VOID choldecomp P4H(double *, int, double, double *);

/* clmath.c */
#ifdef BYTECODE
extern LVAL xladd2 _((LVAL x, LVAL y));
extern LVAL xlsub2 _((LVAL x, LVAL y));
extern LVAL xlmul2 _((LVAL x, LVAL y));
extern LVAL xldiv2 _((LVAL x, LVAL y));
extern LVAL xlpmax2 _((LVAL x, LVAL y));
extern LVAL xlpmin2 _((LVAL x, LVAL y));
extern LVAL xlmax2 _((LVAL x, LVAL y));
extern LVAL xlmin2 _((LVAL x, LVAL y));
extern LVAL xladd1 _((LVAL x));
extern LVAL xlsub1 _((LVAL x));
extern LVAL xllss2 _((LVAL x, LVAL y));
extern LVAL xlleq2 _((LVAL x, LVAL y));
extern LVAL xlgtr2 _((LVAL x, LVAL y));
extern LVAL xlgeq2 _((LVAL x, LVAL y));
extern LVAL xlequ2 _((LVAL x, LVAL y));
extern LVAL xlneq2 _((LVAL x, LVAL y));
extern int num_cmp2 _((int which, LVAL x, LVAL y));
#endif /* BYTECODE */

/* compound.c */
extern LVAL compounddataseq _((LVAL x));
extern int  compounddatalen _((LVAL x));
extern LVAL makecompound _((LVAL form, LVAL seq));
extern LVAL subr_map_elements _((LVAL (*f) _((void))));
extern LVAL recursive_subr_map_elements _((LVAL (*bf) _((void)),
					   LVAL (*f) _((void))));
extern LVAL reduce _((LVAL fcn, LVAL sequence,
		      int has_init, LVAL initial_value));
extern int compoundp _((LVAL x));

/* dialogs.c */
extern Point ListToPoint _((LVAL list));
extern LVAL PointToList _((Point pt));
extern int dialog_p _((LVAL x));
extern LVAL xsgetdialog _((void));
extern int dialog_item_p _((LVAL x));
extern int button_item_p _((LVAL x));
extern int toggle_item_p _((LVAL x));
extern int text_item_p _((LVAL x));
extern int choice_item_p _((LVAL x));
extern int scroll_item_p _((LVAL x));
extern int list_item_p _((LVAL x));
extern LVAL xsgetdialogitem _((void));
extern LVAL xsgetbuttonitem _((void));
extern LVAL xsgettoggleitem _((void));
extern LVAL xsgettextitem _((void));
extern LVAL xsgetchoiceitem _((void));
extern LVAL xsgetscrollitem _((void));
extern LVAL xsgetlistitem _((void));

/* gammab.c */
extern VOID gammabase _((double *x, double *a, double *p));
extern double ppgamma _((double p, double a, int *ifault));

/* gamln.c */
#undef gamma
#define gamma mygamma
extern double gamma _((double xx));

/* hrdwrobs.c */
extern int valid_dialog_address _((LVAL addr));
extern VOID set_dialog_address _((CPTR ptr, LVAL object));
extern VOID standard_hardware_clobber _((LVAL object));
extern int valid_window_address _((LVAL addr));
extern VOID set_window_address _((CPTR ptr, LVAL object));
extern CPTR GETWINDOWADDRESS _((LVAL object));
extern int valid_iview_window_address _((LVAL addr));
extern VOID set_iview_window_address _((CPTR ptr, LVAL object));
extern CPTR GETIVIEWWINDOWADDRESS _((LVAL object));
extern int valid_iview_address _((LVAL addr));
extern VOID set_iview_address _((CPTR ptr, LVAL object));
extern CPTR get_iview_address _((LVAL object));
extern CPTR GETIVIEWADDRESS _((LVAL object));
extern CPTR GETDIALOGADDRESS _((LVAL object));
extern int valid_menu_address _((LVAL addr));
extern VOID set_menu_address _((CPTR ptr, LVAL object));
extern CPTR get_menu_address _((LVAL object));
extern LVAL get_hardware_object_by_address _((CPTR ptr));
#ifdef MACINTOSH
extern int valid_apple_menu_address _((LVAL addr));
extern VOID set_apple_menu_address _((CPTR ptr, LVAL object));
extern CPTR get_apple_menu_address _((LVAL object));
extern int valid_edit_window_address _((LVAL addr));
extern VOID set_edit_window_address _((CPTR ptr, LVAL object));
extern CPTR get_edit_window_address _((LVAL object));
extern int valid_display_window_address _((LVAL addr));
extern VOID set_display_window_address _((CPTR ptr, LVAL object));
extern CPTR get_display_window_address _((LVAL object));
#endif /* MACINTOSH */
#ifdef AMIGA
extern int valid_amiga_menu_address _((LVAL addr));
extern void set_amiga_menu_address _((void *ptr, LVAL object));
extern void *get_amiga_menu_address _((LVAL object));
extern void set_amiga_menu_window _((void *ptr, LVAL object));
extern void *get_amiga_menu_window _((LVAL object));
#endif /* AMIGA */

/* iview.c */
extern int StGrNumVariables _((StGWWinInfo *gwinfo));
extern int IViewNumVariables _((IVIEW_WINDOW w));
extern VOID IViewSetVariableLabel _((IVIEW_WINDOW w, int var, char *s));
extern char *IViewVariableLabel _((IVIEW_WINDOW w, int var));
extern VOID GetNiceRange _((double *low, double *high, int *ticks));
extern int IViewNumPoints _((IVIEW_WINDOW w));
extern VOID IViewSetPointState _((IVIEW_WINDOW w, int point, PointState state));
extern PointState IViewPointState _((IVIEW_WINDOW w, int point));
extern VOID IViewSetPointScreenState _((IVIEW_WINDOW w, int point, PointState state));
extern PointState IViewPointScreenState _((IVIEW_WINDOW w, int point));
extern VOID IViewSetScaledRange _((IVIEW_WINDOW w, int var, double low, double high));
extern VOID IViewGetScaledRange _((IVIEW_WINDOW w, int var, double *low, double *high));
extern VOID IViewSetScreenRange _((IVIEW_WINDOW w, int var, int low, int high));
extern VOID IViewGetScreenRange _((IVIEW_WINDOW w, int var, int *low, int *high));
extern VOID IViewSetIdentityTransformation _((IVIEW_WINDOW w));
extern VOID IViewSetTransformation _((IVIEW_WINDOW w, double **a));
extern double **IViewTransformation _((IVIEW_WINDOW w));
extern int IViewIsTransformed _((IVIEW_WINDOW w));
extern VOID IViewStdResize _((IVIEW_WINDOW w));
extern VOID IViewStdRedraw _((IVIEW_WINDOW w));
extern VOID IViewStdRedrawBackground _((IVIEW_WINDOW w));
extern VOID IViewGetContentMarginRect _((IVIEW_WINDOW w,
                                         int *left, int *top, int *width, int *height));
extern VOID IViewStdClearContent P1H(IVIEW_WINDOW);
extern VOID IViewStdRedrawContent _((IVIEW_WINDOW w));
extern VOID IViewStdMarkPointsInRect _((IVIEW_WINDOW w,
                                        int left, int top, int width, int height));
extern VOID IViewSetPointSymbol _((IVIEW_WINDOW w, int point, int sym, int hsym));
extern VOID IViewGetPointSymbol _((IVIEW_WINDOW w, int point, int *sym, int *hsym));
extern int IViewNumLines _((IVIEW_WINDOW w));
extern VOID IViewSetLineWidth _((IVIEW_WINDOW w, int line, int width));
extern VOID IViewGetLineWidth  _((IVIEW_WINDOW w, int line, int *width));
extern VOID IViewRotate2 _((IVIEW_WINDOW w,
                            unsigned var1, unsigned var2, double newalpha));
extern VOID IViewSetRange _((IVIEW_WINDOW w, int var, double low, double high));
extern VOID IViewGetRange _((IVIEW_WINDOW w, int var, double *low, double *high));
extern VOID IViewDrawDataPoints _((IVIEW_WINDOW w,
                                   unsigned var1, unsigned var2, unsigned m, unsigned n));
extern VOID IViewDrawDataLines _((IVIEW_WINDOW w,
                                  unsigned var1, unsigned var2, unsigned m, unsigned n));
extern VOID IViewResetScreenStates _((IVIEW_WINDOW w));
extern VOID IViewSetPointMask _((IVIEW_WINDOW w, int point, int masked));
extern int IViewPointMasked _((IVIEW_WINDOW w, int point));
extern int IViewPointScreenValue _((IVIEW_WINDOW w, int var, int point));
extern VOID IViewClearPointMarks _((IVIEW_WINDOW w));
extern VOID IViewGetScreenPointValues _((IVIEW_WINDOW w, int point, int *x));
extern VOID IViewSetPointColor _((IVIEW_WINDOW w, int point, int color));
extern int IViewPointColor _((IVIEW_WINDOW w, int point));
extern VOID IViewSetPointLabel _((IVIEW_WINDOW w, int point, char *s));
extern char *IViewPointLabel _((IVIEW_WINDOW w, int point));
extern VOID IViewSetPointMark _((IVIEW_WINDOW w, int point, int marked));
extern int IViewPointMarked _((IVIEW_WINDOW w, int point));
extern VOID IViewApplyTransformation _((IVIEW_WINDOW w, double **a, int *inbasis));
extern double IViewEncodeValue _((IVIEW_WINDOW w, double value, int var));
extern double IViewDecodeValue _((IVIEW_WINDOW w, double value, int var));
extern VOID IViewAddPoints _((IVIEW_WINDOW w, int n));
extern VOID IViewClearPoints _((IVIEW_WINDOW w));
extern VOID IViewSetPointValue _((IVIEW_WINDOW w, int var, int point, double value));
extern double IViewPointValue _((IVIEW_WINDOW w, int var, int point));
extern VOID IViewSetPointScaledValue _((IVIEW_WINDOW w, int var, int point, double value));
extern double IViewPointScaledValue _((IVIEW_WINDOW w, int var, int point));
extern double IViewPointTransformedValue _((IVIEW_WINDOW w, int var, int point));
extern int IViewPointScreenValue _((IVIEW_WINDOW w, int var, int point));
extern VOID IViewGetScreenPointValues _((IVIEW_WINDOW w, int point, int *x));
extern VOID IViewSetPointMask _((IVIEW_WINDOW w, int point, int masked));
extern int IViewPointMasked _((IVIEW_WINDOW w, int point));
extern VOID IViewSetPointColor _((IVIEW_WINDOW w, int point, int color));
extern int IViewPointColor _((IVIEW_WINDOW w, int point));
extern VOID IViewSetPointState _((IVIEW_WINDOW w, int point, PointState state));
extern PointState IViewPointState _((IVIEW_WINDOW w, int point));
extern VOID IViewSetPointScreenState _((IVIEW_WINDOW w, int point, PointState state));
extern PointState IViewPointScreenState _((IVIEW_WINDOW w, int point));
extern VOID IViewResetScreenStates _((IVIEW_WINDOW w));
extern VOID IViewSetPointMark _((IVIEW_WINDOW w, int point, int marked));
extern int IViewPointMarked _((IVIEW_WINDOW w, int point));
extern VOID IViewClearPointMarks _((IVIEW_WINDOW w));
extern VOID IViewSetPointLabel _((IVIEW_WINDOW w, int point, char *s));
extern char *IViewPointLabel _((IVIEW_WINDOW w, int point));
extern VOID IViewSetPointSymbol _((IVIEW_WINDOW w, int point, int sym, int hsym));
extern VOID IViewGetPointSymbol _((IVIEW_WINDOW w, int point, int *sym, int *hsym));
extern int IViewNumLines _((IVIEW_WINDOW w));
extern VOID IViewAddLines _((IVIEW_WINDOW w, int n));
extern VOID IViewClearLines _((IVIEW_WINDOW w));
extern VOID IViewSetLineValue _((IVIEW_WINDOW w, int var, int line, double value));
extern double IViewLineValue _((IVIEW_WINDOW w, int var, int line));
extern VOID IViewSetLineScaledValue _((IVIEW_WINDOW w, int var, int line, double value));
extern double IViewLineScaledValue _((IVIEW_WINDOW w, int var, int line));
extern double IViewLineTransformedValue _((IVIEW_WINDOW w, int var, int line));
extern int IViewLineScreenValue _((IVIEW_WINDOW w, int var, int line));
extern VOID IViewSetLineMask _((IVIEW_WINDOW w, int line, int masked));
extern int IViewLineMasked _((IVIEW_WINDOW w, int line));
extern VOID IViewSetLineColor _((IVIEW_WINDOW w, int line, int color));
extern int IViewLineColor _((IVIEW_WINDOW w, int line));
extern VOID IViewSetNextLine _((IVIEW_WINDOW w, int line, int next));
extern int IViewNextLine _((IVIEW_WINDOW w, int line));
extern VOID IViewSetLineType _((IVIEW_WINDOW w, int line, int type));
extern int IViewLineType _((IVIEW_WINDOW w, int line));
extern VOID IViewSetLineWidth _((IVIEW_WINDOW w, int line, int width));
extern VOID IViewGetLineWidth _((IVIEW_WINDOW w, int line, int *width));
extern VOID IViewDepthCuePoints _((IVIEW_WINDOW w, unsigned var,
                                   unsigned cut1, unsigned cut2, unsigned cut3,
                                   unsigned m, unsigned n));
extern VOID IViewStdAdjustScreen _((IVIEW_WINDOW w));
extern VOID IViewStdAdjustPointsInRect _((IVIEW_WINDOW w,
                                          int left, int top, int width, int height,
                                          PointState state));
extern VOID IViewStdAdjustScreenPoint _((IVIEW_WINDOW w, int i));

/* iviewdat.c */
extern VOID IViewDataSetPointColor _((IViewData p, unsigned point, int color));
extern VOID IViewDataSetPointState _((IViewData p, unsigned point, int state));
extern VOID IViewDataSetPointSymbol _((IViewData p, unsigned point, int sym, int hsym));
extern VOID IViewDataSetLineValue _((IViewData p, int var, int line, double value));
extern VOID IViewDataSetLineColor _((IViewData p, unsigned line, int color));
extern VOID IViewDataSetLineWidth _((IViewData p, unsigned line, unsigned width));
extern VOID IViewDataSetNextLine _((IViewData p, unsigned line, int next));
extern VOID IViewDataSetLineType _((IViewData p, unsigned line, int type));
extern int IViewDataNumVariables _((IViewData p));
extern VOID IViewDataSetVariableLabel _((IViewData p, unsigned var, char *s));
extern char *IViewDataVariableLabel _((IViewData p, unsigned var));
extern VOID IViewDataSetRange _((IViewData p, unsigned var, double low, double high));
extern VOID IViewDataGetRange _((IViewData p, unsigned var, double *low, double *high));
extern VOID IViewDataSetScreenRange _((IViewData p, unsigned var, int low, int high));
extern VOID IViewDataGetScreenRange _((IViewData p, unsigned var, int *low, int *high));
extern IViewData IViewDataNew _((int vars));
extern VOID IViewDataFree _((IViewData p));
extern VOID IViewDataSetIdentityTransformation _((IViewData data));
extern VOID IViewDataSetTransformation _((IViewData data, double **a));
extern double **IViewDataTransformation _((IViewData data));
extern VOID IViewDataApplyTransformation _((IViewData data, double **a, int *inbasis));
extern int IViewDataIsTransformed _((IViewData data));
extern VOID IViewDataAddPoints _((IViewData p, int n));
extern VOID IViewDataClearPoints _((IViewData p));
extern int IViewDataNumPoints _((IViewData p));
extern VOID IViewDataSetPointValue _((IViewData p, int var, int point, double value));
extern double IViewDataPointValue _((IViewData p, int var, int point));
extern double IViewDataPointTransformedValue _((IViewData p, int var, int point));
extern int IViewDataPointScreenValue _((IViewData p, int var, int point));
extern VOID IViewDataGetScreenPointValues _((IViewData d, int point, int *x));
extern VOID IViewDataSetPointMask _((IViewData p, unsigned point, int masked));
extern int IViewDataPointMasked _((IViewData p, unsigned point));
extern VOID IViewDataSetPointColor _((IViewData p, unsigned point, int color));
extern int IViewDataPointColor _((IViewData p, unsigned point));
extern VOID IViewDataSetPointState _((IViewData p, unsigned point, int state));
extern PointState IViewDataPointState _((IViewData p, unsigned point));
extern VOID IViewDataSetPointScreenState _((IViewData p, unsigned point, PointState state));
extern PointState IViewDataPointScreenState _((IViewData p, unsigned point));
extern VOID IViewDataResetScreenStates _((IViewData p));
extern VOID IViewDataSetPointMark _((IViewData p, unsigned point, int marked));
extern int IViewDataPointMarked _((IViewData p, unsigned point));
extern VOID IViewDataClearPointMarks _((IViewData p));
extern VOID IViewDataSetPointLabel _((IViewData p, unsigned point, char *s));
extern char *IViewDataPointLabel _((IViewData p, unsigned point));
extern VOID IViewDataSetPointSymbol _((IViewData p, unsigned point, int sym, int hsym));
extern VOID IViewDataGetPointSymbol _((IViewData p, unsigned point, int *sym, int *hsym));
extern int IViewDataNumLines _((IViewData p));
extern VOID IViewDataAddLines _((IViewData p, int n));
extern VOID IViewDataClearLines _((IViewData p));
extern VOID IViewDataSetLineValue _((IViewData p, int var, int line, double value));
extern double IViewDataLineValue _((IViewData p, int var, int line));
extern double IViewDataLineTransformedValue _((IViewData p, int var, int line));
extern int IViewDataLineScreenValue _((IViewData p, int var, int line));
extern VOID IViewDataSetLineMask _((IViewData p, unsigned line, int masked));
extern int IViewDataLineMasked _((IViewData p, unsigned line));
extern VOID IViewDataSetLineColor _((IViewData p, unsigned line, int color));
extern int IViewDataLineColor _((IViewData p, unsigned line));
extern VOID IViewDataSetLineWidth _((IViewData p, unsigned line, unsigned width));
extern VOID IViewDataGetLineWidth _((IViewData p, unsigned line, unsigned *width));
extern VOID IViewDataSetNextLine _((IViewData p, unsigned line, int next));
extern int IViewDataNextLine _((IViewData p, unsigned line));
extern VOID IViewDataSetLineType _((IViewData p, unsigned line, int type));
extern int IViewDataLineType _((IViewData p, unsigned line));
extern VOID IViewDataDrawPoints _((IViewData data, IVIEW_WINDOW w,
                                   unsigned var1, unsigned var2, unsigned m, unsigned n,
                                   int offset));
extern VOID IViewDataDrawLines _((IViewData data, IVIEW_WINDOW w,
                                  unsigned var1, unsigned var2, unsigned m, unsigned n));
extern VOID IViewDataCuePoints _((IViewData data, unsigned var,
                                  int cut1, int cut2, int cut3, unsigned m, unsigned n));

/* iviewint.c */
extern VOID IViewSetShowingLabels _((IVIEW_WINDOW w, int show));
extern int IViewShowingLabels _((IVIEW_WINDOW w));
extern VOID IViewSetFixedAspect _((IVIEW_WINDOW w, int fixed));
extern int IViewFixedAspect _((IVIEW_WINDOW w));
extern VOID IViewSetMouseMode _((IVIEW_WINDOW w, MouseMode mode));
extern VOID IViewSetData _((IVIEW_WINDOW w, IViewData data));
extern VOID IViewSetScale _((IVIEW_WINDOW w, unsigned var, double scale));
extern double IViewScale _((IVIEW_WINDOW w, unsigned var));
extern VOID IViewSetShift _((IVIEW_WINDOW w, unsigned var, double shift));
extern double IViewShift _((IVIEW_WINDOW w, unsigned var));
extern VOID StGrSetContentRect _((StGWWinInfo *gwinfo,
                                  int left, int top, int width, int height));
extern VOID StGrGetContentRect _((StGWWinInfo *gwinfo,
                                  int *left, int *top, int *width, int *height));
extern VOID StGrSetContentOrigin _((StGWWinInfo *gwinfo, int x, int y));
extern VOID StGrGetContentOrigin _((StGWWinInfo *gwinfo, int *x, int *y));
extern VOID StGrSetContentVariables _((StGWWinInfo *gwinfo, int x, int y));
extern VOID StGrGetContentVariables _((StGWWinInfo *gwinfo, int *x, int *y));
extern VOID StGrSetClickRange _((StGWWinInfo *gwinfo, int width, int height));
extern VOID StGrGetClickRange _((StGWWinInfo *gwinfo, int *width, int *height));
extern VOID StGrSetMargin _((StGWWinInfo *gwinfo,
                             int left, int top, int right, int bottom));
extern VOID StGrGetMargin _((StGWWinInfo *gwinfo,
                             int *left, int *top, int *right, int *bottom));
extern VOID IViewSetMouseMode _((IVIEW_WINDOW w, MouseMode mode));
extern MouseMode IViewMouseMode _((IVIEW_WINDOW w));
extern int StGrDirty _((StGWWinInfo *gwinfo));
extern VOID StGrSetDirty _((StGWWinInfo *gwinfo, int dirty));
extern VOID IViewSetXaxis _((IVIEW_WINDOW w, int showing, int labeled, int ticks));
extern VOID IViewGetXaxis _((IVIEW_WINDOW w, int *showing, int *labeled, int *ticks));
extern VOID IViewSetYaxis _((IVIEW_WINDOW w, int showing, int labeled, int ticks));
extern VOID IViewGetYaxis _((IVIEW_WINDOW w, int *showing, int *labeled, int *ticks));
extern VOID IViewSetBrush _((IVIEW_WINDOW w, int x, int y, int width, int height));
extern VOID IViewGetBrush _((IVIEW_WINDOW w, int *x, int *y, int *width, int *height));
extern VOID IViewEraseBrush _((IVIEW_WINDOW w));
extern VOID IViewDrawBrush _((IVIEW_WINDOW w));
extern VOID IViewMoveBrush _((IVIEW_WINDOW w, int x, int y));
extern VOID IViewStdMouseAction _((IVIEW_WINDOW w, int x, int y,
                                   MouseEventType type, MouseClickModifier mods));
extern VOID IViewStdUnselectAllPoints _((IVIEW_WINDOW w));
extern VOID IViewEraseSelection _((IVIEW_WINDOW w));
extern VOID IViewMaskSelection _((IVIEW_WINDOW w));
extern VOID IViewUnmaskAllPoints _((IVIEW_WINDOW w));
extern VOID IViewShowAllPoints _((IVIEW_WINDOW w));
extern int IViewAllPointsShowing _((IVIEW_WINDOW w));
extern int IViewAllPointsUnmasked _((IVIEW_WINDOW w));
extern int IViewAnyPointsSelected _((IVIEW_WINDOW w));
extern VOID IViewSetLinks _((IVIEW_WINDOW w, LVAL links));
extern LVAL IViewGetLinks _((IVIEW_WINDOW w));
extern int IViewIsLinked _((IVIEW_WINDOW w));
extern char *StGrData _((StGWWinInfo *gwinfo));
extern IViewData IViewDataPtr _((IVIEW_WINDOW w));
extern VOID IViewGetAxisMargin _((IVIEW_WINDOW w,
                                  int *left, int *top, int *right, int *bottom));
extern VOID IViewDrawAxes _((IVIEW_WINDOW w));
extern VOID IViewFreeMem _((IVIEW_WINDOW w));
extern VOID IViewStdSelectingMouseAction _((IVIEW_WINDOW w, int x, int y,
                                            MouseEventType type, MouseClickModifier mods));
extern VOID IViewStdBrushingMouseAction _((IVIEW_WINDOW w, int x, int y,
                                           MouseEventType type, MouseClickModifier mods));


/* iviewscl.c */
extern VOID IViewScaleToRange _((IVIEW_WINDOW w, unsigned var,
	                             double low, double high));
extern VOID IViewApplyScaleShift _((IVIEW_WINDOW w, unsigned var,
                                    double scale, double shift));
extern VOID IViewGetVisibleRange _((IVIEW_WINDOW w, unsigned var,
                                    double *plow, double *phigh));

/* mats1.c */
#define matrixp(a) (darrayp(a) && (getdarrayrank(a) == 2))
#define xlgamatrix() (testarg(typearg(matrixp)))
#define numrows(a) ((int) getfixnum(getelement(getdarraydim(a), 0)))
#define numcols(a) ((int) getfixnum(getelement(getdarraydim(a), 1)))

/* kernel.c */
extern int kernel_smooth P10H(double *, double *, int, double, double *,
			      double *, double *, double *, int, int);

/* linalg.c */
#define seqlen(x) (listp(x) ? llength(x) : gettvecsize(x))
extern int anycomplex P1H(LVAL);
extern double macheps _((void));

/* lowess.c */
extern int lowess _((double *x, double *y, int n, double f, int nsteps,
                     double delta, double *ys, double *rw, double *res));

/* makerot.c */
extern VOID make_rotation _((int n, double *rot, double *x, double *y,
                             int use_alpha, double alpha));

/* menus.c */
extern int menu_p _((LVAL x));
extern LVAL xsgetmenu _((void));
extern int StMObAllocated _((LVAL menu));
extern int menu_item_p _((LVAL x));
extern LVAL xsgetmenuitem _((void));
extern VOID StMObAllocate _((LVAL menu));
VOID StMObAllocate _((LVAL menu));
VOID StMObDispose _((LVAL menu));

/* obinit.c */
extern VOID init_objects _((void));

/* objects.c */
extern int kind_of_p _((LVAL x, LVAL y));
extern LVAL slot_value _((LVAL x, LVAL slot));
extern LVAL set_slot_value _((LVAL x, LVAL slot, LVAL value));
extern LVAL send_message_stk _((LVAL object, LVAL selector));
extern int lex_slot_value _((LVAL object, LVAL sym, LVAL *pval));
extern LVAL init_root_object _((void));
extern LVAL xsnewproto _((char *str, LVAL parents));

/* nor.c */
extern VOID normbase _((double *x, double *phi));

/* obinit.c */
extern VOID init_objects _((void));

/* objects.c */
extern VOID xsaddmsg _((LVAL object, char *str));
extern VOID xsaddslot _((LVAL object, char *str));
extern VOID xsaddinstanceslot _((LVAL x, char *s));
extern VOID xssetslotval _((LVAL x, char *s, LVAL val));
extern VOID statobsymbols _((void));
extern VOID object_isnew _((LVAL object));
extern VOID print_mobject _((LVAL object, LVAL stream));

/* ppnd.c */
extern double ppnd _((double p, int *ifault));

/* splines.c */
extern int fit_spline _((int n, double *x, double *y,
                         int ns, double *xs, double *ys, double *work));

/* statinit.c */
extern VOID statsymbols _((void));

/* stats.c */
extern LVAL elementseq _((LVAL x));

/* stmem.c */
extern ALLOCTYPE *StCalloc _((int n, int m));
extern VOID StFree _((ALLOCTYPE *));
extern ALLOCTYPE *StRCalloc _((int n, int m));
extern VOID StRFree _((ALLOCTYPE *p));
extern long StRSize _((ALLOCTYPE *p));
extern ALLOCTYPE *StRRealloc _((ALLOCTYPE *p, int n, int m));
extern ALLOCTYPE *StRPtr _((ALLOCTYPE *p));
extern VOID StRLock _((ALLOCTYPE *p));
extern VOID StRUnlock _((ALLOCTYPE *p));

/* studentb.c */
extern VOID studentbase _((double *x, double *df, double *cdf));
extern double ppstudent _((double pp, double n, int *ifault));     

/* utils.c */
extern LVAL list2 _((LVAL x1, LVAL x2));
extern LVAL list3 _((LVAL x1, LVAL x2, LVAL x3));
extern LVAL getnextelement _((LVAL *pseq, int i));
extern VOID setnextelement _((LVAL *pseq, int i, LVAL value));
extern LVAL peekarg _((int i));
extern LVAL xsgetsequence _((void));
extern int xsboolkey _((LVAL key, int dflt));
extern LVAL xsfuncall0 _((LVAL fun));
extern LVAL xsfuncall1 _((LVAL fun, LVAL x));
extern LVAL xsfuncall2 _((LVAL fun, LVAL x, LVAL y));
extern LVAL list3 _((LVAL x1, LVAL x2, LVAL x3));
extern VOID pushargvec _((LVAL fun, int argc, LVAL  *argv));
extern LVAL xsapplysubr _((LVAL (*f) _((void)), LVAL args));
extern LVAL xscallsubrvec _((LVAL (*f) _((void)), int argc, LVAL *argv));

/* utils2.c */
extern LVAL integer_list_2 _((int a, int b));
extern LVAL integer_list_3 _((int a, int b, int c));
extern LVAL integer_list_4 _((int a, int b, int c, int d));
extern LVAL send_message _((LVAL object, LVAL msg));
extern LVAL send_callback_message _((LVAL object, LVAL msg));
extern LVAL send_message1 _((LVAL object, LVAL msg, int a));
extern LVAL send_callback_message1 _((LVAL object, LVAL msg, int a));
extern LVAL send_message_1L _((LVAL object, LVAL symbol, LVAL value));
extern LVAL send_callback_message_1L _((LVAL object, LVAL msg, LVAL value));
extern LVAL apply_send _((LVAL object, LVAL symbol, LVAL args));
extern LVAL double_list_2 _((double a, double b));

/* windows.c */
extern VOID get_window_bounds _((LVAL object,
	                             int *left, int *top, int *width, int *height));

/* xsiview.c */
extern VOID check_redraw _((LVAL object, int deflt, int content_only));
extern int draw_key_arg _((int deflt));
extern VOID check_add_to_screen _((LVAL object,
				   int which, int old_n, int n, int redraw));
extern VOID internal_iview_add_points _((IVIEW_WINDOW w, LVAL object, LVAL data)); 
extern VOID IViewRedrawBackground _((IVIEW_WINDOW w));
extern VOID IViewClearContent P1H(IVIEW_WINDOW);
extern VOID IViewRedrawContent _((IVIEW_WINDOW w));
extern VOID IViewRedrawOverlays _((IVIEW_WINDOW w));
extern VOID IViewResizeOverlays _((IVIEW_WINDOW w));

/* xsiview3.c */
extern PointState decode_point_state _((LVAL state));

/* xsivint.c */
extern VOID initialize_graph _((LVAL object));
extern VOID initialize_iview _((IVIEW_WINDOW w, LVAL object));
extern VOID IViewMatchPointState _((IVIEW_WINDOW w, unsigned p));
extern VOID IViewAdjustScreens _((IVIEW_WINDOW w));
extern VOID IViewAdjustScreens _((IVIEW_WINDOW w));
extern VOID IViewCheckLinks _((IVIEW_WINDOW w));
extern VOID get_iview_ivars _((LVAL object, int *vars));
extern VOID IViewMarkPointsInRect _((IVIEW_WINDOW w,
                                     int left, int top, int width, int height));
extern VOID IViewAdjustPointsInRect _((IVIEW_WINDOW w,
                                       int left, int top, int width, int height,
                                       PointState state));
extern VOID IViewAdjustOwnScreenPoint _((IVIEW_WINDOW w, int point));
extern VOID IViewUnselectAllPoints _((IVIEW_WINDOW w));
extern VOID initialize_iview _((IVIEW_WINDOW w, LVAL object));
extern VOID IViewDoClick _((LVAL object));
extern VOID IViewDoMotion _((LVAL object));
extern int IViewInternalIsLinked _((IVIEW_WINDOW w));
extern VOID IViewUnlinkWindow _((IVIEW_WINDOW w));

/* xsivwin.c */
extern VOID initialize_graph_window _((LVAL object));
extern VOID StGWObResize _((LVAL object));
extern VOID StGWObRedraw _((LVAL object));
extern VOID internal_iview_add_lines _((IVIEW_WINDOW w, LVAL object, LVAL data));
extern VOID StGWObDoMouse _((LVAL object, int x, int y,
                             MouseEventType type, MouseClickModifier mods));
extern VOID StGWObDoKey _((LVAL object, int key, int shift, int opt));
extern VOID StGWObDoClobber _((LVAL object));
extern VOID StGWObDoIdle _((LVAL object));
extern VOID StGWGetAllocInfo _((LVAL object, char **title,
                                int *left, int *top, int *width, int *height,
                                int *goAway));

/* xsivwin2.c */
extern VOID decode_point_symbol _((LVAL lsym, int *psym, int *phsym));
extern int decode_lisp_color _((LVAL arg));
extern LVAL encode_lisp_color _((int color));
extern VOID decode_point_symbol _((LVAL lsym, int *psym, int *phsym));
extern LVAL encode_point_symbol _((int sym, int hsym));
extern int decode_cursor _((LVAL arg));
extern LVAL encode_cursor _((int cursor));

/* xshist.c */
extern VOID newhistinternals _((LVAL object));

/* xsnewplt.c */
extern VOID StGrObAdjustToData _((LVAL object, int draw));

/* graphics interface */
extern VOID SysBeep _((int n));
extern VOID StInitGraphics _((void));
extern VOID StGWSetColRefCon _((unsigned int index, long rc));
extern VOID StGWSetCursRefCon _((unsigned int index, long rc));
extern VOID StGWSetSymRefCon _((unsigned int index, long rc));
extern VOID StShowWindow _((IVIEW_WINDOW w));
extern VOID StHideWindow _((IVIEW_WINDOW w));
extern VOID StWSetTitle _((IVIEW_WINDOW w, char *str));
extern VOID StGWShowWindow _((StGWWinInfo *gwinfo));
extern int IViewGetNewBrushSize _((IVIEW_WINDOW w, int *new_width, int *new_height));
extern int StGWTextAscent _((StGWWinInfo *gwinfo));
extern int StGWTextDescent _((StGWWinInfo *gwinfo));
int StGWTextWidth _((StGWWinInfo *gwinfo, char *text));
extern VOID StGWGetViewRect _((StGWWinInfo *gwinfo,
                               int *left, int *top, int *width, int *height));
extern VOID StGWSetClipRect _((StGWWinInfo *gwinfo,
                               int clipped,
                               int left, int top, int width, int height));
extern int StGWGetClipRect _((StGWWinInfo *gwinfo,
                              int *left, int *top, int *width, int *height));
extern VOID StGWStartBuffering _((StGWWinInfo *gwinfo));
extern VOID StGWBufferToScreen _((StGWWinInfo *gwinfo,
                           int left, int top, int width, int height));
extern VOID StGWDrawLine _((StGWWinInfo *gwinfo, int x1, int y1, int x2, int y2));
extern VOID StGWDrawPoint _((StGWWinInfo *gwinfo, int x, int y));
extern VOID StGWEraseRect _((StGWWinInfo *gwinfo,
	                         int left, int top, int width, int height));
extern VOID StGWFrameRect _((StGWWinInfo *gwinfo,
	                         int left, int top, int width, int height));
extern VOID StGWPaintRect _((StGWWinInfo *gwinfo,
	                         int left, int top, int width, int height));
extern VOID StGWEraseOval _((StGWWinInfo *gwinfo,
                             int left, int top, int width, int height));
extern VOID StGWFrameOval _((StGWWinInfo *gwinfo,
                             int left, int top, int width, int height));
extern VOID StGWPaintOval _((StGWWinInfo *gwinfo,
                             int left, int top, int width, int height));
extern VOID StGWEraseArc _((StGWWinInfo *gwinfo,
                            int left, int top, int width, int height,
	                        double angle1, double angle2));
extern VOID StGWFrameArc _((StGWWinInfo *gwinfo,
                            int left, int top, int width, int height,
	                        double angle1, double angle2));
extern VOID StGWPaintArc _((StGWWinInfo *gwinfo,
	                        int left, int top, int width, int height,
	                        double angle1, double angle2));
extern VOID StGWErasePoly _((StGWWinInfo *gwinfo, int n, short *p, int from_origin));
extern VOID StGWFramePoly _((StGWWinInfo *gwinfo, int n, short *p, int from_origin));
extern VOID StGWPaintPoly _((StGWWinInfo *gwinfo, int n, short *p, int from_origin));
extern VOID StGWDrawString _((StGWWinInfo *gwinfo, char *s, int x, int y));
extern VOID StGWDrawText _((StGWWinInfo *gwinfo, char *text,
                            int x, int y, int h, int v));
extern VOID StGWDrawStringUp _((StGWWinInfo *gwinfo, char *s, int x, int y));
extern VOID StGWDrawTextUp _((StGWWinInfo *gwinfo, char *text,
                              int x, int y, int h, int v));
extern int StGWUseColor _((StGWWinInfo *gwinfo));
extern VOID StGWGetSymbolSize _((int sym, int *width, int *height));
extern VOID StGWSetLineType _((StGWWinInfo *gwinfo, int type));
extern VOID StGWSetDrawMode _((StGWWinInfo *gwinfo, int mode));
extern VOID StGWSetDrawColor _((StGWWinInfo *gwinfo, ColorCode color));
extern VOID StGWSetBackColor _((StGWWinInfo *gwinfo, ColorCode color));
extern VOID StGWSetUseColor _((StGWWinInfo *gwinfo, int use));
extern VOID StGWSetLineWidth _((StGWWinInfo *gwinfo, int width));
extern VOID StGWReverseColors _((StGWWinInfo *gwinfo));
extern VOID StGWDrawSymbol _((StGWWinInfo *gwinfo, int sym, int x, int y));
extern VOID StGWReplaceSymbol _((StGWWinInfo *gwinfo,
                                 unsigned oldsym, unsigned newsym,
	                             int x, int y));
extern int StGWCanvasWidth _((StGWWinInfo *gwinfo));
extern int StGWCanvasHeight _((StGWWinInfo *gwinfo));
extern int StGWLineType _((StGWWinInfo *gwinfo));
extern int StGWDrawMode _((StGWWinInfo *gwinfo));
extern ColorCode StGWDrawColor _((StGWWinInfo *gwinfo));
extern ColorCode StGWBackColor _((StGWWinInfo *gwinfo));
extern int StGWUseColor _((StGWWinInfo *gwinfo));
extern VOID StGWGetLineWidth _((StGWWinInfo *gwinfo, int *width));
extern int StScreenHasColor _((void));
extern int StHasWindows _((void));
extern VOID StFlushGraphics _((void));
extern VOID StGWSetSize _((StGWWinInfo *gwinfo, int width, int height, int frame));
extern VOID StWSetSize _((IVIEW_WINDOW w, int width, int height, int frame));
extern VOID StGWSetHasHscroll _((StGWWinInfo *gwinfo, int has, int size));
extern VOID StGWSetHasVscroll _((StGWWinInfo *gwinfo, int has, int size));
extern int StGWHasHscroll _((StGWWinInfo *gwinfo));
extern int StGWHasVscroll _((StGWWinInfo *gwinfo));
extern VOID StGWSetScroll _((StGWWinInfo *gwinfo, int h, int v, int move));
extern VOID StGWGetScroll _((StGWWinInfo *gwinfo, int *h, int *v));
extern VOID StGWSetHscrollIncs _((StGWWinInfo *gwinfo, int inc, int pageInc));
extern VOID StGWGetHscrollIncs _((StGWWinInfo *gwinfo, int *inc, int *pageInc));
extern VOID StGWSetVscrollIncs _((StGWWinInfo *gwinfo, int inc, int pageInc));
extern VOID StGWGetVscrollIncs _((StGWWinInfo *gwinfo, int *inc, int *pageInc));
extern int StMObInstalled _((LVAL m));
extern VOID StMObAllocateMach _((LVAL menu));
extern VOID StMObDisposeMach _((LVAL menu));
extern LVAL get_window_object _((IVIEW_WINDOW w));
extern VOID set_window_object _((IVIEW_WINDOW w, LVAL object));
extern VOID StWSetLocation _((IVIEW_WINDOW w, int left, int top, int frame));
extern VOID StWGetLocation _((IVIEW_WINDOW w, int *left, int *top, int frame));
extern VOID StWGetSize _((IVIEW_WINDOW w, int *width, int *height, int frame));
extern VOID StGetScreenSize _((int *width, int *height));
extern VOID StGWSetFreeMem _((StGWWinInfo *gwinfo, void (*FreeMem) _((IVIEW_WINDOW))));
extern int StGWIdleOn _((StGWWinInfo *gwinfo));
extern VOID StGWSetIdleOn _((StGWWinInfo *gwinfo, int on));
extern VOID StGWInitialDraw _((StGWWinInfo *gwinfo));
extern VOID StGWRemove _((StGWWinInfo *gwinfo));
extern VOID StGWWhileButtonDown _((StGWWinInfo *gwinfo,
                                   VOID (*action) _((IVIEW_WINDOW,int, int)),
                                   int motionOnly));
extern VOID DialogAllocate _((LVAL dialog));
extern VOID DialogRemove _((LVAL dialog));
extern VOID DialogSetDefaultButton _((LVAL dialog, LVAL item));
extern LVAL DialogGetModalItem _((LVAL dialog));
extern VOID DialogButtonGetDefaultSize _((LVAL item, int *width, int *height));
extern VOID DialogToggleGetDefaultSize _((LVAL item, int *width, int *height));
extern LVAL DialogToggleItemValue _((LVAL item, int set, LVAL value));
extern VOID DialogTextGetDefaultSize _((LVAL item, int *width, int *height));
extern LVAL DialogTextItemText _((LVAL item, int set, char *text));
extern VOID DialogChoiceGetDefaultSize _((LVAL item, int *width, int *height));
extern LVAL DialogChoiceItemValue _((LVAL item, int set, int value));
extern VOID DialogScrollGetDefaultSize _((LVAL item, int *width, int *height));
extern LVAL DialogScrollItemValue _((LVAL item, int set, int value));
extern LVAL DialogScrollItemMax _((LVAL item, int set, int value));
extern LVAL DialogScrollItemMin _((LVAL item, int set, int value));
extern VOID DialogListGetDefaultSize _((LVAL item, int *width, int *height));
extern VOID DialogListItemSetText _((LVAL item, LVAL index, char *text));
extern LVAL DialogListItemSelection _((LVAL item, int set, LVAL index));
extern LVAL StGWGetObject _((StGWWinInfo *gwinfo));
extern int StGWWinInfoSize _((void));
extern VOID StGWInitWinInfo _((LVAL object));
extern VOID StGWSetObject _((StGWWinInfo *gwinfo, LVAL x));
#ifdef MACINTOSH
extern VOID graph_update_action _((StGWWinInfo *gwinfo, int resized));
extern VOID graph_activate_action _((StGWWinInfo *gwinfo, int active));
extern pascal void UpdateLispMenus _((void));
#endif /* MACINTOSH */
extern VOID StMObAppendItems _((LVAL menu, LVAL items));
extern VOID StMObDeleteItem _((LVAL menu, LVAL item));
extern VOID StMObInstall _((LVAL menu));
extern VOID StMObRemove _((LVAL menu));
extern VOID StMObEnable _((LVAL menu, int enable));
extern int StMObPopup _((LVAL menu, int left, int top, LVAL window));
extern VOID StMObSetItemProp _((LVAL item, int which));
extern int StGWMakeCursor _((int n, char *image, char *mask, int h, int v, long refcon));
extern int StGWMakeResCursor _((char *name, int num, long refcon));
extern VOID StGWFreeCursor _((unsigned int index));
extern VOID StGWSetCursor _((StGWWinInfo *gwinfo, int cursor));
extern int StGWCursor _((StGWWinInfo *gwinfo));
extern VOID StGWSetRefCon _((StGWWinInfo *gwinfo, long x));
extern long StGWGetRefCon _((StGWWinInfo *gwinfo));
extern int StGWMakeColor _((double red, double green, double blue, long refcon));
extern VOID StGWFreeColor _((unsigned int index));
extern VOID StGWDrawBitmap _((StGWWinInfo *gwinfo,
                              int left, int top, int width, int height,
                              char *image));
#ifdef MACINTOSH
extern VOID StGWCopyToClip _((StGWWinInfo *gwinfo));
#endif
extern VOID StGWResetBuffer _((void));
#ifndef UNIX
extern LVAL get_menu_by_hardware _((IVIEW_MENU m));
#endif
#if (defined(AMIGA) || defined(MACINTOSH))
extern VOID StGWDumpImage _((StGWWinInfo *gwinfo, char *fname, double scale));
#else
extern VOID StGWDumpImage _((StGWWinInfo *gwinfo, FILEP file, double scale));
#endif /* AMIGA || MACINTOSH */

#ifdef _Windows
/* function prototypes */
extern int MSWAnyIdleActions(void);
extern int MSWAnyEventQueued(void);
extern void MSWDoIdleActions(void);
extern void MSWDoEventQueue(void);
extern int IsLispMenuHandle(HMENU);
extern void LispMenuUpdate(HMENU);
extern int IsLispMenuItem(WORD);
extern void LispMenuSelect(WORD);
extern void xlsmain(int, char **);
extern LVAL string2stream(char *);
extern LVAL readevalstream(LVAL);
extern void MSWCloseHelp(void);
extern BOOL InitApplResizeBrush(HANDLE hInstance);
extern BOOL StGwPrinting(StGWWinInfo *gwinfo);
extern void StGwSetPrintingDC(StGWWinInfo *gwinfo, HDC hdc);
#endif /* _Windows */
#endif /* XLSTAT_H */
