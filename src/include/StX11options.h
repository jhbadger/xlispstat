/* StX11options - X11 compile options                                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */


/* Default values for options settable by x11-options. Values should   */
/* be TRUE or FALSE.                                                   */

#define USE_FAST_LINES_DEFAULT   FALSE
#define USE_FAST_SYMBOLS_DEFAULT TRUE
#define MOTION_SYNC_DEFAULT      TRUE
#define DO_CLIPPING_DEFAULT      TRUE
#define USE_ICCCM_DEFAULT        TRUE
#define WAIT_FOR_MAP_DEFAULT     TRUE


/* If you get X errors when you quit from xlispstat on a color or      */
/* greyscale workstation this might be due to a possible server bug    */
/* related to freeing color resources. Defining the preprocessor       */
/* variable SERVER_COLOR_FREE_PROBLEM may help.                        */

#define SERVER_COLOR_FREE_PROBLEM

/* On Ultrix 4.0 there is a bug in the Xmfb server's handling of the   */
/* XDrawPoints request used in drawing fast symbols. To enable a       */
/* workaround define the preprocessor variable DRAWPOINTSBUG.          */

/*#define DRAWPOINTSBUG*/

