/* postscript - XLISP-STAT postscript hard copy routines.              */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
/* Postscript adapted from file pbmtops.c of Jef Poskanzer's pbm       */
/* system:                                                             */

/* pbmtops.c - read a portable bitmap and produce a PostScript bitmap file
**
** Copyright (C) 1988 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include "xlisp.h"

LOCAL VOID putrlitem _((int rlitem));
LOCAL VOID putrlbuffer _((void));
LOCAL VOID putitem _((void));

#ifdef DODO
sample()
{
  scale = 1.0;
    
  /* Compute padding to round cols up to the nearest multiple of 8. */
  padright = ( ( cols + 7 ) / 8 ) * 8 - cols;

  psputinit(file, cols, rows, scale );
  for ( row = 0; row < rows; row++ ) {
    for ( col = 0; col < cols; col++ )
      psputbit( bits[row][col] );
    for ( col = 0; col < padright; col++ )
      psputbit( 0 );
  }
  psputrest( );
}
#endif /* DODO */

/**************************************************************************/
/**                                                                      **/
/**                            Global Variables                          **/
/**                                                                      **/
/**************************************************************************/

static int item, bitsperitem, bitshift, rlitemsperline;
static int repeat, itembuf[128], count, repeatitem, repeatcount;
static FILE *fp;

/**************************************************************************/
/**                                                                      **/
/**                            Public Interface                          **/
/**                                                                      **/
/**************************************************************************/

/* set up global variables and print the postscript preamble */
psputinit(file, cols, rows, scale )
     FILEP file;
     int cols, rows;
     double scale;
{
  int scols, srows, left, bottom, top, right;
  
#ifdef FILETABLE
  fp = filetab[file].fp;
#else
  fp = file;
#endif
  
  scols = cols * scale * 0.96 + 0.5;	/*   0.96 is the multiple of   */
  srows = rows * scale * 0.96 + 0.5;	/* 72/300 that is closest to 1 */
  left = 300 - ( scols/2 );
  bottom = 400 - ( srows/2 );
  right = left + scols;
  top = bottom + srows;
  
  fprintf(fp, "%%!PS\n");
  fprintf(fp, "%%%%Creator:XLISP-STAT postscript image\n");
  fprintf(fp, "%%%%BoundingBox: %d %d %d %d\n", left, bottom, right, top);
  fprintf(fp, "\n" );
  fprintf(fp, "/rlestr1 1 string def\n" );
  fprintf(fp, "/rlestr 128 string def\n" );
  fprintf(fp, "/readrlestring {\n" );
  fprintf(fp, "  currentfile rlestr1 readhexstring pop  0 get\n" );
  fprintf(fp, "  dup 127 le {\n" );
  fprintf(fp, "    currentfile rlestr 0  4 3 roll  1 add  getinterval\n" );
  fprintf(fp, "    readhexstring  pop\n" );
  fprintf(fp, "  } {\n" );
  fprintf(fp, "    256 exch sub  dup\n" );
  fprintf(fp, "    currentfile rlestr1 readhexstring pop  0 get\n" );
  fprintf(fp, "    exch 0 exch 1 exch 1 sub { rlestr exch 2 index put } for\n" );
  fprintf(fp, "    pop  rlestr exch 0 exch getinterval\n" );
  fprintf(fp, "  } ifelse\n" );
  fprintf(fp, "} bind def\n" );
  fprintf(fp, "\n" );
  fprintf(fp,
	 "%d %d translate\t%% move to lower left corner of box\n",
	 left, bottom );
  fprintf(fp, "%d %d scale\t\t%% scale box\n", scols, srows );
  fprintf(fp, "\n" );
  fprintf(fp, "%d %d 1\t\t\t%% width height bits/sample\n", cols, rows );
  fprintf(fp,
	"[ %d 0 0 -%d 0 %d ]\t%% transformation matrix\n", cols, rows, rows );
  fprintf(fp, "{ readrlestring }\t%% proc\n" );
  fprintf(fp, "image\n" );

  rlitemsperline = 0;
  item = 0;
  bitsperitem = 0;
  bitshift = 7;

  repeat = 1;
  count = 0;
}

/* enter a bit into the image */
psputbit(b)
     int b;
{
  if ( bitsperitem == 8 ) {
    putitem( );
  }
  if ( ! b )
    item += 1 << bitshift;
  bitsperitem++;
  bitshift--;
}

/* clean up and print the showpage command */
psputrest( )
{
  if ( bitsperitem > 0 )
    putitem( );
  if ( count > 0 )
    putrlbuffer( );
  fprintf(fp, "\nshowpage\n" );
}

/**************************************************************************/
/**                                                                      **/
/**                           Internal Routines                          **/
/**                                                                      **/
/**************************************************************************/

LOCAL VOID putrlitem( rlitem )
     int rlitem;
{
  if ( rlitemsperline == 30 ) {
    putc('\n', fp);
    rlitemsperline = 0;
  }
  rlitemsperline++;
  fprintf(fp, "%02x", rlitem );
}

LOCAL VOID putrlbuffer( )
{
  int i;

  if ( repeat ) {
    putrlitem( 256 - count );
    putrlitem( repeatitem );
  }
  else {
    putrlitem( count - 1 );
    for ( i = 0; i < count; i++ )
      putrlitem( itembuf[i] );
  }
  repeat = 1;
  count = 0;
}

LOCAL VOID putitem( )
{
  int i;

  if ( count == 128 )
    putrlbuffer( );

  if ( repeat && count == 0 ) {
    /* Still initializing a repeat buf. */
    itembuf[count] = repeatitem = item;
    count++;
  }
  else if ( repeat ) {
    /* Repeating - watch for end of run. */
    if ( item == repeatitem ) {
      /* Run continues. */
      itembuf[count] = item;
      count++;
    }
    else { 
      /* Run ended - is it long enough to dump? */
      if ( count > 2 ) {
	/* Yes, dump a repeat-mode buffer and start a new one. */
	putrlbuffer( );
	itembuf[count] = repeatitem = item;
	count++;
      }
      else {
	/* Not long enough - convert to non-repeat mode. */
	repeat = 0;
	itembuf[count] = repeatitem = item;
	count++;
	repeatcount = 1;
      }
    }
  }
  else {
    /* Not repeating - watch for a run worth repeating. */
    if ( item == repeatitem ) {
      /* Possible run continues. */
      repeatcount++;
      if ( repeatcount > 3 ) {
	/* Long enough - dump non-repeat part and start repeat. */
	count = count - ( repeatcount - 1 );
	putrlbuffer( );
	count = repeatcount;
	for ( i = 0; i < count; i++ )
	  itembuf[i] = item;
      }
      else {
	/* Not long enough yet - continue as non-repeat buf. */
	itembuf[count] = item;
	count++;
      }
    }
    else {
      /* Broken run. */
      itembuf[count] = repeatitem = item;
      count++;
      repeatcount = 1;
    }
  }

  item = 0;
  bitsperitem = 0;
  bitshift = 7;
}
