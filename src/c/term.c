/*
 *
 *    G N U P L O T  --  term.c
 *
 *  Copyright (C) 1986, 1987  Colin Kelley, Thomas Williams
 *
 *  You may use this code as you wish if credit is given and this message
 *  is retained.
 *
 *  Please e-mail any useful additions to vu-vlsi!plot so they may be
 *  included in later releases.
 *
 *  This file should be edited with 4-column tabs!  (:set ts=4 sw=4 in vi)
 */

#include "xlisp.h"
#include "gnuplot.h"
#ifdef DODO
#ifndef BCGRAPH
#include <stdio.h>
#else
#include <graphics.h>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>
#endif
#endif /* DODO */
extern LVAL s_stdout, s_unbound, s_plot_output;
static LVAL s_plotout = NIL;

#ifdef MSDOS
#define long int  /* a quick hack that seems to fix things */
#endif /* MSDOS */

LOCAL LVAL get_plot_stream()
{
  LVAL stream;

  stream = getvalue(s_plot_output);
  if (! streamp(stream) && ! ustreamp(stream)) xlerror("not a stream", stream);

  return(stream);
}

extern struct termentry term_tbl[];
int term = 1;  /* gives tek */

#define NICE_LINE		0
#define POINT_TYPES		6

void do_point(x,y,number)
     int x,y;
     int number;
{
  register int htic,vtic;
  register struct termentry *t;

  number %= POINT_TYPES;
  t = &term_tbl[term];
  htic = (t->h_tic/2);	/* should be in term_tbl[] in later version */
  vtic = (t->v_tic/2);	

  if ( x < t->h_tic || y < t->v_tic || x >= t->xmax-t->h_tic ||
      y >= t->ymax-t->v_tic ) 
    return;		/* add clipping in later version maybe */

  switch(number) {
  case 0: /* do diamond */ 
    (*t->move)(x-htic,y);
    (*t->vector)(x,y-vtic);
    (*t->vector)(x+htic,y);
    (*t->vector)(x,y+vtic);
    (*t->vector)(x-htic,y);
    (*t->move)(x,y);
    (*t->vector)(x,y);
    break;
  case 1: /* do plus */ 
    (*t->move)(x-htic,y);
    (*t->vector)(x-htic,y);
    (*t->vector)(x+htic,y);
    (*t->move)(x,y-vtic);
    (*t->vector)(x,y-vtic);
    (*t->vector)(x,y+vtic);
    break;
  case 2: /* do box */ 
    (*t->move)(x-htic,y-vtic);
    (*t->vector)(x+htic,y-vtic);
    (*t->vector)(x+htic,y+vtic);
    (*t->vector)(x-htic,y+vtic);
    (*t->vector)(x-htic,y-vtic);
    (*t->move)(x,y);
    (*t->vector)(x,y);
    break;
  case 3: /* do X */ 
    (*t->move)(x-htic,y-vtic);
    (*t->vector)(x-htic,y-vtic);
    (*t->vector)(x+htic,y+vtic);
    (*t->move)(x-htic,y+vtic);
    (*t->vector)(x-htic,y+vtic);
    (*t->vector)(x+htic,y-vtic);
    break;
  case 4: /* do triangle */ 
    (*t->move)(x,y+(4*vtic/3));
    (*t->vector)(x-(4*htic/3),y-(2*vtic/3));
    (*t->vector)(x+(4*htic/3),y-(2*vtic/3));
    (*t->vector)(x,y+(4*vtic/3));
    (*t->move)(x,y);
    (*t->vector)(x,y);
    break;
  case 5: /* do star */ 
    (*t->move)(x-htic,y);
    (*t->vector)(x-htic,y);
    (*t->vector)(x+htic,y);
    (*t->move)(x,y-vtic);
    (*t->vector)(x,y-vtic);
    (*t->vector)(x,y+vtic);
    (*t->move)(x-htic,y-vtic);
    (*t->vector)(x-htic,y-vtic);
    (*t->vector)(x+htic,y+vtic);
    (*t->move)(x-htic,y+vtic);
    (*t->vector)(x-htic,y+vtic);
    (*t->vector)(x+htic,y-vtic);
    break;
  }
}

/*
 * general point routine
 */
int line_and_point(x,y,number)
     int x,y,number;
{
  /* temporary(?) kludge to allow terminals with bad linetypes 
     to make nice marks */
  
  (*term_tbl[term].linetype)(NICE_LINE);
  do_point(x,y,number);
  return 0;
}

#ifdef UNIX

#define TEK40XMAX 1024
#define TEK40YMAX 780

#define TEK40XLAST (TEK40XMAX - 1)
#define TEK40YLAST (TEK40YMAX - 1)

#define TEK40VCHAR		25
#define TEK40HCHAR		14
#define TEK40VTIC		11
#define TEK40HTIC		11	

#define HX 0x20		/* bit pattern to OR over 5-bit data */
#define HY 0x20
#define LX 0x40
#define LY 0x60

#define LOWER5 31
#define UPPER5 (31<<5)


int TEK40init() { return 0; }


int TEK40graphics()
{
  xlputstr(get_plot_stream(), "\033\014");
  /*                   1
		       1. clear screen
		       */
  return(0);
}


int TEK40linetype(linetype)
     int linetype;
{
  return 0;
}


int TEK40vector(x,y)
     unsigned int x,y;
{
  xlputc(get_plot_stream(), (HY | (y & UPPER5)>>5));
  xlputc(get_plot_stream(), (LY | (y & LOWER5)));
  xlputc(get_plot_stream(), (HX | (x & UPPER5)>>5));
  xlputc(get_plot_stream(), (LX | (x & LOWER5)));
  return 0;
}


int TEK40move(x,y)
     unsigned int x,y;
{
  xlputc(get_plot_stream(), '\035');	/* into graphics */
  TEK40vector(x,y);
  return 0;
}


int TEK40text()
{
  TEK40move(0,12);
  xlputstr(get_plot_stream(), "\037");
  /*                   1
		       1. into alphanumerics
		       */
  return 0;
}


int TEK40lrput_text(row,str)
     unsigned int row;
     char str[];
{
  TEK40move(TEK40XMAX - TEK40HTIC - TEK40HCHAR*(strlen(str)+1),
	    TEK40VTIC + TEK40VCHAR*(row+1));
  xlputc(get_plot_stream(), '\037');
  xlputstr(get_plot_stream(), str);
  xlputc(get_plot_stream(), '\n');
  return 0;
}


int TEK40ulput_text(row,str)
     unsigned int row;
     char str[];
{
  TEK40move(TEK40HTIC, TEK40YMAX - TEK40VTIC - TEK40VCHAR*(row+1));
  xlputc(get_plot_stream(), '\037');
  xlputstr(get_plot_stream(), str);
  xlputc(get_plot_stream(), '\n');
  return 0;
}


int TEK40reset() { return 0; }

#else


#ifndef BCGRAPH
#define PC_XMAX 620
#define PC_YMAX 200

#define PC_XLAST (PC_XMAX - 1)
#define PC_YLAST (PC_YMAX - 1)

#define PC_VCHAR	8
#define PC_HCHAR	8
#define PC_VTIC	6
#define PC_HTIC	12


#include <dos.h> 

/* Arrays for storing address information for all pixels */ 

unsigned PCyaddr[200];
unsigned PCxaddr[640];
char PCshift[640];
char PCpoint[640];

char far *PCscrn = (char far *)0xB8000000;	/* Screen buffer	*/

int PCmode,PCx,PCy;

/* Draw a dot	*/ 

#define DOT(x,y)  if( 0 <= x && x < PC_XMAX \
	&& 0 <= y && y < PC_YMAX ) {\
		total = PCxaddr[x]+PCyaddr[PC_YMAX-y-1];\
 		PCscrn[total] = ( (int) 1 << PCshift[x]) | (PCscrn[total] & PCpoint[x]);\
}

PC_init()
{

	int indx = 0, indx2 = 0;
	unsigned mask;
	union REGS inregs, outregs;
	PCmode = PC_getmode();

	while (indx < PC_YMAX) {	/* calculate all y offsets      */ 
		PCyaddr[indx] = 80 * indx2;
		++indx; 
		PCyaddr[indx] = (80 * indx2) + 0x2000;
		++indx; 
		++indx2; 
	}
				/* Calculate all x offsets      */ 

	for (indx = 0; indx < PC_XMAX; ++indx) {
		mask = 0x80 >> (indx % 8);

		     /* High resolution offsets	*/ 

		PCshift[indx] = 7 - (indx % 8);
		PCpoint[indx] = ~mask;
		PCxaddr[indx] = indx / 8;
	}

	inregs.h.ah = 0;	/* Use BIOS call to set mode	*/
	inregs.h.al = 6;
	int86(0x10,&inregs,&outregs);

}
	

int PC_getmode()

{
	union REGS inregs, outregs;

	inregs.h.ah = 0x0F;	/* Use BIOS call to get mode	*/

	inregs.h.al = 0; 
	int86(0x10,&inregs,&outregs);
/*	if (outregs.h.al == 7) {
		puts("Can't run with monochrome adapter.\a");
		exit(7);
	}
	else
*/
	return(outregs.h.al);
}


PC_graphics()
{
	union REGS inregs, outregs;
	long i;

	inregs.h.ah = 0;	/* Use BIOS call to set mode	*/
	inregs.h.al = 6;
	int86(0x10,&inregs,&outregs);

	for(i=0;i<16000;i++)	/* clear screen */
		PCscrn[i]=0;
	return(0);
}


PC_text()
{
	union REGS inregs, outregs;

	getch();
	inregs.h.ah = 0;	/* Use BIOS call to set mode	*/
	inregs.h.al = PCmode;
	int86(0x10,&inregs,&outregs);
}



PC_linetype(linetype)
long linetype;
{
}


PC_move(x,y)
long x,y;
{
	PCx = x;
	PCy = y;
}


PC_vector(x,y)
long x,y;
{
	int total,i,imax,jmax,ix,iy;
	float xrat,yrat;
	
	imax = (x > PCx ? x - PCx : PCx - x) + 1;
	jmax = (y > PCy ? y - PCy : PCy - y) + 1;
	imax = (imax > jmax ? imax : jmax );
	xrat = (float) (x - PCx) / (float) imax;
	yrat = (float) (y - PCy) / (float) imax;
	for(i=0;i<imax;i++) {
		ix = PCx + xrat*i;
		iy = PCy + yrat*i;
		DOT(ix,iy)
	}
	PCx = x;
	PCy = y;
}


PC_str_text(str) /* write string to screen while still in graphics mode */
char str[];
{
	int i = 0;

	while( str[i] ) {
		PC_chr_text(str[i]);
		PCx += PC_HCHAR;
		i++;
	}
}



PC_chr_text(chr) /* write character to screen while still in graphics mode */
char chr;
{
#ifdef DODO
	char far *PCletters = (char far *) 0xF000FA6E;  /* rom character set */
	char bits;
	int total,i,j;

	for(i=0;i<8;i++) {  /* for each row of character */
		bits = PCletters[i+8*chr];
		for(j=7;j>=0;j--) {
			if( bits & 1 ) {
				DOT(PCx+j,PCy-4+i)
			}
			bits = bits >> 1;
		}
	}
#endif /* DODO */
}


PC_reset()
{
}
#else
#define PC_XMAX 620
#define PC_YMAX 200


#define PC_VCHAR	8
#define PC_HCHAR	8
#define PC_VTIC	6
#define PC_HTIC	12

int PCgdrv = VGA, PCmode = 2, PCx, PCy, PCymax;


PC_init()
{
  int errcode;
  struct termentry *t = &term_tbl[term];

  initgraph(&PCgdrv, &PCmode, "C:\\borlandc\\bgi");
  if ((errcode = graphresult()) != grOk) {
    printf("Graphics error: %s\n", grapherrormsg(errcode));
    xlfail("graphics initialization failed");
  }
  t->xmax = getmaxx();
  t->ymax = PCymax = getmaxy();
}

PC_graphics()
{
  setgraphmode(PCmode);
  return(0);
}


PC_text()
{
  getch();
  restorecrtmode();
}



PC_linetype(linetype)
long linetype;
{
}


PC_move(x,y)
long x,y;
{
	PCx = x;
	PCy = y;
}


PC_vector(x,y)
long x,y;
{
  line(PCx, PCymax - PCy, x, PCymax - y);
  PCx = x;
  PCy = y;
}


PC_str_text(str) /* write string to screen while still in graphics mode */
char *str;
{
}



PC_chr_text(chr) /* write character to screen while still in graphics mode */
char chr;
{
}


int PC_reset() { return 0; }
#endif /* BCGRAPH */

#endif /* UNIX */

int UNKNOWN_null() { return 0; }


/*
 * term_tbl[] contains an entry for each terminal.  "unknown" must be the
 *   first, since term is initialized to 0.
 */
struct termentry term_tbl[] = {
  {"unknown", 100, 100, 1, 1, 1, 1, UNKNOWN_null, UNKNOWN_null, UNKNOWN_null,
     UNKNOWN_null, UNKNOWN_null, UNKNOWN_null, UNKNOWN_null, UNKNOWN_null,
     UNKNOWN_null, UNKNOWN_null},
#ifdef UNIX
  {"tek40xx",TEK40XMAX,TEK40YMAX,TEK40VCHAR, TEK40HCHAR, TEK40VTIC, 
      TEK40HTIC, TEK40init, TEK40reset, TEK40text, TEK40graphics, 
      TEK40move, TEK40vector, TEK40linetype, TEK40lrput_text,
      TEK40ulput_text, line_and_point}
#else
  {"PC", PC_XMAX, PC_YMAX, PC_VCHAR, PC_HCHAR,
    PC_VTIC, PC_HTIC, PC_init, PC_reset,
    PC_text, PC_graphics, PC_move, PC_vector,
    PC_linetype, PC_chr_text, PC_str_text, do_point} 
#endif  
};

#define TERMCOUNT 2


