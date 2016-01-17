/* -*-C-*-
*
******************************************************************************
*
* UNIX primitive additions to XLISP-PLUS.
*
* Originally from:
*
******************************************************************************
*
* WINTERP 1.0 Copyright 1989 Hewlett-Packard Company (by Niels Mayer).
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Hewlett-Packard and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Hewlett-Packard and David Betz
* make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* HEWLETT-PACKARD AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL HEWLETT-PACKARD NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
* See ./winterp/COPYRIGHT for information on contacting the authors.
* 
* Please send modifications, improvements and bugfixes to mayer@hplabs.hp.com
* Post XLISP-specific questions/information to the newsgroup comp.lang.lisp.x
*
********************************************************************************
*
* Modified for XLISP-PLUS 2.1d by Brian Anderson.
*
*/

#include "xlisp.h"
#include "osdefs.h"

/* Function Prototypes */
LOCAL FILEP ospipeopen _((char *name, char *mode));
LOCAL int ospipeclose _((FILEP f));

#ifdef FILETABLE
/******************************************************************************
 * Prim_POPEN - start a process and open a pipe for read/write 
 * (code stolen from xlfio.c:xopen())
 *
 * syntax: (popen <command line> :direction <direction>)
 *                <command line> is a string to be sent to the subshell (sh).
 *                <direction> is either :input (to read from the pipe) or
 *                                      :output (to write to the pipe).
 *                                      (:input is the default)
 *
 * Popen returns a stream, or NIL if files or processes couldn't be created.
 * The  success  of  the  command  execution  can be checked by examining the 
 * return value of pclose. 
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL Prim_POPEN()
{
  char *name;           /* file name string */
  int iomode = 0;       /* file mode */
  FILEP fp;         /* opened file pointer */
  LVAL dir;         /* :direction keyword arg */
  LVAL fname;           /* file name string LVAL */

  /* get the process name */
  name = getstring(fname = xlgetfname());

  /* get direction */
  if (!xlgetkeyarg(k_direction, &dir))
    dir = k_input;      /* default is :input */
  
  /* set the mode */
  if (dir == k_input)
    iomode = S_FORREADING;
  else if (dir == k_output)
    iomode = S_FORWRITING;
  else
    xlerror("bad direction",dir);
  
  /* try to open the pipe */
  if ((fp = ospipeopen (name, (iomode & S_FORWRITING) ? CREATE_WR : OPEN_RO)) == CLOSED)
    xlfail("error opening pipe");
  
  /* return the xlisp stream as a Lisp datum*/
  return cvfile(fp,iomode);
}

LOCAL FILEP ospipeopen(name, mode)
     char *name, *mode;
{
    int i=getslot();
    char namebuf[FNAMEMAX+1];
    FILE *fp;
    
    if (!truename((char *)name, namebuf))
        strcpy(namebuf, name);  /* should not happen */

    if ((filetab[i].tname = (char *)malloc(strlen(namebuf)+1)) == NULL) {
        /* free(filetab[i].tname); */
        xlfail("insufficient memory");
    }
    
    
    if ((fp = popen(name,mode)) == NULL) {
        free(filetab[i].tname);
        return CLOSED;
    }

    filetab[i].fp = fp;

    strcpy(filetab[i].tname, namebuf);

    return i;
}

/******************************************************************************
 * Prim_PCLOSE - close a pipe opened by Prim_POPEN().
 * (code stolen from xlfio.c:xclose())
 *
 * syntax: (pclose <stream>)
 *                  <stream> is a stream created by popen.
 * returns T if the command executed successfully, otherwise, 
 * returns the exit status of the opened command.
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL Prim_PCLOSE()
{
  LVAL fptr;            /* the pipe stream to close */
  FILEP fp;

  int  result;

  /* get stream arg as a Lisp datum */
  fptr = xlgetarg();
  xllastarg();

  /* give error of not file stream */
  if (!streamp(fptr)) xlbadtype(fptr);

  /* get the stream from the Lisp datum
   * make sure the stream exists */
  if ((fp = getfile(fptr)) == CLOSED)
    return (NIL);

  /* close the pipe */
  result = ospipeclose(fp);

  if (result == -1)
    xlfail("<stream> has not been opened with popen");
    
  setsavech(fptr, '\0');
  setfile(fptr,CLOSED);

  /* return T if success (exit status 0), else return exit status */
  return (result ? cvfixnum((FIXTYPE) result) : s_true);
}

LOCAL int ospipeclose (f)
     FILEP f;
{
  int result;

  result = pclose(filetab[f].fp);
  free(filetab[f].tname);
  filetab[f].tname = NULL;
  filetab[f].fp = NULL;
  return result;
}
#endif /* FILETABLE */

/*
 * others to be converted later from Winterp version:
 *
 * fscanf-fixnum
 * fscanf-string
 * fscanf-flonum
 * copy-array
 * array-insert-pos
 * array-delete-pos
 *
 */
