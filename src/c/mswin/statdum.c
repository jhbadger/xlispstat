#ifdef XLISP_ONLY
long time_stamp;

statfinit() { return(0); }
statsymbols() { return(0); }
set_function_docstring() { return(0); }
#endif

#define TRUE 1
#define FALSE 0

#ifdef NOGRAPHICS
StHasWindows() { return(FALSE); }
StScreenHasColor() { return(FALSE); }
StInitGraphics() { return(0); }
StGWGetCursRefCon() { return(0); }
StGWSetCursRefCon() { return(0); }
StGWSetColRefCon() { return(0); }
StGWSetSymRefCon() { return(0); }
#endif /* NOGRAPHICS */

