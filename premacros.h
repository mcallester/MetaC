#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <time.h>
#include <dlfcn.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <fcntl.h>
#include <string.h>


/** ========================================================================
The catch macro must be a macro so that that catch point remains on the stack while executing the body.

The catch macro exposes the variables catch_freeptr and catch_stack in the DLL files.

Hence these variables must be dynamically linked
======================================================================== **/

#define CATCH_DIM 1000
jmp_buf *catch_stack;

int *catch_freeptr;

#define catch_check() {if(catch_freeptr[0] == CATCH_DIM){berror("catch stack exhausted");}}

#define catch(body1,body2){catch_check(); if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){body1; catch_freeptr[0]--;} else {body2;}}

#define unwind_protect(body1,body2){catch(body1,{body2;throw();})}

#define stop_throw(body){catch(body,{})}

void throw();

/** ========================================================================
undo_set
======================================================================== **/

#define undo_set(pointer,value) undo_set_proc((void **) &(pointer),value)

#define undo_set_int(pointer,value) undo_set_int_proc((int *) &(pointer),value)

/** ========================================================================
nil
======================================================================== **/
#define STRING_DIM 10000

#define nil() string_atom("")
