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
The state variables of the catch and throw macros must be visible to dynamically linked code.

See mcA_init at the end of mcA.c and base_decls.h

I believe that catch needs to be a macro so that the stack frame in which the setjump is run
remains on the stack while the body is executed.
======================================================================== **/

#define CATCH_DIM 1000
int *catch_freeptr;
#define STRING_DIM 10000

jmp_buf *catch_stack;
int *error_flg;

#define throw_check() {if(catch_freeptr[0] == 0){fprintf(stderr,"\n uncaught throw --- C process fatal\n"); cbreak(); exit(1);}}
#define catch_check() {if(catch_freeptr[0] == CATCH_DIM){berror("catch stack exhausted");}}

#define throw_error() {throw_check(); error_flg[0]=1; longjmp(catch_stack[catch_freeptr[0]-1], 1);}
#define catch_error(body) {catch_check(); error_flg[0]=0; if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){ \
  body; catch_freeptr[0]--;\
  } else{\
    catch_freeptr[0]--;if(!error_flg[0]){fprintf(stderr, "uncaught throw caught as error\n"); cbreak();}}}

#define throw() {throw_check(); error_flg[0]=0; longjmp(catch_stack[catch_freeptr[0]-1], 1);}
  
#define continue_throw() {throw_check(); longjmp(catch_stack[catch_freeptr[0]-1], 1);}
#define catch(body) {catch_check(); if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){body; catch_freeptr[0]--;} else{catch_freeptr[0]--; if(error_flg[0])continue_throw();}}

#define unwind_protect(body, cleanup) {catch_check(); if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){body; catch_freeptr[0]--;} else { catch_freeptr[0]--; cleanup; continue_throw();}}


/** ========================================================================
undo_set
======================================================================== **/

#define undo_set(pointer,value) undo_set_proc((void **) &(pointer),value)

#define undo_set_int(pointer,value) undo_set_int_proc((int *) &(pointer),value)

/** ========================================================================
nil
======================================================================== **/

#define nil() string_atom("")
