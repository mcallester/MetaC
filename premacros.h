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

jmp_buf *catch_stack;
int *error_flg;

#define throw_check() {if(catch_freeptr[0] == 0){fprintf(stderr,"\n throw without a catch\n"); exit(1);}}
#define catch_check() {if(catch_freeptr[0] == CATCH_DIM){berror("catch stack exhausted");}}

#define throw_error() {throw_check(); error_flg[0]=1; longjmp(catch_stack[catch_freeptr[0]-1], 1);}
#define catch_error(body) {catch_check(); error_flg[0]=0; if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){ \
  body; catch_freeptr[0]--;\
  } else{\
  catch_freeptr[0]--;if(!error_flg[0])fprintf(stderr, "uncaught throw\n");}}

#define throw() {throw_check(); error_flg[0]=0; longjmp(catch_stack[catch_freeptr[0]-1], 1);}
  
#define continue_throw() {throw_check(); longjmp(catch_stack[catch_freeptr[0]-1], 1);}
#define catch(body) {catch_check(); if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){body; catch_freeptr[0]--;} else{catch_freeptr[0]--; if(error_flg[0])continue_throw();}}

#define unwind_protect(body, cleanup) {catch_check(); if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){body; catch_freeptr[0]--;} else { catch_freeptr[0]--; cleanup; continue_throw();}}


/** ========================================================================
The state variables used in undo_set must be visible to dynamically linked code.

see mcA_init at the end of mcA.c and base_decls.h
======================================================================== **/

typedef struct undopair{
  void * * location;
  void * oldval;
}undopair;

#define UNDO_TRAIL_DIM  (1 << 16)
undopair *undo_trail;
int *undo_trail_freeptr;

//in undo_set(p,v) it is important that sizeof(p) = sizeof(v) = 8 ----  8 bytes will be written at undo time.

#define undo_set(p, v)\
  {if(undo_trail_freeptr[0] >= UNDO_TRAIL_DIM)berror("undo trail exhausted"); \
    undo_trail[undo_trail_freeptr[0]].location = (void *) &(p);undo_trail[undo_trail_freeptr[0]++].oldval = (void *) p;p=v;}

/** ========================================================================
nil
======================================================================== **/

#define nil() string_atom("")
