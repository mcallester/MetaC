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

#define CATCH_DIM 10000
int catch_freeptr;

jmp_buf catch_stack[CATCH_DIM];
int error_flg;

#define throw_check() {if(catch_freeptr == 0){fprintf(stderr,"\n throw without a catch\n"); exit(1);}}
#define catch_check() {if(catch_freeptr == CATCH_DIM){berror("catch stack exhausted");}}

#define throw_error() {throw_check(); error_flg=1; longjmp(catch_stack[catch_freeptr-1], 1);}
#define catch_error(body) {catch_check(); error_flg=0; if(setjmp(catch_stack[catch_freeptr++]) == 0){ \
  body; catch_freeptr--;\
  } else{\
  catch_freeptr--;if(!error_flg)fprintf(stderr, "uncaught throw\n");}}

#define throw() {throw_check(); error_flg=0; longjmp(catch_stack[catch_freeptr-1], 1);}
#define continue_throw() {throw_check(); longjmp(catch_stack[catch_freeptr-1], 1);}
#define catch(body) {catch_check(); if(setjmp(catch_stack[catch_freeptr++]) == 0){body; catch_freeptr--;} else{catch_freeptr--; if(error_flg)continue_throw();}}


#define unwind_protect(body, cleanup) {catch_check(); if(setjmp(catch_stack[catch_freeptr++]) == 0){body; catch_freeptr--;} else { catch_freeptr--; cleanup; continue_throw();}}

