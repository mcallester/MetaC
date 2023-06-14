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
defining catch here allows unwind protextion prior to the establishment of
mc macros in the bootstrapping procecss.

we also include definitions shared by both the static and dynamic compilations
========================================================================**/

#define STRING_DIM (1<< 18)

#define CATCH_DIM 1000

#define UNDO_HEAP_DIM (1<<30)

#define catch_check() {if(catch_freeptr[0] == CATCH_DIM){berror("catch stack exhausted");}}

#define precatch(body1,body2){catch_check(); if(setjmp(catch_stack[catch_freeptr[0]++]) == 0){body1;catch_freeptr[0]--;} else {catch_freeptr[0]--;body2;}}

#define undo_set(pointer,value) undo_set_proc((void **) &(pointer),value)

#define undo_set_int(pointer,value) undo_set_int_proc((int *) &(pointer),value)

