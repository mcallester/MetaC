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
#include <dlfcn.h>

typedef char * charptr;

typedef void * voidptr;

typedef voidptr * voidptrptr;

typedef FILE * FILEptr;

typedef struct pliststruct{
  void * key;
  struct expstruct * data;
  struct pliststruct * rest;
}pliststruct,*plist;

typedef struct expstruct{
  plist data;
  char constructor;
  struct expstruct * arg1;
  struct expstruct * arg2;
}expstruct,*expptr;

/** ========================================================================
errors and breaks
========================================================================**/

void cbreak();

void breakpt(charptr s);

void berror(charptr s);


/** ========================================================================
interning (procedures called by the expansion of backquote)
========================================================================**/

charptr intern_string(charptr s);

expptr string_atom(char * s);

int atomp(expptr e);

charptr atom_string(expptr a);

expptr cons(expptr x, expptr y);

int cellp(expptr e);

expptr car(expptr x);

expptr cdr(expptr x);

expptr intern_paren(char openchar, expptr arg);

int parenp(expptr e);

expptr paren_inside(expptr e);

char constructor(expptr e);
/** ========================================================================
case

Case needs constructor, arg1, and arg2 which are all preprocessor inlines.
It also uses case_error
========================================================================**/

void match_failure(expptr topexp, expptr patterns);

/** ========================================================================
properties of expressions
========================================================================**/

expptr getprop(expptr e, expptr key, expptr defaultval);
void setprop(expptr e, expptr key, expptr val);
void addprop(expptr e, expptr key, expptr val);

/** ========================================================================
conversions
========================================================================**/

charptr exp_string(expptr s);

// expptr string_exp(charptr s);

expptr int_exp(int i);

int exp_int(expptr s);

/** ========================================================================
gensym
========================================================================**/

expptr gensym(char * s);


/** ========================================================================
reading and printing
========================================================================**/

void pprint(expptr e, FILEptr f, int i);

/** ========================================================================
macros
========================================================================**/

void set_macro(expptr sym, expptr f(expptr));

expptr macroexpand(expptr e);

expptr macroexpand1(expptr e);

/** ========================================================================
cons and nil 
========================================================================**/

expptr append(expptr l1, expptr l2);

expptr reverse(expptr l);

typedef  expptr exp_to_exp(expptr);
typedef  void exp_to_void(expptr);

expptr mapcar(exp_to_exp f, expptr l);

void mapc(exp_to_void f, expptr l);

int length(expptr l);

/** ========================================================================
undo frames
========================================================================**/

void push_undo_frame();

void pop_undo_frame();

voidptr undo_alloc(int size);

/** ========================================================================
stack frames
========================================================================**/

voidptrptr stack;

void push_stack_frame();
void push_stack_frame2(voidptr frame);
void pop_stack_frame();

voidptr current_frame();

voidptr stack_alloc(int size);

