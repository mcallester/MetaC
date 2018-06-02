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

void berror(charptr s);


/** ========================================================================
interning (procedures called by the expansion of backquote)
========================================================================**/

expptr intern_exp(char constr, expptr arg1, expptr arg2);

charptr intern_string(charptr s);

/** ========================================================================
case

Case needs constructor, arg1, and arg2 which are all preprocessor inlines.
It also uses case_error
========================================================================**/

char constructor(expptr e);
expptr arg1(expptr e);
expptr arg2(expptr e);
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

expptr string_symbol(charptr s);

// expptr string_exp(charptr s);

expptr int_exp(int i);

int symbol_int(expptr s);

/** ========================================================================
gensym
========================================================================**/

expptr gensym(charptr s);


/** ========================================================================
reading and printing
========================================================================**/

expptr read_from_terminal();

void open_input_file(charptr s);

expptr read_from_file();

void open_output_file(charptr s);

void pprint(expptr e, FILEptr f, int i);

/** ========================================================================
macros
========================================================================**/

void set_macro(expptr sym, expptr f(expptr));

expptr macroexpand(expptr e);

expptr macroexpand1(expptr e);

/** ========================================================================
expression walking
========================================================================**/

int atomp(expptr e);

int symbolp(expptr e);

//expptr mapargs(exptr f(exptr), e expptr);

/** ========================================================================
cons and nil 
========================================================================**/

expptr cons(expptr x, expptr y);  //abbreviates `{{$x},$y}

typedef  expptr exp_to_exp(expptr);

expptr mapcar(exp_to_exp f, expptr l);

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

