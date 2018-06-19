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

typedef char * charptr;
typedef void * voidptr;

/** ========================================================================
catch, throw, catch-error, throw-error, and unwind_protect.
catch_error is used in the read eval print loop and so the definitions are placed in mc.h
========================================================================**/

#define CATCH_DIM 10000
int catch_freeptr;

jmp_buf catch_stack[CATCH_DIM];
int error_flg;

#define throw_check() {if(catch_freeptr == 0){berror( "throw without a catch");}}
#define catch_check() {if(catch_freeptr == CATCH_DIM){berror("catch stack exhausted");}}

#define throw_error() {throw_check(); error_flg=1; longjmp(catch_stack[catch_freeptr-1], 1);}
#define catch_error(body) {catch_check(); error_flg=0; if(setjmp(catch_stack[catch_freeptr++]) == 0){ \
  body; catch_freeptr--;\
  } else{\
  catch_freeptr--;if(!error_flg)fprintf(stderr, "uncaught throw\n");}}

#define throw() {throw_check(); error_flg=0; longjmp(catch_stack[catch_freeptr-1], 1);}
#define continue_throw() {throw_check(); longjmp(catch_stack[catch_freeptr-1], 1);}
#define catch(body) {catch_check(); if(setjmp(catch_stack[catch_freeptr++]) == 0){body; catch_freeptr--;} else{catch_freeptr--; if(error_flg)continue_throw();}}


#define unwind_protect(body, cleanup) {catch_check(); if(setjmp(catch_stack[catch_freeptr++]) == 0){body; catch_freeptr--; cleanup;} else { catch_freeptr--; cleanup; continue_throw();}}


/** ========================================================================
undo
======================================================================== **/
typedef struct undopair{
  void * * location;
  void * oldval;
}undopair;

#define UNDO_TRAIL_DIM  (1 << 16)
undopair undo_trail[UNDO_TRAIL_DIM];
int undo_trail_freeptr;

//in undo_set(p,v) it is important that sizeof(p) = sizeof(v) = 8 ----  8 bytes will be written at undo time.

#define undo_set(p, v) {if(undo_trail_freeptr >= UNDO_TRAIL_DIM)berror("undo trail exhausted"); undo_trail[undo_trail_freeptr].location = (void *) &(p);undo_trail[undo_trail_freeptr++].oldval = (void *) p;p=v;}

void * undo_alloc(int size);

void push_undo_frame();

void pop_undo_frame();

/** ========================================================================
cbreak, berror and macro_error

see berror and macro_error in mcA.mc
========================================================================**/

void cbreak();

void berror(char *s);

void activate_break();

void deactivate_break();

void act_break();

#define EPHEMERAL_DIM (1<<10)
char ephemeral_buffer[EPHEMERAL_DIM];

/** ========================================================================
push_stack_frame, pop_stack_frame, and stack_alloc
========================================================================**/
#define STACK_DIM  (1 << 17)
int stack_restore[STACK_DIM];
void * stack[STACK_DIM];
int stack_frame_count;

#define STACK_HEAP_DIM (1<<19)
char stack_heap[STACK_HEAP_DIM];
int stack_heap_freeptr;


void * stack_alloc(int size);
void push_stack_frame();
void push_stack_frame2(void * frame);
void pop_stack_frame();
void * current_frame();

/** ========================================================================
expressions
========================================================================**/
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

expptr string_atom(char * s);

int atomp(expptr e);

char * atom_string(expptr a);


expptr cons(expptr x, expptr y);

int cellp(expptr e);

expptr car(expptr x);

expptr cdr(expptr x);


expptr intern_paren(char openchar, expptr arg);

int parenp(expptr e);

expptr paren_inside(expptr e);

int alphap(char), connp(char), miscp(char), string_quotep(char);

static inline char constructor(expptr e){
  if(e == NULL)berror("attempt to take the constructor of a null expression");
  return e-> constructor;}

static inline plist data(expptr e){
  if(e == NULL)berror("attempt to take data of null expression");
  return e -> data;}

char * intern_string(char * s);
expptr int_exp(int i);
int symbol_int(expptr s);
charptr exp_string(expptr e);

#define DBG_DIM 10000
#define DBG_DIM_ARGS 5

expptr dbg_stack[DBG_DIM];
expptr dbg_stack_args[DBG_DIM][DBG_DIM_ARGS];
int dbg_freeptr;

void push_dbg_expression(expptr e);

void pop_dbg_stack();

void match_failure(expptr,expptr);

/** ========================================================================
gensym
========================================================================**/

expptr gensym(char * s);

/** ========================================================================
properties
========================================================================**/

expptr getprop(expptr e, expptr key, expptr defaultval);
void setprop(expptr e, expptr key, expptr val);
void addprop(expptr e, expptr key, expptr val);

int getprop_int(expptr e, expptr key, int defaultval);
void setprop_int(expptr e, expptr key, int x);


/** ========================================================================
expansion
========================================================================**/

void mcexpand(char * source, char * destination);
void mcexpand1(char * source, char * destination);

/** ========================================================================
io
========================================================================**/
FILE * fileout;
FILE * filein;

void open_input_file(char * s);
void open_output_file(char * s);

char readchar;  //this is used in mcD, in file_expressions, for detecting end of file.

expptr read_from_terminal();
expptr read_from_file();
void pprint(expptr e, FILE * f, int i);
void print_line(expptr e, FILE * f);
void printexp(expptr e);
void put_return(FILE * ws);


/** ========================================================================
macros
========================================================================**/
void set_macro(expptr sym, expptr f(expptr));
expptr macroexpand(expptr e);
expptr macroexpand1(expptr e);
expptr full_expansion(expptr);

expptr preamble;
void add_preamble(expptr e);

expptr init_forms;
void add_init_form(expptr e);

expptr args_variables(expptr args);

void match_failure();

expptr top_atom(expptr e);

expptr replace_returns(expptr,expptr,expptr);
  
/** ========================================================================
utilities
========================================================================**/

int containsp(expptr e1, expptr e2);

expptr cons(expptr x, expptr y);
expptr car(expptr x);
expptr cdr(expptr x);
void mapc(void f(expptr), expptr l);
expptr mapcar(expptr f(expptr), expptr l);
int length(expptr);
expptr append(expptr,expptr);
expptr reverse(expptr);

expptr file_expressions(char * fname);

FILE * writestrm;

void indent(int i);

expptr commaop;
expptr colonop;
expptr spaceop;

/** ========================================================================
dynamic linking
========================================================================**/

#define SYMBOL_DIM 10000
voidptr symbol_value[SYMBOL_DIM];

/** ========================================================================
initialization
========================================================================**/

void install(expptr);

void mcA_init();
void mcB_init();
void mcC_init();
void mcD_init();
void mcE_init1();
void mcE_init2();

int rep_column;


/** ========================================================================
expression_constants
======================================================================== **/

expptr comma, colon, semi, backquote, dollar, backslash, exclam, question;
expptr nil, macro;

expptr bquote_code(expptr);
expptr quote_code(expptr);
