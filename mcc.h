#include <dlfcn.h>

typedef char * charptr;
typedef void * voidptr;

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
catch, throw, catch-error, throw-error, and unwind_protect.
catch_error is used in the read eval print loop and so the definitions are placed in mcc.h
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
cbreak, berror and macro_error

see berror and macro_error in mccA.mcc
========================================================================**/
void cbreak();

void berror(char *s);

#define DBG_DIM 10000
#define DBG_DIM_ARGS 5

expptr dbg_stack[DBG_DIM];
expptr dbg_stack_args[DBG_DIM][DBG_DIM_ARGS];
int dbg_freeptr;

void push_dbg_expression(expptr e);

void pop_dbg_stack();

void match_failure(expptr,expptr);

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
undo frames
========================================================================**/
void push_undo_frame();
void pop_undo_frame();
void * undo_alloc(int size);

/** ========================================================================
expressions
========================================================================**/
expptr intern_exp(char constr, expptr arg1, expptr arg2);

static inline char constructor(expptr e){
  if(e == NULL)berror("attempt to take the constructor of a null expression");
  return e-> constructor;}

static inline expptr arg1(expptr e){
  if(e == NULL)berror("attempt to take arg1 of a null expression");
  return e-> arg1;}

static inline expptr arg2(expptr e){
  if(e == NULL)berror("attempt to take arg2 of a null expression");
  return e-> arg2;}

expptr op_arg1(expptr e);
expptr op_arg2(expptr e);
expptr intern_exp_op(char c, expptr a1, expptr a2);

static inline plist data(expptr e){
  if(e == NULL)berror("attempt to take data of null expression");
  return e -> data;}

int atomp(expptr e);
int symbolp(expptr e);
int string_quotep(char x);
char * intern_string(char * s);
expptr string_symbol();
charptr symbol_string(expptr s);
expptr int_exp(int i);
int symbol_int(expptr s);
charptr exp_string(expptr e);
expptr make_app(expptr sym, expptr arg);
expptr quote_code(expptr e);

/** ========================================================================
gensym
========================================================================**/

expptr gensym(expptr sym);

/** ========================================================================
properties
========================================================================**/

expptr getprop(expptr e, expptr key, expptr defaultval);
void setprop(expptr e, expptr key, expptr val);
void addprop(expptr e, expptr key, expptr val);


/** ========================================================================
expansion
========================================================================**/

void mccexpand(char * source, char * destination);
void mccexpand1(char * source, char * destination);

/** ========================================================================
io
========================================================================**/
FILE * fileout;
FILE * filein;

void open_input_file(char * s);
void open_output_file(char * s);

char readchar;  //this is used in mccD, in file_expressions, for detecting end of file.

expptr read_from_terminal();
expptr read_from_file();
void pprint(expptr e, FILE * f, int i);
void print_line(expptr e, FILE * f);
void gud_pprint(expptr e);
void put_return(FILE * ws);


/** ========================================================================
macros
========================================================================**/
void set_macro(expptr sym, expptr f(expptr));
expptr macroexpand(expptr e);
expptr macroexpand1(expptr e);

expptr preamble;
void add_preamble(expptr e);

expptr init_forms;
void add_init_form(expptr e);

expptr args_variables(expptr args);

void case_error();
expptr top_symbol(expptr e);

/** ========================================================================
utilities
========================================================================**/

int containsp(expptr e1, expptr e2);

expptr cons(expptr x, expptr y);
expptr car(expptr x);
expptr cdr(expptr x);
expptr mapcar(expptr f(expptr), expptr l);
expptr append(expptr,expptr);
expptr reverse(expptr);

expptr file_expressions(expptr fname);

FILE * writestrm;

void indent(int i);

expptr semiop;
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

void mccA_init();
void mccB_init();
void mccC_init();
void mccD_init();
void mccE_init1();
void mccE_init2();


