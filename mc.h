#include "premacros.h"

typedef char * charptr;
typedef void * voidptr;

typedef struct pliststruct{
  void * key;
  void * value;
  struct pliststruct * rest;
}pliststruct,*plistptr;

typedef struct expstruct{ //in undo memory
  plistptr plist;
  void * extension;
  char constructor;
  struct expstruct * arg1;
  struct expstruct * arg2;
}expstruct,*expptr;

/** ========================================================================
push_memory_frame, pop_memory_frame, and stack_alloc
========================================================================**/

void * stack_alloc(int size);
void push_memory_frame();
void pop_memory_frame();

/** ========================================================================
undo
======================================================================== **/

void * undo_alloc(int size);
void undo_set_proc(void ** loc, void * val);
void undo_set_int_proc(int * loc, int val);
void add_undone_int(int * loc);
void add_undone_pointer(void * * loc);

void push_undo_frame();
void pop_undo_frame();
void clear_undo_frame();
void restart_undo_frame(int n);

expptr expptr_to_stack(expptr exp);
expptr expptr_to_undo(expptr exp);
expptr clean_undo_frame(expptr exp);


/** ========================================================================
cbreak, berror and macro_error

see berror and macro_error in mcA.mc
========================================================================**/

void cbreak();

void breakpt(char * s);

void berror(char *s);

/** ========================================================================
expressions
========================================================================**/

expptr string_atom(char * s);

int atomp(expptr e);

int symbolp(expptr e);

char * atom_string(expptr a);

expptr cons(expptr x, expptr y);

expptr stack_cons(expptr x, expptr y);

int cellp(expptr e);

expptr car(expptr x);

expptr cdr(expptr x);

expptr intern_paren(char openchar, expptr arg);

int parenp(expptr e);

expptr paren_inside(expptr e);

static inline char constructor(expptr e){
  if(e == NULL)berror("attempt to take the constructor of a null expression");
  return e-> constructor;}

expptr int_exp(int i);
int exp_int(expptr s);
expptr pointer_exp(void * p);
expptr undo_heap_freeptr_exp();

expptr atom_quote_code(expptr a);

/** ========================================================================
gensym
========================================================================**/

expptr gensym(char * s);

/** ========================================================================
properties
========================================================================**/

void * getprop(expptr e, void * key, void * defaultval);
int getprop_int(expptr e, void * key, int defaultval);
void setprop(expptr e, void * key, void * val);
void setprop_int(expptr e, void * key, int x);


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

expptr read_from_repl();
expptr read_from_ide();
expptr read_from_file();

expptr file_expressions(char * fname);
void pprint(expptr e, FILE * f, int i);
void pp(expptr e);
char * exp_string(expptr);

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

void add_form(expptr e);

expptr args_variables(expptr args);

void match_failure(expptr,expptr);

expptr top_atom(expptr e);

/** ========================================================================
utilities
========================================================================**/

int containsp(expptr e1, expptr e2);

void mapc(void f(expptr), expptr l);
expptr mapcar(expptr f(expptr), expptr l);
int length(expptr);
expptr append(expptr,expptr);
int member(expptr,expptr);
expptr reverse(expptr);

FILE * writestrm;

void indent(int i);

#define SYMBOL_DIM 10000

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

expptr period, comma, colon, semi, backquote, dollar, backslash, exclam, question, any, nil, dot;
expptr nil, macro, intern_noticers;

expptr bquote_code(expptr);
expptr quote_code(expptr);
expptr constructor_code(char);

void uerror(expptr);


/** ========================================================================
The following "source flags" are currently used in berror and, to a very minor extent, in printing.
======================================================================== **/

int in_doit;
int in_repl;
int in_expand;
int in_ide;

char *  ignore_tag;
char *  result_tag;
char *  reader_error_tag;
char *  expansion_error_tag;
char *  comp_error_tag;
char *  exec_error_tag;
char *  breakpoint_tag;
char *  continue_from_gdb_tag;
char * print_tag;
char * running_tag;

void send_emacs_tag(char *);
void send_result(char *);
void send_print_tag();

int in_ide_proc();
FILE* read_stream_proc();

void return_to_NIDE();

void mcpprint(expptr);

char * MetaC_directory;

expptr symbolcount();

expptr intern_index_exp(expptr e);

void * current_undo_heap_pointer();

expptr index_symbol(int i);

expptr symbol_index_exp(expptr e);

int undostack_freeptr;

int occurs_in(expptr symbol, expptr exp);
