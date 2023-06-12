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
int in_ide_proc();
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
ephemeral memory  This is for the construction of strings.
========================================================================**/

#define EPHEMERAL_DIM (1<<10)
char ephemeral_buffer[EPHEMERAL_DIM];
int ephemeral_freeptr;

/** ========================================================================
cbreak, berror and macro_error

see berror and macro_error in mcA.mc
========================================================================**/

void breakpt(char * s);

void cbreak();

void berror(char *s);

/** ========================================================================
expressions
========================================================================**/

expptr string_atom(char * s);
expptr quote_char(char c);

int atomp(expptr e);

int symbolp(expptr e);

char * atom_string(expptr a);

expptr cons(expptr x, expptr y);

expptr stack_cons(expptr x, expptr y);

int cellp(expptr e);

expptr car(expptr x);

expptr cdr(expptr x);

expptr mk_connection(expptr connector, expptr leftarg, expptr rightarg);

expptr mkspace(expptr leftarg, expptr rightarg);

int connectionp(expptr e);

int connofp(expptr e, expptr conn);

expptr connector(expptr e);

expptr leftarg(expptr e);

expptr rightarg(expptr e);

expptr intern_paren(char openchar, expptr arg);

int parenp(expptr e);

expptr paren_inside(expptr e);

static inline char constructor(expptr e){
  if(e == NULL)berror("attempt to take the constructor of a null expression");
  return e-> constructor;}

expptr int_exp(int i);
int exp_int(expptr s);
expptr pointer_exp(void * p);

expptr atom_quote_code(expptr a);

/** ========================================================================
gensym
========================================================================**/

expptr gensym(expptr sym);

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

typedef struct explist_struct * explist;

typedef struct explist_struct{
  expptr first;
  explist rest;}explist_struct, *explist;

void open_input_file(char * s);
void open_output_file(char * s);

expptr read_from_NIDE();
expptr mcread(char*);
explist file_expressions(char * fname);

void pprint(expptr e, FILE * f);
void pp(expptr e);
char* exp_string(expptr);
char* exp_pps(expptr);
expptr reparse(expptr);

/** ========================================================================
macros
========================================================================**/
void set_macro(expptr sym, expptr f(expptr));
expptr macroexpand(expptr e);
expptr macroexpand1(expptr e);

explist preamble;
void add_preamble(expptr e);

explist init_forms;
void add_init_form(expptr e);

void add_form(expptr e);

expptr args_variables(expptr args);

void match_failure(expptr,expptr);

expptr head_symbol(expptr e);

/** ========================================================================
utilities
========================================================================**/

int containsp(expptr e1, expptr e2);

explist expcons(expptr, explist);

explist explist_append(explist,explist);
void explist_mapc(void f(expptr), explist l);
explist explist_mapcar(expptr f(expptr), explist l);
int explist_length(explist);
int explist_member(expptr,explist);
explist explist_reverse(explist);

FILE * writestrm;

void indent(int i);

/** ========================================================================
initialization
========================================================================**/

void install(expptr);

void mcA_init();
void mcB_init();
void mcC_init();
void mcD_init();
void NIDE_init();
void mcF_init();
void expandF_init();
void install_base();

int rep_column;


/** ========================================================================
expression_constants
======================================================================== **/

expptr comma, colon, semi, backquote, dollar, backslash;
expptr exclam, question, any, dot;
expptr nil, macro, intern_noticers, space, tab;

char leftbrace, leftparen, leftbracket;

expptr bqcode(expptr);
expptr bqcode_ignore_dollar(expptr);

/** ========================================================================
The following "source flags" are currently used in berror and, to a very minor extent, in printing.
======================================================================== **/

int in_doit;
int in_ide;
int in_repl;

char *  ignore_tag;
char *  result_tag;
char *  uncaught_throw_tag;
char *  reader_error_tag;
char *  expansion_error_tag;
char *  comp_error_tag;
char *  exec_error_tag;
char *  breakpoint_tag;
char *  continue_from_gdb_tag;
char * print_tag;
char * mc_ready_tag;

void send_emacs_tag(char *);
void send_result(char *);
void send_print_tag();
void send_ready();

int in_ide_proc();
FILE* read_stream_proc();

void return_to_NIDE();

void mcpprint(expptr);
void send_print_tag();

char * MetaC_directory;

expptr symbolcount();

expptr index_symbol(int i);

int symbol_index(expptr e);

int occurs_in_exp(expptr symbol, expptr exp);
int occurs_in_explist(expptr symbol, explist lst);

int catch_freeptr[1];
jmp_buf catch_stack[CATCH_DIM];
expptr catch_name[1];
expptr catch_type[1];
voidptr catch_val[1];
char undo_heap[UNDO_HEAP_DIM];

void* undo_freeptr();

void declare_except_fun(expptr name, expptr argtype);
void throw_primitive();
void throw_NIDE();

void expptr_error(expptr x, char* s);
void expptr_breakpt(expptr x, char* s);
expptr combine_atoms(expptr a1, expptr a2);

FILE * read_stream;
void init_stream();
expptr read_from_file();

