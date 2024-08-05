typedef char * charptr;

typedef int * intptr;

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
  void * extension;
  char constructor;
  struct expstruct * arg1;
  struct expstruct * arg2;
}expstruct,*expptr;

/** ========================================================================
errors and breaks
========================================================================**/

void breakpt(charptr s);

void berror(charptr s);

/** ========================================================================
the catch and throw data structures must be visible to dynamically linked procedures.
======================================================================== **/
int catch_freeptr[1];
jmp_buf catch_stack[CATCH_DIM];

/** ========================================================================
interning (procedures called by the expansion of backquote)
and destructuring pro
========================================================================**/

expptr string_atom(charptr s);

int atomp(expptr e);

charptr atom_string(expptr a);

charptr exp_string(expptr e);

expptr intern_paren(char openchar, expptr arg);

expptr mkspace(expptr left, expptr right);
expptr mk_connection(expptr connector, expptr leftarg, expptr rightarg);
int connectionp(expptr e);
expptr connector(expptr e);
expptr leftarg(expptr e);
expptr rightarg(expptr e);

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

voidptr getprop(expptr e, expptr key, expptr defaultval);
void setprop(expptr e, expptr key, voidptr val);
int getprop_int(expptr e, expptr key, int defaultval);
void setprop_int (expptr e, voidptr key, int val);


/** ========================================================================
conversions
========================================================================**/

expptr int_exp(int i);

int exp_int(expptr s);

expptr pointer_exp(voidptr p);

/** ========================================================================
gensym
========================================================================**/

expptr gensym(expptr sym);


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

int symbolp(expptr e);

/** ========================================================================
undo frames
========================================================================**/

voidptr undo_alloc(int size);
int in_ide_proc();
void undo_set_proc(voidptrptr loc, voidptr val);
void undo_set_int_proc(intptr loc, int val);
void add_undone_int(intptr loc);
void add_undone_pointer(voidptrptr loc);

void push_undo_frame();
void pop_undo_frame();
void clear_undo_frame();
void restart_undo_frame(int n);
void restart_event(expptr name);
expptr clean_undo_frame(expptr e);

/** ========================================================================
stack frames
========================================================================**/

voidptrptr stack;

void push_memory_frame();
void pop_memory_frame();

voidptr previous_heap_boundary[1];
voidptr stack_alloc(int size);


/** ========================================================================
misc.
========================================================================**/

explist file_expressions(charptr name);
expptr read_from_NIDE();

void send_result(charptr result);
void send_print_tag();

void mcpprint(expptr e);

expptr expptr_to_undo(expptr stack_exp);

expptr expptr_to_stack(expptr exp);

void add_init_form(expptr form);

void add_preamble(expptr form);

void add_form(expptr form);

int catch_freeptr[1];
jmp_buf catch_stack[CATCH_DIM];
expptr catch_name[1];
expptr catch_type[1];
voidptr catch_val[1];
char undo_heap[UNDO_HEAP_DIM];

void declare_except_fun(expptr name, expptr argtype);
void throw_primitive();

void expptr_error(expptr x, charptr s);
void expptr_breakpt(expptr x, charptr s);
expptr combine_atoms(expptr a1, expptr a2);

typedef struct explist_struct * explist;

typedef struct explist_struct{
  expptr first;
  explist rest;}explist_struct, *explist;

expptr explist_exp(explist l);

int occurs_in_exp(expptr symbol, expptr exp);

explist expcons(expptr first, explist rest);

expptr cons(expptr x, expptr y);

int undo_heap_freeptr_fun();

int in_stackheap(voidptr p);

int stackheap_allocatedp(voidptr p);

int break_on_throw[1];

void throw_NIDE(expptr msg);
