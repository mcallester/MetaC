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
  void * extension;
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
the catch and throw data structures must be visible to dynamically linked procedures.
======================================================================== **/
int catch_freeptr[1];
jmp_buf catch_stack[CATCH_DIM];
int error_flg[0];

/** ========================================================================
interning (procedures called by the expansion of backquote)
and destructuring pro
========================================================================**/

expptr string_atom(char * s);

int atomp(expptr e);

charptr atom_string(expptr a);

charptr exp_string(expptr e);

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

voidptr getprop(expptr e, expptr key, expptr defaultval);
void setprop(expptr e, expptr key, void * val);
int getprop_int(expptr e, expptr key, int defaultval);
void setprop_int (expptr e, void * key, int val);


/** ========================================================================
conversions
========================================================================**/

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

expptr top_atom(expptr e);

void set_macro(expptr sym, expptr f(expptr));

expptr macroexpand(expptr e);

expptr macroexpand1(expptr e);

int symbolp(expptr e);

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

voidptr undo_alloc(int size);
void undo_set_proc(void ** loc, void * val);
void undo_set_int_proc(int * loc, int val);
void add_undone_int(int * loc);
void add_undone_pointer(voidptr * loc);

void push_undo_frame();
void pop_undo_frame();
void clear_undo_frame();
void restart_undo_frame(int n);
expptr clean_undo_frame(expptr e);
void set_undo_checkpoint();
void pop_to_checkpoint();


/** ========================================================================
stack frames
========================================================================**/

voidptrptr stack;

void push_memory_frame();
void pop_memory_frame();

voidptr stack_alloc(int size);

expptr file_expressions(char *name);

void send_emacs_tag(char * tag);

void send_print_tag();

int in_ide_proc();

void mcpprint(expptr e);

expptr expptr_to_undo(expptr stack_exp);

expptr expptr_to_stack(expptr exp);

void add_init_form(expptr form);

void add_preamble(expptr form);

void add_form(expptr form);
