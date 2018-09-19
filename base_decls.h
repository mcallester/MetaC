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
and destructuring pro
========================================================================**/

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
void setprop(expptr e, expptr key, void * val);
expptr getprop_int(expptr e, expptr key, int defaultval);
void setprop_int (expptr e, expptr key, int val);


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

expptr clean_undo_frame(expptr e);

voidptr undo_alloc(int size);

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

expptr stack_copy_memo_hits();

expptr intern_memo_hits ();

expptr intern_from_stack(expptr stack_exp);

expptr stack_copy_exp(expptr exp);

expptr add_init_form(expptr form);

expptr add_preamble(expptr form);
