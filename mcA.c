#include "mc.h"

/** ========================================================================
undo_alloc, undo_set_int and undo_set
premacros.h (included form mc.h) contains

#define undo_set_int(pointer,value) undo_set_int_proc((int *) &pointer,value)

#define undo_set(pointer,value) undo_set_proc((void **) &pointer,value)
========================================================================**/

#define UNDO_HEAP_DIM (1<<26)
char undo_heap[UNDO_HEAP_DIM];
int undo_heap_freeptr;

void * undo_alloc(int size){
  if(undo_heap_freeptr + size > UNDO_HEAP_DIM)berror("undo heap exhausted");
  char * result = &undo_heap[undo_heap_freeptr];
  undo_heap_freeptr += size;
  return result;
}


typedef struct undopair_int{
  int * location;
  int oldval;
}undopair_int;

#define UNDO_TRAIL_INT_DIM  (1 << 14)
undopair_int undo_trail_int[UNDO_TRAIL_INT_DIM];
int undo_trail_int_freeptr;

void undo_set_int_proc(int * loc, int val){
  if(undo_trail_int_freeptr == UNDO_TRAIL_INT_DIM)berror("undo trail exhausted");
  undo_trail_int[undo_trail_int_freeptr].location = loc;
  undo_trail_int[undo_trail_int_freeptr++].oldval = *loc;
  *loc = val;}


typedef struct undopair{
  void * * location;
  void * oldval;
}undopair;

#define UNDO_TRAIL_DIM  (1 << 16)
undopair undo_trail[UNDO_TRAIL_DIM];
int undo_trail_freeptr;

void undo_set_proc(void ** loc, void * val){
  if(undo_trail_freeptr == UNDO_TRAIL_DIM)berror("undo trail exhausted");
  undo_trail[undo_trail_freeptr].location = loc;
  undo_trail[undo_trail_freeptr++].oldval = *loc;
  *loc = val;}

int * undone_integer[100];
int undoneint_freeptr;

void add_undone_int(int * loc){
  if(undoneint_freeptr == 100)berror("too many undone integers");
  undone_integer[undoneint_freeptr++] = loc;
}

void** undone_pointer[100];
int undoneptr_freeptr;

void add_undone_pointer(voidptr * loc){
  if(undoneptr_freeptr == 100)berror("too many undone pointers");
  undone_pointer[undoneptr_freeptr++] = loc;
}

typedef struct undo_frame{
  int undo_trail_freeptr;
  int undo_trail_int_freeptr;
}undo_frame;

#define UNDOSTACK_DIM (1<<10)
undo_frame undo_stack[UNDOSTACK_DIM];
int undostack_freeptr;
int undo_checkpoint;

void save_undones(){
  for(int i = 0;i<undoneint_freeptr;i++){
    undo_trail_int[undo_trail_int_freeptr].location = undone_integer[i];
    undo_trail_int[undo_trail_int_freeptr++].oldval = *undone_integer[i];}
  for(int i = 0;i<undoneptr_freeptr;i++){
    undo_trail[undo_trail_freeptr].location = undone_pointer[i];
    undo_trail[undo_trail_freeptr++].oldval = *undone_pointer[i];}
}

void push_undo_frame(){
  if(undostack_freeptr == UNDOSTACK_DIM)berror("undo freeptr stack exhausted");
  undo_stack[undostack_freeptr].undo_trail_int_freeptr = undo_trail_int_freeptr;
  undo_stack[undostack_freeptr++].undo_trail_freeptr = undo_trail_freeptr;
  save_undones();
}

void clear_undo_frame(){
  if(undostack_freeptr == 0)berror("MetaC bug: no undo frame to clear");

  int old_trail_int_freeptr = undo_stack[undostack_freeptr-1].undo_trail_int_freeptr;
  while(undo_trail_int_freeptr != old_trail_int_freeptr){
    undo_trail_int_freeptr--;
    *(undo_trail_int[undo_trail_int_freeptr].location) = undo_trail_int[undo_trail_int_freeptr].oldval;}

  int old_trail_freeptr = undo_stack[undostack_freeptr-1].undo_trail_freeptr;
  while(undo_trail_freeptr != old_trail_freeptr){
    undo_trail_freeptr--;
    *(undo_trail[undo_trail_freeptr].location) = undo_trail[undo_trail_freeptr].oldval;}

  save_undones();
}
  
void pop_undo_frame(){
  if(undostack_freeptr == 0)berror("attempt to pop base undo frame");
  clear_undo_frame();
  undostack_freeptr--;
}

void restart_undo_frame(int n){
  if(n == undostack_freeptr){push_undo_frame();return;}
  if(n > undostack_freeptr || n < 0)berror("attempt to restarting non-existent undo frame");
  while(undostack_freeptr > n+1)pop_undo_frame();
  clear_undo_frame();
}

void init_undo1(){
  undo_heap_freeptr = 0;
  add_undone_int(&undo_heap_freeptr);
  undo_trail_int_freeptr = 0;
  undo_trail_freeptr = 0;
  undoneint_freeptr = 0;
  undoneptr_freeptr = 0;
  undostack_freeptr = 0;

}

/** ========================================================================
undo expression

in mc.h:


typedef struct expstruct{ //in undo memory
  plist plistptr; //property list
  void * internal; //application specific structure
  char constructor;
  struct expstruct * arg1;
  struct expstruct * arg2;
}expstruct,*expptr;

======================================================================== **/

#define UNDOEXP_HASH_DIM  (1 << 24)
expptr undoexp_hash_table[UNDOEXP_HASH_DIM];
int undoexp_count;

expptr intern_exp(char constr, expptr a1, expptr a2){
  if(constr == '\0')berror("bad constructuctor in intern_exp");
  unsigned int j = (constr + 729*((long int) a1) + 125*((long int) a2)) & UNDOEXP_HASH_DIM-1;
  for(int i = j;1;i++){
    if(i == UNDOEXP_HASH_DIM)i=0;
    expptr oldexp = undoexp_hash_table[i];
    if(oldexp == NULL){
      if(undoexp_count >= (2*UNDOEXP_HASH_DIM)/3)berror("expression heap exhausted");
      undoexp_count++;
      expptr newexp = (expptr) undo_alloc(sizeof(expstruct));
      newexp->plist = NULL;
      newexp->constructor = constr;
      newexp->arg1 = a1;
      newexp->arg2 = a2;
      undo_set(undoexp_hash_table[i],newexp);
      return newexp;
    }else{
      if(oldexp -> constructor == constr && oldexp->arg1 == a1 && oldexp-> arg2 == a2)return oldexp;
    }
  }
}


#define UNDOSTRING_HASH_DIM 10000
char * undostring_hash_table[UNDOSTRING_HASH_DIM];
int undostring_count;

int undostring_key(char * s){
  int i, key;

  key=0;
  for(i=0;s[i] != 0;i++){
    key = (1458*key + s[i]);
  }
  key = key&(UNDOSTRING_HASH_DIM-1);

  while(undostring_hash_table[key] != NULL
	&& strcmp(undostring_hash_table[key], s) != 0){
    key++;
    if(key==UNDOSTRING_HASH_DIM)key=0;
  }
  return key;
}

char * string_to_undo(char * s){
  int key = undostring_key(s);
  if(undostring_hash_table[key]==NULL){
    if(undostring_count >= (2*UNDOSTRING_HASH_DIM)/3)berror("string hash table exhausted");
    char * s2 = undo_alloc(strlen(s) + 1);
    strcpy(s2,s);
    undo_set(undostring_hash_table[key],s2);
    undostring_count++;
  }
  return undostring_hash_table[key];
}

void init_undo_memory(){
  init_undo1();

  for(int i=0;i<UNDOEXP_HASH_DIM;i++)undoexp_hash_table[i] = NULL;
  undoexp_count = 0;
  add_undone_int(&undoexp_count);

  for(int i=0;i<UNDOSTRING_HASH_DIM;i++)undostring_hash_table[i]=NULL;
  undostring_count = 0;
  add_undone_int(&undostring_count);
}

/** ========================================================================
undo properties

in mc.h:

typedef struct pliststruct{
  void * key;
  void * value;
  struct pliststruct * rest;
}*plistptr;

======================================================================== **/

plistptr exp_plist(expptr e){
  if(e == NULL)berror("attempt to take plist of null expression");
  return e -> plist;}

plistptr getprop_cell(expptr e, void * key){
  for(plistptr p = exp_plist(e); p!= NULL; p=p->rest){
    if(p->key == key) return p;}
  return NULL;
}

void * getprop(expptr e, void * key, void * defaultval){
  if(e == NULL)berror("attempt to get a property of the null expression");
  plistptr p = getprop_cell((expptr) e, key);
  if(p == NULL)return defaultval;
  return p -> value;
}

void addprop(expptr e, void * key, void * val){
  if(e == NULL)berror("attempt to add a property of the null expression");
  plistptr new = (plistptr) undo_alloc(sizeof(pliststruct));
  new->key = key;
  new->value = val;
  new->rest = e->plist;
  undo_set(e-> plist,new);
}

void setprop(expptr e, void * key, void * val){
  if(e == NULL)berror("attempt to set a property of the null expression");
  plistptr cell = getprop_cell(e, key);
  if(cell != NULL){undo_set(cell->value,val); return;}
  addprop(e,key,val);
}

int getprop_int(expptr e, void * key, int defaultval){
  if(e == NULL)berror("attempt to get a property of the null expression");
  plistptr p = getprop_cell(e, key);
  if(p == NULL)return defaultval;
  int * y = (int *) &(p -> value);
  return *y;
}

void addprop_int(expptr e, void * key, int val){
  if(e == NULL)berror("attempt to add a property of the null expression");
  plistptr new = (plistptr) undo_alloc(sizeof(pliststruct));
  new->key = key;
  int * y = (int *) &(new->value);
  * y = val;
  new->rest = e->plist;
  undo_set(e-> plist,new);
}

void setprop_int(expptr e, void * key, int val){
  if(e == NULL)berror("attempt to set a property of the null expression");
  plistptr cell = getprop_cell(e, key);
  if(cell != NULL){
    int * y = (int *) &(cell->value);
      undo_set_int(y,val);
      return;}
  addprop_int(e,key,val);
}

/** ========================================================================
stack memory

stack objects are not interned and stack expressions do not have properties
========================================================================**/

#define STACKHEAP_DIM (1<<19)
char stackheap[STACKHEAP_DIM];
int stackheap_freeptr;

void * stack_alloc(int size){
  if(stackheap_freeptr + size > STACKHEAP_DIM)berror("stack memory heap exhausted");
  char * result = &stackheap[stackheap_freeptr];
  stackheap_freeptr += size;
  return result;
}

#define STACK_DIM  (1 << 10)
int stackmem_stack[STACK_DIM];
int stack_frame_freeptr;

void init_stack_memory(){
  stackheap_freeptr = 0;
  stack_frame_freeptr = 0;
}

void push_memory_frame(){
  if(stack_frame_freeptr >= STACK_DIM)berror("stack memory stack exhausted");
  stackmem_stack[stack_frame_freeptr++] = stackheap_freeptr;
}

void pop_memory_frame(){
  if(stack_frame_freeptr == 0)berror("attempt to pop base stack memory frame");
  stackheap_freeptr = stackmem_stack[--stack_frame_freeptr];
}

char * string_to_stack(char * s){
  char * s2 = stack_alloc(strlen(s) + 1);
  strcpy(s2,s);
  return s2;
}

expptr stack_exp(char constr, expptr a1, expptr a2){
  if(constr == '\0')berror("bad constructuctor in stack_exp");
  expptr newexp = stack_alloc(sizeof(expstruct));
  newexp->constructor = constr;
  newexp->arg1 = a1;
  newexp->arg2 = a2;
  return newexp;
}

/** ========================================================================
clean undo frame
======================================================================== **/

expptr expptr_to_stack(expptr exp){
  if(!exp)return NULL;
  if (atomp(exp))return stack_exp('A', (expptr) string_to_stack((char *) exp->arg1), NULL);
  return stack_exp(exp->constructor,expptr_to_stack(exp->arg1),expptr_to_stack(exp->arg2));
}

expptr expptr_to_undo(expptr exp){
  if(!exp)return NULL;
  if (atomp(exp))return intern_exp('A',(expptr) string_to_undo((char *) exp->arg1),NULL);
  return intern_exp(exp->constructor,expptr_to_undo(exp->arg1),expptr_to_undo(exp->arg2));
}

expptr clean_undo_frame(expptr exp){
  //this is safe --- no user code.
  push_memory_frame();
  expptr stack_exp = expptr_to_stack(exp);
  clear_undo_frame();
  expptr result = expptr_to_undo(stack_exp);
  pop_memory_frame();
  return result;
}

/** ========================================================================
character types
========================================================================**/

int string_quotep(char x){return (x == '"' || x == '\'');}

int alphap(char c){
  return  (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || (c == '_');
}

int connp(char c){
  return c == '#' || c == '*' || c == '/' || c == '+' || c == '-' || c == '.' || c == ':'
    || c == ',' || c == '<' || c == '=' ||c == '>' || c == '@' || c == '^'
    || c == '|' || c == '&' || c == '~' ||c ==';' || c == '%' || c == '!' || c == '?';
}

int whitep(char c){return c == ' ' || c == '\n' || c== '\t';}

int closep(char c){return (c ==')' || c =='}' || c == ']');}

int openp(char c){return (c =='(' ||  c== '{' || c == '[');}

char close_for(char c){
  if(c == '(')return  ')';
  if(c == '{')return  '}';
  if(c == '[')return  ']';
  berror("illegal char given to close_for");
  return 'a';  // avoids compiler warning
}

int endp(char c){return c == EOF || c == '\0';}

int terminatorp(char c){return closep(c) || endp(c);}

int specialp(char c){return c == '`' || c == '$' || c == '\\';}

//the characters \, $ and ` are treated explicitly in the grammar.

/** ========================================================================
cons-car-cdr interface to expressions.  conses into undo space.
========================================================================**/

int atomp(expptr e){return e && constructor(e) == 'A';}

int symbolp(expptr e){return atomp(e) && alphap(atom_string(e)[0]);}

char * atom_string(expptr a){
  if(constructor(a) != 'A'){
    berror("attempt to get string of non-atom");}
  return (char *) a->arg1;}

expptr string_atom(char * s){
  return intern_exp('A', (expptr) string_to_undo(s),NULL);
}

expptr cons(expptr x, expptr y){
  if(!x || !y)berror("null argument given to cons");
  return intern_exp(' ',x,y);}

int cellp(expptr e){return e && constructor(e) == ' ';}

expptr car(expptr x){
  if(constructor(x) != ' '){berror("taking car of non-cell");}
  return x->arg1;}

expptr cdr(expptr x){
  if(constructor(x) != ' '){berror("taking cdr of non-cell");}
  return x->arg2;}

expptr intern_paren(char openchar, expptr arg){
  if(!arg)berror("null argument given to intern_paren");
  return intern_exp(openchar, arg, NULL);}

int parenp(expptr e){return e && openp(constructor(e));}

expptr paren_inside(expptr e){
  if(!openp(constructor(e)))berror("paren_inside applied to non-paren");
  return e->arg1;}

/** ========================================================================
basic list procedures (dolist is defined as a macro in mc.D)
======================================================================== **/
expptr nil;

expptr append(expptr l1, expptr l2){
  if(cellp(l1))return cons(car(l1), append(cdr(l1),l2));
  else return l2;
}

expptr reverse(expptr l){
  expptr result = nil;
  while(cellp(l)){
    result = cons(car(l), result);
    l = cdr(l);}
  return result;
}

expptr mapcar(expptr f(expptr), expptr l){
  if(cellp(l))return cons(f(car(l)),mapcar(f,cdr(l)));
  return nil;
}

void mapc(void f(expptr), expptr l){
  while (cellp(l)){f(car(l)); l = cdr(l);}
}

int length(expptr l){
  if(cellp(l))return length(cdr(l)) + 1;
  else return 0;
}

/** ========================================================================
printing: exp_string
======================================================================== **/
void putexp(expptr);
void putone(char);

char print_lastchar;

char * exp_string(expptr e){
  char * s = &(stackheap[stackheap_freeptr]);
  print_lastchar = '\0';
  putexp(e);
  putone('\0');
  return s;
}

void putone(char c){
  if(stackheap_freeptr == STACKHEAP_DIM)berror("stack heap exhausted");
  stackheap[stackheap_freeptr++] = c;
  print_lastchar=c;
}

void putstring(char * s){
  for(int i = 0;s[i] != '\0';i++){
    putone(s[i]);}
}

void putexp(expptr w){
  if(atomp(w)){
    char * s = atom_string(w);
    if((connp(s[0]) && connp(print_lastchar)) || (alphap(s[0]) && alphap(print_lastchar))) putone(' ');
    putstring(s);}
  else if(parenp(w)){char c = constructor(w); putone(c); putexp(paren_inside(w));putone(close_for(c));}
  else if(cellp(w)){putexp(car(w)); putexp(cdr(w));}
}
     
/** ========================================================================
pprint
======================================================================== **/

FILE * pprint_stream;
int pprint_paren_level;
int pprint_indent_level;

#define PPRINT_DEPTH_LIMIT 1000

char pprint_newlinep[PPRINT_DEPTH_LIMIT];

#define PAREN_LENGTH_LIMIT 60

expptr semi;
expptr comma;

void writeone(char c){fputc(c,pprint_stream); print_lastchar = c;}

void writestring(char * s){
  for(int i = 0;s[i] != '\0';i++){
    writeone(s[i]);}
}

void writeexp(expptr w){
  if(atomp(w)){
    char * s = atom_string(w);
    if((connp(s[0]) && connp(print_lastchar)) || (alphap(s[0]) && alphap(print_lastchar))) writeone(' ');
    writestring(s);}
  else if(parenp(w)){char c = constructor(w); writeone(c); writeexp(paren_inside(w));writeone(close_for(c));}
  else if(cellp(w)){writeexp(car(w)); writeexp(cdr(w));}
}

void maybe_newline(){
  if(pprint_newlinep[pprint_paren_level]){
    fprintf(pprint_stream, "\n");
    for(int i=0;i< pprint_indent_level + pprint_paren_level; i++)fprintf(pprint_stream, "  ");}
}

int exp_length(expptr e){
  if(atomp(e))return strlen(atom_string(e));
  if(cellp(e))return exp_length(car(e)) + exp_length(cdr(e));
  if(parenp(e))return 2+exp_length(paren_inside(e));
  return 0;
}

void pprint_exp(expptr w){
  //pprinting formatting is determined by the represented string independent of tree structure.
  //formatting is determined by the placement opf parantheses, semicolons and commas.
  if(atomp(w)){
    char * s = atom_string(w);
    if((connp(s[0]) && connp(print_lastchar)) || (alphap(s[0]) && alphap(print_lastchar))) writeone(' ');
    writestring(s);
    if(w == semi || w == comma)maybe_newline();}
  else if(parenp(w)){
    if(pprint_paren_level == PPRINT_DEPTH_LIMIT-1)berror("pprint depth limit exceeded");
    pprint_paren_level++;
    pprint_newlinep[pprint_paren_level] = (exp_length(w) > PAREN_LENGTH_LIMIT);
    maybe_newline();
    char c = constructor(w);
    writeone(c);
    pprint_exp(paren_inside(w));
    writeone(close_for(c));
    pprint_paren_level--;
    if(c == '{')maybe_newline();}
  else if(cellp(w)){pprint_exp(car(w)); pprint_exp(cdr(w));}
}

void pprint(expptr w, FILE * f, int indent_level){
  pprint_stream = f;
  pprint_paren_level=0;
  print_lastchar = '\0';
  pprint_indent_level = indent_level;
  pprint_newlinep[0] = (exp_length(w) + 2) > PAREN_LENGTH_LIMIT;
  if(in_repl){fprintf(f,"\n");}
  pprint_exp(w);
  fprintf(f,"\n");
}

void pp(expptr e){
  pprint(e,stdout,rep_column);}

void mcpprint(expptr e){
  if(in_ide_proc()){pprint(e,stdout,0); send_print_tag();}
  else pprint(e,stdout,0);
}

/** ========================================================================
section: backquote

dollar variables (metavariables) should be viewed as "reverse deBruijn numbers" where
number one (a naked dollar) refers to the OUTERMOST backquote and each backslash moves in one backquote.
An expression is closed if every dollar is bound by some enclosing backquote.
With reverse numbering, when a closed expression e is substituted into a context C[.] with an encolsing backquote
the indeces in e should be incretmented.

Macros generally place bodies in contexts with bound variables (inside lambda bindings).
In such situations gensym is used to avoid capture.
But I have never encountered a macro that places a body inside a backquote.
========================================================================**/


expptr app_code(char * proc, expptr arg_code){
  return cons(string_atom(proc), intern_paren('(',arg_code));}

expptr app_code2(char * proc, expptr arg1_code, expptr arg2_code){
  return cons(string_atom(proc), intern_paren('(',cons(arg1_code, cons(comma, arg2_code))));
}

#define EPHEMERAL_DIM (1<<8)
char ephemeral_buffer[EPHEMERAL_DIM];
int ephemeral_freeptr;

void ephemeral_putc(char c){
  if(ephemeral_freeptr == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[ephemeral_freeptr++]=c;
}

expptr atom_quote_code(expptr a){
  if(a == backslash){
    return app_code("string_atom",string_atom("\"\\\\\""));}
  char  * s = atom_string(a);
  ephemeral_freeptr = 0;
  ephemeral_putc('"');
  if(string_quotep(s[0])) ephemeral_putc('\\');
  for(int i = 0; s[i] != '\0'; i++)ephemeral_putc(s[i]);
  if(string_quotep(s[0])){
    ephemeral_buffer[ephemeral_freeptr-1] = '\\';
    ephemeral_putc(s[0]);}
  ephemeral_putc('"');
  ephemeral_putc('\0');
  return app_code("string_atom",string_atom(ephemeral_buffer));
}

expptr constructor_code(char c){
  sprintf(ephemeral_buffer,"'%c'",c);
  return string_atom(ephemeral_buffer);
}

expptr quote_code(expptr e){
  //this is used mcB for quotting patterns containing dollar.
  if(atomp(e))return atom_quote_code(e);
  if(parenp(e))return app_code2("intern_paren",constructor_code(constructor(e)),quote_code(paren_inside(e)));
  return app_code2("cons",quote_code(car(e)),quote_code(cdr(e)));
}

expptr bquote_code(expptr e){
  if(atomp(e))return atom_quote_code(e);
  if(parenp(e))return app_code2("intern_paren",constructor_code(constructor(e)),bquote_code(paren_inside(e)));
  if(specialp(constructor(cdr(e))))berror("illegal use of special character in backquote");
  if(car(e) == dollar){
    if(parenp(cdr(e))){return paren_inside(cdr(e));}
    if(symbolp(cdr(e)))return cdr(e);
    berror("illegal use of dollar sign in backquote");}
  if(car(e) == backslash){
    if(parenp(cdr(e)))return quote_code(paren_inside(cdr(e)));
    return quote_code(cdr(e));}
  return app_code2("cons",bquote_code(car(e)),bquote_code(cdr(e)));
}

expptr bquote_macro(expptr e){ //this is the non-recursive entry point (the macro procedure) for backquote
  if(parenp(cdr(e)))return bquote_code(paren_inside(cdr(e)));
  if(symbolp(cdr(e)))return atom_quote_code(cdr(e));
  berror("illegal backquote syntax");
  return NULL;
}

/** ========================================================================
section: macroexpand
========================================================================**/

void set_macro(expptr sym, expptr f(expptr)){
  setprop(sym, macro, (expptr) f);
}

 expptr top_atom(expptr e){
  if(!cellp(e))return NULL;
  expptr ae = car(e);
  if(atomp(ae))return ae;
  return NULL;
}

expptr macroexpand1(expptr e){
  if(!cellp(e))return e;
  expptr s = car(e);
  if(!atomp(s))return e;
  if(!(symbolp(cdr(e)) || parenp(cdr(e)) || (cellp(cdr(e)) && parenp(car(cdr(e))))))return e;
  expptr (*f)(expptr);
  f = (expptr (*)(expptr)) getprop(s,macro,NULL);
  if(f == NULL){return e;}
  return f(e);
}

expptr macroexpand(expptr e){
  if(atomp(e)) return e;
  expptr e2 = macroexpand1(e);
  if(e2 != e) return macroexpand(e2);
  if(parenp(e)) return intern_paren(constructor(e),macroexpand(paren_inside(e)));
  return intern_exp(constructor(e),macroexpand(car(e)),macroexpand(cdr(e)));
}
  

/** ========================================================================
Preamble and init_forms can be added during macro expansion
======================================================================== **/

void add_init_form(expptr form){
  expptr form2 = macroexpand(form); //this can recursively add init_forms prior to the following.
  init_forms = append(init_forms,cons(intern_paren('{',form2),nil));
}

void add_preamble(expptr e){
  expptr e2 = macroexpand(e); //this macro expansion can add to the preamble first.
  preamble = append(preamble, cons(e2,nil));}

//in the NIDE there is no difference between init_forms and preamles so we use the following

void add_form(expptr e){add_preamble(e);}

/** ========================================================================
section: gensym
========================================================================**/

expptr int_exp(int i){
  if(i < 0)berror("attempt to convert negative integer to expression");
  sprintf(ephemeral_buffer,"%d",i);
  return string_atom(ephemeral_buffer);
}

int exp_int(expptr s){
  if(!atomp(s)){berror("illegal call to exp_int");}
  return atoi((char *) atom_string(s));
}

int gensym_count;

expptr gensym(char * s){
  while(1){
    int i = rand()%(3*gensym_count);
    gensym_count++;
    int length = snprintf(ephemeral_buffer,EPHEMERAL_DIM,"%s_%d",s,i);
    if(length >= EPHEMERAL_DIM)berror("ephemeral buffer exauhsted");
    int key = undostring_key(ephemeral_buffer);
    if(undostring_hash_table[key]==NULL)break;}
  return string_atom(ephemeral_buffer);
}

/** ========================================================================
read__from_repl and read_from_ide
========================================================================**/

int from_repl;
int from_file;
int from_ide;

char readchar;
int next;
int paren_level; // this is the paren_level immediately after readchar.

expptr mcread();;

void init_readvars(){
  from_repl = 0;
  from_file = 0;
  from_ide = 0;
  paren_level = 0;
  readchar = ' ';
  next = ' ';
}

int level_adjustment(char c){
  if(openp(c))return 1;
  if(closep(c))return -1;
  return 0;
}

FILE * read_stream;
FILE* read_stream_proc(){return read_stream;}
  
expptr read_from_repl(){
  init_readvars();
  from_repl = 1;
  read_stream = stdin;
  return mcread();
}

expptr read_from_ide(){
  init_readvars();
  from_ide = 1;
  read_stream = stdin;
  expptr e = mcread();
  return e;
}

void reader_error(){
  if(in_ide)send_emacs_tag(reader_error_tag);
  if(from_file)throw_error();
  if(from_ide){
    while(next != 0)next = fgetc(read_stream);
    fgetc(read_stream);
    throw_error();}
  if(from_repl){
    while(next != '\n')next = fgetc(read_stream);
    throw_error();}
}

void simple_advance(){
  if(next == EOF){readchar = 0; return;}
  readchar = next;
  if(!(next == EOF || next == '\0'))next = fgetc(read_stream);
  if(next < EOF || next > 126 || (next > 0 && next < 32 && next != 10 && next != 9) ){
    fprintf(stdout,"illegal input character %d\n",next);
    reader_error();
  }
  paren_level += level_adjustment(readchar);
}


/** ========================================================================
file_expressions
======================================================================== **/

expptr file_expressions2();

expptr file_expressions(char * fname){
  init_readvars();
  from_file = 1;
  read_stream = fopen(fname, "r");
  if(read_stream == NULL){
    fprintf(stdout,"attempt to open %s failed",fname);
    berror("");}
  expptr exps;
  unwind_protect({
      exps = file_expressions2();
      fclose(read_stream);},
    {fclose(read_stream);});
  return exps;
}

expptr file_expressions2(){
  if(next == EOF)return nil;
  if(closep(readchar))berror("file contains unmatched close\n");
  expptr e = mcread();
  if(e == nil)return file_expressions2();
  return cons(e, file_expressions2());
}

/** ========================================================================
mcexpand
======================================================================== **/

FILE * fileout;
void process_def(expptr e);

void mcexpand(char * source, char * destination){
  preamble = nil;
  init_forms = nil;
  expptr exps = file_expressions(source);
  fileout = fopen(destination, "w");
  if(fileout == NULL)berror("attempt to open output file failed");
  while(cellp(exps)){process_def(car(exps)); exps = cdr(exps);}
  fclose(fileout);
}

void process_def(expptr e){
  if(e==nil)return;
  preamble = nil;
  expptr e2 = macroexpand(e);
  if(preamble != nil){
    pprint(preamble,fileout,0);
    fputc('\n',fileout);}
  pprint(e2,fileout,0);
}

/** ========================================================================
advance_readchar
========================================================================**/

void advance_readchar(){
  simple_advance();

  //readchar modifications
  if(readchar == '/' && next == '*'){
    //replace comment with white space
    if(from_repl && paren_level == 0){
      fprintf(stdout,"comment from REPL outside of parens");
      reader_error();
    }
    while(!(readchar == '*' && next == '/')){if(endp(readchar))return; simple_advance();}
    simple_advance();
    readchar = ' ';}

  else if(readchar == '/' && next == '/'){
    //replace comment with white space
    if(from_repl && paren_level == 0){
      fprintf(stdout,"comment from REPL outside of parens");
      reader_error();
    }
    while(readchar != '\n' && !endp(readchar))simple_advance();}

  else if(readchar == '\\' && next == '\n'){
    //advance past quoted return --- needed for parsing #define from a file.
    simple_advance(); advance_readchar();}

  else if(from_file && readchar == '\n' && !whitep(next) && !closep(next)){
    //file segmentation
    readchar = '\0';}

  else{
    //REPL termination
    if(from_repl && next == '\n' && paren_level == 0)next = '\0';}
}

void skipwhite(){
  while(whitep(readchar))advance_readchar();
}

void advance_past_white(){
  advance_readchar();
  skipwhite();
}

/** ========================================================================
The lexer --- Symbols, Connectives and Strings

mcread_symbol and mcread_connective are white-space sensitive and use
advance_readchar rather than advance_past_white.

mcread_quote must avoid readchar modifications of advance_readchar
and uses simple_advance directly.

All three of these procedures advance past white before returning.

All other reader procedures use only advance_past_white.
======================================================================== **/

expptr mcread_symbol(){
  if(!alphap(readchar))return NULL;
  ephemeral_freeptr = 0;
  while(alphap(readchar)){ephemeral_putc(readchar); advance_readchar();}
  ephemeral_putc('\0');
  expptr e = string_atom(ephemeral_buffer);
  skipwhite();
  return e;
}

expptr mcread_connective(){
  if(!connp(readchar))return NULL;
  ephemeral_freeptr = 0;
  while(connp(readchar)){ephemeral_putc(readchar); advance_readchar();}
  ephemeral_putc('\0');
  expptr e = string_atom(ephemeral_buffer);
  skipwhite();
  return e;
}

expptr mcread_quote(){
  if(!string_quotep(readchar))return NULL;
  //we must prevent an unterminated string from swalling all further input.
  //we solve this by not allowing return characters in strings.
  char q = readchar; //remember the quote character
  ephemeral_freeptr = 0;
  ephemeral_putc(q);
  simple_advance();
  int quoted = 0;
  while(1){
    if(readchar == EOF || readchar == '\0' || readchar == '\n'){
      fprintf(stdout,"unterminated string constant");
      reader_error();
    }
    ephemeral_putc(readchar);
    if(readchar == q && quoted == 0)break;
    if(readchar == '\\' && !quoted) quoted = 1; else quoted = 0;
    simple_advance();}
  ephemeral_putc('\0');
  expptr e = string_atom(ephemeral_buffer);
  advance_past_white();
  return e;
}

expptr mcread_special(){
  if(!specialp(readchar))return NULL;
  ephemeral_freeptr = 0;
  ephemeral_putc(readchar);
  ephemeral_putc('\0');
  expptr e = string_atom(ephemeral_buffer);
  advance_past_white();
  return e;
}


/** ========================================================================
  The reader
========================================================================**/
// parens

void declare_unmatched(char, expptr, char);

expptr mcread_Ep(int);

expptr mcread_open(){
  if(!openp(readchar))return NULL;
  char c = readchar;
  char cl = close_for(c);
  advance_past_white();
  expptr e = mcread_Ep(0);
  if(readchar != cl)declare_unmatched(c,e,readchar);
  advance_past_white();
  return intern_paren(c,e ? e : nil);
}

void declare_unmatched(char openchar, expptr e, char closechar){
  fprintf(stdout,"unmatched parentheses %c%c\n",openchar, closechar);
  pprint(e,stdout,rep_column);
  reader_error();
}

// mcread_Einf

expptr pcons(expptr x, expptr y){return x? (y? cons(x,y) : x) : y;}

expptr mcread_B(){
  if(readchar == '$')return mcread_special();
  if(readchar == '\\'){
    expptr s = mcread_special();
    return pcons(s,mcread_B());}
  return NULL;}

expptr mcread_A(){
  expptr s = mcread_open();
  if(s)return s;
  s = mcread_quote();
  if(s)return s;
  s = mcread_symbol();
  if(s)return s;
  if(readchar == '$' || readchar == '`'){
    s = mcread_special();
    return pcons(s, mcread_A());}
  if(readchar == '\\'){
    s = mcread_B();
    return pcons(s,mcread_A());}
  return NULL;
}

expptr mcread_Einf(){
  expptr s = mcread_A();
  if(s)return pcons(s,mcread_Einf());
  return NULL;
}

// mcread

int precedence(char c){
  if(terminatorp(c))return 0;
  if(c==';')return 1;
  if(c==',')return 2;
  if(c == ':')return 3;
  if(c=='@')return 4;
  if(c=='|' || c =='&' || c == '!' || c == '?')return 5;
  if(c=='=' || c=='<' || c=='>' || c =='~') return 6;
  if(c=='+' || c=='-')return 7;
  if(c=='*' || c=='/')return 8;
  if(c == '%' || c == '^' )return 9;
  if(c=='.' || c == '#')return 10;
  berror("undefined precedence");
  return 11; //prevents compiler warning
}

#define LEFT_THRESHOLD 9

expptr mcread_Ep(int p_left){
  //The stack (held on the C stack) ends in a consumer (open paren or connective) with precedence p_left
  //This returns a (possibly phantom) general expression (category E) to be consumed by the stack.
  expptr arg = mcread_Einf();
  if(!arg)arg = nil;
  //readchar must now be a connective or a terminator.
  int p_right = precedence(readchar);
  while(!terminatorp(readchar)
	&& (p_left < p_right
	    || (p_left == p_right
		&& p_left < LEFT_THRESHOLD))){
    expptr op = mcread_connective();
    // op must be non-null
    arg = pcons(pcons(arg,op),mcread_Ep(p_right));
    p_right = precedence(readchar);}
  return arg;
}

expptr mcread(){//this is called from top level read functions only
  advance_past_white();
  expptr arg = mcread_Ep(0);
  if(closep(readchar))declare_unmatched('-',arg,readchar);
  return arg ? arg : nil;
}

/** ========================================================================
cbreak, berror
========================================================================**/

void cbreak(){};

void send_emacs_tag(char * tag){
  if(!in_ide)berror("sending to emacs from REPL");
  fflush(stderr); //this is needed for the ignore tag to operate on stderr.
  fprintf(stdout,"%s",tag);
  fflush(stdout); //without this stderr can later add to the input of the tag.
}

void send_result(char * result){
  if(!in_ide)berror("send_result undefined outside of NIDE");
  fprintf(stdout,"%s",result);
  send_emacs_tag(result_tag);}

void send_print_tag(){send_emacs_tag(print_tag);}

int in_ide_proc(){return in_ide;}

void breakpt(char *s){
  if(in_ide){
    fprintf(stdout,"breakpt: %s\n",s);
    send_emacs_tag(breakpoint_tag);
    cbreak();
    send_emacs_tag(continue_from_gdb_tag);
  }
}

void berror(char *s){
  fprintf(stdout,"\n%s\n",s);
  if(in_ide){
    if(in_doit) send_emacs_tag(exec_error_tag);
    else send_emacs_tag(expansion_error_tag);}
  cbreak();
  if(in_ide)send_emacs_tag(continue_from_gdb_tag);
  throw_error();
}

void uerror(expptr e){
  pp(e);
  berror("");
}

/** ========================================================================
section: initialization
========================================================================**/

void init_source(){
  in_repl = 0;
  in_expand = 0;
  in_ide = 0;
  in_doit = 0;
}

void init_tags(){
  ignore_tag = "*#*#dsflsadk#*#*ignore*#*#dsflsadk#*#*";
  result_tag = "*#*#dsflsadk#*#*result*#*#dsflsadk#*#*";
  reader_error_tag = "*#*#dsflsadk#*#*reader-error*#*#dsflsadk#*#*";
  expansion_error_tag = "*#*#dsflsadk#*#*expansion-error*#*#dsflsadk#*#*";
  comp_error_tag = "*#*#dsflsadk#*#*comp-error*#*#dsflsadk#*#*";
  exec_error_tag = "*#*#dsflsadk#*#*exec-error*#*#dsflsadk#*#*";
  breakpoint_tag = "*#*#dsflsadk#*#*breakpoint*#*#dsflsadk#*#*";
  continue_from_gdb_tag = "*#*#dsflsadk#*#*continue-from-gdb*#*#dsflsadk#*#*";
  print_tag = "*#*#dsflsadk#*#*print*#*#dsflsadk#*#*";
}

void init_exp_constants(){
  period = string_atom(".");
  comma = string_atom(",");
  colon = string_atom(":");
  semi = string_atom(";");
  backquote = string_atom("`");
  dollar = string_atom("$");
  backslash = string_atom("\\");
  exclam = string_atom("!");
  question = string_atom("?");
  any = cons(dollar,string_atom("any"));
  dot = string_atom(".");

  nil = string_atom("");
  macro = string_atom("macro");
}

void mcA_init(){

  //state variables of catch and throw macros must be visible to dynamically linked code.
  catch_freeptr = malloc(sizeof(int));
  catch_freeptr[0] = 0;
  catch_stack = malloc(CATCH_DIM*sizeof(jmp_buf));
  error_flg = malloc(sizeof(int));
  error_flg[0] = 0;

  init_undo_memory();
  init_stack_memory();
  init_exp_constants();
  init_source();
  init_tags();
  
  gensym_count = 1;

  set_macro(backquote, bquote_macro);

  MetaC_directory = "/Users/davidmcallester/MC/";}
