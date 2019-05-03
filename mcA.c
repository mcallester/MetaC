#include "mc.h"

/** ========================================================================
undo memory

premacros.h contains

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

/** ========================================================================
undo strings
======================================================================== **/

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

/** ========================================================================
undo properties
======================================================================== **/

static inline plist exp_plist(expptr e){
  if(e == NULL)berror("attempt to take plist of null expression");
  return e -> plist;}

plist getprop_cell(expptr e, expptr key){
  for(plist p = exp_plist(e); p!= NULL; p=p->rest){
    if(p->key == key) return p;}
  return NULL;
}

void * getprop(expptr e, expptr key, void * defaultval){
  if(e == NULL)berror("attempt to get a property of the null expression");
  plist p = getprop_cell((expptr) e, key);
  if(p == NULL)return defaultval;
  return p -> value;
}

void addprop(expptr e, expptr key, void * val){
  if(e == NULL)berror("attempt to add a property of the null expression");
  plist new = (plist) undo_alloc(sizeof(pliststruct));
  new->key = key;
  new->value = val;
  new->rest = e->plist;
  undo_set(e-> plist,new);
}

void setprop(expptr e, expptr key, void * val){
  if(e == NULL)berror("attempt to set a property of the null expression");
  plist cell = getprop_cell(e, key);
  if(cell != NULL){undo_set(cell->value,val); return;}
  addprop(e,key,val);
}

int getprop_int(expptr e, expptr key, int defaultval){
  if(e == NULL)berror("attempt to get a property of the null expression");
  plist p = getprop_cell(e, key);
  if(p == NULL)return defaultval;
  int * y = (int *) &(p -> value);
  return *y;
}

void setprop_int(expptr e, expptr key, int x){
  char buffer[8]; //buffer is a pointer.
  int * y = (int *) buffer;
  *y = x;
  setprop(e,key, * (expptr *) buffer);
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
undo stack frames and the undo stack
======================================================================== **/

typedef struct undo_frame{
  int undo_trail_freeptr;
  int undo_heap_freeptr;
  int undoexp_count;
  int undostring_count;
}undo_frame;

#define UNDOSTACK_DIM (1<<10)
undo_frame undo_stack[UNDOSTACK_DIM];
int undostack_freeptr;

void push_undo_frame(){
  if(undostack_freeptr == UNDOSTACK_DIM)berror("undo freeptr stack exhausted");
  undo_stack[undostack_freeptr].undo_trail_freeptr = undo_trail_freeptr;
  undo_stack[undostack_freeptr].undo_heap_freeptr = undo_heap_freeptr;
  undo_stack[undostack_freeptr].undoexp_count = undoexp_count;
  undo_stack[undostack_freeptr++].undostring_count = undostring_count;
}

void init_undo_memory(){
  undo_heap_freeptr = 0;
  undo_trail_freeptr = 0;
  for(int i=0;i<UNDOEXP_HASH_DIM;i++)undoexp_hash_table[i] = NULL;
  undoexp_count = 0;
  for(int i=0;i<UNDOSTRING_HASH_DIM;i++)undostring_hash_table[i]=NULL;
  undostring_count = 0;
  undostack_freeptr = 0;
  push_undo_frame();
}

void pop_undo_frame(){
  if(undostack_freeptr == 0)berror("MetaC bug: attempt to pop base undo frame");
  undostack_freeptr--;

  int old_trail_freeptr = undo_stack[undostack_freeptr].undo_trail_freeptr;
  while(undo_trail_freeptr != old_trail_freeptr){
    undo_trail_freeptr--;
    *(undo_trail[undo_trail_freeptr].location) = undo_trail[undo_trail_freeptr].oldval;}

  undo_heap_freeptr = undo_stack[undostack_freeptr].undo_heap_freeptr;
  undoexp_count = undo_stack[undostack_freeptr].undoexp_count;
  undostring_count = undo_stack[undostack_freeptr].undostring_count;

  if(undostack_freeptr == 0)push_undo_frame();
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
  pop_undo_frame();
  push_undo_frame();
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

expptr constructor_code(char c){  //this is only used for the three open paren characters
  sprintf(ephemeral_buffer,"'%c'",c);
  return string_atom(ephemeral_buffer);
}

expptr quote_code(expptr e){
  //this is used mcB for quotting patterns containing dollar.
  if(atomp(e))return atom_quote_code(e);
  if(parenp(e))return app_code2("intern_paren",constructor_code(constructor(e)),quote_code(paren_inside(e)));
  return app_code2("cons",quote_code(car(e)),quote_code(cdr(e)));
}

expptr bquote_code(expptr);

int Vp(expptr e){
  return (cellp(e)
	  && ( (car(e) == dollar)
	       || ((car(e) == backslash)
		   && Vp(cdr(e)))));
}

expptr backslash_code(expptr e){ //we have Vp(e).
  if(car(e) == dollar){
    return app_code2("cons",atom_quote_code(dollar), bquote_code(cdr(e)));}
  return  app_code2("cons", atom_quote_code(backslash), backslash_code(cdr(e)));
}

expptr bquote_code(expptr e){
  if(atomp(e))return atom_quote_code(e);
  if(parenp(e))return app_code2("intern_paren",constructor_code(constructor(e)),bquote_code(paren_inside(e)));
  if(car(e) == dollar){
    if(parenp(cdr(e))){return paren_inside(cdr(e));}
    if(symbolp(cdr(e)))return cdr(e);}
  if(car(e) == backslash && Vp(e)){
    return backslash_code(cdr(e));}
  return app_code2("cons",bquote_code(car(e)),bquote_code(cdr(e)));
}

expptr bquote_macro(expptr e){ //this is the non-recursive entry point (the macro procedure) for backquote
  if(parenp(cdr(e)))return bquote_code(paren_inside(cdr(e)));
  return e;
}

/** ========================================================================
section: macroexpand
========================================================================**/

void set_macro(expptr sym, expptr f(expptr)){
  setprop(sym, macro, (expptr) f);
}

expptr top_atom(expptr e){
  if(!cellp(e))return NULL;
  expptr f = car(e);
  if(atomp(f))return f;
  if(!cellp(f))return NULL;
  expptr half = car(f);
  if(!cellp(half))return NULL;
  if(atomp(cdr(half))) return cdr(half);
  return NULL;
}
expptr macroexpand1(expptr e){
  expptr s = top_atom(e);
  if(s == NULL)return e;
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
  return intern_exp(constructor (e),macroexpand(car(e)),macroexpand(cdr(e)));
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
    int length = snprintf(ephemeral_buffer,EPHEMERAL_DIM,"_gen%s%d",s,i);
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
Symbols, Connectives and Strings

mcread_symbol and mcread_connective are white-space sensitive and use
advance_readchar rather than advance_past_white.

mcread_string must avoid readchar modifications of advance_readchar
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

expptr mcread_string(){
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

/** ========================================================================
parens
======================================================================== **/

void declare_unmatched(char, expptr, char);

expptr mcread_E(int);

expptr mcread_open(){
  if(!openp(readchar))return NULL;
  char c = readchar;
  char cl = close_for(c);
  advance_past_white();
  expptr e = mcread_E(0);
  if(readchar != cl)declare_unmatched(c,e,readchar);
  advance_past_white();
  return intern_paren(c,e ? e : nil);
}

void declare_unmatched(char openchar, expptr e, char closechar){
  fprintf(stdout,"unmatched parentheses %c%c\n",openchar, closechar);
  pprint(e,stdout,rep_column);
  reader_error();
}

/** ========================================================================
mcread_arg
======================================================================== **/

expptr pcons(expptr x, expptr y){return x? (y? cons(x,y) : x) : y;}

expptr mcread_parenstar(){
  expptr p = mcread_open();
  if(p)return pcons(p,mcread_parenstar());
  return NULL;
}

int junkp(expptr x){
  return
    x == backslash
    || x == dollar
    || (cellp(x) && junkp(cdr(x)));
}

expptr mcread_V();

expptr mcread_S(){
  expptr s = mcread_symbol();
  if(s)return s;
  return mcread_V();
}

expptr mcread_V(){
  if(readchar == '$'){
    advance_past_white();
    expptr s = mcread_symbol();
    if(s) return cons(dollar,s);
    expptr p = mcread_open();
    if(p) return cons(dollar,p);
    return dollar; //junk
  }
  if(readchar == '\\'){
    advance_past_white();
    return pcons(backslash,mcread_V());}
  return NULL;
}

expptr mcread_arg(){
  expptr s = mcread_string(); //quoted string
  if(s) return s;
  expptr S = mcread_S();
  if(S){
    if(junkp(S)) return S;
    return pcons(S,mcread_parenstar());}
  if(readchar == '`'){
    advance_past_white();
    return pcons(backquote, mcread_open());}
  expptr p = mcread_open();
  if(p) return p;
  return NULL;
}


/** ========================================================================
mcread
======================================================================== **/

int precedence(char c){
  if(terminatorp(c))return 0;
  if(c==';')return 1;
  if(c==',')return 2;
  if(c=='|')return 3;
  if(c =='&' || c == '!' || c == '?')return 5;
  if(c=='=' || c=='<' || c=='>' || c =='~') return 6;
  if(c=='+' || c=='-')return 7;
  if(c=='*' || c=='/')return 8;
  if(c == '%' || c == '^' || c == '#')return 9;
  if(c=='@' || c=='.' || c == ':')return 10;
  return 4; //precedence of combining adjacent arguments.
}

#define LEFT_THRESHOLD 10

expptr mcread_E(int p_left){
  //The stack (held on the C stack) ends in a consumer (open paren or connective) with precedence p_left
  //This returns a (possibly phantom) general expression (category E) to be consumed by the stack.
  if(terminatorp(readchar)) return NULL;
  expptr arg = mcread_arg();
  int p_right = precedence(readchar);
  while(!terminatorp(readchar)
	&& (p_left < p_right
	    || (p_left == p_right
		&& p_left < LEFT_THRESHOLD))){
    expptr op = mcread_connective();
    //at least one of arg and op must be non-null
    arg = pcons(pcons(arg,op),mcread_E(p_right));
    p_right = precedence(readchar);}
  return arg;
}

expptr mcread(){//this is called from top level read functions only
  advance_past_white();
  expptr arg = mcread_E(0);
  if(closep(readchar))declare_unmatched('-',arg,readchar);
  return arg ? arg : nil;
}

/** ========================================================================
cbreak, berror
========================================================================**/

void cbreak(){};

void send_emacs_tag(char * tag){
  fflush(stderr);
  fprintf(stdout,"%s",tag);
  fflush(stdout);
}

void send_print_tag(){send_emacs_tag(print_tag);}

int in_ide_proc(){return in_ide;}

void NIDE(){
  throw_error();
}

void breakpt(char *s){
  fprintf(stdout,"breakpt: %s\n",s);
  if(!in_ide)cbreak();
  else {
    send_emacs_tag(breakpoint_tag);
    cbreak();
    send_emacs_tag(request_input_tag);
  }
}

void berror(char *s){
  fprintf(stdout,"\n%s\n",s);
  if(in_ide){
    if(in_doit) send_emacs_tag(exec_error_tag);
    else send_emacs_tag(expansion_error_tag);}
  cbreak();
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
  request_input_tag = "*#*#dsflsadk#*#*IDE*#*#dsflsadk#*#*";
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
