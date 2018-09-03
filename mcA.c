#include "mc.h"

/** ========================================================================
See mc.h for catch, throw, catch-error, throw-error, and unwind_protect.
========================================================================**/

/** ========================================================================
the following flags are primarily used in the reader but also used in a
minor way in berror below and in the printer.
======================================================================== **/

void init_source(){
  in_repl = 0;
  in_expand = 0;
  in_ide = 0;
  in_doit = 0;
}

void init_tags(){
  ignore_tag = "*#*#dsflsadk#*#*ignore*#*#dsflsadk#*#*";
  result_tag = "*#*#dsflsadk#*#*result*#*#dsflsadk#*#*";
  comp_error_tag = "*#*#dsflsadk#*#*comp-error*#*#dsflsadk#*#*";
  exec_error_tag = "*#*#dsflsadk#*#*exec-error*#*#dsflsadk#*#*";
  breakpoint_tag = "*#*#dsflsadk#*#*breakpoint*#*#dsflsadk#*#*";
  ide_tag = "*#*#dsflsadk#*#*IDE*#*#dsflsadk#*#*";
  print_tag = "*#*#dsflsadk#*#*print*#*#dsflsadk#*#*";
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

void breakpt(char *s){
  fprintf(stdout,"%s\n",s);
  if(!in_ide)cbreak();
  else {
    send_emacs_tag(breakpoint_tag);
    cbreak();
    send_emacs_tag(ide_tag);
  }
}

void berror(char *s){
  fprintf(stdout,"\n%s\n",s);
  if(!in_ide){cbreak(); throw_error();}
  else if(in_doit){
    send_emacs_tag(exec_error_tag);
    cbreak();
    send_emacs_tag(ide_tag);
    throw_error();}
  else {
    send_emacs_tag(comp_error_tag);
    throw_error();}
}

void uerror(expptr e){
  printexp(e);
  berror("");
}

/** ========================================================================
push_stack_frame, pop_stack_frame, and stack_alloc
========================================================================**/

void * stack_alloc(int size){
  if(stack_heap_freeptr + size > STACK_HEAP_DIM)berror("stack heap exhausted");
  char * result = &stack_heap[stack_heap_freeptr];
  stack_heap_freeptr += size;
  return result;
}

void push_stack_frame(){
  if(stack_frame_count >= STACK_DIM)berror("MC stack exhausted");
  stack_restore[stack_frame_count++] = stack_heap_freeptr;
}

void push_stack_frame2(void * frame){
  stack[stack_frame_count] = frame;
  push_stack_frame();
}

void pop_stack_frame(){
  if(stack_frame_count == 0)berror("attempt to pop base stack frame");
  stack_heap_freeptr = stack_restore[--stack_frame_count];
}

void * current_frame(){
  if(stack_frame_count == 0) return NULL;
  return stack[stack_frame_count-1];
}
				 
void init_stack_frames(){
  stack_heap_freeptr = 0;
  stack_frame_count = 0;
}

/** ========================================================================
undo
========================================================================**/
#define UNDO_HEAP_DIM (1<<26)
char undo_heap[UNDO_HEAP_DIM];
int undo_heap_freeptr;
float heap_reserve;

void * undo_alloc(int size){
  if(undo_heap_freeptr + size > UNDO_HEAP_DIM*(1-heap_reserve)){
    fprintf(stdout,"undo heap exhausted\n");
    heap_reserve = heap_reserve/2;
    cbreak();
    throw_error();}
  char * result = &undo_heap[undo_heap_freeptr];
  undo_heap_freeptr += size;
  return result;
}

typedef struct freeptr_frame{
  int undo_trail_freeptr;
  int undo_heap_freeptr;
}freeptr_frame;

#define UNDO_RESTORE_DIM (1<<10)
freeptr_frame undo_restore[UNDO_RESTORE_DIM];
int undo_restore_freeptr;


void push_undo_frame(){
  if(undo_restore_freeptr >= UNDO_RESTORE_DIM)berror("undo freeptr stack exhausted");
  undo_restore[undo_restore_freeptr].undo_trail_freeptr = undo_trail_freeptr;
  undo_restore[undo_restore_freeptr].undo_heap_freeptr = undo_heap_freeptr;
  undo_restore_freeptr++;
}

void pop_undo_frame(){
  if(undo_restore_freeptr == 0)berror("attempt to pop base undo frame");

  undo_restore_freeptr--;
  int old_trail_freeptr = undo_restore[undo_restore_freeptr].undo_trail_freeptr;
  while(undo_trail_freeptr != old_trail_freeptr){
    undo_trail_freeptr--;
    *(undo_trail[undo_trail_freeptr].location) = (void *) undo_trail[undo_trail_freeptr].oldval;
  }

  undo_heap_freeptr = undo_restore[undo_restore_freeptr].undo_heap_freeptr;
}

void init_undo_frames(){
  undo_heap_freeptr = 0;
  undo_restore_freeptr = 0;
  undo_trail_freeptr = 0;
  heap_reserve = .1;
}

/** ========================================================================
Interning strings see also mc.h
========================================================================**/

#define STRING_HASH_DIM 10000
char * string_hash_table[STRING_HASH_DIM];
int string_count;

char * copy_string_to_undo(char *s){
  char * s2 = undo_alloc(strlen(s) + 1);
  strcpy(s2,s);
  return s2;
}

int preintern_string(char * s){
  int i, key;

  key=0;
  for(i=0;s[i] != 0;i++){
    key = (1458*key + s[i]);
  }
  key = key&(STRING_HASH_DIM-1);

  while(string_hash_table[key] != NULL
	&& strcmp(string_hash_table[key], s) != 0){
    key++;
    if(key==STRING_HASH_DIM)key=0;
  }
  return key;
}

char * intern_string(char * s){
  int key = preintern_string(s);
  if(string_hash_table[key]==NULL){
    if(string_count >= (2*STRING_HASH_DIM)/3)berror("string hash table exhausted");
    undo_set(string_hash_table[key],copy_string_to_undo(s));
    string_count++;
  }
  return string_hash_table[key];
}

void init_strings(){
  string_count = 0;
  for(int i=0;i<STRING_HASH_DIM;i++)string_hash_table[i]=NULL;
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
interning expressions

see mc.h for the definitions of types plist and expptr.
========================================================================**/

#define EXP_HASH_DIM  (1 << 24)
expptr exp_hash_table[EXP_HASH_DIM];
int exp_count;

expptr intern_exp(char constr, expptr a1, expptr a2){
  if(constr == '\0')berror("bad constructuctor in tern_exp");
  expptr e;
  unsigned int j = (constr + 729*((long int) a1) + 125*((long int) a2)) & EXP_HASH_DIM-1;
  expptr oldexp;
  for(int i = j;1;i++){
    if(i == EXP_HASH_DIM)i=0;
    oldexp = exp_hash_table[i];
    if(oldexp == NULL){
      if(exp_count >= (2*EXP_HASH_DIM)/3)berror("expression heap exhausted");
      exp_count++;
      expptr newexp = (expptr) undo_alloc(sizeof(expstruct));
      newexp->data = NULL;
      newexp->constructor = constr;
      newexp->arg1 = a1;
      newexp->arg2 = a2;
      undo_set(exp_hash_table[i],newexp);
      return newexp;
    }else{
      if(oldexp -> constructor == constr && oldexp->arg1 == a1 && oldexp-> arg2 == a2)return oldexp;
    }
  }
}

void init_expressions(){
  exp_count = 0;
  for(int i=0;i<EXP_HASH_DIM;i++)exp_hash_table[i] = NULL;
}

expptr string_atom(char * s){return intern_exp('A', (expptr) intern_string(s), NULL);}

int atomp(expptr e){return constructor(e) == 'A';}

int symbolp(expptr e){return atomp(e) && alphap(atom_string(e)[0]);}

char * atom_string(expptr a){
  if(constructor(a) != 'A'){
    berror("attempt to get string of non-atom");}
  return (char *) a->arg1;}

expptr cons(expptr x, expptr y){
  if(!x || !y)berror("null argument given to cons");
  return intern_exp(' ',x,y);}

int cellp(expptr e){return constructor(e) == ' ';}

expptr car(expptr x){
  if(constructor(x) != ' '){
    berror("taking car of non-cell");}
  return x->arg1;}

expptr cdr(expptr x){
  if(constructor(x) != ' '){
    berror("taking cdr of non-cell");}
  return x->arg2;}

expptr intern_paren(char openchar, expptr arg){
  if(!arg)berror("null argument given to intern_paren");
  return intern_exp(openchar, arg, NULL);}

int parenp(expptr e){return openp(constructor(e));}

expptr paren_inside(expptr e){
  if(!openp(constructor(e)))berror("paren_inside applied to non-paren");
  return e->arg1;}


/** ========================================================================
common expression constants
======================================================================== **/

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



/** ========================================================================
list operations
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
properties  see mc.h for the definition of the type plist
========================================================================**/

plist getprop_cell(expptr e, expptr key){
  for(plist p = data(e); p!= NULL; p=p->rest){
    if(p->key == key) return p;}
  return NULL;
}

expptr getprop(expptr e, expptr key, expptr defaultval){
  if(e == NULL)berror("attempt to get a property of the null expression");
  plist p = getprop_cell(e, key);
  if(p == NULL)return defaultval;
  return p -> data;
}

void addprop(expptr e, expptr key, expptr val){
  if(e == NULL)berror("attempt to add a property of the null expression");
  plist new = (plist) undo_alloc(sizeof(pliststruct));
  new->key = key;
  new->data = val;
  new->rest = e->data;
  undo_set(e-> data,new);
}

void setprop(expptr e, expptr key, expptr val){
  if(e == NULL)berror("attempt to set a property of the null expression");
  plist cell = getprop_cell(e, key);
  if(cell != NULL){
    undo_set(cell->data,val);
    return;}
  addprop(e,key,val);
}

void setprop_int(expptr e, expptr key, int x){
  char buffer[8]; //buffer is a pointer.
  int * y = (int *) buffer;
  *y = x;
  setprop(e,key, * (expptr *) buffer);
}

int getprop_int(expptr e, expptr key, int defaultval){
  if(e == NULL)berror("attempt to get a property of the null expression");
  plist p = getprop_cell(e, key);
  if(p == NULL)return defaultval;
  int * y = (int *) &(p -> data);
  return *y;
}

/** ========================================================================
printing: exp_string
======================================================================== **/
void putexp(expptr);
void putone(char);

char print_lastchar;

char * exp_string(expptr e){
  char * s = &(stack_heap[stack_heap_freeptr]);
  print_lastchar = '\0';
  putexp(e);
  putone('\0');
  return s;
}

void putone(char c){
  if(stack_heap_freeptr == STACK_HEAP_DIM)berror("stack heap exhausted");
  stack_heap[stack_heap_freeptr++] = c;
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

void printexp(expptr e){
  pprint(e,stdout,rep_column);}

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

expptr atom_quote_code(expptr a){
  if(a == backslash){
    return app_code("string_atom",string_atom("\"\\\\\""));}
  char  * s = atom_string(a);
  int eph_freeptr = 0;
  ephemeral_buffer[eph_freeptr++] = '"';
  if(string_quotep(s[0])) ephemeral_buffer[eph_freeptr++] = '\\';
  for(int i = 0; s[i] != '\0'; i++)ephemeral_buffer[eph_freeptr++]=s[i];
  if(string_quotep(s[0])){
    ephemeral_buffer[eph_freeptr-1] = '\\';
    ephemeral_buffer[eph_freeptr++] = s[0];}
  ephemeral_buffer[eph_freeptr++] = '"';
  ephemeral_buffer[eph_freeptr++] = '\0';
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

expptr macroexpand1(expptr);

expptr macroexpand(expptr e){
  if(atomp(e)) return e;
  expptr e2 = macroexpand1(e);
  if(e2 != e) return macroexpand(e2);
  if(parenp(e)) return intern_paren(constructor(e),macroexpand(paren_inside(e)));
  return intern_exp(constructor (e),macroexpand(car(e)),macroexpand(cdr(e)));
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

/** ========================================================================
Preamble and init_forms can be added during macro expansion
======================================================================== **/

void add_init_form(expptr form){
  expptr form2 = macroexpand(form); //this can recursively add init_forms prior to the following.
  init_forms = append(init_forms,cons(intern_paren('{',form2),nil));
}

void add_preamble(expptr e){
  expptr e2 = macroexpand(e); //this macro expansion can add to the preamble first.
  preamble = append(preamble, cons(e2,NULL));}

expptr full_expansion(expptr e){
  preamble = NULL;
  init_forms = NULL;
  expptr e2 = macroexpand(e);
  return append(preamble,cons(e2,init_forms));
}

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
    int key = preintern_string(ephemeral_buffer);
    if(string_hash_table[key]==NULL)break;}
  return string_atom(ephemeral_buffer);
}

/** ========================================================================
read__from_repl and read_from_ide
========================================================================**/

int from_repl;
int from_file;
int from_ide;

char readchar;
char next;
int paren_level; // this is the paren_lever immediately after readchar.

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

void simple_advance(){
  readchar = next;
  if(!(next == EOF || next == '\0'))next = fgetc(read_stream);
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
  if(read_stream == NULL)berror("attempt to open input file failed");
  expptr exps = file_expressions2();
  fclose(read_stream);
  return exps;
}

expptr file_expressions2(){
  if(readchar == EOF)return nil;
  if(closep(readchar))berror("file contains unmatched close\n");
  expptr e = mcread();
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
    if(from_repl && paren_level == 0)berror("comment from REPL outside of parens");
    while(!(readchar == '*' && next == '/')){if(endp(readchar))return; simple_advance();}
    simple_advance();
    readchar = ' ';}

  else if(readchar == '/' && next == '/'){
    //replace comment with white space
    if(from_repl && paren_level == 0)berror("comment from REPL outside of parens");
    while(readchar != '\n' && !endp(readchar))simple_advance();}

  else if(readchar == '\\' && next == '\n'){
    //advance past quoted return --- needed for parsing #define from a file.
    simple_advance(); advance_readchar();}

  else if(from_file && readchar == '\n' && !whitep(next) && !closep(next)){
    //file segmentation
    readchar = '\0';}

  else{
    //REPL termination
    if(from_repl && next == '\n' && paren_level == 0)readchar = '\0';}
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

int ephemeral_freeptr;

void ephemeral_putc(char c){
  if(ephemeral_freeptr == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[ephemeral_freeptr++]=c;
}
  
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
      berror("unterminated string constant");
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
  berror("");
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
section: initialization
========================================================================**/

void mcA_init(){
  init_source();
  init_tags();
  init_stack_frames();
  init_undo_frames();
  init_strings();
  init_expressions();
  init_exp_constants();
  
  gensym_count = 1;
  dbg_freeptr = 0;
  catch_freeptr = 0;
  in_doit = 0; //this controlled in the IDE and not touched in the REPL or file_expressions.
  
  set_macro(backquote, bquote_macro);
}
