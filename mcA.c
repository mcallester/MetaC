#include "mc.h"

/** ========================================================================
See mc.h for catch, throw, catch-error, throw-error, and unwind_protect.
========================================================================**/


/** ========================================================================
cbreak, berror
========================================================================**/

void cbreak(){};

void push_dbg_expression(expptr e){
  if(dbg_freeptr == DBG_DIM)berror("debugging stack exhausted");
  dbg_stack[dbg_freeptr++] = e;
}

void pop_dbg_stack(){
  if(dbg_freeptr == 0)berror("attempt to pop empty dbg stack ");
  dbg_freeptr--;
}

void berror(char *s){
  fprintf(stderr,"%s\n",s);
  cbreak();
  throw_error();}

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
    fprintf(stderr,"undo heap exhausted\n");
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
character and atom types
========================================================================**/

int string_quotep(char x){return (x == '"' || x == '\'');}

int alphap(char c){
  return  (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || (c == '_');
}

int miscp(char c){
  return c == '!' || c == '$' || c == '#' || c == '`' ||c == '\\' || c == '?';
}

int connp(char c){
  return c == '*' || c == '/' || c == '+' || c == '-' || c == '.' || c == ':'
    || c == ',' || c == '<' || c == '=' ||c == '>' || c == '@' || c == '^'
    || c == '|' || c == '&' || c == '~' ||c ==';' || c == '%';
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

int terminatorp(char c){return (closep(c) || c == EOF || c == '\0');}

/** ========================================================================
interning expressions

see mc.h for the definitions of types plist and expptr.
========================================================================**/

#define EXP_HASH_DIM  (1 << 24)
expptr exp_hash_table[EXP_HASH_DIM];
int exp_count;

expptr intern_exp(char constr, expptr a1, expptr a2){
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

char * atom_string(expptr a){return (char *) arg1(a);}


expptr cons(expptr x, expptr y){return intern_exp(' ',x,y);}

int cellp(expptr e){return constructor(e) == ' ';}

expptr car(expptr x){return arg1(x);}

expptr cdr(expptr x){return arg2(x);}


expptr intern_paren(char openchar, expptr arg){return intern_exp(openchar, arg, NULL);}

int parenp(expptr e){return openp(constructor(e));}

expptr paren_inside(expptr e){return arg1(e);}


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

int length(expptr l){
  if(cellp(l))return length(arg2(l)) + 1;
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

char * exp_string(expptr e){
  char * s = &(stack_heap[stack_heap_freeptr]);
  putexp(e);
  putone('\0');
  return s;
}

void putone(char c){
  if(stack_heap_freeptr == STACK_HEAP_DIM)berror("stack heap exhausted");
  stack_heap[stack_heap_freeptr++] = c;
}

void putstring(char * s){
  for(int i = 0;s[i] != '\0';i++){
    putone(s[i]);}
}

void putexp(expptr w){
  if(atomp(w)){
    char * s = atom_string(w);
    putstring(s);
    if(connp(s[0]) || alphap(s[0])) putone(' ');}
  else if(parenp(w)){char c = constructor(w); putone(c); putexp(paren_inside(w));putone(close_for(c));}
  else if(cellp(w)){putexp(car(w)); putexp(cdr(w));}
}

/** ========================================================================
pprint
======================================================================== **/

FILE * pprint_stream;
int pprint_column;
expptr semi;
expptr comma;
#define COMMA_THRESHOLD 20

void writeone(char c){ fputc(c,pprint_stream);}

void writestring(char * w){
  fprintf(pprint_stream, "%s",w);
}

void newline(){
  fprintf(pprint_stream, "\n");
  for(int i=0;i<pprint_column;i++)fprintf(pprint_stream, " ");
}

void pprint_exp(expptr w){
  if(atomp(w)){
    char * s = atom_string(w);
    writestring(s);
    if(connp(s[0]) || alphap(s[0]))writeone(' ');
    if(w == semi || (w == comma && pprint_column >= COMMA_THRESHOLD))newline();}
  else if(parenp(w)){
    char c = constructor(w);
    writeone(c);
    pprint_column += 2;
    if(c == '\{') newline();
    pprint_exp(paren_inside(w));
    writeone(close_for(c));
    pprint_column -= 2;
    if(c == '\{')newline();}
  else if(cellp(w)){pprint_exp(car(w)); pprint_exp(cdr(w));}
}

void pprint(expptr w, FILE * f, int col){
  pprint_stream = f;
  pprint_column = col;
  newline();
  pprint_exp(w);
  fprintf(f,"\n");
}

void printexp(expptr e){
  pprint(e,stdout,rep_column);}

/** ========================================================================
section: backquote
========================================================================**/

expptr app_code(char * proc, expptr arg_code){
  return cons(string_atom(proc), intern_paren('(',arg_code));}

expptr comma_code(expptr arg1_code, expptr arg2_code){
  return cons(arg1_code, cons(string_atom(","), arg2_code));
}

expptr app_code2(char * proc, expptr arg1_code, expptr arg2_code){
  return app_code(proc, comma_code(arg1_code,arg2_code));
}

expptr constructor_code(char c){  //this is only used for the three open paren characters
  sprintf(ephemeral_buffer,"'%c'",c);
  return app_code("string_atom",string_atom(ephemeral_buffer));
}

expptr atom_quote_code(expptr a){
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

expptr quote_code(expptr e){
  if(atomp(e))return atom_quote_code(e);
  else if(parenp(e))return app_code2("intern_paren",constructor_code(constructor(e)),quote_code(paren_inside(e)));
  else return app_code("cons",comma_code(quote_code(car(e)),quote_code(cdr(e))));
}

expptr backslash;
expptr dollar;
expptr backslash_code(expptr);

expptr bquote_code(expptr e){
  if(atomp(e))return atom_quote_code(e);
  if(parenp(e))return app_code2("intern_paren",constructor_code(constructor(e)),bquote_code(paren_inside(e)));
  if(car(e) == dollar && parenp(cdr(e))){return paren_inside(cdr(e));}
  if(car(e) == backslash)return backslash_code(cdr(e));
  return app_code("cons",comma_code(quote_code(car(e)),quote_code(cdr(e))));
}

expptr backslash_code(expptr e){
  if(cellp(e) && car(e) == backslash)return app_code2("cons", atom_quote_code(backslash), backslash_code(cdr(e)));
  if(cellp(e) && car(e) == dollar)return app_code2("cons", atom_quote_code(dollar), bquote_code(cdr(e)));
  return bquote_code(e);
}

expptr bquote_macro(expptr e){ //this is the non-recursive entry point (the macro procedure) for backquote
  if(!parenp(cdr(e)))berror("syntax error: backquote must be followed by parentheses");
 return bquote_code(paren_inside(cdr(e)));
}

/** ========================================================================
section: macroexpand
========================================================================**/

expptr macro_token; //has value `{macro}

void set_macro(expptr sym, expptr f(expptr)){
   setprop(sym, macro_token, (expptr) f);
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
  f = (expptr (*)(expptr)) getprop(s,macro_token,NULL);
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
section: read_from_terminal, mcexpand, and file_expressions
========================================================================**/

int from_terminal;
char readchar;
char next;
char next2;
int paren_level;

expptr mcread();;

void init_readvars(){
  paren_level = 0;
  readchar = '\0';
  next = ' ';
  next2 = ' ';
}

FILE * read_stream;

expptr read_from_terminal(){
  init_readvars();
  from_terminal = 1;
  read_stream = stdin;
  return mcread();
}

expptr file_expressions2();

expptr file_expressions(char * fname){
  init_readvars();
  from_terminal = 0;
  read_stream = fopen(fname, "r");
  if(read_stream == NULL)berror("attempt to open input file failed");
  expptr exps = file_expressions2();
  fclose(filein);
  return exps;
}

expptr file_expressions2(){
  if(readchar == EOF)return nil;
  expptr e = mcread();
  return cons(e, file_expressions2());
}

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

char quotechar; //this is internal to preprocessing, mcread_string uses its own flag.


void advance_readchar();

void simple_advance(){
  //This is used when reading multi-character atom strings and as the base case in advance_readchar
  //we must still prevent reading past the terminating return when reading from the terminal

  if(from_terminal && next2 == '\n'){
    //paren_level is for the position between readchar and next.
    //we want the paren level between next and next2.
    int next_level = openp(next)? paren_level+1 : closep(next)? paren_level-1 : paren_level;
    if(next_level == 0)next2 = '\0';}
  readchar = next;
  next = next2;
  next2 = (next2 == EOF || (from_terminal && next2 == '\0'))? next2 : fgetc(read_stream);
}

void skip_comment1(){
  if(from_terminal && paren_level == 0)berror("comment from terminal outside of parens");
  while(!(next == '*' && next2 == '/')){
    simple_advance();
    if(next2 == EOF)berror("end of file in comment");}
  advance_readchar();
  advance_readchar();
}

void skip_comment2(){
  if(from_terminal && paren_level == 0)berror("comment from terminal outside of parens");
  while(next != '\n')simple_advance();
}
  
void advance_readchar(){
  //skip comments
  if(next == '/' && next2 == '*'){skip_comment1(); advance_readchar();return;}
  if(next == '/' && next2 == '/'){skip_comment2(); advance_readchar();return;}
  //advance past quoted return --- needed for parsing #define.
  if(!from_terminal && next == '\\' && next2 == '\n'){
    simple_advance();
    simple_advance();
    advance_readchar();return;}
  //file segmentation
  if(!from_terminal && next == '\n' && !whitep(next2) && !closep(next2)){next = '\0';}
  simple_advance();
  if(whitep(readchar))advance_readchar();
}

/** ========================================================================
Lexicalization and parethesis expressions
======================================================================== **/
int ephemeral_freeptr;

void ephemeral_putc(char c){
  if(ephemeral_freeptr == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[ephemeral_freeptr++]=c;
}
  
expptr mcread_symbol(){
  if(!alphap(readchar))berror("mcread_symbol called on non-alphap character");
  ephemeral_freeptr = 0;
  while(1){
    ephemeral_putc(readchar);
    if(!alphap(next))break;
    simple_advance();}
  ephemeral_putc('\0');
  advance_readchar(); //does not use ephemeral buffer
  return string_atom(ephemeral_buffer);
}

expptr mcread_connective(){
  if(!connp(readchar))berror("mcread_conn called on non-connp character");
  ephemeral_freeptr = 0;
  while(1){
    ephemeral_putc(readchar);
    if(!connp(next))break;
    simple_advance();}
  ephemeral_putc('\0');
  advance_readchar(); //does not use ephemeral buffer
  return string_atom(ephemeral_buffer);
}

expptr mcread_string(){
  if(!string_quotep(readchar))berror("mcread_string called on non-quotep");
  //we must prevent an unterminated string from swalling all further input.
  //we solve this by not allowing return characters in strings.
  char q = readchar; //remember the quote character
  ephemeral_freeptr = 0;
  ephemeral_putc(q);
  simple_advance();
  int quoted = 0;
  while(1){
    if(readchar == EOF || readchar == '\0' || readchar == '\n')berror("unterminated string constant");
    ephemeral_putc(readchar);
    if(readchar == q && quoted == 0)break;
    if(readchar == '\\' && !quoted) quoted = 1; else quoted = 0;
    simple_advance();}
  ephemeral_putc('\0');
  advance_readchar(); //does not use ephemeral buffer
  return string_atom(ephemeral_buffer);
}

expptr mcread_misc(){
  if(!miscp(readchar))berror("mcread_misc called on non-miscp");
  ephemeral_buffer[0] = readchar;
  ephemeral_buffer[1] = '\0';
  advance_readchar(); //does not use ephemeral buffer
  return string_atom(ephemeral_buffer);
}

void declare_unmatched(char, expptr, char);

expptr mcread_open(){
  if(!openp(readchar))berror("mcread_open called on non-open character");
  char c = readchar;
  char cl = close_for(c);
  paren_level++;
  expptr e = mcread();
  if(readchar != cl)declare_unmatched(c,e,cl);
  paren_level--;
  advance_readchar();
  return intern_paren(c,e);
}

void declare_unmatched(char openchar, expptr e, char closechar){
  fprintf(stderr,"unmatched parentheses %c\n",openchar);
  pprint(e,stderr,rep_column);
  fprintf(stderr, "%c\n", closechar);
  berror("");
}

/** ========================================================================
Deterministic shift-reduce parsing with phantom arguments.
======================================================================== **/

expptr pcons(expptr x, expptr y){return x? (y? cons(x,y) : x) : y;}

expptr mcread_gatom(){
  if(string_quotep(readchar))return mcread_string();
  if(alphap(readchar))return mcread_symbol();
  if(openp(readchar))return mcread_open();
  if(miscp(readchar)){expptr tag = mcread_misc(); return pcons(tag, mcread_gatom());}
  return NULL;
}

expptr mcread_arg(){
  expptr gatom = mcread_gatom();
  if(!gatom){return NULL;}
  return pcons(gatom,mcread_arg());
}

int precedence(char c){
  if(terminatorp(c))return 0;
  if(c==';')return 1;
  if(c==',')return 2;
   if(c=='^')return 3;
  if(c=='|' || c =='&')return 4;
  if(c=='=' || c=='<' || c=='>' || c =='~') return 5;
  if(c=='+' || c=='-')return 6;
  if(c=='*' || c=='/')return 7;
  if(c == '%')return 8;
  if(c=='@' || c=='.' || c==':')return 9;
  berror("unknown character in precedence");
  return 1; //avoids compiler warning
}

#define LEFT_THRESHOLD 9

expptr mcread_E(int p_left){
  //The stack (held on the C stack) ends in a consumer (open paren or connective) with precedence p_left
  //This returns a (possibly phantom) general expression (category E) to be consumed by the stack.
  expptr arg = mcread_arg();
  if(terminatorp(readchar) || closep(readchar)) return arg;
  int p_right = precedence(readchar);
  while(p_left < p_right || (p_left == p_right && p_left < LEFT_THRESHOLD)){
    expptr op = mcread_connective();
    arg = pcons(pcons(arg,op),mcread_E(p_right));
    p_right = precedence(readchar);
  }
  return arg;
}

expptr mcread(){//This does not use readchar but assumes that next and next2 are defined.
  if(!(readchar == '\0' || openp(readchar)))berror("readchar is not connp or '\0' in mcread");
  advance_readchar();
  expptr arg = mcread_E(0);
  return arg ? arg : nil;
}

/** ========================================================================
section: initialization
========================================================================**/

void mcA_init(){
  init_stack_frames();
  init_undo_frames();
  init_strings();
  init_expressions();

  gensym_count = 1;
  dbg_freeptr = 0;
  catch_freeptr = 0;

  nil = string_atom("");
  semi = string_atom(";");
  dollar = string_atom("$");
  backslash = string_atom("//");
  macro_token = string_atom("macro");
  comma = string_atom(",");
  
  set_macro(string_atom("`"), bquote_macro);
}
