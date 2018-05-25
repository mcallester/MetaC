#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <time.h>
#include <dlfcn.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <fcntl.h>
#include <string.h>
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
  if(dbg_freeptr > 0){
    fprintf(stderr,"in \n");
    pprint(dbg_stack[dbg_freeptr-1],stderr,0);}
  cbreak();
  throw_error();}

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
interning expressions

see mc.h for the definitions of types plist and expptr.
========================================================================**/

#define EXP_HASH_DIM  (1 << 24)
expptr exp_hash_table[EXP_HASH_DIM];
int exp_count;

int binaryp(char);

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

int symbolp(expptr e){return constructor(e) == 'a';}

int atomp(expptr e){
  if(e == NULL)return 1;
  char c = constructor(e);
  return (c == 'a' || c=='o' ||c == '\"' ||c == '\'');
}

int containsp(expptr e1, expptr e2){ //this is a utility used in REPL
  if(e1 == e2)return 1;
  if(atomp(e1))return 0;
  return (containsp(arg1(e1),e2) || containsp(arg2(e1),e2));
}

expptr make_app(expptr sym, expptr arg){
  return intern_exp('A', sym, intern_exp('(',arg,NULL));
}

expptr intern_exp_op(char c, expptr a1, expptr a2){
  ephemeral_buffer[0] = c;
  ephemeral_buffer[1] = 0;
  expptr op = intern_exp('o',(expptr) intern_string(ephemeral_buffer),NULL);
  return intern_exp('O',op,intern_exp(' ',a1,a2));
}

expptr op_arg1(expptr e){
  if(constructor(e) != 'O')berror("illegal call to op_arg1");
  return arg1(arg2(e));}

expptr op_arg2(expptr e){
  if(constructor(e) != 'O')berror("illegal call to op_arg2");
  return arg2(arg2(e));}

expptr string_symbol(char * s){
  return intern_exp('a', (expptr) intern_string(s), NULL);
}

char * symbol_string(expptr s){
  if(!(constructor(s) == 'a' || constructor(s) == 'o'))berror("attempt to convert non-symbol t string");
  return (char *) arg1(s);
}


/** ========================================================================
lists
======================================================================== **/

expptr cons(expptr x, expptr y){
  return intern_exp(' ',x,y);
}

expptr car(expptr x){
  return arg1(x);}

expptr cdr(expptr x){
  return arg2(x);}

expptr append(expptr l1, expptr l2){
  if(l1 == NULL)return l2;
  return cons(car(l1), append(cdr(l1),l2));
}

expptr reverse(expptr l){
  expptr result = NULL;
  while(l != NULL){
    result = cons(car(l), result);
    l = cdr(l);
  }
  return result;
}

expptr mapcar(expptr f(expptr), expptr l){
  if(l == NULL) return NULL;
  return cons(f(car(l)),mapcar(f,cdr(l)));
}

int length(expptr l){
  if(l == NULL || constructor(l) != ' ') return 0;
  return length(arg2(l)) + 1;
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


/** ========================================================================
character properties. The character '/n' has different properties
when reading from files vs. the terminal.


The character type tag on an expression has the following possible values:

'a' --- a symbol.  The first argument is an interned string.
a symbol tag (a unary character) --- a tagged symbol.
'A' --- an application expression. The first argument is a structured symbol and the second argument is a parenthesis expression.
'o' --- a binary connective. --- The first argument is an interned string.
'O' --- a connective expression.  arg1 is the oprtator and arg2 is a the pair of arguments.
' ' --- a pair expression.  This is used as the second argument to a connective expression and also for cons cells.
        When a pair expression is passed directly to the printer, as opposed to being the second argument of a binary connective expression,
        The printer prints the space as a binary connective.
        Space can also appear as the connective of a connective expression.
/',/" ---  a string expression.
(,{,[ ---  a parenthesis expression.
';' --- a semicolon terminated expression.
========================================================================**/

int string_quotep(char x){return (x == '"' || x == '\'');}

int unaryp(char c){
  return c == '!' || c == '$' || c == '#' || c == '`' ||c == '\\' || c == '%' || c == '?';
}

int binaryp(char c){
  return c == '*' || c == '/' || c == '+' || c == '-' || c == '.' || c == ':'
      || c == ',' || c == '<' || c == '=' ||c == '>' || c == '@' || c == '^' || c == '|' || c == '&' || c == '~';
}

int openp(char x){return ((x)=='(' || (x)=='{' || (x) == '[');}

int whitep(char x){return (x == ' ' || x == '\t' || x == '\n');}

int closep(char x){return ((x)==')' || (x)=='}' || (x) == ']');}

int terminatorp(char c){return (closep(c) || c == EOF || c == '\0' || c == ';');}

char close_for(char o){
  if(o == '(')return  ')';
  if(o == '{')return  '}';
  if(o == '[')return  ']';
  berror("not an open in close_for");
  return 'a';  // avoids compiler warning
}

int alphap(char c){
  return  (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || (c == '_');
}

int precedence(char op){
  if(op==',')return 2;
  if(op==' ')return 3;
  if(op=='=' || op=='<' || op=='>' || op =='~') return 4;
  if(op=='|' || op =='&')return 5;
  if(op=='+' || op=='-')return 6;
  if(op=='*' || op=='/')return 7;
  if(op == '^')return 8;
  if(op=='@')return 9;
  if(op=='.')return 10;
  if(op==':')return 12;
  berror("unknown character in precedence");
  return 1; //avoids compiler warning
}

#define LEFT_THRESHOLD 8
//operators with precedence >= LEFT_THRESHOLD are left associative.

int op_precedence(expptr e){
  if(e == NULL){return 0;}
  if(constructor(e) != 'o')berror("illegal call to op_precedence");
  char * s = (char *) arg1(e);
  if(s[1] == '\0')return precedence(s[0]);
  return 11;}

/** ========================================================================
preprocessing

advance_readchar yields a series of values of readchar with comments removed.

preprocessing REPL inputs behaves differently from preprocessing files.

Comments are removed from REPL inputs only in positions inside parantheses.

the "lookahead" slots next and next2 are active for file input and for terminal
input with paren_level > 0.

mcread_open maintains the invariant that when advance_readchar is called we have that paren_level is the level of the position immediately preceding read_stream->next
========================================================================**/
char readchar;

typedef struct wrapper{
  FILE * stream;
  char next;
  char next2;
  char quotechar;
  int paren_level;} wrapper, * wstream;

wstream file_stream;
wstream terminal_stream;
wstream read_stream;

int paren_level;

wstream cwrap_stream(FILE * f);
void advance_readchar();

void init_lookahead(wstream s){
  s->quotechar = 0;
  (s->paren_level) = 0;
  s->next = ' ';
  s->next2 = ' ';
}

void open_input_file(char * source){
  filein = fopen(source, "r");
  if(filein == NULL)berror("attempt to open input file failed");
  file_stream = cwrap_stream(filein);
}

void skipwhite(){while(whitep(readchar) && !(readchar == '\n' && read_stream == terminal_stream && paren_level == 0)){advance_readchar();}
}

void advance_readchar_past_white(){
  advance_readchar();
  skipwhite();
}

wstream cwrap_stream(FILE * s){
  wstream result = (wstream) malloc(sizeof(wrapper));
  result->stream = s;
  init_lookahead(result);
  return result;
}

void print_stream_state(){
  fputc('|',stderr);
  fputc((char) file_stream->next,stderr);
  fputc((char) file_stream->next2,stderr);
  fputc('|',stderr);
}

void advance_lookahead(){
  char nextchar;
  if(read_stream->next2 == EOF){nextchar = EOF;}
  else {nextchar = fgetc(read_stream->stream);}
  read_stream->next = read_stream->next2;
  read_stream->next2 = nextchar;
}

void advance_readchar(){
  //readchar is undefined on entry --- only read_stream->next and read_stream->next2 are defined.
  
  //the following removes comments and quoted returns.

  if(read_stream->quotechar == 0){
    while(1){
      if(read_stream->next == '/' && read_stream->next2 == '*'){
	if(read_stream == terminal_stream && paren_level == 0)
	  berror("comment from terminal outside of parens");
	while(!(read_stream->next == '*' && read_stream->next2 == '/')){
	  advance_lookahead();
	  if(read_stream->next2 == EOF)berror("end of file in comment");
	}
	advance_lookahead();
	advance_lookahead();}
      else if(read_stream->next == '/' && read_stream->next2 == '/'){
	if(read_stream == terminal_stream && paren_level == 0)
	  berror("comment from terminal outside of parens");
	while(read_stream->next != '\n'){
	  advance_lookahead();
	}}
      else if(read_stream->next == '\\'
	      && read_stream->next2 == '\n'
	      && read_stream->quotechar == 0){
	advance_lookahead();
	advance_lookahead();}
      else break;}}

  //file segmentation
  if(read_stream == file_stream
     && read_stream->quotechar == 0
     && read_stream->next == '\n'
     && !whitep(read_stream->next2)
     && !closep(read_stream->next2)){
       read_stream->next = '\0';}

  if(read_stream == terminal_stream
     && read_stream->quotechar == 0
     && read_stream->paren_level == 0){
    if(read_stream->next2 == '\n'){
      readchar = read_stream->next;
      read_stream->next = read_stream->next2;
      read_stream->next2 = ' ';
      return;}
    if(read_stream->next == '\n'){
      readchar = '\0';
      read_stream->next = read_stream->next2;;
      read_stream->next2 = ' ';
      return;}}

  //setting readchar
  readchar = read_stream->next;
  advance_lookahead();

  //maintaining the quotation flag
  if(string_quotep(read_stream->next)){
    if(read_stream->quotechar == 0){read_stream->quotechar = read_stream->next;}
    else if(readchar != '\\' && read_stream->next == read_stream->quotechar){read_stream->quotechar = 0;}}
}

/** ========================================================================
the reader on a preprocessed stream

We note that for any triple ... op1 e op2 ..., where op1 and op2 are binary connectives, the
priority of op1 and op2 determines whether e1 binds to the right or to the left.  A higher priority
operator binds. If the priority is the same we have e1 binds left for left-assiative priories and
binds right for right-associative priorities.

In mcread_tree(p1), p1 is the precedence of the connective to the left of readcar
or zero if there is no such connective

mcread_tree(p1) returns arg and op such that arg binds left.  The operator argument is returned in the variable readop.

The null connective is represented by a space connective and application of the space connective are represented by expressions tagged with 'O'.
This simplifies mcread_tree by allowing the null connective to be handled by general code for binary connectives.
The null connective prints as space. Having the space connective print as space guarantees that adjacent symbols remain separated in the printed string.

The space connective has priority higher than comma (and higher than the terminator symbol ';') but lower than all other binary connectives.  This causes friendly tree structures
for C argument lists and statement lists.

If there is no left operator then p1 == 0 which is weaker than all connectives other than the true null connective
inserted at terminators.  This causes mcread_tree(0) to return the tree between the current readchar and the next terminator.

Each of the following mcread functions can return NULL but if mcread_atom() is followed by mcread_connective() either one of
the two must be non-null or readchar is a terminator --- the initial readcar must contribute to one of these three cases.
==================================================================== **/

expptr mcread_tree(int p);
expptr mcread_atom();
expptr mcread_tagged();
expptr mcread_connective();
expptr mcread_string();
expptr mcread_open();
expptr mcread_symbol();
void declare_unmatched();
expptr readop;

expptr last_def;
char last_readchar;

expptr read_from(wstream s){
  read_stream = s;
  paren_level = 0;
  advance_readchar_past_white();   //readchar is undefined on entry.  This sets readchar.
  return mcread_tree(0);
  //on return readchar is either 0 or EOF and is ignored in future calls to read_from.
}

expptr read_from_terminal(){return read_from(terminal_stream);}

expptr read_from_file(){return read_from(file_stream);}

expptr mcread_tree(int p1){
  if(whitep(readchar))berror("whitespace visible to mcread_tree");
  expptr a1 = mcread_atom();
  expptr op2 = mcread_connective();
  int p2 = op_precedence(op2);
  while(p2 > p1 || (p2 == p1 && p1 != 0 && p1 < LEFT_THRESHOLD) || (p1 == 0 && readchar ==';')){
    if(p1 == 0 && readchar == ';'){ //op2 == NULL in this case
      a1 = intern_exp(';',a1,NULL);
      advance_readchar_past_white();
      op2 = mcread_connective();
      p2 = op_precedence(op2);}
    else {
      expptr a2 = mcread_tree(p2);
      a1 = intern_exp('O',op2,intern_exp(' ',a1,a2));
      op2 = readop;
      p2 = op_precedence(op2);}}
  readop = op2;
  return a1;
}

expptr mcread_atom(){
  if(string_quotep(readchar)){return mcread_string();}
  if(openp(readchar)){return mcread_open();}
  if(alphap(readchar) || unaryp(readchar)){
    expptr f = mcread_tagged();
    while(openp(readchar)){
      f = intern_exp('A',f,mcread_open());}
    return f;}
  return NULL;
}

expptr mcread_tagged(){
  if(unaryp(readchar)){
    char c = readchar;
    advance_readchar_past_white();
    if(alphap(readchar)) return intern_exp(c,mcread_symbol(),NULL);
    return intern_exp(c,NULL,NULL);}
  return mcread_symbol();
}
      
expptr mcread_symbol(){
  if(!alphap(readchar))return NULL;
  int i=0;
  while(alphap(readchar)){
    if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
    ephemeral_buffer[i++]=readchar;
    advance_readchar();}
  if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[i]=0;
  char * s  = intern_string(ephemeral_buffer);
  skipwhite();
  return intern_exp('a', (expptr) s, NULL);
}

expptr mcread_connective(){ //same as mcread_symbol but for strings of operator symbols.
  if(terminatorp(readchar) || (readchar == '\n' && read_stream == terminal_stream && paren_level == 0)) return NULL;
  if(!binaryp(readchar))return intern_exp('o',(expptr) intern_string(" "), NULL);
  int i=0;
  while(binaryp(readchar)){
    if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
    ephemeral_buffer[i++]=readchar;
    advance_readchar();}
  if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[i]=0;
  char * s  = intern_string(ephemeral_buffer);
  skipwhite();
  return intern_exp('o', (expptr) s, NULL);
}

expptr mcread_string(){ // readchar is either " or '
  int i = 0;
  char q = readchar;
  advance_readchar();
  int quoted = 0;
  while(1){
    if(readchar == q && !quoted)break;
    if(readchar == EOF || readchar == '\0' || readchar == '\n')berror("unterminated string constant");
    if(readchar == '\\' && !quoted) quoted = 1; else quoted = 0;
    if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
    ephemeral_buffer[i++]=readchar;
    advance_readchar();}
  if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[i]=0;
  advance_readchar_past_white();
  return intern_exp(q, (expptr) intern_string(ephemeral_buffer),NULL);
}

expptr last_parens;

void declare_unmatched(){
  fprintf(stderr,"unmatched parentheses after:\n\n");
  pprint(last_parens,stderr,0);
  if(read_stream == terminal_stream){
      while(readchar != '\n')readchar = fgetc(stdin);}
  else
    berror("");
}

expptr mcread_open(){ // readchar is openp
  char openchar = readchar;
  char closechar = close_for(openchar);

  paren_level++;
  advance_readchar_past_white();
  expptr e = mcread_tree(0);
  if(readchar != closechar)declare_unmatched();
  paren_level--;
  
  advance_readchar_past_white();
  expptr val = intern_exp(openchar,e,NULL);
  last_parens = val;
  return val;
}

/** ========================================================================
printing to a string

This is used in mcE for constructing system calls --- system calls take strings.
========================================================================**/

void putone(char c){
  if(stack_heap_freeptr == STACK_HEAP_DIM)berror("stack heap exhausted");
  stack_heap[stack_heap_freeptr++] = c;
}

void copy_to_stack_heap(char * s){
  for(int i = 0;s[i] != '\0';i++){
    putone(s[i]);}
}
  
void print_to_stack_heap(expptr w){
  if(w == NULL)return;
  char c = constructor(w);
  if(string_quotep(c)){
    putone(c);
    copy_to_stack_heap((char *) arg1(w));
    putone(c);}
  else if(c == 'a' || c == 'o'){
    copy_to_stack_heap((char *) arg1(w));}
  else if(openp(c)){
    putone(c);
    print_to_stack_heap(arg1(w));
    putone(close_for(c));}
  else if(c == 'A'){
    print_to_stack_heap(arg1(w));
    print_to_stack_heap(arg2(w));}
  else if(c == 'O'){
    print_to_stack_heap(arg1(arg2(w)));
    print_to_stack_heap(arg1(w)); //this might be the space character.
    print_to_stack_heap(arg2(arg2(w)));}
  else if(c == ';'){
    print_to_stack_heap(arg1(w));
    putone(';');}
  else if(unaryp(c)){
    putone(c);
    print_to_stack_heap(arg1(w));}
  else berror("unrecognized constructor in print_to_stack_heap");
}

char * exp_string(expptr e){
  char * result = (char *) &stack_heap[stack_heap_freeptr];
  print_to_stack_heap(e);
  putone('\0');
  return result;
}

/** ========================================================================
section: pretty printing
========================================================================**/

#define PRINT_WIDTH 50
expptr print_length_token; //has value `print_length
FILE * writestrm;

int plength(expptr);

int compute_plength(expptr w){
  if(w == NULL)return 0;
  char c = constructor(w);
  if(string_quotep(c)){return (2 + strlen((char *) arg1(w)));}
  if(c=='a' || c=='o'){return (strlen((char *) arg1(w)));} //
  if(c=='A'){return (plength(arg1(w)) + plength(arg2(w)));}
  if(c=='O'){return (plength(arg1(w)) + plength(arg2(w))-1);}
  if(c == ' '){return (plength(arg1(w)) + plength(arg2(w))+1);}
  if(openp(c)){ return (2 + plength(arg1(w)));}
  if(unaryp(c) || c == ';'){ return (1 + plength(arg1(w)));}
  berror("unknown expression type in compute_plength");
  return 1; //avoids compiler warning
}

int plength(expptr w){
  if(w == NULL)return 0;
  expptr negative_one = (expptr) ((long int) -1);
  expptr explength = getprop(w,print_length_token, negative_one);
  if(explength == negative_one){
    int intlength = compute_plength(w);
    addprop(w,print_length_token, (expptr) ((long int) intlength));
    return intlength;}
  return (int) explength;
}

void print_string(char * w){
  fprintf(writestrm, "%s",w);
}

void indent(int column){
  int i;
  for(i=0;i<column;i++)fprintf(writestrm, " ");
}

void newline(){
  fprintf(writestrm, "\n");
}

void print_exp_linear(expptr w){
  if(w == NULL)return;
  char c = constructor(w);
  if(string_quotep(c)){
    fputc(c,writestrm);
    print_string((char *) arg1(w));
    fputc(c,writestrm);}
  else if(c == 'a' || c == 'o'){
    print_string((char *) arg1(w));}
  else if(openp(c)){
    fputc(c,writestrm);
    print_exp_linear(arg1(w));
    fputc(close_for(c),writestrm);}
  else if(c == 'A'){
    print_exp_linear(arg1(w));
    print_exp_linear(arg2(w));}
  else if(c == 'O'){
    print_exp_linear(arg1(arg2(w)));
    print_exp_linear(arg1(w));
    print_exp_linear(arg2(arg2(w)));}
  else if(c == ' '){
    print_exp_linear(arg1(w));
    fputc(' ',writestrm);
    print_exp_linear(arg2(w));}
  else if(c == ';'){
    print_exp_linear(arg1(w));
    fputc(';',writestrm);}
  else if(unaryp(c)){
    fputc(c,writestrm);
    print_exp_linear(arg1(w));}
  else berror("unrecognized constructor in pprint_exp");
}

void pprint_exp(expptr w, int column){ //column is the current print column
  if(w == NULL)return;
  char c = constructor(w);
  if(string_quotep(c) || c == 'a' || c == 'o' || plength(w) <= PRINT_WIDTH){
    print_exp_linear(w);
    return;}
  else if(openp(c)){
    fputc(c,writestrm);
    pprint_exp(arg1(w),column+1);
    fputc(close_for(c),writestrm);}
  else if(c == 'A'){
    pprint_exp(arg1(w),column);
    pprint_exp(arg2(w),column);}
  else if(c == 'O'){
    pprint_exp(op_arg1(w), column);
    print_exp_linear(arg1(w));
    if(op_arg2(w)){
      newline();
      indent(column);
      pprint_exp(op_arg2(w), column);}}
  else if(c == ' '){
    pprint_exp(arg1(w), column);
    fputc(' ',writestrm);
    if(arg2(w)){
      newline();
      indent(column);
      pprint_exp(arg2(w), column);}}
  else if(c == ';'){
    pprint_exp(arg1(w), column);
    fputc(';',writestrm);}
  else if(unaryp(c)){
    fputc(c,writestrm);
    pprint_exp(arg1(w),column+1);}
  else berror("unrecognized constructor in pprint_exp");
}

void pprint(expptr w, FILE * f, int col){
  writestrm = f;
  indent(col);
  pprint_exp(w,col);
  fprintf(f,"\n\n");
}

void print_line(expptr w, FILE * f){
  writestrm = f;
  fprintf(f,"\n");
  print_exp_linear(w);
  fprintf(f,"\n");
}

void gud_pprint(expptr e){
  pprint(e,stdout,0);}


/** ========================================================================
section: backquote
========================================================================**/

expptr constructor_code(char c){
  if(c == '\''){return intern_exp('\'', (expptr) intern_string("\\'"), NULL);}
  if(c == '\\'){return intern_exp('\'', (expptr) intern_string("\\"), NULL);}
  sprintf(ephemeral_buffer,"%c",c);
  return intern_exp('\'', (expptr) intern_string(ephemeral_buffer) , NULL);
}

expptr intern_exp_code(char c, expptr a1, expptr a2){
  return intern_exp('A',
		    string_symbol("intern_exp"),
		    intern_exp('(',
			       intern_exp_op(',',
					  constructor_code(c),
					  intern_exp_op(',',a1,a2)),
			       NULL));
}

expptr intern_string_code(char * s){
  return intern_exp_op(' ',
		       intern_exp('(', string_symbol("expptr"), NULL),
		       make_app(string_symbol("intern_string"),
				intern_exp('"', (expptr) intern_string(s), NULL)));
}

expptr bquote_code(expptr e){
  if(e == NULL)return string_symbol("NULL");
  char c = constructor(e);
  if(c=='a' || c == 'o' || string_quotep(c))return intern_exp_code(c,intern_string_code((char *) arg1(e)),bquote_code(NULL));
  expptr a1 = arg1(e);
  expptr a2 = arg2(e);
  if(c=='$'){
    if(a1 != NULL && constructor(a1) == 'a'){return a1;}
    berror("Illegal syntax for $");}
  if(c=='\\'){
    if(a1 == NULL)berror("Illegal syntax for backslash in backquote");
    char c2 = constructor(a1);
    if(!unaryp(c2))berror("illegal syntax of '\' in backquote --- '\' must be followed by a unary operator such as '$' or itself");
    return intern_exp_code(c2, bquote_code(arg1(a1)), bquote_code(NULL));}
  if(c == 'A' && constructor(a1) == '$'){ // for const = 'A' both arguments are guaranteed to be non-null.
    if(arg1(a1) == NULL) return arg1(a2);
    berror("$ must be followed by curly braces");}
  return intern_exp_code(c,bquote_code(a1),bquote_code(a2));
}

expptr quote_code(expptr e){ // this is used in mcB but is not used for backquote
  if(e == NULL || constructor(e) == 'a' || constructor(e) == 'o' || string_quotep(constructor(e))) return bquote_code(e);
  return intern_exp_code(constructor(e),quote_code(arg1(e)),quote_code(arg2(e)));
}

expptr bquote_macro(expptr e){ //this is the non-recursive entry point (the macro procedure) for backquote
  if(constructor(arg2(e)) != '{')berror("syntax error: backquote must be followed by curly braces");
 return  bquote_code(arg1(arg2(e)));
}

/** ========================================================================
section: macroexpand
========================================================================**/
expptr macro_token; //has value `{macro}

void set_macro(expptr sym, expptr f(expptr)){
   setprop(sym, macro_token, (expptr) f);
}

void set_symbol_expansion(expptr sym, expptr expansion){
  setprop(sym, string_symbol("symbol_macro_expansion"), expansion);
}

expptr macroexpand(expptr);

expptr macroexpand_args(expptr e){
  if(e == NULL || atomp(e)) berror("illegal call to macroexpand_args");
  expptr a1 = macroexpand(arg1(e));
  expptr a2 = macroexpand(arg2(e));
  return intern_exp(constructor(e),a1,a2);
}

expptr macroexpand2(expptr name, expptr e){
  expptr (*f)(expptr);
  f = (expptr (*)(expptr)) getprop(name,macro_token,NULL);
  if(f == NULL){return e;}
  push_dbg_expression(e);
  expptr val = f(e);
  pop_dbg_stack();
  return val;
}

expptr top_symbol(expptr e){
  if(e == NULL)return NULL;
  char c  = constructor(e);
  if(c == 'a' || unaryp(c))return e;
  if(c == 'A')return top_symbol(arg1(e));
  if(c == 'O')return arg1(e);
  return NULL;
}
  
expptr macroexpand1(expptr e){
  if(atomp(e))return e;
  if(constructor(e) == 'a'){
    expptr expansion = getprop(e,string_symbol("symbol_macro_expansion"),NULL);
    if(expansion != NULL)return expansion;}
  expptr s = top_symbol(e);
  if(s == NULL)return e;
  return macroexpand2(s,e);
}

expptr macroexpand(expptr e){
  expptr e2 = macroexpand1(e);
  if(e2 != e) return macroexpand(e2);
  if(atomp(e))return e;
  return macroexpand_args(e);
}

/** ========================================================================
section: gensym
========================================================================**/

expptr int_exp(int i){
  if(i < 0)berror("attempt to convert negative integer to expression");
  sprintf(ephemeral_buffer,"%d",i);
  return string_symbol(ephemeral_buffer);
}

int symbol_int(expptr s){
  if(!symbolp(s)){berror("call to symbol_int on non-symbol");}
  return atoi(symbol_string(s));
}

expptr gensym(expptr sym){
  if(!symbolp(sym))berror("argument to gensym is not a symbol");
  char * s = symbol_string(sym);
  
  for(int i=1;1;i++){
    int length = snprintf(ephemeral_buffer,EPHEMERAL_DIM,"_mcgen_%s%d",s,i);
    if(length >= EPHEMERAL_DIM)berror("ephemeral buffer exauhsted");
    int key = preintern_string(ephemeral_buffer);
    if(string_hash_table[key]==NULL)break;}
  return string_symbol(ephemeral_buffer);
}

/** ========================================================================
section: file expansion
========================================================================**/

void process_def(expptr e);

void put_return(FILE * ws){
  fputc('\n',ws);}

void open_output_file(char * destination){
  fileout = fopen(destination,"w");
  if(fileout == NULL)berror("attempt to open output file failed");
}

void mcexpand(char * source, char * destination){
  open_input_file(source);
  open_output_file(destination);
  
  while(1){
    expptr e = read_from_file();
    if(e != NULL)process_def(e);
    if(readchar == EOF)break;
  }
  
  fclose(filein);
  fclose(fileout);
}

void add_preamble(expptr e){
  expptr e2 = macroexpand(e);
  preamble = append(preamble, cons(e2,NULL));}

    
void pprint_reversed(expptr exps){
  if(exps == NULL)return;
  pprint_reversed(cdr(exps));
  pprint(car(exps),fileout,0);
}

void process_def(expptr e){
  if(e==NULL)return;
  push_dbg_expression(e);
  preamble = NULL;
  expptr e2 = macroexpand(e);
  if(preamble != NULL){
    pprint(preamble,fileout,0);
    fputc('\n',fileout);}
  pprint(e2,fileout,0);
  pop_dbg_stack();
}

/** ========================================================================
section: initialization
========================================================================**/

void mcA_init(){
  init_stack_frames();
  init_undo_frames();
  init_strings();
  init_expressions();
  
  dbg_freeptr = 0;
  
  print_length_token = string_symbol("print_length");
  macro_token = string_symbol("macro");

  last_parens = NULL;
  
  readchar = ' ';
  catch_freeptr = 0;
  terminal_stream = cwrap_stream(stdin);
  commaop = intern_exp('o',(expptr) intern_string(","),NULL);
  colonop = intern_exp('o',(expptr) intern_string(":"),NULL);
  spaceop = intern_exp('o',(expptr) intern_string(" "),NULL);

  set_macro(intern_exp('`',NULL,NULL), bquote_macro);
}

