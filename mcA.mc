#include "mc.h"

/** ========================================================================
See mc.h for catch, throw, catch-error, throw-error, and unwind_protect.
========================================================================**/


/** ========================================================================
cbreak, berror
========================================================================**/

void cbreak(){};

int break_active;

void activate_break(){break_active = 1;}

void deactivate_break(){break_active = 0;}

void act_break(){
  if(break_active)cbreak();
}

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

expptr intern_expA(char * s, expptr arg2){
  return intern_exp('A', (expptr) intern_string(s), arg2);
}

expptr string_exp(char * s){
 return intern_expA(s,NULL);
}

expptr intern_expB(expptr arg1, char * s){
  return intern_exp('B', arg1 (expptr) intern_string(s));
}

expptr intern_exp_op( expptr a1, (char *) s, expptr a2){
  return intern_exp(' ',intern_expB(a1, intern_string(s)), a2);
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
printing
========================================================================**/

int prefixp(char c){
  return c == '!' || c == '$' || c == '#' || c == '`' ||c == '\\' || c == '%' || c == '?';
}

int closep(char x){return ((x)==')' || (x)=='}' || (x) == ']');}

char close_for(char o){
  if(o == '(')return  ')';
  if(o == '{')return  '}';
  if(o == '[')return  ']';
  berror("not an open in close_for");
  return 'a';  // avoids compiler warning
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
  if(w == NULL)return;
  char c = constructor(w);
  if(c == 'A'){putstring((char *)arg1(w)); putone(' '); putexp(arg2(w));}
  else if(c == 'B'){ putexp(arg1(w)); putstring((char *)arg2(w));}
  else if(c == ' '){putexp(arg1(w)); putexp(arg2(w));}
  else if(prefixp(c)){putone(c); putexp(arg1(w)); putexp(arg2(w));}
  else if(openp(c)){putone(c);putexp(arg1(w));putone(close_for(w));putexp(arg2(w));}
  else berror("unrecognized constructor in putexp");
}

char * exp_string(expptr e){
  char * result = &stack_heap[stack_heap_freeptr];
  print_to_stack_heap(e);
  putone('\0');
  return result;
}

FILE * writestrm;

void writeone(char c){ fputc(c,writestrm);}

void writestring(char * w){
  fprintf(writestrm, "%s",w);
}

void writeexp(expptr w){
  if(w == NULL)return;
  char c = constructor(w);
  if(c == 'A'){writestring((char *)arg1(w)); writeone(' '); writeexp(arg2(w));}
  else if(c == 'B'){ writeexp(arg1(w)); writestring((char *)arg2(w));}
  else if(c == ' '){writeexp(arg1(w)); writeexp(arg2(w));}
  else if(prefixp(c)){writeone(c); writeexp(arg1(w)); writeexp(arg2(w));}
  else if(openp(c)){writeone(c);writeexp(arg1(w));writeone(close_for(w));writeexp(arg2(w));}
  else berror("unrecognized constructor in writeexp");
}

void indent(int column){
  int i;
  for(i=0;i<column;i++)fprintf(writestrm, " ");
}

void newline(){
  fprintf(writestrm, "\n");
}

void pprint_exp(expptr w, int column){ //column is the current print column
  if(w == NULL)return;
  char c = constructor(w);
  if(c == A){writestring((car *)arg1(w));writeone(' '); writeexp(arg2(w),column);}
  else if(c == B){
    pprint_exp(arg1(w),column);
    s = (char *) arg2(x);
    writestring(s);
    if(s[0] == ';' && s[1] = 0){
      newline();
      indent(column);
      pprint_exp(arg2(w),column);
    }}
  else if(c == ' '){pprint_exp(arg1(w), column); writeexp(arg2(w),column);}
  else if(prefixp(c)){writeone(c); pprint_exp(arg1(w),column); pprint_exp(arg2(w),column);}
  else if(openp(c)){
    newline();
    indent(column);
    writeone(c);
    indent(2);
    pprint_exp(arg1(w),column+3);
    writeone(close_for(c));
    newline();
    indendent(column);
    pprint_exp(arg2(w),column);}
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
  writeexp(w);
  fprintf(f,"\n");
}

void printexp(expptr e){
  pprint(e,stdout,rep_column);}


/** ========================================================================
section: backquote
========================================================================**/

expptr constructor_code(char c){
  if(c == '\''){return intern_exp('\'', (expptr) intern_string("\\'"), NULL);}
  if(c == '\\'){return intern_exp('\'', (expptr) intern_string("\\"), NULL);}
  sprintf(ephemeral_buffer,"'%c'",c);
  return intern_exp('\'', (expptr) intern_string(ephemeral_buffer) , NULL);
}

expptr intern_exp_code(char c, expptr a1, expptr a2){
  return intern_exp('A',
		    string_exp("intern_exp"),
		    intern_exp('(',
			       intern_exp_op(constructor_code(c) ,",", intern_exp_op(a1 ,",", a2)),
			       NULL));
}

expptr make_app(car * s, expptr arg){
  return intern_exp('A', sym, intern_exp('(',arg,NULL));
}

expptr intern_string_code(char * s){
  return intern_exp_op(intern_exp('(', string_exp("expptr"), NULL),
		       " ",
		       make_app("intern_string",
				intern_exp('"', (expptr) intern_string(s), NULL)));
}

expptr bquote_code(expptr e){
  if(e == NULL)return string_exp("NULL");
  char c = constructor(e);
  if(c=='a' || c == 'o' || string_quotep(c))return intern_exp_code(c,intern_string_code((char *) arg1(e)),bquote_code(NULL));
  expptr a1 = arg1(e);
  expptr a2 = arg2(e);
  if(c =='$'){}
  if(c =='$'){berror("Illegal syntax for $");}
  if(c=='\\'){
    if(a1 == NULL)berror("Illegal syntax for backslash in backquote");
    char c2 = constructor(a1);
    if(!prefixp(c2))berror("illegal syntax of '\' in backquote --- '\' must be followed by a unary operator such as '$' or itself");
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
  setprop(sym, string_exp("symbol_macro_expansion"), expansion);
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
  expptr val = f(e);
  pop_dbg_stack();
  return val;
}

char * top_string(expptr e){
  if(e == NULL)return NULL;
  char c  = constructor(e);
  if(c == 'A')return arg1(a);
  if(c == 'A')return top_symbol(arg1(e));
  if(c == 'O')return arg1(e);
  return NULL;
}
  
expptr macroexpand1(expptr e){
  expptr s = top_symbol(e);
  if(s == NULL)return e;
  return macroexpand2(s,e);
}

expptr macroexpand(expptr e){
  if(e == NULL)return NULL;
  expptr e2 = macroexpand1(e);
  if(e2 != e) return macroexpand(e2);
  return macroexpand_args(e);
}

/** ========================================================================
section: gensym
========================================================================**/

expptr int_exp(int i){
  if(i < 0)berror("attempt to convert negative integer to expression");
  sprintf(ephemeral_buffer,"%d",i);
  return string_exp(ephemeral_buffer);
}

int exp_int(expptr s){
  if(constructor(s) != 'A'){berror("illegal call to exp_int");}
  return atoi((char *) arg1(s));
}

expptr gensym(char * s){
  for(int i=1;1;i++){
    int length = snprintf(ephemeral_buffer,EPHEMERAL_DIM,"_mcgen_%s%d",s,i);
    if(length >= EPHEMERAL_DIM)berror("ephemeral buffer exauhsted");
    int key = preintern_string(ephemeral_buffer);
    if(string_hash_table[key]==NULL)break;}
  return string_exp(ephemeral_buffer);
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
  char next2;} wrapper, * wstream;

wstream file_stream;
wstream terminal_stream;
wstream read_stream;

int paren_level;
char quotechar; //this is internal to preprocessing, mcread_string uses its own flag.

wstream cwrap_stream(FILE * f);
void advance_readchar();

void open_input_file(char * source){
  filein = fopen(source, "r");
  if(filein == NULL)berror("attempt to open input file failed");
  file_stream = cwrap_stream(filein);
}

void skipwhite(){while(whitep(readchar))advance_readchar();}

void advance_readchar_past_white(){
  advance_readchar();
  skipwhite();
}

wstream cwrap_stream(FILE * s){
  wstream result = (wstream) malloc(sizeof(wrapper));
  result->stream = s;
  result->next = ' ';
  result->next2 = ' ';
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
  if(read_stream->next2 == EOF)nextchar = EOF;
  else nextchar = fgetc(read_stream->stream);
  read_stream->next = read_stream->next2;
  read_stream->next2 = nextchar;
}

void advance_readchar(){
  //readchar is undefined on entry --- only read_stream->next and read_stream->next2 are defined.
  
  //the following removes comments and quoted returns.

  if(quotechar == 0){
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
	      && quotechar == 0){
	advance_lookahead();
	advance_lookahead();}
      else break;}}

  //file segmentation
  if(read_stream == file_stream
     && quotechar == 0
     && read_stream->next == '\n'
     && !whitep(read_stream->next2)
     && !closep(read_stream->next2)){
       read_stream->next = '\0';}

  readchar = read_stream->next;
  
  if(read_stream == terminal_stream){
    //ensuring immediate response at the terminal
    //and limiting non-terminated quotations

    int lookahead_level = paren_level;
    if(openp(read_stream->next))lookahead_level++;
    if(closep(read_stream->next))lookahead_level--;
    
    if(read_stream->next2 == '\n' && (quotechar != 0 ||  lookahead_level == 0)){
      read_stream->next = '\0';
      read_stream->next2 = ' ';}
    else if(read_stream->next == '\0'){
      read_stream->next = ' ';
      read_stream->next2 = ' ';}
    else advance_lookahead();}
  else advance_lookahead();

  //maintaining quotechar
  if(string_quotep(read_stream->next)){
    if(quotechar == 0){quotechar = read_stream->next;}
    else if(readchar != '\\' && read_stream->next == quotechar){quotechar = 0;}}
}

/** ========================================================================
lexicalizatrion
======================================================================== **/

int string_quotep(char x){return (x == '"' || x == '\'');}

int binaryp(char c){
  return c == '*' || c == '/' || c == '+' || c == '-' || c == '.' || c == ':'
      || c == ',' || c == '<' || c == '=' ||c == '>' || c == '@' || c == '^' || c == '|' || c == '&' || c == '~';
}

int terminatorp(char c){return (closep(c) || c == EOF || c == '\0' || c == ';');}

char * mcread_symbol(){
  if(!alphap(readchar))berror("non alphap charcter visible to mcread_symbol");
  int i=0;
  while(alphap(readchar)){
    if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
    ephemeral_buffer[i++]=readchar;
    advance_readchar();}
  skipwhite();
  if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[i]=0;
  return intern_string(ephemeral_buffer);
}

char * mcread_connective(){
  if(!binaryp(readchar))berror("non binaryp charcter visible to mcread_connective");
  int i=0;
  while(alphap(readchar)){
    if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
    ephemeral_buffer[i++]=readchar;
    advance_readchar();}
  skipwhite();
  if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[i]=0;
  return intern_string(ephemeral_buffer);
}

char * mcread_string(){
  //we must prevent an unterminated string from swalling all further input.
  //we solve this by not allowing return characters in strings.
  if(!string_quotep(readchar))berror("non string_quotep character visibile to mcread_string");

  char q = readchar; //remember the quote character
  ephemeral_buffer[0] = q;
  advance_readchar();
  int i = 1;
  int quoted = 0;
  while(!(readchar == q && !quoted)){
    if(readchar == EOF || readchar == '\0' || readchar == '\n')berror("unterminated string constant");
    if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
    ephemeral_buffer[i++]=readchar;
    if(readchar == '\\' && !quoted) quoted = 1; else quoted = 0;
    advance_readchar();
  }
  skipwhite();
  if(i == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[i]=0;
  return intern_string(ephemeral_buffer);
}


/** ========================================================================
The reader

The reader makes use of the following addional character classifications
======================================================================== **/

int alphap(char c){
  return  (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || (c == '_');
}

int precedence(char * s){
  car op = s[0];
  if(op==',')return 2;
  if(op==' ')return 3;
  if(op=='=' || op=='<' || op=='>' || op =='~') return 4;
  if(op=='|' || op =='&')return 5;
  if(op=='+' || op=='-')return 6;
  if(op=='*' || op=='/')return 7;
  if(op == '^')return 8;
  if(op=='@')return 9;
  if(op=='.')return 10;
  if(op==' ')return 11;
  if(op==':')return 12;
  berror("unknown character in precedence");
  return 1; //avoids compiler warning
}

#define LEFT_THRESHOLD 8
//operators with precedence >= LEFT_THRESHOLD are left associative.

/** ========================================================================
Parsing Arguments
======================================================================== **/
expptr mcread_tree(int p);
expptr mcread_arg();
expptr mcread_unary();
expptr mcread_application();
expptr mcread_paren_sequence();
expptr mcread_open();

expptr mcread_arg(){
  char c = readchar; //remember readchar
  if(string_quotep(readchar)){return mcread_string();}
  return mcread_unary();
}

expptr mcread_unary(){
  char c = readchar; //remember readchar
  if(prefixp(c)){
    advance_readchar_past_white();
    return intern_exp(c,mcread_unary(),NULL);}
  if(openp(c))return intern_exp(c,mcread_open(),NULL);
  return mcread_application();
}

expptr mcread_application(){
  char c = readchar; //remember readchar
  //c  is not string_quote, prefixp, or openp
  if(alphap(c)){
    expptr sym = mcread_symbol();
    return intern_exp('A',sym,mcread_paren_sequence());}
  return NULL;
}

expptr mcread_paren_sequence(){
  if(!openp(readchar))return NULL;
  char c = readchar; //remember before advancing
  expptr first = mcread_open();
  return intern_exp(c,first,mcread_paren_sequence());
}

void declare_unmatched(char openchar, expptr e, char closechar){
  fprintf(stderr,"unmatched parentheses %c\n",openchar);
  pprint(e,stderr,rep_column);
  fprintf(stderr, "%c\n", closechar);
  berror("");
}

expptr mcread_open(){ // readchar is openp
  char openchar = readchar;
  char closechar = close_for(openchar);

  paren_level++;
  advance_readchar_past_white();
  expptr e = mcread_tree(0);
  if(readchar != closechar)declare_unmatched(openchar,e,closechar);
  paren_level--;
  
  advance_readchar_past_white();
  expptr val = intern_exp(openchar,e,NULL);
  return val;
}

/** ========================================================================
Parsing

We note that for any triple ... op1 arg op2 ..., where op1 and op2 are binary connectives, the
precedence of op1 and op2 determines whether arg binds to the right or to the left.  A higher precedence
operator binds. If the precedence is the same we have arg binds left for left-assiative priories and
binds right for right-associative priorities.  Associativity is the same for all same-precedence opertors.

In mcread_tree(p1), p1 is the precedence of the connective to the left of readcar
or zero if there is no such connective

mcread_tree(p1) returns arg and op such that arg binds left.  The operator argument is returned in the variable readop.

The null connective is represented by a space connective and application of the space connective are represented by expressions tagged with 'O'.
This simplifies mcread_tree by allowing the null connective to be handled by general code for binary connectives.
The null connective prints as space. Having the space connective print as space guarantees that adjacent symbols remain separated in the printed string.

The space connective has precedence higher than comma (and higher than the terminator symbol ';') but lower than all other binary connectives.  This causes friendly tree structures
for C argument lists and statement lists.

If there is no left operator then p1 == 0 which is weaker than all connectives other than the true null connective
inserted at terminators.  This causes mcread_tree(0) to return the tree between the current readchar and the next terminator.

Each of the following mcread functions can return NULL but if mcread_arg() is followed by mcread_connective() either one of
the two must be non-null or readchar is a terminator --- the initial readcar must contribute to one of these three cases.
==================================================================== **/

expptr readop;

expptr last_def;
char last_readchar;

expptr read_from(wstream s){
  read_stream = s;
  paren_level = 0;
  quotechar = 0;

  //readchar is undefined on entry.  The following sets readchar.
  advance_readchar_past_white();
  return mcread_tree(0);
  //on return readchar is either '\0' or EOF
}

expptr read_from_terminal(){return read_from(terminal_stream);}

expptr read_from_file(){return read_from(file_stream);}

expptr mcread_tree(int p1){
  if(whitep(readchar))berror("whitespace visible to mcread_tree");
  expptr a1 = mcread_arg();
  expptr op2 = mcread_connective();
  int p2 = op_precedence(op2);
  while(p2 > p1 || (p2 == p1 && p1 != 0 && p1 < LEFT_THRESHOLD) || (p1 == 0 && readchar ==';')){
      op2 = mcread_connective();
      p2 = op_precedence(op2);}
    else {
      expptr a2 = mcread_tree(p2);
      a1 = intern_exp('O',op2,intern_exp(' ',a1,a2));
      op2 = readop;
      p2 = op_precedence(op2);}}
  readop = op2;
  return a1;
  if(whitep(readchar))berror("white space on return of mcread_tree");
}

/** ========================================================================
section: initialization
========================================================================**/

void mcA_init(){
  break_active = 0;
  init_stack_frames();
  init_undo_frames();
  init_strings();
  init_expressions();
  rep_column = 0;
  
  dbg_freeptr = 0;
  
  print_length_token = string_exp("print_length");
  macro_token = string_exp("macro");

  readchar = ' ';
  catch_freeptr = 0;
  terminal_stream = cwrap_stream(stdin);
  commaop = intern_exp('o',(expptr) intern_string(","),NULL);
  colonop = intern_exp('o',(expptr) intern_string(":"),NULL);
  spaceop = intern_exp('o',(expptr) intern_string(" "),NULL);

  set_macro(intern_exp('`',NULL,NULL), bquote_macro);
}

