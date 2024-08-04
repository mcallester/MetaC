#include "mc.h"

/** ========================================================================
versions of throw, cbreak and berror.  Versions of catch are in premaros and mcD.mc.
========================================================================**/
int in_ide_proc(){return in_ide;} //used in mcprint macro in mcD.mc

void throw_primitive(){
  if(catch_freeptr[0] == 0){
    fprintf(stdout,"uncaught throw: c process fatal");
    cbreak();
    exit(-1);}
  longjmp(catch_stack[catch_freeptr[0]]-1, 1);
  }

void throw_NIDE(expptr msg){ //the throw macro is not available here.
  if(in_ide || in_repl){
    catch_name[0]= string_atom("NIDE");
    catch_val[0]= msg;
    throw_primitive();}
  exit(-1);
}

void cbreak(){};  //this procedure has a break set in gdb

void berror(char *);

void send_ready(){
  //this is a return acknowledgement from C
  //we do not wait for a return acknowledgement from a return acknowledgement
  if(!in_ide)berror("sending to emacs while not in the IDE");
  fflush(stderr); //this is needed for the ignore tag to operate on stderr.
  fprintf(stdout,"%s",mc_ready_tag);
  fflush(stdout); //without this stderr can later add to the input of the tag.
  }

void send_emacs_tag(char * tag){
  if(!in_ide)berror("sending to emacs while not in the NIDE");
  fflush(stderr); //this is needed for the ignore tag to operate on stderr.
  fprintf(stdout,"%s",tag);
  fflush(stdout); //without this stderr can later add to the input of the tag.
  read_from_NIDE(); //we wait for emacs to acknowledge completion of tag task.
  }

void send_print_tag(){ //the global variable print_tag is not visible to the NIDE
  send_emacs_tag(print_tag);
}

void berror(char *s){
  fprintf(stdout,"\n%s\n",s);
  if(in_ide){
    if(in_doit) send_emacs_tag(exec_error_tag);
    else send_emacs_tag(expansion_error_tag);}
  cbreak();
  if(in_ide){send_emacs_tag(continue_from_gdb_tag);}
  throw_NIDE(NULL);
}

void send_result(char * result){
  berror("send_result undefined outside of NIDE");
  fprintf(stdout,"%s",result);
  send_emacs_tag(result_tag);}

void breakpt(char *s){
  fprintf(stdout,"breakpt: %s\n",s);
  send_emacs_tag(breakpoint_tag);
  cbreak();
  send_emacs_tag(continue_from_gdb_tag);
  }

/** ========================================================================
The permanent heap (not garbage collected or freed in any way)
========================================================================**/

#define PERM_HEAP_DIM (1<<20)
char perm_heap[PERM_HEAP_DIM] = {0};
int perm_heap_freeptr;

void * perm_alloc(int size){
  if(perm_heap_freeptr + size > PERM_HEAP_DIM)berror("perm heap exhausted");
  void * result = &perm_heap[perm_heap_freeptr];
  perm_heap_freeptr += size;
  memset(result,0,size);
  return result;
}

//permanent strings with two properties.

char * string_hash_table[STRING_DIM] = {0};  //STRING_DIM is defined in premacros.h.  It is needed in mcE.mc for string property tables.
int string_count;

int string_key(char* s){
  int i, key;

  key=0;
  for(i=0;s[i] != 0;i++){
    key = (1458*key + s[i]);
  }
  key = key&(STRING_DIM-1); //fancy mod function for powers of 2.

  while(string_hash_table[key] != NULL
	&& strcmp(string_hash_table[key], s) != 0){
    key++;
    if(key==STRING_DIM)key=0;
  }
  return key;
}

int intern_string(char * s){
  int key = string_key(s);
  if(string_hash_table[key]==NULL){
    if(string_count >= (2*STRING_DIM)/3)berror("string hash table exhausted");
    char * s2 = perm_alloc(strlen(s) + 1);
    strcpy(s2,s);
    string_hash_table[key] = s2;
    string_count++;
  }
  return key;
}

/** ========================================================================
undo_alloc, undo_set_int and undo_set

premacros.h (included form mc.h included above) contains the declarations of the undo heap.

#define undo_set_int(pointer,value) undo_set_int_proc((int *) &pointer,value)

#define undo_set(pointer,value) undo_set_proc((void **) &pointer,value)
========================================================================**/

int undo_heap_freeptr;

void * undo_alloc(int size){
  if(undo_heap_freeptr + size > UNDO_HEAP_DIM)berror("undo heap exhausted");
  void * result = &undo_heap[undo_heap_freeptr];
  undo_heap_freeptr += size;
  memset(result,0,size);
  return result;
}

int undo_heap_freeptr_fun(){
  return undo_heap_freeptr;}

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

#define UNDO_TRAIL_DIM  (1 << 20)
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
    if(undo_trail_int_freeptr == UNDO_TRAIL_INT_DIM)berror("undo trail exhausted");
    undo_trail_int[undo_trail_int_freeptr].location = undone_integer[i];
    undo_trail_int[undo_trail_int_freeptr++].oldval = *undone_integer[i];}
  for(int i = 0;i<undoneptr_freeptr;i++){
    if(undo_trail_freeptr == UNDO_TRAIL_DIM)berror("undo trail exhausted");
    undo_trail[undo_trail_freeptr].location = undone_pointer[i];
    undo_trail[undo_trail_freeptr++].oldval = *undone_pointer[i];}
  }

void push_undo_frame(){
  int previous_freeptr = undo_heap_freeptr;
  if(undostack_freeptr == UNDOSTACK_DIM)berror("undo freeptr stack exhausted");
  undo_stack[undostack_freeptr].undo_trail_int_freeptr = undo_trail_int_freeptr;
  undo_stack[undostack_freeptr++].undo_trail_freeptr = undo_trail_freeptr;
  save_undones();
  undo_set(previous_heap_boundary[0],&undo_heap[previous_freeptr]);
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

void init_undo1(){
  undo_heap_freeptr = 0;
  perm_heap_freeptr = 0;
  undoneint_freeptr = 0;
  undoneptr_freeptr = 0;
  add_undone_int(&undo_heap_freeptr);
  undo_trail_int_freeptr = 0;
  undo_trail_freeptr = 0;
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
expptr undoexp_hash_table[UNDOEXP_HASH_DIM] = {0};
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

expptr intern_index_exp(expptr e){
  char constr = e->constructor;
  expptr a1 = e->arg1;
  expptr a2 = e->arg2;
  if(constr == '\0')berror("bad constructuctor in intern_exp");
  unsigned int j = (constr + 729*((long int) a1) + 125*((long int) a2)) & UNDOEXP_HASH_DIM-1;
  for(int i = j;1;i++){
    if(i == UNDOEXP_HASH_DIM)i=0;
    expptr oldexp = undoexp_hash_table[i];
    if(oldexp == NULL)return int_exp(i);
    if(oldexp -> constructor == constr && oldexp->arg1 == a1 && oldexp-> arg2 == a2)return int_exp(i);
  }
}

void init_undo_memory(){
  init_undo1();

  undoexp_count = 0;
  add_undone_int(&undoexp_count);

  string_count = 0;
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
    undo_set_int(cell->value,val);
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
  if(stackheap_freeptr + size > STACKHEAP_DIM){
    stackheap_freeptr = 0;
    berror("stack memory heap exhausted");}
  char * result = &stackheap[stackheap_freeptr];
  stackheap_freeptr += size;
  return result;
  }

int in_stackheap(void* p){
  return (p >= (void*) &stackheap[0] && p < (void*) &stackheap[STACKHEAP_DIM]);
  }

int stackheap_allocatedp(void* p){
  return (p >= (void*) &stackheap[0] && p < (void*) &stackheap[stackheap_freeptr]);
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
  if (atomp(exp))return stack_exp('A', exp->arg1, NULL);
  return stack_exp(exp->constructor,expptr_to_stack(exp->arg1),expptr_to_stack(exp->arg2));
}

expptr expptr_to_undo(expptr exp){
  if(!exp)return NULL;
  if (atomp(exp))return intern_exp('A',(expptr) exp->arg1,NULL);
  return intern_exp(exp->constructor,expptr_to_undo(exp->arg1),expptr_to_undo(exp->arg2));
}

expptr clean_undo_frame(expptr exp){
  //this is safe --- no user code.
  expptr stack_exp = expptr_to_stack(exp);
  clear_undo_frame();
  expptr result = expptr_to_undo(stack_exp);
  return result;
  }

/** ========================================================================
character types and precedence
========================================================================**/

int quotecharp(char x){return (x == '"' || x == '\'');}

int alphap(char c){
  return  (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || (c == '_');
}

int connp(char c){
  return c == '#' || c == '*' || c == '/' || c == '+' || c == '-' || c == '.' || c == ':'
    || c == ',' || c == '<' || c == '=' ||c == '>' || c == '@' || c == '^'
    || c == '|' || c == '&' || c == '~' ||c ==';' || c == '%' || c == '!' || c == '?'
    || c < 0; //unicode
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

int specialcharp(char c){return c == '`' || c == '$' || c == '\\';}

int precedence(char c){
  if(terminatorp(c))return -1;
  if(c==';')return 1;
  if(c==',')return 2;
  if(c ==':')return 3;
  if(c=='@' || c < 0)return 4; //unicode
  if(c=='|')return 5;
  if(c=='&')return 6;
  if(c=='?')return 8;
  if(c=='=' || c=='<' || c=='>' || c =='~' || c=='!') return 9;
  if(c=='+' || c=='-')return 10;
  if(c=='*' || c=='/')return 11;
  if(c == '%' || c == '^' || c== '#')return 12;
  if(c==' ')return 13;
  if(c=='.')return 14;
  berror("undefined precedence");
  return 13; //prevents compiler warning
  }


/** ========================================================================
interface to expressions.  conses into undo space.  The concept of cons cell is
depricated in favor of connection.
========================================================================**/

//atoms
int atomp(expptr e){return e && constructor(e) == 'A';}

int symbolp(expptr e){
  return e && e->constructor == 'A' && alphap(atom_string(e)[0]);
  }

int connectorp(expptr e){
  if(!e || e->constructor != 'A')return 0;
  char c = atom_string(e)[0];
  return connp(c) || whitep(c);
  }

int white_connectorp(expptr e){
  if(!e || e->constructor != 'A')return 0;
  char c = atom_string(e)[0];
  return whitep(c);
  }

int specialp(expptr e){
  return e && e->constructor == 'A' && specialcharp(atom_string(e)[0]);
  }

char * atom_string(expptr a){
  if(constructor(a) != 'A'){
    berror("attempt to get string of non-atom");}
  return (char *) string_hash_table[(int) (long int) a->arg1];}

expptr string_atom(char * s){
  return intern_exp('A', (expptr) (long int) intern_string(s),NULL);
  }

void ephemeral_putc(char c){
  if(ephemeral_freeptr == EPHEMERAL_DIM)berror("ephemeral buffer exhausted");
  ephemeral_buffer[ephemeral_freeptr++]=c;
  }

//cells
expptr cons(expptr x, expptr y){
  return intern_exp(' ',x,y);}

int cellp(expptr e){return e && constructor(e) == ' ';}

expptr car(expptr x){
  if(!x || constructor(x) != ' '){berror("taking car of non-cell");}
  return x->arg1;}

expptr cdr(expptr x){
  if(!x || constructor(x) != ' '){berror("taking cdr of non-cell");}
  return x->arg2;}

//parens

expptr intern_paren(char openchar, expptr arg){
  return intern_exp(openchar, arg, NULL);}

int parenp(expptr e){return e && openp(constructor(e));}

char open_char(expptr e){return constructor(e);}

expptr paren_inside(expptr e){
  if(!openp(constructor(e)))berror("paren_inside applied to non-paren");
  return e->arg1;}

//connections --- the parser produces expressions such that every expression
//is either an atom, paren, or connection.


expptr mk_connection(expptr connector, expptr leftarg, expptr rightarg){
  return cons(cons(leftarg,connector),rightarg);
  }

int connectionp(expptr e){
  return cellp(e) && cellp(car(e)) && connectorp(cdr(car(e)));
  }

int connofp(expptr e, expptr conn){return connectionp(e) && connector(e) == conn;}

expptr connector(expptr e){
  return cdr(car(e));
  }

expptr leftarg(expptr e){
  return car(car(e));
  }

expptr rightarg(expptr e){
  return cdr(e);
  }

/** ========================================================================
lists of expressions

the do_explist macro is defined in mcD.mc
========================================================================**/

explist expcons(expptr first, explist rest){
  explist result = (explist) undo_alloc(sizeof(explist_struct));
  result->first = first;
  result->rest = rest;
  return result;
  }

explist explist_append(explist l1, explist l2){
  if(!l1)return l2;
  return expcons(l1->first,explist_append(l1->rest,l2));
  }

explist explist_reverse(explist lst){
  explist r = NULL;
  while(lst){
    r = expcons(lst->first,r);
    lst = lst->rest;}
  return r;
  }

int explist_member(expptr x, explist lst){
  if(!lst)return 0;
  if(x == lst->first)return 1;
  return explist_member(x,lst->rest);
}

explist explist_mapcar(expptr f(expptr), explist lst){
  if(lst)return expcons(f(lst->first),explist_mapcar(f,lst->rest));
  return NULL;
}

void explist_mapc(void f(expptr), explist lst){
  while(lst){
    f(lst->first);
    lst = lst->rest;}  
}

int explist_length(explist lst){
  if(lst)return explist_length(lst->rest) + 1;
  else return 0;
  }

/** ========================================================================
explist file_expressions(char*) reads the cells of a file.

expptr read_from_NIDE() reads a cell from the NIDE()

advance_char(mc_read_stream)  advances one charachter while skipping over comments.
========================================================================**/

expptr mcread(char* s);

typedef struct read_stream_struct{
  FILE* stream;
  char readchar;
  char next;
  }read_stream_struct, *mc_read_stream;

mc_read_stream input_stream;

void init_input_stream(){
  input_stream = (mc_read_stream) perm_alloc(sizeof(read_stream_struct));
  input_stream->stream = stdin;
  }

void advance_char(mc_read_stream);

void init_read_stream(mc_read_stream read_stream){
  read_stream->readchar = ' ';
  read_stream->next = fgetc(read_stream->stream);
  advance_char(read_stream); //skips over comments
  }

void addchar(char c){
  if(stackheap_freeptr == STACKHEAP_DIM)berror("stack heap exhausted");
  stackheap[stackheap_freeptr++] = c;
  }

expptr read_from_NIDE(){ //reads a string from stdin
  if(!in_ide)berror("calling read_from_NIDE while not in NIDE");
  init_read_stream(input_stream);
  char* s = &stackheap[stackheap_freeptr];
  push_memory_frame();
  while(!endp(input_stream->readchar)){
    addchar(input_stream->readchar);
    advance_char(input_stream);}
  addchar('\0');
  expptr e = mcread(s);
  pop_memory_frame();
  return e;
  }

int cell_endp(mc_read_stream read_stream){
  return (read_stream->readchar == '\n' &&
	  !whitep(read_stream->next) &&
	  !closep(read_stream->next) &&
	  read_stream->next != '/');
  }

expptr read_from_file(mc_read_stream read_stream){ //reads one cell from a file
  //the stream must be initialized
  char* s = &stackheap[stackheap_freeptr];
  push_memory_frame();
  while(!endp(read_stream->readchar) &&
	!cell_endp(read_stream)){
    addchar(read_stream->readchar);
    advance_char(read_stream);}
  addchar('\0');
  if(!endp(read_stream->readchar))advance_char(read_stream);
  expptr e = mcread(s);
  pop_memory_frame();
  return e;}

explist file_expressions(char * fname){
  FILE* stream = fopen(fname, "r");
  if(stream == NULL){
    fprintf(stdout,"attempt to open %s failed",fname); //no sformat yet
    berror("");}
  mc_read_stream read_stream = (mc_read_stream) stack_alloc(sizeof(read_stream_struct));
  read_stream->stream = stream;
  init_read_stream(read_stream);
  explist exps = NULL;
  precatch({ //premacros version of catch_all, catch_all is defined in mcD.mc
	     while(read_stream->readchar != EOF){
	       exps = expcons(read_from_file(read_stream),exps);}
	     fclose(stream);
	     },{
	     fclose(stream);
	     throw_NIDE(NULL);});
  return explist_reverse(exps);
  }

void simple_advance(mc_read_stream read_stream){
  if(read_stream->next < 32 &&
     read_stream->next > 0 &&
     read_stream->next != 10 &&
     read_stream->next != 9){
    berror("illegal input character");}
  read_stream->readchar = read_stream->next;
  read_stream->next = fgetc(read_stream->stream);
  }

void advance_char(mc_read_stream read_stream){
  simple_advance(read_stream);
  
  if(read_stream->readchar == '/' && read_stream->next == '*'){
    //replace comment with ' ' (whitep) or endp
    while(!endp(read_stream->readchar) &&
	  !(read_stream->readchar == '*' &&
	    read_stream->next == '/')){
      simple_advance(read_stream);}
    if(!endp(read_stream->readchar)){
      simple_advance(read_stream);
      read_stream->readchar = ' ';}}
  
  else if(read_stream->readchar == '/' && read_stream->next == '/'){
    //replace comment with '\n' (whitep) or endp
    while(!endp(read_stream->readchar) && read_stream->readchar != '\n'){
      simple_advance(read_stream);}}
  
  else if(read_stream->readchar == '\\' && read_stream->next == '\n'){
    //advance past quoted return --- needed for parsing #define from a file.
    simple_advance(read_stream); advance_char(read_stream);}
  }

/** ========================================================================
parsing a string.  This will be used for general reading and also
for normalizing the result of a backquote.  Parsed expressions consists of
atoms, connections, and parens only.  The parse tree of connectives respects
precedence and associativity.  Arguments to connectives can be NULL.  The inside
of a penthesis is always a null-terminated space list.
========================================================================**/

char* pps; //string being parsed
int pprest; //pointer to current character

void movechar(){
  if(stackheap_freeptr == STACKHEAP_DIM)berror("stack heap exhausted");
  stackheap[stackheap_freeptr++] = pps[pprest];
  pprest++;
  }

void remove_white(){
  while(whitep(pps[pprest]))pprest++;
  }

char* exp_pps(expptr e);

void declare_unmatched(char open, char close, expptr e){
  char* openstring = (char*) stack_alloc(2);
  openstring[0]=open;
  openstring[1]=0;
  expptr openexp = string_atom(openstring);
  char* closestring = (char*) stack_alloc(2);
  closestring[0]=close;
  closestring[1]=0;
  expptr closeexp = string_atom(closestring);
  expptr msg = intern_paren('{',
			      mk_connection(semi,
					    mk_connection(space,
							  string_atom("unmatched"),
							  string_atom("parens")),
					    mk_connection(space,
							  openexp,
							  mk_connection(space,
									e,
									closeexp))));
  throw_NIDE(msg);
}

expptr mcread_Ep(int);
expptr mcread_arg();
expptr mcread_conn();

expptr mcread(char* s){
  pps = s;
  pprest = 0;
  remove_white();
  expptr e = mcread_Ep(-1);
  if(closep(pps[pprest]))declare_unmatched('-',pps[pprest],e);
  return e;
  }

//subsec: mcread-Ep: The stack (held on the C stack) ends in a consumer
//(open paren or connective) with precedence p_left.
//mcread_Ep returns a (possibly phantom)
//general expression to be consumed by the consumer.

int right_precedence(){
  if(connp(pps[pprest]))return precedence(pps[pprest]);
  return precedence(' ');
  }

expptr mcread_Ep(int p_left){
  expptr arg = mcread_arg();
  int p_right = right_precedence();
  while(!terminatorp(pps[pprest])
	&& (p_left < p_right || (p_left == p_right && pps[pprest] != '.'))){
    expptr conn = mcread_conn();
    arg = mk_connection(conn,arg,mcread_Ep(p_right));
    p_right = right_precedence();}
  return arg;
  }

expptr mcread_quote();
expptr mcread_open();
expptr mcread_alpha_phrase();
expptr mcread_dollar_phrase();
expptr mcread_backquote_phrase();

expptr mcread_arg(){//each line corresponds to a value of the character pps[pprest].
  expptr s = mcread_quote();
  if(s)return s;
  s = mcread_open();
  if(s)return s;
  s = mcread_alpha_phrase();
  if(s)return s;
  s = mcread_dollar_phrase(); //includes backslash.
  if(s)return s;
  return mcread_backquote_phrase();
  }

//subsec the lexicalizer --- there is a procedure for each class of character

expptr mcread_conn(){ //never returns NULL
  if(!connp(pps[pprest]))return space;
  char* result = &stackheap[stackheap_freeptr];
  while(connp(pps[pprest])){movechar();}
  addchar('\0');
  remove_white();
  return string_atom(result);
  }

expptr mcread_alpha(){
  if(!alphap(pps[pprest]))return NULL;
  char* result = &stackheap[stackheap_freeptr];
  while(alphap(pps[pprest])){movechar();}
  addchar('\0');
  remove_white();
  return string_atom(result);
  }

expptr mcread_open(){
  if(!openp(pps[pprest]))return NULL;
  char c = pps[pprest];
  char cl = close_for(c);
  movechar(); remove_white();
  expptr e = mcread_Ep(-1);
  char end = pps[pprest];
  if(end != cl){
    if(closep(end))declare_unmatched(c,end,e);
    declare_unmatched(c,'-',e);}
  movechar();remove_white();
  return intern_paren(c,e);
  }

expptr mcread_quote(){
  if(!quotecharp(pps[pprest]))return NULL;
  char q = pps[pprest]; //remember the quote character
  char * result = &stackheap[stackheap_freeptr];
  movechar();
  int quoted = 0;
  while(1){
    char next = pps[pprest];
    if(next == '\0' || next == '\n'){//returns in strings not allowed
      berror("unterminated string constant");
      }
    movechar();
    if(next == q && quoted == 0)break;
    if(next == '\\' && !quoted) quoted = 1; else quoted = 0;}
  addchar('\0');
  expptr e = string_atom(result);
  remove_white();
  return e;
  }

expptr mcread_dollar(){
  if(pps[pprest] != '$')return NULL;
  pprest++; remove_white(); return dollar;}

expptr mcread_backslash(){
  if(pps[pprest] != '\\')return NULL;
  pprest++; remove_white(); return backslash;}

expptr mcread_backquote(){
  if(pps[pprest] != '`')return NULL;
  pprest++; remove_white(); return backquote;}

//subsec phrases
expptr mkspace(expptr left, expptr right){
  return mk_connection(space,left,right);}

expptr mcread_open_sequence(){
  expptr first = mcread_open();
  if(!first)return NULL;
  expptr rest = mcread_open_sequence();
  if(!rest)return first;
  return mkspace(first,rest);
  }


expptr mcread_alpha_phrase(){
  expptr s = mcread_alpha();
  if(!s)return NULL;
  expptr seq = mcread_open_sequence();
  if(seq)return mkspace(s,seq);
  return s;
  }

expptr mcread_alpha_or_open(){
  expptr s = mcread_alpha();
  if(s) return s;
  return mcread_open();
  }

expptr mcread_backquote_phrase(){
  expptr b = mcread_backquote();
  if(!b)return NULL;
  expptr arg = mcread_alpha_or_open();
  if(arg)return mkspace(b,arg);
  return b;
  }

expptr mcread_dollar_seq(){//it is important to "hide" dollar under backslash
  expptr s = mcread_dollar();
  if(s) return s;
  expptr first = mcread_backslash();
  if(!first)return NULL;
  expptr rest = mcread_dollar_seq();
  if(!rest)return first;
  return mkspace(first,rest);
  }

expptr mcread_dollar_phrase(){//backslashes must hide the dollar from backquote
  expptr s = mcread_dollar_seq();
  if(!s)return NULL;
  expptr arg = mcread_alpha_or_open();
  if(arg)return mkspace(s,arg);
  return s;
  }


/** ========================================================================
exp_string: this does not pretty print,
======================================================================== **/
char lastchar;  //used to insert a space between symbols or connectives

void paddchar(char c){
  if(stackheap_freeptr == STACKHEAP_DIM)berror("stack heap exhausted");
  stackheap[stackheap_freeptr++] = c;
  lastchar = c;
  }

void putexp(expptr);

char * exp_string(expptr e){
  char oldlast = lastchar;
  char * s = &(stackheap[stackheap_freeptr]);
  lastchar = '\0';
  putexp(e);
  paddchar('\0');
  lastchar=oldlast;
  return s;
}

void putstring(char * s){
  for(int i = 0;s[i] != '\0';i++){
    paddchar(s[i]);}
}

void putexp(expptr w){
  if(!w)return;
  if(atomp(w)){
    char * s = atom_string(w);
    if((connp(s[0]) && connp(lastchar)) ||
       (alphap(s[0]) && alphap(lastchar))){
      paddchar(' ');}
    putstring(s);}
  else if(parenp(w)){
    char c = constructor(w); paddchar(c);
    putexp(paren_inside(w));paddchar(close_for(c));}
  else if(connectionp(w)){
    putexp(leftarg(w));
    if(!white_connectorp(connector(w)))putexp(connector(w));
    putexp(rightarg(w));}
  else berror("illegal expptr");
  }

/** ========================================================================
exp_string_safe: this does not pretty print and does not generate an error
for non-expression
======================================================================== **/

void putexp_safe(expptr);

char * exp_string_safe(expptr e){
  char * s = &(stackheap[stackheap_freeptr]);
  lastchar = '\0';
  putexp_safe(e);
  paddchar('\0');
  return s;
}

void putexp_safe(expptr w){
  if(!w)return;
  if(atomp(w)){
    char * s = atom_string(w);
    if((connp(s[0]) && connp(lastchar)) ||
       (alphap(s[0]) && alphap(lastchar))){
      paddchar(' ');}
    putstring(s);}
  else if(parenp(w)){
    char c = constructor(w); paddchar(c);
    putexp_safe(paren_inside(w));paddchar(close_for(c));}
  else if(connectionp(w)){
    putexp_safe(leftarg(w));
    if(!white_connectorp(connector(w)))putexp_safe(connector(w));
    putexp_safe(rightarg(w));}
  else{
    paddchar('<');
    putexp_safe(car(w));
    paddchar(',');
    putexp_safe(cdr(w));
    paddchar('>');
    }
  }

/** ========================================================================
exp_pps this is like exp_string but inserts pretty printing white space.
======================================================================== **/

int current_indent;
int newline_indent;
expptr newline_connector;
void putexp_pretty(expptr e);

void paddchar_pretty(char c){
  paddchar(c);
  current_indent++;
}

void putstring_pretty(char * s){
  for(int i = 0;s[i] != '\0';i++){
    paddchar_pretty(s[i]);}
}

char * exp_pps(expptr e){
  char * s = &(stackheap[stackheap_freeptr]);
  lastchar = '\0';
  newline_indent = 0;
  current_indent = 0;
  newline_connector = NULL;
  putexp_pretty(e);
  paddchar('\n');
  paddchar('\0');
  return s;
}
	
void add_newline(){
  paddchar('\n');
  current_indent = 0;
  for(int i = 0;i< newline_indent;i++){paddchar_pretty(' ');}
  }

int exp_length(expptr e){
  if(!e)return 0;
  if(atomp(e))return strlen(atom_string(e));
  if(cellp(e))return exp_length(car(e)) + exp_length(cdr(e));
  if(parenp(e))return 2+exp_length(paren_inside(e));
  berror("illegal expression in exp_length");
  return 0;
}

void putexp_pretty(expptr e){ //line breaks at (all) curly braces and semicolons
  if(!e)return;

  if(atomp(e)){
    char * s = atom_string(e);
    if((connp(s[0]) && connp(lastchar)) ||
       (alphap(s[0]) && alphap(lastchar))){
      paddchar_pretty(' ');}
    putstring_pretty(s);
    return;}

  if(connectionp(e)){
    putexp_pretty(leftarg(e));
    expptr con = connector(e);
    if(!white_connectorp(con))putexp_pretty(con);
    expptr right = rightarg(e);
    if(!right)return;
    if(con != newline_connector){
      putexp_pretty(right);
      return;}
    add_newline();
    putexp_pretty(right);
    return;}
	
  if(parenp(e)){
    char c = constructor(e);
    paddchar_pretty(c);
    expptr old_connector = newline_connector;
    
    if(exp_length(e) < 30){
      newline_connector = NULL;
      putexp_pretty(paren_inside(e));
      paddchar_pretty(close_for(c));
      newline_connector = old_connector;
      return;}
    
    if(c == '{'){
      newline_indent++;
      newline_indent++;
      add_newline();
      newline_connector = semi;
      putexp_pretty(paren_inside(e));
      paddchar_pretty('}');
      newline_connector = old_connector;
      newline_indent--;
      newline_indent--;
      return;}

    //c = '(' or '['
    int old_indent = newline_indent;
    newline_indent = current_indent;
    newline_connector = comma;
    putexp_pretty(paren_inside(e));
    paddchar_pretty(close_for(c));
    newline_indent = old_indent;
    newline_connector = old_connector;
    return;}

  berror("illegal expression in putexp_pretty");
  }

    
/** ========================================================================
section: macroexpand
========================================================================**/

void set_macro(expptr sym, expptr f(expptr)){
  setprop(sym, macro, (expptr) f);
  }

expptr head_symbol (expptr e){
  if(!connofp(e,space))return NULL;
  expptr h = leftarg(e);
  if(atomp(h))return h;
  return NULL;
  }
  
expptr macroexpand1(expptr e){
  expptr head = head_symbol(e);
  if(!head)return e;
  expptr (*f)(expptr);
  f = (expptr (*)(expptr)) getprop(head,macro,NULL);
  if(f)return f(e);
  return e;
  }

expptr macroexpand2(expptr e){
  expptr e2 = macroexpand1(e);
  if(e2 != e)return macroexpand2(e2);
  if(parenp(e)) return intern_paren(constructor(e),macroexpand2(paren_inside(e)));
  if(connectionp(e)) return mk_connection(connector(e),
					  macroexpand2(leftarg(e)),
					  macroexpand2(rightarg(e)));
  return e;
  }


expptr reparse(expptr e){
  push_memory_frame();
  expptr e2 = mcread(exp_string(e));
  pop_memory_frame();
  return e2;
  }

expptr macroexpand(expptr e){
  expptr e2 = macroexpand2(e);
  if(e2 != e)return reparse(e2);
  return e2;
  }
  

/** ========================================================================
section: backquote
========================================================================**/
expptr bqcode(expptr e);

expptr bquote_macro(expptr e){ //e is a space connection applying backquote
  expptr arg = rightarg(e);
  if(atomp(arg))return bqcode(arg);
  if(parenp(arg))return bqcode(paren_inside(arg));
  berror("backquote applied to connective (not allowed)");
  }

expptr quote_quote(expptr);
expptr quote_symbol(expptr);
expptr quote_paren(expptr);
expptr dollar_code(expptr);
expptr quote_connection(expptr);
expptr paren_code(expptr,expptr);
expptr connection_code(expptr,expptr,expptr);
expptr quote_char(char);

int quotep(expptr e){
  return e && e->constructor == 'A' && quotecharp(atom_string(e)[0]);
  }


expptr bqcode(expptr e){
  if(!e) return string_atom("NULL");
  if(quotep(e))return quote_quote(e);
  if(atomp(e)) return quote_symbol(e);
  if(parenp(e))return quote_paren(e);
  expptr dcode = dollar_code(e);
  if(dcode)return dcode;
  return quote_connection(e);
  }

expptr bqcode_ignore_dollar(expptr e){
  if(!e) return string_atom("NULL");
  if(quotep(e))return quote_quote(e);
  if(atomp(e)) return quote_symbol(e);
  if(parenp(e)){
    return paren_code(quote_char(open_char(e)),
		      bqcode_ignore_dollar(paren_inside(e)));}
  return connection_code(quote_symbol(connector(e)),
			 bqcode_ignore_dollar(leftarg(e)),
			 bqcode_ignore_dollar(rightarg(e)));
  }

//atoms

expptr atom_code(expptr string_code){
  return mkspace(string_atom("string_atom"),
		intern_paren('(',string_code));}

expptr quote_symbol(expptr a){
  if(a == backslash)return atom_code(string_atom("\"\\\\\""));
  sprintf(ephemeral_buffer,"\"%s\"",atom_string(a));
  return atom_code(string_atom(ephemeral_buffer));
  }

expptr quote_quote(expptr a){
  char* s_in = atom_string(a);
  char* s_out = &stackheap[stackheap_freeptr];
  addchar('\"');addchar('\\');addchar('\"');
  for(int i = 1;s_in[i];i++){
    if(!s_in[i+1]){addchar('\\'); addchar('\"');}
    else if(quotecharp(s_in[i])){
      addchar('\\');addchar('\\');addchar(s_in[i]);}
    else addchar(s_in[i]);}
  addchar('\"'); addchar('\0');
  return atom_code(string_atom(s_out));
  }

//connections
expptr connection_code(expptr connector_code, expptr left_code, expptr right_code){
  return mkspace(string_atom("mk_connection"),
		intern_paren('(',
			       mk_connection(comma,
					     connector_code,
					     mk_connection(comma,
							   left_code,
							   right_code))));}

expptr mkspace_code(expptr leftcode, expptr rightcode){
  return mkspace(string_atom("mkspace"),
	       intern_paren('(',mk_connection(comma,leftcode,rightcode)));}
	       

expptr quote_connection(expptr e){
  if(connector(e) == space)return mkspace_code(bqcode(leftarg(e)),
					       bqcode(rightarg(e)));
  return connection_code(quote_symbol(connector(e)),
			 bqcode(leftarg(e)),
			 bqcode(rightarg(e)));
  }

//parens
expptr paren_code(expptr char_code, expptr inside_code){
  return mkspace(string_atom("intern_paren"),
	       intern_paren('(',
			      mk_connection(comma,
					    char_code,
					    inside_code)));}

expptr quote_char(char c){
  sprintf(ephemeral_buffer,"\'%c\'",c);
  return string_atom(ephemeral_buffer);
  }

expptr quote_paren(expptr e){
  return paren_code(quote_char(open_char(e)),bqcode(paren_inside(e)));
  }

//dollar

expptr dollar_code(expptr e){
  if(!connectionp(e) || connector(e) != space)return NULL;
  expptr opseq = leftarg(e);
  expptr arg = rightarg(e);
  if(opseq == dollar){
    if(!arg)berror("parser bug: impossible dollar expression");
    if(atomp(arg))return arg;
    if(parenp(arg))return paren_inside(arg);
    berror("parser bug: impossible dollar expression");}
  if(!connectionp(opseq) || connector(opseq) != space || leftarg(opseq) != backslash){
    return NULL;}
  return mkspace_code(bqcode(rightarg(opseq)),bqcode(arg));
  }

/** ========================================================================
Preamble and init_forms can be added during macro expansion
======================================================================== **/

void add_init_form(expptr form){
  //init forms are fullymacro expanded but in reverse order
  expptr form2 = macroexpand(form); //can recursively add preambles and init_forms
  init_forms = expcons(form2,init_forms);
}

void add_preamble(expptr e){
  //preambles are fullymacro expanded but in reverse order
  expptr e2 = macroexpand(e); //this can recursively add preambles and init forms
  preamble = expcons(e2,preamble);
  }

//in the NIDE there is no difference between init_forms and preamles so we use the following

void add_form(expptr e){add_preamble(e);} //used in the NIDE

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

expptr pointer_exp(void* p){
  sprintf(ephemeral_buffer,"%p",p);
  return string_atom(ephemeral_buffer);
  }

expptr gensym(expptr sym){
  if(strlen(atom_string(sym))+100 >= EPHEMERAL_DIM)berror("ephemeral buffer overflow");
  int i = 0;
  sprintf(ephemeral_buffer,"%s__%d",atom_string(sym),i);
  while(string_hash_table[string_key(ephemeral_buffer)]){
    i++;
    sprintf(ephemeral_buffer,"%s__%d",atom_string(sym),i);}
  return string_atom(ephemeral_buffer);
  }

/** ========================================================================
mcexpand:

preamble elements and init_forms elements get fully macroexpanded
======================================================================== **/

FILE * fileout;
void write_preamble(explist pre);

void mcexpand(char * source, char * destination){
  init_forms = NULL;
  fileout = fopen(destination, "w");
  if(fileout == NULL)berror("attempt to open output file failed");
  for(explist exps = file_expressions(source);exps;exps=exps->rest){
    preamble = NULL;
    expptr e2=macroexpand(exps->first);
    write_preamble(preamble);
    fprintf(fileout,"%s\n\n",exp_pps(e2));}
  fclose(fileout);
  }
void write_preamble(explist defs){
  //preamble elements are fully macro expanded but in reverse order.
  if(defs){
    write_preamble(defs->rest);
    fprintf(fileout,"%s\n\n",exp_pps(defs->first));}
  }

void pprint(expptr e, FILE* f){
  fprintf(f,"%s",exp_pps(e));
  }

void mcpprint(expptr e){
  if(in_ide){pprint(e,stdout); send_emacs_tag(print_tag);}
  else pprint(e,stdout);
}

void restart_undo_frame(int n){
  if(n == undostack_freeptr){
    push_undo_frame();
    return;}
  if(n > undostack_freeptr || n < 0)berror("attempt to restarting non-existent undo frame");
  while(undostack_freeptr > n+1){
    clear_undo_frame();
    undostack_freeptr--;}
  clear_undo_frame();
}

/** ========================================================================
section: initialization
========================================================================**/

void init_source(){
  in_ide = 0;
  in_repl = 0;
  in_doit = 0;
}

void init_tags(){
  ignore_tag = "*#*#dsflsadk#*#*ignore*#*#dsflsadk#*#*";
  result_tag = "*#*#dsflsadk#*#*result*#*#dsflsadk#*#*";
  uncaught_throw_tag = "*#*#dsflsadk#*#*uncaught-throw*#*#dsflsadk#*#*";
  reader_error_tag = "*#*#dsflsadk#*#*reader-error*#*#dsflsadk#*#*";
  expansion_error_tag = "*#*#dsflsadk#*#*expansion-error*#*#dsflsadk#*#*";
  comp_error_tag = "*#*#dsflsadk#*#*comp-error*#*#dsflsadk#*#*";
  exec_error_tag = "*#*#dsflsadk#*#*exec-error*#*#dsflsadk#*#*";
  breakpoint_tag = "*#*#dsflsadk#*#*breakpoint*#*#dsflsadk#*#*";
  continue_from_gdb_tag = "*#*#dsflsadk#*#*continue-from-gdb*#*#dsflsadk#*#*";
  print_tag = "*#*#dsflsadk#*#*print*#*#dsflsadk#*#*";
  mc_ready_tag = "*#*#dsflsadk#*#*mc-ready*#*#dsflsadk#*#*";
  }

void init_exp_constants(){
  //connectives
  comma = string_atom(",");
  colon = string_atom(":");
  semi = string_atom(";");
  exclam = string_atom("!");
  question = string_atom("?");
  dot = string_atom(".");
  space = string_atom(" ");
  
  //specials
  backslash = string_atom("\\");
  dollar = string_atom("$");
  backquote = string_atom("`");
  
  macro = string_atom("macro");
  nil = string_atom("");

  //openchars
  leftbrace = '{';
  leftparen = '(';
  leftbracket = '[';
  }

void mcA_init(){
  
  catch_freeptr[0] = 0;
  break_on_throw[0]=0;
  init_undo_memory();
  init_stack_memory();
  init_input_stream();
  init_exp_constants();
  init_source();
  init_tags();
  set_macro(backquote, bquote_macro);
  
  MetaC_directory = "/home/david/MC/";
  }

