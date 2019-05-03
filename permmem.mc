/** ========================================================================
Permanent memory.  Permanent memory is not garbage collected.

perm_alloc and intern_permexp

See mc.h for the types expstruct and expptr
======================================================================== **/

#define PERMHEAP_DIM (1<<20)
char permheap[PERMHEAP_DIM];
int permheap_freeptr;

void * perm_alloc(int size){
  if(permheap_freeptr + size > PERMHEAP_DIM)berror("perm heap exhausted");
  void * result = &permheap[permheap_freeptr];
  permheap_freeptr += size;
  return result;
}

#define PERMEXP_HASH_DIM  (1 << 15)
expptr permexp_hash_table[PERMEXP_HASH_DIM];
int permexp_count;

expptr intern_permexp(char constr, expptr a1, expptr a2){
  unsigned int j = (constr + 729*((long int) a1) + 125*((long int) a2)) & PERMEXP_HASH_DIM-1;
  for(int i = j;1;i++){
    if(i == PERMEXP_HASH_DIM)i=0;
    expptr oldexp = permexp_hash_table[i];
    if(oldexp == NULL){
      if(permexp_count >= (2*PERMEXP_HASH_DIM)/3)berror("permanent expression hash table exhausted");
      permexp_count++;
      expptr newexp = perm_alloc(sizeof(expstruct));
      newexp->plist = NULL;
      newexp->internal = NULL;
      newexp->constructor = constr;
      newexp->arg1 = a1;
      newexp->arg2 = a2;
      permexp_hash_table[i] = newexp;
      return newexp;
    }else{
      if(oldexp -> constructor == constr && oldexp->arg1 == a1 && oldexp-> arg2 == a2)return oldexp;
    }
  }
}


/** ========================================================================
permanent strings
======================================================================== **/

#define PERMSTRING_HASH_DIM 10000
char * permstring_hash_table[PERMSTRING_HASH_DIM];
int permstring_count;

int permstring_key(char * s){
  int i, key;

  key=0;
  for(i=0;s[i] != 0;i++){
    key = (1458*key + s[i]);
  }
  key = key&(PERMSTRING_HASH_DIM-1);

  while(permstring_hash_table[key] != NULL
	&& strcmp(permstring_hash_table[key], s) != 0){
    key++;
    if(key==PERMSTRING_HASH_DIM)key=0;
  }
  return key;
}

char * string_to_perm(char * s){
  int key = permstring_key(s);
  if(permstring_hash_table[key]==NULL){
    if(permstring_count >= (2*PERMSTRING_HASH_DIM)/3)berror("string hash table exhausted");
    char * s2 = perm_alloc(strlen(s) + 1);
    strcpy(s2,s);
    permstring_hash_table[key] = s2;
    permstring_count++;
  }
  return permstring_hash_table[key];
}

void init_perm_memory(){
  permheap_freeptr = 0;
  for(int i=0;i<PERMEXP_HASH_DIM;i++)permexp_hash_table[i] = NULL;
  permexp_count = 0;
  for(int i=0;i<PERMSTRING_HASH_DIM;i++)permstring_hash_table[i]=NULL;
  permstring_count = 0;
}

/** ========================================================================
permanent properties of permanent expressions
========================================================================**/

void perm_addprop(expptr e, expptr key, void * val){
  if(e == NULL)berror("attempt to add a property of the null expression");
  plist new = (plist) perm_alloc(sizeof(pliststruct));
  new->key = key;
  new->value = val;
  new->rest = e->plist;
  e-> plist = new;
}

void perm_setprop(expptr e, expptr key, void * val){
  if(e == NULL)berror("attempt to set a property of the null expression");
  plist cell = getprop_cell(e, key);
  if(cell != NULL){
    cell->value = val;
    return;}
  perm_addprop(e,key,val);
}

void perm_setprop_int(expptr e, expptr key, int x){
  char buffer[8]; //buffer is a pointer.
  int * y = (int *) buffer;
  *y = x;
  perm_setprop(e,key, * (expptr *) buffer);
}


/** ========================================================================
exp_to_perm
======================================================================== **/


expptr expptr_to_perm(expptr exp){
  if(!exp)return NULL;
  if (atomp(exp))return intern_permexp('A', (expptr) string_to_perm((char *) exp->arg1), NULL);
  return stack_exp(exp->constructor,expptr_to_perm(exp->arg1),expptr_to_perm(exp->arg2));
}
