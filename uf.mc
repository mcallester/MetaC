
/**
========================================================================
section: union-find
========================================================================
**/


#include "uf.h"

/**
expptr prop_type_token[0] = string_symbol("prop_type");
expptr prop_default_token[0] = string_symbol("prop_default");
expptr prop_value_token[0] = string_symbol("prop_value");

umacro{defprop(?prop,!type,!val)}
   {return 
       `{setprop(`{\${prop}},prop_type_token[0],${type});
         setprop(`{\${prop}},prop_default_token[0],${val});}}

umacro{!x .> ?prop <- !val}
   {return
       `{setprop(`{\${prop}},prop_value_token[0],(expptr) ${val});}]


umacro{!x.>?prop)}  
   {return `{(${getprop(prop,prop_type_token[0],`{(expptr})})
              getprop(${x},
                      `{\${prop}},
                      getprop(${prop},prop_default_token[0],NULL))}}

**/

/* defprop(count,int,1) */
/* defprop(next,exp,NULL) */
/* defprop(uf_parents,listof(exp),NULL) */
/* defprop(justification,exp,NULL) */


uf_ptr uf_make(void *identity) {

  uf_ptr new_node = (uf_ptr) undo_alloc(sizeof(struct uf_struct));

  new_node->node_id=next_uf_node_id;
  undo_set(next_uf_node_id,(uint64_t)++next_uf_node_id)
  new_node->next=NULL;
  new_node->count=1;
  new_node->identity=identity;

  return new_node;
}
  

uf_ptr uf_find(uf_ptr e){

  uf_ptr next = e->next;

  if (next == NULL){
    return e;}
  else{
    uf_ptr result = uf_find(next);
    undo_set (e->next,result);  // path compression
    return result;}
}


void set_find(uf_ptr die, uf_ptr live){
  undo_set(live->count, (uint64_t) (live->count + die->count)); //
  undo_set(die->next, live)
    // update justification
}




uf_ptr uf_equate(uf_ptr e1, uf_ptr e2){
  uf_ptr f1 = uf_find(e1);
  uf_ptr f2 = uf_find(e2);

  int f1_lives = (f1->count > f2->count);

  uf_ptr live = (f1_lives ? f1 : f2);
  uf_ptr die = (f1_lives ? f2 : f1);

  if (live == die) {berror("live and die are equal");};
  set_find(die, live);  

  return live;
}


init_form(next_uf_node_id=0;)


init_fun(uf_init)
