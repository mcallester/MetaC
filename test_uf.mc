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
#include "uf.h"
#include "test_uf.h"

int symbol_index(expptr);
expptr symbol_index_exp(expptr);
expptr file_preamble;
expptr procedures;
expptr arrays;


expptr uf_node_token;

void print_uf_node(uf_ptr n){
  if (n==NULL) {berror("Attempt to print uf node NULL");}
  fprintf(stdout, "\n  node id: %d", n->node_id );
  fprintf(stdout, "\n  count: %d", (int) n->count );
  fprintf(stdout, "\n  identity: "); pprint(n->identity,stdout,3);
  if (n->next != NULL) {
    fprintf(stdout, "\n  next:\n");
    print_uf_node(n->next);
  }
  fprintf(stdout,"\n");
}


void uf_show_exp(expptr exp){
  uf_ptr uf_node=(uf_ptr)getprop(exp,uf_node_token,NULL);
  if (uf_node==NULL){setprop(exp,uf_node_token,(void *)(uf_node=uf_make(exp)));};
  print_uf_node(uf_node);
}
  
void uf_equate_exps(expptr exp1, expptr exp2){
  uf_ptr uf_node1=(uf_ptr)getprop(exp1,uf_node_token,NULL);
  uf_ptr uf_node2=(uf_ptr)getprop(exp2,uf_node_token,NULL);
  if (uf_node1==NULL) {berror("First expression not known to uf");};
  if (uf_node2==NULL) {berror("Second expression not known to uf");};
  uf_equate(uf_node1,uf_node2);
}


void uf_show_find(expptr exp){
  fprintf(stdout,"Expression uf_node before find:");
  uf_show_exp(exp);
  uf_ptr uf_node=(uf_ptr)getprop(exp,uf_node_token,NULL);
  if (uf_node==NULL) {berror("Expression not known to uf");};
  uf_find(uf_node);
  fprintf(stdout,"Expression uf_node after find:");
  uf_show_exp(exp);
}

/* int main(int argc, char **argv){ */
/*   mcA_init(); */
/*   mcB_init(); */
/*   mcC_init(); */
/*   mcD_init(); */
/*   mcE_init1(); */
/*   mcE_init2(); */
/*   uf_init(); */

/*   uf_node_token = string_symbol("uf_node"); */

/*   while(1){ */
/*     catch_error({ */
/*         fprintf(stdout, "UF>"); */

/* 	expptr e = read_from_terminal(); */

/*         ucase {e; */
/*           {quit}:{break} */
/*           {show(!exp)} : {uf_show_exp(exp);} */
/*           {equate(!exp1,!exp2)} : {uf_equate_exps(exp1,exp2);} */
/*           {find(!exp)} : {uf_show_find(exp);} */
/*         }})} */
/*   return 0; */
/* } */


expptr load(expptr forms);

void MC_doit(expptr e){
  fputc('\n',stdout);
  pprint(load(append(preamble,append(init_forms,cons(e,NULL)))),stdout,0);
}

int rep_column;

void read_eval_print(){
  rep_column += 3;
  while(1){
    catch_error({
	indent(rep_column);
	fprintf(stdout, "MC> ");
	preamble = NULL;
	init_forms = NULL;
	expptr e = macroexpand(read_from_terminal());
	if(e == NULL)continue;
	ucase{e;
	  {quit}:{if(rep_column == 0)break; else throw_error();}
	  {continue}:{if(rep_column != 0)break;}
	  {describe(?sym)}:{
	    indent(rep_column);
	    pprint(getprop(sym,`{declaration},NULL),stdout,rep_column);}
	  {!s;}:{MC_doit(e);}
	  {{!s}}:{MC_doit(e);}
	  {?type ?f(!args){!body}}:{MC_doit(e);}
	  {!e}:{MC_doit(`{return ${e};})}
	}})
      }
  rep_column -=3;
}

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  install_base(`{test_uf.h}); /* provide definition info for load */
  rep_column = -3;
  
  if(error_flg != 0)return error_flg;

  catch_error({insert_base()})  /* load symbol table for all definitions, requires expand to know new defs */

  read_eval_print();
  return 0;
}
