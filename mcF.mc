#include "mc.h"

explist file_preamble; // must avoid name clash with preamble in mcA.c
explist procedures;
explist arrays;


/** ========================================================================
install_base and install_value_properties

install_value_properties must be a macro because each installation needs to be
a seperately compiled line of code.

install_base must be called before inatall_value_properties is macro expanded.
Hence intall_base must be called in ExpandD main and ExpandD must depend on
base_decls.h in the makefile.

ExpandF main is

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcF_init();
  install_base();
  in_ide = 0;
  catch_all{mcexpand(argv[1],argv[2]);}{return -1;};
  }

NIDE main is

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcF_init();
  install_value_properties(); //this gets macro-expanded with the base installed
  NIDE_init(); //this does install_base
  in_ide = 1;
  IDE_loop();
  }

NIDE.mc is expanded by expandE.  The install_value_properties macro
in NIDE main will be expanded peoperly because
ExpandE has done inatall_base prior to expansion.  The resulting
installed procedure and array
pointers are the values they have in the NIDE executable.
======================================================================== **/

void install_preamble(expptr e){
  if(!getprop_int(e,`{installed},0)){
    explist_push(e,file_preamble);
    setprop_int(e,`{installed},1);}
}

void install_array(expptr e){
  if(!getprop_int(e,`{installed},0)){
    explist_push(e,arrays);
    setprop_int(e,`{installed},1);}
}

void install_procedure(expptr e){
  if(!getprop_int(e,`{installed},0)){
    explist_push(e,procedures);
    setprop_int(e,`{installed},1);}
}

void install_base(){
  explist_do(sig,file_expressions(sformat("%sbase_decls.h", MetaC_directory))){
    ucase(sig){
      {$type $f($args);}.(symbolp(type) && symbolp(f)):{
	setprop(f,`{base},`{true});
	explist_push(f,procedures);
	setprop(f,`{signature},sig);};
      {$type $x[$dim];}.(symbolp(type) && symbolp(x)):{
	explist_push(x,arrays);
	setprop(x,`{signature},sig);};
      {$e}:{explist_push(e,file_preamble);};}; //typedefs
    }
  }

umacro{install_value_properties()}{
  expptr result = `{};
  explist_do(f,procedures){
    result = `{setprop(`{$f},`symbol_value,$f) ; $result};}
  explist_do(X,arrays){
    result = `{setprop(`{$X},`symbol_value,$X) ; $result};}
  return result;
  }

declare_exception(NIDE());

init_fun(mcF_init)



