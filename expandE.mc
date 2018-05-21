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
loading a set of declarations and procedure definitions.
========================================================================**/

expptr preamble;
expptr env_syms;
int REPdef_count;

void load(expptr forms);
void eval(expptr statement);
void preprocess(expptr form);

int main(int argc, char **argv){
  mcA_init();
  mcB_init();
  mcC_init();
  mcD_init();
  mcE_init1();
  mcE_init2();
  catch_error(mapc(install,file_expressions(`{base_decls.h})));
  if(error_flg != 0)return error_flg;
  catch_error({mcexpand(argv[1], argv[2]);});
  return error_flg;
}
