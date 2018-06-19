#include "mc.h"

expptr foo(expptr x){
  ucase{x;
    {$f($x)}:{return `{${f},${x}};}
    {$a,$b}:{return `{${a},${b}};}}
  return NULL;
}   
