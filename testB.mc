#include "mc.h"

expptr foo(expptr x){
  ucase{x;
    {$f($x)}:{return `{${f},${x}};}
    {$a,$b}:{return `{$a,$b};}
    {$x}.(x==car(x)):{return x;}}
  
  return NULL;
}
