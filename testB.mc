#include "mc.h"

ucase(e){{$foo($bar)}:{return `{$foo($bar)};};}

expptr foo(expptr x){
  ucase(f(x)){
    {$f($x)}:{return `{${f},${x}};};
    {$a,$b}:{return `{$a,$b};};
    {$x}.(x==car(x)):{return x;};
    }
  return NULL;
  }

