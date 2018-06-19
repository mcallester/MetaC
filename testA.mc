#include "mc.h"

expptr foo(){
  expptr bar = `{barval};
  return `{foo[3] "foo" ${bar} $bar ${`{bar}} \${foo} \\${foo}};}



