/* comment test*/

`a

`{a}

`{"say \"hi\""}

`{a:b}

`{foo(x)}

`{\${foo}}

`{\\${foo}}


expptr foo(){
  expptr bar = `barval;
  return `{$bar};}


`{foo(argxxxxxxxxxxx,argxxxxxxxxx,argxxxxxxx,argxxxxxx)}

expptr foo(){
  expptr bar = `barval;
  return `{\\${$bar}};}


