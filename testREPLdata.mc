expptr bar(int i);
/** 1: compilation error **/

expptr foo(int i){
  if(i == 0){return `{foo};}
  return bar(--i);}
/** 2: compilation error **/

expptr bar(int i){
  if(i == 0){return `{bar};}
  return foo(--i);}

/** 4: done **/

expptr bar(){return foo();}
/** 2: done **/

bar()
/** 5: two **/

`{foo}
/** 3: foo **/

fooint x[10];
/** 2: done **/

for(int i = 0; i < 10; i++)x[i] = i;
/** 3: done **/

for(int i = 0; i < 10; i++)fprintf(stdout,"%d",x[i]);
/** 4: 0123456789done **/

int_exp(x[5])
/** 5: 5 **/

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}
/** 6: 45 **/

typedef struct myexpstruct{
  char * label;
  struct myexpstruct * car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;
/** 7: done **/

myexp mycons(char * s, myexp x, myexp y){
      myexp cell = malloc(sizeof(myexpstruct));
      cell->label = s;
      cell->car = x;
      cell->cdr = y;
      return cell;}
/** 8: done **/

expptr myexp_exp(myexp x){
  if(x == NULL) return string_atom("nil");
  return `{${string_atom(x->label)} ${myexp_exp(x->car)} ${myexp_exp(x->cdr)}};
}
/** 9: done **/

myexp_exp(mycons("foo",mycons("bar",NULL,NULL),NULL))
/** 10: foo bar nil nil nil **/

int y[0] = 2;
/** 11: done **/

y[0] += 1;
/** 12: done **/

int_exp(y[0])
/** 13: 3 **/

expptr friend[0] = `{Bob Givan};
/** 14: done **/

int height[0] = 6;
/** 15: done **/

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}
/** 16: My friend Bob Givan is 6 feet tall. **/

expptr e[0] = `{a+b};
/** 17: done **/

`{bar(${e[0]})}
/** 18: bar(a+b) **/

int numeralp(expptr x){
  if(!atomp(x))return 0;
  char * s= atom_string(x);
  for(int i = 0; s[i] != '\0'; i++){
    if(s[i] < '0' || s[i] > '9')return 0;}
  return 1;
}
/** 19: done **/

int value(expptr e){
  ucase{e;
   {$x+$y}:{return value(x)+value(y);}
   {$x*$y}:{return value(x)*value(y);}
   {($x)}:{return value(x);}
   {$z}.(numeralp(z)):{return atoi(atom_string(z));}}
  return 0;
}
/** 20: done **/

int_exp(value(`{5+2*10}))
/** 21: 25 **/

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym("rest");
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}
/** 22: done **/

macroexpand(`{mydolist(item,list){f(item);}})
/** 23: for
  (explist _genrest33=list;
   !atomp(_genrest33);
  _genrest33=cdr(_genrest33);
  ){expptr item=car(_genrest33);f(item);}
 **/

int_exp(value(`foo))
/** 24: compilation error **/

int_exp(value(`{foo}))
/** 25: execution error (running gdb) **/


