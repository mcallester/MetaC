`{foo}
/** 1: foo **/

int x[10];
/** 2: done **/

for(int i = 0; i < 10; i++)x[i] = i;
/** 3: done **/

for(int i = 0; i < 10; i++)fprintf(stdout,"%d",x[i]);
/** 4: 0123456789done **/

int_exp(x[5])
/** 4: 5 **/

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}
/** 5: 45 **/

typedef struct myexpstruct{
  char * label;
  struct myexpstruct * car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;
/** 6: done **/

myexp mycons(char * s, myexp x, myexp y){
      myexp cell = malloc(sizeof(myexpstruct));
      cell->label = s;
      cell->car = x;
      cell->cdr = y;
      return cell;}
/** 7: done **/

expptr myexp_exp(myexp x){
  if(x == NULL) return string_atom("nil");
  return `{${string_atom(x->label)} ${myexp_exp(x->car)} ${myexp_exp(x->cdr)}};
}
/** 8: done **/

myexp_exp(mycons("foo",mycons("bar",NULL,NULL),NULL))
/** 9: foo bar nil nil nil **/

int y[0] = 2;
/** 10: done **/

y[0] += 1;
/** 11: done **/

int_exp(y[0])
/** 12: 3 **/

expptr friend[0] = `{Bob Givan};
/** 13: done **/

int height[0] = 6;
/** 14: done **/

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}
/** 15: My friend Bob Givan is 6 feet tall. **/

expptr e[0] = `{a+b};
/** 16: done **/

`{bar(${e[0]})}
/** 1: compilation error **/

int numeralp(expptr x){
  if(!atomp(x))return 0;
  char * s= atom_string(x);
  for(int i = 0; s[i] != '\0'; i++){
    if(s[i] < '0' || s[i] > '9')return 0;}
  return 1;
}
/** 1: done **/

int value(expptr e){
  ucase{e;
   {$x+$y}:{return value(x)+value(y);}
   {$x*$y}:{return value(x)*value(y);}
   {($x)}:{return value(x);}
   {$z}.(numeralp(z)):{return atoi(atom_string(z));}}
  return 0;
}
/** 2: done **/

int_exp(value(`{5+2*10}))
/** 3: 25 **/

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym("rest");
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}
/** 21: done **/

macroexpand(`{mydolist(item,list){f(item);}})
/** 22: for
  (explist _genrest33=list;
   !atomp(_genrest33);
  _genrest33=cdr(_genrest33);
  ){expptr item=car(_genrest33);f(item);}
 **/

int_exp(value(`{foo}))
/** 6: execution error (running gdb) **/
