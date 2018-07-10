int x[10];
/** 26: done **/

for(int i = 0; i < 10; i++)x[i] = i;
/** 27: done **/

for(int i = 0; i < 10; i++)fprintf(stdout,"%d",x[i]);
/** 28: 0123456789done **/

int_exp(x[5])
/** 29: 5 **/

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}
/** 30: 45 **/

typedef struct myexpstruct{
  char * label;
  struct myexpstruct * car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;
/** 31: done **/

myexp mycons(char * s, myexp x, myexp y){
      myexp cell = malloc(sizeof(myexpstruct));
      cell->label = s;
      cell->car = x;
      cell->cdr = y;
      return cell;}
/** 32: done **/

expptr myexp_exp(myexp x){
  if(x == NULL) return string_atom("nil");
  return `{${string_atom(x->label)} ${myexp_exp(x->car)} ${myexp_exp(x->cdr)}};
}
/** 33: done **/

myexp_exp(mycons("foo",mycons("bar",NULL,NULL),NULL))
/** 34: foo bar nil nil nil **/

int y[0] = 2;
/** 35: done **/

y[0] += 1;
/** 36: done **/

int_exp(y[0])
/** 37: 3 **/

expptr friend[0] = `{Bob Givan};
/** 38: done **/

int height[0] = 6;
/** 39: done **/

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}
/** 40: My friend Bob Givan is 6 feet tall. **/

expptr e[0] = `{a+b};
/** 41: done **/

`{bar(${e[0]})}
/** 42: bar(a+b) **/

int numeralp(expptr x){
  if(!atomp(x))return 0;
  char * s= atom_string(x);
  for(int i = 0; s[i] != '\0'; i++){
    if(s[i] < '0' || s[i] > '9')return 0;}
  return 1;
}
/** 43: done **/

int value(expptr e){
  ucase{e;
   {$x+$y}:{return value(x)+value(y);}
   {$x*$y}:{return value(x)*value(y);}
   {($x)}:{return value(x);}
   {$z}.(numeralp(z)):{return atoi(atom_string(z));}}
  return 0;
}
/** 44: done **/

int_exp(value(`{5+2*10}))
/** 45: 25 **/

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym("rest");
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}
/** 46: done **/

macroexpand(`{mydolist(item,list){f(item);}})
/** 47: for
  (explist _genrest88=list;
   !atomp(_genrest88);
  _genrest88=cdr(_genrest88);
  ){expptr item=car(_genrest88);f(item);}
 **/

int_exp(value(`{foo}))
/** 48: Error: C-M-m will go to REPL in buffer *MetaC* **/

/lkdfj
