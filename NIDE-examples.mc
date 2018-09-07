`{Hello World}

expptr f(expptr exp){return exp;}

f(`{a})

int x[10];

for(int i = 0; i < 10; i++)x[i] = i;

for(int i = 0; i < 10; i++)fprintf(stdout,"%d",x[i]);

int_exp(x[5])

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}

typedef struct myexpstruct{
  char * label;
  struct myexpstruct * car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;

myexp mycons(char * s, myexp x, myexp y){
      myexp cell = malloc(sizeof(myexpstruct));
      cell->label = s;
      cell->car = x;
      cell->cdr = y;
      return cell;}

expptr myexp_exp(myexp x){
  if(x == NULL) return string_atom("nil");
  return `{${string_atom(x->label)} ${myexp_exp(x->car)} ${myexp_exp(x->cdr)}};
}

myexp_exp(mycons("foo",mycons("bar",NULL,NULL),NULL))

int y[0] = 2;

y[0] += 1;

int_exp(y[0])

expptr friend[0] = `{Bob Givan};

int height[0] = 6;

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}

expptr e[0] = `{a+b};

`{bar(${e[0]})}

expptr g(expptr x){return x;}

g(`{a})

expptr g(expptr x){return `{$x $x};}

g(`{a})

expptr bar(int i);

expptr foo(int i){
  if(i == 0){return `{foo};}
  return bar(--i);}

expptr bar(int i){
  if(i == 0){return `{bar};}
  return foo(--i);}

foo(1)

expptr bar(int i){
  if(i == 0){return `{bar2};}
  return foo(--i);}

foo(1)

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym("rest");
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}

macroexpand(`{mydolist(item,list){f(item);}})

macroexpand(`{ucase{`{a;b};{\$a;\$any}:{return a;}}})

ucase{`{a;b};{$a;$any}:{return a;}}

int numeralp(expptr x){
  if(!atomp(x))return 0;
  char * s= atom_string(x);
  for(int i = 0; s[i] != '\0'; i++){
    if(s[i] < '0' || s[i] > '9')return 0;}
  return 1;
}

int value(expptr e){
  ucase{e;
   {$x+$y}:{return value(x)+value(y);}
   {$x*$y}:{return value(x)*value(y);}
   {($x)}:{return value(x);}
   {$z}.(numeralp(z)):{return atoi(atom_string(z));}}
  return 0;
}

int_exp(value(`{5+2*10}))

int_exp(value(`foo))

int_exp(value(`{foo}))

expptr bar(){
  breakpt("bar break");
  return `{a};
}

bar()

