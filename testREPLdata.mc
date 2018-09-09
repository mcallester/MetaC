//
//
//
//
//

/** ========================================================================
 Hello world
======================================================================== **/

`{a}
/** 1: compilation error **/


/** ========================================================================
 Procedure defininition
======================================================================== **/

expptr f(expptr exp){return exp;}
/** 2: done **/

f(`{a})
/** 3: a **/

/** ========================================================================
 Imperative Programming
======================================================================== **/

int x[10];
/** 4: done **/

for(int i = 0; i < 10; i++)x[i] = i;
/** 5: done **/

for(int i = 0; i < 10; i++)fprintf(stdout,"%d",x[i]);
/** 6: 0123456789done **/

int_exp(x[5])
/** 7: 5 **/

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}
/** 8: 45 **/


/** ========================================================================
 C data types
======================================================================== **/

typedef struct myexpstruct{
  char * label;
  struct myexpstruct * car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;
/** 1: done **/

myexp mycons(char * s, myexp x, myexp y){
      myexp cell = malloc(sizeof(myexpstruct));
      cell->label = s;
      cell->car = x;
      cell->cdr = y;
      return cell;}
/** 2: done **/

expptr myexp_exp(myexp x){
  if(x == NULL) return string_atom("nil");
  return `{${string_atom(x->label)} ${myexp_exp(x->car)} ${myexp_exp(x->cdr)}};
}
/** 3: done **/

myexp_exp(mycons("foo",mycons("bar",NULL,NULL),NULL))
/** 4: foo bar nil nil nil **/


/** ========================================================================
 Variable as x[0].
======================================================================== **/

int y[0] = 2;
/** 13: done **/

y[0] += 1;
/** 14: done **/

int_exp(y[0])
/** 15: 3 **/

expptr friend[0] = `{Bob Givan};
/** 16: done **/

int height[0] = 6;
/** 17: done **/

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}
/** 18: My friend Bob Givan is 6 feet tall. **/

expptr e[0] = `{a+b};
/** 19: done **/

`{bar(${e[0]})}
/** 20: bar(a+b) **/


/** ========================================================================
 redefinition
======================================================================== **/

expptr g(expptr x){return x;}
/** 1: done **/

g(`{a})
/** 2: a **/

expptr g(expptr x){return `{$x $x};}
/** 3: done **/

g(`{a})
/** 4: a a **/

/** ========================================================================
 mutual recursion and redefinition
======================================================================== **/

expptr bar(int i);
/** 1: done **/

expptr foo(int i){
  if(i == 0){return `{foo};}
  return bar(--i);}
/** 2: done **/

expptr bar(int i){
  if(i == 0){return `{bar};}
  return foo(--i);}
/** 3: done **/

foo(1)
/** 4: bar **/

expptr bar(int i){
  if(i == 0){return `{bar2};}
  return foo(--i);}
/** 5: done **/

foo(1)
/** 6: bar2 **/

/** ========================================================================
 macros
======================================================================== **/

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym("rest");
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}
/** 1: done **/

macroexpand(`{mydolist(item,list){f(item);}})
/** 2: for
    (explist _genrest23=list;
     !atomp(_genrest23);
    _genrest23=cdr(_genrest23);
    ){expptr item=car(_genrest23);f(item);}
   **/


/** ========================================================================
 the "any" variable in patters
======================================================================== **/

macroexpand(`{ucase{`{a;b};{\$a;\$any}:{return a;}}})
/** 1: 
    {expptr _gentop1=cons(cons(string_atom("a"),string_atom(";")),string_atom("b"));
    expptr _gen5=_gentop1;
    if(cellp(_gen5))
      {expptr _gen2=car(_gen5);
      if(cellp(_gen2))
        {expptr _gen8=car(_gen2);
        
          {expptr a=_gen8;
          expptr _gen10=cdr(_gen2);
          if(_gen10==string_atom(";")){return a;goto _gendone1;}
          }
        }
      }
    match_failure
      (_gentop1,
      cons
        (intern_paren
          ('{',
          cons
            (cons(cons(string_atom("$"),string_atom("a")),string_atom(";")),
            cons(string_atom("$"),string_atom("any")))),
        string_atom("")));
    _gendone1: ;
    }
   **/

ucase{`{a;b};{$a;$any}:{return a;}}
/** 2: a **/

/** ========================================================================
 various
======================================================================== **/

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

int_exp(value(`foo))
/** 4: compilation error **/

int_exp(value(`{foo}))
/** 3: execution error **/

expptr bar(){
  breakpt("bar break");
  return `{a};
}
/** 5: done **/

bar()
/** 6: a **/

/** ========================================================================
 no arguments
======================================================================== **/

expptr f(){return `{a};}
/** 1: done **/

f()
/** 2: a **/

/** ========================================================================
 Procedure definition failure should not leave the procedure semi-defined.
 the following should generate a compilation error on both invocations rather than
 a segment fault on the second invocation.
======================================================================== **/

expptr g(expptr exp){returni exp;}
/** 3: compilation error **/

g(`{a})
/** 4: execution error (running gdb) **/


/** ========================================================================
 strange
======================================================================== **/

expptr test(){
  return NULL;//a comment here used to cause a problem
}
/** 5: done **/

int x[0]; //a comment here used to cause a problem
/** 6: done **/


/** ========================================================================
 null expressions in file_expressions
======================================================================== **/

expptr parenthesize(expptr l){
  if(!cellp(l))return l;
  return cons(`{(${car(l)})}, parenthesize(cdr(l)));
}
/** 1: done **/

parenthesize(file_expressions("test1.mc"))
/** 3: (int f(int x){return x+1;})(int g(int x){return f(x+1);})(int h(int x){return x;}) **/


/** ========================================================================
 printing
======================================================================== **/

mcpprint(`{foo});
/** 1: done **/


/** ========================================================================
 segment fault test
======================================================================== **/
expptr e[0];
/** 2: done **/

e[0] = NULL;
/** 3: done **/

e[0]->arg1
/** 4: execution error **/


/** ========================================================================
incldue
======================================================================== **/
#require("include_test")
/** 1: include_test.mc Provided **/

included(`{a})
/** 2: a **/
