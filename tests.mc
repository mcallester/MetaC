//
//
//
//
//

/** ========================================================================
 Hello world
======================================================================== **/

`{a}
/** 1: a **/


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
/** 10: done **/

expptr myexp_exp(myexp x){
  if(x == NULL) return string_atom("nil");
  return `{${string_atom(x->label)} ${myexp_exp(x->car)} ${myexp_exp(x->cdr)}};
}
/** 11: done **/

myexp_exp(mycons("foo",mycons("bar",NULL,NULL),NULL))
/** 12: foo bar nil nil nil **/


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
/** 21: done **/

g(`{a})
/** 22: a **/

expptr g(expptr x){return `{$x $x};}
/** 23: done **/

g(`{a})
/** 24: a a **/

/** ========================================================================
 mutual recursion and redefinition
======================================================================== **/

expptr bar(int i);
/** 25: done **/

expptr foo(int i){
  if(i == 0){return `{foo};}
  return bar(--i);}
/** 26: done **/

expptr bar(int i){
  if(i == 0){return `{bar};}
  return foo(--i);}
/** 27: done **/

foo(1)
/** 28: bar **/

expptr bar(int i){
  if(i == 0){return `{bar2};}
  return foo(--i);}
/** 29: done **/

foo(1)
/** 30: bar2 **/

/** ========================================================================
 macros
======================================================================== **/

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym("rest");
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}
/** 31: done **/

macroexpand(`{mydolist(item,list){f(item);}})
/** 32: for
    (explist _genrest10=list;
     !atomp(_genrest10);
    _genrest10=cdr(_genrest10);
    ){expptr item=car(_genrest10);f(item);}
   **/


/** ========================================================================
 the "any" variable in patters
======================================================================== **/

macroexpand(`{ucase{`{a;b};{\$a;\$any}:{return a;}}})
/** 33: 
    {expptr _gentop33=cons(cons(string_atom("a"),string_atom(";")),string_atom("b"));
    expptr _gen59=_gentop33;
    if(cellp(_gen59))
      {expptr _gen25=car(_gen59);
      if(cellp(_gen25))
        {expptr _gen50=car(_gen25);
        
          {expptr a=_gen50;
          expptr _gen37=cdr(_gen25);
          if(_gen37==string_atom(";")){return a;goto _gendone51;}
          }
        }
      }
    match_failure
      (_gentop33,
      cons
        (intern_paren
          ('{',
          cons
            (cons(cons(string_atom("$"),string_atom("a")),string_atom(";")),
            cons(string_atom("$"),string_atom("any")))),
        string_atom("")));
    _gendone51: ;
    }
   **/

ucase{`{a;b};{$a;$any}:{return a;}}
/** 34: a **/

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
/** 35: done **/

int value(expptr e){
  ucase{e;
   {$x+$y}:{return value(x)+value(y);}
   {$x*$y}:{return value(x)*value(y);}
   {($x)}:{return value(x);}
   {$z}.(numeralp(z)):{return atoi(atom_string(z));}}
  return 0;
}
/** 36: done **/

int_exp(value(`{5+2*10}))
/** 37: 25 **/

int_exp(value(`foo))
/** 38: compilation error **/

int_exp(value(`{foo}))
/** 39: dynamic-check execution error **/

expptr barf(){
  breakpt("bar break");
  return `{a};
}
/** 40: done **/

barf()
/** 41: a **/

/** ========================================================================
 no arguments
======================================================================== **/

expptr foobar(){return `{a};}
/** 42: done **/

foobar()
/** 43: a **/

/** ========================================================================
 Procedure definition failure should leave the procedure undefined
======================================================================== **/

expptr goo(expptr exp){returni exp;}
/** 44: compilation error **/

goo(`{a})
/** 45: compilation error **/


/** ========================================================================
 procedure type declaration without procedure definition should result
 in a reasonable state.
======================================================================== **/

expptr goop(expptr exp);
/** 46: done **/

goop(`{a})
/** 47: dynamic-check execution error **/

/** ========================================================================
 strange
======================================================================== **/

expptr test(){
  return NULL;//a comment here used to cause a problem
}
/** 48: done **/

int y[0]; //a comment here used to cause a problem
/** 49: done **/


/** ========================================================================
 file_expressions
======================================================================== **/

expptr parenthesize(expptr l){
  if(!cellp(l))return l;
  return cons(`{(${car(l)})}, parenthesize(cdr(l)));
}
/** 50: done **/

parenthesize(file_expressions("file-expressions-test-file.mc"))
/** 51: (int f(int x){return x+1;})(int g(int x){return f(x+1);})(int h(int x){return x;}) **/

/** ========================================================================
 printing
======================================================================== **/

mcpprint(`{foo});
/** 52: done **/


/** ========================================================================
 segment fault
======================================================================== **/
expptr e[0];
/** 53: done **/

e[0] = NULL;
/** 54: done **/

e[0]->arg1
/** 55: gdb-trapped execution error **/


/** ========================================================================
 load
======================================================================== **/
load("include_test")
/** 56: include_test.mc Provided **/

included(`{a})
/** 57: a **/


/** ========================================================================
 expansion error
======================================================================== **/

dolist{}{}
/** 58: macro expansion error **/


/** ========================================================================
 exp_from_undo_frame
======================================================================== **/

exp_from_undo_frame(`{a})
/** 59: a **/


/** ========================================================================
 illegal signature should not be installed
======================================================================== **/

notype bad(int x){return x;}
/** 60: compilation error **/

int bad(int x){return x;}
/** 61: done **/

int_exp(bad(3))
/** 62: 3 **/


/** ========================================================================
reader errors
======================================================================== **/

foo(lkj)))`{foo bar}
/** 63: reader error **/

expptr friend[0] = ‘{Bob Givan};
/** 64: reader error **/


/** ========================================================================
some version of the system failed to recover from this expansion error
======================================================================== **/

umacro{test()}{return file_expressions("nonexistent_file");}
/** 65: done **/

test()
/** 66: macro expansion error **/


/** ========================================================================
 preamble bug
======================================================================== **/

umacro{foo()}{
  add_preamble(`{int z[0]=0;});
  return `{intexp(z[0])};}
/** 67: done **/

foo()
 // compilation error expected
/** 68: compilation error **/

umacro{bar()}{
  add_preamble(`{int z[0]=0;});
  return `{int_exp(z[0])};}
/** 69: done **/

bar()
/** 70: 0 **/


/** ========================================================================
 funky recursive test.
======================================================================== **/

file_expressions("tests.mc")
/** 71: reader error **/

0
/** 72: value prints as empty string **/

`{}
/** 73: value prints as empty string **/

`{value prints as empty string}
/** 74: value prints as empty string **/

nil()
/** 75: value prints as empty string **/
