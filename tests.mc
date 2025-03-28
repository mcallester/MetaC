/** ========================================================================
there are still bugs in MetaC mostly involving load.  see bugs .txt
========================================================================**/

/** ========================================================================
Hello world
======================================================================== **/

string_atom("a")

`a

pointer_exp(`a)
/** non-break error (likely a segment fault) --- to resume type p NIDE()3:0x55555eb2f760 **/

/** ========================================================================
procedure definitions
========================================================================**/

expptr f(expptr exp){return exp;}

f(`a)

/** ========================================================================
 Imperative Programming
======================================================================== **/

int x[10];

for(int i = 0; i < 10; i++)x[i] = i;

int_exp(x[5])

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}


/** ========================================================================
  printing
======================================================================== **/

mcpprint(`a);

for(int i = 0; i < 10; i++)mcprint("%d",x[i]);


/** ========================================================================
 C data types
======================================================================== **/

typedef struct myexpstruct{
  char * label;
  struct myexpstruct * car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;

myexp mycons(charptr s, myexp x, myexp y){
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


/** ========================================================================
 Variable as x[0].
======================================================================== **/

int y[0] = 2;

y[0] += 1;

int_exp(y[0])

expptr friend[0] = `{Bob Givan};

int height[0] = 6;

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}

expptr e[0] = `{a+b};

`{bar(${e[0]})}


/** ========================================================================
 redefinition
======================================================================== **/

expptr g(expptr x){return x;}

expptr g2(expptr x){return g(x);}

g2(`a)

expptr g(expptr x){return `{$x $x};}

g2(`a)


/** ========================================================================
null redefinition of void function fails (not fixed)
========================================================================**/
expptr e[1];

void g(){e[0]=`a;}

g();

e[0]

e[0]=`b;

void g(){}

g();

e[0]

/** ========================================================================
 mutual recursion and redefinition
======================================================================== **/

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

/** ========================================================================
macros
======================================================================== **/

gensym(`x)

gensym(`x)

umacro{mydolist($x, $L){$body}}{
     expptr rest = gensym(`rest);
     return `{for(explist $rest = $L;
                  !atomp($rest);
                  $rest = cdr($rest);)
	 {expptr $x = car($rest); $body}};}

macroexpand(`{mydolist(item,list){f(item);}})


/** ========================================================================
 the "any" variable in patters
======================================================================== **/

macroexpand(`{ucase(`{a;b}){{\$a;\$any}:{return a;};}})

ucase(`{a;b}){{$a;$any}:{return a;};}

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

int value(expptr e){
  ucase(e){
    {$x+$y}:{return value(x)+value(y);};
    {$x*$y}:{return value(x)*value(y);};
    {($x)}:{return value(x);};
    {$z}.(numeralp(z)):{return atoi(atom_string(z));};};
  return 0;
}

int_exp(value(`{5+2*10}))


/** ========================================================================
 breakpoints 
======================================================================== **/
expptr barf(){
  breakpt("bar break");
  return `a;
}

barf()

/** ========================================================================
no arguments
======================================================================== **/

expptr foobar(){return `a;}

foobar()

/** ========================================================================
 Procedure definition failure should leave the procedure undefined
======================================================================== **/

expptr goo(expptr exp){returni exp;}
/** c compilation error **/

goo(`a)
/** c compilation error **/


/** ========================================================================
compilation error of type defintion should not leave trash in file_preamble
======================================================================== **/

typedef struct myexpstruct{
  char * label;
  myexp car;
  struct myexpstruct * cdr;
} myexpstruct, *myexp;
/** c compilation error **/

`a

/** ========================================================================
 procedure type declaration without procedure definition should result
 in a reasonable state.
======================================================================== **/

expptr goop(expptr exp);

goop(`a)
/** dynamic-check error **/

/** ========================================================================
 strange
======================================================================== **/

expptr test(){
  return NULL;//a comment here used to cause a problem
}

int y[0]; //a comment here used to cause a problem


/** ========================================================================
 segment fault
======================================================================== **/
expptr e[0];

e[0] = NULL;

e[0]->arg1
/** non-break error (likely a segment fault) --- to resume type p NIDE() **/


/** ========================================================================
a bad signature should not be preserved
========================================================================**/
notype bad(int x){return x;}
/** c compilation error **/

int bad(int x){return x;}

int_exp(bad(3))


/** ========================================================================
reader errors
======================================================================== **/

foo(lkj)))`{foo bar}
/** dynamic-check error **/

expptr friend[0] = ‘{Bob Givan};
/** c compilation error **/


/** ========================================================================
some version of the system failed to recover from this expansion error
======================================================================== **/

umacro{test2()}{return file_expressions("nonexistent_file");}
/** c compilation error **/

test2()
/** c compilation error **/

`a


/** ========================================================================
This example failed to behave in some version.

compilation error expected for foo2() because of intexp rather than int_exp

bar2() has the bug fixed.
======================================================================== **/

umacro{foo2()}{
  add_preamble(`{int z[0]=0;});
  return `{intexp(z[0])};}

foo2()
/** c compilation error **/

umacro{bar2()}{
  add_preamble(`{int z[0]=0;});
  return `{int_exp(z[0])};}

bar2()


/** ========================================================================
  NULL values
======================================================================== **/
0

`{}

/** ========================================================================
checking for resetting of undo freeptr.
========================================================================**/

restart_undo_frame(0);

setprop(`a,`b,`c);

getprop(`a,`b,NULL)

restart_undo_frame(0);

getprop(`a,`b,NULL)


/** ========================================================================
In a previous version the linker got confused when symbol indeces changed from the last undo state.
This is fixed by making symbol indeces permanent (not undone).
========================================================================**/

restart_undo_frame(0);

expptr foo3(){return `a;}

restart_undo_frame(0);

expptr foo4(){return `b;}

expptr foo3(){return `a;}

foo3()


/** ========================================================================
The following is a test of dlopen(RTLD_NOW | RTDL_DEEPBIND) which
causes the DLL to give top priority to its own symbol definitions when linking
the DLL code.  with dlopen(RTDL_NOW | RTDL_GLOBAL) the last cell used to return `b
because the procedure in the DLL linked to the previous version of bar which
which fewer arguments.  This happened even though the DLL contained the correct
version of bar for the new signature.
========================================================================**/

umacro{mention($x)}{
  return `{if($x ? $x : $x){}};}

restart_undo_frame(1);

expptr bar5(expptr x1, expptr x2);

restart_undo_frame(1);

expptr bar5(expptr x1, expptr x2, expptr x3){
  mention(x1);
  mention(x2);
  return x3;
  }

bar5(`a,`b,`c)

/** ========================================================================
catch and throw
======================================================================== **/

declare_exception(ex1());

//the following should generate "uncaught throw".
throw(ex1());
/** uncaught throw **/

void foob(){value2[0] = `b;}

void catch_ex1_b(){
  catch(ex1()){foob();}{value2[0] = `a;};
  }

catch_ex1_b();

value2[0]

void foothrow(){throw(ex1());}

void catch_test_throw(){
  catch(ex1()){foothrow();}{value2[0] = `a;};
  }

catch_test_throw();

value2[0]


declare_exception(bar(expptr));

void catch_test3(){
  catch(bar(e)){value2[0]= `a;}{value2[0]=e;};
  }

catch_test3();

value2[0]

void foo5(){
  throw(bar(`b));
  }

void catch_test4(){
  catch(bar(e)){foo5();}{value2[0]=e;};
  }

catch_test4();

value2[0]


/** ========================================================================
load_test
========================================================================**/

load("nofile");
/** mc to c dynamic-check error **/

load("badfile");
/** mc to c dynamic-check error **/

explist_exp(file_expressions("badfile2.mc"))

load("badfile2");
/** c compilation error **/

load("include_test");

included(`a)

