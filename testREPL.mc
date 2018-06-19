int x[10];

for(int i = 0; i < 10; i++)x[i] = i;

for(int i = 0; i < 10; i++)fprintf(stdout,"%d",x[i]);

int_exp(x[5])

{int sum = 0; for(int i = 0; i < 10; i++)sum += x[i]; return int_exp(sum);}

typedef struct expliststruct{
      expptr car;
      struct expliststruct * cdr;
    } expliststruct, *explist;

explist mycons(expptr x, explist l){
      explist cell = malloc(sizeof(expliststruct));
      cell->car = x;
      cell->cdr = l;
      return cell;}

expptr list_exp(explist l){
     if(l == NULL) return NULL;
     return `{ ${l->car} ${list_exp(l->cdr)} };
}

list_exp(mycons(`{foo},mycons(`{bar},NULL)))

int y[0] = 2;

y[0] += 1;

int_exp(y[0])

expptr friend[0] = `{Bob Givan};

int height[0] = 6;

`{My friend ${friend[0]} is ${int_exp(height[0])} feet tall.}

expptr x[0] = `{a+b};

`{bar(${x[0]})}

int value(expptr e){
  ucase{e;
   {!x+!y}:{return value(x)+value(y);}
   {!x*!y}:{return value(x)*value(y);}
   {(!x)}:{return value(x);}
   {?z}:{return symbol_int(z);}}
return 0;
}

int_exp(value(`{5+2*10}))

umacro{mydolist(?x, !L){!body}}{
     expptr rest = gensym(`{rest});
     return `{for(explist ${rest} = ${L};
                  ${rest} != NULL;
                  ${rest} = ${rest}->cdr;)
               {expptr ${x} = ${rest}->car; ${body}}}}

macroexpand(`{dolist(item,list){f(item);}})



	
