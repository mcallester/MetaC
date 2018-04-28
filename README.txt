McC is a C extension supporting the symbolic programming features of
Lisp and the interactive programming environment of scripting
languages such as Python.

The fundamental language features of interest are pattern
matching\footnote{Pattern matching is not a feature of Lisp but is
easily implemented given backquote and computed macros}, backquote and
computed macros. Pattern matching is familiar in many languages.
Backquote is a generalization of Lisp quotation supporting the
insertion of computed values into the quoted expression. Backquote
will be familiar to those versed in writing Lisp computed
macros. Pattern matching and backquote together allow one to express
rewriting.  To rewrite an expression ones used pattern matching to
bind variables to parts of the expression and uses backquote to
construct an expression using the values of the bound variables.
Computed macros allow arbitrary source code (Lisp code in the case of
Lisp) to be used in computing the expansion of a macro.  Computed
macros should be viewed as compilers.  A computed macro can do
sophisticated type inference or data-flow analysis as part of macro
expansion.

Packages of computed macros are often used as compilers for
sophisticated languages.  However, given that one is writing a
compiler, C is a preferable target language.  Experience with Lisp
indicates that for computed macros to work smoothly it is important
that the source language be the same as the target language.  The
target language should be C.

It is not obvious how to implement light weight quotation for C.
Parsing C is complex.  We bypass the syntactic complexity of C by
introducing ``universal algebraic syntax''.  Universal algebraic
syntax is a compromise between the extreme simplicity of Lisp syntax
and the desire for a more user-friendly algebraic syntax with features
like parenthesis-free expressions with standard operator precedence
conventions. While universal algebraic syntax has a variety of
user-friendly features it remains relatively simple --- it is
specified below in eight short bullet points.  The universal algebraic
syntax is universal in three senses.  First, as in Lisp, the syntactic
trees are universal in the sense that they can be assigned any desired
semantics.  Second, any semantics for the input character strings is
preserved in the conversion of strings to trees. In particular, C
semantics can be assigned to a tree by printing the tree as a string
and passing that string to a C compiler.  For this to work the
conversion of a string to an abstract syntax tree (the reader) must
preserve the string --- the printer must invert the reader.  The
reader can then be used for many languages other than C.  A third
sense of universality is that the reader makes very few assumptions
about the string to be read.  Any character string with balanced
parentheses, braces, brackets and quotations can be read to produce an
abstract syntax tree while preserving the information in the string.

See /manual/manual.pdf for more information.
