
Project: Desugar Python (Phase II)
==================================

In this project, you will further desugar more constructs in Python.

Assume Python has been parsed into the format described in the output of Phase
I.



Provided utilities
------------------

Two utilities have been provided to make writing this phase easier: the
[`pywalk`](https://github.com/mattmight/pywalk) package
and [`sxpy`](https://github.com/mattmight/sexp-to-python).


### pywalk

The `pywalk` package provides facilities for walking over Python abstract syntax
trees and making point transformations.


### sxpy

The `sxpy` utility converts a Python AST in s-expression form back into running
Python code.


### pydesugar

The `pydesugar` utility (provided on the class server) can perform any of the
desugarings (including Phase I desugarings) individually.  It accepts the
following command-line arguments:

  + `--p3`: perform all transformations from project 3
  + `--p4`: perform all transformations from project 4
  + `--insert-locals` or `-Il`
  + `--canonicalize-return`  or `-Cr`
  + `--canonicalize-exceptions` or `-Ce`
  + `--lift-decarators` or `-Lc`
  + `--lift-annotations` or `-La`
  + `--lift-defaults` or `-Ld`
  + `--eliminate-for` or `-Ef`
  + `--eliminate-with` or `-Ew`
  + `--eliminate-assert` or `-Ea`
  + `--eliminate-classes` or `-Ec`
  + `--flatten-assignments` or `-Fa`
  + `--normalize` or `-N`

### `pysugarp3_rkt.zo`

This shared library file contains all of the transformations from p3, and can
be imported with:

```
(require (file "/usr/local/share/python-compiler/pysugarp3_rkt.zo"))
```

on the class server.

(You can copy this file to your local machine as well.)

You can access all of the transformation primitives and pass them to pywalk:

```
insert-locals
canonicalize-return
lift-decorators
lift-annotations
lift-defaults
flatten-assignments
eliminate-for
eliminate-classes/expr
eliminate-classes/stmt
```

You can use all of these for this phase.


Turn-in requirements
--------------------

Running `make` in `$HOME/project-python-desugar-2/` should produce a file
called `pydesugar2`, and `pydesugar2` should be semantically equivalent to
`pydesugar --p4`.

It will be graded by running it through the transformer and ensuring that the
resulting program matches the behavior of the original program in the python
interpreter.

In other words, your output may differ from the output of `pydesugar --p4` as
long as it produces the same result.

It will then be checked to make sure it conforms to the subset of python
specified below.





Required transformations
------------------------




### Canonicalize exceptions

All `try`-`except` structures should have exactly one handler clause, and the
caught exception must be `BaseException`.


In general, a schema like:

```
try:
  <body>
except <ex1> as <name1>:
  <handler1>
except <ex2> as <name2>:
  <handler2>
...
except <exN> as <nameN>:
  <handlerN>
except:
  <otherwise>
```

becomes:

```
try:
  <body>
except BaseException as $tmp:
  if isinstance($tmp,<ex1>):
     <name1> = $tmp
     <handler1>
  elif isinstance($tmp,<ex2>):
     <name2> = $tmp
     <handler2>
  ...
  elif isinstance($tmp,<exN>):
     <nameN> = $tmp
     <handlerN>
  else:
     <otherwise> 
```

If no except clause is specified, then the `else` clause should raise `$tmp`
again.



### `with` elimination

With statements should be transformed into `try` blocks.

In general, code obeying this schema:

```
with <ctxt> as <o>:
   <body>
```

should become:

```
$tmp = <ctxt>
<o> = $tmp.__enter__()
try:
  <body>
except BaseException as $tmp2:
  $tmp1.__exit__(type($tmp2),$tmp2,$tmp2.__traceback__)
else:
  $tmp1.__exit__()
```


### `assert` elimination

Assert statements should be transformed into conditionals guarded by a test of
`__debug__`.


In general, code matching the schema:

```
assert <cond>
```

becomes:

```
if __debug__:
  if not <cond>:
    raise AssertionError()
```



### Normalize expressions

Normalization will force all complex expressions to become the right-hand-side
of an assignment statement.

An expression is _atomic_ if it is a variable, a numeric literal, a special
named constant (`True`, `False` or `None`) or an ellipsis (`...`).

An expression is complex if it is not atomic, yet all of its sub-expressions
are atomic.

Normalizing complex expressions typically involves assigning any complex
sub-expressions to variable names.

For example:

```
f(a + b, c + d)
```

could become:

```
$tmp2 = a + b
$tmp3 = c + d
f($tmp2,$tmp3)
```


In the course of normalization, several desugarings will become unlocked and
should be performed as well (to unlock more opportunities for normalization).

These desugarings include:

 + `lambda` elimination
 + Boolean expression elimination
 + `if` expression elimination
 + Generator elimination
 + Comprehension elimination
 + `yield from` elimination
 + Comparison simplification




#### `lambda` elimination

Once a `lambda` has been lifted out during normalization, it becomes eligible
for elimination.

For example:

```
f = lambda a,b: a + b
```

becomes:

```
def f(a,b):
  $tmp1 = a + b
  return $tmp
```


#### Boolean expression elimination

Once a Boolean expression has been normalized, it becomes eligible for
conversion into an `if` statement.

For example:

```
value = a and b and c
```

becomes:

```
if a:
  if b:
    value = c
  else:
    value = b
else:
  value = a
```


#### `if` expression elimination

Normalization converts `if`-expressions into `if` statements.


In general, code following this schema:

```
<lhs> = <on-true> if <cond> else <on-false>
```

becomes:

```
if <cond>:
   <lhs> = <on-true>
else:
   <lhs> = <on-false>
```


#### Generator elimination

Once generator expressions are normalized, it is possible to eliminate them by
converting them into a procedure containing nested `for`-loops.


For example, a generator like:

```
name = (x+1 for x in [1,2,3,4])
```

becomes:

```
def $tmp1():
  $tmp2 = [1,2,3,4]
  for x in $tmp2:
    $tmp4 = x + 1
    $tmp3 = yield $tmp4

name = $tmp()
```


And, if there are conditionals on several generators, 
then code like this:

```
pairs = ((x,y+1) for x in xlist if x > 0 for y in ylist)
```

becomes:

```
def $tmp1():
  for x in xlist:
    $tmp2 = x > 0
    if $tmp2:
      for y in ylist:
        $tmp5 = y + 1
        $tmp4 = (x,$tmp5)
        $tmp3 = yield ($tmp4)

pairs = $tmp1()
```


#### Comprehension elimination

Once generators are eliminable, list, set and dictionary comprehensions are
easy to eliminate as well.

For instance:

```
{ x+1 for x in [1,2,3] }
```

can become: 

```
set(x+1 for x in [1,2,3])
```

which may then be eliminated via the prior transformation.



#### `yield from` elimination

The `yield from` form should be desugared into a loop.


In general, the form:

```
<lhs> = yield from <expr>
```

should become

```
for $tmp in <expr>:
   yield $tmp
<lhs> = None
```



#### Comparison simplification

The comparison `x = a < b < c` is really equivalent to `x = (a < b) and (b < c)`.

So long as `b` has no side effects, this desugaring is safe.

But, for `x = a() < b() < c()` the same desugaring is not safe.

Instead, it should become:

```
$tmp1 = a()
$tmp2 = b()
$tmp3 = $tmp1 < $tmp2
if $tmp3:
   $tmp4 = c()
   x = $tmp2 < $tmp4
else:
   x = $tmp3
```







Simplifications
---------------

  1. The transformation need not be "error-raise-preserving."

     That is, you may opt to use a transformation that does not raise an
     `Error`-level exception when Python would have raised an `Error`-level
     exception.
  
     Rather than raise an error, your implementation may do anything.

  2. You may assume that the names in `builtins` are never assigned.  (This
     will make it easier to write hygienic transformations that want to 
     desugar into builtins.)  For example, you can assume `list` will always
     be the `list` class.





Output grammar
--------------

The output must conform to the following grammar:


```
<mod> ::= (Module <stmt>*)

<stmt> ::=

        (FunctionDef
          (name <identifier>)
          ; NOTE: <arguments> is simplified below.
          (args <arguments>) 
          (body <stmt>*)
          (decorator_list)
          (returns #f))

      | (Return <aexpr>) 

      | (Delete <cexpr>) 

      | (Assign (targets <cexpr>) (value <cexpr>)) 
      | (AugAssign <aexpr> <operator> <cexpr>)  

      | (While (test <aexpr>) (body <stmt>*) (orelse <stmt>*))

      | (If (test <aexpr>) (body <stmt>*) (orelse <stmt>*))

      | (Raise <aexpr>)  
      | (Raise <aexpr> <aexpr>)

      | (Try (body <stmt>*)
             (handlers <excepthandler>)
             (orelse <stmt>*)
             (finalbody <stmt>*))

      | (Import <alias>*)
      | (ImportFrom (module <identifier?>)
                    (names <alias>*)
                    (level <int?>))

      | (Global <identifier>+)
      | (Nonlocal <identifier>+)

      | (Pass) 
      | (Break)
      | (Continue)

      ;; Added:

      ; Use Local to specify variables assinged here:
      | (Local <identifier>+) 

      ; Use Comment to specify a comment (useful in debugging):
      | (Comment <string>)


<cexpr> ::=

     | (BinOp <aexpr> <operator> <aexpr>)
     | (UnaryOp <unaryop> <aexpr>)

     | (Dict (keys <aexpr>*) (values <aexpr>*))
     | (Set <expr>*)

     | (Yield <expr>)

     | (Compare (left        <aexpr>) 
                (ops         <cmpop>)
                (comparators <aexpr>))

     | (Call (func <aexpr>)
             (args <aexpr>*)
             (keywords <keyword>*)
             (starags <aexpr?>)
             (kwargs <aexpr?>))

     | (Str <string>)
     | (Bytes <byte-string>)

     ; the following expression can appear in assignment context:
     | (Attribute <aexpr> <identifier>)
     | (Subscript <aexpr> <slice>)
     | (List <aexpr>*)
     | (Tuple <aexpr>*)


<aexpr> ::= 
       (Num <number>)
     | (NameConstant <name-constant>)
     | (Ellipsis)
     | (Name <identifier>)



<name-constant> ::= True | False | None

<slice> ::= (Slice <aexpr?> <aexpr?> <aexpr?>)
         |  (ExtSlice <slice>*) 
         |  (Index <aexpr>)

<operator> ::= Add | Sub | Mult | Div | Mod | Pow | LShift 
               | RShift | BitOr | BitXor | BitAnd | FloorDiv

<unaryop> ::= Invert | Not | UAdd | USub

<cmpop> ::= Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

<excepthandler> ::= [except <expr> <identifier> <stmt>*]

<arguments> ::= (Arguments
                   (args <arg>*)
                   (arg-types #f*)
                   (vararg <arg?>) 
                   (kwonlyargs <arg>*)
                   (kwonlyarg-types #f*)
                   (kw_defaults #f*)
                   (kwarg <arg?>) 
                   (defaults #f*))
 
<arg> ::= <identifier>

<keyword> ::== [<identifier> <expr>]

<alias> ::= [<identifier> <identifier?>]

<arg?> ::= <arg> | #f

<expr?> ::= <expr> | #f

<int?> ::= <int> | #f

<identifier?> ::= <identifier> | #f
```



