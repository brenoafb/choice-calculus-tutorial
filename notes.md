# Choice Calculus
## Choice Calculus Tutorial

- The Choice Calculus is a way of representing variability in programs
- Variability appears in specific points and is classified according to a dimension

### Motivating Example

Consider a function `twice` that doubles a number. We can implement such
function by either summing the parameter with itself or by multiplying
the parameter by 2.

``` haskell
twice x = x + x
twice x = 2 * x
```

We can also name our parameter however we like.

``` haskell
twice y = y + y
twice y = 2 * y
```

So we identified two dimensions of variability:
- The implementation of the function
- The naming of the parameter

In the choice calculus, we can represent the following concept as follows

```
dim Par<x,y> in
dim Impl<plus,times> in
twice Par<x,y> = Impl<Par<x,y> + Par<x,y>, 2*Par<x,y>>
```

If we select a particular tag for a certain dimension, we can eliminate it.
Say we pick the tag *y* for the *Par* dimension. As a result we now have

```
dim Impl<plus,times> in
twice y = Impl<y + y, 2*y>
```

### Sharing

We can also denote dependent dimensions by nesting shared parameters.

For conciseness, we can implement function in Haskell in pointfree style.
If we don't pick the pointfree implementation, we can name the parameter
in multiple ways.

```haskell
twice = \x -> 2*x   twice = \y -> 2*y
twice = (2*)
```

This choice can be captured as follows.

```
dim Pointfree<yes,no> in
twice v = Pointfree<(2*),
                    share v = (dim Par<x,y> in Par<x,y>) in \v -> 2 * v>
```

Note that, in the choice calculus, sharing is expanded *after* dimensions
and choices are resolved.
This is different than in the Haskell implementation presented later, in
which sharing is expanded *before* (view Exercise 9)

#### Exercise 1

Add a new dimension *FunName* that is used to vary the name of the function
between `twice` and `double`.

```
dim Par<x,y> in
dim Impl<plus,times> in
dim FunName<twice,double> in
FunName<twice,double> Par<x,y> = Impl<Par<x,y> + Par<x,y>, 2*Par<x,y>>
```

#### Exercise 2

```
dim Impl<plus, times> in
share v = (dim Par<x,y,z> in Par<x,y,z>) in
twice v = Impl<v+v, 2*v>
```

Extend the above choice calculus expression to include a second function
`thrice` that triples the value of its input, and that varies synchronously in the same
dimensions as twice. That is, a selection of *Impl.plus* and *Par.x*
(followed by share-variable expansion) should produce the following expression.

```
twice x = x + x
thrice x = x + x + x
```

```
dim Impl<plus, times> in
share v = (dim Par<x,y,z> in Par<x,y,z>) in
twice v = Impl<v+v, 2*v>
thrice v = Impl<v+v+v, 3*v>
```

#### Exercise 3

Modify the expression developed in Exercise 2 so that the implementation
methods of the two functions vary independently. (Hint: Since dimensions are locally
scoped, you can reuse the dimension name Impl.) Finally, extend thrice’s Impl
dimension to include an option that implements thrice in terms of twice.

```
dim Impl<plus, times> in
share v = (dim Par<x,y,z> in Par<x,y,z>) in
twice v = Impl<v+v, 2*v>
thrice v = Impl<v+v+v, 3*v>
```

```
share v = (dim Par<x,y,z> in Par<x,y,z>) in
dim Impl<plus, times> in
twice v = Impl<v+v, 2*v>
dim Impl<plus, times, twice> in
thrice v = Impl<v+v+v, 3*v, twice(v) + v>
```

### Syntax

The syntax of choice calculus is very simple

```
e ::= a-<e,...,e>-           Object Structure
    | dim D<t,...,t> in e    Dimension
    | D<e,...,e>             Choice
    | share v = e in e       Sharing
    | v                      Reference
```

#### Exercise 4

Which of the following are syntactically valid choice calculus expressions?

```
(a) dim D<t1,t2,t3> in (dim D<t1,t2> in D<e1,e2,e3>)

Not valid since the choice in D (with 3 choices)
refers to the outer definition of D,
which is shadowed by the inner definition (with 2 options)

(b) share v = D<e1,e2> in (dim D<t1,t2> in v)

Not valid since the choice dimension is used 'before' it is declared

(c) dim D<t1,t2,t3> in (share v = D<e1,e2,e3> in (dim D<t1,t2> in v))

Valid
```

### Choice Elimination

- **Plain expression**: Expression without variability
- **Decision**: Selection of one or more **tags**
- **Complete decision**: a decision that eliminates all choice

#### Exercise 5

Given `e = dim A<a1, a2> in A<A<1,2>,3>`, what is the result of selection `{ e } A.a1`?
Is it possible to select the plain expression `2`?

```
{ e } A.a1 = 1
```

It is not possible to select `2`, since that branch in the selection is
unreachable.

### Semantics

Semantics are denoted using Oxford brackets: `[| e |]` denotes the semantics
of expression `e`.

We can think of choice calculus expressions as denoting a set of plain expressions.
These expressions can be indexed by the selection of tags whose elimination
results in that expression.

- **Tag Ordering Constraint**: Tags must be selected from dimensions in a fixed order.

Example:

```
[| dim A<a1,a2> in A<1, dim B<b1,b2> in B<2,3>> |] =
  { (A.a1, 1)
  , ((A.a2, B.b1), 2)
  , ((A.a2, B.b2), 3)
  }
```

Note that `A` tags are always selected before `B` tags.

#### TODO Exercise 6

```
dim A<a1,a2> in A<1, dim B<b1,b2> in B<2,3>>
```
Write the semantics of the above expression if the tag ordering
constraint is removed

#### TODO Formal definition

p. 9


### Haskell Implementation

```haskell
data V a = Obj a
         | Dim Dim [Tag] (V a)
         | Chc Dim [V a]
```

where

```
type Dim = String
type Tag = String
```

Here we have a direct correspondence between some choice calculus
structures and constructors.

```
Obj a      <-> a-<e,...,e>-           Object Structure
Dim d ts v <-> dim D<t,...,t> in e    Dimension
Chc d vs   <-> D<e,...,e>             Choice
```

Examples

```
dim A<a1,a2> in A<1, dim B<b1,b2> in B<2,3>>

Dim "A" ["a1", "a2"]
    ( Chc "A"
      [ 1
      , Dim "B" ["b1", "b2"]
            ( Chc "b" [1, 2] )
      ]
```

#### Sharing vs let/where

##### Exercise 9

Compare the smantics of the following expression if we expand sharing before
dimensions and choices are resolved(Haskell),
with the semantics if we expand sharing after dimentions and choices are
resolved (choice calculus).

```
share v = (dim A<a1, a2> in A<1,2>) in (v, v)

Before:
share v = (dim A<a1, a2> in A<1,2>) in (v, v)
(dim A<a1, a2> in A<1,2>, dim A<a1, a2> in A<1,2>)

Picking A.a1:
(1, 1)

Picking A.a2:
(2, 2)

Note: I'm not sure whether having two dim A declarations is invalid.
The beginning of section 3 talks about dimension and tag rules, but I
didn't find anything mentioning whether we can have the same dimension
name in different subexpressions.

After:
share v = (dim A<a1, a2> in A<1,2>) in (v, v)

Picking A.a1

share v = 1 in (v, v)
-> (1, 1)

Picking A.a2

share v = 2 in (v, v)
-> (2, 2)
```

### Functor

```haskell
data V a = Obj a
         | Dim Dim [Tag] (V a)
         | Chc Dim [V a]

instance Functor V where
  fmap f (Obj x)       = Obj $ f x
  fmap f (Dim d ts vs) = Dim d ts $ fmap f vs
  fmap f (Chc d vs)    = Chc d $ fmap (fmap f) vs
```

#### Exercise 10

```
ab = dimA $ chcA [dimB $ chcB [Obj 1, Obj 2], Obj 3]
dim A<a1,a2> in A<dim B<b1,b2> in B<1,2>,3>
```

Write an expression that maps every integer `i` in `ab`
to a choice between `i` and `i+1`.
What is the type of the resulting value?

```
fmap (\i -> Dim "Inc" ["no", "yes"]
                $ Chc "Inc" [Obj i, Obj i+1]
     ) ab
```

The resulting object has type `Num a => V (V a)`
(when we have the `Monad` instance, we can use `join` to eliminate
one level of `V`).

### Monad (, Applicative)

```haskell
data V a = Obj a
         | Dim Dim [Tag] (V a)
         | Chc Dim [V a]

instance Monad V where
  return = Obj
  Obj a     >>= f = f a
  Dim d t v >>= f = Dim d t (v >>= f)
  Chc d vs  >>= f = Chc d (map (>>= f) vs)
```

Since this tutorial was released, the `Functor -> Applicative -> Monad`
typeclass chain was introduced to GHC, so we have to write an Applicative
instance of `V`.

One quick solution is to exploit the fact that every Monad is also an Applicative.
So we can write a `bindV` function identical to `>>=` and write `<*>` in terms of it.

```haskell
bindV :: V a -> (a -> V b) -> V b
Obj a     `bindV` f = f a
Dim d t v `bindV` f = Dim d t (v >>= f)
Chc d vs  `bindV` f = Chc d (map (>>= f) vs)

instance Applicative V where
  pure = Obj
  mf <*> mx = mf `bindV` (\f -> mx `bindV` (\x -> pure $ f x))

instance Monad V where
  (>>=) = bindV
```

There is a problem when implementing the Monad instance without implementing
the Applicative instance in the current version

One application of the Monad instance is to easily introduce variability in variable
expressions.

Recall the `ab` presented earlier.

```
ab = dimA $ chcA [dimB $ chcB [Obj 1, Obj 2], Obj 3]
dim A<a1,a2> in A<dim B<b1,b2> in B<1,2>,3>
```

Suppose we want to introduce a new dimension `S` which defines whether
we want the values squared.
We can easily do this as follows.

```
Dim "S" ["yes", "no"] $ ab >>= (\i -> Chc "S" [Obj i, Obj (i*i)])
dim S<yes, no> in
  dim A<a1, a2> in
    A<dim B<b1, b2> in
        B<S<1, 1>, S<2, 4>>,
      S<3, 9>>
```

Or, using `do` notation.

```haskell
f x = do
  i <- x
  Chc "S" [Obj i, Obj (i*i)]

Dim "S" ["yes", "no"] $ f ab
```

Most importantly, the monadic instance allows us to work with variable values
as if they were plain values.

### Variational Lists

```haskell
type VList a = V (List a)

data List a = Cons a (List a)
            | Empty
            | VList (VList a)
```

#### Exercise 11

The function `vcons` shown in Figure 4 adds a single (non-variational)
element to a variational list. Define a function `vvcons` that adds
a choice (that is, a variational element) to a variational list.
(Hint: Since you have to deal with two occurrences of the `V` constructor,
you might want to exploit the fact that `V` is a monad.

```haskell
vvcons :: V a -> VList a -> VList a
vvcons v vs = do
  x <- v    -- x  :: a
  xs <- vs  -- xs :: List a
  pure $ Cons x xs
```

#### Exercise 12

```haskell
dessert :: Menu
dessert = atomic "Dessert" ["yes","no"] [vsingle Cake, vempty]

menu :: Menu
menu = atomic "Main" ["meat","pasta"]
              [vlist [Steak, Fries], Pasta `vcons` dessert]
```

Change the definition of `menu` so that we can choose dessert also for a meat
main course. There are two ways of achieving this change:

(a) by copying the `dessert` dimension expression into the other choice, or
(b) by lifting the dimension declaration out of the main choice.

```haskell
menu' :: Menu
menu' = atomic "Main" ["meat","pasta"]
               [ Steak `vcons` (Fries `vcons` dessert)
               , Pasta `vcons` dessert]

menu'' :: Menu
menu'' = atomic "Main" ["meat","pasta"]
                [ vcat (vlist [Steak, Fries]) dessert
                , Pasta `vcons` dessert]

```

#### Exercise 13

Implement the function

```haskell
sumL :: List Int -> V Int
```

using pattern matching and recursion.
Then define the function

```haskell
vsum :: List Int -> V Int
```

```haskell
sumL :: List Int -> V Int
sumL = fold (+) 0

vsum :: VList Int -> V Int
vsum = join . fmap sumL
```

#### Exercise 14

Define the function `rev` for reversing expanded lists.
You may want to use the function `cat` in your definition.
Also provide a definition of the function `vrev` for
reversing variational lists.
Before testing your implementation, try to predict what
the result of the expression `vrev menu` should be.

```haskell
rev :: List a -> List a
rev Empty = Empty
rev (Cons x xs) = (rev xs) `cat` (single x)
rev (VList vl) = VList $ vl >>= (pure . rev)

vrev :: VList a -> VList a
vrev vs = vs >>= (pure . rev)
```

#### Exercise 15

Define the function `filterL :: (a -> Bool) -> List a -> List a` and
give a definition for the corresponding function `vfilter` that
operates on variational lists.

```haskell
filterL :: (a -> Bool) -> List a -> List a
filterL p Empty = Empty
filterL p (Cons x xs)
  | p x       = Cons x $ filterL p xs
  | otherwise = filterL p xs
filterL p (VList vl) = VList $ vl >>= (pure . filterL p)

vfilter :: (a -> Bool) -> VList a -> VList a
vfilter p vs = vs >>= (pure . filterL p)
```

#### Exercise 16

Implement the function `zipL :: List a -> List b -> List (a,b)` and
give a definition for the corresponding function `vzip` that
operates on variational lists.

```haskell
zipL :: List a -> List b -> List (a,b)
zipL Empty _ = Empty
zipL _ Empty = Empty
zipL (Cons x xs) (Cons y ys) = Cons (x, y) $ zipL xs ys
zipL (VList xs)  (VList ys)  = VList $ zipL <$> xs <*> ys
zipL xs (VList ys)  = VList $ ys >>= (pure . zipL xs)
zipL (VList xs)  ys = VList $ xs >>= (\xs' -> pure $ zipL xs' ys)

vzip :: VList a -> VList b -> VList (a, b)
vzip xs ys = zipL <$> xs <*> ys
```

### Edit Operations for Variational Lists (5.3)

This section describes how we may modify existing variational
list expressions in to evolve a variational data structure.

One example of such an operation is that of *hoisting* a dimension
declaraction.

Recall the menu example. The hoist operation would allow us to
pull the `Dessert` dimension declaration toward the beginning of
the data structure.

```haskell
> hoist "Dessert" menu
dim Dessert<yes,no> in dim Main<meat,pasta> in
    Main<[Steak;Fries],[Pasta;Dessert<[Cake],[]>]>
```

The implementation of this operation relies on zippers
and the SYB library.

#### Exercise 17

The implementation of `safeHoist` prevents the lifting of the dimension `d`
if this would cause the capture of `d` choices.

(a) What other condition would cause, at least in principle, the hoisting of a dimension
    to be unsafe (in the sense of changing the semantics of the variational list?)

    If there are multiple dimensions with the same name.

(b) Why don't we have to check for this condition in the implementation of `safeHoist`?
    (*Hint*: Revisit the description of how `find` works.)

    `find` takes the topmost, leftmost, expression that satisfies the given predicate.


#### TODO Exercise 18

The implementation of `prioritize` assumes that the choice to be lifted
is located in the second alternative of the choice in `a`.
Generalize the implementation of prioritize so that the choice in `b`
can be lifted out of either alternative.

### Variational Haskell

Section 6 uses the choice calculus DSL to introduce variability
into the Haskell language itself.
Although a bit more complicated, the transformation of a Haskell AST into a
variable one is similar to the transformation of a plain list into a variable list.

Given a data declaration representing the Haskell AST, we derive the variational
version by adding a type alias and new data enumeration to the original type.

To illustrate

```haskell
data Haskell = App Haskell Haskell
             | Var Name
             | Val Int
             | Fun Name [Haskell] Haskell Haskell
               ...
```

becomes

```haskell
type VHaskell = V Haskell

data Haskell = App Haskell Haskell
             | Var Name
             | Val Int
             | Fun Name [Haskell] Haskell Haskell
               ...
             | VHaskell VHaskell
```

To carry out this section, I picked the syntax of a
[Lisp implementation of mine](https://github.com/brenoafb/haskell-lisp)
and added variability.

```haskell
type VExpr = V Expr

data Expr = Atom       T.Text
          | Str        T.Text
          | IntExpr    Int
          | DoubleExpr Double
          | Quote      Expr
          | List       [Expr]
          | VExpr      VExpr
          deriving Data
```

To illustrate the variable `twice` function in this language, we
start with a definition as a nonvariable program.

```
(define (twice x)
  (+ x x))
```

which yields the following AST

```haskell
List
  [ Atom "define"
  , List [ Atom "twice", Atom "x" ]
  , List [ Atom "+", Atom "x", Atom "x" ]
  ]
```

We can then add variability in the parameter and implementation dimension.

```haskell
vtwice =
    Dim "Impl" ["plus", "times"]
  $ Dim "Par" ["x", "y"]
  $ Obj $ List
    [ Atom "define"
    , List [ Atom "twice", v ]
    , i
    ]
  where v = choice "Par" [Atom "x", Atom "y"]
        i = choice "Impl"
            [ List [ Atom "+", v , v ]
            , List [ Atom "*", IntExpr 2, v ]
            ]
```

#### Edit operations on variational Haskell

We will now see how, starting from a nonvariable program,
add variability incrementally.

The idea is to define the operations that turn the target plain expressions
into variable expressions.
Then we traverse the tree using SYB operations to apply the modification
everywhere.

```haskell
addPar :: Expr -> Expr
addPar (Atom "x") = choice "Par" [Atom "x", Atom "y"]
addPar e = e

varyPar :: VExpr -> VExpr
varyPar = Dim "Par" ["x", "y"] . everywhere (mkT addPar)

addImpl :: Expr -> Expr
addImpl e@(List [ Atom "+", v, v' ])
   | v == v' = choice "Impl" [e, List [ Atom "*", IntExpr 2, v ]]
addImpl e = e

varyImpl :: VExpr -> VExpr
varyImpl = Dim "Impl" ["plus", "times"] . everywhere (mkT addImpl)
```

##### Exercise 19

One might think that even though the two expressions `twice` and
`varyImpl (varyPar xp)` are not syntactically equal, their semantics
might be, because, after all, they really represent the same variations.
Explain why this is, in fact, *not* the case.

```haskell
> varyImpl (varyPar xp)
dim Impl<plus,times> in
dim Par<x,y> in
  twice Par<x,y> = Impl<Par<x,y>+Par<x,y>,2*Par<x,y>>
> varyPar (varyImpl xp)
dim Par<x,y> in
dim Impl<plus,times> in
  twice Par<x,y> = Impl<Par<x,y>+Par<x,y>,2*Par<x,y>>
```

The only difference is the order of declaration of the dimensions.
Thus, when evaluating the semantics, tags fo `Par` would come
before tags for `Dim`.

##### Exercise 20

Define a function `swapOptions` that exchanges the two tags of a binary
dimension and the corresponding alternatives in all bound choices.

```haskell
swapAlt :: V a -> V a
swapAlt (Chc d [a1, a2]) = Chc d [a2, a1]

swapOptions :: Data a => Dim -> V a -> V a
swapOptions d e = withFallback e $ do
  (c, Dim _ ts e) <- extract (dimDef d) e
  let e' = swapAlt `inRange` (chcFor d, dimDef d) $ e
  pure $ c <@ Dim d (reverse ts) e'
```

##### Exercise 21

Define a function `renamePar` that adds a choice of parameter names to the
definition of a specific function `f` by creating a dimension and corresponding
choices that store the existing parameter name and a newly given name.
Be careful to extend only those parameters that are bound by `f`.

The function should be defined so that the expression `renamePar xp "x" "y"`
produces the same resulta as `varyPar xp`.

I will present the solution that fits with my `Expr` type.

We start with a function smart constructor and body/args getters.

```haskell
getArgs :: Expr -> Maybe [Expr]
getArgs (List [ Atom "define", List args, _ ]) = Just args
getArgs _ = Nothing

getBody :: Expr -> Maybe Expr
getBody (List [ Atom "define", _ , body ]) = Just body
getBody _ = Nothing

fun :: [Expr] -> Expr -> Expr
fun args body = List [ Atom "define", List args, body ]
```

Now we define the function that renames an variable reference.

```haskell
renameRef :: Ident -> Ident -> Expr -> Expr
renameRef v0 v1 (Atom v)
  | v0 == v = Atom v1
renameRef _ _ e = e
```

Finally, the solution.

```haskell
renamePar :: VExpr -> Ident -> Ident -> VExpr
renamePar e v0 v1 = withFallback e $ do
  (c, Obj f) <- extract defn e
  args <- getArgs f
  body <- getBody f
  let args' = everywhere (mkT (renameRef v0 v1)) args
      body' = everywhere (mkT (renameRef v0 v1)) body
  return (c <@ Dim "Par" [T.unpack v0, T.unpack v1] (Obj $ fun args' body'))
```

Here are the results.

```
> twice
List [Atom define,List [Atom twice,Atom x],List [Atom +,Atom x,Atom x]]
> renamePar (Obj twice) (T.pack "x") (T.pack "y")
dim Par<x,y> in List [Atom define,List [Atom twice,Atom y],List [Atom +,Atom y,Atom y]]
```
