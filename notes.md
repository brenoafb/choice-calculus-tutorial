# Choice Calculus

## Choice Calculus Tutorial

- The Choice Calculus is a way of representing variability in programs
- Variability appears in specific points and is classified according to a dimension

## Motivating Example

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

## Sharing

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

### Exercise 1

Add a new dimension *FunName* that is used to vary the name of the function
between `twice` and `double`.

```
dim Par<x,y> in
dim Impl<plus,times> in
dim FunName<twice,double> in
FunName<twice,double> Par<x,y> = Impl<Par<x,y> + Par<x,y>, 2*Par<x,y>>
```

### Exercise 2

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

### Exercise 3

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

## Syntax

The syntax of choice calculus is very simple

```
e ::= a-<e,...,e>-           Object Structure
    | dim D<t,...,t> in e    Dimension
    | D<e,...,e>             Choice
    | share v = e in e       Sharing
    | v                      Reference
```

### Exercise 4

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

## Choice Elimination

- **Plain expression**: Expression without variability
- **Decision**: Selection of one or more **tags**
- **Complete decision**: a decision that eliminates all choice

### Exercise 5

Given `e = dim A<a1, a2> in A<A<1,2>,3>`, what is the result of selection `{ e } A.a1`?
Is it possible to select the plain expression `2`?

```
{ e } A.a1 = 1
```

It is not possible to select `2`, since that branch in the selection is
unreachable.

## Semantics

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

### TODO Exercise 6

```
dim A<a1,a2> in A<1, dim B<b1,b2> in B<2,3>>
```
Write the semantics of the above expression if the tag ordering
constraint is removed

### TODO Formal definition

p. 9


## Haskell Implementation

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

### Sharing vs let/where

#### Exercise 9

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

## Functor

```haskell
data V a = Obj a
         | Dim Dim [Tag] (V a)
         | Chc Dim [V a]

instance Functor V where
  fmap f (Obj x)       = Obj $ f x
  fmap f (Dim d ts vs) = Dim d ts $ fmap f vs
  fmap f (Chc d vs)    = Chc d $ fmap (fmap f) vs
```

### Exercise 10

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

## Monad (, Applicative)

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

## Variational Lists

```haskell
type VList a = V (List a)

data List a = Cons a (List a)
            | Empty
            | VList (VList a)
```

## Exercise 11

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

## Exercise 12

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

## Exercise 13

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

## Exercise 14

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

## TODO Exercise 15

Define the function `filterL :: (a -> Bool) -> List a -> List a` and
give a definition for the corresponding function `vfilter` that
operates on variational lists.



