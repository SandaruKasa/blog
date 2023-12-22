+++
title = "Purely Functional Complaints"
slug = "purely-functional-complaints"
summary = "Some of my pain points when dealing with Haskell"
tags = ["Haskell", "complaints"]

date = "2023-12-20"
lastMod = "2023-12-22"
+++

Fun(?) fact: this website was created because I wanted to write a giant post
with all the complaints that I have amassed over several years of using Rust.
Not because I hate the language --- quite the opposite.
I love it a lot, that's why its weaker parts worry me so much.

But that post is taking ages to write,
so in the meantime here's a smaller one about Haskell
(which still took 5 times longer to write than I anticipated)
with frequent comparisons to Rust, Java, C, C++, and Python.

If you're not familiar with Haskell's syntax,
I recommend you check [an appendix](#comparisons).

Anyways, the complaints.
Roughly in order from the least to the most annoying:

## Naming

### Operators

Why name something using letters when you can assign it a bunch of symbols instead?

First there's basic stuff like `:`,
[`$`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-36-),
[`.`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:.),
or [`++`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-43--43-).

Btw, do you concatenate strings using `++` or
[`<>`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-60--62-)?
`++` is the actual list concatenation (and `String` is just an alias for `[Char]`),
but lists are also an instance of a
[Semigroup](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Semigroup),
with their associative operation of choice being concatenation. I.e.
```haskell
instance Semigroup [a] where
    (<>) = (++)
```

Moving on,
[`<$>`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-60--36--62-)
isn't a combination of `$` and `<>`, but instead just a pseudonym for
[`fmap`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:fmap),
because letters are lame[^fmap].

[^fmap]: That's a joke, I don't think operators are that evil.
    See [the appendix](#operators-appendix) for an explanation.

It *is* similar to `$`, though.
And just like `$` has a mirrored version
[`&`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Function.html#v:-38-),
`<$>` has
[`<&>`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-60--38--62-).
But
[`<$`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-60--36-)'s
mirror version is
[`$>`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Functor.html#v:-36--62-)
and not `<&` or `&>`.

You can kinda think of `<$` as a `<$>` that discards the thing on the right.
Which is why
[`<*>`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-60--42--62-)
(that isn't a combination of `*` and `<>`, but is similar to `<>`) has
[`<*`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-60--42-) &
[`*>`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-42--62-),
which discard the thing on the right and the thing on the left respectively.

[`>>=`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-62--62--61-)
isn't bit shift right and assign but has a mirrored version
[`=<<`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-61--60--60-),
as well as a discarding version
[`>>`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-62--62-).
(No `<<`, sadly.)

The best part about all of this is that you can make your own operators!
My personal favorite is

```haskell
infixr 9 .*
(.*) = (.) . (.)
```

It is like `.`, but composes two-argument functions.

### Monad is just a monoid in the category of endofunctors

[Monads](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Monad) &
[Monoids](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Monoid)
are different things: Monoid is a
[Semigroup](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Semigroup)
with a neutral element, while Monad is a special kind of an
[Applicative](`https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Applicative`),
which is a special kind of a
[Functor](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Functor).
There is also another special case of Applicative,
an [Alternative](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Applicative.html#t:Alternative).
Unrelated to Functor, there is
[Foldable](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Foldable),
but sometimes a Foldable Functor can be
[Traversable](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Traversable).

### Folds

Speaking of Foldable, there is `foldr`, `foldl` & `foldl'`.
[You probably don't want to use `foldl`](https://wiki.haskell.org/Foldr_Foldl_Foldl'#Rules_of_Thumb_for_Folds).

### Classes

`class` isn't what it is in Java or C++.
Haskell's class is what Rust calls traits and Java calls interfaces.
For Java's `class` (Rust's or C's `struct`,
C++'s `class` or `struct`
[because it would be too boring to only have one](https://stackoverflow.com/q/92859))
Haskell offers `data` & `newtype`.
`newtype` being a special `data` with 1 constructor, 1 field,
[slightly different laziness semantics & layout guarantees](https://stackoverflow.com/a/5889784)
as well as
[some automatic derivation perks](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html).
For more comparisons, see [the appendix](#comparisons).

### Just use Either

I, personally find
```rust
enum Option<T> {
    Some(T),
    None,
}
```
to be a more concise naming than
```haskell
data Maybe a = Just a | Nothing
```
but this might be subjective.

What is closer to objectively unfortunate naming is Haskell's `Either`.
On the surface, a pretty simple concept: a union of two types

```haskell
data Either a b = Left a | Right b
```

`Left` and `Right` might not be the most clear ways to name the two alternatives,
but I'm not sure how else you would name the options in such a general type.

Except it's not general at all:
> The `Either` type is sometimes used to represent a value
> which is either correct or an error;
> by convention, the `Left` constructor is used to hold an error value
> and the `Right` constructor is used to hold a correct value
> (mnemonic: "right" also means "correct").
>
> [[source]((https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Either)])]

Sadly, this piece of documentation lies.
It's not "sometimes", it's *most of the time*.

So instead of using a type like
```rust
enum Result<T, E> {
    Ok(T),
    Err(E),
}
```
you just have to remember that `Left` means `Error`.

Not only that, but the type also ends up being asymmetric without it being
reflected in its name in any way.
What I mean is that `Either` is an instance of
`Functor`, `Applicative`, `Monad`, `Foldable`, and `Traversable`
but only over its right part[^instance].

[^instance]: Or, more accurately, `Either a` is an instance of all of those classes.

As for the use-case that *is* reflected in the name
(that is, a union of two arbitrary types),
you probably shouldn't actually use `Either` here,
because I'm pretty sure everyone expects it to be used for error handling.


### But oh well

Honestly speaking, I don't think it is actually possible to come up with
sensible names for **all** of that stuff.

After all, is `<$>` as a name that harder to memorize than,
say, [`std::cout <<`](https://en.cppreference.com/w/cpp/io/cout#Example) or 
[`unwrap_or_else`](https://doc.rust-lang.org/stable/std/result/enum.Result.html#method.unwrap_or_else)?
[I wouldn't say so](#operators-appendix).

Unusual? True. Bad? Not necessarily.

A lot of syntax (and way of thinking in general) in Haskell
takes some time to get used to. But not because it is hard or unreasonable,
rather just because of how different it is from the mainstream languages.

## Infrastructure

No package manager out of the box.

Yes, the situation is better than with C or C++,
but a bit worse than with Python.

Python has [pip](https://docs.python.org/3/installing/index.html).
Which isn't that great, but at least serves as a base for other things like
[poetry](https://github.com/python-poetry/poetry),
[pipenv](https://github.com/pypa/pipenv),
or just manual management of a [venv](https://docs.python.org/3/library/venv.html) prefix.

And there's one registry, [PyPi](https://pypi.org/).

Haskell has [Cabal](https://github.com/haskell/cabal).
Which has [Hackage](https://hackage.haskell.org/).
But there is also [Stack](https://docs.haskellstack.org/en/stable/).
Which is backward-compatible with Cabal using [hpack](https://github.com/sol/hpack).
And has its own superset of Hackage, the [Stackage](https://www.stackage.org/).
It can also be integrated with [Nix](https://nixos.org/),
a project that seems to be rather popular with Haskell folks (I wonder why).
But Nix has its own collection of packages, downstreamed from Stackage (iirc),
as well as [several other ways to do Haskell with Nix](https://nixos.wiki/wiki/Haskell).
If you're on Linux, your package manager's repositories probably downstream Stackage as well.

### Installing GHC

Screw dependencies, how do you want to install GHC?
From your package manager?
From Nix?
From [GHCup](https://www.haskell.org/ghcup/)?
From Stack?

Both Stack and GHCup are quite hungry when it comes to disk space.
Nix is slightly better, but only if you enable file deduplication in the config.

Your package manager probably has a relatively old version of GHC:

> Arch Linux, a rolling release distro,
> has GHC 9.0.2 in its repos (as of writing this in December 2023).
> 
> GHC 9.0.1 was released in February 2021 (almost 3 years ago).
> 
> GHC 9.0.2 was released in December 2021 (only 2 years ago).
> 
> Current latest release is GHC 9.8.1 (October 2023).
> 
> The latest Ubuntu (23.10) also ships GHC 9.0.2.
> 
> Ubuntu LTS 22.04 ships GHC 8.8.4 (June 2020).
> 
> Alpine, surprisingly, ships 9.4.7 (August 2023, and 9.4.1 being August 2022).

And still, this is a bit too many options for my liking,
with none of them being clearly superior
~~except for stack with Nix integrations installed through Nix~~.

### Dependency shenanigans

#### The Type That Shall Not Be Named

Unrelated, but why was Voldemort's (pseudo)name so avoided?
This is a very unrealistic plot point in my favorite book about magic and wizards.
I mean, in real life even Hitler did not get such treatment.

\</tangent>

Anyways, say you have a package `a`:

```haskell
module A (T) where

data T = {- ... -}
```

and a package `b` (which depends on `a`):

```haskell
module B (foo) where

import A (T)

foo :: Int -> T
foo = {- ... -}
```

Now if you're using package `b` in your project
```haskell
module MyCoolProject

import B (foo)

x :: {- ??? -}
x = foo 1
```
you cannot actually name the type of `x`.

##### Solutions?

There's a few:

1. Add `a` as a dependency to `MyCoolProject`
2. Reexport `T` in `B` (good luck if `b` is upstream):
    ```diff haskell
    --- old/B.hs
    +++ new/B.hs
    @@ -1,3 +1,3 @@
    -module B (foo) where
    +module B (foo, T) where

     import A (T)
    ```
3. Same as 2. but ~~in a more convoluted way~~ with a type alias
    (might be convenient if `T` is parametrized):
    ```diff haskell
    --- old/B.hs
    +++ new/B.hs
    @@ -1,6 +1,8 @@
    -module B (foo) where
    +module B (foo, X) where

     import A (T)

    +type X = T
    +
    -foo :: Int -> T
    +foo :: Int -> X
     foo = {- ... -}
    ```

Sadly, I have run into this problem more than once
(despite not having that many experience with Haskell)
with `a` & `b` always being upstream.

I have never run into this problem with Rust even though I frequently use it.
And not because Rust prevents this from happening somehow.

Oh, by the way,

##### How do other languages deal with this problem?

###### Rust

Doesn't ([at least yet](https://github.com/rust-lang/rust/issues/44663)).

Surprising, considering how well it lints against leaking private types in
[function signatures](https://doc.rust-lang.org/rustc/lints/listing/warn-by-default.html#private-interfaces)
or [generic constraints](https://doc.rust-lang.org/rustc/lints/listing/warn-by-default.html#private-bounds).

But for now, you pretty much have the same options as with Haskell:
1. Making `a` your dependency (and then keeping it in sync with what `b` wants).
2. Changing `use a::T` in `b` to `pub use b::T`.
3. Doing something like
    ```rust
    use a::T
    pub type X = T;
    ```

###### Python

If you have `b` installed, you have `a` installed. So just import it.


###### C & C++

Header files.

I'm pretty sure you cannot declare a function that returns an undeclared type.

In C++ you also have [`decltype`](https://en.cppreference.com/w/cpp/language/decltype),
as a last resort.

IDK about modules, but it's not like anyone supports them yet.

#### No namespaces for modules

Module names aren't prefixed with package names.
Not only is not enforced like in C or C++, it is also not the standard practice.

So your package `gigaparsec` can provide a module `Data.Text.Monad.Lazy`
while another package `megalens` provides `Data.Text.Monad.Whatever`.

As a consequence, you can never tell where some type / function / monad transformer
comes from.
Well, at least not straight away.
Your IDE might be able to tell you about this when you hover your mouse over something
and there's still [Hoogle](https://hoogle.haskell.org/).

Not critical, but could have been easier.

Usually you probably don't even care where the stuff comes from anyway.

But in some cases I found it to be considerately inconvenient.

1. Trying to write some code for an std-only target[^target]
    while using GHC installed with my system's package manager.
    Ended up accidentally using things from system-wide-installed packages,
    which were not available on the target.

    [^target]: If we're using
        [GCC parlance](https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html),
        it was *build* & *host*, but not *target*.
2. Searching for functions by signature on Hoogle gives a ton of results
    from foreign packages, which makes it easier to miss that such
    a function already exists in std / your dependencies,
    so you don't have to re-implement it or add any new dependencies.

    Yes, you *can* filter by `included-with-ghc` or run `stack hoogle` on your project,
    but the former won't also include the packages you already depend on
    & the latter requires you to build/download hoogle as well as run it locally.

    And yes, this would still be a problem even if modules were prefixed with package names,
    but at least it would be a bit easier to quickly tell them apart.
3. Expectations about code quality.

    Usually std has some battle-tested code.
    To some degree, the same applies to the more popular packages in the ecosystem.
    But this cannot be said about the smaller ones.

    So when I'm really struggling with composing things
    so that they will do what I need them to,
    I want to be able to easily tell who is in the wrong here:
    is there a chance that the APIs are poorly designed
    or is it more likely that I'm just missing some important idea
    and doing things wrong?

### Unit tests

The best way to write unit tests for private interfaces in Haskell
is [to make those interfaces public](https://stackoverflow.com/a/14379426)
and write your tests in a separate package using a hand-rolled or a 3rd-party framework.
Unfortunate.

~~Java's situation is a bit better: JUnit is still a 3rd-party framework but at least
it allows you to test implementation details.~~
Nope, it doesn't. I just double-checked and apparently you have to use reflections for that.
Also, googling "junit test private method" gives you several articles
starting with that you shouldn't write unit tests for your private code anyways,
because ***incapsulation***, SOLID, and all of the other OOP stuff.
Lmao, what a cope, "You shouldn't write tests for your code, it's ideologically incorrect!"

Speaking of ideology. Rust.

Rust has [first-class support for unit tests](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html).
You don't need to slap a 3rd-party framework on top of it.
You *can* if you want more specific options, but most of the time there is no need for that.

In my experience, this low barrier for writing tests is really important.
Usually I find myself not writing any tests in my personal projects,
just because I'm too lazy to set the entire thing up.
The most I am willing to do, is to launch a REPL to poke once or twice
the function I'm currently writing
or maybe to slap a temporary debug statement somewhere in `main`.
Those checks doesn't persist anywhere after that.

With Rust I just write unit tests,
because I can write them in the same file that I am writing the code in
(or a file right next to it) and the only boilerplate I have to write is
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_ ...
```
No additional configuration, no extra dependencies, just
```shell
$ cargo test
```
and there you are!

Most of the time I don't want to test big pieces of code in public interfaces.
I just want to make sure that this small private function does what I want it to.
I won't bother with an entire framework for a simple sanity check.
I don't wan't to expose this stupid ten-liner in a public interface just to be able to test it.
I just want to make sure it doesn't break when I pass in `-1`.

Surprisingly, even these small checks help catching bugs or
spotting that something is wrong faster.
Plus just like proper unit tests, they persist, so you can easily make sure
you do not break any stuff later on.

Writing these is so straight-forward that occasionally
I will even start with writing a test for what I'm about to create.
This often helps to evaluate how good the interface is before even attempting to implement it.
Is this effectively [design by contract](https://en.wikipedia.org/wiki/Design_by_contract)?

## Language design

### No namespaces

I don't mean modules.

What I mean is that there's no distinction between `data`/`newtype` fields,
freestanding functions, functions closely associated with some data type,
or belonging to a particular typeclass.

And while it might not be crucial to differentiate
between freestanding functions & those associated with some datatype
(see: [D](https://dlang.org/spec/function.html#pseudo-member) &
[UFCS](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax))
or between those associated with a datatype & belonging to some typeclass
(see: Java making no difference between classes' and interfaces' methods),
the fact that fields become essentially freestanding functions in the outer scope
is genuinely annoying.

You cannot have two data types with a same field name in one module.
And even if you have them in different modules,
now you have to qualified-import those modules
(probably renaming them to 1 letter in the process)
and prefix all field accesses with that module name.
Instead of, y'know, prefixing them with the datatype's name
or resolving them from the context.

So you end up prefixing all of your fields' names with some abbreviation.

Here's an extract from a real style guide used by real Haskell programmers:

> Field name for `newtype` *should* start with `un` or `run` prefix followed by
> type name (motivated by [this
> discussion](https://www.reddit.com/r/haskell/comments/7rl9hx/newtype_field_naming_getx_vs_runx/)):
> 
>   - `run` for wrappers with monadic semantic
>   - `un` for wrappers introduced for type safety
> 
> ``` haskell
> newtype Coin = Coin { unCoin :: Int }
> newtype PureDHT a = PureDHT { runPureDHT :: State (Set NodeId) a }
> ```
> 
> Field names for record data type should start with every capital letter in type
> name.
> 
> ``` haskell
> data NetworkConfig = NetworkConfig
>   { ncDelay :: Microsecond  -- `nc` corresponds to `_N_etwork_C_onfig`
>   , ncPort  :: Word
>   }
> ```

What's funny is that I can't even go,
"Having to prefix your names to avoid collisions is so 1970's!
What are you, C with no namespaces?"
because even C actually nailed scoping field names & using context to get rid of collisions.

## Appendices

### Appendix A. Operators {#operators-appendix}

In all honesty, I *can* see why operators are used so much.
Let me explain using an example.

First, let's get rid of all those strange `fmap`/`liftA2`-like names
and weird operators.
Now, let's look at a snippet from some parser (because what else do you write in Haskell?).

```haskell
as Literal (
    or
    (right (consume '(') (left parseNumber (consume ')')))
    (and parseReal parseImaginary)
)
```
Oops, that's not Haskell, that's just Lisp with missing parenthesis around the main expression.
Not inherently bad, and of course sometimes syntactic simplicity is great
([not always though](https://matklad.github.io/2023/01/26/rusts-ugly-syntax.html)).
But I argue this piece of code will require less effort to read when we Haskell-ify it back.

Let's recall that `$` exists to get rid of parenthesis spanning until the end of an expression
and that we can turn any function into an operator like ``` `so` ```.
```haskell
as Literal $
    (consume '(' `right` parseNumber `left` consume ')')
    `or`
    (parseReal `and` parseImaginary)
```

That's a bit better I'd say.
Humans don't usually think in [S-expressions](https://en.wikipedia.org/wiki/S-expression)
(or maybe it's just my
[SVO](https://en.wikipedia.org/wiki/Subject%E2%80%93verb%E2%80%93object_word_order)-wired brain).
Also I'm pretty sure `and` has a higher priority than `or`.

```haskell
as Literal $
    (consume '(' `right` parseNumber `left` consume ')')
    `or` parseReal `and` parseImaginary
```

Still a bit tough on the eyes. Maybe capital letters?

```haskell
AS Literal $
    (consume '(' RIGHT parseNumber LEFT consume ')')
    OR parseReal AND parseImaginary
```

Not bad. Let's compare those side-by-side

```haskell
as Literal (
    or
    (right (consume '(') (left parseNumber (consume ')')))
    (and parseReal parseImaginary)

AS Literal $
    (consume '(' RIGHT parseNumber LEFT consume ')')
    OR parseReal AND parseImaginary
)
```

Unfortunately, there's still one problem left.
I'm pretty sure `OR` & `AND` were taken by boolean operations.
We could make our language have signature-based function overloads,
but then we would have to resolve functions from the surrounding types,
while currently we infer the types by looking at the surrounding functions.
So unless you want to manually specify types everywhere,
let's rename those two to, idk, `OR_ELSE` & `AND_THEN`.
Same deal with `LEFT` & `RIGHT`. Let's prefix them with `TAKE_`.

```haskell
AS Literal $
    (consume '(' TAKE_RIGHT parseNumber TAKE_LEFT consume ')')
    OR_ELSE parseReal AND_THEN parseImaginary
```

Not as great. Maybe we can...

```haskell
fmap Literal $
    (consume '(' *> parseNumber <* consume ')')
    <|> parseReal <*> parseImaginary
```

Oh, now that looks like a real Haskell snippet. Except for one thing:

`fmap Constructor $ ...` is so common that we usually just

```haskell
Literal <$>
    (consume '(' *> parseNumber <* consume ')')
    <|> parseReal <*> parseImaginary
```
And here we are!

Is it really worse than
```haskell
as Literal (
    or
    (right (consume '(') (left parseNumber (consume ')')))
    (and parseReal parseImaginary)
)
```
or
```c
Literal {
    Parser::either(
        consume('(').discard_and(parseNumber).and_discard(consume(')')),
        parseReal.and(parseImaginary)
    )
}
```
?

### Appendix B. Haskell vs Rust vs Java {#comparisons}

This sections would've been great as a table with codeblocks,
but you cannot do that in Markdown[^markdown].
So instead, I made some headers and ordered code snippets as follows:

1. Haskell
2. Rust
3. Java

[^markdown]: Unless you are willing to write HTML by hand.
    Including all the classes necessary for your CSS to apply
    as well as the terrible boilerplate for syntax highlighting.
    I.e.: do the job of your markdown renderer for it.

    The closest you get is [grid tables with pandoc](https://stackoverflow.com/a/35635553),
    but my renderer doesn't support those, and even pandoc
    [doesn't seem to support syntax highlighting inside of those](https://stackoverflow.com/questions/32085498/markdown-how-to-insert-java-code-block-inside-table-cell/35635553#comment86930341_35635553).


Note that this comparisons are very inaccurate and are here
only to roughly familiarize you with Haskell.

#### Functions

```haskell
foo :: Int -> Int -> Int
foo a b = a * 2 - b + 3

bar :: Int -> Int
bar a = foo a a

hello :: Named t => t -> String
hello = "Hello, " ++ name t
```

```rust
fn foo(a: i32, b: i32) -> i32 {
    a * 2 - b + 3
}

fn bar(a: i32) -> i32 {
    foo(a, a)
}

fn hello<T: Named>(t: T) -> String {
    format!("Hello, {}", t.name())
}
```


```java
class Example {
    static int foo(int a, int b) {
        return a * 2 - b + 3;
    }

    static int bar(int a) -> i32 {
        return foo(a, a);
    }

    static <T extends Named> String hello(T t) {
        return "Hello, " + t.name();
    }
}
```

#### Data structures

```haskell
data A = A Int String

data B = B { field1 :: Int, field2 :: String }

data C t = C { ts :: [t] }
```

```rust
struct A(i32, String)

struct B {
    field1: i32,
    field2: String,
}

struct C<T> {
    ts: Vec<T>,
}
```

```java
class AB {
    int field1;
    String field2;
}

class C<T> {
    List<T> ts;
}
```

#### class / trait / interface

All of the following code snippets print
```
Hello, I'm John!
Bye!
```

##### Haskell: class, =>, instance

```haskell
class Name a where
    getName :: a -> String

class (Name a) => Greet a where
    sayHi :: a -> String
    sayHi me = "Hello, I'm " <> getName me <> "!"

    sayBye :: a -> String
    sayBye me = "Sincerely yours,\n" <> getName me

data Person = Person { name :: String }

instance Name Person where
    getName = name

instance Greet Person where
    sayBye _ = "Bye!"

main :: IO ()
main = do
    let john = Person { name = "John" }
    putStrLn $ sayHi john
    putStrLn $ sayBye john
```

##### Rust: trait, :, impl for

```rust
trait Name {
    fn get_name(&self) -> String;
}

trait Greet: Name {
    fn say_hi(&self) -> String {
        format!("Hello, I'm {}!", self.get_name())
    }
    fn say_bye(&self) -> String {
        format!("Sincerely yours,\n{}", Name::get_name(self))
    }
}

struct Person {
    name: String,
}

impl Name for Person {
    fn get_name(&self) -> String {
        self.name.clone()
    }
}

impl Greet for Person {
    fn say_bye(&self) -> String {
        "Bye!".to_string()
    }
}

fn main() {
    let john = Person {
        name: "John".to_string(),
    };
    println!("{}", john.say_hi());
    println!("{}", john.say_bye());
}
```

##### Java: interface, extends, implements

```java
interface Name {
  String getName();
}

interface Greet extends Name {
  default String sayHi() {
    return "Hello, I'm " + this.getName() + "!";
  }

  default String sayBye() {
    return "Sincerely yours,\n" + this.getName();
  }
}

class Person implements Greet {
  String name;

  Person(String name) {
    this.name = name;
  }

  @Override
  public String getName() {
    return this.name;
  }

  @Override
  public String sayBye() {
    return "Bye!";
  }
}

public class Example {
  public static void main(String[] args) {
    var john = new Person("John");
    System.out.println(john.sayHi());
    System.out.println(john.sayBye());
  }
}
```

#### Sigma type

```haskell
data Response
    = Received
        { statusCode :: Int
        , body       :: String
        }
    | IOError String
```

```rust
enum Response {
    Received { status_code: u16, body: String },
    IOError(String),
}
```

Java does have `enum`s, but they do not support having different fields per variant.

#### Newtype

```haskell
newtype URL = URL String
```

```rust
#[repr(transparent)]
struct URL(String)
```

Isn't really a thing in Java.

Haskell has deriving & [Generalized Newtype Deriving](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html),
Rust has deriving & [Deref](https://doc.rust-lang.org/stable/std/ops/trait.Deref.html).
Both can be useful for propagating the behavior of the underlying type to the wrapper.

### Appendix C. Updates {#updates}

#### 2023-12-22

- Tweaked some headings.
- Added a [section about naming](#naming).
- Added a [subsection on unit tests](#unit-tests).
- Added an appendix: [Operators](#operators-appendix).
- Added an appendix: [Haskell vs Rust vs Java comparisons](#comparisons).
- Added an appendix: [Updates](#updates).

In other words, made this post twice or even thrice as long.
