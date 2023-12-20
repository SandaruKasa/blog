+++
title = "Purely Functional Complaints"
slug = "purely-functional-complaints"
summary = "Some of my pain points when dealing with Haskell"
tags = ["Haskell", "complaints"]

date = "2023-12-20"
+++

Fun(?) fact: this website was created because I wanted to write a giant post
with all the complaints that I have amassed over several years of using Rust.
Not because I hate the language --- quite the opposite.
I love it a lot, that's why its weaker parts worry me so much.

But that post is taking ages to write,
so in the meantime here's a smaller one about Haskell
(which still took 5 times longer to write than I anticipated).

Roughly in order from the least to the most annoying:

## Dependency Hell

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


## Installing GHC

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

### The Type That Shall Not Be Named

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

#### Solutions?

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

#### How do other languages deal with this problem?

##### Rust

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

##### Python

If you have `b` installed, you have `a` installed. So just import it.


##### C & C++

Header files.

I'm pretty sure you cannot declare a function that returns an undeclared type.

In C++ you also have [`decltype`](https://en.cppreference.com/w/cpp/language/decltype),
as a last resort.

IDK about modules, but it's not like anyone supports them yet.

### No namespaces for modules

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

1. Trying to write some code for an std-only target
    (if we're using [GCC parlance](https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html),
    it was *build* & *host*, but not *target*)
    while using GHC installed with my system's package manager.
    Ended up accidentally using things from system-wide-installed packages,
    which were not available on the target.
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

## No namespaces

I don't mean modules.

What I mean is that there's no distinction between `data`/`newtype` fields,
freestanding functions, functions closely associated with some data type,
or belonging to a particular typeclass.

And while it might not be crucial to differentiate
between freestanding functions &
those associated with some datatype (see:
[D](https://dlang.org/spec/function.html#pseudo-member) &
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

Here's an extract from a real style guide used by real haskell programmers:

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
