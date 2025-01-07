+++
title = "The `Void` in my head"
summary = "Some thoughts about `Nothing` and `Never`"
tags = ["math", "programming", "Haskell", "Rust", "C++", "complaints"]

draft = true
math = true
+++

## Gentle introduction to Algebraic Data Types

If you're familiar with "sum" and "product" types, feel free to skip [to the next section](#void)
(unless you want to hear me complain about how Kotlin, Java, C, C++ and everyone else
gets everything wrong in their design).
If not, then I'm sorry your programming language doesn't let you have nice things.
But read on and I'll try to explain this to you!

### `sizeof(T)` & $|T|$

Every type represents a piece of data, some value, constrained to
one of all the possible values for this type.
For `bool` it's 2 values: `true` or `false`.
For a `uint8_t` there's 256 of them: integers from `0` to `255`.

Essentially, you can think of a type as of a set of all its possible values.

For a type `T`, let's denote the number of all its possible values as $|T|$.
Yes, this just cardinality of a set.

Observe that we need at least $\log_2 |T|$ bits to store a value of type $T$.[^length]
Indeed, $\log_2 2 = 1$ bit is enough to store a `bool` with its two possible values,
and `uint8_t` fits nicely into $8 = \log_2 256$ bits.

In practice, the amount of bits can be higher than that,
because either some of them are wasted on padding
(such is often the case with `bool`,
where its 1 bit occupies 8 bits of memory,
just because computers usually work with bytes)
or because $|T|$ is not a power of two,
so you will just round up $\log_2 |T|$ to the nearest integer
(or a multiple of 8, for the same reasons as with `bool`);
this would be the case with something like `enum Color { Red, Green, Blue };`---
it has 3 values, and $1 < \log_2 3 <2$, so you can't really cram it into 1 bit,
but you can in 2, and you probably will just waste 8 bits on it in the end anyway[^non-whole].

[^length]: The pedantic among you may point out that my reasoning does not hold for infinite types.

	And yes, you are right.
	You do not need $\log_2 \infty = \infty$ bits to store an arbitrarily big integer,
	because you can just throw away the infinitely many leading zeros,
	and use what effectively is a variable-length encoding.
	And the type does not even have to be infinite!
	See, for example,
	[how UTF-8 stores its $1 112 064$ possible Unicode codepoints](https://en.wikipedia.org/wiki/UTF-8#Description).

	A more accurate way to phrase what I'm trying to say here is that
	an injection$f: T \to \left\\{ 0,1 \right\\}^N$ can exist only if $N\ge\log_2|T|$.

	And as for the infinite types in particular, only finite subsets of their values
	are actually used in practice, because computer memory is fundamentally
	finite in capacity. And even then, for any type, whose size is limited not by some
	small constant known in advance, but rather by the amount of allocatable memory
	or by the size of input data,	an indirection would be used anyways,
	so as far as the type layouts go,	you just get a $\le 64$ bit pointer, and that's it.
	Most of the time the burden of dealing with variably-sized data is shifted
	from a programming language's type system to the runtime memory allocator.

[^non-whole]: It is actually possible to exploit the fact that $\log_2 3 < 2$.

	For a `Color[3]` array there is only 27 possible combinations of values,
	which can be stored in $\lceil \log_2 27 \rceil = 5$ bits,
	which is less than `2*3=6` bits needed fot the two-bits-per-element encoding.
	But it is also presumably less pleasant to del with.

Now, to the "sum" & "product" types.

### Product types

Let's start with those, because they were a thing even in the 70s, when C was born.
You probably know them as structures.

```c
struct two_octets {
	uint8_t a;
	uint8_t b;
};
```

Why is a *product* type?
Because the number of its possible values is the *product* of the number of possible values of its fields.

$$|\texttt{two_octets}| = |\texttt{typeof(a)}|\cdot|\texttt{typeof(b)}|=|\texttt{uint8_t}|^2=256^2$$

Or you can take logarithm of both sides of this equation and get a more familiar
```c
sizeof(two_octets) = sizeof(a) + sizeof(b) = sizeof(uint8_t) * 2
```
because logarithms turn multiplication into addition and exponentiation into multiplication.

So even though you can just add up the sizes of the fields,
in actuality you are multiplying cardinalities.
The addition of sizes is just a nice consequence of logarithm properties.

Some languages also provide a convenient way to create product types
without having to come up with a name for every single one of them,
usually called "tuples". Usually written using parentheses
both to denote the type `(T, U, V)` and to construct an instance `(t, u, v)`.

Scala has those. Haskell has those. As a consequence, Rust has those too.
Even Python has them!

But not C, C++, Java, or Kotlin. In C & Java they are absent completely.
C++ & Kotlin try to mitigate the lack of a language feature
by providing some types in the standard library
(namely, `Pair` & `Triple` for Kotlin, `std::pair<T, U>` & `std::tuple<T...>` for C++),
but ergonomics of those is admittedly underwhelming.
This is pretty much a status quo for C++: never add builtin types (or type constructors),
always twist and rape the shit out of template metaprogramming in std to try
and poorly mimic those language features. (We will see this approach again very soon.)
The fact that they are actually able to do that speaks volumes of C++'s
flexibility and expressivity, but just as much about its stagnation.
This is one of the reasons I like to think of C++ not as of a programming language,
but rather as of a very powerful framework for building your own languages.
Anyways, why Kotlin did not manage to add tuples is beyond me.
Scala is nearly twice as old as Kotlin and works on top of a JVM too,
but has tuples just fine, so that's clearly not a JVM limitation.
Speaking of Java, shout outs to Java for making product types less ergonomic to use than in C!
It took them until Java 16 in 2021 to add `record`s.
Congratulations on beating Go in ignoring decades of advances in programming language design,
as even Go has normal structures and tuples.

Moving on...

### Sum types

are surprisingly less common.
They are known as `enum`s in Rust, "tagged unions" in C, and `std::variant` in C++17.

A sum of `T` and `U` is a type that either has a value that is `T`, or a value that is `U`.

Rust:

```rust
enum Sum<T, U> {
	Either(T),
	Or(U),
}
```

C:

```c
typedef ... T;
typedef ... U;
struct sum {
	enum {
		IsT,
		IsU,
	} active;
	union {
		U u;
		T t;
	};
};
```

C++17:
```c++
template<typename T, typename U>
using Sum = std::variant<T, U>;
```

You might have already guessed why they are called "sum" types.
Indeed, $|T+U|=|T|+|U|$.
Or, in term of sizes:
$$\texttt{bits}(T+U)=\log_2 \left( 2^{\texttt{bits} (T)} + 2^{\texttt{bits}(U)} \right)$$
Ugly, isn't it?
Unfortunately, there are no nice properties for logarithms of sums, but I'd like to
point out one particular case specifically: the one where $|T|=|U|$.
In that case $|T+U|=2|T|=2|U|$ and
$\texttt{bits}(T+U)=\log_2 ( 2\cdot 2^{\texttt{bits}(T)})=1+\texttt{bits}(T)$.
That $1+$ is often called the "discriminant" bit, and in practice sum types
are often represented as a union of all summed types with a couple of
discriminant bits (or a tag) signifying which type that union actually holds.
You write that by hand in C, C++17 has that code written for you in std,
and Rust has that representation generated for you by the compiler itself.

What is a union though?

#### `union`s

Quite funny. The size of a union is the maximum of all its field sizes.
And since exponentiation and logarithms are monotonic,
you can alternatively say that cardinality of a union of some types
is the maximum cardinality of those types.

This time, I'll admit, unlike with `struct`s (product types),
the bit-wise view of them is fundamental and any talks about
their cardinality are secondary.

`union`s do not make much sense from the "type is a set" point of view,
but are very natural when you are dealing with bits.
Product types are somewhat agnostic in that regard,
while sum types are very natural from the set point of view,
but very inconvenient when it comes to bits (you've seen that formula for their bitsize).
I guess this is why sum types became mainstream in PL design
so much later than product types despite their neatness.

### Algebraic Data Type

In lame terms it is when you are able to easily create sums of products and products of sums.

In Rust:
```rust
enum WhatDidWeGetFromTheServer {
	HttpStatus {
		code: u16,
		message: String,
	},
	AnError(std::io::Error),
	NothingYet,
}
```

In Haskell:
```haskell
data WhatDidWeGetFromTheServer
	= HttpStatus Int String
	| AnError Error,
	| NothingYet
```

In C++:
```c++
struct HttpStatus {
	int code;
	std::string message;
};
struct NothingYet { };
using WhatDidWeGetFromTheServer = std::variant<
	HttpStatus,
	std::exception,
	NothingYet,
>;
```

With all that out of the way, let's finally talk about **nothing**.

## Void

> "Void" (adjective): containing nothing; empty; not occupied or filled.

from Wiktionaty.

### Void is Nothing

In C/C++ and Java the `void` type is not actually an empty type.
I.e. the set of it's possible values is not empty,
it actually contains a single element,
which, unsurprisingly, means that you need $\log_2 1 = 0$ bit to store it.

Observe that instead of using void you can just do
```c
struct Unit { };
```
and then replace every `return;` with a `Unit unit; return unit;`.
This name, "Unit", comes from the fact that it has exactly one value,
and this is actually what it is called in Scala and Kotlin.
In Rust too, but you spell it as `()`, which is a nice parallel
to the fact that you can view it as an empty tuple,
a product of zero types, and it actually makes sense in math
to say that $\prod\limits_{\varnothing}x=1$ same way as $\sum\limits_{\varnothing}x=0$,
because 0 is neutral for addition and 1 is neutral for multiplication.

Finally, in Python this type is known as `None` and it is inhabited
by exactly one value of the same name. This is actually what your Python
function returns when you do not return anything explicitly:
```python
def foo():
	return
print(foo()) # None
```

It is also often used to signify absence of a value.
Not a big surprise! After all, `Optional<T> = Either<T, ()>`.

#### C/C++

Even though you theoretically could do the `struct Unit { }` trick I've described above,
it comes with some issues in practice.

First of all, a `struct` with no fields isn't valid C.
However, GCC and Clang do accept that as an extension to the language.
The `sizeof(struct Unit)` is `0`, as you might expect, when these extensions are on (the default).

With C++, empty structures *are* a well-defined part of the language,
and they are defined... to have `sizeof` equal to `1`. Hooray.
But it gets even more complicated with inheritance.

```c++
struct empty { };

template<typename T>
struct as_field {
	T t;
	uint8_t u8;
};

static_assert(sizeof(as_field<empty>) == 2);

template<typename T>
struct as_base: T {
	uint8_t u8;
};

static_assert(sizeof(as_base<empty>) == 1);
```

Aside from postulating that `sizeof(T) >= 1` for any `T`,
C++ also requires that every field in a struct has a unique address.
But this requirement does not hold for the base class of a `struct`,
and even though you can *mostly* think of
```c++
struct derived: base {
	int field;
};
```
as of syntactic sugar for
```c++
struct derived {
	base _parent;
	int field;
};
```
this analogy does not hold here, and when you declare a derived type
it actually matters whether its base class is 1 byte in size because it needs that byte
or it is 1 byte in size, because it cannot be 0 bytes on its own.

This difference in sizes often gets exploited in libraries where
you want to have a class with a user-provided destructor/comparator/allocator.
Because the thing that the user provides can very likely be just an empty class
with an overloaded `operator()` (either manually written or an automatic expansion of a lambda),
it makes sense to inherent from it instead of storing it as a field,
because going even 1 byte up in size can very well be a difference between a `sizeof(T*)` structure
and a `2 * sizeof(T*)` structure due to alignment requirements.
To ease the pain just a bit C++20 provides
[a special attribute](https://en.cppreference.com/w/cpp/language/attributes/no_unique_address)
to override the typical "any field must be at least 1 byte" behavior.

Anyways, back to the `void`. Returning it does not actually require you to allocate a single byte,
but its `sizeof` is not well defined anyways
(but with the enabled by default GCC extensions `sizeof(void) == 1`,
so fuck you if you thought that things can ever be simple in C++ land).
`void` is treated somewhat like an incomplete type: you cannot create a value this type.
But you can return it from a function! How cool is that! We even have a special syntax for that:
`return;`

In practice this means that `void` is special-cased like hell syntactically.
This is why you can often encounter horrors like this in C++ libraries (even in std):
```c++
template <typename R>
R visit(Function<T, R> callback) {
    T my_t = ...;
    if constexpr (std::is_same_v<R, void>) {
        callback(my_t);
        return;
    } else {
        return callback(my_t);
    }
}
```
How cool.

#### Java

Java also special-cases `void` when it comes to `return` syntax,
but there's yet another can of worms to unpack here: Java's generics.

You see, in Java there are two kinds of types: primitives and Objects.
There are just a handful of primitive types. Namely

- `int`, `short`, `long` --- need no introduction.
- `boolean` --- also pretty clear.
- `float`, `double` --- IEEE-754 floating point numbers deserve to be complained about separately,
	and indeed people have been complaining about them since the dawn of time, deservingly so.
	To add salt to injury, Java's floating point numbers can be slightly non-conforming to IEEE-754,
	but that's another story and shall be told another time. Look up `strictfp` if curious.
- `char` --- Java almost managed to get this one right.
	After all, it was born after people realized insufficiency of ASCII and 7 or 8 bit `char`s,
	but, sadly, before people realized that $2^{16}=65'536$ characters won't really suffice
	to fit every script possible (Chinese alone has tens of thousands of characters),
	so Java's `char`, while not a 17-something (rounded up to 32) bit long Unicode Codepoint,
	is at least a 16 bit long UTF-16 character, so yay, surrogates.
- `byte` --- is what you want for 8 bit values. Except fuck you, it's signed
	with no unsigned alternative, so have a great time doing your `0` to `255` RGB arithmetics.
- Finally, there's `void`. It's not a proper type per se, but feels similar enough.

These types are cool and all, but you can't use them in generics.
You see, Java's generics do not actually go through monomorphization,
creating a copy of a function/class for each substituted type,
instead they are just a fancy set of compile-time checks
that do not even survive compilation and just get replaced with `java.lang.Object`.
Speaking of which.

Aside from primitive types, Java also has objects.
They all are heap-allocated (let's not talk about JIT optimizations right now, ok?),
GC-managed, and they all inherit from `java.lang.Object`, the so-called "top type".
Primitives are more of an exception to this pure-OOP "everything is an object" approach
made for the sake of performance.
And for each primitive type there is an Object counterpart, named the same way
(just with a capital letter[^capital]) and residing in the automatically imported `java.lang` package.
They also have implicit conversion to and from their primitive counterparts.[^performance]

[^capital]: and `Integer` & `Character` instead of `Int` & `Char` :-)

[^performance]: These conversions can bite you at runtime,
	because you can accidentally write code that converts back and forth
	between primitives and objects, which costs a bit and massively exhausts GC
	by performing a ton of instantly abandoned small allocations.

	Then there's a funny thing with `Collection<T>` having `get(int i)` method for indexed
	accesses and `get(T obj)` for by-element lookup. So good luck with calling the right one
	when your `T` is `Integer`.

	While we're at it, let's talk about performance.
	Java's `ArrayList<Integer>`, which would be `std::vector<int>` in C++ parlance,
	is stupidly inefficient, because it is closer to a `std::vector<int*>`.
	True story, the first real-world Java application I've ever dealt with (from the code side)
	was a tool for calculating enchantment levels in Minecraft,
	and it shipped its own hand-written `IntList`
	which was more or less an `ArrayList<T>` with `s/T/int/g` applied.
	You'd think that a modern compiler could do that for you,	but nope.
	Not Java and, up until fairly recently, not Go.

	Java does provide a few hand-written monomorphized things like `IntStream`, `LongStream`,
	or `LongFunction<R>` for `Function<long, R>` and so on.
	[Take a look.](https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html)
	But no `ArrayList<int>` for some reason.

	There *is* a project for extending Java generics to primitive types called "Project Valhalla".
	Started in 2014, but by the time it ends, we will probably have gotten both
	proper module support in C++ compilers and GTA VI.
	Maybe even a Half-Life 3 on top, if the recent rumors are anything to go by.

Surprisingly, there is `java.lang.Void`.
But you cannot create it.
You cannot return it from functions.
You cannot use it in templates, so no `Function<T, Void>` for you,
use a manually monomorphized `Consumer<T>` instead.
Why not be consistent here and have `java.lang.Void` be a singleton
with only one value, exposed as its static field? No idea.

This is actually a significant difference.
While `void` in Java is by all accounts a unit type,
just with a lot of syntactic special-casing,
`java.lang.Void` is fundamentally different.
It is not creatable. It has no possible values.
It is not a unit type. It's a "never" type.

### Void is Never

## TODO
- monostate
- mutex poisoning
- poison has ! for aborting panics
- panic handler returns !
- ! is bottom, java.lang.Object is top. Kinda.
	! typechecks to anything, but is not a subtype per se.
- Infallible is a way to define Never.
	Other way is enum Never { lol(Never) };
- Nothing is 1, Never is 0. They behave like they should in sums an products.
- it makes sense that Never is -inf bits, because adding it to a struct does not change size
- you can get anything from Never. As empty match and as recursion.
- Make fun of C++ trying to deliver lang features as std components.
- Never does not exist in C/C++. But it has [[no_return]]. Similarly ! is stable in rust only for fn()->!.
