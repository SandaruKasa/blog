+++
title = "`as` Considered Harmful"
slug = "as-considered-harmful"
summary = """*C-style* casts from C++, now in Rust"""
tags = ["Rust", "complaints"]
series = ["rust-defects"]

draft = true
+++

To clarify, I'm not talking about
```rust
use crate::{Foo as Bar};
```
or
```rust
<T as Trait>::Item
```
They are completely fine by me (even though the latter has a bit of a funky syntax).
What I want to complain about is the `as` cast.

So what's the problem with it? It's twofold:
1. It does way too many different things at once,
2. There are some implicit and quirky behaviors that it exhibits.

Let's dive into the first one, because it can be more or less categorized into different use-cases,
and we'll point out the quirky behaviors along the way.

## What can an `as` cast do?

### A numeric cast

#### Integer to integer

The behavior of the `as` cast is actually quite straightforward here
if you approach it from the bit manipulation perspective:
1. If the input and the output are the same size, it just reinterprets the bits,
   specifically using [two's complement](https://en.wikipedia.org/wiki/Two's_complement) for signed integers.[^complement]
2. If the output is shorter than the input, the most significant bits
    get truncated until the value is the right size, then it gets reinterpreted.
3. If the output is bigger than the input, the value gets sign-extended
    (i.e. padded with 0 for unsigned and non-negative signed,
    and with 1 for negative signed) to the right size, then reinterpreted.

[^complement]:
    Thankfully, Rust appeared late enough for two complement's to have become the de-facto standard.
    C++ was not that lucky.
    Prior to C++20, the language did not guarantee this exact representation.
    Thus a signed 8-bit int was only guaranteed to be able to represent values
    from -127 to 127. Sorry, kid, -128 wasn't part of the deal.
    Thankfully,
    [this got fixed in C++20](https://en.cppreference.com/w/cpp/language/types#Range_of_values),
    alongside with
    [bitwise shift of negative ints](https://en.cppreference.com/w/cpp/language/operator_arithmetic#Built-in_bitwise_shift_operators).
    ([Signed overflow is still UB though](https://en.cppreference.com/w/cpp/language/operator_arithmetic#Overflows).)

From the semantical value perspective it makes less sense:

1. Sometimes it's a lossless extension to a wider type. (`u8` -> `u16`, `u8` -> `i16`, `i8` -> `i16`)
2. Sometimes it's a modulo truncation. (`u16` -> `u8`)
3. Sometimes it's just a reinterpretation to change signedness. (`u8` <-> `i8`)
4. And sometimes it's a weirdly behaving mess that does whatever it wants with the sign.
    (`i32` -> `i16`, `u32` -> `i16`, etc.)

A quiz!

What is `-400i16 as i8`? What about `400u16 as i8`? What if I use 600 instead of 400?
Do the answers `112`, `-112`, `-88`, `88` make immediate sense to you?
I don't mean whether you can explain how we got these values,
but whether it sounds like a sensible behavior for arithmetic operations.

Okay, and what about `0xFE70i16 as i8`? `0x0190u16 as i8`? You can give the answer in the hex.
Easier now?

The point I'm trying to make is that `as` cast for integers makes more sense
as a bit manipulation utility and not as an arithmetic operation.
And even then, it covers a bit too many use-cases for my tasting.

Almost forgot! There's this `usize` thing, which has different sizes on different platforms.
Wouldn't it be funny if your lossless cast to or from this type suddenly & silently became lossy
when moving to another platform? That would surely be hilarious to debug!
(More on this [later](#refactoring)).

#### Floating point

Int-to-float conversion is straight-forward. Kinda.
Sometimes it's lossless (`u16` -> `f32`, `i32` -> `f64`), sometimes it's not (`u64` -> `f64`),
but it behaves more or less as you would expect:
maps an int to the closest representable floating point value.

Float-to-int though...

What about values that are out of range? Does it panic? Does it wrap? Saturate? Produce gibberish?

Well, the answer is that it saturates, so `300f32 as i8` is `127` and `-300f32 as i8` is `-128`.

What about `-1f32 as u8`? Yeah, it's `0`! Makes sense, right?

It's quite obvious now what happens to +INF and -INF. But what happens to NaN?
You're probably screaming, "*Gosh, this is so obvious, of course 'Not-A-Number' is zero!*"
at your monitor right about now. And yes, you are right.

I mean, if I would *have to* choose what integer a NaN should get converted to,
and the answers "It panics", "UB", or "Garbage value" were *off the table*,
I would probably pick `0` myself.
But that's really not a position I would want to find myself in.

And finally, you can obviously do `f32` <-> `f64` with an `as` cast, but that's boring.

### Primitive casts

#### `bool`

You can cast it to any int or float. `true` is `1`, `false` is `0`.
As unremarkable as it can be.

#### `char`

##### `u8` -> `char`

Even though `char` is 32-bit, you can only cast `u8`s to it.
And no, you are not limited to the 7-bit ASCII, any of the first 256 Unicode symbols are OK.

Why not more? Because 16 bits is already enough for you to refer to a surrogate,
and Rust doesn't allow those in `char`s, so you'll have to use a [`char::from_u32(i: u32) -> Option<char>`](https://doc.rust-lang.org/stable/std/primitive.char.html#method.from_u32)
instead.

##### `char` -> Int

But when you cast it in the opposite direction, anything goes.
You can cast a char to an integer. Any integer.
This is equivalent to taking char's 32-bit value[^char-sign] and then `as`-casting it

[^char-sign]: It doesn't really matter whether it's `u32` or `i32`,
    because there isn't 2^31 characters in Unicode (at least yet),
    so the most significant bit will always stay zero.
    And with only 824652 (less than 2^20) codepoints reserved
    [as of September 2023](https://www.unicode.org/versions/stats/charcountv15_1.html),
    this will probably stay the case for the forseeable future.
    Not to mention that only ~290k of those ~825k are actually designated:
    ~150k for actual characters, 2048 for surrogates, and ~137k for private use.


#### enum
    3. enum

### Pointers, references, addresses

#### & -> *

You can cast `&T` to `*const T` and `&mut T` to `*mut T`. So far so good.

#### * -> &

You cannot do this with `as` actually. You do `unsafe{ &*ptr }` instead.

#### * <-> *

You can change the mutability and/or the pointee type with an `as` cast.
Even at the same time.
Even in safe Rust.

#### * -> 0x

To get a pointer's address, you can cast it to an integer. Any integer.
Same story as with `char` pretty much.

#### 0x -> *

To turn an address to a pointer, you can cast an integer to it.
Yes, any integer.
Yes, even signed.
Yes, even if it's too long.
Yes, even in safe Rust.

### Trait objects

<!-- TODO -->

### Functions

<!-- TODO -->

## Why is this a problem?

Anyways, I didn't write this post *just* to infodump on you about the intricacies
of the `as` casts in Rust. I also wanted to talk about a problem with them.

As I have previously stated, it's twofold:
1. `as` does way too many different things at once,
2. there are some implicit and quirky behaviors that it exhibits.

<!-- TODO -->

## What can we do?

1. Divide `as` into separate, less broad (and quirky) APIs.
2. Nerf `as` when those APIs get stabilized.

The latter can probably be done across an
[edition](https://doc.rust-lang.org/edition-guide/editions/index.html)
boundary without much trouble.
And since there is no way it will be ready for the 2024 edition,
we probably have to wait until 2027. But in the meantime,
nothing prevents us from working on the APIs.
In fact, some of them are already in place or will be there soon.
Let's actually go over the use cases for `as` and see where there is
an alternative API already available or going through the stabilization process.

### State of affairs

<!-- TODO -->

## An aside, or "Thus Spoke Stroustrup"

<!-- TODO -->
