+++
title = "Rusty corners of Rust"
slug = "rusty-rust"
summary = """
some parts of Rust feel a bit... rusty
(as in "shabby", not as in "unique to the Rust programming language")
"""
tags = ["Rust", "complaints"]

draft = true
+++

<!-- TODO: mention expected reader's knowledge -->

## Preface

I love Rust. Definitely one of the programming languages ever made.

I **love** its expressive type system, its cool exhaustive pattern matching,
its focus on "safety",
and the combined effect that they have on the experience of writing code.

But [others have already summed this up](https://matklad.github.io/2020/02/14/why-rust-is-loved.html)
better than I ever could. (Better than I would have ever been able to?)
Anyways, the point is, I love complaining.
And complaining about things that you love is way more interesting than just hating on something.
Plus maybe it's just my echo chamber,
or maybe Rust indeed has some serious lack of constructive criticism.
Not the "I just can't support that kind of mindset" or "Just don't try to shove it down my throat"
that many Rust evangelists (deservingly) get,
but something directed at the language itself, not at the community around it.
I'm not implying that I'm a brilliant constructive criticizer,
but I want to at least share some thoughts and discoveries I've gathered over the last few years.
Hopefully, they will be of some interest to you.

So, let's get into this.
What are the things about Rust that suck?

## Complexity

Unfortunately, Rust is quite complex.
Some of its complexity is, sadly, necessary if you want the language to protect you
from accidentally writing bad code without noticing or from getting bitten by the imperfections of
reality and the world of computers specifically.
Rust's type system and its notoriously-annoying-to-fight borrowchecker fall into this category.
But, thankfully, if you don't want to deal with *this* kind of complexity, you don't have to!
Just write in go. Or C. Or Python. Or some other language really.
Because this complexity is just a tradeoff for making making errors harder.
But still, it can be viewed as a disadvantage in some cases.

There's also a different kind of complexity, one that could've been avoided.
Fortunately, Rust doesn't really have much of unnecessary convolutedness,
at least from my experience. The only example that comes to mind is
occasional [confusing naming](#bad-naming), which I cover later in this post.

## Lack of versatility

<!-- TODO: black background -->
> Rust is a low-level programming language.
> <br>
> Rust is a high-level programming language.

Although it may sound contradictory or like some literally Orwellian bullshit,
it is actually true, I'd say.
Rust allows you to go really low to all the nitty-gritty details of memory management,
FFI, and what not, but it doesn't *require* you to.
Thanks to a pretty expressive type system that serves as a good tool for building abstractions,
as well as a vast variety of available libraries at [crates.io](htttp://crates.io)
& the ease to manage your dependencies with
[cargo](https://doc.rust-lang.org/cargo/guide/why-cargo-exists.html)
(honestly, my favorite build system out of all I've ever dealt with),
it is surprisingly easy to throw together an application that, say,
[renders source code as pretty images](https://github.com/Aloxaf/silicon)
and doesn't have to care about all of the "ugly" details of how
personal desktop electronic computing devices work.

But what if you're not programming for userspace of a PC/server.
What if you have to do embedded or, god forbid, kernel development?

Weeell,

### `panic!` at the kernel

Rust's approach to error-handling is different than what C++, Python, Java,
or many other languages have. It is more similar to Haskell, for that matter.

There are no throwable/raisable exceptions in Rust.
Instead, there are return values.
Not C-style "returns `-1` if failed", but something more similar to Java's checked exceptions.

However, return values are for failures that your program *can* and *should* handle.
The "you've tried to open a file but it's not there" kinda thing.

But what about failures your program cannot reasonably handle?
Like out-of-bounds array accesses, division by zero, etc.
If you knew it could happen, you would've inserted a `if index < array.length()` /
`if divisor != 0` check.
But if you didn't and such a bad thing already happened, what should your program do?

Rust decides to `panic!`.
Essentially, it aborts your program
(usually also unwinding the stack and running the destructors).
You can read more about it in
[the dedicated chapter of Rustbook](https://doc.rust-lang.org/stable/book/ch09-00-error-handling.html).

This approach works fine in userland, where if your problem has a bug,
it's better to abort with a stacktrace than to continue running in a possibly corrupt state
potentially allowing bug's influence to screw up more and more things.
You probably has some service manager anyways, be it Docker, SystemD or whatever,
and it will restart your program if needed.

But what if there's no supervisor? What if you're running on bare metal,
possibly in a critical system?
You don't want your OS kernel to immediately panic if there's a bug in the audio driver, do you?
[Neither does Mr. Torvalds](https://lkml.org/lkml/2021/4/14/1099).

The actual reason Linus talks about`panic!`s in the email above
has to do with Rust's implicitness when ot comes to memory allocations,
which I also want to touch on, but not before I mention
there are [some](https://github.com/dtolnay/no-panic)
community-made [libraries](https://github.com/lachlansneff/no-panics-whatsoever)
that ensure (during compilation) that there are no panics possible in the given pars of the code.
Sadly, they are somewhat limited and it would be nice to have this as a language feature
(and not a 3rd party library) anyway.

### Implicit memory allocation

Actually, let's talk about being explicit first.

#### Explicit memory allocation

Have you heard of [Zig](https://ziglang.org/)? I've had, but only briefly.
It seems to be one of those "BetterC" / "C++ but actually good" languages
that for some reason get seriously underappreciated, just like
[the poor ol' D](https://dlang.org/).

(Zig is also
[a pretty cool build system and a C/C++ compiler toolchain](https://youtu.be/YXrb-DqsBNU?t=262),
but that’s another story and shall be told another time.)

Anyways, the reason I'm bringing it up is not to rant about how
C++, while an important pioneer, is a language with a disastrous legacy,
it's not even [Zig's WASI-based compiler bootstrapping](https://ziglang.org/news/goodbye-cpp/),
although I will touch on this [later](#bootstrapping).

The reason I'm bringing Zig up is its
[manual memory management](https://ziglang.org/learn/overview/#manual-memory-management).
Not the lame C/C++ "call `malloc` / use `new` and then check for `NULL` / catch `bad_alloc`", no.
An actually good API that allows you to be explicit about allocators and the allocation process.

Rust doesn't have that.

#### Aborting on failure

You can actually change your global allocator
[in a pretty reasonable way](https://doc.rust-lang.org/std/alloc/trait.GlobalAlloc.html),
but even then, an unsuccessful attempt to allocate memory will cause Rust to panic.
Suboptimal, but I guess works for most of use cases in userland.
I mean, how many times did you actually try to catch `bad_alloc` in C++?
And even if you caught it, now what? What should your program do?
I guess there's a reason why Java's `OutOfMemoryError` is an `Error`, not an `Exception`,
and thus shouldn't be caught (it *can* be, but generally you're advised not to catch `Error`s in Java).

My C++ teacher brought up the topic of allocation failures during one of the lectures,
and asked, "Suppose you've run out of memory and got `bad_alloc`, what do you do now?"
The reality is, you don't have that many reasonable options:
* You can abort immediately. Pros: reliable. Cons: destructors aren't executed.
* You can let the exception propagate and unwind the stack.
Pros: destructors are executed. Cons: doesn't scale well with multiple threads,
plus you have to pray your destructors don't allocate.
* You allocate some kilobytes in a global buffer when the program starts,
and then, once you run out of memory, you deallocate that buffer,
thus, hopefully, giving you enough memory to do something (preferably shutdown gracefully).
Pros: might actually work. Cons: hacky as hell.

He also mentioned that Firefox chooses to abort immediately in its codebase (iirc),
so I wonder if Mozilla carried over this decision into Rust as well.

This strategy actually isn't that bad for standard allocators like `malloc`.
The reason? Virtual memory, overcommitment, OOM killer.

#### Overpromise. Underdeliver.

[Virtual Memory](https://en.wikipedia.org/wiki/Virtual_memory) is kinda cool.
Sadly, I don't have a good article or video that would explain it, so I'll try to do it myself.
Feel free to skip to the next section if you already know this stuff.

<!-- TODO: explain Virtual Memory -->
<!-- Mention: address collisions between processes, shared memory, mmaped IO, swap,
stable vector,
fork+exec, rwx page permissions, tlb, stack protection,
overcommitment (can be toggled on linux, ??? on mac and windows), OOM killer
what it means to your program: allocations never/rarely fail, but at a random point at time
you can SEGFAULT(??? check what signal is actually being sent, if any),
so it makes sense to panic!
overcommitment isn't actually useless, give example of REDIS and maybe something else -->


#### Custom allocators

<!-- TODO: mention, local allocators that can fail (arena shit),
and that for them you can try falling back to different allocation (another arena or just malloc),
or maybe you just wanna abort the task in this case, without aborting the whole process
(say, tell client to GTFO and give them a 5XX).
kalloc doesn't overcommit (probably)
Alexandrescu's talk about combining allocators
-->
Even though it may be unrealistic to properly handle allocation failures at the scale of
the entire program, it might make sense to handle them locally.
<!-- TODO: restructure this part -->

Also, sometimes you want to use different allocators in different parts of your code.
Changing the global allocator won't do. Rust has an API for that, but it's still
experimental nightly-only and doesn't seem to be advancing towards completion,
considering [the corresponding issue](https://github.com/rust-lang/rust/issues/32838)
is 7 years old.

But if you look at the definition of the `Vec` type from std, you'll see
```rust
pub struct Vec<T, A = Global>
where
    A: Allocator,
{ /* private fields */ }
```
And there even are some methods that do not panic on allocation failure:
```rust
pub fn try_reserve(
    &mut self,
    additional: usize
) -> Result<(), TryReserveError>

pub fn try_reserve_exact(
    &mut self,
    additional: usize
) -> Result<(), TryReserveError>
```
but that's pretty much it.
You won't find `try_append`, `try_push`, `try_extend`, `try_insert`, `try_resize`, etc. (At least yet?)
And even if they existed, the naming scheme that emerges here is kinda...
meh. Really makes you feel that fallible allocation was an afterthought for the language.

### OOP
<!-- OOP considered harmful, case for using dyn Traits -->
#### verbosity
#### pattern matching on types + general box pattern matching (especially for recursive types, haskell example)
#### equality


## Soundness
### [In Internet Historian's voice]: "What IS *soundness*?"
Are leaks sound? Am I sound? Why is it called soundness if I can't hear it?

<!-- Soundness = inability of safe programs to cause UB -->
<!-- Safe shouldn't be able to cause unsoundness (AKA UB)  -->
<!-- https://matklad.github.io/2023/04/02/ub-might-be-the-wrong-term-for-newer-languages.html -->
<!-- Leaks are safe, sometimes advised -->
### What is UB?
<!-- Null ptr, use-after-free (including double-free), invalid pointers, misalignment, etc -->

<!-- Forward progress guarantees -->
<!-- mention https://blog.rust-lang.org/inside-rust/2020/03/19/terminating-rust.html -->
<!-- "LLVM made a C++-centric assumption which resulted in unsoundness but in Rust, speaking of which... -->

### fake-static

Rust has [69 open soundness issues](https://github.com/rust-lang/rust/issues?q=is%3Aopen+is%3Aissue+label%3AI-unsound)
as of writing this post.

Some of them are pretty boring,
like [an upstream LLVM bug](https://github.com/llvm/llvm-project/issues/48911) resulting in a
[possible double-free](https://github.com/rust-lang/rust/issues/100914) when using big stack sizes on Linux.

Some are pretty funny, like a risk of getting a collision in
[type ids](ahttps://doc.rust-lang.org/stable/std/any/struct.TypeId.html),
because those are (currently) implemented as 64-bit hashes of the type information.
And while, yeah, `2^64` is not a small number,
remember that [birthday paradox](https://en.wikipedia.org/wiki/Birthday_problem) exists.

But some issues are pretty sad, like [the infamous fake-static bug](https://github.com/rust-lang/rust/issues/25860),
which is turning 8 years old as of writing this 🥳🎉.

Thankfully though, this is an implementation bug and not a soundness hole in the type system,
and it is pretty esoteric for that matter
(as with many current soundness bugs; the chances you encounter any of them in the wild are pretty low).
I guess it's worth mentioning that this bug can potentially be fixed
by an [upcoming new version of the trait solver](https://github.com/rust-lang/rust/issues/107374),
which is a thing that essentially type-checks your Rust code (including lifetimes),
but this is a huge undertaking and it still has a long way to go.

### Safety

Anyways, the point that I'm trying to get across is that
the notion of safe Rust programs being UB-free relies on two assumptions:

1. Compiler isn't buggy
2. Unsafe code that your safe program relies on is correct

The first assumption is pretty reasonable.
I mean, of course there is no such thing as code that isn't buggy.
But if you are not willing to make the assumption that your compiler is sufficiently bugless,
then I'm not even sure how you are planning to write any code in any language at all.

The second assumption is a bit trickier.
While rustc is a giant open-source project with years of extensive testing
and frequent contributions from a wide community,
the unsafe code that you yourself write may not necessarily hold up to the same standards.

But the good news is that most of the time you don't even need to write unsafe code at all.
This allows to scale down the surface area for potential bugs drastically.

The bad news is, unsafe Rust is significantly harder to write than C/C++ (which are 100% unsafe in Rust's sense),
due to the fact that it has to not cause any UB even when the outside safe code used it incorrectly.
The Rustonomicon puts it better than I can
in [this chapter](https://doc.rust-lang.org/nomicon/safe-unsafe-meaning.html).

Safe/Unsafe separation isn't a panacea. It's a tradeoff. But hopefully a worthy one.


## Questionable design choices
### Closure syntax
### print! syntax
### Range: !Copy
### Nullable pointers

The "[billion-dollar mistake](https://en.wikipedia.org/w/index.php?title=Null_pointer&oldid=1154390844#History)",
now in Rust.

All the emphasis on the great `Option<T>` type, but `*const T` and `*mut T` can still be null. Why?
Well, I guess it kinda makes sense for FFI purposes to have some consistency
with how `T const*` and `T*` work in C/C++, but damn...
At least let me use your type system and give me some special type to denote that the pointer isn't null.
After all, you have a
[`NonZero_` variation for pretty much every integer type](https://doc.rust-lang.org/stable/std/?search=NonZero).

Oh wait, there *is* an `std::ptr::NonNull<T>`!
Let's take a look at [its docs](https://doc.rust-lang.org/stable/std/ptr/struct.NonNull.html):
> `*mut T` but non-zero and [covariant](https://doc.rust-lang.org/reference/subtyping.html).
>
> This is often the correct thing to use when building data structures using raw pointers,
> but is ultimately more dangerous to use because of its additional properties.
> If you’re not sure if you should use `NonNull<T>`, just use `*mut T`!
>
> ...
>
> If your type cannot safely be covariant, you must ensure it contains some additional field to provide invariance.
> Often this field will be a [PhantomData](https://doc.rust-lang.org/stable/std/marker/struct.PhantomData.html) type
> like `PhantomData<Cell<T>>` or `PhantomData<&'a mut T>`.
>
> Notice that `NonNull<T>` has a `From` instance for `&T`.
> However, this does not change the fact that mutating through a (pointer derived from a) shared reference
> is undefined behavior unless the mutation happens inside
> an [`UnsafeCell<T>`](https://doc.rust-lang.org/stable/std/cell/struct.UnsafeCell.html).
> The same goes for creating a mutable reference from a shared reference.
> When using this `From` instance without an `UnsafeCell<T>`,
> it is your responsibility to ensure that `as_mut` is never called, and `as_ptr` is never used for mutation.

Wut. So you do have a non-nullable alternative for raw pointers, but only for `*mut T` (screw `*const T`, I guess),
with a name that mentions neither `Mut` nor `Const`,
also aside from nullability this type bears additional implications about variance too,
and, finally, it is safe to get this thing from a non-mutable `&T`?
Wow.

> NOTE: While writing this, I've learned that apparently
> `*const T` can be cast to `*mut T` in safe Rust using the `as` keyword.
> Huh.

What a peculiar thing.
I guess it is something more of a middleground between `*mut T` and `&mut T`
(with an API convenient for writing containers),
rather than just a `p: *mut T` such that `p.is_null() == false`.
This explains why it exists, but doesn't explain why it's named so poorly.

Oh look, the next chapter is exactly about bad naming!

### Bad Naming

`NULL` pointers isn't the only terrible programming language legacy that we have.
There are also null-terminated sequences,
that shrink `{T* ptr; size_t len}` down to `T* ptr`
at the tiniest cost of a small terminator at the end of the sequence,
which causes numerous off-by-one errors and awful asymptotics.

[GTA Online was parsing JSON in `O(N^2)`time because of this](https://nee.lv/2021/02/28/How-I-cut-GTA-Online-loading-times-by-70/).

[C++ programmers suffer because of this](https://youtu.be/kPR8h4-qZdk?t=1150).

But good luck trying to get rid of it, you'd have to
[redevelop the entirety of the Linux Kernel](https://en.wikipedia.org/w/index.php?title=Linux_kernel&oldid=1154452828#Estimated_cost_to_redevelop),
or at least refactor millions upon millions of lines of code that it has
and then all of the applications that depend on it (which there are quite a few).

But that's not what this story is about.
This story is about a guy named Alex Stepanov, who, together with someone named Meng Lee, developed a small C++ library
that was named STL totally because it was a "Standard Template Library" and not because "**ST**epanov + **L**ee".
This small library eventually made its way into C++'s `std`, bringing with it a funny little container,
a dynamically sized array named `vector`,

#### Vec

which is a terrible name for a sequence that can change its length.
The term "vector" is already popular in maths and physics, and there its size is always fixed.

But not in C++. In C++, `std::array` [has a fixed size](https://en.cppreference.com/w/cpp/container/array),
but `std::vector` [doesn't](https://en.cppreference.com/w/cpp/container/vector).

Rust happily carries this over:
- [a fixed-size array, denoted `[T; N]`](https://doc.rust-lang.org/stable/std/primitive.array.html) and
- [a contiguous growable array type, written as `Vec<T>`, short for 'vector'](https://doc.rust-lang.org/stable/std/vec/struct.Vec.html).

#### `dbg!`

A pretty cool macro from std for [quick & dirty debugging](https://doc.rust-lang.org/stable/std/macro.dbg.html).
Who needs lldb or gdb, when you can just print to stderr?
But, perhaps surprisingly, this macro still works in `--release`.
I guess it makes sense to not limit it just to debug builds,
after all, you can always just
```rust
macro_rules! debug {
    ($($tokens:tt)*) => {
        #[cfg(debug_assertions)]
        dbg!($($tokens)*);
    };
}
```
if you want, but maybe it could have been named better.

#### crates/modules/packages, crates.io

#### `unsafe fn`

`unsafe fn` is a `fn` that is unsafe to call.
In other words, it cannot be called outside of an `unsafe{ }` block.
However, it doesn't mean that this function itself uses any unsafe operations.
For example, take a look at [`Vec::set_len`](https://doc.rust-lang.org/stable/std/vec/struct.Vec.html#method.set_len):
```rust
pub unsafe fn set_len(&mut self, new_len: usize) {
    debug_assert!(new_len <= self.capacity());

    self.len = new_len;
}
```
It only modifies a private integer field of a struct, which you can easily do in safe Rust.
But since there is unsafe code relying on this field holding a value that makes sense,
it is possibly to indirectly cause UB by writing something bad to this variable.
Thus, this function is unsafe to call.

Yeah, it could have performed additional checks to make sure that the values is okay,
and then we would be able to mark this fn as safe,
but that's an API design choice, and neither option is inherently wrong.

Similar situation with
[`unsafe trait`s](https://doc.rust-lang.org/book/ch19-01-unsafe-rust.html#implementing-an-unsafe-trait).
The fact that a trait is unsafe, does not mean that it uses unsafe code.
It simply denotes that when you implement this trait for your type,
you must uphold some additional guarantees that cannot be enforced by the compiler.


For some weird reason though, Rust currently treats bodies of unsafe functions as unsafe blocks.
Which means that unsafe function are not only unsafe to call
but are also able to perform any unsafe operations in their implementation
without an explicit `unsafe{ }` block.
That's suboptimal, for two reasons:
1. a function doesn't necessarily have to execute unsafe code to be unsafe to call (see above)
2. even if it does have to do something unsafe inside, I would still love to limit the area of unsafe blocks.
The body of the function might be big, and if I have to dereference a raw pointer in one type,
it doesn't mean I also want to opt out of compiler checks that would save me from erroneously calling, say,
[some `str` method that can break UTF-8 invariant](https://doc.rust-lang.org/stable/std/primitive.str.html#method.as_bytes_mut)
when I do not need it.

Oh wait, [they are already working on it](https://github.com/rust-lang/rust/issues/71668).

#### Pin

## In-place semantics
### Construction
### difference between semantic move and actual move
### Pin-guarantees, pin projection, move does not move, it transfers ownership, &mut can move, pin is an afterthought, Unpin is terrible name, ?Move like ?Sized, C++ is coming up with trivially_reallocatable

## State of async
### no async in std, being generally confusing
### structured concurrency

## Some good things to get
### readf
### generators
### Ok-wrapping
### immutable members
### named tuple fields: less boilerplate of #[derive(Copy, Clone, Debug, Default, Hash, PartialEq, PartialOrd, Eq, Ord)] (not to mention foreign traits) (thankfully (u8,u16,u8) is 4 bytes), difference with tuple-like structs
### variadic tuples, mention impls for up to 12 function args, lol (waffle recently merged one like impl From<(T, T)> for [T; 2])
### more generality over primitive numbers (an example of writing a fraction or a complex number)?
#### autorefs for Copy types
### generic over asynchronicity (async map, etc)
### stable fn traits
### ! type (still unstable, but you can actually get it from some signature into stable, has problems with falling back to (), is required? for panic handlers)

## Misc
### early drops and scope guards, maybe defer / with-resources would be better? Drop considered harmful
### compile times???
### Build-time arbitrary code execution, the new norm
### Bootstrapping
because Zig's compiler is, surprise-surprise, written in Zig, so how do you
compile Zig compiler if you need a Zig compiler to compile it?
Rust also has this problem, and [it's rather painful](https://youtu.be/oUIjG-y4zaA),
but I'm not going to dive into this here, because, thankfully, I never had to deal with it myself
### Second compiler (link to a talk (was mentioned in Zig paragraph), mention gnu rust compiler and the (abandoned?) one written in C++)
<!-- https://matklad.github.io/2023/04/13/reasonable-bootstrap.html in case I read it -->
### [What Linux wants](https://github.com/Rust-for-Linux/linux/labels/meta)
### async trait methods
### Unbound generic parameters on structs, implied bounds, specialization

<!--  TODO: conclusion, tool way longer than anticipated, actually a reason for creating the blog in the first place -->
