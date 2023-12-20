+++
title = "A rusty toolbox"
slug = "rusty-tools"
summary = "Some (subjectively) useful tools for your Rust projects"
tags = ["Rust"]

draft = true

toc = true
+++

Since it will take me forever to finish the post I initially wanted to write
and created this entire website for in the first place,
here's a smaller thing in the meantime.
Which still took more time to write than I would like to admit
& [one OSS contribution](https://github.com/gohugoio/hugo/pull/11559) more
than I expected.

## Testing

Rust has first-class support for testing your code:
just write a testing function, slap `#[test]` on top,
and type `cargo test` to it.
Simple as that!

So, since most of the tooling you'd need is already provided out of the box,
and I'm not covering frameworks & libraries in this post,
this section is pretty short.

### Tarpaulin
[GitHub](https://github.com/xd009642/tarpaulin)

Code coverage made simple.
Just type `cargo tarpaulin` and it will run all of your tests,
while keeping track of which lines in your source code got executed and which didn't.

This way you can easily get an overview of what percentage of your code is actually tested
& make sure that your tests don't miss some if/match branch in some function or whatnot.

I did encounter a couple of insignificant issues while using it though.
Namely,

1. Occasional false negatives when a line consists solely of `loop {` or stuff like `Some(x) => {`.
Might be annoying if you're aiming for 100% coverage, but otherwise not a big deal.
2. Speaking of 100% coverage. It would be nice to get
[the `#[coverage(off)]` attribute](https://github.com/rust-lang/rust/issues/84605),
(which allows to not take some pieces of code into account when calculating coverage)
stabilized, but that's not a tarpaulin-specific thing.

### cargo-careful
[Blog post](https://www.ralfj.de/blog/2022/09/26/cargo-careful.html),
[GitHub](https://github.com/RalfJung/cargo-careful),
requires nightly

Rust ships with its standard library pre-built (for most of the targets).
And this pre-built version has `debug_assert`ions turned off.

cargo-careful reenables them.
This is mostly useful for `unsafe` code & catching (some) UB,
because outside of the `unsafe` world, Rust already has your back.

It won't catch as much UB as [Miri](#miri) can,
but is significantly faster & can run *any* Rust code,
without any FFI or API limitations that Miri imposes.
(Because it produces and runs a regular binary, yes.)

Speaking of,

### Miri
[GitHub](https://github.com/rust-lang/miri), requires nightly

Instead of compiling your Rust code,
Miri interprets it with all kinds of additional checks in place.

Well, technically you do compile your code,
but only to MIR (Rust's internal simplified representation)
and not an actual binary, but even
[CPython compiles Python to some bytecode before interpreting it](https://docs.python.org/3/glossary.html#term-bytecode),
so this shouldn't be surprising.

Anyways, as cool as it is, Miri isn't a panacea.
It doesn't catch *all* the UB there is
(not to mention Rust still hasn't made up its mind on what is UB and what isn't),
and it has some limitations on what code it can run.
And, as expected, this kind of interpreting is slower than
running compiled code (even unoptimized).

### Kani Rust Verifier
[GitHub](https://github.com/model-checking/kani)

Catching UB is cool and all, but what about correctness?
Well, hopefully your test suite is extensive enough
to catch most of the correctness issues,
but, in general, covering **all** possible inputs with a test suite is infeasible.

You can resort to fuzzing, but even at Google's or Facebook's
scale it didn't help to catch
[CVE-2023-4863 in libwebp](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2023-4863)
or [zstd#3517](https://github.com/facebook/zstd/pull/3517)
even [after several years of fuzzing](https://t.me/experimentalchill/255).

I mean, even for a simple piece of code that transforms one `u64` in some way,
checking all the possible inputs will take an enormous amount of time.
Say, you spend only one nanosecond testing each input.
2^64 nanoseconds is ~585 years. Yikes.

Enter *model verification*: instead of running the code,
represent it as a big boolean expression,
where each variable is a bit in the input data and the value
of the expression corresponds to the correctness of the program
(as determined by assertions, either explicitly written by the programmer
or implicitly inserted to check arithmetic overflow, null pointer dereferencing, etc),
and solve [SAT](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem) for it.

Although SAT is NP-complete in the general case,
hopefully checking your particular formula over 64 variables
will be faster than running the same piece of code 2^64 times.

Once again, this is not panacea. For example,
[kani doesn't handle `for` loops particularly well](https://model-checking.github.io/kani/tutorial-loop-unwinding.html).
But in many cases,
[it still can do a fantastic job](https://model-checking.github.io/kani-verifier-blog/2023/03/31/how-kani-helped-find-bugs-in-hifitime.html).
I, personally, found kani to be **the** thing I go to, whenever I need to write
some arithmetical or bitwise manipulations, because, oh boy,
do I not want blow my leg off due to some stupid off-by-one error.

## Managing dependencies

### cargo-msrv
[GitHub](https://github.com/foresterre/cargo-msrv), requires rustup

[TODO](https://doc.rust-lang.org/cargo/reference/manifest.html#the-rust-version-field)

### cargo update

### cargo-semver-checks
[GitHub](https://github.com/obi1kenobi/cargo-semver-checks)

### cargo-udeps
[GitHub](https://github.com/est31/cargo-udeps), requires nightly

### cargo-audit
[Website](https://rustsec.org/)

## Analyzing

### cargo-expand
[GitHub](https://github.com/dtolnay/cargo-expand)

### cargo-bloat
[GitHub](https://github.com/RazrFalcon/cargo-bloat)

### cargo-asm
[GitHub](https://github.com/gnzlbg/cargo-asm)

## Optimizing

### [cargo-]flamegraph
[GitHub](https://github.com/flamegraph-rs/flamegraph), requires perf

### cargo-remark
[Blog post](https://kobzol.github.io/rust/cargo/2023/08/12/rust-llvm-optimization-remarks.html),
[GitHub](https://github.com/kobzol/cargo-remark), requires nightly

### cargo-pgo
[GitHub](https://github.com/Kobzol/cargo-pgo), requires some things (see repo)

## Building

There's only one entry in this section.
Which, I guess, is a testament to how good Rust's out-of-the-box build system is.

There's no need to write a [convoluted build script](https://gradle.org/) in some DSL
based on a [niche JVM language](http://www.groovy-lang.org/) with an option to switch
to a version of the same DSL overlaid on top of
[a different, less obscure language](https://kotlinlang.org/).

There's no need to juggle setuptools, pip, wheel, distutils,
`requirements.txt`, `setup.py`, `pyproject.toml`, `poetry.lock`, & venv
at the same time just to *hopefully* get your project working.

(Though, I've once heard someone genuinely praising Python's infrastructure of
managing dependencies and building your projects.
The reason, however, was that that person came from the C/C++ world,
where you have to download and install your dependencies manually.
So yeah, I can see how having pip in this situation is like a heaven on earth.)

Most of the time, you can get away with
[a simple and pretty straight-forward toml](https://doc.rust-lang.org/cargo/reference/manifest.html).
But if that's not enough, you can write a build script.
[In Rust](https://doc.rust-lang.org/cargo/reference/build-scripts.html).
No need to get used to Groovy just to build Java.

All that being said, Rust's situation isn't 100% Utopia.

### cargo-zigbuild
[GitHub](https://github.com/rust-cross/cargo-zigbuild)

(Un?)fortunately, Rust doesn't have its own backend for native machine code generation.
Instead it relies on LLVM to do the heavy lifting for it.
(Well, it's not like converting Rust code into LLVM IR is that simple either,
but that's beside the point.)
And LLVM comes from the C's world of compilation units, object files, linking,
and other nightmarish things.

Neither does Rust try to reinvent the wheel when it comes to platform-specific runtimes.
Instead of hand-coding all the syscall numbers and what have you,
it relies on C's runtime (or its analogue) for most of the platform it targets.
You get musl or glibc for Linux, some MSVC dll's for Windows, or even
[Objective C for Apples' stuff](https://github.com/rust-lang/rust/blob/df4379b4eb5357263f0cf75475953f9b5c48c31f/library/std/src/sys/unix/args.rs#L205).

And while
```shell
rustup target add $target_triple
cargo build --target $target_triple
```
can get you pretty far, it won't be able to link the binary for you or
build the platform-native libraries without a proper toolchain installed on your part.

If you're on Windows, ehhh... Why?
Even gaming on Linux, thanks to Valve's recent tremendous efforts,
has become less of a meme than developing on Window.
Your best option here is probably to install Linux.
Preferably as a standalone system, but a WSL is fine too.
Then, proceed to "If you're on Linux" section.

If you're on Mac, you probably know it better than I do.

If you're on Linux, use your package manager, duh!
If you're lucky, it will have the *32-bit MIPS Little Endian Linux Musl* toolchain that you need.
Or, hopefully, your tastes aren't **that** specific.

But if they are, may I interest you in [Zig](https://ziglang.org/)?
Despite its "1930s Germany"-sounding name and its "2022 Russia"-looking logo,
it's a pretty neat thing.
Not only is it a "better-C" language with cool compile-time capabilities,
it is also a build system, and a cross-compilation toolchain.

Here, take a look:
{{< youtube id="YXrb-DqsBNU" start=453 >}}

And as you might have guessed by now, `cargo-zigbuild` is a wrapper
that makes `cargo` use Zig's toolchain for native code compilation/linking.
