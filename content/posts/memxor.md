+++
title = "`memxor`"
summary = "Watching a couple of compiler frontends & backends try to deal with aliasing & produce vectorized code."
tags = ["programming", "complaints", "C++", "Rust"]

draft = true
+++

## Foreword

I remember reading somewhere about Google's efforts to optimize `memcpy` and how it led to ~0.5%
speedup of their entire fleet[^percents], which might sound like a laughably small number, but at their scale
it translated to millions of dollars being saved in electricity bills annually.

[^percents]: To be clear, they did not make `memcpy` 0.5% faster.
	The raw speedup of `memcpy` was a bit more drastic.
	It's just that `memcpy` isn't the only thing that they are executing on their fleet,
	but it's a very commonly used routine nonetheless, so its sped-up still led to a statistically
	noticeable improvement in the performance of their entire fleet.

Unfortunately, I can't seem to find exactly where I've read this,
so I might be misremembering some details.

In any case, today I want to do something similar and write a "`memxor`" that would be
just as cool and fast and optimized and performant. Aaand wait, maybe the compiler can generate it for me?

## The task at hand

Let me clarify what I actually want to do, as there's no such thing as "`memxor`" in C or POSIX.

Essentially what I want is a version of `memcpy` that does `dst[i] ^= src[i]`
instead of `dst[i] = src[i]` in its `for (size_t i = 0; i < n; ++i)` cycle.
Just like with `memcpy`, I will guarantee that ranges `[src, src + n)` and `[dst, dst + n)`
do not overlap, and, you know what? I'm feeling generous. Let's also guarantee that `n = 4096`.[^page]
Hopefully it'll make compiler's job a bit easier.

[^page]: Actually both pointers are also aligned to 4096 bytes, because in reality.

Oh, I guess it needs to be said that we're on x86_64 (what a surprise),
and since there's quite a few SIMD extensions on this architecture with varying degrees of support
from different CPU generations, let's target Tigerlake specifically (for no reason other than
it was the first modern-sounding Intel microarchitecture that I managed to remember the name of).

## The grind

### Rust

```rust
const N: usize = 4096;

pub fn memxor(dst: &mut [u8; N], src: &[u8; N]) {
	for i in 0..N {
		dst[i] ^= src[i];
	}
}
```
<iframe width="100%" height="400em" src="https://godbolt.org/e#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:rust,selection:(endColumn:1,endLineNumber:2,positionColumn:1,positionLineNumber:2,selectionStartColumn:1,selectionStartLineNumber:2,startColumn:1,startLineNumber:2),source:'const+N:+usize+%3D+4096%3B%0A%0Apub+fn+memxor(dst:+%26mut+%5Bu8%3B+N%5D,+src:+%26%5Bu8%3B+N%5D)+%7B%0A%09for+i+in+0..N+%7B%0A%09%09dst%5Bi%5D+%5E%3D+src%5Bi%5D%3B%0A%09%7D%0A%7D%0A'),l:'5',n:'0',o:'Rust+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:r1930,filters:(b:'0',binary:'1',binaryObject:'1',commentOnly:'0',debugCalls:'1',demangle:'0',directives:'0',execute:'1',intel:'0',libraryCode:'0',trim:'1',verboseDemangling:'0'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:rust,libs:!(),options:'-O+-Ctarget_cpu%3Dtigerlake',overrides:!((name:edition,value:'2024')),selection:(endColumn:9,endLineNumber:3,positionColumn:9,positionLineNumber:3,selectionStartColumn:9,selectionStartLineNumber:3,startColumn:9,startLineNumber:3),source:1),l:'5',n:'0',o:'+rustc+1.93.0+(Editor+%231)',t:'0')),header:(),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4"></iframe>

Would you look at that! We get vectorized instructions immediately!

Judging by the code, it does 16 iterations of a loop, XORing 256 bytes on each iteration.
Each iterations is itself an unrolled loop that does the same thing twice: loads 4 256-bit AVX registers, XORs and stores them back.

The best part? This is idiomatic safe Rust. We didn't have to go out of the way
to add any additional annotations, asserts, keywords, checks, `unsafe` blocks or anything.

What we *did* do is we encoded quite a lot of information in the type system.
First of all, the function takes references to arrays with their sizes set explicitly at compile time.
Secondly, we take *references*, one of which is mutable, so Rust knows that those two arrays
cannot possibly overlap at all. In other words, they do not alias,
which is actually quite important for compiler optimizations.
(If you're curious as to why, try to looking up the difference between `memcpy` & `memmove`
and why it exists in the first place, hopefully it'll be enough to give you some idea.)

Anyways, that was rustc 1.93.0 with LLVM 21.1.8, who worked in tandem to give us this assembly.
rustc made sure to sprinkle as many `noalias noundef dereferenceable(4096)` annotations as it could,
while LLVM managed to optimize away all the bounds checks that Rust inserts in your code.

It even manages to optimize away Rust's iterators!

```rust
const N: usize = 4096;

pub fn memxor(dst: &mut [u8; N], src: &[u8; N]) {
	for (to, from) in dst.iter_mut().zip(src) {
		*to ^= from;
	}
}
```

<iframe width="100%" height="300em" src="https://godbolt.org/e#g:!((g:!((g:!((h:codeEditor,i:(filename:'1',fontScale:14,fontUsePx:'0',j:1,lang:rust,selection:(endColumn:1,endLineNumber:2,positionColumn:1,positionLineNumber:2,selectionStartColumn:1,selectionStartLineNumber:2,startColumn:1,startLineNumber:2),source:'const+N:+usize+%3D+4096%3B%0A%0Apub+fn+memxor(dst:+%26mut+%5Bu8%3B+N%5D,+src:+%26%5Bu8%3B+N%5D)+%7B%0A++++for+(to,+from)+in+dst.iter_mut().zip(src)+%7B%0A++++++++*to+%5E%3D+from%3B%0A++++%7D%0A%7D%0A'),l:'5',n:'0',o:'Rust+source+%231',t:'0')),k:50,l:'4',n:'0',o:'',s:0,t:'0'),(g:!((h:compiler,i:(compiler:r1930,filters:(b:'0',binary:'1',binaryObject:'1',commentOnly:'0',debugCalls:'1',demangle:'0',directives:'0',execute:'1',intel:'0',libraryCode:'0',trim:'1',verboseDemangling:'0'),flagsViewOpen:'1',fontScale:14,fontUsePx:'0',j:1,lang:rust,libs:!(),options:'-O+-Ctarget_cpu%3Dtigerlake',overrides:!((name:edition,value:'2024')),selection:(endColumn:9,endLineNumber:3,positionColumn:9,positionLineNumber:3,selectionStartColumn:9,selectionStartLineNumber:3,startColumn:9,startLineNumber:3),source:1),l:'5',n:'0',o:'+rustc+1.93.0+(Editor+%231)',t:'0')),header:(),k:50,l:'4',n:'0',o:'',s:0,t:'0')),l:'2',n:'0',o:'',t:'0')),version:4"></iframe>

### C

<!-- TODO -->

### C++

<!-- TODO -->

## Afterword

Technically, there's no guarantee that the SSE/AVX code will actually be faster than a dumb loop.
Modern CPUs are a nightmare in terms of complexity, and the actual performance will depend on
- code size & layout and how well it utilizes the instruction cache
- data caches too, obviously
- speculative loads from memory
- register renaming performed by the CPU
- CPU port contention
- Intel CPUs throttling their frequency on AVX-512 instructions, lmao

So I'm not trying to say that vectorized = better,
but I have a feeling that compilers tend to generate those instructions for a reason,
and the cases where we saw them *not* do that today weren't really examples of them
taking a calculated decision in order to improve runtime performance, but rather failures
somewhere in the optimization pipeline either due to compiler's own imperfections
or maybe due to semantics of the compiled language getting in the way of optimization.

What I *am* trying to say is: avoid C++ and GNU software. (Not GPL. *GNU*.)
