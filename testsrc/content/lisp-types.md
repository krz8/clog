(article
    title "Performance and Types in Lisp"
    desc "Part 1 about using numeric types in your Lisp code for
          robustness and speed."
    date "2014-03-21 12:00:00 -04:00")

Some time ago, a few friends of mine encouraged me to write some posts
about Lisp: why I enjoy developing in it, what sets it apart from
other languages, and so on.  Other people do an admirable job of that,
I've found, and often say things far better than I could.  But maybe,
instead, I could occasionally post about things I find particularly
nifty about Lisp.

So, bearing in mind that there's no order or any ranking to these
articles, let me start with some aspects of types in Lisp.

I know what some of you are thinking: _"But, Lisp doesn't have
types!"_ Yes, it does.  In fact, the Common Lisp type system is a
surprisingly complete hierarchy of types for all kinds of different
values in the running system.  Using it judiciously is how one
helps the optimizer in a typical Common Lisp compiler to generate
efficient code.  This article will dive into that.

Among the uninitiated, Lisp has a reputation for being dynamically
typed and therefore slow. This is only half true. Let's take a closer
look at a common situation and see what's really going on.

_Nota bene:_ The examples below take advantage of a particularly
powerful implementation of types in the freely available
[SBCL](http://sbcl.org/ "SBCL Home") implementation, and the examples
are taken directly from SBCL as well. I know that
[LispWorks](http://lispworks.com/ "LispWorks Home") also provides a
similar level of optimization. The point of this article is to
demonstrate what is possible by reasoning within a well defined type
system, but not to cover every potential system available.



A Simple Example
----------------

Here's a simple function in Lisp that takes two values and returns the
exclusive-OR of their bitwise values.

```lisp
(defun foo (a b)
  (logxor a b))
```

For you C types, you might be tempted to think that's the same as

```c
unsigned int
foo( unsigned int a, unsigned int b )
{
        return a^b;
}
```

Actually, it's not the same at all. The most glaring difference is
that the Lisp definition is bereft of all type information. One could
be forgiven for thinking there's some kind of default typing going on,
although the reality is far more interesting as you'll see below.

In the C/C++ world, there's quite a bit of implicit type conversion
that goes on in the compiler. For example, you could call `foo()` with
byte arguments, and the compiler would take care of converting those
arguments into your platform's long word type, before handing you back
a long word as the result.  As long as you don't mind that "bytes go
in, ints come out" of `foo()` (and many people don't), everything
works just fine.

Lisp takes a different approach, though. Because Lisp is dynamically
typed, it isn't until runtime that the system knows what kind of
values you're passing as arguments. If you happened to call it with
byte arguments, you would get a byte as a return value. If you called
it with integer arguments, you would get a integer value returned.

_"How does it know what type to return?"_

Good question. Before we answer that, though, let's turn our attention
to the types of the arguments passed in. Then we can look closer at
the type of the value returned.



How Big is an Argument?
-----------------------

In the Lisp definition above, no type information was provided at
all. The system has to examine both arguments to see what types it has
been presented with, and then consult what it knows about `logxor` in
order to handle those arguments. There _could_ be different
implementations of exclusive-OR that handle different combinations of
types; _e.g.,_ there might be a `logxor-byte-byte` that's called when
two bytes are presented, a `logxor-byte-word` that's called when a
byte and a word are presented, and so on. Sure, that's a possibility.

With a C background, you might reasonably instead suspect that for any
type smaller than the intrinsic integer type, it will be converted to
that integer type. The exclusive-OR is performed on those converted
values, and the resulting value is handed back to the caller. This
conversion would be performed at runtime, though, and you can bet
there's more than a few functions being called to handle these
conversions. This is where a performance penalty might appear, of
course. The C version determines what kind of upcasting was necessary
at compile time, whereas the Lisp version waits until run time to
perform that same analysis.

Before we get too carried away berating Lisp for making a bad decision
in the trade-off between compile time and run time, consider what
happens if `foo()` is called with arguments larger than the size it
was compiled with. To wit, assuming a system with 32 bit integers that
does happen to support 64 bit words via `unsigned long long`,

```c
unsigned int x = foo( 13, 12345678901234567890ULL );
```

Oops! That's going to elicit some kind of diagnostic from the
compiler. And it should, because as compiled, this isn't going to do
what's expected. At best, because `foo()` was compiled with 32 bit
`int` semantics, passing in a 64 bit value is probably going to chop
the top half of the word away.

> _Aside:_ Much like the growing pains of 16→32 bit conversions
> (sadly, I still remember porting lots of PDP code onto VAX and Tahoe
> systems long ago), we're mostly done now with the transition from 32
> to 64 bit systems. Modern C environments provide 64 bit integers,
> and C99 guarantees that `unsigned long long` is 64 bits in length. A
> 128 bit version of ULL may be right around the corner, though, so
> don't be too quick to blow this off.

Now that we've seen some hidden traps in the as-written C definition,
let's try the same thing on the Lisp version seen above.

```
* (foo 13 12345678901234567890) 
12345678901234567903
```

Hmm. So, maybe our earlier guess about `foo` was wrong? Maybe, because
we didn't supply any type information, it did use a default integer
size but chose 64 bits instead of 32? If that's the case, it should be
easy to trip it up with an even bigger value.

```
* (foo 13 123456789012345678901234567890)
123456789012345678901234567903
```

Alright, so it could be that the compiler decided to get all fancypants and
make foo use 128 bit integers by default. Okay, fine.

```
* (foo 13 999999999999999999999999999999999999999)
999999999999999999999999999999999999986
```

…or not. Hopefully, if you've never worked in a Lisp environment
before, that should give you a moment's pause. We've supplied
absolutely no type information to Lisp, and yet it seems to be freely
dealing with integer values of any size. Just for the sake of
argument, we will make one more call to `foo` with a 678 bit
integer. Let's XOR 13 and 2<sup>677</sup>.

```
* (foo 13 (expt 2 677))
627057063764139831929324851379409869378845668175598843037877190478889006888518431438644711527536922839520331484815861906173161536477065546885468336421475511783984145060592245840032548652210559519683510285
```

Yowza. Clearly there's more to that minimalist definition of `foo`
earlier that meets the eye, right?

What we're seeing here is the numeric side of the Lisp type system in
play. Lisp automatically promotes values that can't fit inside
hardware registers to arbitrary precision integers (often called
“multiple precision numbers” as well as just “bignums” for short). To
be fair, Lisp isn't unique in this regard; lots of other languages
(_e.g.,_ Mathematica, Python) have adopted this convention as well,
and there are libraries available for C and others that give you the
same functionality when you need it.

There is no such thing as a free lunch, the saying goes, and that
applies here. Obviously, working with bignums is expensive, so Common
Lisp and other systems will promote values to them only when
necessary. That avoids the computation penalty, of course, but there
is still all the runtime type analysis of the argument values passed
to foo. We've still got a long way to go before we can even get close
to the speed of statically-typed languages.

Or do we?



What Did We Actually Tell the Compiler?
---------------------------------------

Let's re-examine the foo function we defined earlier. We will tell the
compiler to optimize the heck out of this function, paying no regard
to space or even runtime safety. On the other hand, we'll ask it to
leave as much extra infomation behind as it can regarding the function
itself.

```
* (declaim (optimize (speed 3) (safety 0) (space 0) (debug 3)))
* (defun foo (x y)
    (logxor x y))
FOO
```

Now, let's ask the compiler what it knows about foo.

```
* (describe 'foo)

COMMON-LISP-USER::FOO
  [symbol]

FOO names a compiled function:
  Lambda-list: (X Y)
  Derived type: (FUNCTION (T T) (VALUES INTEGER &OPTIONAL))
  Source form:
    (SB-INT:NAMED-LAMBDA FOO
        (X Y)
      (BLOCK FOO (LOGXOR X Y)))
```

Most of that you can safely ignore, but let's look at the **Derived
type** line. It's telling us that foo names a function taking two
arguments of type `T`, returning an `INTEGER`.

Without getting too deep into the Lisp type system, it's helpful to
note that `T` is the superclass of all types in the
hierarchy. `INTEGER` is a subclass in that hierarchy, a type of
`NUMBER` (other numeric subclasses are ratios (fractions), reals, and
complex numbers). So what we're seeing already is that Lisp knows that
foo returns an integer.

_"How could it know that, we didn't tell it anything?"_ Ah, but we
did. We called `logxor`, the logical exclusive-OR function. Many Lisp
engines are very good at inferring types, so it analyzed all the code
inside `foo` (just a single function call in this case) and determined
that since `logxor` returns an integer, so must foo. How does it know
this?  `logxor` was defined that way, taking one or more integers as
its arguments:

```lisp
* (describe 'logxor)
⋮
LOGXOR names a compiled function:
  Lambda-list: (&REST INTEGERS)
  Declared type: (FUNCTION (&REST INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the bit-wise exclusive or of its arguments. Args must be integers.
  Known attributes: foldable, flushable, unsafely-flushable, movable, explicit-check
  Source file: SYS:SRC;CODE;NUMBERS.LISP
```

But note again that the arguments to foo are of type `T`. Basically,
this is the Lisp compiler saying it knows nothing about those
arguments, so it must be ready for anything. But, could you really
call it with anything?

```lisp
* (foo 'bogus "bogus")
debugger invoked on a SIMPLE-TYPE-ERROR: Argument X is not a INTEGER: BOGUS
⋮
```

Of course not. But it's nice to see Lisp catching the problem at
runtime and describing what's gone wrong, rather than just blindly
accepting a string or symbol or what-have-you as a numeric value and
attempting to compute its bitwise exclusive-OR.

So, if `logxor` can be defined with type information for its arguments,
can't we do the same with `foo`? Of course we can. All we need to do is
provide a declaration in the body of `foo` that says just what sort of
types we intend to use with it.

```lisp
* (defun foo (x y)
    (declare (type integer x y))
    (logxor x y))
```    

Now, when we quiz the Lisp engine about `foo`, we see a different
definition regarding its types.

```
* (describe 'foo)
⋮
Derived type: (FUNCTION (INTEGER INTEGER) (VALUES INTEGER &OPTIONAL))
```

That's a bit more sensible. `foo` is now a function that takes two
integers, and returns an integer.

But, still, there's a lot of generality lurking in there. Remember,
integers can be any size. Hundreds and thousands of bits in length, in
fact. We're still looking nothing like that C function `foo()`, that's
for sure. Here's what `foo` looks like in memory:

```
* (disassemble 'foo)
; disassembly for FOO
; 02B8BC8F:     4883EC18         SUB RSP, 24  ; no-arg-parsing entry point
;       93:     48896C2408       MOV [RSP+8], RBP
;       98:     488D6C2408       LEA RBP, [RSP+8]
;       9D:     B904000000       MOV ECX, 4
;       A2:     488B0425980B1020 MOV RAX, [#x20100B98]
;       AA:     FFD0             CALL RAX
;       AC:     488BE5           MOV RSP, RBP
;       AF:     F8               CLC
;       B0:     5D               POP RBP
;       B1:     C3               RET
```

The call through the `AX` register is another function that does the
actual exclusive-OR, and that function is still worrying about what
kind of integers we've been passed. Ugh. That's still going to hurt
our performance.

What if we knew what kind of integers we were going to need? For
example, in the C version of `foo()` we knew that we only wanted
register sized integers, and so we defined the function that way. We
could do the same here. Instead of saying that foo can accept any
integer, let's say that it only takes 32 bit unsigned integers, just
like the C code.

```
* (defun foo (x y)
    (declare (type (unsigned-byte 32) x y))
    (logxor x y))
FOO

* (describe 'foo)
⋮
Derived type: (FUNCTION ((UNSIGNED-BYTE 32) (UNSIGNED-BYTE 32))
               (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
```

_Note:_ Don't let the reference to `byte` fool you. It's mostly
historical, dating from a time when “byte” meant the smallest thing
addressable in memory. These days, we say “byte” to mean an eight bit
value, which is usually the same thing (to be precise, when we say
“byte” we often really mean “octet”). In Lisp, however, any range of
bits within an integer can be directly addressed easily enough, and so
a byte really is an arbitrary number of bits in length.

So, the function signature above looks encouraging. We've made a
promise to the compiler that we're going to call foo only with values
that are a 32 bit unsigned integer, and in return, we're going to get
the same back from `foo`. Let's look at the code generated by the
compiler.

```
* (disassemble 'foo)
; disassembly for FOO
; 02C9EB0F:     4831FA           XOR RDX, RDI ; no-arg-parsing entry point
;       12:     488BE5           MOV RSP, RBP
;       15:     F8               CLC
;       16:     5D               POP RBP
;       17:     C3               RET
```

_Yes!_ Now that's more like it. Sure, there's still a little bit of
noise in there with the stack and status flags. Various environments
and ABI standards pass arguments and return values on the stack or in
registers, so you might see more or less code in there depending on
your own system.



Type Inference
--------------

I waved my hands around and near this earlier, but here I'd like to
focus on type inference for a moment. Common Lisp was the first system
I used that could strongly reason about types, and when I finally
understood what was going on behind the curtain, it was one of the
first genuinely impressive “a ha” moments I had with Lisp.

Examples are always preferable, of course, but by way of introduction,
let me explain that most Common Lisp implementations are quite handy
at analyzing entire expressions (not just nested functions, but even
loops and other control structures) and determining what types are
implied by the expression itself. In that way, Common Lisp can do all
sorts of nice things for you, including working out return types on
its own. Notice in the fully typed implementation of foo that we still
never told it what its return type was. Those sort of declarations are
a peculiarity of the Algol family of languages.

So, imagine a new function, bar, that performs a simple addition on
its arguments. Just to make life interesting, we will add two values,
`x|1` and `y|2`; that is, we will set bits 0 and 1 in x and y
(respectively), and add the results.

```lisp
(defun bar (x y)
  (declare (type (integer 0 255) x y))
  (+ (logior x 1) (logior y 2)))
```

It's worth noting that `(integer 0 255)` is exactly the same as
`(unsigned-byte 8)`. The Lisp implementation I'm using, SBCL, has an
excellent understanding of numeric types, and an eight bit byte is the
same as an integer whose values are limited from 0 through 255. This
sounds trivial at first, but hold on, things are about to get
interesting.

Let's get a description of `bar` now.

```lisp
(describe 'bar)
⋮
  Derived type: (FUNCTION ((UNSIGNED-BYTE 8) (UNSIGNED-BYTE 8))
               (VALUES (INTEGER 3 510) &OPTIONAL))
```

Check out that deduced return type: an integer [3, 510]. We've
promised Lisp that we will only pass in integers [0, 255] in value,
yet Lisp correctly determined both the minimum and maximum values that
will result from calling bar. Further, this information is now
available to any future type analysis calling `bar`, so that all
functions compiled downstream get the same benefits.

Just to drive the point home, let's try multiplication. To keep the
values sane, we'll also restrict our incoming values to just five bit
integers [0,31]. Anyone who has coded double word arithmetic in the
past will appreciate the attention to detail here.

```lisp
* (defun baz (x y)
    (declare (type (unsigned-byte 5) x y))
      (* x y))
BAZ

* (describe 'baz)
⋮
  Derived type: (FUNCTION ((UNSIGNED-BYTE 5) (UNSIGNED-BYTE 5))
                 (VALUES (MOD 962) &OPTIONAL))
```

One might guess correctly that `(MOD 962)` as a type simply means an
integer [0,961]. And 961 is the value you would get from multplying
31, the largest unsigned 5 bit value, by itself. This kind of type
analysis and inference can really come in handy when you're trying to
pack arrays and other data into memory, or when serializing for
transmission or storage. Having the Lisp environment analyze your code
and determine all the intermediate and resulting types required to
support your algorithms is like having an assistant to handle all that
drudgery and book keeping for you… and isn't that what the computer
should do best in the first place?

It's also worth noting that this type information does not bloat the
compiled functions themselves. Despite recognizing “five bit bytes”
and other esoterica you might find in your algorithms, the resulting
CPU instructions are as careful or as optimized for speed as well as
any other language compiler might generate.



Correctness vs. Performance
---------------------------

There's a quick trip down Lisp typing lane, paying attention to
performance and not much else. I've glossed over a lot of stuff here,
to be sure. All potential subjects for a future post.

The main point to remember is this: **Lisp has historically preferred
correctness over performance.** That heritage still shines through
today. Common Lisp sometimes feels slow because it's bending over
backwards to “do the right thing” when it comes to your code.

Even better, Lisp is perfectly happy figuring out return value types
based on the arguments you want to provide. Even if you're providing
types on your arguments, you almost never need to say what a return
type is, Lisp propagates types from arguments throughout your function
and deduces return types all on its own.

It comes down to this: the more you that you can tell the compiler,
the more Lisp can do with your code. C has great performance, in part
because it requires that you to tell it everything about a
function. Lisp tries harder and works with what you've told it, even
when you've left out key parts of a definition. Remember, we started
with a function definition that contained no type information at all,
and so Lisp gave us not just an implementation that worked, but one
that worked with a surprising range of integers without requiring any
special input from us.

Lisp can be pretty cool that way.



Thanks
------

Many thanks to [Pat][] and [Brad][] for reviewing drafts of
this. Subsequent feedback from [lispm][] and [xach][] received on
[reddit](https://reddit.com/) is gratefully acknowledged.



  [Pat]:   https://nklein.com/                        "Pat Stein's home"
  [Brad]:  https://plus.google.com/+BradfordWerner432 "Brad Werner at G+"
  [lispm]: https://www.reddit.com/u/lispm             "lispm at reddit"
  [xach]:  https://www.reddit.com/u/xach              "Zach Beane at reddit"
