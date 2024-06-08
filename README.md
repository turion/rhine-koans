# Learn rhine with koans

This repository is a playground to learn the Functional Reactive Programming (FRP) framework [`rhine`](https://hackage.haskell.org/package/rhine).

By solving many small, self-contained exercises,
you can learn about all the fundamentals, and some advanced topic of `rhine`,
a Haskell library for reactive programming with type level clocks.

## How do I do this?

### Prerequisites

You should have Haskell and cabal installed.
Stack is not required.
Haskell Language Server is highly recommended.
Have a look at https://www.haskell.org/downloads/ for installation instructions.

### Get started

Clone this repository and enter it on a console.
Then run this command:

```
cabal test basic-1-1-hello-rhine-test
```

It will fail! And it is your task now to fix it.
The error message tells you which file to edit.
By reading through the file, and filling in the missing part,
you will learn a bit about `rhine`.

### How to go on

If the previous test now passes, you're ready to tackle the next task!

Run this command:

```
cabal test basic-1-2-fix-the-bug-test
```

Again it will fail, and again it is your job to fix it and learn something in the process.

This way, you can go on step by step, increasing the track, chapter number and koan number (see below).

There are many small programs for you to edit and fix,
each of which will teach you something new.
All programs are organised by _tracks_,
which in turn are subdivided into a few chapters.
Tracks have names (like `basic`), chapters and koans have numbers:
```
#            /--- The track
#            |
#            |   /--- The chapter
#            |   |
#            |   | /--- The koan number
#            |   | |
#            v   v v
cabal test basic-1-2-fix-the-bug-test
```

### What tracks are there?

* `basic`: Write some simple Rhine programs. Start here.
  Learn about signal functions, clocks, and resampling buffers.
* `ui`: Create a clone of the classic Snake game by using `rhine-gloss`.

### Stuck?

#### Want to figure out what your program is doing?
Instead of running the test straight ahead,
you can also run your program you've been writing directly, for example:
```
cabal run basic-1-2-fix-the-bug
```

#### Can't find the solution?
Have a look at the file `koans/your-track-here/your-chapter-number/your-koan-number-koan-title/solution/Koan.hs`.
For example, if you can't get `cabal test basic-1-2-fix-the-bug-test` to work,
you'd be looking for the file `koans/basic/1/2-fix-the-bug/solution/Koan.hs`.
It contains the solution to this task.
You can also run the solution by adding the `solution` cabal flag:
```
cabal run basic-1-2-fix-the-bug -fsolution
```
You can even test the solution:
```
cabal test basic-1-2-fix-the-bug-test -fsolution
```

#### Maybe something is wrong in the `rhine-koans` repo?
We're glad to hear from you in this case!
Feel free to file an issue here:
https://github.com/turion/rhine-koans/issues/new

## I'd rather read about `rhine` first

You're heartily invited! Have a look at https://github.com/turion/rhine?tab=readme-ov-file#learn-rhine for all the resources available.

## I'm attending/I was attending Zurihac '24

That's great! The slides for [the workshop](https://zfoh.ch/zurihac2024/#track-frp) are here:

https://github.com/turion/rhine-koans/blob/main/presentation/presentation.pdf
