This repo is a work in progress to organize and refine the scattered
code I developed for computing the electoral apportionment functions
described in my 2016 paper _Better apportionment through vacancies_,
available online:

> HTML: https://galen.xyz/apportion/
>
> PDF: https://galen.xyz/apportion.pdf

It is written in Haskell, and is an ongoing project.

The demo can be build with `stack` (e.g., `stack run`).  It can
also be built with `cabal`, although that requires installing
[`hpre`](https://github.com/galenhuntington/hpre).

Note that due to the least error being shown with precision necessary
to distinguish all values, printing stream values close to 0 can
require considerable computation and, more importantly, memory.
Computing the `abc` demo's least error values past 18,000 requires
tens of gigabytes.  Running with `+RTS -c` helps somewhat.  Since such
values are only of theoretical interest, I have not done much to
optimize for them.

