# Free Category
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![Haskell/CI](https://github.com/coot/free-category/workflows/Haskell/CI/badge.svg)](https://github.com/coot/free-category/actions)

This package contains efficient implementations of free categories. There are
various representations available:

* real-time queues (C. Okasaki 'Pure Functional Data Structures')
* type aligned lists
* continuation passing style (Church encoding)

Free arrows and free Kleisli categories are also included.

Free categories are useful to model state machines in a simple yet type safe
manner.  For that purpose `Kleisli` categories are a very useful target which
allows to include monadic computations.  This package contains a useful
generalisation of `Kleisli` categories captured by `EffectCategory` class
(categories with effects), and a (free) transformer which lifts a category to
a category with effects.

## Benchmarks

Check performance characteristics of various representations:

* [report-O0](/bench/report-O0.md)
* [report-O1](/bench/report-O1.md)
* [report-O2](/bench/report-O2.md)

## Resources
* [LoginStateMachine](https://github.com/coot/free-category/blob/master/examples/src/LoginStateMachine.hs):
  based on [State Machines All The Way
  Down](https://www.youtube.com/watch?v=xq7ZuSRgCR4) by Edwin Bradly, 2017.
  You can run it with `cabal new-run examples:login-state-machine`.
* Read more [here](https://coot.me/posts/finite-state-machines.html) on
  a simple example of a finite state machine encoded using a free category
  using a simple GADT.
* Another
  [example](https://github.com/coot/free-algebras/blob/master/examples/src/Control/Category/Free.hs).
* [Blog post](https://coot.me/posts/kleisli-categories-and-free-monads.html) on Kleisli categories.
