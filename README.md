# Free Category
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![CircleCI](https://circleci.com/gh/coot/free-category/tree/master.svg?style=svg)](https://circleci.com/gh/coot/free-category/tree/master)

This package contains efficient free categories. There are two presentations:

* using realtime queues (C. Okasaki 'Pure Functional Data Structures')
* using continuation passing style

Free arrows and free Kleisli categories are also included.

Free categories are useful to model state machines in a simple yet type safe
manner.  For that purpose `Kleisli` categroies are a very useful target which
allows to include monadic computations.  This packge contains a useful
generalisation of `Kliesli` categories captured by `EffCategory` class
(effectful categories), and a (free) transformer which lifts a category to
an effectful one.

## Benchmarks

Check performence characteristics of various presentations free categories:

* [raport-O0](./bench/raport-O0.md)
* [raport-O1](./bench/raport-O1.md)
* [raport-O2](./bench/raport-O2.md)

## Some examples
* [LoginStateMachine](https://github.com/coot/free-category/blob/master/examples/src/LoginStateMachine.hs):
  based on [State Machines All The Way
  Down](https://www.youtube.com/watch?v=xq7ZuSRgCR4) by Edwin Bradly, 2017 You
  can run it with `cabal new-run examples:login-state-machine`.
* Read more [here](https://coot.me/posts/finite-state-machines.html) on
  a simple example of a finite state machine encoded using a free category
  using a simple GADT.
* Another
  [example](https://github.com/coot/free-algebras/blob/master/examples/src/Control/Category/Free.hs).
* [Blog post](https://coot.me/posts/kleisli-categories-and-free-monads.html) on Keleisli categories.
