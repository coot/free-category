# Free Category
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![CircleCI](https://circleci.com/gh/coot/free-category/tree/master.svg?style=svg)](https://circleci.com/gh/coot/free-category/tree/master)

This package contains efficient free category using realtime
queues (C. Okasaki 'Pure Functional Data Structures') and another one using CPS
style.  Also free arrows are included and free Kleisli categories.

Free categories are useful to model state machines in a simple yet type safe
way and for that purpose `Kleisli` categroies are a very useful target which
allows to include monadic computations.  This packge contains a useful
generalisation of `Kliesli` categories captured by `EffCategory` class
(effectful categories), and a (free) transformer which lifts a category to
an effectful one.

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
