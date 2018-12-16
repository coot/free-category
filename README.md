# Free Category
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![CircleCI](https://circleci.com/gh/coot/free-category/tree/master.svg?style=svg)](https://circleci.com/gh/coot/free-category/tree/master)

This package introduces variouos encodings of free categories in Haskell.

Free categories are useful to model state machines in a simple yet type safe
way and for that purpose `Kleisli` categroies are a very useful target which
allows to include monadic computations.  Read more
[here](https://coot.me/posts/finite-state-machines.html) on a simple example of
a finite state machine encoded using a free category using a simple GADT.
Another simple
[example](https://github.com/coot/free-algebras/blob/master/examples/src/Control/Category/Free.hs).

## Some examples

* [LoginStateMachine](https://github.com/coot/free-category/blob/master/examples/src/LoginStateMachine.hs):
  based on [State Machines All The Way
  Down](https://www.youtube.com/watch?v=xq7ZuSRgCR4) by Edwin Bradly, 2017 You
  can run it with `cabal new-run examples:login-state-machine`.
