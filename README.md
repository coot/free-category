# Free Category
[![Maintainer: coot](https://img.shields.io/badge/maintainer-coot-lightgrey.svg)](http://github.com/coot)
[![Travis Build Status](https://travis-ci.org/coot/free-category.svg?branch=master)](https://travis-ci.org/coot/free-category)

This package introduces variouos encodings of free categories in Haskell.

Free categories are useful to model state machines in a simple yet type safe
way and for that purpose `Kleisli` categroies are a very useful target which
allows to include monadic computations.  Read more
[here](https://coot.me/posts/finite-state-machines.html) on a simple example of
a finite state machine encoded using a free category using a simple GADT.
Another simple
[example](https://github.com/coot/free-algebras/blob/master/examples/src/Control/Category/Free.hs).
