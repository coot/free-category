Example State Machine using 'EffCat'

The example state machine is inspired by
`State Machines All The Way Down` by Edwin Bradly, 2017
https://www.youtube.com/watch?v=xq7ZuSRgCR4

It allows to login, access a secret token and logout. 
The secret token can only be accessed upon successful login.  The
implementation type checks this requirement.

Run quickcheck tests and an example program:
```
cabal run examples
+++ OK, passed 100 tests.

Provide a password:
123456
Provide a password:
password
secret: Hello saylor!
```
