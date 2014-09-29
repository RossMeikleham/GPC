[![Build Status](https://travis-ci.org/RossMeikleham/Gannet-Parallel-C.svg?branch=master)](https://travis-ci.org/RossMeikleham/Gannet-Parallel-C)

About
===

GPC is a front end language to facilitate the communication between tasks in the Glasgow Parallel Reduction Machine (GPRM) framework and is also my honours year project as a Student at the University of Glasgow. The compiler is written in pure Haskell.

GPC statements are evaluated in parallel by default; the language aims to be functional (discouraging the mutation of state) and easy to program in for those who are familiar with C++/Java like syntax.

The GPRM is a research project at the University Of Glasgow, more information on it can be found [here]( http://arxiv.org/pdf/1312.2703v1.pdf)

Building
========
```
cabal configure --enable-tests
cabal build
```

Running Tests
=============
`cabal test`
