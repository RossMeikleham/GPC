[![Build Status](https://travis-ci.org/RossMeikleham/GPC.svg?branch=master)](https://travis-ci.org/RossMeikleham/GPC)

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

Installing
==========
`cabal install`

Running
=======
`Gannet-Parallel-C source-file.gpc`
If compilation is successful this should generate a GPIR file of the same name as the source but with a .td extension, this file will be in the same folder as the source file.


Running Tests
=============
`cabal test`
