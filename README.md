[![Build Status](https://travis-ci.org/RossMeikleham/GPC.svg?branch=master)](https://travis-ci.org/RossMeikleham/GPC)

About
===

GPC is a front end language for the Glasgow Parallel Reduction Machine(GPRM) framework and is also my honours year project as a Student at the University of Glasgow. The compiler is written in pure Haskell.

GPC statements are evaluated in parallel by default.

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
`gpcc source-file.gpc`
If compilation is successful this should generate a GPIR file of the same name as the source but with a .td extension, this file will be in the same folder as the source file.

See the examples folder for some programs which can currently be compiled

Running Tests
=============
`cabal test`
