#!/bin/bash
ghc --make -O2 -threaded -prof -fprof-auto -fprof-auto-calls -rtsopts Feeders.hs && ./Feeders +RTS -p
# ghc --make -O2 -threaded Feeders.hs && ./Feeders +RTS -N4 -RTS
