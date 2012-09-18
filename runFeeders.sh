#!/bin/bash
ghc --make -O2 -threaded -prof -fprof-auto -fprof-auto-calls Feeders.hs && ./Feeders 