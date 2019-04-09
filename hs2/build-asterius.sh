#!/bin/bash

set -eu

mkdir -p build/

sudo docker run -it \
  -v `pwd`:/mirror \
  -v `pwd`/build:/root/.cabal \
  terrorjack/asterius:latest \
  bash -c 'cd /mirror/ && ahc-cabal new-build --project-file=ahc-cabal.project'
  #bash -c 'ahc-cabal new-update && cd /mirror/ && ahc-cabal new-build --project-file=ahc-cabal.project'
  #> ahc-cabal-new-build.log 2>&1
