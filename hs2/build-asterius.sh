#!/bin/bash

set -eu

mkdir -p build/ out/

sudo docker run -it \
  -v `pwd`:/mirror \
  -v `pwd`/build:/root/.cabal \
  terrorjack/asterius:latest \
  bash -c 'cd /mirror/ && ahc-cabal new-build --project-file=ahc-cabal.project && ahc-dist --browser --input-exe dist-newstyle/build/x86_64-linux/*/igrep-cashbook-0.1.0.0/x/sum/build/sum/sum --output-directory out --input-mjs out/sum.mjs'
sudo chown -R "$USER" out
  #bash -c 'ahc-cabal new-update && cd /mirror/ && ahc-cabal new-build --project-file=ahc-cabal.project'
  #> ahc-cabal-new-build.log 2>&1
