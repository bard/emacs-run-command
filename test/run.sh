#!/bin/bash

set -e

trap failure ALRM

failure() {
  printf "\tFAILURE\n"
}

for scenario in scenario-*.el; do
  echo running $scenario...
  screen -D -m sh -c "emacs -nw -Q -l director-bootstrap.el -l $scenario || kill -s ALRM $$"
done
