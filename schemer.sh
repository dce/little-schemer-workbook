#!/bin/sh

if ! type racket > /dev/null 2>&1; then
  echo "brew install racket, nerd"
  exit
fi

if ! type rlwrap > /dev/null 2>&1; then
  echo "brew install rlwrap, nerd"
  exit
fi

rlwrap racket --load toc.rkt --repl
