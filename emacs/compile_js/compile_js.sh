#!/bin/bash

set -o pipefail

__dir="$(cd $(dirname "$0") ; pwd)"
echo $__dir

export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(anyenv init -)"

(
  cd $__dir
  npm run build 2>&1 | col -bx
)
