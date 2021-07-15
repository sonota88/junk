#!/bin/bash

set -o errexit
set -o nounset

for test_file in $(ls test/test_*.rb); do
  echo "----"
  echo $test_file
  ruby -w $test_file
done

echo ""
echo "OK"
