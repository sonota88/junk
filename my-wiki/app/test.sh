#!/bin/bash

set -o errexit

ruby -w test/test_lib.rb
ruby -w test/test_lib_wiki.rb
