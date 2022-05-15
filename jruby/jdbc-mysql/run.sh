#!/bin/bash

print_this_dir() {
  (
    cd "$(dirname "$0")"
    pwd
  )
}

readonly __DIR__="$(print_this_dir)"
readonly JRUBY_CMD=${__DIR__}/jruby-9.3.4.0/bin/jruby

## 環境変数 CLASSPATH で指定しないと動かない場合がある？
# export CLASSPATH="${__DIR__}/mysql-connector-java-8.0.29.jar"

$JRUBY_CMD mysql_sample.rb "$@"

## -J-cp で指定してもよい？
# $JRUBY_CMD \
#   -J-cp "${__DIR__}/mysql-connector-java-8.0.29.jar" \
#   mysql_sample.rb "$@"
