#!/bin/bash

tmpfile=/tmp/my_git_commit_template.txt

{
  echo "__wip $(date "+%F %T")"
  printf "\n"
} > $tmpfile

{
  git log --graph --oneline --decorate --all \
    | head -n 20

  # echo "----"
  # cat /path/to/static_template.txt

} | awk '{ print "; " $0 }' \
  >> $tmpfile

git commit --verbose --template=$tmpfile
