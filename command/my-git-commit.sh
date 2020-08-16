#!/bin/bash

set -o errexit

changed_files() {
  local all_files="$(
    git status --porcelain --untracked-file=no \
      | egrep -v '^ ' \
      | awk '{ print $NF }'
  )"

  local size=$(echo "$all_files" | wc -l)

  local size_limit=4

  if [ $size -gt $size_limit ]; then
    files="$(
      echo "$all_files" | head -${size_limit}
      printf "..."
    )"
  else
    files="$all_files"
  fi

  echo "$files" \
    | awk '{ printf "%s, ", $0 }' \
    | awk '{ sub(/, $/, ""); print }'
}

# --------------------------------

tmpfile=/tmp/my_git_commit_template.txt

{
  echo "__wip $(date "+%F %T") ($(changed_files))"
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
