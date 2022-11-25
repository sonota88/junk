#!/bin/bash

## .bashrc
# wip-commit() {
#   bash /path/to/this-script.sh
# }

## .tigrc
# bind status C !/path/to/this-script.sh

set -o errexit

changed_files() {
  # egrep -v '^ '
  #   ステージング領域にあるものだけに限定
  # awk '{ print $NF }'
  #   最後のフィールドを抽出
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

to_basename() {
  while IFS= read -r line; do
    basename $line
  done
}

changed_basenames() {
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
    | to_basename \
    | awk '{ printf "%s, ", $0 }' \
    | awk '{ sub(/, $/, ""); print }'
}

# --------------------------------

tmpfile=/tmp/my_git_commit_template.txt

{
  cat <<..MSG
__wip
 $(date "+%F %T")
 ($(changed_files))
 ($(changed_basenames))
..MSG

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
