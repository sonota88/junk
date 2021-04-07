date_seq() {
  local from="$1"; shift
  local to="$1"; shift

  local dt=$from
  local i=0

  while [ "$dt" != "$to" ]; do
    dt=$(date --date "${from} +${i}days" "+%Y%m%d")
    echo $dt
    i=$((i + 1))

    if [ $i -ge 100 ]; then
      echo "too many days" >&2
      exit 1
    fi
  done
}

for dt in $(date_seq "$1" "$2"); do
  echo $dt
done
