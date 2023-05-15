make clean
cloc --by-file --include-lang=OCaml --exclude-dir=_build .
git ls-files | awk '/\.ml/ {print}' | while read f; do git blame -w -M -C -C --line-porcelain $f | grep '^author '; done | sort -f | uniq -ic | sort -n
make build
