#!/usr/bin/env bash
# Run `actor` and `actor-eval` for every program listed by `actor-list`.
set -euo pipefail

cd "$(dirname "$0")"

run() { cabal run -v0 concurrent-letrec -- "$@"; }

run actor-list | while read -r prg; do
  [ -n "$prg" ] || continue
  echo "=== actor $prg ==="
  run actor "$prg"
  echo "=== actor-eval $prg ==="
  run actor-eval "$prg"
done
