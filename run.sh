#!/bin/bash
dune exec bin/main.exe -- \
  --year=2024 \
  --day="$1" \
  --part="$2"
