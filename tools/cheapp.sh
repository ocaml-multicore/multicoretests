#!/bin/sh

ocamlformat --impl --enable-outside-detected-project --no-version-check --type-decl=sparse - | gawk -f "$(dirname "$0")/cheapp.awk" | sort | cut -c 13- | ocamlformat --impl --enable-outside-detected-project --no-version-check - | sed 's/^./  &/'
