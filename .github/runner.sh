#!/bin/sh

set -e

OCAMLDIR=ocaml
DUNEDIR=dune
MULTICORETESTSDIR=multicoretests

fatal() {
  printf %s "$1"
  exit 1
}

compiler_sha() {
  # Expect $COMPILER_REPO and $COMPILER_REF to be set
  # Note: only GitHub-hosted compiler forks are supported for now
  git ls-remote "https://github.com/$COMPILER_REPO.git" "$COMPILER_REF" \
    | cut -f 1
}

setup() {
  if [ -n "$GITHUB_ENV" ] && [ -n "$GITHUB_PATH" ] ; then
    sha="$(compiler_sha)" || fatal "Cannot find compiler's SHA"
    arch="$(uname -m)"
    opts="$(printf %s "$OCAML_OPTIONS" | tr " " -)"
    opts="${opts:+-}$opts"
    echo "cache_key=ocaml-$sha-$OCAML_PLATFORM-$arch$opts" >> "$GITHUB_ENV"
    case "$OCAML_PLATFORM" in
      mingw|msvc|cygwin)
        PREFIX='D:\ocaml'
        bin='D:\ocaml\bin'
        ;;
      *)
        PREFIX="$HOME/local"
        bin="$HOME/local/bin"
        ;;
    esac
    printf "PREFIX=%s\n" "$PREFIX" >> "$GITHUB_ENV"
    printf "%s\n" "$bin" >> "$GITHUB_PATH"
    if [ -z "$JOBS" ] ; then
      if command -v nproc > /dev/null; then
        echo "JOBS=$(nproc)" >> "$GITHUB_ENV"
      elif command -v sysctl > /dev/null; then
        echo "JOBS=$(sysctl -n hw.ncpu)" >> "$GITHUB_ENV"
      fi
    fi

    echo Environment set up:
    cat "$GITHUB_ENV"
    echo PATH addition:
    cat "$GITHUB_PATH"
  fi

  case "$OCAML_PLATFORM,$OCAML_OPTIONS" in
    linux,*32bit*)
      sudo dpkg --add-architecture i386
      sudo apt-get update
      sudo apt-get install pkg-config:i386 libzstd1:i386 libzstd-dev:i386 \
        libgcc-s1:i386 gcc-multilib g++-multilib
      ;;
  esac
}

build_ocaml() {
  echo "${LOGBEGINGRP}Building OCaml"
  # We let standard OCaml CI test for warnings
  opts="--disable-warn-error \
        --disable-stdlib-manpages \
        --disable-ocamltest \
        --disable-ocamldoc"
  case "$OCAML_OPTIONS" in
    *fp*)
      opts="$opts --enable-frame-pointers"
      ;;
  esac
  case "$OCAML_OPTIONS" in
    *bytecode-only*)
      opts="$opts --disable-native-compiler"
      ;;
  esac
  case "$OCAML_PLATFORM" in
    msvc|mingw|cygwin)
      opts="$opts --prefix=/cygdrive/d/ocaml"
      ;;
    *)
      opts="$opts --prefix=$PREFIX"
      ;;
  esac

  cd "$OCAMLDIR"
  case "$OCAML_PLATFORM,$OCAML_OPTIONS" in
    msvc,*32bit*)
      eval $(tools/msvs-promote-path)
      printf 'Running: %s\n' "./configure --host=i686-pc-windows $opts"
      if ! ./configure --host=i686-pc-windows $opts ; then
        cat config.log
        exit 1
      fi
      ;;
    msvc,*)
      eval $(tools/msvs-promote-path)
      printf 'Running: %s\n' "./configure --host=x86_64-pc-windows $opts"
      if ! ./configure --host=x86_64-pc-windows $opts ; then
        cat config.log
        exit 1
      fi
      ;;
    mingw,*)
      printf 'Running: %s\n' "./configure --host=x86_64-w64-mingw32 $opts"
      if ! ./configure --host=x86_64-w64-mingw32 $opts ; then
        cat config.log
        exit 1
      fi
      ;;
    cygwin,*)
      case $COMPILER_REF in
        */5.1*)
          git -C flexdll fetch origin 0.43
          git -C flexdll checkout FETCH_HEAD
          ;;
      esac

      printf 'Running: %s\n' "./configure $opts"
      if ! ./configure $opts ; then
        cat config.log
        exit 1
      fi
      ;;
    linux,*32bit*)
      printf 'Running: %s\n' \
        "./configure --host=i386-linux \"CC=gcc -m32\" $opts"
      if ! ./configure --host=i386-linux "CC=gcc -m32" $opts ; then
        cat config.log
        exit 1
      fi
      ;;
    *) # linux, macos, default options
      printf 'Running: %s\n' "./configure $opts"
      if ! ./configure $opts ; then
        cat config.log
        exit 1
      fi
      ;;
  esac

  make -j"$JOBS"
  make install

  echo "$LOGENDGRP"
}

build_dune() {
  echo "${LOGBEGINGRP}Building dune"
  case "$OCAML_PLATFORM" in
    msvc)
      eval $("$OCAMLDIR"/tools/msvs-promote-path)
      ;;
  esac

  cd "$DUNEDIR"
  make release
  make install PREFIX="$PREFIX"
  echo "$LOGENDGRP"
}

show_config() {
  set -x
  ocamlc -config
  dune --version
}

build_testsuite() {
  case "$OCAML_PLATFORM" in
    msvc)
      eval $("$OCAMLDIR"/tools/msvs-promote-path)
      ;;
  esac

  cd "$MULTICORETESTSDIR"
  dune build
  dune build test/
}

case "$1" in
  setup)
    setup
    ;;
  ocaml)
    build_ocaml
    ;;
  dune)
    build_dune
    ;;
  show_config)
    show_config
    ;;
  build)
    build_testsuite
    ;;
  testsuite)
    cd "$MULTICORETESTSDIR"
    dune build @ci -j1 --no-buffer --display=quiet --cache=disabled --error-reporting=twice
    ;;
  internaltests)
    cd "$MULTICORETESTSDIR"
    dune build @internaltests -j1 --no-buffer --display=quiet --cache=disabled --error-reporting=twice
    ;;
  *)
    fatal "Unknown command '$1'"
    ;;
esac
