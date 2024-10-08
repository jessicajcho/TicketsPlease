#!/bin/bash

#Bonnie: Edits made to this file include
# - removing checks for zip files
# - removing checks for hours worked 
# - removing MD5 checks 
# - removing a2 specific checks such as function signatures

OPAMV=2.*
OCAMLV=4.12.*
OUNITV=2.*

BLUE="\033[0;34m"
RED="\033[0;31m"
NOCOLOR="\033[0m"

print_meta () {
  printf "\n🐫 $BLUE<><> $1 <><>$NOCOLOR\n\n"
}

print_info () {
  printf "$1\n"
}

print_err () {
  printf "$RED$1$NOCOLOR\n"
}

print_fatal () {
  print_err "\n$1\n"
}

print_meta "Checking your OCaml environment"

environment=good

OPAM_LOCATION="$(command -v opam)"
if [[ $OPAM_LOCATION == "" ]]; then
  print_err "OPAM is NOT available.  This is bad."
  environment=bad
else
  print_info "OPAM is available.  Good."
fi

OPAM_VERSION="$(opam --version)"
if [[ $OPAM_VERSION =~ $OPAMV ]]; then
  print_info "OPAM version $OPAM_VERSION is installed.  Good."
else
  print_err "OPAM version $OPAMV is NOT installed.  This is bad."
  print_err "The installed version is: $OPAM_VERSION"
  environment=bad
fi

OCAMLC_VERSION="$(ocamlc --version 2>&1)"
if [[ $OCAMLC_VERSION =~ $OCAMLV ]]; then
  print_info "OCaml compiler version $OCAMLC_VERSION is active.  Good."
else
  print_err "OCaml compiler version $OCAMLV is NOT active.  This is bad."
  print_err "The active version is: $OCAMLC_VERSION"
  environment=bad
fi

OUNIT_VERSION="$(opam info ounit -f version 2>&1)"
if [[ $OUNIT_VERSION =~ $OUNITV ]]; then
  print_info "OUnit version $OUNIT_VERSION is active.  Good."
else
  print_err "OUnit version $OUNITV is NOT active.  This is bad."
  print_err "The active version of OUnit is: $OUNIT_VERSION"
  environment=bad
fi

if [[ ! $environment == good ]]; then
  print_fatal "Your OCaml environment is broken."
  exit 1
fi

print_meta "Checking whether your code compiles"

dune build src test
if [[ $? -ne 0 ]]; then
  print_fatal "Your code does not compile."
  exit 1
fi


print_meta "Congratulations! You have passed [make check]."

if [[ $1 != "final" ]]; then
  exit 0
fi

print_meta "Congratulations! You have passed [make finalcheck]."