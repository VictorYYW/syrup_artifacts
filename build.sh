#!/bin/sh
OCAML_VERSION=4.10.0+flambda
if ! command -v opam &> /dev/null
then
    echo "OPAM could not be found. Please visit https://opam.ocaml.org/doc/Install.html"
    exit 1
fi
opam switch create $OCAML_VERSION
eval $(opam env)
opam install --yes dune qcheck z3 ppx_inline_test core core_unix iter bark menhir.20211128 ppx_deriving smart-print async_unix js_of_ocaml-ppx
(cd burst && make)
(cd smyth && dune build src/main.exe)
(cd syrup && dune build)
