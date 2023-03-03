from ocaml/opam:debian-ocaml-4.10-flambda
run sudo apt install -y python3 libgmp-dev
run opam install --yes dune qcheck z3 ppx_inline_test core core_unix iter bark menhir.20211128 ppx_deriving smart-print async_unix
run opam install --yes js_of_ocaml-ppx
copy --chown=opam:opam . .
env PATH=/home/opam/.opam/4.10/bin:$PATH
run cd ~/burst && dune build app/BurstCmdLine.exe --profile release
run cd ~/smyth && dune build src/main.exe
run cd ~/syrup && dune build


