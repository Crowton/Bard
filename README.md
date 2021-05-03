# AU Compilation student project template

## Installation

1. Make sure `opam` is installed.
2. Install all dependencies by running `make deps`
3. Compile the template by running `make`
4. Run the tests by running `make runtest`

## Using custom utop

Run `make utop` to start a custom ocaml utop that loads the libraries defined in this directory. Note that we pass a custom .ocamlinit file that additionally open the Tigerc_lib module -- this makes the various custom modules, e.g., Symbol, immediately accussible through the utop.


## Notes on usage

We use opam and dune package managers. See their respective home pages for more information. The Makefile also contains an option for reporting missing dependencies and clearing the source tree.
# Bard
