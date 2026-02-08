
# makefile commands

build: _build/install/default/bin/dusk

test: FORCE
	dune runtest

run: main.exe

help: _build/install/default/bin/dusk
	_build/install/default/bin/dusk -help

# build targets

main.exe:
	dune exec -- dusk "workspace/game" -w -r _runtime

_build/install/default/bin/dusk: FORCE
	dune build

FORCE: ;