config-preprocessor: config_preprocessor.ml
	ocamlfind ocamlopt -o config-preprocessor -linkpkg \
	-package str \
	-package unix \
	config_preprocessor.ml
