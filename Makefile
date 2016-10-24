NAME := krokr
PREFIX = $(shell opam config var prefix)

test: build
	ocaml pkg/pkg.ml test

build:
	ocaml pkg/pkg.ml build --tests true

$(NAME).install:
	$(MAKE) build

clean:
	ocamlbuild -clean
	rm -f $(NAME).install

install: $(NAME).install
	opam-installer -i --prefix $(PREFIX) $(NAME).install

uninstall: $(NAME).install
	opam-installer -u --prefix $(PREFIX) $(NAME).install

reinstall: $(NAME).install
	opam-installer -u --prefix $(PREFIX) $(NAME).install &> /dev/null || true
	opam-installer -i --prefix $(PREFIX) $(NAME).install


VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
# opam publish prepare $(NAME_VERSION) $(ARCHIVE)
# opam publish submit $(NAME_VERSION)
# rm -rf $(NAME_VERSION)

.PHONY: test
