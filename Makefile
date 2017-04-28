LOCAL_DEPS = $(wildcard local-deps/*)

compile: app component-deps
	@echo
	@echo "  Congratulations - you've compiled Guild AI!"
	@echo
	@echo "  Run tests by running 'make test'"
	@echo
	@echo "  Run 'scripts/guild-dev' directly or create a symbolic link"
	@echo "  to it somewhere on your path."
	@echo

app:
	./rebar3 compile

component-deps: priv/components/.deps-resolved

priv/components/.deps-resolved:
	bower install
	scripts/patch-components
	touch priv/components/.deps-resolved

sync-tf-components:
	scripts/sync-tf-components
	scripts/patch-components

lint-components: polylint
	polylint --root priv/components --input `(cd priv/components && find -name guild-*.html)`

polylint:
	@if ! which polylint >/dev/null; then \
	  echo "polylint is not installed - use 'npm install -g polylint' to install"; \
	  exit 1; \
	fi

vulcanize-view-index:
	cd priv && vulcanize \
	  --inline-scripts \
	  --inline-css \
	  --strip-comments \
	  view-index.html | gzip > view-index-all.html.gz

clean: clean-local-deps clean-bin-deps clean-components clean-vulcanized
	rm -rf build; rm -f rebar.lock
	rm -f compile_commands.json

clean-vulcanized:
	cd priv && rm -f *.html.gz

clean-local-deps: $(LOCAL_DEPS:=.clean)

local-deps/%.clean:
	make -C local-deps/$* clean

clean-bin-deps:
	rm -f priv/bin/multimarkdown

clean-components:
	rm -f priv/components/.deps-resolved
	cd priv/components && ls \
	  | grep -v '^guild-' \
	  | grep -v '^tf-' \
	  | grep -v '^vz-' \
	  | xargs rm -rf

upgrade:
	./rebar3 upgrade
	./rebar3 compile

test: app
	test/internal $(TESTS)
	priv/bin/tensorflow-port test

test-operations: compile
	test/operations

test-app: test test-operations

shell:
	ERL_LIBS=local-deps:build/default/lib erl -s guild

shell-reload:
	ERL_LIBS=local-deps:build/default/lib erl -s e2_reloader -s guild

version:
	@if [ -z "$(VERSION)" ]; then \
	  echo "VERSION must be defined"; \
	  exit 1; \
	fi

# Ordering dependencies so invoking targets explicitly
release: version
	make vulcanize-view-index
	make compile
	scripts/mkrel $(VERSION)
