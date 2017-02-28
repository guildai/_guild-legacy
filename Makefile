LOCAL_DEPS = $(wildcard local-deps/*)

compile: bin-deps
	./rebar3 compile

bin-deps: priv/bin/multimarkdown

mmd_repo = https://github.com/fletcher/MultiMarkdown-4.git

priv/bin/multimarkdown:
	mkdir -p build/default/lib
	git clone $(mmd_repo) build/default/lib/mmd
	cd build/default/lib/mmd && git submodule init && git submodule update && make
	cp build/default/lib/mmd/multimarkdown priv/bin/multimarkdown

component-deps:
	bower install

lint-components:
	polylint --root priv/components --input `(cd priv/components && find -name guild-*.html)`

clean: clean-local-deps clean-bin-deps clean-components
	rm -rf build; rm -f rebar.lock
	rm -f compile_commands.json

clean-local-deps: $(LOCAL_DEPS:=.clean)

local-deps/%.clean:
	make -C local-deps/$* clean

clean-bin-deps:
	rm -f priv/bin/multimarkdown

clean-components:
	cd priv/components && ls | grep -v guild- | xargs rm -rf

upgrade:
	./rebar3 upgrade
	./rebar3 compile

test: compile
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

release: version compile
	scripts/mkrel $(VERSION)
