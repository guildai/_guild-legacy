LOCAL_DEPS = $(wildcard local-deps/*)

compile:
	./rebar3 compile

clean: clean-local-deps
	rm -rf build; rm -f rebar.lock
	rm -f compile_commands.json

clean-local-deps: $(LOCAL_DEPS:=.clean)

local-deps/%.clean:
	make -C local-deps/$* clean

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

.PHONY : docs
docs:
	cd docs && make

serve-docs:
	cd docs && make serve
