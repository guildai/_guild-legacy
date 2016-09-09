LOCAL_DEPS = $(wildcard local-deps/*)
MAN_SOURCES = $(wildcard man/*.ronn)
MAN_FILES = $(MAN_SOURCES:.ronn=)
MAN_ORG = "TensorHub 0.0.0"

compile:
	./rebar3 compile

clean: clean-local-deps
	rm -rf build; rm -f rebar.lock

clean-local-deps: $(LOCAL_DEPS:=.clean)

local-deps/%.clean:
	make -C local-deps/$* clean

upgrade:
	./rebar3 upgrade
	./rebar3 compile

script:
	./rebar3 escriptize

test: compile
	scripts/test $(TESTS)

shell:
	ERL_LIBS=local-deps:build/default/lib erl -s e2_reloader -s tensorhub_client

man: $(MAN_FILES)

man/%: man/%.ronn
	ronn --organization $(MAN_ORG) $<
