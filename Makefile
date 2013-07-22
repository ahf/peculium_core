REPOSITORY = peculium_core
APPS = crypto kernel stdlib sasl inets syntax_tools mnesia eunit release

all: compile

test: eunit

compile:
	rebar compile skip_deps=true

get-deps:
	rebar get-deps

build-deps:
	rebar compile

clean:
	rebar clean

release: compile
	rebar -v skip_deps=true generate

console: release
	./rel/peculium_core/bin/peculium_core console

eunit:
	rebar skip_deps=true eunit

quickcheck:
	rebar skip_deps=true qc

qc: quickcheck

doc:
	rebar skip_deps=true doc

PLT = $(HOME)/.$(REPOSITORY)_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(PLT) --apps $(APPS) deps/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS) deps/*/ebin

dialyzer:
	dialyzer --fullpath --plt $(PLT) ebin/

clean_plt:
	rm $PLT

.PHONY: all get-deps build-deps compile clean eunit doc test dialyzer \
	check_plt build_plt clean_plt
