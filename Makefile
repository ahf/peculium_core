REPOSITORY = peculium
APPS = crypto kernel stdlib sasl inets syntax_tools mnesia eunit release

all: compile

test: eunit

compile:
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean

release: compile
	rebar -v skip_deps=true generate

console: release
	./rel/peculium/bin/peculium console

eunit:
	rebar skip_deps=true eunit

quickcheck:
	rebar skip_deps=true qc

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

.PHONY: all get-deps compile clean eunit doc test \
	    dialyzer check_plt build_plt clean_plt
