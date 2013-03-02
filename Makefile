.PHONY: all get-deps compile clean

all: compile

compile:
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean
