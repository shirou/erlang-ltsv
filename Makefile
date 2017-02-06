.PHONY: all compile test

APP_NAME = erlang-ltsv

all: compile test

compile:
	@./rebar3 xref

test:
	@./rebar3 as test eunit
