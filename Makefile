.PHONY: all compile

APP_NAME = erlang-ltsv

all: compile

compile:
	@./rebar3 xref
