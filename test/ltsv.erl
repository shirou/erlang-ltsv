-module(ltsv).

-include_lib("eunit/include/eunit.hrl").

parse_line_test_() ->
	?assert(ltsv:parse_line("a:b") =:= [{<<"a">>,<<"b">>}]).
