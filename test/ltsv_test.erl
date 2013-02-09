-module(ltsv_test).

-include_lib("eunit/include/eunit.hrl").

parse_line_test() ->
	%% one tuple
	?assert(ltsv:parse_line("a:b") =:= [{<<"a">>,<<"b">>}]),
	%% with tab
	?assert(ltsv:parse_line("a:b\tc:d") =:=
				[{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]),
	%% with newline
	?assert(ltsv:parse_line("a:b\tc:d\n") =:=
				[{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]).
