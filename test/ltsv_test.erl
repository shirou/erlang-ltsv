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

parse_file_test() ->
	T = ltsv:parse_file("../test/test.tsv"),
	?assert(lists:nth(1, T) =:=
				[{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}]),
	?assert(lists:nth(2, T) =:=
				[{<<"11">>,<<"a">>},{<<"12">>,<<"bb">>},{<<"13">>,<<"d:e">>}]),
	?assert(lists:nth(3, T) =:=
				[{<<"21">>,<<"a">>},{<<"22">>,<<"bb">>},{<<"33">>,<<227,131,134,227,130,185,227,131,136>>}]),
	?assert(lists:nth(4, T) =:=
				[{<<"21">>,<<"double">>},{<<"22">>,<<"bb">>},{<<"21">>,<<"double2">>}]).
