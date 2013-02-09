-module(ltsv_tests).
-import(ltsv, [parse_line/1, parse_file/1, write/1]).

-include_lib("eunit/include/eunit.hrl").

parse_line_test() ->
	%% one tuple
	?assertEqual(ltsv:parse_line("a:b"), [{<<"a">>,<<"b">>}]),
	%% with tab
	?assertEqual(ltsv:parse_line("a:b\tc:d"),
				[{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]),
	%% with newline
	?assertEqual(ltsv:parse_line("a:b\tc:d\n"),
				[{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]).

parse_file_test() ->
	T = ltsv:parse_file("../test/test.tsv"),
	?assertEqual(lists:nth(1, T),
				[{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}]),
	?assertEqual(lists:nth(2, T),
				[{<<"11">>,<<"a">>},{<<"12">>,<<"bb">>},{<<"13">>,<<"d:e">>}]),
	?assertEqual(lists:nth(3, T),
				[{<<"21">>,<<"a">>},{<<"22">>,<<"bb">>},{<<"33">>,<<227,131,134,227,130,185,227,131,136>>}]),
	?assertEqual(lists:nth(4, T),
				[{<<"21">>,<<"double">>},{<<"22">>,<<"bb">>},{<<"21">>,<<"double2">>}]).

write_test() ->
	Src = [{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}],
	Expected = "1:a\t2:bb\t3:d:e",
	?assertEqual(ltsv:write(Src), Expected).
