-module(ltsv_tests).
-import(ltsv, [parse/1, parse_line/1, parse_file/1,
			   to_binary/1, to_list/1,
			   get_fields/2]).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
	?assertEqual([[{<<"a">>,<<"b">>}]], parse("a:b")),
	?assertEqual([[{<<"a">>,<<"b">>}, {<<"1">>,<<"2">>}]], parse("a:b\t1:2")),
	?assertEqual([[{<<"a">>,<<"b">>}, {<<"1">>,<<"2">>}],
				  [{<<"a">>,<<"b">>}, {<<"1">>,<<"2">>}]],
				 parse("a:b\t1:2\na:b\t1:2")),
	%% check binary
	?assertEqual([[{<<"a">>,<<"b">>}, {<<"1">>,<<"2">>}],
				  [{<<"a">>,<<"b">>}, {<<"1">>,<<"2">>}]],
				 parse(<<"a:b\t1:2\na:b\t1:2">>)).

parse_line_test() ->
	%% one tuple
	?assertEqual([{<<"a">>,<<"b">>}], parse_line("a:b")),
	%% with tab
	?assertEqual([{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}], parse_line("a:b\tc:d")),
	%% with newline
	?assertEqual([{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}], parse_line("a:b\tc:d\n")).

parse_file_test() ->
	T = parse_file("../test/test.tsv"),
	?assertEqual([{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}],
				 lists:nth(1, T)),
	?assertEqual([{<<"11">>,<<"a">>},{<<"12">>,<<"bb">>},{<<"13">>,<<"d:e">>}],
				 lists:nth(2, T)),
	?assertEqual([{<<"21">>,<<"a">>},{<<"22">>,<<"bb">>},{<<"33">>,<<227,131,134,227,130,185,227,131,136>>}],
				 lists:nth(3, T)),
	?assertEqual([{<<"21">>,<<"double">>},{<<"22">>,<<"bb">>},{<<"21">>,<<"double2">>}],
				 lists:nth(4, T)).

to_binary_test() ->
	Src = [[{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}],
		   [{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}]],
	Expected = <<"1:a\t2:bb\t3:d:e1:a\t2:bb\t3:d:e">>,
	?assertEqual(Expected, to_binary(Src)).

to_list_test() ->
	Src = [[{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}],
		   [{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}]],
	Expected = [<<"1:a\t2:bb\t3:d:e">>, <<"1:a\t2:bb\t3:d:e">>],
	?assertEqual(Expected, to_list(Src)).


get_fields_test() ->
	Src = "1:a\t2:bb\t3:d:e\n1:a\t2:bb\t3:d:e",
	S = parse(Src),
	?assertEqual([<<"bb">>, <<"bb">>], get_fields(S, <<"2">>)).
