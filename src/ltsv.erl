%% -------------------------------------------------------------------
%%
%% erlang-ltsv: Labeled Tab-separated Values Parser for Erlang
%%
%% -------------------------------------------------------------------

%% @doc Implementation of Labeled Tab-separated Values Parser
%% @reference [http://ltsv.org/]

-module(ltsv).

-export([parse/1, parse_line/1, parse_file/1,
		 to_binary/1, to_list/1,
		 get_fields/2]).


-spec parse(binary() | string()) -> [[{binary(), binary()}]].
%% @doc parse LTSV formated lines.
parse(Source) when is_list(Source) ->
	parse(list_to_binary(Source));
parse(Source) when is_binary(Source) ->
	lists:map(fun(N) -> parse_line(N) end,
			  binary:split(Source, <<$\n>>, [global])).

-spec parse_line(binary()) -> [{binary(), binary()}].
%% @doc parse a LTSV formated line.
parse_line(Line) ->
    F = fun(N) ->
                case binary:split(N, <<$:>>) of
                    [Label, Field] ->
                        {Label, Field};
                    _ ->
                        error(invalid_syntax)
                end
        end,
    lists:map(F, binary:split(rstrip(Line), <<$\t>>, [global])).

-spec parse_file(string()) -> [[{binary(), binary()}]].
%% @doc parse LTSV formated file.
parse_file(File) ->
	case file:open(File, [read, binary, raw]) of
		{ok, Device} ->
			parse_file(Device, []);
		{error, Reason} ->
			error(Reason)
	end.
parse_file(Device, Acc) ->
	case file:read_line(Device) of
		eof  ->
			file:close(Device),
			lists:reverse(Acc);
		{ok, Line} ->
			parse_file(Device, [parse_line(Line)|Acc]);
		{error, Reason} ->
			error(Reason)
    end.

-spec to_binary([[{binary(), binary()}]]) -> binary().
%% @doc convert lists to LTSV format binary.
to_binary(Data) ->
	D = lists:map(fun(N) -> to_binary_one(N) end, Data),
	binary:list_to_bin(D).

-spec to_list([{binary(), binary()}]) -> list().
%% @doc convert lists to LTSV format list.
to_list(Data) ->
	lists:map(fun(N) -> to_binary_one(N) end, Data).

-spec to_binary_one([{binary(), binary()}]) -> binary().
%% @doc convert one list to LTSV format binary.
to_binary_one(Data) ->
	F = fun({Label, Field}) ->
				<<Label/binary, $:, Field/binary>>
		end,
	join(lists:map(F, Data), <<$\t>>).


-spec get_fields(binary() | list(), binary()) -> list().
%% @doc get a list of value which is specified by the key
get_fields(Data, Key) when is_list(Data) ->
	F = fun(N) ->
				{_, Field} = lists:keyfind(Key, 1, N),
				Field
		end,
	lists:map(F, Data);
get_fields(Data, Key) when is_binary(Data) ->
	get_fields(binary:bin_to_list(Data), Key).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

-spec rstrip(binary()) -> binary().
rstrip(<<>>) ->
    <<>>;
rstrip(String) when is_list(String) ->
    rstrip(list_to_binary(String));
rstrip(Binary) when is_binary(Binary) ->
    Size = byte_size(Binary) - 1,
    case Binary of
        <<Rest:Size/binary, C>> when C =:= $\s orelse C =:= $\t orelse
                                     C =:= $\r orelse C =:= $\n ->
            rstrip(Rest);
        _ ->
            Binary
    end.

-spec join(binary(), binary()) -> binary().
join([First|Rest], JoinWith) ->
    list_to_binary( [First|[ <<JoinWith/binary, X/binary>> || X <- Rest]] ).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
rstrip_test() ->
	%% strip
	?assertEqual(rstrip(<<"ab\n">>), <<"ab">>),
	%% no newline
	?assertEqual(rstrip(<<"ab">>), <<"ab">>),
	%% keep newline in the middle
	?assertEqual(rstrip(<<"ab\ncd">>), <<"ab\ncd">>).

join_test() ->
	?assertEqual(join([<<"a">>, <<"b">>], <<$:>>), <<"a:b">>).

-endif.
