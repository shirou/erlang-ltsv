%% -------------------------------------------------------------------
%%
%% erlang-ltsv: Labeled Tab-separated Values Parser for Erlang
%%
%% -------------------------------------------------------------------

%% @doc Implementation of Labeled Tab-separated Values Parser
%% @reference [http://ltsv.org/]

-module(ltsv).

-export([parse_line/1, parse_file/1, write/1]).


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

-spec write([{term(), term()}]) -> string().
%% @doc convert data to LTSV format string.
write(Data) ->
	F = fun({Label, Field}) ->
				<<Label/binary, $:, Field/binary>>
		end,
	binary:bin_to_list(join(lists:map(F, Data), <<$\t>>)).


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
