%% -------------------------------------------------------------------
%%
%% erlang-ltsv: Labeled Tab-separated Values Parser for Erlang
%%
%% -------------------------------------------------------------------

%% @doc Implementation of Labeled Tab-separated Values Parser
%% @reference [http://ltsv.org/]

-module(ltsv).

-export([parse_line/1, parse_file/1]).


-spec parse_line(sring()) -> [{term(), term()}].
%% @doc parse a LTSV formated line.
parse_line(Line) ->
    F = fun(N) ->
                case binary:split(N, <<$:>>) of
                    [Label | Field] ->
                        {Label, hd(Field)};
                    _ ->
                        error(invalid_syntax)
                end
        end,
	lists:map(F, binary:split(rstrip(Line), <<$\t>>, [global])).

-spec parse_file(sring()) -> [[{term(), term()}]].
%% @doc parse LTSV formated file.
parse_file(File) ->
	case file:open(File, [read, binary]) of
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

%% -------------------------------------------------------------------
%% UNIT TESTS
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_line_() ->
	?assert(parse_line("a:b") =:= [{<<"a">>,<<"b">>}]).

-endif.
