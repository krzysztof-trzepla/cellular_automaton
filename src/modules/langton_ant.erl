%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2015 KI AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(langton_ant).
-author("Krzysztof Trzepla").
-behaviour(cellular_worker_behaviour).

-include("cellular_logger.hrl").

%% Callbacks
-export([width/0, height/0, border_width/0, border_height/0, max_desynchronization/0,
    init/0, step/2]).

-record(cell, {color = white, ant = undefined}).

%%%===================================================================
%%% Cellular worker behaviour callbacks
%%%===================================================================

width() -> 10.

height() -> 10.

border_width() -> 5.

border_height() -> 5.

max_desynchronization() -> 5.

init() -> #{{5, 5} => #cell{ant = {1, 0}}}.

step(_Step, Board) ->
    % draw(Step, Board),
    maps:fold(fun
        (Pos, #cell{color = black, ant = undefined}, NewBoard) ->
            Cell = maps:get(Pos, NewBoard, #cell{}),
            maps:put(Pos, Cell#cell{color = black}, NewBoard);
        ({X, Y}, #cell{color = Color, ant = Dir}, NewBoard) ->
            {DX, DY} = NewDir = rotate(Color, Dir),
            NewPos = {X + DX, Y + DY},
            Cell = maps:get(NewPos, NewBoard, #cell{}),
            PartialBoard = maps:put(NewPos, Cell#cell{ant = NewDir}, NewBoard),
            case swap_color(Color) of
                black -> maps:put({X, Y}, #cell{color = black}, PartialBoard);
                white -> PartialBoard
            end
    end, #{}, Board).

%%%===================================================================
%%% Internal functions
%%%===================================================================


rotate(black, {0, 1}) -> {1, 0};
rotate(black, {1, 0}) -> {0, -1};
rotate(black, {0, -1}) -> {-1, 0};
rotate(black, {-1, 0}) -> {0, 1};
rotate(white, {0, 1}) -> {-1, 0};
rotate(white, {-1, 0}) -> {0, -1};
rotate(white, {0, -1}) -> {1, 0};
rotate(white, {1, 0}) -> {0, 1}.

swap_color(black) -> white;
swap_color(white) -> black.

draw(Step, Board) ->
    Filename = filename:join("../../data", integer_to_list(Step) ++ ".txt"),
    Header = <<"Step: ", (integer_to_binary(Step))/binary, "\n\n">>,
    file:write_file(Filename, Header, [write]),
    lists:foreach(fun(Y) ->
        NewLine = lists:foldl(fun(X, Line) ->
            case maps:find({X, Y}, Board) of
                {ok, #cell{color = black, ant = undefined}} ->
                    <<Line/binary, "x">>;
                {ok, #cell{}} ->
                    <<Line/binary, "o">>;
                error ->
                    <<Line/binary, ".">>
            end
        end, <<>>, lists:seq(-1, 11)),
        file:write_file(Filename, <<NewLine/binary, "\n">>, [append])
    end, lists:seq(11, -1, -1)).