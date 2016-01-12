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

-include("cellular_automaton.hrl").
-include("cellular_logger.hrl").

%% Callbacks
-export([width/0, height/0, border_width/0, border_height/0, max_desynchronization/0,
    init/0, step/2]).

-record(cell, {color = white, ant = undefined}).

%%%===================================================================
%%% Cellular worker behaviour callbacks
%%%===================================================================

width() ->
    Config = application:get_env(?APPLICATION_NAME, langton_ant, []),
    proplists:get_value(width, Config, 10).

height() ->
    Config = application:get_env(?APPLICATION_NAME, langton_ant, []),
    proplists:get_value(height, Config, 10).

border_width() ->
    Config = application:get_env(?APPLICATION_NAME, langton_ant, []),
    proplists:get_value(border_width, Config, 5).

border_height() ->
    Config = application:get_env(?APPLICATION_NAME, langton_ant, []),
    proplists:get_value(border_height, Config, 5).

max_desynchronization() ->
    Config = application:get_env(?APPLICATION_NAME, langton_ant, []),
    proplists:get_value(max_desynchronization, Config, 1).

init() ->
    Config = application:get_env(?APPLICATION_NAME, langton_ant, []),
    AntNum = proplists:get_value(ant_number, Config, 1),
    Width = width(),
    Height = height(),
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    board(AntNum, Width, Height, #{}).

step(Ctx, Board) ->
    draw(Ctx#{draw => true}, Board),
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

draw(#{draw := true, step := Step, fd := Fd}, Board) ->
    Width = width(),
    Height = height(),
    Header = <<"Step: ", (integer_to_binary(Step))/binary, "\n">>,
    file:write(Fd, Header),
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
        end, <<>>, lists:seq(0, Width)),
        file:write(Fd, <<NewLine/binary, "\n">>)
    end, lists:seq(Height, 0, -1));
draw(_, _) ->
    ok.

board(AntNum, _, _, Board) when AntNum =< 0 ->
    Board;
board(AntNum, Width, Height, Board) ->
    X = random:uniform(Width) - 1,
    Y = random:uniform(Height) - 1,
    Dir = lists:nth(random:uniform(4), [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]),
    case maps:find({X, Y}, Board) of
        {ok, _} ->
            board(AntNum, Width, Height, Board);
        error ->
            board(AntNum - 1, Width, Height, maps:put({X, Y}, #cell{ant = Dir}, Board))
    end.