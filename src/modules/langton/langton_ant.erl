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
    init/1, step/2]).

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

init(Board) ->
    Config = application:get_env(?APPLICATION_NAME, langton_ant, []),
    AntNum = proplists:get_value(ant_number, Config, 1),
    Width = width(),
    Height = height(),
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    board(Board, AntNum, Width, Height).

step(Ctx, Board) ->
    draw(Ctx#{draw => true}, Board),
    cellular_board:foreach(Board, fun
        (_, #cell{color = black, ant = undefined}) ->
            ok;
        (Pos, #cell{color = white, ant = undefined}) ->
            cellular_board:delete(Board, Pos);
        ({X, Y}, #cell{color = Color, ant = Dir}) ->
            {DX, DY} = NewDir = rotate(Color, Dir),
            NewPos = {X + DX, Y + DY},
            case cellular_board:find(Board, NewPos) of
                {ok, Cell} ->
                    cellular_board:put(Board, NewPos, Cell#cell{ant = NewDir});
                {error, not_found} ->
                    cellular_board:put(Board, NewPos, #cell{ant = NewDir})
            end,
            case swap_color(Color) of
                black -> cellular_board:put(Board, {X, Y}, #cell{color = black});
                white -> ok
            end
    end).

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
            case cellular_board:find(Board, {X, Y}) of
                {ok, #cell{color = black, ant = undefined}} ->
                    <<Line/binary, "x">>;
                {ok, #cell{}} ->
                    <<Line/binary, "o">>;
                {error, not_found} ->
                    <<Line/binary, ".">>
            end
        end, <<>>, lists:seq(0, Width)),
        file:write(Fd, <<NewLine/binary, "\n">>)
    end, lists:seq(Height, 0, -1));
draw(_, _) ->
    ok.

board(_, AntNum, _, _) when AntNum =< 0 ->
    ok;
board(Board, AntNum, Width, Height) ->
    X = random:uniform(Width) - 1,
    Y = random:uniform(Height) - 1,
    Dir = lists:nth(random:uniform(4), [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]),
    case cellular_board:find(Board, {X, Y}) of
        {ok, _} ->
            board(Board, AntNum, Width, Height);
        {error, not_found} ->
            cellular_board:put(Board, {X, Y}, #cell{ant = Dir}),
            board(Board, AntNum - 1, Width, Height)
    end.