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
-export([init_step/2, compute_next_state/2, merge_state/3, merge_neighbour_state/5]).

-define(TILE_WIDTH, 9).
-define(TILE_HEIGHT, 9).

-type color() :: black | white.
-type coordinate() :: non_neg_integer().
-type position() :: {coordinate(), coordinate()}.
-type direction() :: left | up | right | down.

-record(ant, {
    x :: coordinate(),
    y :: coordinate(),
    direction :: direction()
}).

-record(tile, {
    ants :: [#ant{}],
    colors :: #{position() => color()}
}).

%%%===================================================================
%%% Cellular worker behaviour callbacks
%%%===================================================================

init_step(0, _) ->
    AntX = ?TILE_WIDTH div 2,
    AntY = ?TILE_WIDTH div 2,
    Colors = lists:foldl(fun(X, Map) ->
        lists:foldl(fun(Y, PartialMap) ->
            maps:put({X, Y}, white, PartialMap)
        end, Map, lists:seq(0, ?TILE_HEIGHT - 1))
    end, #{}, lists:seq(0, ?TILE_WIDTH - 1)),
    #tile{ants = [#ant{x = AntX, y = AntY, direction = up}],
        colors = maps:put({AntX, AntY}, black, Colors)};
init_step(Step, State) ->
    write_tile(Step, State),
    State.

compute_next_state(#tile{ants = Ants, colors = Colors} = Tile, NbrsTiles) ->
    {NewAnts, NewColors} = lists:foldl(fun
        (#ant{x = X, y = Y} = Ant, {AntsAcc, ColorsAcc}) ->
            case maps:find({X, Y}, ColorsAcc) of
                {ok, black} ->
                    {[rotate_right_and_move(Ant) | AntsAcc], maps:put({X, Y}, white, ColorsAcc)};
                {ok, white} ->
                    {[rotate_left_and_move(Ant) | AntsAcc], maps:put({X, Y}, black, ColorsAcc)}
            end
    end, {[], Colors}, Ants),
    {FilteredAnts, NewNbrsTiles} = lists:foldl(fun
        (Ant, {AntsAcc, NbrsTilesAcc}) ->
            case in_tile(Ant) of
                true ->
                    {[Ant | AntsAcc], NbrsTilesAcc};
                {false, NbrTag} ->
                    {AntsAcc, move_to_neighbour(Ant, NbrTag, NbrsTilesAcc)}
            end
    end, {[], NbrsTiles}, NewAnts),
    {Tile#tile{ants = FilteredAnts, colors = NewColors}, NewNbrsTiles}.

merge_state(_NbrTag, State, _NextState) ->
    State.

merge_neighbour_state(NbrTag, State, NbrState, NextState, NextNbrState) ->
    {
        merge_state(NbrTag, State, NextState),
        merge_state(cellular_worker2:inverse_neighbour_tag(NbrTag), NbrState, NextNbrState)
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rotate_right_and_move(#ant{direction = left, y = Y} = Ant) ->
    Ant#ant{direction = up, y = Y + 1};
rotate_right_and_move(#ant{direction = up, x = X} = Ant) ->
    Ant#ant{direction = right, x = X + 1};
rotate_right_and_move(#ant{direction = right, y = Y} = Ant) ->
    Ant#ant{direction = down, y = Y - 1};
rotate_right_and_move(#ant{direction = down, x = X} = Ant) ->
    Ant#ant{direction = left, x = X - 1}.

rotate_left_and_move(#ant{direction = left, y = Y} = Ant) ->
    Ant#ant{direction = down, y = Y - 1};
rotate_left_and_move(#ant{direction = up, x = X} = Ant) ->
    Ant#ant{direction = left, x = X - 1};
rotate_left_and_move(#ant{direction = right, y = Y} = Ant) ->
    Ant#ant{direction = up, y = Y + 1};
rotate_left_and_move(#ant{direction = down, x = X} = Ant) ->
    Ant#ant{direction = right, x = X + 1}.

in_tile(#ant{x = X, y = Y}) ->
    case {X < 0, X >= ?TILE_WIDTH} of
        {true, false} -> {false, left};
        {false, true} -> {false, right};
        {false, false} ->
            case {Y < 0, Y >= ?TILE_HEIGHT} of
                {true, false} -> {false, down};
                {false, true} -> {false, up};
                {false, false} -> true
            end
    end.

move_to_neighbour(Ant, NbrTag, NbrsTiles) ->
    NbrTile = #tile{ants = Ants} = proplists:get_value(NbrTag, NbrsTiles),
    NewNbrsTiles = proplists:delete(NbrTag, NbrsTiles),
    [{NbrTag, NbrTile#tile{ants = [Ant | Ants]}} | NewNbrsTiles].

write_tile(Step, #tile{colors = Colors}) ->
    Filename = filename:join("../../data", integer_to_list(Step) ++ ".txt"),
    Header = <<"Step: ", (integer_to_binary(Step))/binary, "\n\n">>,
    file:write_file(Filename, Header, [write]),
    lists:foreach(fun(Y) ->
        NewLine = lists:foldl(fun(X, Line) ->
            case maps:find({X, Y}, Colors) of
                {ok, black} -> <<Line/binary, "x">>;
                {ok, white} -> <<Line/binary, ".">>
            end
        end, <<>>, lists:seq(0, ?TILE_WIDTH - 1)),
        file:write_file(Filename, <<NewLine/binary, "\n">>, [append])
    end, lists:seq(?TILE_HEIGHT - 1, 0, -1)).
