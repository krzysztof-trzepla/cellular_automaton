%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 KI AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(foram).
-author("Tomasz Lichon").

-include("modules/forams/algae.hrl").
-include("modules/forams/foram.hrl").

%% API
-export([init/1, insert/2, move_all/1, reproduce_all/1, starve_all/1, remove_dead/1, count/1]).

%%%===================================================================
%%% API
%%%===================================================================

init(Board) ->
    NumberOfForams = round(forams_config:foram_initial_generation_size() *
        forams_config:width() * forams_config:height()),
    RandomForams = [random_foram() || _ <- lists:seq(1, NumberOfForams)],
    Forams = lists:usort(RandomForams),
    lists:foreach(fun(Foram) -> foram:insert(Board, Foram) end, Forams).

insert(Board, Foram = #foram{coords = Coords, energy = Energy}) ->
    case cellular_board:find(Board, Coords) of
        {ok, #algae{energy = OldEnergy}} ->
            cellular_board:put(Board, Coords, Foram#foram{energy = OldEnergy + Energy});
        _ ->
            cellular_board:put(Board, Coords, Foram)
    end.

move_all(Board) ->
    cellular_board:foreach(Board, fun
        (_, #foram{} = V) -> move(Board, V);
        (_, _) -> ok
    end).

move(Board, Foram = #foram{coords = Coords}) ->
    case random_valid_move(Board, Coords) of
        undefined ->
            ok;
        NewCoord ->
            cellular_board:delete(Board, Coords),
            insert(Board, Foram#foram{coords = NewCoord})
    end.

reproduce_all(Board) ->
    cellular_board:foreach(Board, fun
        (_, V = #foram{energy = Energy}) ->
            case Energy >= forams_config:foram_reproduction_limit() of
                true ->
                    reproduce(Board, V);
                false ->
                    ok
            end;
        (_, _) ->
            ok
    end).

reproduce(Board, Foram = #foram{coords = Coords, energy = Energy}) ->
    case random_valid_move(Board, Coords) of
        undefined ->
            ok;
        NewCoord ->
            cellular_board:put(Board, Coords, Foram#foram{energy = Energy / 2}),
            insert(Board, Foram#foram{coords = NewCoord, energy = Energy / 2})
    end.

starve_all(Board) ->
    cellular_board:foreach(Board, fun
        (_, #foram{} = V) -> starve(Board, V);
        (_, _) -> ok
    end).

starve(Board, Foram = #foram{coords = Coords, energy = Energy}) ->
    cellular_board:put(Board, Coords, Foram#foram{energy = Energy - forams_config:foram_starvation_rate()}).

remove_dead(Board) ->
    cellular_board:foreach(Board, fun
        (Key, #foram{energy = Energy}) when Energy =< 0 ->
            cellular_board:delete(Board, Key);
        (_, _) -> ok
    end).

count(Board) ->
    cellular_board:fold(Board, fun
        (_, #foram{}, {ForamNum, OthersNum}) ->
            {ForamNum + 1, OthersNum};
        (_, _, {ForamNum, OthersNum}) ->
            {ForamNum, OthersNum + 1}
    end, {0, 0}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_foram() ->
    #foram{coords = utils:random_coordinates(forams_config:width(), forams_config:height())}.

random_valid_move(_, []) ->
    undefined;
random_valid_move(Board, [Move | Rest]) ->
    case is_valid_move(Board, Move) of
        true ->
            Move;
        false ->
            random_valid_move(Board, Rest)
    end;
random_valid_move(Board, {X, Y}) ->
    DeltasOfMoves = utils:random_shuffle(forams_config:valid_moves()),
    Moves = [{X + DX, Y + DY} || {DX, DY} <- DeltasOfMoves],
    random_valid_move(Board, Moves).

is_valid_move(Board, Coords = {X, Y}) ->
    case (X < 0
        orelse X >= forams_config:width()
        orelse Y < 0
        orelse Y >= forams_config:height())
    of
        true ->
            false;
        _ ->
            case cellular_board:find(Board, Coords) of
                {ok, #foram{}} ->
                    false;
                _ ->
                    true
            end
    end.
