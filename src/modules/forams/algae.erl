%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 KI AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(algae).
-author("Tomasz Lichon").

-include("cellular_logger.hrl").
-include("modules/forams/algae.hrl").
-include("modules/forams/foram.hrl").

%% API
-export([init/1, insert/2, grow_all/1, reproduce_all/1, spawn_individuals/1]).

%%%===================================================================
%%% API
%%%===================================================================
init(Board) ->
    spawn_individuals(Board, forams_config:algae_initial_generation_size()).

insert(Board, Algae = #algae{coords = Coords}) ->
    case cellular_board:find(Board, Coords) of
        {ok, #foram{}} ->
            ok;
        _ ->
            cellular_board:put(Board, Coords, Algae)
    end.

grow_all(Board) ->
    cellular_board:foreach(Board, fun
        (_, #algae{} = V) -> grow(Board, V);
        (_, _) -> ok
    end).

grow(Board, Algae = #algae{energy = Energy}) ->
    insert(Board, Algae#algae{energy = Energy + forams_config:algae_growth_rate()}).

reproduce_all(Board) ->
    cellular_board:foreach(Board, fun
        (_, V = #algae{energy = Energy}) ->
            case Energy >= forams_config:algae_reproduction_limit() of
                true ->
                    reproduce(Board, V);
                false ->
                    ok
            end;
        (_, _) ->
            ok
    end).

reproduce(Board, Algae = #algae{coords = Coords, energy = Energy}) ->
    case random_valid_move(Board, Coords) of
        undefined ->
            ok;
        NewCoord ->
            cellular_board:put(Board, Coords, Algae#algae{energy = Energy / 2}),
            insert(Board, Algae#algae{coords = NewCoord, energy = Energy / 2})
    end.

spawn_individuals(Board) ->
    spawn_individuals(Board, forams_config:algae_spawn_size()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

spawn_individuals(Board, Num) ->
    NumberOfAlgaes = round(Num *
        forams_config:width() * forams_config:height()),
    RandomAlgaes = [random_algae() || _ <- lists:seq(1, NumberOfAlgaes)],
    Algaes = lists:usort(RandomAlgaes),
    lists:foreach(fun(Algae) -> algae:insert(Board, Algae) end, Algaes).

random_algae() ->
    #algae{coords = utils:random_coordinates(forams_config:width(),
        forams_config:height())}.

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
        true -> false;
        _ ->
            case cellular_board:find(Board, Coords) of
                {ok, #foram{}} ->
                    false;
                {ok, #algae{}} ->
                    false;
                _ ->
                    true
            end
    end.
