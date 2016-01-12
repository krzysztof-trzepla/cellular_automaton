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
init(Map) ->
    spawn_individuals(Map, forams_config:algae_initial_generation_size()).

insert(Map, Algae = #algae{coords = Coords}) ->
    case maps:get(Coords, Map, undefined) of
        #foram{} ->
            Map;
        _ ->
            maps:put(Coords, Algae, Map)
    end.

grow_all(Map) ->
    maps:fold(
        fun
            (_, #algae{} = V, M) ->
                grow(M, V);
            (_, _, M) ->
                M
        end, Map, Map
    ).

grow(Map, Algae = #algae{energy = Energy}) ->
    insert(Map, Algae#algae{energy = Energy + forams_config:algae_growth_rate()}).

reproduce_all(Map) ->
    maps:fold(
        fun
            (_, V = #algae{energy = Energy}, M) ->
                case Energy >= forams_config:algae_reproduction_limit() of
                    true ->
                        reproduce(M, V);
                    false ->
                        M
                end;
            (_, _, M) ->
                M
        end, Map, Map
    ).

reproduce(Map, Algae = #algae{coords = Coords, energy = Energy}) ->
    case random_valid_move(Map, Coords) of
        undefined ->
            Map;
        NewCoord ->
            Map2 = maps:put(Coords, Algae#algae{energy = Energy/2}, Map),
            insert(Map2, Algae#algae{coords = NewCoord, energy = Energy/2})
    end.

spawn_individuals(Map) ->
    spawn_individuals(Map, forams_config:algae_spawn_size()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

spawn_individuals(Map, Num) ->
    NumberOfAlgaes = round(Num *
        forams_config:width() * forams_config:height()),
    RandomAlgaes = [random_algae() || _ <- lists:seq(1, NumberOfAlgaes)],
    Algaes = lists:usort(RandomAlgaes),
    lists:foldl(fun(Algae, M) -> algae:insert(M, Algae) end, Map, Algaes).

random_algae() ->
    #algae{coords = utils:random_coordinates(forams_config:width(),
        forams_config:height())}.

random_valid_move(_, []) ->
    undefined;
random_valid_move(Map, [Move | Rest]) ->
    case is_valid_move(Map, Move) of
        true ->
            Move;
        false ->
            random_valid_move(Map, Rest)
    end;
random_valid_move(Map, {X, Y}) ->
    DeltasOfMoves = utils:random_shuffle(forams_config:valid_moves()),
    Moves = [{X + DX, Y + DY} || {DX, DY} <- DeltasOfMoves],
    random_valid_move(Map, Moves).

is_valid_move(Map, Coords = {X, Y})->
    case (X < 0
        orelse X >= forams_config:width()
        orelse Y < 0
        orelse Y >= forams_config:height())
    of
        true -> false;
        _ ->
            case maps:find(Coords, Map) of
                {ok, #foram{}} ->
                    false;
                {ok, #algae{}} ->
                    false;
                _ ->
                    true
            end
    end.
