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

init(Map) ->
    NumberOfForams = round(forams_config:foram_initial_generation_size()*
        forams_config:width()* forams_config:height()),
    RandomForams = [random_foram() || _ <- lists:seq(1, NumberOfForams)],
    Forams = lists:usort(RandomForams),
    lists:foldl(fun(Foram, M) -> foram:insert(M, Foram) end, Map, Forams).

insert(Map, Foram = #foram{coords = Coords, energy = Energy}) ->
    case maps:get(Coords, Map, undefined) of
        #algae{energy = OldEnergy} ->
            maps:put(Coords, Foram#foram{energy = OldEnergy + Energy}, Map);
        #foram{} ->
            Map;
        _ ->
            maps:put(Coords, Foram, Map)
    end.

move_all(Map) ->
    maps:fold(
        fun
            (_, #foram{} = V, M) ->
                move(M, V);
            (_, _, M) ->
                M
        end, Map, Map
    ).

move(Map, Foram = #foram{coords = Coords}) ->
    case random_valid_move(Map, Coords) of
        undefined ->
            Map;
        NewCoord ->
            insert(Map, Foram#foram{coords = NewCoord})
    end.

reproduce_all(Map) ->
    maps:fold(
        fun
            (_, V = #foram{energy = Energy}, M)->
                case Energy >= forams_config:foram_reproduction_limit() of
                    true ->
                        reproduce(M, V);
                    false ->
                        M
                end;
            (_, _, M) ->
                M
        end, Map, Map
    ).

reproduce(Map, Foram = #foram{coords = Coords, energy = Energy}) ->
    case random_valid_move(Map, Coords) of
        undefined ->
            Map;
        NewCoord ->
            Map2 = maps:put(Coords, Foram#foram{energy = Energy/2}, Map),
            insert(Map2, Foram#foram{coords = NewCoord, energy = Energy/2})
    end.

starve_all(Map) ->
    maps:fold(
        fun
            (_, #foram{} = V, M) ->
                starve(M, V);
            (_, _, M) ->
                M
        end, Map, Map
    ).

starve(Map, Foram = #foram{coords = Coords, energy = Energy}) ->
    maps:put(Coords, Foram#foram{energy = Energy - forams_config:foram_starvation_rate()}, Map).

remove_dead(Map) ->
    maps:fold(
        fun
            (Key, #foram{energy = Energy}, M) when Energy =< 0 ->
                maps:remove(Key, M);
            (_, _, M) ->
                M
        end, Map, Map).

count(Map) ->
    maps:fold(fun
        (_, #foram{}, {ForamNum, OthersNum}) ->
            {ForamNum + 1, OthersNum};
        (_, _, {ForamNum, OthersNum}) ->
            {ForamNum, OthersNum + 1}
    end, {0, 0}, Map).

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_foram() ->
    #foram{coords = utils:random_coordinates(forams_config:width(), forams_config:height())}.

random_valid_move(_, []) ->
    undefined;
random_valid_move(Map, [Move | Rest]) ->
    case is_valid_move(Map, Move) of
        true ->
            Move;
        false ->
            random_valid_move(Map, Rest)
    end;
random_valid_move(Map, {X,Y}) ->
    DeltasOfMoves = utils:random_shuffle(forams_config:valid_moves()),
    Moves = [{X + DX, Y + DY} || {DX, DY} <- DeltasOfMoves],
    random_valid_move(Map, Moves).

is_valid_move(Map, Coords = {X, Y})->
    case (X < 0
        orelse X >= forams_config:width()
        orelse Y < 0
        orelse Y >= forams_config:height())
    of
        true ->
            false;
        _ ->
            case maps:find(Coords, Map) of
                {ok, #foram{}} ->
                    false;
                _ ->
                    true
            end
    end.
