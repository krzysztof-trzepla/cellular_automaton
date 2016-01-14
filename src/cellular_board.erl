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
-module(cellular_board).
-author("Krzysztof Trzepla").

-include("cellular_automaton.hrl").
-include("cellular_logger.hrl").

%% API
-export([find/2, find/3, get/3, put/3, put/4, delete/2, delete/3, foreach/2, fold/3]).

%%%===================================================================
%%% API
%%%===================================================================

find(#board{} = B, {X, Y}) ->
    find(B, X, Y).

find(#board{w = W, h = H, ids = Ids}, X, Y) ->
    ?info("===> op: ~p", [self()]),
    Id = board_id(X, Y, W, H, Ids),
    Nx = normalize(X, W),
    Ny = normalize(Y, H),
    case ets:lookup(Id, {Nx, Ny}) of
        [] -> {error, not_found};
        [{_, V}] -> {ok, V}
    end.

get(#board{} = B, X, Y) ->
    {ok, V} = find(B, X, Y),
    V.

put(#board{} = B, {X, Y}, V) ->
    put(B, X, Y, V).

put(#board{w = W, h = H, ids = Ids}, X, Y, V) ->
    ?info("===> op: ~p", [self()]),
    Id = board_id(X, Y, W, H, Ids),
    Nx = normalize(X, W),
    Ny = normalize(Y, H),
    ets:insert(Id, {{Nx, Ny}, V}).

delete(#board{} = B, {X, Y}) ->
    delete(B, X, Y).

delete(#board{w = W, h = H, ids = Ids}, X, Y) ->
    ?info("===> op: ~p", [self()]),
    Id = board_id(X, Y, W, H, Ids),
    Nx = normalize(X, W),
    Ny = normalize(Y, H),
    ets:delete(Id, {Nx, Ny}).

foreach(#board{w = W, h = H, dw = Dw, dh = Dh, ids = Ids}, F) ->
    ?info("===> op: ~p", [self()]),
    maps:fold(fun
%%        ({-1, -1}, Id, _) -> foreach(F, W - Dw, H - Dh, W - 1, H - 1, -W, -H, Id);
%%        ({-1, 0}, Id, _) -> foreach(F, W - Dw, 0, W - 1, H - 1, -W, 0, Id);
%%        ({-1, 1}, Id, _) -> foreach(F, W - Dw, 0, W - 1, Dh - 1, -W, H, Id);
%%
%%        ({0, -1}, Id, _) -> foreach(F, 0, H - Dh, W - 1, H - 1, 0, -H, Id);
        ({0, 0}, Id, _) -> ets:foldl(fun({K, V}, _) -> F(K, V) end, ok, Id);
        (_, _, _) -> ok
%%        ({0, 1}, Id, _) -> foreach(F, 0, 0, W - 1, Dh - 1, 0, H, Id);
%%
%%        ({1, -1}, Id, _) -> foreach(F, 0, H - Dh, Dw - 1, H - 1, W, -H, Id);
%%        ({1, 0}, Id, _) -> foreach(F, 0, 0, Dw - 1, H - 1, W, 0, Id);
%%        ({1, 1}, Id, _) -> foreach(F, 0, 0, Dw - 1, Dh - 1, W, H, Id)
    end, ok, Ids).

fold(#board{ids = Ids}, F, Acc) ->
    ?info("===> op: ~p", [self()]),
    ets:foldl(fun({K, V}, A) -> F(K, V, A) end, Acc, maps:get({0,0}, Ids)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

board_id(X, Y, W, H, Ids) ->
    Sx = 0 = sigma(X, 0, W - 1),
    Sy = 0 = sigma(Y, 0, H - 1),
    maps:get({Sx, Sy}, Ids).

sigma(X, Min, _) when X < Min ->
    ?info("sigma -1"),
    -1;
sigma(X, _, Max) when X > Max ->
    ?info("sigma +1"),
    1;
sigma(_, _, _) -> 0.


normalize(X, Max) ->
    (X + Max) rem Max.


foreach(F, MinX, MinY, MaxX, MaxY, Dx, Dy, Id) ->
    foreach(fun(X) ->
        foreach(fun(Y) ->
            case ets:lookup(Id, {X, Y}) of
                [] -> ok;
                [{_, V}] -> F({X + Dx, Y + Dy}, V)
            end
        end, MinY, MaxY)
    end, MinX, MaxX).

foreach(_, Min, Max) when Min > Max ->
    ok;
foreach(F, Min, Max) ->
    F(Min),
    foreach(F, Min + 1, Max).
