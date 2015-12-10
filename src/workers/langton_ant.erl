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

-include("cellular_logger.hrl").

%% Callbacks
-export([init/2, compute_stuff/1]).

-record(cell, {color, direction}).

%%%===================================================================
%%% Cellular worker behaviour callbacks
%%%===================================================================

init(Width, Height) ->
    A = maps:put({Width div 2, Height div 2}, #cell{color = white, direction = right}, #{}),
%%    ?info("~p", [A]),
    A.

compute_stuff(Board) ->
    A = maps:fold(fun({X, Y}, Cell, NewBoard) ->
        case Cell#cell.direction of
            undefined -> maps:put({X, Y}, Cell, NewBoard);
            Direction ->
                NewDirection = turn(Cell#cell.color, Direction),
                maps:put({X, Y}, Cell#cell{direction = NewDirection}, NewBoard)
        end
    end, #{}, Board),

    B = maps:fold(fun({X, Y}, Cell, NewBoard) ->
        case Cell#cell.direction of
            undefined -> maps:put({X, Y}, Cell, NewBoard);
            _ ->
                maps:put({X, Y}, Cell#cell{color = swap(Cell#cell.color)}, NewBoard)
        end
    end, #{}, A),

    C = maps:fold(fun({X, Y}, Cell, NewBoard) ->
        case Cell#cell.direction of
            undefined ->
                OldCell = maps:get({X, Y}, NewBoard, #cell{color = white}),
                maps:put({X, Y}, OldCell#cell{color = Cell#cell.color}, NewBoard);

            Direction ->
                NewPos = move(X, Y, Direction),
                OldCell = maps:get(NewPos, NewBoard, #cell{color = white}),
                Omg = maps:put(NewPos, OldCell#cell{direction = Direction}, NewBoard),
                case Cell#cell.color of
                    white -> Omg;
                    black -> maps:put({X, Y}, #cell{color = black}, Omg)
                end
        end
    end, #{}, B),

%%    ?info("~p", [C]),
    C.



move(X, Y, right) -> {X + 1, Y};
move(X, Y, down) -> {X, Y - 1};
move(X, Y, left) -> {X - 1, Y};
move(X, Y, up) -> {X, Y + 1}.

swap(black) -> white;
swap(white) -> black.

turn(black, right) -> up;
turn(black, up) -> left;
turn(black, left) -> down;
turn(black, down) -> right;
turn(white, right) -> down;
turn(white, down) -> left;
turn(white, left) -> up;
turn(white, up) -> right.