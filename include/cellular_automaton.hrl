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
-author("Krzysztof Trzepla").

-ifndef(CELLULAR_AUTOMATON).
-define(CELLULAR_AUTOMATON, true).

-define(APPLICATION_NAME, cellular_automaton).
-define(CELLULAR_MANAGER_NAME, cellular_manager).
-define(CELLULAR_WORKER_SUP_NAME, cellular_worker_sup).
-define(WORKER_NEIGHBOURS, [{0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}, {-1, -1},
    {-1, 0}, {-1, 1}]).

-record(board, {
    x :: integer(),
    y :: integer(),
    w :: non_neg_integer(),
    h :: non_neg_integer(),
    dw :: non_neg_integer(),
    dh :: non_neg_integer(),
    ids :: #{{integer(), integer()} => ets:tid()}
}).

-endif.
