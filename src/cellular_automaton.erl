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
-module(cellular_automaton).
-author("Krzysztof Trzepla").

-include("cellular_automaton.hrl").

%% API
-export([start_simulation/0, stop_simulation/0]).

-type tag() :: atom().
-type coordinate() :: non_neg_integer().
-type position() :: {coordinate(), coordinate()}.

-export_type([tag/0, coordinate/0, position/0]).

%%%===================================================================
%%% API
%%%===================================================================

start_simulation() ->
    gen_server:cast(?CELLULAR_MANAGER_NAME, start_simulation).

stop_simulation() ->
    gen_server:cast(?CELLULAR_MANAGER_NAME, stop_simulation).

%%%===================================================================
%%% Internal functions
%%%===================================================================