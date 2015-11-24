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
-export([init/0, compute_next_state/2, merge_state/2, merge_neighbour_state/5]).

%%%===================================================================
%%% Cellular worker behaviour callbacks
%%%===================================================================

init() ->
    #{}.

compute_next_state(State, NbrsStates) ->
    {State, NbrsStates}.

merge_state(State, _NextState) ->
    State.

merge_neighbour_state(_NbrTag, State, NbrState, _NextState, _NextNbrState) ->
    {State, NbrState}.

%%%===================================================================
%%% Internal functions
%%%===================================================================