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
-export([run_simulation/5]).

%%%===================================================================
%%% API
%%%===================================================================

run_simulation(Module, MaxSteps, Width, Height, Timeout) ->
    gen_server:cast(?CELLULAR_MANAGER_NAME, {run_simulation, Module, MaxSteps,
        {0, Width - 1}, {0, Height - 1}, self()}),
    receive
        simulation_finished -> ok
    after
        timer:seconds(Timeout) -> {error, timeout}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================