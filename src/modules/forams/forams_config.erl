%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(forams_config).
-author("Tomasz Lichon").

%% API
-include("cellular_automaton.hrl").

-export([width/0, height/0, border_width/0, border_height/0, max_desynchronization/0,
    foram_initial_generation_size/0, foram_reproduction_limit/0, foram_initial_energy/0,
    foram_starvation_rate/0, algae_initial_generation_size/0, algae_reproduction_limit/0,
    algae_initial_energy/0, algae_growth_rate/0, algae_spawn_size/0, valid_moves/0]).

%%%===================================================================
%%% API
%%%===================================================================

width() ->
    get_env(width).

height() ->
    get_env(height).

border_width() ->
    get_env(border_width).

border_height() ->
    get_env(border_height).

max_desynchronization() ->
    get_env(max_desynchronization).

foram_initial_generation_size() ->
    get_env(foram_initial_generation_size).

foram_reproduction_limit() ->
    get_env(foram_reproduction_limit).

foram_initial_energy() ->
    get_env(foram_initial_energy).

foram_starvation_rate() ->
    get_env(foram_starvation_rate).

algae_initial_generation_size() ->
    get_env(algae_initial_generation_size).

algae_reproduction_limit() ->
    get_env(algae_reproduction_limit).

algae_initial_energy() ->
    get_env(algae_initial_energy).

algae_growth_rate() ->
    get_env(algae_growth_rate).

algae_spawn_size() ->
    get_env(algae_spawn_size).

valid_moves() ->
    get_env(valid_moves).


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_env(Name) ->
    Config = application:get_env(?APPLICATION_NAME, forams_automaton, []),
    proplists:get_value(Name, Config).