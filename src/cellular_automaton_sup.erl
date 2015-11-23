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
-module(cellular_automaton_sup).
-author("Krzysztof Trzepla").
-behaviour(supervisor).

-include("cellular_automaton.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(),
        [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 1000, period => 3600}, [
        cellular_worker_sup_spec(),
        cellular_manager_spec()
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns supervisor child specification of cellular manager.
%% @end
%%--------------------------------------------------------------------
-spec cellular_manager_spec() -> ChildSpec :: supervisor:child_spec().
cellular_manager_spec() ->
    #{
        id => cellular_manager,
        start => {cellular_manager, start_link, []},
        restart => transient,
        shutdown => timer:seconds(10),
        type => worker,
        modules => [cellular_manager]
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns supervisor child specification of cellular worker supervisor.
%% @end
%%--------------------------------------------------------------------
-spec cellular_worker_sup_spec() -> ChildSpec :: supervisor:child_spec().
cellular_worker_sup_spec() ->
    #{
        id => cellular_worker_sup,
        start => {cellular_worker_sup, start_link, []},
        restart => permanent,
        shutdown => timer:seconds(10),
        type => supervisor,
        modules => [cellular_worker_sup]
    }.