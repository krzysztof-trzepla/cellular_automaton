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
-module(cellular_worker_sup).
-author("Krzysztof Trzepla").
-behaviour(supervisor).

-include("cellular_automaton.hrl").

%% API
-export([start_link/0, start_cellular_worker/6, stop_cellular_worker/1]).

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
    supervisor:start_link({local, ?CELLULAR_WORKER_SUP_NAME}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts cellular worker supervised by cellular worker supervisor.
%% @end
%%--------------------------------------------------------------------
-spec start_cellular_worker(X :: non_neg_integer(), Y :: non_neg_integer(),
    Width :: non_neg_integer(), Height :: non_neg_integer(),
    Epsilon :: non_neg_integer(), Module :: module()) ->
    supervisor:startchild_ret().
start_cellular_worker(X, Y, Width, Height, Epsilon, Module) ->
    supervisor:start_child(?CELLULAR_WORKER_SUP_NAME, [X, Y, Width, Height, Epsilon, Module]).

%%--------------------------------------------------------------------
%% @doc
%% Stops cellular worker supervised by cellular worker supervisor.
%% @end
%%--------------------------------------------------------------------
-spec stop_cellular_worker(Pid :: pid()) -> ok | {error, Reason :: term()}.
stop_cellular_worker(Pid) ->
    supervisor:terminate_child(?CELLULAR_WORKER_SUP_NAME, Pid).

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
    {ok, {#{strategy => simple_one_for_one, intensity => 1000, period => 3600}, [
        cellular_worker_spec()
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns supervisor child specification of cellular worker.
%% @end
%%--------------------------------------------------------------------
-spec cellular_worker_spec() -> ChildSpec :: supervisor:child_spec().
cellular_worker_spec() ->
    #{
        id => cellular_worker,
        start => {cellular_worker, start_link, []},
        restart => transient,
        shutdown => timer:seconds(10),
        type => worker,
        modules => [cellular_worker]
    }.
