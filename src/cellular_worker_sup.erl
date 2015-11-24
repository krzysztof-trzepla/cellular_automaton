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
-export([start_link/0, start_child/2, stop_child/1]).

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
-spec start_child(X :: non_neg_integer(), Y :: non_neg_integer()) ->
    supervisor:startchild_ret().
start_child(X, Y) ->
    supervisor:start_child(?CELLULAR_WORKER_SUP_NAME, [X, Y]).

%%--------------------------------------------------------------------
%% @doc
%% Stops cellular worker supervised by cellular worker supervisor.
%% @end
%%--------------------------------------------------------------------
-spec stop_child(Pid :: pid()) -> ok | {error, Reason :: term()}.
stop_child(Pid) ->
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