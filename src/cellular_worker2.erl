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
-module(cellular_worker2).
-author("Krzysztof Trzepla").

-include("cellular_automaton.hrl").
-include("cellular_logger.hrl").

%% API
-export([start_link/2, inverse_neighbour_tag/1, simulate/1]).

-type step() :: non_neg_integer().
-type state() :: term().
-type coordinate() :: integer().
-type position() :: {coordinate(), coordinate()}.
-type neighbour() :: {neighbour_tag(), pid()}.
-type neighbour_tag() :: atom().
-type neighbour_state() :: {neighbour_tag(), state()}.

-export_type([neighbour_tag/0, state/0, coordinate/0, position/0]).

-record(state, {
    x :: coordinate(),
    y :: coordinate(),
    step = 0 :: step(),
    max_steps :: step(),
    state :: state(),
    neighbours = [] :: [neighbour()],
    neighbours_states = [] :: [neighbour_state()],
    neighbours_merged = 0 :: non_neg_integer(),
    behaviour :: module()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start cellular worker simulation loop and links it to the calling process.
%% @end
%%--------------------------------------------------------------------
-spec start_link(X :: coordinate(), Y :: coordinate()) -> {ok, Pid :: pid()}.
start_link(X, Y) ->
%%    {ok, Behaviour} = application:get_env(?APPLICATION_NAME, cellular_worker_behaviour),
%%    {ok, MaxSteps} = application:get_env(?APPLICATION_NAME, simulation_max_steps),
    Pid = spawn_link(?MODULE, simulate, [#state{
        x = X, y = Y
    }]),
%%    Pid ! initialize,
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @doc
%% Returns opposite neighbour tag to provided.
%% @end
%%--------------------------------------------------------------------
-spec inverse_neighbour_tag(NbrTag :: neighbour_tag()) ->
    InvertedNbrTag :: neighbour_tag().
inverse_neighbour_tag(left) -> right;
inverse_neighbour_tag(right) -> left;
inverse_neighbour_tag(up) -> down;
inverse_neighbour_tag(down) -> up.

%%--------------------------------------------------------------------
%% @doc
%% Cellular worker simulation loop.
%% @end
%%--------------------------------------------------------------------
-spec simulate(WrkState :: #state{}) -> ok.
simulate(#state{x = X, y = Y, step = Step, max_steps = Step}) ->
    ?info("Cellular worker (~p, ~p) finished simulation.", [X, Y]);
simulate(#state{x = X, y = Y, step = Step, state = State, behaviour = Behaviour,
    neighbours = Nbrs, neighbours_states = NbrsStates,
    neighbours_merged = NbrsMerged} = WrkState) ->
    NextWrkState = receive
        initialize ->
            InitState = Behaviour:init_step(Step, State),
            InitNbrs = get_neighbours(X, Y),
            send_current_state(Step, InitState, InitNbrs),
            WrkState#state{state = InitState, neighbours = InitNbrs};
        {neighbour_state, {Step, NbrTag, State}} ->
            NewNbrsStates = [{NbrTag, State} | NbrsStates],
            case length(NewNbrsStates) == 4 of
                true ->
                    {NextState, NextNbrsStates} = Behaviour:compute_next_state(
                        State, NewNbrsStates
                    ),
                    send_next_state(Step, up, NextState, NextNbrsStates, Nbrs),
                    WrkState#state{
                        state = NextState,
                        neighbours_states = NextNbrsStates
                    };
                false ->
                    WrkState#state{neighbours_states = NewNbrsStates}
            end;
        {merge_neighbour_state, {Step, NbrTag, NextNbrState, NextState}} ->
            NbrState = proplists:get_value(NbrTag, NbrsStates),
            {MergedState, MergedNbrState} = Behaviour:merge_neighbour_state(
                NbrTag, State, NbrState, NextState, NextNbrState
            ),
            send_merged_state(Step, NbrTag, MergedNbrState, Nbrs),
            case NbrsMerged + 1 == 4 of
                true ->
                    send_current_state(Step + 1, MergedState, Nbrs),
                    WrkState#state{
                        step = Step + 1,
                        state = Behaviour:init_step(Step + 1, MergedState),
                        neighbours_states = [], neighbours_merged = 0
                    };
                false ->
                    WrkState#state{
                        state = MergedState,
                        neighbours_merged = NbrsMerged + 1
                    }
            end;
        {merged_neighbour_state, {Step, NbrTag, NextState}} ->
            NextMergedState = case NbrTag of
                up ->
                    MergedState = Behaviour:merge_state(NbrTag, State, NextState),
                    send_next_state(Step, left, MergedState, NbrsStates, Nbrs),
                    MergedState;
                left ->
                    Behaviour:merge_state(NbrTag, State, NextState)
            end,
            case NbrsMerged + 1 == 4 of
                true ->
                    send_current_state(Step + 1, NextMergedState, Nbrs),
                    WrkState#state{
                        step = Step + 1,
                        state = Behaviour:init_step(Step + 1, NextMergedState),
                        neighbours_states = [], neighbours_merged = 0
                    };
                false ->
                    WrkState#state{
                        state = NextMergedState,
                        neighbours_merged = NbrsMerged + 1
                    }
            end
    after timer:seconds(1) ->
            WrkState
    end,
    ?MODULE:simulate(NextWrkState).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns list of worker neighbours. Worker neighbour is defined as a pid of
%% worker process with associated tag.
%% @end
%%--------------------------------------------------------------------
-spec get_neighbours(X :: coordinate(), Y :: coordinate()) -> Nbrs :: [neighbour()].
get_neighbours(X, Y) ->
    Nbrs = lists:map(fun({NbrTag, {Dx, Dy}}) ->
        case gen_server:call(?CELLULAR_MANAGER_NAME, {get_worker, {X + Dx, Y + Dy}}) of
            {ok, Pid} ->
                {NbrTag, Pid};
            ({redirect, Node, NewPos}) ->
                {ok, Pid} = gen_server:call({?CELLULAR_MANAGER_NAME, Node},
                    {get_worker, NewPos}),
                {NbrTag, Pid}
        end
    end, [{left, {-1, 0}}, {up, {0, 1}}, {right, {1, 0}}, {down, {0, -1}}]),
    ?debug("Neighbours of worker (~p, ~p) => ~p", [X, Y, Nbrs]),
    Nbrs.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends current cellular worker state to all its neighbours.
%% @end
%%--------------------------------------------------------------------
-spec send_current_state(Step :: step(), State :: state(), Nbrs :: [neighbour()]) ->
    ok.
send_current_state(Step, State, Nbrs) ->
    lists:foreach(fun({NbrTag, Nbr}) ->
        Nbr ! {neighbour_state, {Step, inverse_neighbour_tag(NbrTag), State}}
    end, Nbrs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends next cellular worker state to its neighbour identified by tag along with
%% its next state seen from cellular worker perspective.
%% @end
%%--------------------------------------------------------------------
-spec send_next_state(Step :: step(), NbrTag :: neighbour_tag(), State :: state(),
    NbrsStates :: [neighbour_state()], Nbrs :: [neighbour()]) -> ok.
send_next_state(Step, NbrTag, State, NbrsStates, Nbrs) ->
    Nbr = proplists:get_value(NbrTag, Nbrs),
    NbrState = proplists:get_value(NbrTag, NbrsStates),
    Nbr ! {merge_neighbour_state, {
        Step, inverse_neighbour_tag(NbrTag), State, NbrState
    }},
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends to cellular worker neighbour identified by tag its merged state.
%% @end
%%--------------------------------------------------------------------
-spec send_merged_state(Step :: step(), NbrTag :: neighbour_tag(),
    MergedNbrState :: state(), Nbrs :: [neighbour()]) -> ok.
send_merged_state(Step, NbrTag, MergedNbrState, Nbrs) ->
    Nbr = proplists:get_value(NbrTag, Nbrs),
    Nbr ! {merged_neighbour_state, {
        Step, inverse_neighbour_tag(NbrTag), MergedNbrState
    }},
    ok.
