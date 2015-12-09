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
-module(cellular_worker_behaviour).
-author("Krzysztof Trzepla").

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This callback is called by cellular worker before each step of simulation
%% in order to initialize its state.
%% @end
%%--------------------------------------------------------------------
-callback init_step(Step :: cellular_worker2:step(), State :: cellular_worker2:state()) ->
    NewState :: cellular_worker2:state().

%%--------------------------------------------------------------------
%% @doc
%% This callback is called by cellular worker in order to compute its state
%% it the next step along with states of neighbours seen from its perspective.
%% @end
%%--------------------------------------------------------------------
-callback compute_next_state(
    State :: cellular_worker2:state(),
    NbrsStates :: [{cellular_worker2:neighbour_tag(), cellular_worker2:state()}]
) -> {
    NextState :: cellular_worker2:state(),
    NextNbrsStates :: [{cellular_worker2:neighbour_tag(), cellular_worker2:state()}]
}.

%%--------------------------------------------------------------------
%% @doc
%% This callback is called by cellular worker whenever its state is about to
%% change.
%% @end
%%--------------------------------------------------------------------
-callback merge_state(
    NbrTag :: cellular_worker2:neighbour_tag(),
    State :: cellular_worker2:state(),
    NextState :: cellular_worker2:state()
) ->
    MergedState :: cellular_worker2:state().

%%--------------------------------------------------------------------
%% @doc
%% This callback is called by cellular worker in order to merge states
%% between him and its neighbour. 'State' is equal to the current cellular worker
%% state, 'NbrState' is equal to current neighbour state seen from its perspective,
%% 'NextState' is equal to next cellular worker state seen from its neighbour 
%% perspective and 'NextNbrState' is equal to the next state of cellular worker 
%% neighbour seen from its neighbour perspective. Should return merged states
%% of both cellular worker and its neighbour.
%% @end
%%--------------------------------------------------------------------
-callback merge_neighbour_state(
    NbrTag :: cellular_worker2:neighbour_tag(),
    State :: cellular_worker2:state(),
    NbrState :: cellular_worker2:state(),
    NextState :: cellular_worker2:state(),
    NextNbrState :: cellular_worker2:state()
) -> {
    MergedState :: cellular_worker2:state(),
    MergedNbrState :: cellular_worker2:state()
}.