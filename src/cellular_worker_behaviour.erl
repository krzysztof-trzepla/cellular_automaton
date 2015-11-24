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
%% This callback is called at the beginning of simulation by cellular
%% worker in order to initialize its state.
%% @end
%%--------------------------------------------------------------------
-callback init() ->
    State :: cellular_worker:state().

%%--------------------------------------------------------------------
%% @doc
%% This callback is called by cellular worker in order to compute its state
%% it the next step along with states of neighbours seen from its perspective.
%% @end
%%--------------------------------------------------------------------
-callback compute_next_state(
    State :: cellular_worker:state(),
    NbrsStates :: [{cellular_worker:neighbour_tag(), cellular_worker:state()}]
) -> {
    NextState :: cellular_worker:state(),
    NextNbrsStates :: [{cellular_worker:neighbour_tag(), cellular_worker:state()}]
}.

%%--------------------------------------------------------------------
%% @doc
%% This callback is called by cellular worker whenever its state is about to
%% change.
%% @end
%%--------------------------------------------------------------------
-callback merge_state(
    State :: cellular_worker:state(),
    NextState :: cellular_worker:state()
) ->
    MergedState :: cellular_worker:state().

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
    NbrTag :: cellular_worker:neighbour_tag(),
    State :: cellular_worker:state(),
    NbrState :: cellular_worker:state(),
    NextState :: cellular_worker:state(),
    NextNbrState :: cellular_worker:state()
) -> {
    MergedState :: cellular_worker:state(),
    MergedNbrState :: cellular_worker:state()
}.