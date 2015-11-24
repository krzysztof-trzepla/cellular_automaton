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
-module(cellular_manager).
-author("Krzysztof Trzepla").
-behaviour(gen_server).

-include("cellular_automaton.hrl").
-include("cellular_logger.hrl").

%% API
-export([start_link/0]).
-export([get_section_id/2]).
-export([get_upper_cellular_manager_node/2, get_lower_cellular_manager_node/2]).
-export([get_section_horizontal_range/0, get_section_vertical_range/2]).
-export([get_worker_neighbours/5]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-type section_id() :: non_neg_integer().
-type worker_map() :: #{cellular_automaton:position() => pid()}.
-type range() :: {non_neg_integer(), non_neg_integer()}.

-record(state, {
    upper_cellular_manager :: node(),
    lower_cellular_manager :: node(),
    section_horizontal_range :: range(),
    section_vertical_range :: range(),
    workers = #{} :: worker_map()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    ?info("Starting cellular manager on node ~p.", [node()]),
    gen_server:start_link({local, ?CELLULAR_MANAGER_NAME}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Returns cellular manager section ID that is defined by its position in
%% a sorted list of configured cellular manager nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_section_id(Node :: node(), Nodes :: [node()]) ->
    SectionId :: section_id().
get_section_id(Node, Nodes) ->
    {SectionId, _} = lists:foldl(fun
        (Name, {undefined, N}) when Name =:= Node -> {N, N + 1};
        (_, {Id, N}) -> {Id, N + 1}
    end, {undefined, 0}, lists:sort(Nodes)),
    case SectionId of
        undefined ->
            throw("Node not found among configured cellular manager nodes.");
        _ ->
            SectionId
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns upper cellular manager node, that is node that directly follows this
%% node in a sorted circular list of configured cellular manager nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_upper_cellular_manager_node(SectionId :: section_id(), Nodes :: [node()]) ->
    Node :: node().
get_upper_cellular_manager_node(SectionId, Nodes) ->
    lists:nth((SectionId + 1) rem length(Nodes) + 1, lists:sort(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Returns lower cellular manager node, that is node that directly proceeds this
%% node in a sorted circular list of configured cellular manager nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_lower_cellular_manager_node(SectionId :: section_id(), Nodes :: [node()]) ->
    Node :: node().
get_lower_cellular_manager_node(SectionId, Nodes) ->
    lists:nth((SectionId + length(Nodes) - 1) rem length(Nodes) + 1, lists:sort(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Returns horizontal range of tiles that is associated with cellular manager
%% section.
%% @end
%%--------------------------------------------------------------------
-spec get_section_horizontal_range() -> Range :: range().
get_section_horizontal_range() ->
    {ok, Width} = application:get_env(?APPLICATION_NAME, board_width),
    {0, Width - 1}.

%%--------------------------------------------------------------------
%% @doc
%% Returns vertical range of tiles that is associated with cellular manager
%% section.
%% @end
%%--------------------------------------------------------------------
-spec get_section_vertical_range(SectionId :: section_id(), Nodes :: [node()]) ->
    Range :: range().
get_section_vertical_range(SectionId, Nodes) ->
    NodesLen = erlang:length(Nodes),
    {ok, Height} = application:get_env(?APPLICATION_NAME, board_height),
    TilesPerNode = Height div NodesLen,
    AdditionalTiles = Height rem NodesLen,
    case SectionId < AdditionalTiles of
        true ->
            VBegin = SectionId * TilesPerNode + SectionId,
            {VBegin, VBegin + TilesPerNode};
        false ->
            VBegin = SectionId * TilesPerNode + AdditionalTiles,
            {VBegin, VBegin + TilesPerNode - 1}
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_worker_neighbours(X :: cellular_automaton:coordinate(),
    Y :: cellular_automaton:coordinate(), Workers :: worker_map(),
    UpperCellMan :: node(), LowerCellMan :: node()) ->
    [{ok, {Tag :: cellular_automaton:tag(), Pid :: pid()}} | {redirect,
        {Node :: node(), Tag :: cellular_automaton:tag(),
            Position :: cellular_automaton:position()}}].
get_worker_neighbours(X, Y, Workers, UpperCellMan, LowerCellMan) ->
    {ok, Neighbours} = application:get_env(?APPLICATION_NAME, worker_neighbours),
    {ok, Height} = application:get_env(?APPLICATION_NAME, board_height),
    {ok, Width} = application:get_env(?APPLICATION_NAME, board_width),
    lists:map(fun({Tag, {Dx, Dy}}) ->
        NewX = (X + Dx + Width) rem Width,
        NewY = (Y + Dy + Height) rem Height,
        case maps:find({NewX, NewY}, Workers) of
            {ok, Pid} ->
                {ok, {Tag, Pid}};
            error ->
                case {Dy > 0, Dy < 0} of
                    {true, false} ->
                        {redirect, {UpperCellMan, Tag, {NewX, NewY}}};
                    {false, true} ->
                        {redirect, {LowerCellMan, Tag, {NewX, NewY}}}
                end
        end
    end, Neighbours).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([]) ->
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, cellular_manager_nodes),
    SectionId = get_section_id(node(), Nodes),
    LowerCellMan = get_lower_cellular_manager_node(SectionId, Nodes),
    UpperCellMan = get_upper_cellular_manager_node(SectionId, Nodes),
    connect_with_cellular_managers(Nodes),
    HRange = get_section_horizontal_range(),
    VRange = get_section_vertical_range(SectionId, Nodes),
    {ok, #state{
        lower_cellular_manager = LowerCellMan,
        upper_cellular_manager = UpperCellMan,
        section_horizontal_range = HRange,
        section_vertical_range = VRange
    }, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call({get_worker, X, Y}, _, #state{workers = Workers} = State) ->
    {reply, maps:find({X, Y}, Workers), State};
handle_call({get_worker_neighbours, X, Y}, _, #state{
    upper_cellular_manager = UpperCellMan,
    lower_cellular_manager = LowerCellMan,
    workers = Workers
} = State) ->
    {reply, get_worker_neighbours(X, Y, Workers, UpperCellMan, LowerCellMan), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(start_simulation, State) ->
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, cellular_manager_nodes),
    lists:foreach(fun(Node) ->
        gen_server:cast({?CELLULAR_MANAGER_NAME, Node}, start_cellular_workers)
    end, Nodes),
    {noreply, State};
handle_cast(stop_simulation, State) ->
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, cellular_manager_nodes),
    lists:foreach(fun(Node) ->
        gen_server:cast({?CELLULAR_MANAGER_NAME, Node}, stop_cellular_workers)
    end, Nodes),
    {noreply, State};
handle_cast(start_cellular_workers, #state{section_horizontal_range = HRange,
    section_vertical_range = VRange} = State) ->
    {noreply, State#state{workers = start_cellular_workers(HRange, VRange)}};
handle_cast(stop_cellular_workers, #state{workers = Workers} = State) ->
    stop_cellular_workers(Workers),
    {noreply, State#state{workers = #{}}};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts process state when code is changed.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) -> {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Connects this node with provided nodes.
%% @end
%%--------------------------------------------------------------------
-spec connect_with_cellular_managers(Nodes :: [node()]) -> ok.
connect_with_cellular_managers(Nodes) ->
    lists:foreach(fun(Node) ->
        case net_kernel:connect(Node) of
            true ->
                ?info("Successfully connected with cellular manager ~p.", [Node]);
            _ -> ?warning("Cannot connect with cellular manager ~p.", [Node])
        end
    end, Nodes).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts cellular workers.
%% @end
%%--------------------------------------------------------------------
-spec start_cellular_workers(HorizontalRange :: range(), VerticalRange :: range()) ->
    WorkersMap :: #{}.
start_cellular_workers({HBegin, HEnd}, {VBegin, VEnd}) ->
    ?info("Starting cellular workers."),
    lists:foldl(fun(Y, Map) ->
        lists:foldl(fun(X, PartialMap) ->
            {ok, Pid} = cellular_worker_sup:start_child(X, Y),
            maps:put({X, Y}, Pid, PartialMap)
        end, Map, lists:seq(HBegin, HEnd))
    end, #{}, lists:seq(VBegin, VEnd)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops cellular workers.
%% @end
%%--------------------------------------------------------------------
-spec stop_cellular_workers(Workers :: worker_map()) -> ok.
stop_cellular_workers(Workers) ->
    ?info("Stopping cellular workers."),
    maps:fold(fun(_, Pid, _) ->
        ok = cellular_worker_sup:stop_child(Pid)
    end, ok, Workers).
