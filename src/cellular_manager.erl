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
-export([get_node_id/2]).
-export([get_up_node/2, get_down_node/2]).
-export([get_x_range/0, get_y_range/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-type node_id() :: non_neg_integer().
-type workers() :: #{cellular_automaton:position() => pid()}.
-type range() :: {non_neg_integer(), non_neg_integer()}.

-record(state, {
    up_node :: node(),
    down_node :: node(),
    x_range :: range(),
    y_range :: range(),
    rows_received = 0 :: non_neg_integer(),
    workers = #{} :: workers()
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
-spec get_node_id(Node :: node(), Nodes :: [node()]) ->
    NodeId :: node_id().
get_node_id(Node, Nodes) ->
    {NodeId, _} = lists:foldl(fun
        (Name, {undefined, N}) when Name =:= Node ->
            {N, N + 1};
        (_, {Id, N}) -> {Id, N + 1}
    end, {undefined, 0}, lists:sort(Nodes)),
    case NodeId of
        undefined ->
            throw("Node not found among configured cellular manager nodes.");
        _ ->
            NodeId
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns upper cellular manager node, that is node that directly follows this
%% node in a sorted circular list of configured cellular manager nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_up_node(NodeId :: node_id(), Nodes :: [node()]) ->
    Node :: node().
get_up_node(NodeId, Nodes) ->
    lists:nth((NodeId + 1) rem length(Nodes) + 1, lists:sort(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Returns lower cellular manager node, that is node that directly proceeds this
%% node in a sorted circular list of configured cellular manager nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_down_node(NodeId :: node_id(), Nodes :: [node()]) ->
    Node :: node().
get_down_node(NodeId, Nodes) ->
    lists:nth((NodeId + length(Nodes) - 1) rem length(Nodes) + 1, lists:sort(Nodes)).

%%--------------------------------------------------------------------
%% @doc
%% Returns horizontal range of tiles that is associated with cellular manager
%% section.
%% @end
%%--------------------------------------------------------------------
-spec get_x_range() -> Range :: range().
get_x_range() ->
    {ok, Width} = application:get_env(?APPLICATION_NAME, board_width),
    {0, Width - 1}.

%%--------------------------------------------------------------------
%% @doc
%% Returns vertical range of tiles that is associated with cellular manager
%% section.
%% @end
%%--------------------------------------------------------------------
-spec get_y_range(NodeId :: node_id(), Nodes :: [node()]) ->
    Range :: range().
get_y_range(NodeId, Nodes) ->
    NodesLen = erlang:length(Nodes),
    {ok, Height} = application:get_env(?APPLICATION_NAME, board_height),
    TilesPerNode = Height div NodesLen,
    AdditionalTiles = Height rem NodesLen,
    case NodeId < AdditionalTiles of
        true ->
            VBegin = NodeId * TilesPerNode + NodeId,
            {VBegin, VBegin + TilesPerNode};
        false ->
            VBegin = NodeId * TilesPerNode + AdditionalTiles,
            {VBegin, VBegin + TilesPerNode - 1}
    end.

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
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, nodes),
    NodeId = get_node_id(node(), Nodes),
    UpNode = get_down_node(NodeId, Nodes),
    DownNode = get_up_node(NodeId, Nodes),
    connect_with_cellular_managers(Nodes),
    XRange = get_x_range(),
    YRange = get_y_range(NodeId, Nodes),
    {ok, #state{
        up_node = UpNode,
        down_node = DownNode,
        x_range = XRange,
        y_range = YRange
    }}.

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
handle_call(Request, _From, State) ->
    ?warning("Invalid request: ~p", [Request]),
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
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, nodes),
    lists:foreach(fun(Node) ->
        gen_server:cast({?CELLULAR_MANAGER_NAME, Node}, start_cellular_workers)
    end, Nodes),
    {noreply, State};
handle_cast(stop_simulation, State) ->
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, nodes),
    lists:foreach(fun(Node) ->
        gen_server:cast({?CELLULAR_MANAGER_NAME, Node}, stop_cellular_workers)
    end, Nodes),
    {noreply, State};
handle_cast(start_cellular_workers, #state{up_node = UpNode, down_node = DownNode,
    x_range = XRange, y_range = {YBegin, YEnd} = YRange} = State) ->
    Wrks = start_cellular_workers(XRange, YRange),
    FirstRow = get_cellular_workers_row(YBegin, Wrks),
    LastRow = get_cellular_workers_row(YEnd, Wrks),
    gen_server:cast({?CELLULAR_MANAGER_NAME, UpNode}, {worker_row, LastRow}),
    gen_server:cast({?CELLULAR_MANAGER_NAME, DownNode}, {worker_row, FirstRow}),
    {noreply, State#state{workers = Wrks}};
handle_cast(stop_cellular_workers, #state{workers = Wrks} = State) ->
    stop_cellular_workers(Wrks),
    {noreply, State#state{workers = #{}}};
handle_cast({worker_row, WrkRow}, #state{rows_received = 0, workers = Wrks} = State) ->
    {noreply, State#state{rows_received = 1, workers = maps:merge(Wrks, WrkRow)}};
handle_cast({worker_row, WrkRow}, #state{x_range = XRange, y_range = YRange,
    rows_received = 1, workers = Wrks} = State) ->
    NewWrks = maps:merge(Wrks, WrkRow),
    send_worker_neighbours(XRange, YRange, NewWrks),
    {noreply, State#state{rows_received = 2, workers = NewWrks}};
handle_cast(Request, State) ->
    ?warning("Invalid request: ~p", [Request]),
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
handle_info(Info, State) ->
    ?warning("Invalid info: ~p", [Info]),
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
-spec start_cellular_workers(XRange :: range(), YRange :: range()) -> Wrks :: workers().
start_cellular_workers({XBegin, XEnd}, {YBegin, YEnd}) ->
    ?info("Starting cellular workers."),
    lists:foldl(fun(Y, Wrks) ->
        lists:foldl(fun(X, WrkRow) ->
            %                                                           W    H   eps  module
            {ok, Pid} = cellular_worker_sup:start_cellular_worker(X, Y, 100, 100, 50, langton_ant),
            maps:put({X, Y}, Pid, WrkRow)
        end, Wrks, lists:seq(XBegin, XEnd))
    end, #{}, lists:seq(YBegin, YEnd)).

get_cellular_workers_row(Row, Wrks) ->
    maps:fold(fun
        ({X, Y}, Pid, WrksRow) when Y =:= Row ->
            maps:put({X, Y}, Pid, WrksRow);
        (_, _, WrksRow) ->
            WrksRow
    end, #{}, Wrks).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops cellular workers.
%% @end
%%--------------------------------------------------------------------
-spec stop_cellular_workers(Wrks :: workers()) -> ok.
stop_cellular_workers(Wrks) ->
    ?info("Stopping cellular workers."),
    maps:fold(fun(_, Pid, _) ->
        ok = cellular_worker_sup:stop_cellular_worker(Pid)
    end, ok, Wrks).

send_worker_neighbours({XBegin, XEnd}, {YBegin, YEnd}, Wrks) ->
    {ok, Height} = application:get_env(?APPLICATION_NAME, board_height),
    {ok, Width} = application:get_env(?APPLICATION_NAME, board_width),
    lists:foreach(fun(Y) ->
        lists:foreach(fun(X) ->
            Wrk = maps:get({X, Y}, Wrks),
            Nbrs = lists:foldl(fun({DX, DY}, Acc) ->
                Nbr = get_neighbour(X, Y, DX, DY, Width, Height),
                [maps:get(Nbr, Wrks) | Acc]
            end, [], ?WORKER_NEIGHBOURS),
            gen_server:cast(Wrk, {neighbours, lists:reverse(Nbrs)})
        end, lists:seq(XBegin, XEnd))
    end, lists:seq(YBegin, YEnd)).


get_neighbour(X, Y, DX, DY, Width, Height) ->
    {(X + DX + Width) rem Width, (Y + DY + Height) rem Height}.
