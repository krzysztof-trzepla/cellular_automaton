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
-export([get_position/0]).
-export([get_lower_neighbour/1, get_upper_neighbour/1]).
-export([get_horizontal_board_range/0, get_vertical_board_range/1]).
-export([get_left_worker_neighbour/3, get_right_worker_neighbour/3,
    get_upper_worker_neighbour/4, get_lower_worker_neighbour/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-type position() :: position().
-type coordinate() :: {position(), position()}.
-type worker_map() :: #{coordinate() => pid()}.
-type range() :: {position(), position()}.

-record(state, {
    upper_neighbour :: node(),
    lower_neighbour :: node(),
    workers :: worker_map()
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
%% Returns this node position in a list of configured cellular manager nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_position() -> Position :: position().
get_position() ->
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, cellular_manager_nodes),
    {Position, _} = lists:foldl(fun
        (Node, {undefined, N}) when Node =:= node() -> {N, N + 1};
        (_, {P, N}) -> {P, N + 1}
    end, {undefined, 0}, Nodes),
    case Position of
        undefined ->
            throw("Node not found among configured cellular manager nodes.");
        _ ->
            Position
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns this node lower neighbour, that is node that directly follows this
%% node in a circular list of configured cellular manager nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_lower_neighbour(Position :: position()) -> Node :: node().
get_lower_neighbour(Position) ->
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, cellular_manager_nodes),
    lists:nth((Position + 1) rem length(Nodes) + 1, Nodes).

%%--------------------------------------------------------------------
%% @doc
%% Returns this node upper neighbour, that is node that directly proceeds this
%% node in a circular list of configured cellular manager nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_upper_neighbour(Position :: position()) -> Node :: node().
get_upper_neighbour(Position) ->
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, cellular_manager_nodes),
    lists:nth((Position + length(Nodes) - 1) rem length(Nodes) + 1, Nodes).

%%--------------------------------------------------------------------
%% @doc
%% Returns horizontal range of tiles that are associated with this cellular manager.
%% @end
%%--------------------------------------------------------------------
-spec get_horizontal_board_range() -> HorizontalRange :: range().
get_horizontal_board_range() ->
    {ok, Width} = application:get_env(?APPLICATION_NAME, board_width),
    {0, Width - 1}.

%%--------------------------------------------------------------------
%% @doc
%% Returns vertical range of tiles that are associated with this cellular manager.
%% @end
%%--------------------------------------------------------------------
-spec get_vertical_board_range(Position :: position()) ->
    VerticalRange :: range().
get_vertical_board_range(Position) ->
    {ok, Nodes} = application:get_env(?APPLICATION_NAME, cellular_manager_nodes),
    NodesLen = erlang:length(Nodes),
    {ok, Height} = application:get_env(?APPLICATION_NAME, board_height),
    TilesPerNode = Height div NodesLen,
    AdditionalTiles = Height rem NodesLen,
    case Position < AdditionalTiles of
        true ->
            VBegin = Position * TilesPerNode + Position,
            {VBegin, VBegin + TilesPerNode};
        false ->
            VBegin = Position * TilesPerNode + AdditionalTiles,
            {VBegin, VBegin + TilesPerNode - 1}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns left worker neighbour.
%% @end
%%--------------------------------------------------------------------
-spec get_left_worker_neighbour(VPos :: position(), HPos :: position(),
    Workers :: worker_map()) -> {ok, Pid :: pid()}.
get_left_worker_neighbour(VPos, HPos, Workers) ->
    {ok, Width} = application:get_env(?APPLICATION_NAME, board_width),
    maps:find({VPos, (HPos + Width - 1) rem Width}, Workers).

%%--------------------------------------------------------------------
%% @doc
%% Returns right worker neighbour.
%% @end
%%--------------------------------------------------------------------
-spec get_right_worker_neighbour(VPos :: position(), HPos :: position(),
    Workers :: worker_map()) -> {ok, Pid :: pid()}.
get_right_worker_neighbour(VPos, HPos, Workers) ->
    {ok, Width} = application:get_env(?APPLICATION_NAME, board_width),
    maps:find({VPos, (HPos + 1) rem Width}, Workers).

%%--------------------------------------------------------------------
%% @doc
%% Returns upper worker neighbour if its position is in worker map or
%% redirection details to another cellular manager with new position.
%% @end
%%--------------------------------------------------------------------
-spec get_upper_worker_neighbour(VPos :: position(), HPos :: position(),
    UNeighbour :: node(), Workers :: worker_map()) ->
    {ok, Pid :: pid()} | {redirect, {Node :: node(), NewVPos :: position(),
        NewHPos :: position()}}.
get_upper_worker_neighbour(VPos, HPos, UNeighbour, Workers) ->
    {ok, Height} = application:get_env(?APPLICATION_NAME, board_height),
    NeighbourVPos = (VPos + Height - 1) rem Height,
    case maps:find({NeighbourVPos, HPos}, Workers) of
        {ok, Pid} -> {ok, Pid};
        error -> {redirect, {UNeighbour, NeighbourVPos, HPos}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns lower worker neighbour if its position is in worker map or
%% redirection details to another cellular manager with new position.
%% @end
%%--------------------------------------------------------------------
-spec get_lower_worker_neighbour(VPos :: position(), HPos :: position(),
    LNeighbour :: node(), Workers :: worker_map()) ->
    {ok, Pid :: pid()} | {redirect, {Node :: node(), NewVPos :: position(),
        NewHPos :: position()}}.
get_lower_worker_neighbour(VPos, HPos, LNeighbour, Workers) ->
    {ok, Height} = application:get_env(?APPLICATION_NAME, board_height),
    NeighbourVPos = (VPos + 1) rem Height,
    case maps:find({NeighbourVPos, HPos}, Workers) of
        {ok, Pid} -> {ok, Pid};
        error -> {redirect, {LNeighbour, NeighbourVPos, HPos}}
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
    {ok, Attempts} = application:get_env(?APPLICATION_NAME, neighbours_connection_attempts),
    Position = get_position(),
    UpperNeighbour = get_upper_neighbour(Position),
    LowerNeighbour = get_lower_neighbour(Position),
    connect_with_neighbours([UpperNeighbour, LowerNeighbour], Attempts),
    HorizontalRange = get_horizontal_board_range(),
    VerticalRange = get_vertical_board_range(Position),
    {ok, #state{
        upper_neighbour = UpperNeighbour,
        lower_neighbour = LowerNeighbour,
        workers = start_cellular_workers(HorizontalRange, VerticalRange)
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
handle_call({get_worker, VPos, HPos}, _, #state{workers = Workers} = State) ->
    {reply, maps:find({VPos, HPos}, Workers), State};
handle_call({get_worker_neighbours, HPos, VPos}, _, #state{
    upper_neighbour = UNeighbour,
    lower_neighbour = LNeighbour,
    workers = Workers} = State) ->
    {reply, [
        {left, get_left_worker_neighbour(VPos, HPos, Workers)},
        {right, get_right_worker_neighbour(VPos, HPos, Workers)},
        {upper, get_upper_worker_neighbour(VPos, HPos, UNeighbour, Workers)},
        {lower, get_lower_worker_neighbour(VPos, HPos, LNeighbour, Workers)}
    ], State};
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
%% Connects this node with a list of nodes provided as first argument. Returns 
%% 'ok' if connection to all nodes can be established successfully within specified 
%% number of attempts, otherwise error.
%% @end
%%--------------------------------------------------------------------
-spec connect_with_neighbours(Nodes :: [node()], Attempts :: position()) ->
    ok | no_return().
connect_with_neighbours(_, 0) ->
    throw("Connection attempts limit exceeded.");
connect_with_neighbours(Nodes, Attempts) ->
    ?info("Connecting with neighbours."),
    {ok, Delay} = application:get_env(?APPLICATION_NAME, neighbours_connection_retry_delay),
    BadNodes = lists:foldl(fun(Node, BadNodesAcc) ->
        case net_kernel:connect(Node) of
            true ->
                ?info("Successfully connected with neighbour ~p.", [Node]),
                BadNodesAcc;
            Other ->
                ?error("Cannot connect with neighbour ~p due to: ~p. "
                "Reconnecting in ~p seconds.", [Node, Other, Delay]),
                [Node | BadNodesAcc]
        end
    end, [], Nodes),
    case BadNodes of
        [] ->
            ?info("Successfully connected with all neighbours."),
            ok;
        _ ->
            timer:sleep(timer:seconds(Delay)),
            connect_with_neighbours(BadNodes, Attempts - 1)
    end.

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
    lists:foldl(fun(VPos, Map) ->
        lists:foldl(fun(HPos, PartialMap) ->
            {ok, Pid} = cellular_worker_sup:start_child(VPos, HPos),
            maps:put({VPos, HPos}, Pid, PartialMap)
        end, Map, lists:seq(HBegin, HEnd))
    end, #{}, lists:seq(VBegin, VEnd)).
