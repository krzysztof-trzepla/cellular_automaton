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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {position, workers}).

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
    {ok, Attempts} = application:get_env(neighbours_connection_attempts),
    Nodes = get_neighbours(),
    ?info("Neighbours: ~p", [Nodes]),
    ok = connect_with_neighbours(Nodes, Attempts),
    {ok, #state{workers = start_cellular_workers()}}.

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



connect_with_neighbours(_, 0) ->
    {error, "Attempts limit exceeded."};
connect_with_neighbours(Nodes, Attempts) ->
    ?info("Connecting with neighbours."),
    {ok, Delay} = application:get_env(neighbours_connection_retry_delay),
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

start_cellular_workers() ->
    #{}.

get_neighbours() ->
    {ok, Nodes} = application:get_env(cellular_manager_nodes),
    ?info("Nodes: ~p", [Nodes]),
    Length = length(Nodes),
    ?info("Length: ~p", [Length]),
    Position = erlang:length(lists:takewhile(fun(Node) ->
        Node =/= node()
    end, Nodes)),
    ?info("Position: ~p", [Position]),
    ?info("Upper: ~p", [lists:nth((Position + 1) rem Length + 1, Nodes)]),
    ?info("Lower: ~p", [lists:nth((Position + Length - 1) rem Length + 1, Nodes)]),
    true = Position =< Length,
    lists:filter(fun(Node) -> Node =/= node() end, [
        lists:nth((Position + 1) rem Length + 1, Nodes),
        lists:nth((Position + Length - 1) rem Length + 1, Nodes)
    ]).