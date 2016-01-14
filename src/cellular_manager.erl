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

-record(state, {
    notify :: pid(),
    wrks = #{} :: #{},
    wrks_boards = #{} :: #{}
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
    ?debug("Starting cellular manager on node ~p.", [node()]),
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
    {ok, #state{}}.

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
handle_cast({run_simulation, Module, MaxSteps, XRange, YRange, Notify}, State) ->
    {Wrks, WrksBoards} = start_cellular_workers(Module, MaxSteps, XRange, YRange),
    run_simulation(Wrks, WrksBoards, XRange, YRange),
    {noreply, State#state{
        notify = Notify,
        wrks = Wrks
    }};

handle_cast({worker_finished, {X, Y}}, #state{wrks = Wrks, notify = Pid} = State) ->
    NewWrks = maps:remove({X, Y}, Wrks),
    case maps:size(NewWrks) == 0 of
        true -> Pid ! simulation_finished;
        false -> ok
    end,
    {noreply, State#state{wrks = NewWrks}};

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

start_cellular_workers(Module, MaxSteps, {XBegin, XEnd}, {YBegin, YEnd}) ->
    lists:foldl(fun(Y, {Wrks, WrksBoard}) ->
        lists:foldl(fun(X, {WrkRow, WrkRowBoard}) ->
            Bid = ets:new(board, [set, public, {read_concurrency, true}, {write_concurrency, true}]),
            {ok, Pid} = cellular_worker_sup:start_cellular_worker(X, Y, Bid, Module, MaxSteps, self()),
            {maps:put({X, Y}, Pid, WrkRow), maps:put({X, Y}, Bid, WrkRowBoard)}
        end, {Wrks, WrksBoard}, lists:seq(XBegin, XEnd))
    end, {#{}, #{}}, lists:seq(YBegin, YEnd)).

run_simulation(Wrks, WrksBoards, {XBegin, XEnd}, {YBegin, YEnd}) ->
    Width = XEnd - XBegin + 1,
    Height = YEnd - YBegin + 1,
    maps:fold(fun({X, Y}, Wrk, _) ->
        {WrkNbrs, WrkBids} = lists:foldl(fun({DX, DY}, {Nbrs, Bids}) ->
            NbrShift = {(X + DX + Width) rem Width, (Y + DY + Height) rem Height},
            {
                maps:put({DX, DY}, maps:get(NbrShift, Wrks), Nbrs),
                maps:put({DX, DY}, maps:get(NbrShift, WrksBoards), Bids)
            }
        end, {#{}, #{}}, ?WORKER_NEIGHBOURS),
        gen_server:cast(Wrk, {run_simulation, WrkNbrs, WrkBids})
    end, ok, Wrks).