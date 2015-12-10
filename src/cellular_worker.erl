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
-module(cellular_worker).
-author("Krzysztof Trzepla").
-behaviour(gen_server).

-include("cellular_automaton.hrl").
-include("cellular_logger.hrl").

%% API
-export([start_link/7, run/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-type coordinate() :: integer().
-type point() :: {coordinate(), coordinate()}.

-record(state, {
    x :: coordinate(),
    y :: coordinate(),
    width :: non_neg_integer(),
    height :: non_neg_integer(),
    eps :: non_neg_integer(),
    module :: module(),
    boards = #{} :: #{point() => #{point() => term()}},
    board :: #{point() => term()},
    neighbours :: #{point() => pid()},
    last_refresh :: #{point() => non_neg_integer()},
    steps_done :: non_neg_integer(),
    max_steps :: non_neg_integer(),
    needs_sync = true :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec start_link(X :: coordinate(), Y :: coordinate(),
    Width :: non_neg_integer(), Height :: non_neg_integer(),
    Epsilon :: non_neg_integer(), Module :: module(),
    MaxSteps :: non_neg_integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(X, Y, Width, Height, Epsilon, Module, MaxSteps) ->
    ?info("Starting cellular worker (~p, ~p, ~p, ~p, ~p, ~p, ~p)",
        [X, Y, Width, Height, Epsilon, Module, MaxSteps]),

    gen_server:start_link(?MODULE,
        [X, Y, Width, Height, Epsilon, Module, MaxSteps], []).


run(Worker, Neighbours) ->
    gen_server:cast(Worker, {neighbours, Neighbours}),
    gen_server:cast(Worker, step).


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
init([X, Y, Width, Height, Epsilon, Module, MaxSteps]) ->
    Board = maps:fold(
        fun({DX, DY}, Stuff, Acc) ->
            maps:put({X + DX, Y + DY}, Stuff, Acc)
        end,
        #{},
        Module:init(Width, Height)),

    LastRefresh = maps:from_list(lists:zip(?WORKER_NEIGHBOURS,
        [-10000000000000 || _ <- lists:seq(1, length(?WORKER_NEIGHBOURS))])),

    {ok, #state{x = X, y = Y, width = Width, height = Height,
        board = Board, eps = Epsilon, module = Module, steps_done = 0,
        last_refresh = LastRefresh, max_steps = MaxSteps}}.

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
handle_cast({neighbours, Neighbours}, State) ->
    N = maps:from_list(lists:zip(?WORKER_NEIGHBOURS, Neighbours)),

    maps:fold(
        fun({DX, DY}, Pid, _Acc) ->
            gen_server:cast(Pid, {board, {-DX, -DY}, 0, State#state.board})
        end,
        undefined,
        N),

    {noreply, State#state{neighbours = N}};

handle_cast({board, Neighbour, AfterStep, Board}, State) ->
%%    ?info("Neighbour: ~p, after step: ~p", [Neighbour, AfterStep]),
    LastRefresh = maps:put(Neighbour, AfterStep, State#state.last_refresh),
    Boards = maps:put(Neighbour, Board, State#state.boards),
    NewState = State#state{last_refresh = LastRefresh, boards = Boards},
    NeedsSync = need_synchronization(NewState),

    gen_server:cast(self(), step),

    {noreply, NewState#state{needs_sync = NeedsSync}};

handle_cast(step, #state{needs_sync = true} = State) ->
    {noreply, State};

handle_cast(step, #state{steps_done = S, max_steps = S} = State) ->
%%    ?info("Board: ~p", [State#state.board]),
    {noreply, State};

handle_cast(step, #state{module = Module} = State) ->
    MegaBoard = merge_boards(State),
    NewMegaBoard = Module:compute_stuff(MegaBoard),
    {MidBoard, NeighbourBoards} = split_board(NewMegaBoard, State),

    StepsDone = State#state.steps_done + 1,
%%    ?info("Step ~p", [StepsDone]),

    maps:fold(
        fun({DX, DY}, Pid, _Acc) ->
            gen_server:cast(Pid, {board, {-DX, -DY}, StepsDone, MidBoard})
        end,
        undefined,
        State#state.neighbours),

    gen_server:cast(self, step),

    {noreply, State#state{
        board = MidBoard,
        boards = NeighbourBoards,
        steps_done = StepsDone,
        needs_sync = need_synchronization(State)}};

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


need_synchronization(State) ->
    lists:any(
        fun(StepsDone) ->
            State#state.steps_done - StepsDone >= State#state.eps
        end, maps:values(State#state.last_refresh)).


merge_boards(State) ->
    lists:foldl(
        fun(NeighborMap, MergeMap) ->
            maps:fold(
                fun({X, Y}, Term, MergeMergeMap) ->
                    case in_eps(X, Y, State) of
                        true -> maps:put({X, Y}, Term, MergeMergeMap);
                        false -> MergeMergeMap
                    end
                end,
                MergeMap,
                NeighborMap)
        end,
        State#state.board,
        maps:values(State#state.boards)).


split_board(Board, State) ->
    maps:fold(
        fun({X, Y}, Term, {MidBoard, OtherBoards}) ->
            case in_box(X, Y, State) of
                true -> {maps:put({X, Y}, Term, MidBoard), OtherBoards};
                false ->
                    case in_eps(X, Y, State) of
                        false -> {MidBoard, OtherBoards};
                        true ->
                            Neigh = assign_neighbour(X, Y, State),
                            OldMap = maps:get(Neigh, OtherBoards),
                            NewMap = maps:put({X, Y}, Term, OldMap),
                            {MidBoard, maps:put(Neigh, NewMap, OtherBoards)}
                    end
            end
        end,
        {#{}, new_neighbour_boards()},
        Board).


assign_neighbour(X, Y, #state{x = SX, y = SY}) when X < SX andalso Y < SY ->
    {-1, -1};
assign_neighbour(X, Y, #state{x = SX, y = SY, height = H}) when X < SX andalso Y < SY + H ->
    {-1, 0};
assign_neighbour(X, _Y, #state{x = SX}) when X < SX ->
    {-1, 1};
assign_neighbour(X, Y, #state{x = SX, y = SY, width = W}) when X < SX + W andalso Y < SY ->
    {0, -1};
assign_neighbour(X, _Y, #state{x = SX, width = W}) when X < SX + W ->
    {0, 1};
assign_neighbour(_X, Y, #state{y = SY}) when Y < SY ->
    {1, -1};
assign_neighbour(_X, Y, #state{y = SY, height = H}) when Y < SY + H ->
    {1, 0};
assign_neighbour(_X, _Y, #state{}) ->
    {1, 1}.

new_neighbour_boards() ->
    maps:from_list(lists:zip(?WORKER_NEIGHBOURS,
        [#{} || _ <- lists:seq(1, length(?WORKER_NEIGHBOURS))])).


in_eps(X, Y, #state{x = SX, y = SY, width = W, height = H, eps = Eps}) ->
    lists:any(fun(Val) -> abs(Val) =< Eps end,
        [X - SX, Y - SY, X - SX + W, Y - SY + H]).

in_box(X, Y, #state{x = SX, y = SY, width = W, height = H}) ->
    in_range(X, SX, SX + W) andalso in_range(Y, SY, SY + H).

in_range(X, A, B) ->
    X >= A andalso X =< B.
