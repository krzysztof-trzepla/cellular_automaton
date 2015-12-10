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
-export([start_link/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).


-record(state, {
    x :: non_neg_integer(),
    y :: non_neg_integer(),
    max_steps :: non_neg_integer(),
    max_desynch :: non_neg_integer(),
    width :: non_neg_integer(),
    height :: non_neg_integer(),
    border_width :: non_neg_integer(),
    border_height :: non_neg_integer(),
    board :: term(),
    neighbours :: term(),
    neighbours_boards :: term(),
    neighbours_synchs :: term(),
    module :: module()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec start_link(X :: non_neg_integer(), Y :: non_neg_integer(),
    MaxSteps :: non_neg_integer(), Module :: module()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(X, Y, MaxSteps, Module) ->
    ?info("Starting cellular worker (~p, ~p) for module ~p", [X, Y, Module]),
    gen_server:start_link(?MODULE, [X, Y, MaxSteps, Module], []).

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
init([X, Y, MaxSteps, Module]) ->
    Width = Module:width(),
    Height = Module:height(),
    Board = Module:init(),
    case check_configuration(Board, Width, Height) of
        ok ->
            {ok, #state{
                x = X,
                y = Y,
                max_steps = MaxSteps,
                max_desynch = Module:max_desynchronization(),
                width = Width,
                height = Height,
                border_width = Module:border_width(),
                border_height = Module:border_height(),
                board = Board,
                module = Module
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

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
handle_cast({neighbours, Nbrs}, #state{} = State) ->
    NbrsShifts = maps:keys(Nbrs),
    NbrsNum = length(NbrsShifts),
    NbrsBoards = maps:from_list(lists:zip(NbrsShifts, lists:duplicate(NbrsNum, #{}))),
    NbrsSynchs = maps:from_list(lists:zip(NbrsShifts, lists:duplicate(NbrsNum, 0))),
    gen_server:cast(self(), {step, 1}),
    {noreply, State#state{
        neighbours = Nbrs,
        neighbours_boards = NbrsBoards,
        neighbours_synchs = NbrsSynchs
    }};
handle_cast({step, Step}, #state{max_steps = MaxSteps} = State) when Step >= MaxSteps ->
    {stop, normal, State};
handle_cast({step, Step}, #state{board = Board, neighbours_boards = NbrsBoards,
    neighbours_synchs = NbrsSynchs, max_desynch = MaxDesynch, module = Module,
    width = Width, height = Height, border_width = BorderWidth,
    border_height = BorderHeight} = State) ->
    case need_synchronization(Step, maps:values(NbrsSynchs), MaxDesynch) of
        true ->
            {noreply, State};
        false ->
            NbrsShifts = maps:keys(NbrsBoards),
            MergedBoard = merge_boards([Board | maps:values(NbrsBoards)]),
            NewMergedBoard = Module:step(MergedBoard),
            {NewBoard, NewNbrsBoards} = split_board(
                NbrsShifts, NewMergedBoard, Width, Height, BorderWidth, BorderHeight
            ),
            send_board(Step, NewBoard),
            gen_server:cast(self(), {step, Step + 1}),
            {noreply, State#state{board = NewBoard, neighbours_boards = NewNbrsBoards}}
    end;
handle_cast({neighbour_board, Step, NbrShift, NbrBoard}, #state{
    neighbours_boards = NbrsBoards, neighbours_synchs = NbrsSynchs} = State) ->
    {noreply, State#state{
        neighbours_boards = maps:put(NbrShift, NbrBoard, NbrsBoards),
        neighbours_synchs = maps:put(NbrShift, Step, NbrsSynchs)
    }};
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

check_configuration(Board, _, _) when
    not is_map(Board) ->
    {error, "Board should be a map."};
check_configuration(Board, Width, Height) ->
    NewBoard = maps:filter(fun
        ({X, Y}, _) -> X < 0 or X > Width or Y < 0 or Y > Height;
        (_, _) -> true
    end, Board),
    case NewBoard of
        #{} ->
            ok;
        _ ->
            {error, "Board map keys should be coordinates that belong to the board."}
    end.

need_synchronization(Step, LastSynchs, MaxDesynch) ->
    lists:any(fun(LastSynch) ->
        Step - LastSynch > MaxDesynch
    end, LastSynchs).

merge_boards(Boards) ->
    lists:foldl(fun(Board, Acc) ->
        maps:merge(Acc, Board)
    end, #{}, Boards).


split_board(NbrsShifts, MergedBoard, Width, Height, BorderWidth, BorderHeight) ->
    Shifts = [{0, 0} | NbrsShifts],
    EmptyBoards = maps:from_list(lists:zip(Shifts, lists:duplicate(length(Shifts), #{}))),
    NewBoards = maps:fold(fun({X, Y}, Value, Boards) ->
        maps:fold(fun({ShiftX, ShiftY} = Shift, Board, PartialBoards) ->
            case inside(X, -BorderWidth, Width + BorderWidth, Width, Width, ShiftX) and
                inside(Y, -BorderHeight, Height + BorderHeight, Height, Height, ShiftY)
            of
                true ->
                    maps:put(Shift, maps:put({X, Y}, Value, Board), PartialBoards);
                false ->
                    maps:put(Shift, Board, PartialBoards)
            end
        end, #{}, Boards)
    end, EmptyBoards, MergedBoard),
    NewBoard = maps:get({0, 0}, NewBoards),
    NewNbrsBoards = maps:remove({0, 0}, NewBoards),
    {NewBoard, NewNbrsBoards}.

inside(X, Min, Max, Offset, Range, Shift) ->
    max(Min, Shift * Range) =< X =< min(Max, Offset + Shift * Range).

send_board(Step, NbrsShifts, NewBoard, Width, Height, BorderWidth, BorderHeight) ->
    EmptyBoards = maps:from_list(lists:zip(NbrsShifts, lists:duplicate(length(NbrsShifts), #{}))),
