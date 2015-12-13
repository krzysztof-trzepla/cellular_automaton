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
-export([start_link/5]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).


-record(state, {
    x :: non_neg_integer(),
    y :: non_neg_integer(),
    step :: non_neg_integer(),
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
    module :: module(),
    notify :: pid(),
    ctx :: #{},
    need_synch = false :: boolean()
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
    Module :: module(), MaxSteps :: non_neg_integer(), Notify :: pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(X, Y, Module, MaxSteps, Notify) ->
    ?debug("Starting cellular worker (~p, ~p) for module ~p", [X, Y, Module]),
    gen_server:start_link(?MODULE, [X, Y, MaxSteps, Module, Notify], []).

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
init([X, Y, MaxSteps, Module, Notify]) ->
    process_flag(trap_exit, true),
    NbrsNum = length(?WORKER_NEIGHBOURS),
    NbrsBoards = maps:from_list(lists:zip(?WORKER_NEIGHBOURS, lists:duplicate(NbrsNum, #{}))),
    NbrsSynchs = maps:from_list(lists:zip(?WORKER_NEIGHBOURS, lists:duplicate(NbrsNum, 0))),
    Ctx = case application:get_env(?APPLICATION_NAME, logging) of
        {ok, true} -> #{fd => open_file(X, Y, Module)};
        _ -> #{}
    end,
    {ok, #state{
        x = X,
        y = Y,
        step = 0,
        max_steps = MaxSteps,
        max_desynch = Module:max_desynchronization(),
        width = Module:width(),
        height = Module:height(),
        border_width = Module:border_width(),
        border_height = Module:border_height(),
        board = Module:init(),
        module = Module,
        notify = Notify,
        neighbours_boards = NbrsBoards,
        neighbours_synchs = NbrsSynchs,
        ctx = Ctx
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
handle_cast({run_simulation, Nbrs}, #state{} = State) ->
    gen_server:cast(self(), step),
    {noreply, State#state{neighbours = Nbrs}};

handle_cast(step, #state{neighbours = undefined} = State) ->
    {noreply, State};

handle_cast(step, #state{step = Step, max_steps = MaxSteps} = State) when Step >= MaxSteps ->
    {stop, normal, State};

handle_cast(step, #state{step = Step, board = Board, neighbours_boards = NbrsBoards,
    neighbours_synchs = NbrsSynchs, max_desynch = MaxDesynch, module = Module,
    neighbours = Nbrs, width = Width, height = Height, border_width = BorderWidth,
    border_height = BorderHeight, ctx = Ctx, need_synch = NeedSynch} = State) ->
    case NeedSynch or need_synchronization(Step, NbrsSynchs, MaxDesynch) of
        true ->
            {noreply, State#state{need_synch = true}};
        false ->
            NbrsShifts = maps:keys(NbrsBoards),
            MergedBoard = merge_boards([Board | maps:values(NbrsBoards)]),
            NewMergedBoard = Module:step(Ctx#{step => Step}, MergedBoard),
            {NewBoard, NewNbrsBoards} = split_board(
                NbrsShifts, NewMergedBoard, Width, Height, BorderWidth, BorderHeight
            ),
            case Step rem MaxDesynch of
                0 ->
                    InnerBoards = split_inner_board(NbrsShifts, Board, Width, Height, BorderWidth, BorderHeight),
                    send_boards(Step, Nbrs, InnerBoards);
                _ ->
                    ok
            end,
            gen_server:cast(self(), step),
            {noreply, State#state{step = Step + 1, board = NewBoard, neighbours_boards = NewNbrsBoards}}
    end;

handle_cast({neighbour_board, NbrStep, NbrShift, NbrBoard}, #state{
    neighbours_boards = NbrsBoards, neighbours_synchs = NbrsSynchs, need_synch =
    NeedSynch, step = Step, max_desynch = MaxDesynch} = State) ->
    NewNbrsBoards = maps:put(NbrShift, NbrBoard, NbrsBoards),
    NewNbrsSynchs = maps:put(NbrShift, NbrStep, NbrsSynchs),
    NewNeedSynch = case NeedSynch of
        true -> need_synchronization(Step, NewNbrsSynchs, MaxDesynch);
        false -> false
    end,
    case {NeedSynch, NewNeedSynch} of
        {true, false} -> gen_server:cast(self(), step);
        _ -> ok
    end,
    {noreply, State#state{
        need_synch = NewNeedSynch,
        neighbours_boards = NewNbrsBoards,
        neighbours_synchs = NewNbrsSynchs
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
terminate(_Reason, #state{x = X, y = Y, notify = Notify, ctx = Ctx}) ->
    case Ctx of
        #{fd := Fd} -> file:close(Fd);
        _ -> ok
    end,
    gen_server:cast(Notify, {worker_finished, {X, Y}}).

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

need_synchronization(Step, NbrsSynchs, MaxDesynch) ->
    lists:any(fun(LastSynch) ->
        Step - LastSynch > MaxDesynch
    end, maps:values(NbrsSynchs)).

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
    (max(Min, Shift * Range) =< X) and (X =< min(Max, Offset + Shift * Range)).

split_inner_board(NbrsShifts, InnerBoard, Width, Height, BorderWidth, BorderHeight) ->
    EmptyBoards = maps:from_list(lists:zip(NbrsShifts, lists:duplicate(length(NbrsShifts), #{}))),
    maps:fold(fun({X, Y}, Value, Boards) ->
        maps:fold(fun({ShiftX, ShiftY} = Shift, Board, PartialBoards) ->
            case inside(X, 0, Width, Width, Width - BorderWidth, ShiftX) and
                inside(Y, 0, Height, Height, Height - BorderHeight, ShiftY)
            of
                true ->
                    NewPos = shift({X, Y}, Shift, Width, Height),
                    maps:put(Shift, maps:put(NewPos, Value, Board), PartialBoards);
                false ->
                    maps:put(Shift, Board, PartialBoards)
            end
        end, #{}, Boards)
    end, EmptyBoards, InnerBoard).


invert_shift({DX, DY}) ->
    {-DX, -DY}.

send_boards(Step, Nbrs, Boards) ->
    maps:fold(fun(NbrShift, NbrBoard, _) ->
        NbrShiftInv = invert_shift(NbrShift),
        gen_server:cast(maps:get(NbrShift, Nbrs), {neighbour_board, Step, NbrShiftInv, NbrBoard})
    end, undefined, Boards).


shift({X, Y}, {ShiftX, ShiftY}, DX, DY) ->
    {X + ShiftX * DX, Y + ShiftY * DY}.

open_file(X, Y, Module) ->
    Filename = string:join([atom_to_list(Module), integer_to_list(X), integer_to_list(Y)], "_"),
    FileExt = ".dat",
    File = filename:join([code:root_dir(), "data", Filename ++ FileExt]),
    {ok, Fd} = file:open(File, [write, raw, delayed_write]),
    Fd.
