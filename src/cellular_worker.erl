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
-export([start_link/6]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {
    step :: non_neg_integer(),
    max_steps :: non_neg_integer(),
    max_desynch :: non_neg_integer(),
    board :: term(),
    nbrs :: term(),
    nbrs_step :: term(),
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
-spec start_link(X :: non_neg_integer(), Y :: non_neg_integer(), Bid :: term(),
    Module :: module(), MaxSteps :: non_neg_integer(), Notify :: pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(X, Y, Bid, Module, MaxSteps, Notify) ->
    ?debug("Starting cellular worker (~p, ~p) for module ~p", [X, Y, Module]),
    gen_server:start_link(?MODULE, [X, Y, Bid, MaxSteps, Module, Notify], []).

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
init([X, Y, Bid, MaxSteps, Module, Notify]) ->
    process_flag(trap_exit, true),
    Board = #board{
        x = X,
        y = Y,
        w = Module:width(),
        h = Module:height(),
        dw = Module:border_width(),
        dh = Module:border_height(),
        ids = #{{0, 0} => Bid}
    },
    NbrsStep = lists:foldl(fun(N, M) ->
        maps:put(N, 0, M)
    end, #{}, ?WORKER_NEIGHBOURS),
    Ctx = case application:get_env(?APPLICATION_NAME, logging) of
        {ok, true} -> #{fd => open_file(X, Y, Module)};
        _ -> #{}
    end,
    Module:init(Board),
    {ok, #state{
        step = 0,
        max_steps = MaxSteps,
        max_desynch = Module:max_desynchronization(),
        board = Board,
        module = Module,
        notify = Notify,
        ctx = Ctx,
        nbrs_step = NbrsStep
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
handle_cast({run_simulation, Nbrs, BIds}, #state{board = #board{ids = Ids} = B} = State) ->
    gen_server:cast(self(), step),
    {noreply, State#state{nbrs = Nbrs, board = B#board{ids = maps:merge(Ids, BIds)}}};

handle_cast(step, #state{step = Step, max_steps = MaxSteps} = State) when Step >= MaxSteps ->
    {stop, normal, State};

handle_cast(step, #state{step = Step, board = Board, nbrs = Nbrs, nbrs_step = NbrsStep,
    max_desynch = MaxDesynch, ctx = Ctx, need_synch = NeedSynch, module = Module} = State) ->
    case NeedSynch or need_synchronization(Step, NbrsStep, MaxDesynch) of
        true ->
            {noreply, State#state{need_synch = true}};
        false ->
            Module:step(Ctx#{step => Step}, Board),
            notify_neighbours(Step, Nbrs),
            gen_server:cast(self(), step),
            {noreply, State#state{step = Step + 1}}
    end;
handle_cast({neighbour_step, Nbr, Step}, #state{need_synch = false, nbrs_step = NbrsStep} = State) ->
    {noreply, State#state{nbrs_step = maps:put(Nbr, Step, NbrsStep)}};

handle_cast({neighbour_step, Nbr, NStep}, #state{step = Step, nbrs_step = NbrsStep,
    max_desynch = MaxDesynch} = State) ->
    NewNbrsStep = maps:put(Nbr, NStep, NbrsStep),
    case need_synchronization(Step, NStep, MaxDesynch) of
        false ->
            gen_server:cast(self(), step),
            {noreply, State#state{nbrs_step = NewNbrsStep, need_synch = false}};
        true ->
            {noreply, State#state{nbrs_step = NewNbrsStep, need_synch = true}}
    end;

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
terminate(_Reason, #state{board = #board{x = X, y = Y}, notify = Notify, ctx = Ctx}) ->
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

need_synchronization(_, _, 0) ->
    false;
need_synchronization(Step, NbrsStep, MaxDesynch) ->
    lists:any(fun(NbrStep) ->
        Step - NbrStep > MaxDesynch
    end, maps:values(NbrsStep)).

notify_neighbours(Step, Nbrs) ->
    maps:fold(fun(N, Pid, _) ->
        gen_server:cast(Pid, {neighbour_step, invert_shift(N), Step})
    end, ok, Nbrs).

invert_shift({DX, DY}) ->
    {-DX, -DY}.

open_file(X, Y, Module) ->
    Filename = string:join([atom_to_list(Module), integer_to_list(X), integer_to_list(Y)], "_"),
    FileExt = ".dat",
    File = filename:join([code:root_dir(), "data", Filename ++ FileExt]),
    {ok, Fd} = file:open(File, [write, raw, delayed_write]),
    Fd.
