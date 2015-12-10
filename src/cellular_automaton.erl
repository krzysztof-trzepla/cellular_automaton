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
-module(cellular_automaton).
-author("Krzysztof Trzepla").

-include("cellular_automaton.hrl").

%% API
-export([start_simulation/2, stop_simulation/0]).

%%%===================================================================
%%% API
%%%===================================================================

start_simulation(MaxSteps, Module) ->
    try
        Width = Module:width(),
        Height = Module:height(),
        BorderWidth = Module:border_width(),
        BorderHeight = Module:border_height(),
        MaxDesynchronization = Module:max_desynchronization(),
        case check_configuration(MaxSteps, MaxDesynchronization, Width, Height,
            BorderWidth, BorderHeight) of
            ok ->
                gen_server:cast(?CELLULAR_MANAGER_NAME, {start_simulation, MaxSteps, Module});
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Reason ->
            {error, {"Unknown configuration error", Reason}}
    end.

stop_simulation() ->
    gen_server:cast(?CELLULAR_MANAGER_NAME, stop_simulation).

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_configuration(MaxSteps, _, _, _, _, _) when
    not is_integer(MaxSteps); MaxSteps < 0 ->
    {error, "Simulation steps limit should be non-negative integer."};
check_configuration(_, MaxDesynchronization, _, _, _, _) when
    not is_integer(MaxDesynchronization); MaxDesynchronization < 0 ->
    {error, "Maximal desynchronization limit should be non-negative integer."};
check_configuration(_, _, Width, _, _, _) when
    not is_integer(Width); Width =< 0 ->
    {error, "Board width should be a positive integer."};
check_configuration(_, _, _, Height, _, _) when
    not is_integer(Height); Height =< 0 ->
    {error, "Board height should be a positive integer."};
check_configuration(_, _, Width, _, BorderWidth, _) when
    not is_integer(BorderWidth); BorderWidth =< 0; BorderWidth > Width ->
    {error, "Board border width should be a positive integer less than or equal to board width."};
check_configuration(_, _, _, Height, _, BorderHeight) when
    not is_integer(BorderHeight); BorderHeight =< 0; BorderHeight > Height ->
    {error, "Board border height should be a positive integer less than or equal to board height."}.