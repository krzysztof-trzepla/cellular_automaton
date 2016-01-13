%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 KI AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Forams automaton
%%% @end
%%%-------------------------------------------------------------------
-module(forams_automaton).
-author("Tomasz Lichon").
-behaviour(cellular_worker_behaviour).

-include("cellular_automaton.hrl").
-include("cellular_logger.hrl").

%% Callbacks
-export([width/0, height/0, border_width/0, border_height/0, max_desynchronization/0,
    init/1, step/2]).

%%%===================================================================
%%% Cellular worker behaviour callbacks
%%%===================================================================

width() ->
    Config = application:get_env(?APPLICATION_NAME, forams_automaton, []),
    proplists:get_value(width, Config, 10).

height() ->
    Config = application:get_env(?APPLICATION_NAME, forams_automaton, []),
    proplists:get_value(height, Config, 10).

border_width() ->
    Config = application:get_env(?APPLICATION_NAME, forams_automaton, []),
    proplists:get_value(border_width, Config, 5).

border_height() ->
    Config = application:get_env(?APPLICATION_NAME, forams_automaton, []),
    proplists:get_value(border_height, Config, 5).

max_desynchronization() ->
    Config = application:get_env(?APPLICATION_NAME, forams_automaton, []),
    proplists:get_value(max_desynchronization, Config, 1).

init(Board) ->
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    algae:init(Board),
    foram:init(Board).

step(Ctx, Board) ->
    draw(Ctx, Board),
    foram:move_all(Board),
    foram:reproduce_all(Board),
    foram:starve_all(Board),
    foram:remove_dead(Board),
    algae:grow_all(Board),
    algae:reproduce_all(Board),
    algae:spawn_individuals(Board).

%%%===================================================================
%%% Internal functions
%%%===================================================================


draw(#{step := Step, fd := Fd}, Board) ->
    {Forams, Algaes} = foram:count(Board),
    Header = <<"Step: ", (integer_to_binary(Step))/binary, "\n">>,
    file:write(Fd, Header),
    file:write(Fd, <<(integer_to_binary(Forams))/binary, ",", (integer_to_binary(Algaes))/binary, "\n">>);
draw(_, _) ->
    ok.
