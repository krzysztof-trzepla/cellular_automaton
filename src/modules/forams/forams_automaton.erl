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
    init/0, step/2]).

%%%===================================================================
%%% Cellular worker behaviour callbacks
%%%===================================================================

width() ->
    Config = application:get_env(?APPLICATION_NAME, forams, []),
    proplists:get_value(width, Config, 10).

height() ->
    Config = application:get_env(?APPLICATION_NAME, forams, []),
    proplists:get_value(height, Config, 10).

border_width() ->
    Config = application:get_env(?APPLICATION_NAME, forams, []),
    proplists:get_value(border_width, Config, 5).

border_height() ->
    Config = application:get_env(?APPLICATION_NAME, forams, []),
    proplists:get_value(border_height, Config, 5).

max_desynchronization() ->
    Config = application:get_env(?APPLICATION_NAME, forams, []),
    proplists:get_value(max_desynchronization, Config, 1).

init() ->
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    foram:init(algae:init(#{})).

step(Ctx, Board) ->
    draw(Ctx, Board),
    Board2 = foram:move_all(Board),
    Board3 = foram:reproduce_all(Board2),
    Board4 = foram:starve_all(Board3),
    Board5 = foram:remove_dead(Board4),
    Board6 = algae:grow_all(Board5),
    Board7 = algae:reproduce_all(Board6),
    algae:spawn_individuals(Board7).

%%%===================================================================
%%% Internal functions
%%%===================================================================


draw(#{step := Step, fd := Fd}, Board) ->
    Header = <<"Step: ", (integer_to_binary(Step))/binary, "\n">>,
    file:write(Fd, Header),
    {Forams, Algaes} = foram:count(Board),
    file:write(Fd, <<Forams/integer, ",", Algaes/integer, "\n">>);
draw(_, _) ->
    ok.
