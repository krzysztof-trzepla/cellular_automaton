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
-module(cellular_manager_tests).
-author("Krzysztof Trzepla").

-include("cellular_automaton.hrl").
-include_lib("eunit/include/eunit.hrl").

get_section_id_test() ->
    ?assertEqual(0, cellular_manager:get_section_id(n1, [n1])),
    ?assertEqual(0, cellular_manager:get_section_id(n1, [n1, n2, n3])),
    ?assertEqual(1, cellular_manager:get_section_id(n2, [n1, n2, n3])),
    ?assertEqual(2, cellular_manager:get_section_id(n3, [n1, n2, n3])),
    ?assertException(throw, _, cellular_manager:get_section_id(n1, [n2, n3])).

get_lower_cellular_manager_node_test() ->
    ?assertEqual(n1, cellular_manager:get_lower_cellular_manager_node(0, [n1])),
    ?assertEqual(n2, cellular_manager:get_lower_cellular_manager_node(0, [n1, n2])),
    ?assertEqual(n1, cellular_manager:get_lower_cellular_manager_node(1, [n1, n2])),
    ?assertEqual(n3, cellular_manager:get_lower_cellular_manager_node(0, [n1, n2, n3])),
    ?assertEqual(n1, cellular_manager:get_lower_cellular_manager_node(1, [n1, n2, n3])),
    ?assertEqual(n2, cellular_manager:get_lower_cellular_manager_node(2, [n1, n2, n3])).

get_upper_cellular_manager_node_test() ->
    ?assertEqual(n1, cellular_manager:get_upper_cellular_manager_node(0, [n1])),
    ?assertEqual(n2, cellular_manager:get_upper_cellular_manager_node(0, [n1, n2])),
    ?assertEqual(n1, cellular_manager:get_upper_cellular_manager_node(1, [n1, n2])),
    ?assertEqual(n2, cellular_manager:get_upper_cellular_manager_node(0, [n1, n2, n3])),
    ?assertEqual(n3, cellular_manager:get_upper_cellular_manager_node(1, [n1, n2, n3])),
    ?assertEqual(n1, cellular_manager:get_upper_cellular_manager_node(2, [n1, n2, n3])).

get_section_horizontal_range_test() ->
    application:set_env(?APPLICATION_NAME, board_width, 10),
    ?assertEqual({0, 9}, cellular_manager:get_section_horizontal_range()).

get_section_vertical_range_test() ->
    application:set_env(?APPLICATION_NAME, board_height, 10),
    ?assertEqual({0, 9}, cellular_manager:get_section_vertical_range(0, [n])),

    application:set_env(?APPLICATION_NAME, board_height, 3),
    ?assertEqual({0, 1}, cellular_manager:get_section_vertical_range(0, [n1, n2])),
    ?assertEqual({2, 2}, cellular_manager:get_section_vertical_range(1, [n1, n2])),

    application:set_env(?APPLICATION_NAME, board_height, 8),
    ?assertEqual({0, 2}, cellular_manager:get_section_vertical_range(0, [n1, n2, n3])),
    ?assertEqual({3, 5}, cellular_manager:get_section_vertical_range(1, [n1, n2, n3])),
    ?assertEqual({6, 7}, cellular_manager:get_section_vertical_range(2, [n1, n2, n3])),

    application:set_env(?APPLICATION_NAME, board_height, 10),
    ?assertEqual({0, 2}, cellular_manager:get_section_vertical_range(0, [n1, n2, n3, n4])),
    ?assertEqual({3, 5}, cellular_manager:get_section_vertical_range(1, [n1, n2, n3, n4])),
    ?assertEqual({6, 7}, cellular_manager:get_section_vertical_range(2, [n1, n2, n3, n4])),
    ?assertEqual({8, 9}, cellular_manager:get_section_vertical_range(3, [n1, n2, n3, n4])).