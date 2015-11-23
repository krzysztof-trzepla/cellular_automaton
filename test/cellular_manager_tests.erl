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

get_position_test() ->
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node()]),
    ?assertEqual(0, cellular_manager:get_position()),
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [
        node(), 'node1@nohost', 'node2@nohost'
    ]),
    ?assertEqual(0, cellular_manager:get_position()),
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [
        'node1@nohost', node(), 'node2@nohost'
    ]),
    ?assertEqual(1, cellular_manager:get_position()),
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [
        'node1@nohost', 'node2@nohost', node()
    ]),
    ?assertEqual(2, cellular_manager:get_position()),
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [
        'node1@nohost', 'node2@nohost'
    ]),
    ?assertException(throw, _, cellular_manager:get_position()).

get_lower_neighbour_test() ->
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node1]),
    ?assertEqual(node1, cellular_manager:get_lower_neighbour(0)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node1, node2]),
    ?assertEqual(node2, cellular_manager:get_lower_neighbour(0)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node1, node2]),
    ?assertEqual(node1, cellular_manager:get_lower_neighbour(1)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes,
        [node1, node2, node3]
    ),
    ?assertEqual(node2, cellular_manager:get_lower_neighbour(0)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes,
        [node1, node2, node3]
    ),
    ?assertEqual(node3, cellular_manager:get_lower_neighbour(1)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes,
        [node1, node2, node3]
    ),
    ?assertEqual(node1, cellular_manager:get_lower_neighbour(2)).

get_upper_neighbour_test() ->
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node1]),
    ?assertEqual(node1, cellular_manager:get_upper_neighbour(0)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node1, node2]),
    ?assertEqual(node2, cellular_manager:get_upper_neighbour(0)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node1, node2]),
    ?assertEqual(node1, cellular_manager:get_upper_neighbour(1)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes,
        [node1, node2, node3]
    ),
    ?assertEqual(node3, cellular_manager:get_upper_neighbour(0)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes,
        [node1, node2, node3]
    ),
    ?assertEqual(node1, cellular_manager:get_upper_neighbour(1)),

    application:set_env(?APPLICATION_NAME, cellular_manager_nodes,
        [node1, node2, node3]
    ),
    ?assertEqual(node2, cellular_manager:get_upper_neighbour(2)).

get_horizontal_board_range_test() ->
    application:set_env(?APPLICATION_NAME, board_width, 10),
    ?assertEqual({0, 9}, cellular_manager:get_horizontal_board_range()).

get_vertical_board_range_test() ->
    application:set_env(?APPLICATION_NAME, board_height, 10),
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node]),
    ?assertEqual({0, 9}, cellular_manager:get_vertical_board_range(0)),

    application:set_env(?APPLICATION_NAME, board_height, 3),
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes, [node1, node2]),
    ?assertEqual({0, 1}, cellular_manager:get_vertical_board_range(0)),
    ?assertEqual({2, 2}, cellular_manager:get_vertical_board_range(1)),

    application:set_env(?APPLICATION_NAME, board_height, 8),
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes,
        [node1, node2, node3]
    ),
    ?assertEqual({0, 2}, cellular_manager:get_vertical_board_range(0)),
    ?assertEqual({3, 5}, cellular_manager:get_vertical_board_range(1)),
    ?assertEqual({6, 7}, cellular_manager:get_vertical_board_range(2)),

    application:set_env(?APPLICATION_NAME, board_height, 10),
    application:set_env(?APPLICATION_NAME, cellular_manager_nodes,
        [node1, node2, node3, node4]
    ),
    ?assertEqual({0, 2}, cellular_manager:get_vertical_board_range(0)),
    ?assertEqual({3, 5}, cellular_manager:get_vertical_board_range(1)),
    ?assertEqual({6, 7}, cellular_manager:get_vertical_board_range(2)),
    ?assertEqual({8, 9}, cellular_manager:get_vertical_board_range(3)).

get_left_worker_neighbour_test() ->
    application:set_env(?APPLICATION_NAME, board_width, 10),
    ?assertEqual({ok, pid},
        cellular_manager:get_left_worker_neighbour(0, 0, #{{0, 9} => pid})),
    ?assertEqual({ok, pid},
        cellular_manager:get_left_worker_neighbour(0, 1, #{{0, 0} => pid})).

get_right_worker_neighbour_test() ->
    application:set_env(?APPLICATION_NAME, board_width, 10),
    ?assertEqual({ok, pid},
        cellular_manager:get_right_worker_neighbour(0, 9, #{{0, 0} => pid})),
    ?assertEqual({ok, pid},
        cellular_manager:get_right_worker_neighbour(0, 0, #{{0, 1} => pid})).

get_upper_worker_neighbour_test() ->
    application:set_env(?APPLICATION_NAME, board_height, 10),
    ?assertEqual({ok, pid},
        cellular_manager:get_upper_worker_neighbour(1, 0, node, #{{0, 0} => pid})),
    ?assertEqual({redirect, {node, 9, 0}},
        cellular_manager:get_upper_worker_neighbour(0, 0, node, #{})).

get_upper_lower_neighbour_test() ->
    application:set_env(?APPLICATION_NAME, board_height, 10),
    ?assertEqual({ok, pid},
        cellular_manager:get_lower_worker_neighbour(0, 0, node, #{{1, 0} => pid})),
    ?assertEqual({redirect, {node, 0, 0}},
        cellular_manager:get_lower_worker_neighbour(9, 0, node, #{})).