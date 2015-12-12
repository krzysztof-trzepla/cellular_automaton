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
-module(langton_ant).
-author("Krzysztof Trzepla").
-behaviour(cellular_worker_behaviour).

-include("cellular_logger.hrl").

%% Callbacks
-export([width/0, height/0, border_width/0, border_height/0, max_desynchronization/0,
    init/0, step/1]).


%%%===================================================================
%%% Cellular worker behaviour callbacks
%%%===================================================================


%%%===================================================================
%%% Internal functions
%%%===================================================================


width() -> 10.

height() -> 10.

border_width() -> 3.

border_height() -> 3.

max_desynchronization() -> 2.

init() -> #{}.

step(Board) ->
    Board.