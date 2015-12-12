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
-module(cellular_worker_behaviour).
-author("Krzysztof Trzepla").

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

-callback width() -> non_neg_integer().

-callback height() -> non_neg_integer().

-callback border_width() -> non_neg_integer().

-callback border_height() -> non_neg_integer().

-callback max_desynchronization() -> non_neg_integer().

-callback init() -> term().

-callback step(Step :: non_neg_integer(), Board :: term()) -> NewBoard :: term().