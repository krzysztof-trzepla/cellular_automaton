%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 KI AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-ifndef(FORAM_HRL).
-define(FORAM_HRL, 1).

-record(foram, {
    coords :: {non_neg_integer(), non_neg_integer()},
    energy = forams_config:foram_initial_energy() :: integer()
}).

-endif.