%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 KI AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ALGAE_HRL).
-define(ALGAE_HRL, 1).

-record(algae, {
    coords :: {non_neg_integer(), non_neg_integer()},
    energy = forams_config:algae_initial_energy() :: non_neg_integer()
}).

-endif.