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
-author("Krzysztof Trzepla").

-ifndef(CELLULAR_LOGGER).
-define(CELLULAR_LOGGER, true).

-define(debug(F), ?debug(F, [])).
-define(debug(F, A), lager:debug(F, A)).

-define(info(F), ?info(F, [])).
-define(info(F, A), lager:info(F, A)).

-define(warning(F), ?warning(F, [])).
-define(warning(F, A), lager:warning(F, A)).

-define(error(F), ?error(F, [])).
-define(error(F, A), lager:error(F, A)).

-define(error_stacktrace(F), ?error_stacktrace(F, [])).
-define(error_stacktrace(F, A), lager:error(F ++ "~nStacktrace: ~p",
    A ++ [erlang:get_stacktrace()])).

-endif.
