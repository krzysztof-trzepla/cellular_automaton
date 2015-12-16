%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2015 KI AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
-module(utils).
-author("Tomasz Lichon").

%% API
-export([for/3, random_coordinates/2, random_shuffle/1]).

%%%===================================================================
%%% API
%%%===================================================================

for(I, N, _Fun) when I > N -> ok;
for(I, N, Fun) ->
    Fun(I),
    for(I + 1, N, Fun).

random_coordinates(MaxW, MaxH) ->
    {random:uniform(MaxW) - 1, random:uniform(MaxH) - 1}.

-spec random_shuffle(List :: list()) -> NewList :: list().
random_shuffle(List) ->
    From = 0,
    To = length(List) + 1,
    [X || {_, X} <- lists:sort([{crypto:rand_uniform(From, To), N} || N <- List])].

%%%===================================================================
%%% Internal functions
%%%===================================================================