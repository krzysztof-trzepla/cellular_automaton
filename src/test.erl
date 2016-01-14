%%%-------------------------------------------------------------------
%%% @author krzysztof
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2016 17:43
%%%-------------------------------------------------------------------
-module(test).
-author("krzysztof").

%% API
-export([test1/0, test2/0]).


test1() ->
    Start = os:timestamp(),
    Self = self(),
    Id = ets:new(tab, [set, public]),
    spawn(fun() ->
        lists:foreach(fun(_) ->
            ets:insert(Id, {0,0}),
            ets:lookup(Id, 0),
            ets:delete(Id, 0)
        end, lists:seq(1, 100000)),
        Self ! lol
    end),
    receive
        lol -> ok
    end,
    timer:now_diff(os:timestamp(), Start).

test2() ->
    Start = os:timestamp(),

    Self = self(),

    Id1 = ets:new(tab, [set, public]),
    spawn(fun() ->
        lists:foreach(fun(_) ->
            ets:insert(Id1, {0,0}),
            ets:lookup(Id1, 0),
            ets:delete(Id1, 0)
        end, lists:seq(1, 50000)),
        Self ! lol
    end),

    Id2 = ets:new(tab, [set, public]),
    spawn(fun() ->
        lists:foreach(fun(_) ->
            ets:insert(Id2, {0,0}),
            ets:lookup(Id2, 0),
            ets:delete(Id2, 0)
        end, lists:seq(1, 50000)),
        Self ! lol
    end),

    receive
        lol -> ok
    end,

    receive
        lol -> ok
    end,

    timer:now_diff(os:timestamp(), Start).
