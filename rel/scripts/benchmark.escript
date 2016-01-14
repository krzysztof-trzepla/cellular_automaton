#!/usr/bin/env escript
%% -*- erlang -*-

-define(NODE, 'worker@127.0.0.1').
-define(COOKIE, 'cellular_automaton').
-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).
-define(CELLULAR_WORKER_MOD, forams_automaton).

%% API
-export([main/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

main([ReportFile]) ->
    try
        init(),
        benchmark(ReportFile),
        halt(?EXIT_SUCCESS)
    catch
        Error:Reason ->
            io:format("Error: ~p\n", [Error]),
            io:format("Reason: ~p\n", [Reason]),
            io:format("Stacktrace: ~p\n", [erlang:get_stacktrace()]),
            halt(?EXIT_FAILURE)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init() ->
    {A, B, C} = os:timestamp(),
    NodeName = "benchmark_" ++ integer_to_list(A, 32) ++ integer_to_list(B, 32)
        ++ integer_to_list(C, 32) ++ "@127.0.0.1",
    net_kernel:start([list_to_atom(NodeName), longnames]),
    erlang:set_cookie(node(), ?COOKIE),
    true = net_kernel:connect(?NODE).

benchmark(ReportFile) ->
    MaxSteps = 2,
    Timeout = 3600,
    report_header(ReportFile, MaxSteps, Timeout),
    lists:foreach(fun({WorkersInRow, WorkersInColumn}) ->
        lists:foreach(fun(MaxDesych) ->
            lists:foreach(fun(Repeat) ->
                Config = overwrite_config(?CELLULAR_WORKER_MOD, [
                    {width, 192 div WorkersInRow},
                    {height, 192 div WorkersInColumn},
                    {border_width, MaxDesych},
                    {border_height, MaxDesych},
                    {max_desynchronization, MaxDesych}
                ]),
                benchmark(MaxSteps, Repeat, WorkersInRow, WorkersInColumn, Config, Timeout, ReportFile)
            end, lists:seq(1, 1))
        end, [200])%, 2, 5, 10, 20, 50])
    end, [{2, 2}]).%, {2, 1}, {2, 2}, {3, 2}, {4, 2}, {4, 3}, {4, 4}, {6, 4}, {8, 4}, {6, 6}, {8, 6}, {8, 8}]).

benchmark(MaxSteps, Repeat, WorkersInRow, WorkersInColumn, Config, Timeout, ReportFile) ->
    io:format("Benchmark case:\n", []),
    io:format("Repeat: ~p\n", [Repeat]),
    io:format("Worker in row: ~p\n", [WorkersInRow]),
    io:format("Worker in column: ~p\n", [WorkersInColumn]),
    io:format("Config: ~p\n\n", [Config]),
    Self = self(),
    Start = os:timestamp(),
    ok = rpc:call(?NODE, application, set_env, [cellular_automaton, ?CELLULAR_WORKER_MOD, Config]),
    ok = rpc:call(?NODE, cellular_automaton, start_simulation,
        [?CELLULAR_WORKER_MOD, MaxSteps, WorkersInRow, WorkersInColumn, Self]),
    Duration = receive
        simulation_finished -> timer:now_diff(os:timestamp(), Start)
    after
        timer:seconds(Timeout) -> timeout
    end,
    report_row(ReportFile, Repeat, WorkersInRow, WorkersInColumn, Config, Duration).

report_header(ReportFile, MaxSteps, Timeout) ->
    Header = <<"Max steps: ", (integer_to_binary(MaxSteps))/binary, "\n",
        "Max duration: ", (integer_to_binary(Timeout))/binary, " [s]\n\n",
        "Repeat [j],Workers [j],Workers in row [j],Workers in column [j],Board width [j],"
        "Board height [j],Border width [j],Border height [j],Max desynchronization [j],"
        "Duration [us]\n">>,
    file:write_file(ReportFile, Header, [write]).

report_row(ReportFile, Repeat, WorkersInRow, WorkersInColumn, Config, Duration) ->
    DurationBinary = case Duration of
        timeout -> <<"timeout">>;
        _ -> integer_to_binary(Duration)
    end,
    Row = <<
        (integer_to_binary(Repeat))/binary, ",",
        (integer_to_binary(WorkersInRow * WorkersInColumn))/binary, ",",
        (integer_to_binary(WorkersInRow))/binary, ",",
        (integer_to_binary(WorkersInColumn))/binary, ",",
        (integer_to_binary(proplists:get_value(width, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(height, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(border_width, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(border_height, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(max_desynchronization, Config)))/binary, ",",
        DurationBinary/binary, "\n"
    >>,
    file:write_file(ReportFile, Row, [append]).


overwrite_config(Module, NewConfig) ->
    AppConfig = rpc:call(?NODE, application, get_all_env, [cellular_automaton]),
    {_, Config} = lists:keyfind(Module, 1, AppConfig),
    lists:foldl(fun({Key, Value}, UpdatedConfig) ->
        lists:keyreplace(Key, 1, UpdatedConfig, {Key, Value})
    end, Config, NewConfig).