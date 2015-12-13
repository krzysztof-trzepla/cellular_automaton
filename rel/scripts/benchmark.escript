#!/usr/bin/env escript
%% -*- erlang -*-

-define(NODE, 'worker@127.0.0.1').
-define(COOKIE, 'cellular_automaton').
-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).

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
    MaxSteps = 100,
    Timeout = 3600,
    report_header(ReportFile, MaxSteps, Timeout),
    lists:foreach(fun({WorkersInRow, WorkersInColumn}) ->
        lists:foreach(fun(MaxDesych) ->
            lists:foreach(fun(AntNumber) ->
                benchmark(MaxSteps, WorkersInRow, WorkersInColumn, [
                    {ant_number, AntNumber div (WorkersInColumn * WorkersInRow)},
                    {width, 1000 div WorkersInRow},
                    {height, 1000 div WorkersInColumn},
                    {border_width, MaxDesych},
                    {border_height, MaxDesych},
                    {max_desynchronization, MaxDesych}
                ], Timeout, ReportFile)
            end, [1600])
        end, [1, 2, 5, 10, 20, 50, 100])
    end, [{2, 2}]).

benchmark(MaxSteps, WorkersInRow, WorkersInColumn, Config, Timeout, ReportFile) ->
    io:format("Benchmark case:\n", []),
    io:format("Worker in row: ~p\n", [WorkersInRow]),
    io:format("Worker in column: ~p\n", [WorkersInColumn]),
    io:format("Config: ~p\n\n", [Config]),
    Self = self(),
    Start = os:timestamp(),
    ok = rpc:call(?NODE, application, set_env, [cellular_automaton, langton_ant, Config]),
    ok = rpc:call(?NODE, cellular_automaton, start_simulation, [langton_ant, MaxSteps, WorkersInRow, WorkersInColumn, Self]),
    Duration = receive
        simulation_finished -> timer:now_diff(os:timestamp(), Start)
    after
        timer:seconds(Timeout) -> timeout
    end,
    report_row(ReportFile, WorkersInRow, WorkersInColumn, Config, Duration).

report_header(ReportFile, MaxSteps, Timeout) ->
    Header = <<"Max steps: ", (integer_to_binary(MaxSteps))/binary, "\n",
        "Max duration: ", (integer_to_binary(Timeout))/binary, " [s]\n\n",
        "Workers [j],Workers in row [j],Workers in column [j],Ants number [j],Board width [j],"
        "Board height [j],Border width [j],Border height [j],Max desynchronization [j],"
        "Duration [us]\n">>,
    file:write_file(ReportFile, Header, [write]).

report_row(ReportFile, WorkersInRow, WorkersInColumn, Config, Duration) ->
    DurationBinary = case Duration of
        timeout -> <<"timeout">>;
        _ -> integer_to_binary(Duration)
    end,
    Row = <<
        (integer_to_binary(WorkersInRow * WorkersInColumn))/binary, ",",
        (integer_to_binary(WorkersInRow))/binary, ",",
        (integer_to_binary(WorkersInColumn))/binary, ",",
        (integer_to_binary(proplists:get_value(ant_number, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(width, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(height, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(border_width, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(border_height, Config)))/binary, ",",
        (integer_to_binary(proplists:get_value(max_desynchronization, Config)))/binary, ",",
        DurationBinary/binary, "\n"
    >>,
    file:write_file(ReportFile, Row, [append]).
