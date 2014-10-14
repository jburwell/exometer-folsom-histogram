-module(histogram_compare).

-export([main/1]).

-define(RUN_LENGTH, 3600000).
-define(TEST_METRIC_NAME, "test_histogram").

start_exometer_with_histogram(Name) ->
    ok = exometer:start(),
    ok = exometer:new(Name, histogram).

start_folsom_metrics_with_histogram(Name) ->
    ok = application:start(folsom),
    %% This histogram configuration matches the configuration in riak_kv_stat
    %% https://github.com/basho/riak_kv/blob/develop/src/riak_kv_stat.erl#L525
    ok = folsom_metrics:new_histogram(Name, slide_uniform, {60, 1028}).

open_file({ok, File}) ->
    {ok, File};
open_file({error, Reason}) ->
    erlang:error(Reason);
open_file(Filename) ->
    open_file(file:open(Filename, [read])).

strip_newline(Value) ->
    re:replace(Value, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

read_data_set(File) ->
    read_data_set(File, []).

read_data_set({ok, Data}, File, DataSet) ->
    {Value, _} = string:to_float(strip_newline(Data)),
    read_data_set(File, lists:append(DataSet, [ Value ]));
read_data_set(eof, _File, DataSet) ->
    {ok, DataSet}.

read_data_set(File, DataSet) ->
    read_data_set(file:read_line(File), File, DataSet).

playback_data_set(DataSet, _Rate) when DataSet == [] ->
    ok;
playback_data_set(DataSet, Rate) ->
    [ Value | NewDataSet ] = DataSet,
    ok = exometer:update(?TEST_METRIC_NAME, Value),
    ok = folsom_metrics:notify({?TEST_METRIC_NAME, Value}),
    timer:sleep(Rate),
    playback_data_set(NewDataSet, Rate).

main(_Args) ->
    %% Initialize the test harness ...
    ok = application:load(histogram_compare),

    %% Spin up the metrics subsystems ...
    ok = start_folsom_metrics_with_histogram(?TEST_METRIC_NAME),
    ok = start_exometer_with_histogram(?TEST_METRIC_NAME),
    
    %% Load the data set
    {ok, File} = open_file("histogram_data_set.txt"),
    {ok, DataSet} = read_data_set(File),
    
    Rate = round(?RUN_LENGTH / length(DataSet)),
    io:format("Playing back ~p records at a rate of 1 record every ~p ms.\n", [length(DataSet), Rate]),
    ok = playback_data_set(DataSet, Rate),
    io:format("Folsom_Metrics value for ~p is ~p.\n", [?TEST_METRIC_NAME, folsom_metrics:get_histogram_statistics(?TEST_METRIC_NAME)]),
    io:format("Exometer value for ~p is ~p.\n", [?TEST_METRIC_NAME, exometer:get_value(?TEST_METRIC_NAME)]).

    
