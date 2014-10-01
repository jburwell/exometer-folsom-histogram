-module(histogram_compare).

-export([main/1]).

-define(TEST_METRIC_NAME, "test_histogram").

start_exometer_with_histogram(Name) ->
    application:start(exometer),
    exometer_admin:preset_defaults(),
    exometer:new(Name, histogram).

start_folsom_metrics_with_histogram(Name) ->
    application:start(folsom),
    folsom_metrics:new_histogram({?MODULE, Name}).

open_file({ok, File}) ->
    {ok, File};
open_file({error, Reason}) ->
    erlang:error(Reason);
open_file(Filename) ->
    open_file(file:open(Filename, [read])).

read_data_set({ok, Data}, File) ->
    Value = list_to_integer(Data),
    exometer:update(?TEST_METRIC_NAME, Value),
    folsom_metrics:notify(?TEST_METRIC_NAME, Value),
    read_data_set(File);
read_data_set(eof, _File) ->
    ok.

read_data_set(File) ->
    read_data_set(file:read_line(File), File).

main(_Args) ->
    %% Spin up the metrics subsystems ...
    ok = start_folsom_metrics_with_histogram(?TEST_METRIC_NAME),
    ok = start_exometer_with_histogram(?TEST_METRIC_NAME),
    
    %% Load the data set
    {ok, File} = open_file("histogram_data_set.txt"),
    ok = read_data_set(File),
    io:format("Exometer value for ~p is ~p.", [?TEST_METRIC_NAME, exometer:get_value(?TEST_METRIC_NAME)]),
    io:format("Folsom_Metrics value for ~p is ~p.", [?TEST_METRIC_NAME, folsom_metrics:get_metric_value(?TEST_METRIC_NAME)]).

    