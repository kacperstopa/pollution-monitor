%%%-------------------------------------------------------------------
%%% @author kacper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2018 15:33
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("kacper").

-import(pollution_server, [start/0, init/0, addStation/2, getMonitor/0, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMinMaxValue/3, stop/0]).

-include_lib("eunit/include/eunit.hrl").

startServer_test() ->
  Received = start(),
  stop(),
  ?assertEqual(true, Received).

addStation_test() ->
  start(),
  addStation("Station1", {1,1}),
  Monitor = getMonitor(),
  stop(),
  ?assertEqual({monitor, [{station, "Station1", {1,1}, []}]}, Monitor).

addValue_test() ->
  start(),
  addStation("Station1", {1,1}),
  addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50),
  Monitor = getMonitor(),
  stop(),
  ?assertEqual({monitor, [{station, "Station1", {1,1}, [{measurements, "PM10", 50, {{2018,4,13},{23,56,58}}}]}]}, Monitor).

removeValue_test() ->
  start(),
  addStation("Station1", {1,1}),
  addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50),
  removeValue("Station1", {{2018,4,13},{23,56,58}}, "PM10"),
  Monitor = getMonitor(),
  stop(),
  ?assertEqual({monitor, [{station, "Station1", {1,1}, []}]}, Monitor).

getOneValue_test() ->
  start(),
  addStation("Station1", {1,1}),
  addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50),
  Value = getOneValue("PM10", "Station1", {{2018,4,13},{23,56,58}}),
  stop(),
  ?assertEqual(50, Value).

getStationMean_test() ->
  start(),
  addStation("Station1", {1,1}),
  addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50),
  addValue("Station1", {{2018,4,13},{22,56,58}}, "PM10", 100),
  MeanValue = getStationMean("Station1", "PM10"),
  stop(),
  ?assertEqual(75.0, MeanValue).

getDailyMean_test() ->
  start(),
  addStation("Station1", {1,1}),
  addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50),
  addValue("Station1", {{2018,4,13},{22,56,58}}, "PM10", 100),
  addStation("Station2", {2,2}),
  addValue("Station2", {{2018,4,13},{22,56,58}}, "PM10", 300),
  MeanValue = getDailyMean("PM10", {2018,4,13}),
  stop(),
  ?assertEqual(150.0, MeanValue).

getMinMaxValue_test() ->
  start(),
  addStation("Station1", {1,1}),
  addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50),
  addValue("Station1", {{2018,4,13},{22,56,58}}, "PM10", 100),
  MinMaxValue = getMinMaxValue("PM10", {2018,4,13}, {1,1}),
  stop(),
  ?assertEqual({50,100}, MinMaxValue).
