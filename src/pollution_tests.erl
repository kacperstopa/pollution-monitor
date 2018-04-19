%%%-------------------------------------------------------------------
%%% @author kacper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2018 23:39
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("kacper").

-import(pollution, [createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getDayTypeMeasurements/3, maximum/1, minimum/1, getMinMaxValue/4]).

-include_lib("eunit/include/eunit.hrl").

createMonitor_test() ->
  ?assertEqual({monitor, []}, createMonitor()).

addStation_test() ->
  M1 = createMonitor(),
  M2 = addStation("Station1", {1,1}, M1),
  ?assertEqual({monitor, [{station, "Station1", {1,1}, []}]}, M2).

addValue_test() ->
  M1 = createMonitor(),
  M2 = addStation("Station1", {1,1}, M1),
  M3 = addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50, M2),
  ?assertEqual({monitor, [{station, "Station1", {1,1}, [{measurements, "PM10", 50, {{2018,4,13},{23,56,58}}}]}]}, M3).

removeValue_test() ->
  M1 = createMonitor(),
  M2 = addStation("Station1", {1,1}, M1),
  M3 = addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50, M2),
  M4 = removeValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", M3),
  ?assertEqual({monitor, [{station, "Station1", {1,1}, []}]}, M4).

getOneValue_test() ->
  M1 = createMonitor(),
  M2 = addStation("Station1", {1,1}, M1),
  M3 = addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50, M2),
  Value = getOneValue("PM10", "Station1", {{2018,4,13},{23,56,58}}, M3),
  ?assertEqual(50, Value).

getStationMean_test() ->
  M1 = createMonitor(),
  M2 = addStation("Station1", {1,1}, M1),
  M3 = addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50, M2),
  M4 = addValue("Station1", {{2018,4,13},{22,56,58}}, "PM10", 100, M3),
  MeanValue = getStationMean("Station1", "PM10", M4),
  ?assertEqual(75.0, MeanValue).

getDailyMean_test() ->
  M1 = createMonitor(),
  M2 = addStation("Station1", {1,1}, M1),
  M3 = addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50, M2),
  M4 = addValue("Station1", {{2018,4,13},{22,56,58}}, "PM10", 100, M3),
  M5 = addStation("Station2", {2,2}, M4),
  M6 = addValue("Station2", {{2018,4,13},{22,56,58}}, "PM10", 300, M5),
  MeanValue = getDailyMean("PM10", {2018,4,13}, M6),
  ?assertEqual(150.0, MeanValue).

getMinMaxValue_test() ->
  M1 = createMonitor(),
  M2 = addStation("Station1", {1,1}, M1),
  M3 = addValue("Station1", {{2018,4,13},{23,56,58}}, "PM10", 50, M2),
  M4 = addValue("Station1", {{2018,4,13},{22,56,58}}, "PM10", 100, M3),
  MinMaxValue = getMinMaxValue("PM10", {2018,4,13}, {1,1}, M4),
  ?assertEqual({50,100}, MinMaxValue).

