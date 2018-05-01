%%%-------------------------------------------------------------------
%%% @author kacper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2018 14:39
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("kacper").

-import(pollution, [createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getDayTypeMeasurements/3, maximum/1, minimum/1, getMinMaxValue/4]).

%% API
-export([start/0, init/0, addStation/2, getMonitor/0, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMinMaxValue/3, stop/0]).

start() ->
  register(pollutionServer, spawn(pollution_server, init, [])).

stop() ->
  call(stop, {}).

init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

call(Message, Arguments) ->
  pollutionServer ! {request, self(), Message, Arguments},
  receive
    {reply, Reply} -> Reply
  end.

addStation(Name, Location) ->
  call(addStation, {Name, Location}).

getMonitor() ->
  call(get, {}).

addValue(StationInfo, Date, Type, Value) ->
  call(addValue, {StationInfo, Date, Type, Value}).

removeValue(StationInfo, Date, Type) ->
  call(removeValue, {StationInfo, Date, Type}).

getOneValue(Type, StationInfo, Date) ->
  call(getOneValue, {Type, StationInfo, Date}).

getStationMean(StationInfo, Type) ->
  call(getStationMean, {StationInfo, Type}).

getDailyMean(Type, Day) ->
  call(getDailyMean, {Type, Day}).

getMinMaxValue(Type, Day, Location) ->
  call(getMinMaxValue, {Type, Day, Location}).

loop(Monitor) ->
  receive
    {request, Pid, addStation, {Name, Location}} ->
      Pid ! {reply, ok},
      loop(pollution:addStation(Name, Location, Monitor));
    {request, Pid, get, {}} ->
      Pid ! {reply, Monitor},
      loop(Monitor);
    {request, Pid, addValue, {StationInfo, Date, Type, Value}} ->
      Pid ! {reply, ok},
      loop(pollution:addValue(StationInfo, Date, Type, Value, Monitor));
    {request, Pid, removeValue, {StationInfo, Date, Type}} ->
      Pid ! {reply, ok},
      loop(pollution:removeValue(StationInfo, Date, Type, Monitor));
    {request, Pid, getOneValue, {Type, StationInfo, Date}} ->
      Pid ! {reply, pollution:getOneValue(Type, StationInfo, Date, Monitor)},
      loop(Monitor);
    {request, Pid, getStationMean, {StationInfo, Type}} ->
      Pid ! {reply, pollution:getStationMean(StationInfo, Type, Monitor)},
      loop(Monitor);
    {request, Pid, getDailyMean, {Type, Day}} ->
      Pid ! {reply, pollution:getDailyMean(Type, Day, Monitor)},
      loop(Monitor);
    {request, Pid, getMinMaxValue, {Type, Day, Location}} ->
      Pid ! {reply, pollution:getMinMaxValue(Type, Day, Location, Monitor)},
      loop(Monitor);
    {request, Pid, stop, {}} ->
      Pid ! {reply, ok}
  end.

