%%%-------------------------------------------------------------------
%%% @author kacper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2018 17:56
%%%-------------------------------------------------------------------
-module(pollution).
-author("kacper").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getMinMaxValue/4]).


-record(station, {name, location, measurements=[]}).
-record(measurements, {type, value, date}).
-record(monitor, {stations = []}).

createMonitor() -> #monitor{}.

addStation(Name, Location, Monitor) ->

  case {lists:member(Name, getStationsName(Monitor)), lists:member(Location, getStationsLocation(Monitor))} of
    {false, false} ->
      Stations = Monitor#monitor.stations,
      NewMonitor = Monitor#monitor{stations = lists:append(Stations, [#station{name=Name, location = Location}])},
      NewMonitor;
    _ ->
      Monitor
  end.

addValue(StationInfo, Date, Type, Value, Monitor) ->
  Station = findStation(StationInfo, Monitor),
  case Station of
    not_found -> Monitor;

    #station{measurements = Measurements} ->
      case getMeasurementWithDateAndType(Date, Type, Measurements) of
        [] ->
          Stations = Monitor#monitor.stations -- [Station],
          NewStation = Station#station{measurements = lists:append(Measurements, [#measurements{type = Type, value = Value, date = Date}])},
          NewMonitor = Monitor#monitor{stations = lists:append(Stations, [NewStation])},
          NewMonitor;
        _ -> Monitor
      end
  end.

removeValue(StationInfo, Date, Type, Monitor) ->
  Station = findStation(StationInfo, Monitor),
  case Station of
    not_found -> Monitor;

    #station{ measurements = Measurements} ->
      case getMeasurementWithDateAndType(Date, Type, Measurements) of
        [] -> Monitor;
        X ->
          NewStation = Station#station{measurements = Measurements -- X},
          Stations = Monitor#monitor.stations -- [Station],
          NewMonitor = Monitor#monitor{stations = lists:append(Stations, [NewStation])},
          NewMonitor
      end
  end.

getOneValue(Type, StationInfo, Date, Monitor) ->
  Station = findStation(StationInfo, Monitor),
  case Station of
    not_found -> {error, "This station doesn't exist"};

    #station{ measurements = Measurements} ->
      case getMeasurementWithDateAndType(Date, Type, Measurements) of
        [] -> {error, "This measurement doesn't exist"};
        [X] -> X#measurements.value
      end
  end.

getStationMean(StationInfo, Type, Monitor) ->
  Station = findStation(StationInfo, Monitor),
  case Station of
    not_found -> {error, "This station doesn't exist"};

    #station{ measurements = Measurements} ->
      MeasurementsWithType = lists:filter(fun(X) -> X#measurements.type == Type end, Measurements),
      case sumAndNumber(MeasurementsWithType) of
        {0, 0} -> {error, "No measurements with given type"};
        {Sum, Num} -> Sum/Num
      end
  end.


getDailyMean(Type, Day, Monitor) ->
  Measurements = getDayTypeMeasurements(Type, Day, Monitor),
  case sumAndNumber(Measurements) of
    {0, 0} -> {error, "No measurements with given type and day"};
    {Sum, Num} -> Sum/Num
  end.

getDayTypeMeasurements(Type, Day, Monitor) ->
  Measurements = lists:flatmap(fun(X) -> X#station.measurements end, Monitor#monitor.stations),
  DayAndTypeMeasurements = lists:filter(fun(X) -> (element(1, X#measurements.date) == Day) and (X#measurements.type == Type) end, Measurements),
  DayAndTypeMeasurements.


getMinMaxValue(Type, Day, Location, Monitor) ->
  case lists:filter(fun(X) -> X#station.location == Location end, Monitor#monitor.stations) of
    [] -> {error, "No stations with this location"};
    [Station] ->
      Measurements = lists:filter(fun(X) -> (element(1,X#measurements.date) == Day) and (X#measurements.type == Type) end, Station#station.measurements),
      Values = lists:map(fun(X) -> X#measurements.value end, Measurements),
      {minimum(Values), maximum(Values)}
  end.



sumAndNumber(Measurements) ->
  lists:foldl(fun(M,{S,N}) -> {S+M#measurements.value, N+1} end, {0,0}, Measurements).

getStationsName(Monitor)->
  Stations = Monitor#monitor.stations,
  lists:map(fun(X) -> X#station.name end, Stations).

getStationsLocation(Monitor)->
  Stations = Monitor#monitor.stations,
  lists:map(fun(X) -> X#station.location end, Stations).

getMeasurementWithDateAndType(Date, Type, Measurements) ->
  lists:filter(fun(X) -> (X#measurements.date == Date) and (X#measurements.type == Type) end, Measurements).

findStation(LocOrName, Monitor) ->
  Stations = Monitor#monitor.stations,
  case LocOrName of
    {A,B} ->
      case lists:filter(fun(X) -> X#station.location == {A,B} end, Stations) of
        [] -> not_found;
        [X] -> X
      end;
    A ->
      case lists:filter(fun(X) -> X#station.name == A end, Stations) of
        [] -> not_found;
        [X] -> X
      end
  end.


maximum([]) -> {error, "empty list"};
maximum([H|T]) -> lists:foldl(fun(Elem, Acc) -> (if Elem > Acc -> Elem; true -> Acc end) end, H, T).

minimum([]) -> {error, "empty list"};
minimum([H|T]) -> lists:foldl(fun(Elem, Acc) -> (if Elem < Acc -> Elem; true -> Acc end) end, H, T).