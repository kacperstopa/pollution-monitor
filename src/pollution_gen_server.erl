%%%-------------------------------------------------------------------
%%% @author kacper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2018 13:17
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("kacper").
-behaviour(gen_server).

%% API
-export([start/0, addStation/2, init/1, start_link/1, handle_call/3, addValue/4, getOneValue/3, handle_cast/2, terminate/2, stop/0, crash/0]).


start_link(_A) ->
  gen_server:start_link(
    {local,pollution_gen_server},
    pollution_gen_server,
    nothing,
    []).

init(_) ->
  Monitor = pollution:createMonitor(),
  {ok, Monitor}.

start() -> start_link(nothing).

stop() -> gen_server:cast(pollution_gen_server, stop).

crash() -> gen_server:cast(pollution_gen_server, crash).

addStation(Name, Coordinates) ->
  gen_server:call(pollution_gen_server, {addStation, {Name, Coordinates}}).

addValue(StationInfo, Date, Type, Value) ->
  gen_server:call(pollution_gen_server, {addValue, {StationInfo, Date, Type, Value}}).

getOneValue(Type, StationInfo, Date) ->
  gen_server:call(pollution_gen_server, {getOneValue, {Type, StationInfo, Date}}).

handle_call({addStation, {Name, Location}}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Name, Location, Monitor),
  {reply, NewMonitor, NewMonitor};
handle_call({addValue, {StationInfo, Date, Type, Value}}, _From, Monitor) ->
  NewMonitor = pollution:addValue(StationInfo, Date, Type, Value, Monitor),
  {reply, NewMonitor, NewMonitor};
handle_call({getOneValue, {Type, StationInfo, Date}}, _From, Monitor) ->
  {reply, pollution:getOneValue(Type, StationInfo, Date, Monitor), Monitor}.

handle_cast(stop, Monitor) ->
  {stop, normal, Monitor};
handle_cast(crash, Monitor) ->
  1/0,
  {noreply, Monitor}.

terminate(_Reason, _Monitor) ->
  ok.
