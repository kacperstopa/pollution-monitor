%%%-------------------------------------------------------------------
%%% @author kacper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2018 13:33
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("kacper").

%% API
-export([start/0, init/0]).



start() -> spawn_link(?MODULE, init, []).

init() ->
  process_flag(trap_exit, true),
  loop().

loop() ->
  register(pollutionServer, spawn_link(pollution_server, init, [])),
  receive
    {'EXIT', Pid, Reason} ->
      loop();
    stop -> ok
  end.
