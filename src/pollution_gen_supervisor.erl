%%%-------------------------------------------------------------------
%%% @author kacper
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. May 2018 23:43
%%%-------------------------------------------------------------------
-module(pollution_gen_supervisor).
-author("kacper").
-behaviour(supervisor).

%% API
-export([init/1, start/0]).

start() -> start_link(nothing).

start_link(_InitValue) ->
  supervisor:start_link(
    {local, serverSupervisor},
    ?MODULE,
    []).

init(_InitValue) ->
  {ok,
    {
      {one_for_all, 2, 3},
      [
        {  pollution_gen_server,
          {
            pollution_gen_server, start, []
          },
          permanent,
          brutal_kill,
          worker,
          [
            pollution_gen_server
          ]
        }
      ]
    }
  }.